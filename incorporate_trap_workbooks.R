library(tidyverse)
library(readxl)
library(writexl)

# Run this section after the existing code has created `animal_ids` and
# `behavior_raw`. All files are assumed to be in data_dir.
data_dir <- "data"

missing_ids <- read_excel(
  file.path(data_dir, "missing_animal_ids.xlsx"),
  sheet = "missing_by_row",
  col_types = "text"
) %>%
  transmute(
    subject = str_extract(str_to_upper(str_squish(subject)), "[A-Z]*\\d+"),
    reward = str_to_lower(str_squish(reward)),
    sex = str_to_lower(str_squish(sex)),
    day = if_else(parse_number(day) == 1, 1L, 30L),
    across(
      c(
        missing_infusion,
        missing_active,
        missing_inactive,
        missing_cue
      ),
      ~ str_to_lower(.x) == "true"
    )
  )

trap_files <- file.path(
  data_dir,
  c(
    "Heroin Incubation + D1 TRAP data 2021.xlsx",
    "Heroin Incubation + D2 TRAP data 2021.xlsx"
  )
)

clean_subject <- function(x) {
  str_extract(
    str_to_upper(str_squish(as.character(x))),
    "[A-Z]*\\d+"
  )
}


# ---------------------------------------------------------------------------
# Read the vertically stacked infusion/active/inactive tables in one cohort tab
# ---------------------------------------------------------------------------

read_trap_ivsa <- function(file, sheet) {

  x <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text"
  )

  labels <- x[[1]] %>%
    str_squish() %>%
    str_to_upper()

  table_rows <- which(
    labels == "INFUSIONS" |
      str_detect(labels, "^ACTIVE\\s+LEVER") |
      str_detect(labels, "^INACTIVE\\s+LEVER")
  )

  map(table_rows, \(header_row) {

    behavior <- case_when(
      str_detect(labels[header_row], "^INACTIVE\\s+LEVER") ~ "inactive",
      str_detect(labels[header_row], "^ACTIVE\\s+LEVER")   ~ "active",
      TRUE                                                  ~ "infusion"
    )

    first_data_row <- header_row + 2L

    # Animal rows form one contiguous block below each table header.
    block_ids <- clean_subject(x[[1]][first_data_row:nrow(x)])
    first_blank <- which(is.na(block_ids))[1]

    last_data_row <- if (is.na(first_blank)) {
      nrow(x)
    } else {
      first_data_row + first_blank - 2L
    }

    data_rows <- first_data_row:last_data_row

    header <- x[header_row, ] %>%
      unlist(use.names = FALSE) %>%
      str_squish() %>%
      str_to_upper()

    candidate_cols <- which(
      str_detect(header, "^DAY\\s*(?:[1-9]|10|OFF)$")
    )

    # Most tabs have 11 dated columns because one is a day off. In one cohort,
    # the DAY OFF label is shifted by one column. Select the 10 populated
    # acquisition columns and number them chronologically rather than trusting
    # that label.
    populated <- map_int(
      candidate_cols,
      \(column) sum(
        !is.na(suppressWarnings(parse_double(x[[column]][data_rows])))
      )
    )

    day_cols <- candidate_cols[
      order(populated, decreasing = TRUE)[seq_len(min(10L, length(candidate_cols)))]
    ] %>%
      sort()

    if (length(day_cols) != 10L) {
      stop("Could not identify 10 IVSA days in sheet: ", sheet)
    }

    out <- x[data_rows, c(1, 3, day_cols)]
    names(out) <- c(
      "subject",
      "condition",
      paste0("day_", 1:10)
    )

    needed <- missing_ids %>%
      filter(.data[[paste0("missing_", behavior)]]) %>%
      distinct(subject, reward, sex)

    out %>%
      mutate(
        subject = clean_subject(subject),
        condition = str_to_lower(str_squish(condition)),
        reward = "heroin"
      ) %>%
      filter(condition == "heroin") %>%
      inner_join(needed, by = c("subject", "reward")) %>%
      pivot_longer(
        starts_with("day_"),
        names_to = "day",
        names_prefix = "day_",
        values_to = "value"
      ) %>%
      transmute(
        subject,
        reward,
        sex,
        phase = "ivsa",
        day = as.integer(day),
        behavior,
        value = parse_double(value)
      )
  }) %>%
    list_rbind()
}


# ---------------------------------------------------------------------------
# Read repeated Day 1/Day 30 blocks in a consolidated cue-reactivity tab
# ---------------------------------------------------------------------------

read_trap_cue <- function(file, sheet = "Cue reactivity") {

  x <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE,
    col_types = "text"
  )

  row_text <- apply(
    x,
    1,
    \(z) str_c(z[!is.na(z)], collapse = " | ")
  )

  header_rows <- which(
    map_lgl(
      seq_len(nrow(x)),
      \(row) any(
        str_to_upper(str_squish(unlist(x[row, ], use.names = FALSE))) ==
          "ANIMAL ID",
        na.rm = TRUE
      )
    )
  )

  # Determine the day attached to each repeated header. One D1 workbook block
  # has a blank Day 30 label, so a second header following Day 1 is Day 30.
  block_days <- rep(NA_integer_, length(header_rows))

  for (i in seq_along(header_rows)) {
    first_context_row <- if (i == 1L) 1L else header_rows[i - 1L] + 1L

    day_labels <- str_extract_all(
      row_text[first_context_row:header_rows[i]],
      regex("\\bDAY\\s*(?:1|3\\d)\\b", ignore_case = TRUE)
    ) %>%
      unlist()

    block_days[i] <- if (length(day_labels) > 0L) {
      if_else(parse_number(last(day_labels)) == 1, 1L, 30L)
    } else if (i > 1L && block_days[i - 1L] == 1L) {
      30L
    } else {
      NA_integer_
    }
  }

  map2(header_rows, block_days, \(header_row, cue_day) {

    header <- x[header_row, ] %>%
      unlist(use.names = FALSE) %>%
      str_squish() %>%
      str_to_upper()

    id_col <- which(header == "ANIMAL ID")[1]
    condition_col <- which(header == "CONDITION")[1]
    infusion_col <- which(header == "INFUSIONS")[1]
    active_col <- which(header == "ACTIVE LEVER")[1]
    inactive_col <- which(header == "INACTIVE LEVER")[1]

    first_data_row <- header_row + 1L
    block_ids <- clean_subject(x[[id_col]][first_data_row:nrow(x)])
    first_blank <- which(is.na(block_ids))[1]

    last_data_row <- if (is.na(first_blank)) {
      nrow(x)
    } else {
      first_data_row + first_blank - 2L
    }

    out <- x[
      first_data_row:last_data_row,
      c(id_col, condition_col, infusion_col, active_col, inactive_col)
    ]

    names(out) <- c(
      "subject",
      "condition",
      "infusion",
      "active",
      "inactive"
    )

    needed <- missing_ids %>%
      filter(missing_cue, day == cue_day) %>%
      distinct(subject, reward, sex, day)

    out %>%
      mutate(
        subject = clean_subject(subject),
        condition = str_to_lower(str_squish(condition)),
        reward = "heroin",
        day = cue_day
      ) %>%
      filter(condition == "heroin") %>%
      inner_join(
        needed,
        by = c("subject", "reward", "day")
      ) %>%
      pivot_longer(
        c(infusion, active, inactive),
        names_to = "behavior",
        values_to = "value"
      ) %>%
      transmute(
        subject,
        reward,
        sex,
        phase = "cue_reactivity",
        day,
        behavior,
        value = parse_double(value)
      )
  }) %>%
    list_rbind()
}


# ---------------------------------------------------------------------------
# Extract only previously missing records and append non-overlapping keys
# ---------------------------------------------------------------------------

trap_raw <- trap_files %>%
  map(\(file) {
    cohort_sheets <- excel_sheets(file) %>%
      keep(
        ~ str_detect(
          .x,
          regex(
            "^(D1 TRAP|D2 TRAP|MIXED D1 D2 TRAP)",
            ignore_case = TRUE
          )
        )
      )

    bind_rows(
      map(cohort_sheets, \(sheet) read_trap_ivsa(file, sheet)) %>%
        list_rbind(),
      read_trap_cue(file)
    )
  }) %>%
  list_rbind()

behavior_key <- c(
  "subject", "reward", "sex", "phase", "day", "behavior"
)

if (
  trap_raw %>%
    count(across(all_of(behavior_key))) %>%
    filter(n > 1L) %>%
    nrow() > 0L
) {
  stop("The TRAP import produced duplicate subject/day/behavior keys.")
}

behavior_raw <- bind_rows(
  behavior_raw,
  trap_raw %>%
    anti_join(
      behavior_raw %>% distinct(across(all_of(behavior_key))),
      by = behavior_key
    )
) %>%
  arrange(reward, sex, subject, phase, day, behavior)


# ---------------------------------------------------------------------------
# Recalculate missingness from the updated behavior_raw and export the audit
# ---------------------------------------------------------------------------

missing_audit <- animal_ids %>%
  transmute(
    subject = clean_subject(subject),
    reward = str_to_lower(str_squish(reward)),
    sex = str_to_lower(str_squish(sex)),
    day = if_else(parse_number(as.character(day)) == 1, 1L, 30L)
  ) %>%
  left_join(
    behavior_raw %>%
      filter(
        phase == "ivsa",
        behavior %in% c("infusion", "active", "inactive")
      ) %>%
      group_by(subject, reward, sex, behavior) %>%
      summarise(
        n_days = n_distinct(day[!is.na(value)]),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = behavior,
        values_from = n_days,
        names_prefix = "n_",
        values_fill = 0L
      ),
    by = c("subject", "reward", "sex")
  ) %>%
  left_join(
    behavior_raw %>%
      filter(
        phase == "cue_reactivity",
        behavior %in% c("infusion", "active", "inactive")
      ) %>%
      group_by(subject, reward, sex, day) %>%
      summarise(
        n_cue_behaviors = n_distinct(behavior[!is.na(value)]),
        .groups = "drop"
      ),
    by = c("subject", "reward", "sex", "day")
  ) %>%
  mutate(
    across(
      c(n_infusion, n_active, n_inactive, n_cue_behaviors),
      ~ replace_na(.x, 0L)
    ),
    missing_infusion = n_infusion < 10L,
    missing_active = n_active < 10L,
    missing_inactive = n_inactive < 10L,
    missing_cue = n_cue_behaviors < 3L,
    missing_data_types = pmap_chr(
      list(
        missing_infusion,
        missing_active,
        missing_inactive,
        missing_cue
      ),
      \(inf, act, inact, cue) {
        str_c(
          c(
            if (inf) "ivsa infusion",
            if (act) "ivsa active",
            if (inact) "ivsa inactive",
            if (cue) "cue reactivity"
          ),
          collapse = "; "
        )
      }
    )
  )

final_missing_by_row <- missing_audit %>%
  filter(
    missing_infusion |
      missing_active |
      missing_inactive |
      missing_cue
  ) %>%
  mutate(day = str_c("day ", day)) %>%
  select(
    subject, reward, sex, day,
    missing_infusion, missing_active,
    missing_inactive, missing_cue,
    missing_data_types
  )

final_missing_unique_animals <- missing_audit %>%
  group_by(subject, reward, sex) %>%
  summarise(
    days_in_animal_ids = str_c(
      "day ",
      sort(unique(day)),
      collapse = ", "
    ),
    missing_infusion = any(missing_infusion),
    missing_active = any(missing_active),
    missing_inactive = any(missing_inactive),
    missing_cue_day1 = any(day == 1L & missing_cue),
    missing_cue_day30 = any(day == 30L & missing_cue),
    missing_any = any(
      missing_infusion |
        missing_active |
        missing_inactive |
        missing_cue
    ),
    .groups = "drop"
  ) %>%
  filter(missing_any)

write_xlsx(
  list(
    final_missing_by_row = final_missing_by_row,
    final_missing_unique = final_missing_unique_animals
  ),
  file.path(data_dir, "final_missing_animal_ids.xlsx")
)
