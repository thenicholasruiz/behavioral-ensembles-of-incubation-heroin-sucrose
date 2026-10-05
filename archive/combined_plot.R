# Prepare the data (sneek calculation and session labels)
inc_dbl_scr %>%
  mutate(seek = ifelse(is.nan(seek), 0, seek),
         sneek = seek + sniff,
         session = recode(session, `1` = "Day 1", `2` = "Day 30"),
         drug = recode(drug, `1` = "Heroin", `2` = "Sucrose")) -> inc_sneek

# Filter by drug 
inc_sneek %>%
  filter(drug == "Heroin") -> inc_sneek_heroin

inc_sneek %>%
  filter(drug == "Sucrose") -> inc_sneek_sucrose

inc_sneek_heroin <- as.data.frame(inc_sneek_heroin)
inc_sneek_sucrose <- as.data.frame(inc_sneek_sucrose)

# Make sure session is a factor with levels that match the color scheme
inc_sneek_heroin$session <- factor(inc_sneek_heroin$session, levels = c("Day 1", "Day 30"))

# Define color scheme for drugs
session_colors <- c("Day 1" = "deeppink", "Day 30" = "deepskyblue")

# Function to create the plots for a given behavior across a session
create_plot <- function(data, x_var, x_label, y_label, x_limits, y_limits, effects_mat_h, effects_mat_s, color_pack) {
  # Ensure the effects matrices are data frames
  data <- as.data.frame(data)
  effects_mat_h <- as.data.frame(effects_mat_h)
  effects_mat_s <- as.data.frame(effects_mat_s)
  
  # Using aes() instead of aes_string()
  ggplot(data, aes(x = .data[[x_var]], y = lever, color = session, fill = session)) +
    geom_point(alpha = 0.2) +  # Add individual data points
    # Day 1 regression line
    geom_ribbon(data = effects_mat_h, aes(x = .data[[x_var]], y = fit, ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
    geom_line(data = effects_mat_h, aes(x = .data[[x_var]], y = fit), linewidth = 2) +
    # Day 30 regression line
    geom_ribbon(data = effects_mat_s, aes(x = .data[[x_var]], y = fit, ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
    geom_line(data = effects_mat_s, aes(x = .data[[x_var]], y = fit), linewidth = 2) +
    scale_color_manual(values = color_pack) +  # Define colors for sessions
    labs(x = x_label, y = y_label, color = "Session") + 
    guides(fill="none") +
    theme(axis.title = element_text(size = 30),
          axis.text.x = element_text(size = 25),  # Adjust x-axis label text size
          axis.text.y = element_text(size = 25)) +  # Adjust y-axis label text size
    xlim(x_limits) +  # Set x-axis limits
    ylim(y_limits)    # Set y-axis limits
}


# Create plots for heroin
p1_h <- create_plot(inc_sneek_heroin, x_var = "beam", x_label = "Locomotion", y_label = "Lever Presses", x_limits = c(0, 50), y_limits = c(0, 10),
                    effects_beam_h_d1,
                    effects_beam_h_d30,
                    session_colors)


p2_h <- create_plot(inc_sneek_heroin, "groom", x_label = "Grooming", y_label = "", x_limits = c(0, 300), y_limits = c(-2, 10),
                    effects_groom_h_d1,
                    effects_groom_h_d30,
                    session_colors)

p3_h <- create_plot(inc_sneek_heroin, "sneek", x_label = "Sniffing", y_label = "", x_limits = c(0, 300), y_limits = c(0, 10),
                    effects_sneek_h_d1,
                    effects_sneek_h_d30,
                    session_colors)

p4_h <- create_plot(inc_sneek_heroin, "shake", x_label = "Shaking", y_label = "", x_limits = c(0, 15), y_limits = c(0, 10),
                    effects_shake_h_d1,
                    effects_shake_h_d30,
                    session_colors)

# Create plots for sucrose
p1_s <- create_plot(inc_sneek_sucrose, "beam", x_label = "Locomotion", y_label = "", x_limits = c(0, 45), y_limits = c(0, 10),
                    effects_beam_s_d1,
                    effects_beam_s_d30,
                    session_colors)

p2_s <- create_plot(inc_sneek_sucrose, "groom", x_label = "Grooming", y_label = "", x_limits = c(0, 300), y_limits = c(-2, 10),
                    effects_groom_s_d1,
                    effects_groom_s_d30,
                    session_colors)

p3_s <- create_plot(inc_sneek_sucrose, "sneek", x_label = "Sniffing", y_label = "", x_limits = c(0, 300), y_limits = c(0, 10),
                    effects_sneek_s_d1,
                    effects_sneek_s_d30,
                    session_colors)

p4_s <- create_plot(inc_sneek_sucrose, "shake", x_label = "Shaking", y_label = "", x_limits = c(0, 15), y_limits = c(0, 10),
                    effects_shake_s_d1,
                    effects_shake_s_d30,
                    session_colors)

# Combine the plots into grids
h_plots <- cowplot::plot_grid(p1_h, p2_h, p3_h, p4_h, nrow = 1)
s_plots <- cowplot::plot_grid(p1_s, p2_s, p3_s, p4_s, nrow = 1)

# Create final plot with "Day 1" and "Day 30" labels
final_plot <- cowplot::plot_grid(
  ggdraw() + draw_label("Heroin", size = 40, fontface = 'bold'),
  h_plots,
  ggdraw() + draw_label("Sucrose", size = 40, fontface = 'bold'),
  s_plots,
  ncol = 1,
  rel_heights = c(0.1, 1, 0.1, 1)
)

# Show the final plot
print(final_plot)

# Save the final plot as a .png file
# ggsave(filename = "lev_by_beh_by_session_combined.svg", 
#       plot = final_plot,   # Specify the plot object to save
 #      width = 20,          # Set the width of the plot (in inches)
  #     height = 10,         # Set the height of the plot (in inches)
   #    dpi = 300)           # Set the resolution of the plot (300 dpi for high-quality)