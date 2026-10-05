### do these relationships change in early vs late abstinence?

# grooming


lev_groom_sess_heroin <- lmer(lever ~ groom*session + (1|subject),data = inc_sneek_heroin)
summary(lev_groom_sess_heroin)
simple_slopes(lev_groom_sess_heroin)

# grooming goes down when lever pressing goes up (same across drug)
# lever pressing goes up across session (same across drug)
# significant negative interaction (different across drug)

lev_groom_sess_sucrose <- lmer(lever ~ groom*session + (1|subject),data = inc_sneek_sucrose)
summary(lev_groom_sess_sucrose)

# grooming goes down when lever pressing goes up (same across drug)
# lever pressing goes up across session (same across drug)
# no interaction (different across drug)

lev_groom_sess_3way <- lmer(lever ~ groom*session*drug + (1|subject),data = inc_sneek)
summary(lev_groom_sess_3way)

# no three-way interaction

# sniffing


lev_sniff_sess_heroin <- lmer(lever ~ sneek*session + (1|subject),data = inc_sneek_heroin)
summary(lev_sniff_sess_heroin)

# sniffing goes up when lever pressing goes up (different across drug)
# lever pressing goes up across session (same across drug)
# no interaction (different across drug)

lev_sniff_sess_sucrose <- lmer(lever ~ sneek*session + (1|subject),data = inc_sneek_sucrose)
summary(lev_sniff_sess_sucrose)
simple_slopes(lev_sniff_sess_sucrose)

# sniffing goes down when lever pressing goes up (different across drug)
# lever pressing goes up across session (same across drug)
# significant negative interaction (different across drug)

lev_sniff_sess_3way <- lmer(lever ~ sneek*session*drug + (1|subject),data = inc_sneek)
summary(lev_sniff_sess_3way)

# no three-way interaction

lev_beam_sess_heroin <- lmer(lever ~ beam*session + (1|subject),data = inc_sneek_heroin)
summary(lev_beam_sess_heroin)
simple_slopes(lev_beam_sess_heroin)

# locomotion goes up when lever pressing goes up (same across drug)
# lever pressing goes up across session (same across drug)
# significant negative interaction (different across drug)

lev_beam_sess_sucrose <- lmer(lever ~ beam*session + (1|subject),data = inc_sneek_sucrose)
summary(lev_beam_sess_sucrose)

# locomotion goes down when lever pressing goes up (same across drug)
# lever pressing goes up across session (same across drug)
# no interaction (different across drug)

lev_beam_sess_3way <- lmer(lever ~ beam*session*drug + (1|subject),data = inc_sneek)
summary(lev_beam_sess_3way)

# no three-way interaction

lev_shake_sess_heroin <- lmer(lever ~ shake*session + (1|subject),data = inc_sneek_heroin)
summary(lev_shake_sess_heroin)

# no relationship bw shaking and lever (different across drug)
# lever pressing goes up across session (same across drug)
# no interaction (same across drug)

lev_shake_sess_sucrose <- lmer(lever ~ shake*session + (1|subject),data = inc_sneek_sucrose)
summary(lev_shake_sess_sucrose)

# shaking goes down when lever pressing goes up (different across drug)
# lever pressing goes up across session (same across drug)
# no interaction (same across drug)

# prepare data for plots

library(effects)

# get effect size estimates from model
effects_groom_h <- as.data.frame(effect(term = "groom:session", mod = lev_groom_sess_heroin))
effects_groom_h %>% filter(session == "Day 1") -> effects_groom_h_d1 # split the matrix by session to plot regression lines for both sessions
effects_groom_h %>% filter(session == "Day 30") -> effects_groom_h_d30

effects_sneek_h <- as.data.frame(effect(term = "sneek:session", mod = lev_sniff_sess_heroin))
effects_sneek_h %>% filter(session == "Day 1") -> effects_sneek_h_d1
effects_sneek_h %>% filter(session == "Day 30") -> effects_sneek_h_d30

effects_shake_h <- as.data.frame(effect(term = "shake:session", mod = lev_shake_sess_heroin))
effects_shake_h %>% filter(session == "Day 1") -> effects_shake_h_d1
effects_shake_h %>% filter(session == "Day 30") -> effects_shake_h_d30

effects_beam_h <- as.data.frame(effect(term = "beam:session", mod = lev_beam_sess_heroin))
effects_beam_h %>% filter(session == "Day 1") -> effects_beam_h_d1
effects_beam_h %>% filter(session == "Day 30") -> effects_beam_h_d30

# get effect size estimates from model
effects_groom_s <- as.data.frame(effect(term = "groom:session", mod = lev_groom_sess_sucrose))
effects_groom_s %>% filter(session == "Day 1") -> effects_groom_s_d1
effects_groom_s %>% filter(session == "Day 30") -> effects_groom_s_d30

effects_sneek_s <- as.data.frame(effect(term = "sneek:session", mod = lev_sniff_sess_sucrose))
effects_sneek_s %>% filter(session == "Day 1") -> effects_sneek_s_d1
effects_sneek_s %>% filter(session == "Day 30") -> effects_sneek_s_d30

effects_shake_s <- as.data.frame(effect(term = "shake:session", mod = lev_shake_sess_sucrose))
effects_shake_s %>% filter(session == "Day 1") -> effects_shake_s_d1
effects_shake_s %>% filter(session == "Day 30") -> effects_shake_s_d30

effects_beam_s <- as.data.frame(effect(term = "beam:session", mod = lev_beam_sess_sucrose))
effects_beam_s %>% filter(session == "Day 1") -> effects_beam_s_d1
effects_beam_s %>% filter(session == "Day 30") -> effects_beam_s_d30

