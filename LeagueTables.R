library(worldfootballR)
library(tidyverse)
library(janitor)
library(ggrepel)
library(plotly)

big5_2024_shooting <- fb_big5_advanced_season_stats(season_end_year = 2024, stat_type = "shooting", team_or_player = "team") %>% 
  clean_names(parsing_option = 3)

epl_2024_shooting <- big5_2024_shooting %>% 
  filter(comp == "Premier League")

epl_2024_shooting <- epl_2024_shooting %>% 
  mutate(npxg_per_sh = npx_g_expected / sh_standard,
         xg_per_sh = x_g_expected / sh_standard)

shooting_defense_plot <- join %>% 
  filter(team_or_opponent == "opponent") %>% 
  ggplot(aes(npxg_per_sh, sh_per_90_standard, label = squad, size = gls_per_90)) +
  geom_point() +
  geom_hline(yintercept = median(epl_2024_shooting$sh_per_90_standard)) +
  geom_vline(xintercept = median(epl_2024_shooting$npxg_per_sh)) +
  labs(title = "Non-penalty xG faced per Shot by Shots faced per Game", x = "npxG per Shot", y = "Shots per 90") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_label_repel() +
  theme_classic()

print(shooting_defense_plot)

shooting_defense_plot_xG <- join %>% 
  filter(team_or_opponent == "opponent") %>% 
  ggplot(aes(xg_per_sh, sh_per_90_standard, label = squad, size = gls_per_90)) +
  geom_point() +
  geom_hline(yintercept = median(epl_2024_shooting$sh_per_90_standard)) +
  geom_vline(xintercept = median(epl_2024_shooting$xg_per_sh)) +
  labs(title = "xG faced per Shot by Shots faced per Game", x = "xG per Shot", y = "Shots per 90") +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_label_repel() +
  theme_classic()

print(shooting_defense_plot_xG)

shooting_offense_plot <- join %>% 
  filter(team_or_opponent == "team") %>% 
  ggplot(aes(xg_per_sh, sh_per_90_standard, label = squad, size = gls_per_90)) +
  geom_point() +
  geom_hline(yintercept = median(epl_2024_shooting$sh_per_90_standard)) +
  geom_vline(xintercept = median(epl_2024_shooting$xg_per_sh)) +
  labs(title = "xG per Shot by Shots per Game", x = "xG per Shot", y = "Shots per 90") +
  geom_label_repel() +
  theme_classic()

print(shooting_offense_plot)

big5_2024_possession <- fb_big5_advanced_season_stats(season_end_year = 2024, stat_type = "possession", team_or_player = "team") %>% 
  clean_names(parsing_option = 3)

epl_2024_possession <- big5_2024_possession %>% 
  filter(comp == "Premier League")

epl_2024_possession <- epl_2024_possession %>% 
  mutate(att_3rd_per_90 =  att_3rd_touches / 90)

possession_offensive_plot <- epl_2024_possession %>%
  filter(team_or_opponent == "team") %>% 
  ggplot(aes(poss, att_3rd_per_90, label = squad)) +
  geom_point() +
  geom_hline(yintercept = median(epl_2024_possession$att_3rd_per_90)) +
  geom_vline(xintercept = median(epl_2024_possession$poss)) +
  geom_label_repel() +
  theme_classic()

print(possession_offensive_plot)

join <- epl_2024_shooting %>% 
  left_join(epl_2024_possession, by = c("squad", "team_or_opponent")) %>% 
  mutate(gls_per_90 = gls_standard / 90)

possession_defensive_plot <- join %>%
  filter(team_or_opponent == "opponent") %>% 
  ggplot(aes(poss, att_3rd_per_90, label = squad, size = gls_per_90)) +
  geom_point() +
  geom_hline(yintercept = median(epl_2024_possession$att_3rd_per_90)) +
  geom_vline(xintercept = median(epl_2024_possession$poss)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_label_repel() +
  theme_classic()

print(possession_defensive_plot)

xg_plotly <- join %>% 
  filter(team_or_opponent == "opponent") %>% 
  plot_ly(x=~xg_per_sh, y=~sh_per_90_standard, z=~gls_per_90, hovertext = ~squad, type="scatter3d", mode="markers")

print(xg_plotly)
