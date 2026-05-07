# Data: hoopR
# Inspiration: https://x.com/NekiasNBA/status/2048462364803313963
# libraries, packages, and themes ----
librarian::shelf(extrafont, ggimage, ggpath, ggplot2, gt, gtExtras, hoopR, lubridate, scales, tidyverse)
loadfonts(device = "win")
theme_shen3340 <- function() { 
  theme_minimal(base_size = 11, base_family = "Consolas") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}
# reading data ----
progressr::with_progress({
  pbp <- load_nba_pbp(seasons = 2020:2026)
})
schedule <- load_nba_schedule(seasons = 2020:2026) |>
  mutate(
    date = as.POSIXct(date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC"),
    date = with_tz(date, tzone = "America/New_York"),
    dateonly = as.Date(date),
    time = format(date, "%H:%M:%S")
  ) 

teams <- espn_nba_teams()

team_tz <- teams |>
  transmute(
    team_id = team_id, 
    timezone = case_when(
      team_id %in% c( 1, 2, 4, 5, 8, 11, 14, 15, 17, 18, 19, 20, 27, 28, 30) ~ "America/New_York",
      
      team_id %in% c(3, 6, 10, 16, 24, 25, 29) ~ "America/Chicago",
      
      team_id %in% c(7, 21, 26) ~ "America/Denver",
      
      team_id %in% c(9, 12, 13, 22, 23) ~ "America/Los_Angeles",
      
      TRUE ~ NA_character_
    )
  )

# cleaning  ----
schedule_filtered <- schedule |>
  filter(
    type_id >= 14,
    type_id <= 17,
    status_type_state == "post",
    dateonly > as.Date("2021-01-01")
  ) |> 
  mutate(
    time_hms = hms(time),
    minutes = hour(time_hms) * 60 + minute(time_hms),
    bucket = floor((minutes - 13 * 60) / 60) + 1,
    start_hour = (13 + (bucket - 1) - 1) %% 12 + 1,
    end_hour   = (13 + bucket - 1) %% 12 + 1,
    bucket_label = paste0(start_hour, "-", end_hour)
  ) |>
  arrange(bucket) |>
  mutate(
    bucket_label = factor(bucket_label, levels = unique(bucket_label))
  ) |>
  left_join(team_tz, by = c("home_id" = "team_id")) |> 
  select(id, dateonly, bucket_label, timezone)

team_meta <- teams |>
  select(team_id, logo)

nba_pbp_filt <- pbp |>
  inner_join(schedule_filtered, by = c("game_id" = "id")) |> 
  left_join(team_meta, by = "team_id") |> 
  filter(shooting_play == TRUE) |>
  mutate(
    distance = str_extract(text, "\\d+(?=-foot)"),
    distance = as.numeric(distance),
    is_three = str_detect(text, regex("three", ignore_case = TRUE)),
    is_three =  (is_three | distance >= 22) & score_value != 2
  ) |>
  filter(is_three)

# Overall 3p by tipoff time ----
three_pt_by_bucket <- nba_pbp_filt |>
  group_by(bucket_label) |>
  summarise(
    attempts = n(),
    makes = sum(scoring_play, na.rm = TRUE),
    pct = makes / attempts
  ) |>
  arrange(bucket_label)

league_avg <- nba_pbp_filt |>
  summarise(
    makes = sum(scoring_play, na.rm = TRUE),
    attempts = n(),
    pct = makes / attempts
  ) |>
  pull(pct)


three_pt_by_bucket_plot <- ggplot(three_pt_by_bucket, aes(x = bucket_label, y = pct)) +
  geom_point(aes(size = attempts)) +
  scale_size(range = c(2, 6)) + 
  theme_shen3340() + 
  geom_hline(yintercept = league_avg, linetype = "dashed") +
  annotate("text", x = 3, y = league_avg + .005, label = "NBA Playoff 3P%\nLeague Average") + 
  scale_y_continuous(labels = label_percent()) + 
  labs(title = "3P% by Tipoff Time ", 
       subtitle = "NBA Playoff Games (2021-2026)", size = "Attempts",
       x = "Game Tipoff Time (PM EST)", y = "3P%", caption = "Data: hoopR\nInspo: Owen Phillips") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

three_pt_by_bucket_plot

ggsave("3_pct_by_game_time.png", three_pt_by_bucket_plot, width = 6, height = 6, dpi = 600)

# by team plot ----
team_tz_bucket <- nba_pbp_filt |>
  group_by(timezone, bucket_label, logo) |>
  summarise(
    attempts = n(),
    makes = sum(scoring_play, na.rm = TRUE),
    pct = makes / attempts,
    .groups = "drop"
  )

plot_tz <- function(tz_name, subtitle, x, bump) {
  
  df <- team_tz_bucket |>
    filter(timezone == tz_name) |> 
    mutate(
      attempts_scaled = rescale(attempts, to = c(0.05, 0.15)) 
      )
      
  
  league_avg_tz <- df |>
    summarise(
      makes = sum(makes, na.rm = TRUE),
      attempts = sum(attempts, na.rm = TRUE),
      pct = makes / attempts
    ) |>
    pull(pct)
  
  ggplot(df, aes(x = bucket_label, y = pct, group = logo)) +
    geom_from_path(aes(path = logo, width = attempts_scaled, height = attempts_scaled)) + 
    theme_shen3340() +
    geom_hline(yintercept = league_avg_tz, linetype = "dashed") +
    annotate("text", x = x, y = league_avg_tz + bump, label = paste0("NBA AVG\nin ", subtitle)) + 
    scale_y_continuous(labels = label_percent()) +
    labs(
      title = "3P% by Tipoff Time",
      subtitle = paste0("NBA Playoff Games (2021-26) Played in ", subtitle),
      x = "Tipoff Time (PM EST)",
      y = "3P%",
      caption = "Data: hoopR | Inspo: Owen Phillips\nLogo size proportional to attempts"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_et <- plot_tz(tz_name = "America/New_York", subtitle = "EST", x = 4, bump = 0.02)
plot_et
ggsave("ET.png", plot_et, width = 6, height = 6, dpi = 600)

plot_ct <- plot_tz(tz_name = "America/Chicago", subtitle = "CT", x = 6, bump = 0.02)
plot_ct
ggsave("CT.png", plot_ct, width = 6, height = 6, dpi = 600)

plot_mt <- plot_tz(tz_name = "America/Denver", subtitle = "MT", x = 2, bump = 0.02)
plot_mt
ggsave("MT.png", plot_mt, width = 6, height = 6, dpi = 600)

plot_pt <- plot_tz(tz_name = "America/Los_Angeles", subtitle = "PT", x = 3, bump = 0.02)
plot_pt
ggsave("PT.png", plot_pt, width = 6, height = 6, dpi = 600)