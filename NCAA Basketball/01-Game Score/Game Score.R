# Inspiration: https://x.com/owenlhjphillips/status/1871639809564999712
# libraries and packages ----
librarian::shelf(data.table, gt, gtExtras, hoopR, tidyverse, wehoop)

# Generating top games ----
generate_top_games <- function(type, season, days, title_prefix) {
  
  # Load appropriate player and team boxes
  player_box <- if (type == "mbb") load_mbb_player_box(seasons = season) else load_wbb_player_box(seasons = season)
  teams <- if (type == "mbb") espn_mbb_teams(year = season) else espn_wbb_teams(year = season)
  
  # Filter for high majors (update this list if needed)
  high_majors <- teams |> 
    filter(conference_short_name %in% c("Big Ten", "SEC", "ACC", "Big 12", "Big East"))
  
  # Process player box
  player_box <- player_box |> 
    select(game_id, team_id, game_date, athlete_display_name, minutes, field_goals_made,
           field_goals_attempted, three_point_field_goals_made, three_point_field_goals_attempted,
           free_throws_made, free_throws_attempted, rebounds, assists, steals, blocks,
           turnovers, fouls, offensive_rebounds, defensive_rebounds, points, team_winner,
           home_away, opponent_team_location, team_logo) |> 
    filter(minutes >= 2, game_date >= Sys.Date() - days & game_date <= Sys.Date()) |> 
    mutate(game_score = round(points + 
                                .4 * field_goals_made - 
                                .7 * field_goals_attempted -
                                .4 * (free_throws_attempted - free_throws_made) + 
                                .7 * offensive_rebounds + 
                                .3 * defensive_rebounds + 
                                steals + 
                                .7 * assists +
                                .7 * blocks -
                                .4 * fouls -
                                turnovers, 1)) |> 
    mutate(ts = round(points / (2 * (field_goals_attempted + .44 * free_throws_attempted)), 3)) |> 
    mutate(ts = if_else(is.na(ts), 0, ts)) |> 
    mutate(FG = paste(field_goals_made, field_goals_attempted, sep = "/"),
           `3PT` = paste(three_point_field_goals_made, three_point_field_goals_attempted, sep = "/"),
           FT = paste(free_throws_made, free_throws_attempted, sep = "/"),
           team_winner = if_else(team_winner == "TRUE", "W", "L"),
           game_info = if_else(home_away == "home", 
                               paste("vs. ", opponent_team_location, " (", team_winner, ")", sep = ""),
                               paste("@ ", opponent_team_location, " (", team_winner, ")", sep = "")))
  
  # Load team box
  team_box <- if (type == "mbb") load_mbb_team_box(seasons = season) else load_wbb_team_box(seasons = season)
  
  team_box <- team_box |> 
    select(game_date, game_id, team_id, field_goals_attempted, free_throws_attempted, turnovers, opponent_team_id) |> 
    filter(game_date >= Sys.Date() - days & game_date <= Sys.Date())
  
  # Combine boxes
  player_box <- as.data.table(player_box)
  team_box <- as.data.table(team_box)
  
  combined_box <- merge(player_box, team_box, by = c("game_id", "team_id", "game_date")) |> 
    filter(opponent_team_id %in% high_majors$team_id & team_id %in% high_majors$team_id)
  
  # conference only ----
  #teams <- teams |> 
  #filter(conference_short_name == "Big Ten")
  # the rest---- 
  
  combined_box <- copy(combined_box)
  combined_box[, USG := 100 * (field_goals_attempted.x + 0.475 * free_throws_attempted.x + turnovers.x) /
                 (field_goals_attempted.y + 0.475 * free_throws_attempted.y + turnovers.y)]
  combined_box <- combined_box |> 
    mutate(ts = ts * 100) |>
    filter(team_id %in% teams$team_id) |> 
    select(athlete_display_name, minutes, USG, ts, points, rebounds, assists, turnovers.x,
           steals, blocks, fouls, FG, `3PT`, FT, game_score, game_info, team_logo) |> 
    arrange(desc(game_score)) |> 
    head(10)
  
  # Generate table
  combined_box %>%
    gt() %>%
    cols_hide(game_info) |> 
    fmt_number(
      columns = c(ts, `USG`),
      decimals = 1
    ) %>%
    cols_label(
      athlete_display_name = "",
      team_logo = "",
      minutes = "MP",
      USG = "USG%",
      ts = "TS%",
      points = "PTS",
      rebounds = "REB",
      assists = "AST",
      turnovers.x = "TOV",
      steals = "STL",
      blocks = "BLK",
      fouls = "PF",
      game_score = "GS"
    ) |> 
    tab_header(
      title = paste0(title_prefix, " Top 10 Games by Game Score (GS)"),
      subtitle = html(
        paste(
          "From ", Sys.Date() - days, " to ", Sys.Date(), 
          " <span style='float: right;'><img src='https://upload.wikimedia.org/wikipedia/commons/6/6f/Logo_of_Twitter.svg' height='20px'/> @wesean4</span>"
        )
      )
    ) |> 
    text_transform(
      locations = cells_body(columns = "athlete_display_name"),
      fn = function(x) {
        game_info <- combined_box$game_info
        game_info_colored <- gsub("\\(W\\)", "<span style='color:green'>(W)</span>", game_info)
        game_info_colored <- gsub("\\(L\\)", "<span style='color:red'>(L)</span>", game_info_colored)
        paste(x, "<br>", game_info_colored)
      }
    ) |> 
    gt_img_rows(team_logo, height = 25) |> 
    cols_move_to_start(team_logo) |> 
    gt_theme_guardian() |> 
    gtsave(paste0(title_prefix, " Top 10 GS ", Sys.Date() - days, " thru ", Sys.Date(), ".png"), expand = 10)
}

# Usage ----
generate_top_games("mbb", 2025, 54, "MBB")
generate_top_games("wbb", 2025, 54, "WBB")
