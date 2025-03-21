# Richmond finish 16th in the 2016 AFL season and life was looking dim for coach Damien Hardwick. In what was one of the
# biggest turn arounds in AFL history, Richmond went from a club notorious of failure and finishing 9th to perhaps the best
# 4 year stretch of all time winning flags in 2017,2019 and 2020 with the help of arguably the best player of all time
# and without a doubt the best big game player of all time, Brownlow winner and 3 time Norm Smith Medalist, Dustin Martin.
# Today we will look at all games from that 4 year period to identify what sort of play style they had. We
# will then look to see which clubs are attempting to replicate it and if they are doing so with success or failure and why.
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
########
# Get Richmond Results
########
seasons <- 2017:2020
afl_results_list <- list()
for (season in seasons) {
  results <- fetch_results(season = season, source = "AFL", comp = "AFLM")
  afl_results_list[[as.character(season)]] <- results
}
afl_results <- do.call(rbind, afl_results_list)
colnames(afl_results)
# 2020 was a covid year, meaning games were affected in length and so was the season
richmond_results <- afl_results %>%
  mutate(
    date = as.Date(match.date, format = '%d/%m/%y'),
    season = year(date),
    round = round.roundNumber,
    match_id = match.matchId,
    venue = venue.name,
    home_team = match.homeTeam.name,
    home_id = match.homeTeamId,
    home_goals = homeTeamScore.matchScore.goals,
    home_behinds = homeTeamScore.matchScore.behinds,
    home_total = homeTeamScore.matchScore.totalScore,
    away_team = match.awayTeam.name,
    away_id = match.awayTeamId,
    away_goals = awayTeamScore.matchScore.goals,
    away_behinds = awayTeamScore.matchScore.behinds,
    away_total = awayTeamScore.matchScore.totalScore,
    home_win = home_total > away_total
  ) %>%
  select(
    date, season, round, match_id, venue,
    home_team, home_id, home_goals, home_behinds, home_total,
    away_team, away_id, away_goals, away_behinds, away_total,
    home_win
  ) %>% filter(
    home_team == 'Richmond' | away_team == 'Richmond' 
  )
###########
# Analysis of Richmond's Performance
###########
richmond_long <- richmond_results %>%
  mutate(
    richmond_is_home = home_team == "Richmond",
    richmond_goals = ifelse(richmond_is_home, home_goals, away_goals),
    richmond_behinds = ifelse(richmond_is_home, home_behinds, away_behinds),
    richmond_total = ifelse(richmond_is_home, home_total, away_total),
    opponent_goals = ifelse(richmond_is_home, away_goals, home_goals),
    opponent_behinds = ifelse(richmond_is_home, away_behinds, home_behinds),
    opponent_total = ifelse(richmond_is_home, away_total, home_total),
    richmond_win = (richmond_is_home & home_win) | (!richmond_is_home & !home_win),
    margin = richmond_total - opponent_total,
    game_type = ifelse(as.numeric(gsub("R", "", round)) > 24 | grepl("F", round), "Finals", "Regular Season")
  )

# Season-by-season summary
season_summary <- richmond_long %>%
  group_by(season) %>%
  summarize(
    games = n(),
    wins = sum(richmond_win),
    losses = sum(!richmond_win),
    win_pct = wins/games,
    avg_score_for = mean(richmond_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin),
    positive_margins = sum(margin > 0),
    blowouts = sum(margin > 40) # We use 40 as the bookies define big wins as 40+
  )
season_summary

finals_summary <- richmond_long %>%
  filter(game_type == "Finals") %>%
  group_by(season) %>%
  summarize(
    games = n(),
    wins = sum(richmond_win),
    losses = sum(!richmond_win),
    win_pct = wins/games,
    avg_score_for = mean(richmond_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin)
  )
finals_summary
# We are missing 2020 here so we should go over the data and check 

overall_summary <- richmond_long %>%
  summarize(
    games = n(),
    wins = sum(richmond_win),
    losses = sum(!richmond_win),
    win_pct = wins/games,
    avg_score_for = mean(richmond_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin),
    regular_season_win_pct = sum(richmond_win & game_type == "Regular Season")/sum(game_type == "Regular Season"),
    finals_win_pct = sum(richmond_win & game_type == "Finals")/sum(game_type == "Finals")
  )
overall_summary

# Insert graph to show year on year performance

scoring_patterns <- richmond_long %>%
  group_by(season) %>%
  summarize(
    avg_goals = mean(richmond_goals),
    avg_behinds = mean(richmond_behinds),
    goal_accuracy = sum(richmond_goals) / (sum(richmond_goals) + sum(richmond_behinds)),
    scoring_efficiency = sum(richmond_total) / sum(opponent_total)
  )
scoring_patterns

# Insert graph to show above

venue_performance <- richmond_long %>%
  group_by(venue) %>%
  summarize(
    games = n(),
    wins = sum(richmond_win),
    win_pct = wins/games,
    avg_margin = mean(margin)
  ) %>%
  arrange(desc(games))
venue_performance

close_games <- richmond_long %>%
  mutate(game_closeness = case_when(
    abs(margin) <= 12 ~ "Very Close",
    abs(margin) <= 24 ~ "Close",
    abs(margin) <= 36 ~ "Moderate",
    TRUE ~ "Blowout"
  )) %>%
  group_by(game_closeness) %>%
  summarize(
    games = n(),
    richmond_wins = sum(richmond_win),
    win_pct = richmond_wins/games
  )
close_games

opponent_performance <- richmond_long %>%
  mutate(opponent = ifelse(richmond_is_home, away_team, home_team)) %>%
  group_by(opponent) %>%
  summarize(
    games = n(),
    wins = sum(richmond_win),
    win_pct = wins/games,
    avg_margin = mean(margin)
  ) %>%
  arrange(desc(win_pct))
opponent_performance

# Insert a graph of the above

season_quarters <- richmond_long %>%
  mutate(
    round_num = as.numeric(gsub("[^0-9]", "", round)),
    season_quarter = case_when(
      round_num <= 6 ~ "Early",
      round_num <= 12 ~ "Mid-Early",
      round_num <= 18 ~ "Mid-Late",
      round_num <= 24 ~ "Late",
      TRUE ~ "Finals"
    )
  ) %>%
  group_by(season_quarter) %>%
  summarize(
    games = n(),
    wins = sum(richmond_win),
    win_pct = wins/games,
    avg_margin = mean(margin)
  )
season_quarters

# Insert a graph of the above

###########
# Gather Richmond's Team Stats 
###########
seasons <- 2017:2020
afl_player_stats_list <- list()
for (season in seasons) {
  results <- fetch_player_stats(season = season, source = "AFL", comp = "AFLM")
  afl_player_stats_list[[as.character(season)]] <- results
}
afl_player_stats <- do.call(rbind, afl_player_stats_list)
afl_player_stats <- afl_player_stats %>% 
  mutate(
    date = as.Date(utcStartTime),
    season =year(date)
  )
colnames(afl_player_stats)

richmond_player_stats <- afl_player_stats %>%
  filter(team.name == "Richmond") %>%
  mutate(
    game_type = ifelse(as.numeric(gsub("R", "", round.roundNumber)) > 24 | 
                         grepl("F", round.roundNumber), "Finals", "Regular Season")
  ) %>%
  select(
    season, 
    date,
    round = round.roundNumber,
    game_type,
    player_name = player.surname,
    kicks, handballs, disposals, marks, tackles, 
    contested_possessions = contestedPossessions,
    uncontested_possessions = uncontestedPossessions,
    inside50s, rebound50s, clearances = clearances.totalClearances,
    metres_gained = metresGained,
    score_involvements = scoreInvolvements,
    turnovers, intercepts, tackles_inside50 = tacklesInside50,
    pressure_acts = extendedStats.pressureActs,
    ground_ball_gets = extendedStats.groundBallGets,
    contested_marks = contestedMarks,
    one_percenters = onePercenters,
    disposal_efficiency = disposalEfficiency,
    clangers
  )

richmond_team_stats <- richmond_player_stats %>%
  group_by(season, round) %>%
  summarize(
    kicks = sum(kicks, na.rm = TRUE),
    handballs = sum(handballs, na.rm = TRUE),
    disposals = sum(disposals, na.rm = TRUE),
    marks = sum(marks, na.rm = TRUE),
    tackles = sum(tackles, na.rm = TRUE),
    contested_possessions = sum(contested_possessions, na.rm = TRUE),
    uncontested_possessions = sum(uncontested_possessions, na.rm = TRUE),
    inside50s = sum(inside50s, na.rm = TRUE),
    rebound50s = sum(rebound50s, na.rm = TRUE),
    clearances = sum(clearances, na.rm = TRUE),
    metres_gained = sum(metres_gained, na.rm = TRUE),
    score_involvements = sum(score_involvements, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    intercepts = sum(intercepts, na.rm = TRUE),
    tackles_inside50 = sum(tackles_inside50, na.rm = TRUE),
    pressure_acts = sum(pressure_acts, na.rm = TRUE),
    ground_ball_gets = sum(ground_ball_gets, na.rm = TRUE),
    contested_marks = sum(contested_marks, na.rm = TRUE),
    one_percenters = sum(one_percenters, na.rm = TRUE),
    disposal_efficiency = mean(disposal_efficiency, na.rm = TRUE),
    clangers = sum(clangers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

richmond_yearly_averages <- richmond_team_stats %>%
  group_by(season) %>%
  summarize(
    games = n(),
    kicks_avg = mean(kicks),
    handballs_avg = mean(handballs),
    disposals_avg = mean(disposals),
    marks_avg = mean(marks),
    tackles_avg = mean(tackles),
    contested_possessions_avg = mean(contested_possessions),
    uncontested_possessions_avg = mean(uncontested_possessions),
    inside50s_avg = mean(inside50s),
    rebound50s_avg = mean(rebound50s),
    clearances_avg = mean(clearances),
    metres_gained_avg = mean(metres_gained),
    score_involvements_avg = mean(score_involvements),
    turnovers_avg = mean(turnovers),
    intercepts_avg = mean(intercepts),
    tackles_inside50_avg = mean(tackles_inside50),
    pressure_acts_avg = mean(pressure_acts),
    ground_ball_gets_avg = mean(ground_ball_gets),
    contested_marks_avg = mean(contested_marks),
    one_percenters_avg = mean(one_percenters),
    disposal_efficiency_avg = mean(disposal_efficiency),
    clangers_avg = mean(clangers),
    contested_uncontested_ratio = mean(contested_possessions)/mean(uncontested_possessions),
    kick_handball_ratio = mean(kicks)/mean(handballs)
  )

richmond_game_type_averages <- richmond_player_stats %>%
  group_by(season, game_type, round) %>%
  summarize(
    kicks = sum(kicks, na.rm = TRUE),
    handballs = sum(handballs, na.rm = TRUE),
    disposals = sum(disposals, na.rm = TRUE),
    marks = sum(marks, na.rm = TRUE),
    tackles = sum(tackles, na.rm = TRUE),
    contested_possessions = sum(contested_possessions, na.rm = TRUE),
    uncontested_possessions = sum(uncontested_possessions, na.rm = TRUE),
    inside50s = sum(inside50s, na.rm = TRUE),
    rebound50s = sum(rebound50s, na.rm = TRUE),
    clearances = sum(clearances, na.rm = TRUE),
    metres_gained = sum(metres_gained, na.rm = TRUE),
    score_involvements = sum(score_involvements, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    intercepts = sum(intercepts, na.rm = TRUE),
    tackles_inside50 = sum(tackles_inside50, na.rm = TRUE),
    pressure_acts = sum(pressure_acts, na.rm = TRUE),
    ground_ball_gets = sum(ground_ball_gets, na.rm = TRUE),
    contested_marks = sum(contested_marks, na.rm = TRUE),
    one_percenters = sum(one_percenters, na.rm = TRUE),
    disposal_efficiency = mean(disposal_efficiency, na.rm = TRUE),
    clangers = sum(clangers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(season, game_type) %>%
  summarize(
    games = n(),
    kicks_avg = mean(kicks),
    handballs_avg = mean(handballs),
    disposals_avg = mean(disposals),
    marks_avg = mean(marks),
    tackles_avg = mean(tackles),
    contested_possessions_avg = mean(contested_possessions),
    uncontested_possessions_avg = mean(uncontested_possessions),
    inside50s_avg = mean(inside50s),
    rebound50s_avg = mean(rebound50s),
    clearances_avg = mean(clearances),
    metres_gained_avg = mean(metres_gained),
    score_involvements_avg = mean(score_involvements),
    turnovers_avg = mean(turnovers),
    intercepts_avg = mean(intercepts),
    tackles_inside50_avg = mean(tackles_inside50),
    pressure_acts_avg = mean(pressure_acts),
    ground_ball_gets_avg = mean(ground_ball_gets),
    contested_marks_avg = mean(contested_marks),
    one_percenters_avg = mean(one_percenters),
    disposal_efficiency_avg = mean(disposal_efficiency),
    clangers_avg = mean(clangers),
    contested_uncontested_ratio = mean(contested_possessions)/mean(uncontested_possessions),
    kick_handball_ratio = mean(kicks)/mean(handballs),
    .groups = "drop"
  )

league_averages <- afl_player_stats %>%
  group_by(season, round.roundNumber) %>%
  summarize(
    kicks = sum(kicks, na.rm = TRUE)/2,
    handballs = sum(handballs, na.rm = TRUE)/2,
    disposals = sum(disposals, na.rm = TRUE)/2,
    marks = sum(marks, na.rm = TRUE)/2,
    tackles = sum(tackles, na.rm = TRUE)/2,
    contested_possessions = sum(contestedPossessions, na.rm = TRUE)/2,
    uncontested_possessions = sum(uncontestedPossessions, na.rm = TRUE)/2,
    inside50s = sum(inside50s, na.rm = TRUE)/2,
    rebound50s = sum(rebound50s, na.rm = TRUE)/2,
    clearances = sum(clearances.totalClearances, na.rm = TRUE)/2,
    metres_gained = sum(metresGained, na.rm = TRUE)/2,
    score_involvements = sum(scoreInvolvements, na.rm = TRUE)/2,
    turnovers = sum(turnovers, na.rm = TRUE)/2,
    intercepts = sum(intercepts, na.rm = TRUE)/2,
    tackles_inside50 = sum(tacklesInside50, na.rm = TRUE)/2,
    pressure_acts = sum(extendedStats.pressureActs, na.rm = TRUE)/2,
    ground_ball_gets = sum(extendedStats.groundBallGets, na.rm = TRUE)/2,
    contested_marks = sum(contestedMarks, na.rm = TRUE)/2,
    disposal_efficiency = mean(disposalEfficiency, na.rm = TRUE),
    clangers = sum(clangers, na.rm = TRUE)/2,
    .groups = "drop"
  ) %>%
  rename(round = round.roundNumber)

league_yearly_averages <- league_averages %>%
  group_by(season) %>%
  summarize(
    kicks_avg = mean(kicks),
    handballs_avg = mean(handballs),
    disposals_avg = mean(disposals),
    marks_avg = mean(marks),
    tackles_avg = mean(tackles),
    contested_possessions_avg = mean(contested_possessions),
    uncontested_possessions_avg = mean(uncontested_possessions),
    inside50s_avg = mean(inside50s),
    rebound50s_avg = mean(rebound50s),
    clearances_avg = mean(clearances),
    metres_gained_avg = mean(metres_gained),
    score_involvements_avg = mean(score_involvements),
    turnovers_avg = mean(turnovers),
    intercepts_avg = mean(intercepts),
    tackles_inside50_avg = mean(tackles_inside50),
    pressure_acts_avg = mean(pressure_acts),
    ground_ball_gets_avg = mean(ground_ball_gets),
    contested_marks_avg = mean(contested_marks),
    disposal_efficiency_avg = mean(disposal_efficiency),
    clangers_avg = mean(clangers),
    contested_uncontested_ratio = mean(contested_possessions)/mean(uncontested_possessions),
    kick_handball_ratio = mean(kicks)/mean(handballs)
  )

richmond_vs_league <- richmond_yearly_averages %>%
  left_join(league_yearly_averages, by = "season", suffix = c("_rich", "_league")) %>%
  mutate(
    kicks_diff_pct = (kicks_avg_rich - kicks_avg_league) / kicks_avg_league * 100,
    handballs_diff_pct = (handballs_avg_rich - handballs_avg_league) / handballs_avg_league * 100,
    disposals_diff_pct = (disposals_avg_rich - disposals_avg_league) / disposals_avg_league * 100,
    marks_diff_pct = (marks_avg_rich - marks_avg_league) / marks_avg_league * 100,
    tackles_diff_pct = (tackles_avg_rich - tackles_avg_league) / tackles_avg_league * 100,
    contested_possessions_diff_pct = (contested_possessions_avg_rich - contested_possessions_avg_league) / contested_possessions_avg_league * 100,
    uncontested_possessions_diff_pct = (uncontested_possessions_avg_rich - uncontested_possessions_avg_league) / uncontested_possessions_avg_league * 100,
    inside50s_diff_pct = (inside50s_avg_rich - inside50s_avg_league) / inside50s_avg_league * 100,
    rebound50s_diff_pct = (rebound50s_avg_rich - rebound50s_avg_league) / rebound50s_avg_league * 100,
    clearances_diff_pct = (clearances_avg_rich - clearances_avg_league) / clearances_avg_league * 100,
    metres_gained_diff_pct = (metres_gained_avg_rich - metres_gained_avg_league) / metres_gained_avg_league * 100,
    score_involvements_diff_pct = (score_involvements_avg_rich - score_involvements_avg_league) / score_involvements_avg_league * 100,
    turnovers_diff_pct = (turnovers_avg_rich - turnovers_avg_league) / turnovers_avg_league * 100,
    intercepts_diff_pct = (intercepts_avg_rich - intercepts_avg_league) / intercepts_avg_league * 100,
    tackles_inside50_diff_pct = (tackles_inside50_avg_rich - tackles_inside50_avg_league) / tackles_inside50_avg_league * 100,
    pressure_acts_diff_pct = (pressure_acts_avg_rich - pressure_acts_avg_league) / pressure_acts_avg_league * 100,
    ground_ball_gets_diff_pct = (ground_ball_gets_avg_rich - ground_ball_gets_avg_league) / ground_ball_gets_avg_league * 100,
    contested_marks_diff_pct = (contested_marks_avg_rich - contested_marks_avg_league) / contested_marks_avg_league * 100,
    disposal_efficiency_diff_pct = (disposal_efficiency_avg_rich - disposal_efficiency_avg_league) / disposal_efficiency_avg_league * 100,
    clangers_diff_pct = (clangers_avg_rich - clangers_avg_league) / clangers_avg_league * 100,
    contested_uncontested_ratio_diff_pct = (contested_uncontested_ratio_rich - contested_uncontested_ratio_league) / contested_uncontested_ratio_league * 100,
    kick_handball_ratio_diff_pct = (kick_handball_ratio_rich - kick_handball_ratio_league) / kick_handball_ratio_league * 100
  )

richmond_finals_vs_regular <- richmond_game_type_averages %>%
  pivot_wider(
    id_cols = season,
    names_from = game_type,
    values_from = c(games, kicks_avg, handballs_avg, disposals_avg, marks_avg, tackles_avg,
                    contested_possessions_avg, uncontested_possessions_avg, inside50s_avg,
                    rebound50s_avg, clearances_avg, metres_gained_avg, score_involvements_avg,
                    turnovers_avg, intercepts_avg, tackles_inside50_avg, pressure_acts_avg,
                    ground_ball_gets_avg, contested_marks_avg, disposal_efficiency_avg, clangers_avg,
                    contested_uncontested_ratio, kick_handball_ratio)
  ) %>%
  mutate(
    tackles_diff_pct = (tackles_avg_Finals - tackles_avg_Regular) / tackles_avg_Regular * 100,
    pressure_acts_diff_pct = (pressure_acts_avg_Finals - pressure_acts_avg_Regular) / pressure_acts_avg_Regular * 100,
    contested_possessions_diff_pct = (contested_possessions_avg_Finals - contested_possessions_avg_Regular) / contested_possessions_avg_Regular * 100,
    ground_ball_gets_diff_pct = (ground_ball_gets_avg_Finals - ground_ball_gets_avg_Regular) / ground_ball_gets_avg_Regular * 100,
    inside50s_diff_pct = (inside50s_avg_Finals - inside50s_avg_Regular) / inside50s_avg_Regular * 100,
    tackles_inside50_diff_pct = (tackles_inside50_avg_Finals - tackles_inside50_avg_Regular) / tackles_inside50_avg_Regular * 100,
    disposal_efficiency_diff_pct = (disposal_efficiency_avg_Finals - disposal_efficiency_avg_Regular) / disposal_efficiency_avg_Regular * 100
  )

# tackles_avg_Regular not found

########
# Analysis of Richmond's Team Stats
########
ggplot(richmond_yearly_averages, aes(x = as.factor(season))) +
  geom_bar(aes(y = tackles_avg), stat = "identity", fill = "yellow") +
  geom_line(aes(y = pressure_acts_avg/5, group = 1), color = "black", size = 1) +
  geom_point(aes(y = pressure_acts_avg/5), color = "black", size = 3) +
  scale_y_continuous(
    name = "Tackles per game",
    sec.axis = sec_axis(~.*5, name = "Pressure Acts per game")
  ) +
  labs(
    title = "Richmond's Pressure Game (2017-2020)",
    x = "Season",
    caption = "Tackles (bars) and Pressure Acts (line)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "yellow", size = 12),
    axis.title.y.right = element_text(color = "black", size = 12)
  )

significant_stats <- richmond_vs_league %>%
  select(season, contains("diff_pct")) %>%
  pivot_longer(cols = contains("diff_pct"), names_to = "statistic", values_to = "diff_percentage") %>%
  mutate(statistic = gsub("_diff_pct", "", statistic)) %>%
  group_by(season) %>%
  arrange(season, desc(abs(diff_percentage))) %>%
  ungroup()
significant_stats

top_stats <- significant_stats %>%
  filter(statistic %in% c(
    "pressure_acts", "tackles", "tackles_inside50", 
    "contested_possessions", "ground_ball_gets", "intercepts", 
    "inside50s", "disposals", "marks"
  )) %>%
  mutate(statistic = factor(statistic, levels = c(
    "pressure_acts", "tackles", "tackles_inside50", 
    "contested_possessions", "ground_ball_gets", "intercepts", 
    "inside50s", "disposals", "marks"
  )))
top_stats

ggplot(top_stats, aes(x = statistic, y = diff_percentage, fill = as.factor(season))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Richmond's Statistical Differentiators vs. League Average (2017-2020)",
    x = "Statistic",
    y = "Percentage Difference from League Average",
    fill = "Season"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("2017" = "yellow", "2018" = "red", "2019" = "blue", "2020" = "black"))

print(richmond_vs_league %>% select(season, tackles_diff_pct, pressure_acts_diff_pct, tackles_inside50_diff_pct, ground_ball_gets_diff_pct, contested_possessions_diff_pct, inside50s_diff_pct))
print(richmond_finals_vs_regular %>% select(season, tackles_diff_pct, pressure_acts_diff_pct, tackles_inside50_diff_pct, ground_ball_gets_diff_pct, contested_possessions_diff_pct, inside50s_diff_pct))

#########
# Check which Teams in 2024 and 2025 are most similar to Richmond
#########
# Hawthorn seem to have the most similar play style according to the eye test and I hypothesize that they are the team
# most set up to replicate Richmond's success














