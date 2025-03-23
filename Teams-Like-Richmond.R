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
library(igraph)
library(ggraph)
library(tidyverse)
library(patchwork)
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
afl_2024 <- fetch_results_afl(2024)
afl_2024_players <- fetch_player_stats(season = 2024, source = "AFL", comp = "AFLM")

afl_2025 <- fetch_results_afl(2025)
afl_2025_players <- fetch_player_stats(season = 2025, source = "AFL", comp = "AFLM")

afl_2024_clean <- afl_2024 %>%
  mutate(
    date = as.Date(match.date, format = '%d/%m/%y'),
    season = 2024,
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
  )

afl_2025_clean <- afl_2025 %>%
  mutate(
    date = as.Date(match.date, format = '%d/%m/%y'),
    season = 2025,
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
  )

afl_2024_players_clean <- afl_2024_players %>%
  mutate(
    season = 2024,
    round = round.roundNumber,
    team_name = team.name,
    player_name = paste(player.givenName, player.surname),
    player_id = player.playerId
  ) %>%
  select(
    season, round, team_name, player_name, player_id,
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

afl_2025_players_clean <- afl_2025_players %>%
  mutate(
    season = 2025,
    round = round.roundNumber,
    team_name = team.name,
    player_name = paste(player.givenName, player.surname),
    player_id = player.playerId
  ) %>%
  select(
    season, round, team_name, player_name, player_id,
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

team_stats_2024 <- afl_2024_players_clean %>%
  group_by(season, round, team_name) %>%
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
  )

team_stats_2025 <- afl_2025_players_clean %>%
  group_by(season, round, team_name) %>%
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
  )

team_avg_2024 <- team_stats_2024 %>%
  group_by(team_name) %>%
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

team_avg_2025 <- team_stats_2025 %>%
  group_by(team_name) %>%
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

####### 
# The Richmond Model
#######
key_richmond_stats <- c(
  "tackles_avg", 
  "pressure_acts_avg", 
  "tackles_inside50_avg", 
  "ground_ball_gets_avg", 
  "contested_possessions_avg", 
  "inside50s_avg",
  "intercepts_avg",
  "contested_uncontested_ratio",
  "kick_handball_ratio"
)

richmond_premiership_avg <- richmond_yearly_averages %>%
  summarize(
    across(everything(), mean)
  ) %>%
  select(all_of(key_richmond_stats))

# Function to calculate Euclidean distance between a team and the Richmond model
# We'll normalize values to account for different scales across metrics

# Euclidean distance is a useful method for measuring the similarity between two sports teams because it provides a 
# straightforward way to quantify the overall difference between them based on numerical performance metrics. 
# By representing each team as a vector of statistics—such as goals, disposals, tackles, and marks—Euclidean 
# distance calculates the straight-line distance between these vectors in multi-dimensional space. 
# This results in a single number that reflects how similar or different the teams are across all the selected metrics. 
# A smaller distance indicates greater similarity, while a larger distance suggests more significant 
# differences in performance. It’s a simple, intuitive, and computationally efficient approach for comparing 
# teams based on multiple quantitative features.

calculate_richmond_similarity <- function(team_data, richmond_model, key_stats) {
  team_subset <- team_data %>% select(team_name, all_of(key_stats))
  richmond_subset <- richmond_model %>% select(all_of(key_stats))
  
  distances <- team_subset %>%
    rowwise() %>%
    mutate(
      tack_dist = (tackles_avg - richmond_subset$tackles_avg)^2 / richmond_subset$tackles_avg^2,
      press_dist = (pressure_acts_avg - richmond_subset$pressure_acts_avg)^2 / richmond_subset$pressure_acts_avg^2,
      tack50_dist = (tackles_inside50_avg - richmond_subset$tackles_inside50_avg)^2 / richmond_subset$tackles_inside50_avg^2,
      gbg_dist = (ground_ball_gets_avg - richmond_subset$ground_ball_gets_avg)^2 / richmond_subset$ground_ball_gets_avg^2,
      cont_dist = (contested_possessions_avg - richmond_subset$contested_possessions_avg)^2 / richmond_subset$contested_possessions_avg^2,
      i50_dist = (inside50s_avg - richmond_subset$inside50s_avg)^2 / richmond_subset$inside50s_avg^2,
      int_dist = (intercepts_avg - richmond_subset$intercepts_avg)^2 / richmond_subset$intercepts_avg^2,
      c_unc_dist = (contested_uncontested_ratio - richmond_subset$contested_uncontested_ratio)^2 / richmond_subset$contested_uncontested_ratio^2,
      k_h_dist = (kick_handball_ratio - richmond_subset$kick_handball_ratio)^2 / richmond_subset$kick_handball_ratio^2,
      
      total_distance = sqrt(tack_dist + press_dist + tack50_dist + gbg_dist + cont_dist + 
                              i50_dist + int_dist + c_unc_dist + k_h_dist),

            similarity_score = 1 / (1 + total_distance) * 100
    ) %>%
    ungroup() %>%
    arrange(desc(similarity_score))
  
  return(distances)
}

team_similarity_2024 <- calculate_richmond_similarity(
  team_avg_2024, 
  richmond_premiership_avg,
  key_richmond_stats
)
team_similarity_2024

team_similarity_2025 <- calculate_richmond_similarity(
  team_avg_2025, 
  richmond_premiership_avg,
  key_richmond_stats
)
team_similarity_2025

top_2024_teams <- team_similarity_2024 %>%
  select(team_name, similarity_score) %>%
  arrange(desc(similarity_score)) %>%
  mutate(rank = row_number())
top_2024_teams

top_2025_teams <- team_similarity_2025 %>%
  select(team_name, similarity_score) %>%
  arrange(desc(similarity_score)) %>%
  mutate(rank = row_number())
top_2025_teams

ggplot(top_2024_teams, aes(x = reorder(team_name, similarity_score), y = similarity_score)) +
  geom_col(fill = "#FFCD00") +
  coord_flip() +
  labs(
    title = "2024 AFL Teams Similarity to Richmond's 2017-2020 Model",
    x = "Team",
    y = "Similarity Score (higher = more similar)"
  ) +
  theme_minimal()

ggplot(top_2025_teams, aes(x = reorder(team_name, similarity_score), y = similarity_score)) +
  geom_col(fill = "#FFCD00") +
  coord_flip() +
  labs(
    title = "2025 AFL Teams Similarity to Richmond's 2017-2020 Model",
    x = "Team",
    y = "Similarity Score (higher = more similar)"
  ) +
  theme_minimal()

top_3_teams_2024 <- top_2024_teams %>%
  slice_head(n = 3) %>%
  pull(team_name)
top_3_teams_2024

top_3_teams_2025 <- top_2025_teams %>%
  slice_head(n = 3) %>%
  pull(team_name)
top_3_teams_2025

detailed_2024 <- team_stats_2024 %>%
  filter(team_name %in% top_3_teams_2024) %>%
  group_by(team_name) %>%
  summarize(
    tackles_avg = mean(tackles),
    pressure_acts_avg = mean(pressure_acts),
    tackles_inside50_avg = mean(tackles_inside50),
    ground_ball_gets_avg = mean(ground_ball_gets),
    contested_possessions_avg = mean(contested_possessions),
    inside50s_avg = mean(inside50s),
    intercepts_avg = mean(intercepts),
    contested_uncontested_ratio = mean(contested_possessions)/mean(uncontested_possessions),
    kick_handball_ratio = mean(kicks)/mean(handballs)
  )
detailed_2024

detailed_2025 <- team_stats_2025 %>%
  filter(team_name %in% top_3_teams_2025) %>%
  group_by(team_name) %>%
  summarize(
    tackles_avg = mean(tackles),
    pressure_acts_avg = mean(pressure_acts),
    tackles_inside50_avg = mean(tackles_inside50),
    ground_ball_gets_avg = mean(ground_ball_gets),
    contested_possessions_avg = mean(contested_possessions),
    inside50s_avg = mean(inside50s),
    intercepts_avg = mean(intercepts),
    contested_uncontested_ratio = mean(contested_possessions)/mean(uncontested_possessions),
    kick_handball_ratio = mean(kicks)/mean(handballs)
  )
detailed_2025

detailed_2024 %>%
  pivot_longer(cols = -team_name, names_to = "statistic", values_to = "value") %>%
  ggplot(aes(x = statistic, y = value, fill = team_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(data = richmond_premiership_avg %>% 
               pivot_longer(cols = everything(), names_to = "statistic", values_to = "richmond_value"),
             aes(yintercept = richmond_value), linetype = "dashed", color = "black") +
  facet_wrap(~statistic, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Top 3 Richmond-like Teams in 2024 vs Richmond 2017-2020",
    subtitle = "Dashed line represents Richmond's 2017-2020 average",
    x = NULL,
    y = "Value",
    fill = "Team"
  ) +
  theme(axis.text.x = element_blank())

detailed_2025 %>%
  pivot_longer(cols = -team_name, names_to = "statistic", values_to = "value") %>%
  ggplot(aes(x = statistic, y = value, fill = team_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(data = richmond_premiership_avg %>% 
               pivot_longer(cols = everything(), names_to = "statistic", values_to = "richmond_value"),
             aes(yintercept = richmond_value), linetype = "dashed", color = "black") +
  facet_wrap(~statistic, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Top 3 Richmond-like Teams in 2025 vs Richmond 2017-2020",
    subtitle = "Dashed line represents Richmond's 2017-2020 average",
    x = NULL,
    y = "Value",
    fill = "Team"
  ) +
  theme(axis.text.x = element_blank())
#############
# Hawthorn 3 Peat vs Richmond 3 flags
#############
# Because I used 4 years for Richmond I will also use 2011 for Hawthorn since they finished on top
seasons <- 2011:2015
old_afl_results_list <- list()
for (season in seasons) {
  results <- fetch_results(season = season, source = "AFL", comp = "AFLM")
  old_afl_results_list[[as.character(season)]] <- results
}
old_afl_results <- do.call(rbind, old_afl_results_list)
colnames(old_afl_results)

hawks_results <- old_afl_results %>%
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
    home_team == 'Hawthorn' | away_team == 'Hawthorn'
  )

hawks_long <- hawks_results %>%
  mutate(
    hawks_is_home = home_team == "Hawthorn",
    hawks_goals = ifelse(hawks_is_home, home_goals, away_goals),
    hawks_behinds = ifelse(hawks_is_home, home_behinds, away_behinds),
    hawks_total = ifelse(hawks_is_home, home_total, away_total),
    opponent_goals = ifelse(hawks_is_home, away_goals, home_goals),
    opponent_behinds = ifelse(hawks_is_home, away_behinds, home_behinds),
    opponent_total = ifelse(hawks_is_home, away_total, home_total),
    hawks_win = (hawks_is_home & home_win) | (!hawks_is_home & !home_win),
    margin = hawks_total - opponent_total,
    game_type = ifelse(as.numeric(gsub("R", "", round)) > 24 | grepl("F", round), "Finals", "Regular Season")
  )

hawks_season_summary <- hawks_long %>%
  group_by(season) %>%
  summarize(
    games = n(),
    wins = sum(hawks_win),
    losses = sum(!hawks_win),
    win_pct = wins/games,
    avg_score_for = mean(hawks_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin),
    positive_margins = sum(margin > 0),
    blowouts = sum(margin > 40)
  )

hawks_finals_summary <- hawks_long %>%
  filter(game_type == "Finals") %>%
  group_by(season) %>%
  summarize(
    games = n(),
    wins = sum(hawks_win),
    losses = sum(!hawks_win),
    win_pct = wins/games,
    avg_score_for = mean(hawks_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin)
  )

hawks_overall_summary <- hawks_long %>%
  summarize(
    games = n(),
    wins = sum(hawks_win),
    losses = sum(!hawks_win),
    win_pct = wins/games,
    avg_score_for = mean(hawks_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin),
    regular_season_win_pct = sum(hawks_win & game_type == "Regular Season")/sum(game_type == "Regular Season"),
    finals_win_pct = sum(hawks_win & game_type == "Finals")/sum(game_type == "Finals")
  )

# Player stats for Hawthorn period
seasons <- 2011:2015
old_afl_player_stats_list <- list()
for (season in seasons) {
  results <- fetch_player_stats(season = season, source = "AFL", comp = "AFLM")
  old_afl_player_stats_list[[as.character(season)]] <- results
}
old_afl_player_stats <- do.call(rbind, old_afl_player_stats_list)
old_afl_player_stats <- old_afl_player_stats %>% 
  mutate(
    date = as.Date(utcStartTime),
    season = year(date)
  )

hawks_player_stats <- old_afl_player_stats %>%
  filter(team.name == "Hawthorn") %>%
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

hawks_team_stats <- hawks_player_stats %>%
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

hawks_yearly_averages <- hawks_team_stats %>%
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

old_league_averages <- old_afl_player_stats %>%
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

old_league_yearly_averages <- old_league_averages %>%
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

hawks_vs_league <- hawks_yearly_averages %>%
  left_join(old_league_yearly_averages, by = "season", suffix = c("_hawks", "_league")) %>%
  mutate(
    kicks_diff_pct = (kicks_avg_hawks - kicks_avg_league) / kicks_avg_league * 100,
    handballs_diff_pct = (handballs_avg_hawks - handballs_avg_league) / handballs_avg_league * 100,
    disposals_diff_pct = (disposals_avg_hawks - disposals_avg_league) / disposals_avg_league * 100,
    marks_diff_pct = (marks_avg_hawks - marks_avg_league) / marks_avg_league * 100,
    tackles_diff_pct = (tackles_avg_hawks - tackles_avg_league) / tackles_avg_league * 100,
    contested_possessions_diff_pct = (contested_possessions_avg_hawks - contested_possessions_avg_league) / contested_possessions_avg_league * 100,
    uncontested_possessions_diff_pct = (uncontested_possessions_avg_hawks - uncontested_possessions_avg_league) / uncontested_possessions_avg_league * 100,
    inside50s_diff_pct = (inside50s_avg_hawks - inside50s_avg_league) / inside50s_avg_league * 100,
    rebound50s_diff_pct = (rebound50s_avg_hawks - rebound50s_avg_league) / rebound50s_avg_league * 100,
    clearances_diff_pct = (clearances_avg_hawks - clearances_avg_league) / clearances_avg_league * 100,
    metres_gained_diff_pct = (metres_gained_avg_hawks - metres_gained_avg_league) / metres_gained_avg_league * 100,
    score_involvements_diff_pct = (score_involvements_avg_hawks - score_involvements_avg_league) / score_involvements_avg_league * 100,
    turnovers_diff_pct = (turnovers_avg_hawks - turnovers_avg_league) / turnovers_avg_league * 100,
    intercepts_diff_pct = (intercepts_avg_hawks - intercepts_avg_league) / intercepts_avg_league * 100,
    tackles_inside50_diff_pct = (tackles_inside50_avg_hawks - tackles_inside50_avg_league) / tackles_inside50_avg_league * 100,
    pressure_acts_diff_pct = (pressure_acts_avg_hawks - pressure_acts_avg_league) / pressure_acts_avg_league * 100,
    ground_ball_gets_diff_pct = (ground_ball_gets_avg_hawks - ground_ball_gets_avg_league) / ground_ball_gets_avg_league * 100,
    contested_marks_diff_pct = (contested_marks_avg_hawks - contested_marks_avg_league) / contested_marks_avg_league * 100,
    disposal_efficiency_diff_pct = (disposal_efficiency_avg_hawks - disposal_efficiency_avg_league) / disposal_efficiency_avg_league * 100,
    clangers_diff_pct = (clangers_avg_hawks - clangers_avg_league) / clangers_avg_league * 100,
    contested_uncontested_ratio_diff_pct = (contested_uncontested_ratio_hawks - contested_uncontested_ratio_league) / contested_uncontested_ratio_league * 100,
    kick_handball_ratio_diff_pct = (kick_handball_ratio_hawks - kick_handball_ratio_league) / kick_handball_ratio_league * 100
  )

richmond_key_stats <- richmond_vs_league %>%
  select(
    season, 
    team = "Richmond",
    tackles_diff_pct,
    pressure_acts_diff_pct,
    contested_possessions_diff_pct,
    uncontested_possessions_diff_pct,
    inside50s_diff_pct,
    disposal_efficiency_diff_pct, 
    marks_diff_pct,
    contested_marks_diff_pct,
    ground_ball_gets_diff_pct,
    tackles_inside50_diff_pct,
    intercepts_diff_pct,
    kick_handball_ratio_diff_pct
  )

hawks_key_stats <- hawks_vs_league %>%
  select(
    season, 
    team = "Hawthorn",
    tackles_diff_pct,
    pressure_acts_diff_pct,
    contested_possessions_diff_pct,
    uncontested_possessions_diff_pct,
    inside50s_diff_pct,
    disposal_efficiency_diff_pct,
    marks_diff_pct,
    contested_marks_diff_pct,
    ground_ball_gets_diff_pct,
    tackles_inside50_diff_pct,
    intercepts_diff_pct,
    kick_handball_ratio_diff_pct
  )

richmond_dynasty_avg <- richmond_key_stats %>%
  summarize(
    team = "Richmond",
    period = "2017-2020",
    tackles_diff_pct = mean(tackles_diff_pct),
    pressure_acts_diff_pct = mean(pressure_acts_diff_pct),
    contested_possessions_diff_pct = mean(contested_possessions_diff_pct),
    uncontested_possessions_diff_pct = mean(uncontested_possessions_diff_pct),
    inside50s_diff_pct = mean(inside50s_diff_pct),
    disposal_efficiency_diff_pct = mean(disposal_efficiency_diff_pct),
    marks_diff_pct = mean(marks_diff_pct),
    contested_marks_diff_pct = mean(contested_marks_diff_pct),
    ground_ball_gets_diff_pct = mean(ground_ball_gets_diff_pct),
    tackles_inside50_diff_pct = mean(tackles_inside50_diff_pct),
    intercepts_diff_pct = mean(intercepts_diff_pct),
    kick_handball_ratio_diff_pct = mean(kick_handball_ratio_diff_pct)
  )

hawks_dynasty_avg <- hawks_key_stats %>%
  summarize(
    team = "Hawthorn",
    period = "2011-2015",
    tackles_diff_pct = mean(tackles_diff_pct),
    pressure_acts_diff_pct = mean(pressure_acts_diff_pct),
    contested_possessions_diff_pct = mean(contested_possessions_diff_pct),
    uncontested_possessions_diff_pct = mean(uncontested_possessions_diff_pct),
    inside50s_diff_pct = mean(inside50s_diff_pct),
    disposal_efficiency_diff_pct = mean(disposal_efficiency_diff_pct),
    marks_diff_pct = mean(marks_diff_pct),
    contested_marks_diff_pct = mean(contested_marks_diff_pct),
    ground_ball_gets_diff_pct = mean(ground_ball_gets_diff_pct),
    tackles_inside50_diff_pct = mean(tackles_inside50_diff_pct),
    intercepts_diff_pct = mean(intercepts_diff_pct),
    kick_handball_ratio_diff_pct = mean(kick_handball_ratio_diff_pct)
  )

dynasties_comparison <- bind_rows(richmond_dynasty_avg, hawks_dynasty_avg)

dynasties_long <- dynasties_comparison %>%
  pivot_longer(
    cols = ends_with("_diff_pct"),
    names_to = "statistic",
    values_to = "percentage_diff"
  ) %>%
  mutate(statistic = gsub("_diff_pct", "", statistic))

ggplot(dynasties_long, aes(x = statistic, y = percentage_diff, fill = team)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Richmond (2017-2020) vs. Hawthorn (2011-2015) Play Styles",
    subtitle = "Percentage Difference from League Average",
    x = "Statistic",
    y = "Percentage Difference (%)",
    fill = "Team"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Hawthorn" = "brown", "Richmond" = "yellow"))

richmond_perf <- data.frame(
  team = "Richmond",
  period = "2017-2020",
  win_pct = overall_summary$win_pct,
  finals_win_pct = overall_summary$finals_win_pct,
  avg_margin = overall_summary$avg_margin,
  premierships = 3
)

hawks_perf <- data.frame(
  team = "Hawthorn",
  period = "2011-2015",
  win_pct = hawks_overall_summary$win_pct,
  finals_win_pct = hawks_overall_summary$finals_win_pct,
  avg_margin = hawks_overall_summary$avg_margin,
  premierships = 3
)

dynasty_performance <- bind_rows(richmond_perf, hawks_perf)

ggplot(dynasty_performance, aes(x = team, y = win_pct, fill = team)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(win_pct * 100, 1), "%")), vjust = -0.5) +
  labs(
    title = "Overall Win Percentage Comparison",
    subtitle = "Richmond (2017-2020) vs Hawthorn (2011-2015)",
    y = "Win Percentage",
    x = ""
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Hawthorn" = "brown", "Richmond" = "yellow"))
########
# How about Brisbane 3 peat
#######
seasons <- 2001:2004
brisbane_afl_results_list <- list()

for (season in seasons) {
  results <- fetch_results(season = season, source = "AFL", comp = "AFLM")
  brisbane_afl_results_list[[as.character(season)]] <- results
}

brisbane_afl_results <- do.call(rbind, brisbane_afl_results_list)

brisbane_results <- brisbane_afl_results %>%
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
  ) %>% 
  filter(
    home_team %in% c('Brisbane Lions', 'Brisbane') | away_team %in% c('Brisbane Lions', 'Brisbane')
  )

brisbane_long <- brisbane_results %>%
  mutate(
    lions_is_home = home_team %in% c('Brisbane Lions', 'Brisbane'),
    lions_goals = ifelse(lions_is_home, home_goals, away_goals),
    lions_behinds = ifelse(lions_is_home, home_behinds, away_behinds),
    lions_total = ifelse(lions_is_home, home_total, away_total),
    opponent_goals = ifelse(lions_is_home, away_goals, home_goals),
    opponent_behinds = ifelse(lions_is_home, away_behinds, home_behinds),
    opponent_total = ifelse(lions_is_home, away_total, home_total),
    lions_win = (lions_is_home & home_win) | (!lions_is_home & !home_win),
    margin = lions_total - opponent_total,
    game_type = ifelse(as.numeric(gsub("R", "", round)) > 24 | grepl("F", round), "Finals", "Regular Season")
  )

brisbane_season_summary <- brisbane_long %>%
  group_by(season) %>%
  summarize(
    games = n(),
    wins = sum(lions_win),
    losses = sum(!lions_win),
    win_pct = wins/games,
    avg_score_for = mean(lions_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin),
    positive_margins = sum(margin > 0),
    blowouts = sum(margin > 40)
  )

brisbane_finals_summary <- brisbane_long %>%
  filter(game_type == "Finals") %>%
  group_by(season) %>%
  summarize(
    games = n(),
    wins = sum(lions_win),
    losses = sum(!lions_win),
    win_pct = wins/games,
    avg_score_for = mean(lions_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin)
  )

brisbane_overall_summary <- brisbane_long %>%
  summarize(
    games = n(),
    wins = sum(lions_win),
    losses = sum(!lions_win),
    win_pct = wins/games,
    avg_score_for = mean(lions_total),
    avg_score_against = mean(opponent_total),
    avg_margin = mean(margin),
    regular_season_win_pct = sum(lions_win & game_type == "Regular Season")/sum(game_type == "Regular Season"),
    finals_win_pct = sum(lions_win & game_type == "Finals")/sum(game_type == "Finals")
  )

seasons <- 2001:2004
brisbane_afl_player_stats_list <- list()

for (season in seasons) {
  results <- fetch_player_stats(season = season, source = "AFL", comp = "AFLM")
  brisbane_afl_player_stats_list[[as.character(season)]] <- results
}

brisbane_afl_player_stats <- do.call(rbind, brisbane_afl_player_stats_list)

brisbane_afl_player_stats <- brisbane_afl_player_stats %>% 
  mutate(
    date = as.Date(utcStartTime),
    season = year(date)
  )

brisbane_player_stats <- brisbane_afl_player_stats %>%
  filter(team.name %in% c("Brisbane Lions", "Brisbane")) %>%
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

brisbane_team_stats <- brisbane_player_stats %>%
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

brisbane_yearly_averages <- brisbane_team_stats %>%
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

old_league_averages_brisbane_era <- brisbane_afl_player_stats %>%
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

old_league_yearly_averages_brisbane_era <- old_league_averages_brisbane_era %>%
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

brisbane_vs_league <- brisbane_yearly_averages %>%
  left_join(old_league_yearly_averages_brisbane_era, by = "season", suffix = c("_lions", "_league")) %>%
  mutate(
    kicks_diff_pct = (kicks_avg_lions - kicks_avg_league) / kicks_avg_league * 100,
    handballs_diff_pct = (handballs_avg_lions - handballs_avg_league) / handballs_avg_league * 100,
    disposals_diff_pct = (disposals_avg_lions - disposals_avg_league) / disposals_avg_league * 100,
    marks_diff_pct = (marks_avg_lions - marks_avg_league) / marks_avg_league * 100,
    tackles_diff_pct = (tackles_avg_lions - tackles_avg_league) / tackles_avg_league * 100,
    contested_possessions_diff_pct = (contested_possessions_avg_lions - contested_possessions_avg_league) / contested_possessions_avg_league * 100,
    uncontested_possessions_diff_pct = (uncontested_possessions_avg_lions - uncontested_possessions_avg_league) / uncontested_possessions_avg_league * 100,
    inside50s_diff_pct = (inside50s_avg_lions - inside50s_avg_league) / inside50s_avg_league * 100,
    rebound50s_diff_pct = (rebound50s_avg_lions - rebound50s_avg_league) / rebound50s_avg_league * 100,
    clearances_diff_pct = (clearances_avg_lions - clearances_avg_league) / clearances_avg_league * 100,
    metres_gained_diff_pct = (metres_gained_avg_lions - metres_gained_avg_league) / metres_gained_avg_league * 100,
    score_involvements_diff_pct = (score_involvements_avg_lions - score_involvements_avg_league) / score_involvements_avg_league * 100,
    turnovers_diff_pct = (turnovers_avg_lions - turnovers_avg_league) / turnovers_avg_league * 100,
    intercepts_diff_pct = (intercepts_avg_lions - intercepts_avg_league) / intercepts_avg_league * 100,
    tackles_inside50_diff_pct = (tackles_inside50_avg_lions - tackles_inside50_avg_league) / tackles_inside50_avg_league * 100,
    pressure_acts_diff_pct = (pressure_acts_avg_lions - pressure_acts_avg_league) / pressure_acts_avg_league * 100,
    ground_ball_gets_diff_pct = (ground_ball_gets_avg_lions - ground_ball_gets_avg_league) / ground_ball_gets_avg_league * 100,
    contested_marks_diff_pct = (contested_marks_avg_lions - contested_marks_avg_league) / contested_marks_avg_league * 100,
    disposal_efficiency_diff_pct = (disposal_efficiency_avg_lions - disposal_efficiency_avg_league) / disposal_efficiency_avg_league * 100,
    clangers_diff_pct = (clangers_avg_lions - clangers_avg_league) / clangers_avg_league * 100,
    contested_uncontested_ratio_diff_pct = (contested_uncontested_ratio_lions - contested_uncontested_ratio_league) / contested_uncontested_ratio_league * 100,
    kick_handball_ratio_diff_pct = (kick_handball_ratio_lions - kick_handball_ratio_league) / kick_handball_ratio_league * 100
  )

brisbane_key_stats <- brisbane_vs_league %>%
  select(
    season, 
    team = "Brisbane Lions",
    tackles_diff_pct,
    pressure_acts_diff_pct,
    contested_possessions_diff_pct,
    uncontested_possessions_diff_pct,
    inside50s_diff_pct,
    disposal_efficiency_diff_pct,
    marks_diff_pct,
    contested_marks_diff_pct,
    ground_ball_gets_diff_pct,
    tackles_inside50_diff_pct,
    intercepts_diff_pct,
    kick_handball_ratio_diff_pct
  )

brisbane_dynasty_avg <- brisbane_key_stats %>%
  summarize(
    team = "Brisbane Lions",
    period = "2001-2004",
    tackles_diff_pct = mean(tackles_diff_pct),
    pressure_acts_diff_pct = mean(pressure_acts_diff_pct),
    contested_possessions_diff_pct = mean(contested_possessions_diff_pct),
    uncontested_possessions_diff_pct = mean(uncontested_possessions_diff_pct),
    inside50s_diff_pct = mean(inside50s_diff_pct),
    disposal_efficiency_diff_pct = mean(disposal_efficiency_diff_pct),
    marks_diff_pct = mean(marks_diff_pct),
    contested_marks_diff_pct = mean(contested_marks_diff_pct),
    ground_ball_gets_diff_pct = mean(ground_ball_gets_diff_pct),
    tackles_inside50_diff_pct = mean(tackles_inside50_diff_pct),
    intercepts_diff_pct = mean(intercepts_diff_pct),
    kick_handball_ratio_diff_pct = mean(kick_handball_ratio_diff_pct)
  )

dynasties_comparison <- bind_rows(richmond_dynasty_avg, hawks_dynasty_avg, brisbane_dynasty_avg)

dynasties_long <- dynasties_comparison %>%
  pivot_longer(
    cols = ends_with("_diff_pct"),
    names_to = "statistic",
    values_to = "percentage_diff"
  ) %>%
  mutate(statistic = gsub("_diff_pct", "", statistic))

ggplot(dynasties_long, aes(x = statistic, y = percentage_diff, fill = team)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "AFL Dynasty Play Styles Comparison",
    subtitle = "Percentage Difference from League Average",
    x = "Statistic",
    y = "Percentage Difference (%)",
    fill = "Team"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Hawthorn" = "brown", "Richmond" = "yellow", "Brisbane Lions" = "maroon"))

brisbane_perf <- data.frame(
  team = "Brisbane Lions",
  period = "2001-2004",
  win_pct = brisbane_overall_summary$win_pct,
  finals_win_pct = brisbane_overall_summary$finals_win_pct,
  avg_margin = brisbane_overall_summary$avg_margin,
  premierships = 3
)

dynasty_performance <- bind_rows(richmond_perf, hawks_perf, brisbane_perf)

ggplot(dynasty_performance, aes(x = team, y = win_pct, fill = team)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(win_pct * 100, 1), "%")), vjust = -0.5) +
  labs(
    title = "Overall Win Percentage Comparison",
    subtitle = "Richmond (2017-2020) vs Hawthorn (2011-2015) vs Brisbane Lions (2001-2004)",
    y = "Win Percentage",
    x = ""
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Hawthorn" = "brown", "Richmond" = "yellow", "Brisbane Lions" = "maroon"))

ggplot(dynasty_performance, aes(x = team, y = finals_win_pct, fill = team)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(finals_win_pct * 100, 1), "%")), vjust = -0.5) +
  labs(
    title = "Finals Win Percentage Comparison",
    subtitle = "Richmond (2017-2020) vs Hawthorn (2011-2015) vs Brisbane Lions (2001-2004)",
    y = "Finals Win Percentage",
    x = ""
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Hawthorn" = "brown", "Richmond" = "yellow", "Brisbane Lions" = "maroon"))

ggplot(dynasty_performance, aes(x = team, y = avg_margin, fill = team)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_margin, 1)), vjust = -0.5) +
  labs(
    title = "Average Winning Margin Comparison",
    subtitle = "Richmond (2017-2020) vs Hawthorn (2011-2015) vs Brisbane Lions (2001-2004)",
    y = "Average Margin",
    x = ""
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Hawthorn" = "brown", "Richmond" = "yellow", "Brisbane Lions" = "maroon"))
######
# Making Coaching Trees
######
# AFL Coaching Tree Visualization
afl_coaching_data <- tibble(
  coach = c(
    'John Kennedy Sr.', "David Parkin", "Alan Joyce", "Allan Jeans", "Robert Walls",
    "Tony Jewell", "Alex Jesaulenko", "Ron Barassi", "Tom Hafey", "John Nicholls",
    "John Coleman", "Norm Smith", "Bob Davis", "Phonse Kyne", "Charlie Sutton",
    "Reg Hickey", "Dick Reynolds", "Frank Hughes", "Percy Bentley", "Jack Dyer",
    "Jock Mchale", "Jack Bisset", "Charlie Clymo", "Albert Chadwick", "Cliff Rankin",
    "Syd Barker Sr.", "Kevin Sheedy", "Denis Pagan", "Malcolm Blight", "Leigh Matthews",
    "Mark Williams", "John Worsfold", "Paul Roos", "Mark Thompson", "Mick Malthouse",
    "John Longmire", "Alastair Clarkson", "Luke Beveridge", "Damien Hardwick", "Adam Simpson",
    "Simon Goodwin", "Chirs Scott", "Craig McRae", "Chris Fagan"
  ),
  mentor = c(
    "Jack Hale", "John Kennedy Sr.", "John Kennedy Sr.", NA, "Ron Barassi",
    "Tom Hafey", "Ron Barassi", "Norm Smith", NA, "Ron Barassi",
    "Dick Reynolds", "Dick Reynolds", "Reg Hickey", "Jock Mchale", "Arthur Olliver",
    "Cliff Rankin", NA, NA, NA, NA,
    NA, NA, NA, NA, NA,
    NA, "Tom Hafey", "Ron Barassi", "Ron Barassi", "John Kennedy Sr.",
    "Fos Williams", "Mick Malthouse", "David Parkin", "Kevin Sheedy", "Tony Jewell",
    "Paul Roos", "Malcolm Blight", "Alastair Clarkson", "Alastair Clarkson", "Denis Pagan",
    "Paul Roos", "Leigh Matthews", "Mick Malthouse", "Alastair Clarkson"
  )
)
additional_mentors_data <- tibble(
  coach = c(
    "John Kennedy Sr.", "Robert Walls", "Reg Hickey", "Reg Hickey", "Kevin Sheedy", 
    "Leigh Matthews", "Leigh Matthews", "Leigh Matthews", "Leigh Matthews",
    "Mark Williams", "Mark Williams", "Mark Williams", "Mark Williams", 
    "John Worsfold", "John Longmire", "John Longmire", "John Longmire", "John Longmire",
    "Alastair Clarkson", "Alastair Clarkson", "Simon Goodwin", "Simon Goodwin", "Simon Goodwin",
    "Craig McRae", "Craig McRae", "Craig McRae", "Craig McRae", "Chris Fagan"
  ),
  mentor = c(
    "Bob McCaskill", "John Nicholls", "Arthur Coghlan", "Charlie Clymo", "Tony Jewell",
    "Bob Rose", "David Parkin", "Allan Jeans", "Tom Hafey", 
    "John Cahil", "Tom Hafey", "Leigh Matthews", "Kevin Sheedy",
    "David Parkin", "Rodney Eade", "Denis Pagan", "Wayne Schimelbusch", "John Kennedy Sr.",
    "Tim Watson", "Mark Williams", "Alastair Clarkson", "Malcolm Blight", "Gary Ayres",
    "Damien Hardwick", "Terry Wallace", "Leigh Matthews", "Alastair Clarkson", "Neale Daniher"
  )
)

primary_edges <- afl_coaching_data %>%
  select(coach, mentor) %>%
  filter(!is.na(mentor)) %>%
  rename(to = coach, from = mentor)

additional_edges <- additional_mentors_data %>%
  rename(to = coach, from = mentor)

all_edges <- bind_rows(primary_edges, additional_edges)

g <- graph_from_data_frame(all_edges, directed = TRUE)

ggraph(g, layout = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(3, 'mm')), 
                 end_cap = circle(3, 'mm'),
                 alpha = 0.7) +
  geom_node_point(color = "steelblue", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "AFL Coaching Tree",
       subtitle = "Mentoring relationships between AFL coaches")

mentor_counts <- all_edges %>%
  count(from, sort = TRUE) %>%
  slice_head(n = 10)
mentor_counts

get_descendants <- function(graph, node) {
  desc <- subcomponent(graph, node, mode = "out")
  desc <- desc[desc != node]
  return(names(desc))
}

all_mentors <- unique(all_edges$from)

influence_data <- tibble(
  mentor = all_mentors,
  descendants = map(mentor, ~get_descendants(g, .)),
  descendants_count = map_int(descendants, length)
) %>%
  arrange(desc(descendants_count)) %>%
  slice_head(n = 10)

root_nodes <- V(g)[degree(g, mode = "in") == 0]
root_names <- names(root_nodes)

calculate_depth <- function(graph, node) {
  all_paths <- lapply(root_names, function(root) {
    all_simple_paths(graph, from = root, to = node)
  })
    all_paths <- unlist(all_paths, recursive = FALSE)
  if(length(all_paths) == 0) return(0)
  
  max_length <- max(sapply(all_paths, length)) - 1  # -1 because we don't count the node itself
  return(max_length)
}

important_coaches <- c(
  "Alastair Clarkson", "Craig McRae", "Ron Barassi", "Tom Hafey", 
  "Leigh Matthews", "Kevin Sheedy", "Paul Roos", "Mick Malthouse"
)

depth_data <- tibble(
  coach = important_coaches,
  generation = map_dbl(coach, ~calculate_depth(g, .))
) %>%
  arrange(desc(generation))

print(mentor_counts)
print(influence_data %>% select(mentor, descendants_count))
print(depth_data)
cat("\nKey Coaching Lineages:\n")

# I think some of that could be wrong where is JK

# do old era trees 
# Then we will do a Matthews Clarko and Hardwick tree

