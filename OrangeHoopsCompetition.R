Data <- read.csv('/Users/rafeburns/Downloads/PBP2324.csv')
Data <- Data %>%
  select(c(game_id, home, away, play_id, half, secs_remaining,
           shooter, shot_team, shot_outcome, three_pt, free_throw,
           win_prob, naive_win_prob,
           home_score, away_score))
Data <- Data %>%
  group_by(game_id) %>%
  mutate(
    homeScoreBefore = lag(home_score, default = 0),  # Default to 0 for the first row
    awayScoreBefore = lag(away_score, default = 0)   # Default to 0 for the first row
  ) %>%
  mutate(newScoreDiff = abs(homeScoreBefore-awayScoreBefore))
Data <- Data %>%
  mutate(wpa = ifelse(shot_team == home, win_prob - lag(win_prob),
                      ifelse(shot_team == away, -1*(win_prob-lag(win_prob)), NA_real_)))
Data <- Data %>%
  filter(secs_remaining <= 300) %>%
  filter(max(half) <= 2) %>%
  group_by(game_id) %>%
  filter(newScoreDiff[n()] <= 3)

team_counts <- Data %>%
  pivot_longer(cols = c(home, away), names_to = "team_type", values_to = "team") %>%
  group_by(team) %>%
  summarise(Data = n_distinct(game_id)) %>%
  arrange(desc(Data))

Data <- Data %>%
  filter(shot_team == "ETSU") %>%
  filter(free_throw == FALSE) %>%
  mutate(gamePlayer = paste0(game_id,"-", shooter))

summedWPA <- Data %>%
  group_by(gamePlayer) %>%
  summarize(WPA = sum(wpa, na.rm = TRUE)) %>%
  separate(gamePlayer, into = c("gameID", "player"), sep = "-")
#####
finalSummedWPA <- summedWPA %>%
  group_by(player) %>%
  summarize(WPA = sum(WPA, na.rm = TRUE))

btMatrix <- summedWPA %>%
  group_by(gameID) %>%
  summarize(
    combos = list(as.data.frame(t(combn(player, 2)))), 
    wpa_combos = list(as.data.frame(t(combn(WPA, 2))))
  ) %>%
  unnest(cols = c(combos, wpa_combos), names_sep = "_") %>%
  rename(playerA = combos_V1, playerB = combos_V2, wpaA = wpa_combos_V1, wpaB = wpa_combos_V2)
btMatrix <- btMatrix %>%
  mutate(
    outcome = ifelse(wpaA > wpaB, 1, 0),  # 1 if playerA "won" (higher WPA), 0 otherwise
    playerA = as.factor(playerA),
    playerB = as.factor(playerB)
  )
comparisons <- with(btMatrix, cbind(outcome, 1 - outcome))

all_players <- unique(c(btMatrix$playerA, btMatrix$playerB))

# Convert playerA and playerB to factors with the same levels
btMatrix$playerA <- factor(btMatrix$playerA, levels = all_players)
btMatrix$playerB <- factor(btMatrix$playerB, levels = all_players)
btModel <- BTm(
  outcome,
  player1 = playerA,
  player2 = playerB,
  data = btMatrix
)

# Display the summary of the model
summary(btModel)

coefs <- coef(btModel)
std_errors <- sqrt(diag(vcov(btModel)))  # Extract standard errors
p_values <- 2 * (1 - pnorm(abs(coefs / std_errors)))  # Two-tailed p-values
model_summary <- data.frame(
  Coefficient = coefs,
  `Std. Error` = std_errors,
  `z Value` = coefs / std_errors,
  `Pr(>|z|)` = p_values
)
library(stargazer)

stargazer(model_summary, type = "html",
          out = "/Users/rafeburns/Downloads/brad_Terry_sum.html",
          title = "Bradley-Terry Model Summary", 
          summary = FALSE, rownames = TRUE)
