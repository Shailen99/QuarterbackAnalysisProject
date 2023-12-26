## Calculating Quarterback Quality:

## Efficiency Metrics (Y/A, AY/A, NY/A)

library("tidyverse")
library("nflverse")
library("ggrepel")  # Don't forget to load ggrepel

#(ANY/A)
qb_ANYA <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>% filter(sum(attempts) > 554) %>% 
  summarize(
    adjusted_yards_per_attempt = (sum(passing_yards) + 20 * (sum(passing_tds)) - 45 * (sum(interceptions)) - sum(sack_yards)) / ((sum(attempts)) + sum(sacks)),
    passing_yards = sum(passing_yards),
    recent_team = first(recent_team)  # Use an appropriate aggregation function for recent_team
  )


team_colors <- c(
  "ARI" = "#97233F",
  "ATL" = "#A71930",
  "BAL" = "#241773",
  "BUF" = "#00338D",
  "CAR" = "#0085CA",
  "CHI" = "#00143F",
  "CIN" = "#FB4F14",
  "CLE" = "#311D00",
  "DAL" = "#041E42",
  "DEN" = "#002244",
  "DET" = "#0076B6",
  "GB"  = "#203731",
  "HOU" = "#03202F",
  "IND" = "#002C5F",
  "JAX" = "#006778",
  "KC"  = "#E31837",
  "LV"  = "#000000",
  "LAC" = "#002A5E",
  "LAR" = "#003594",
  "MIA" = "#008E97",
  "MIN" = "#4F2683",
  "NE"  = "#002244",
  "NO"  = "#D3BC8D",
  "NYG" = "#0B2265",
  "NYJ" = "#125740",
  "PHI" = "#004C54",
  "PIT" = "#FFB612",
  "SF"  = "#AA0000",
  "SEA" = "#002244",
  "TB"  = "#D50A0A",
  "TEN" = "#4B92DB",
  "WAS" = "#773141"
  
)
#take top 10
top_qbs <- qb_ANYA %>% 
  arrange(desc(adjusted_yards_per_attempt)) %>% 
  slice(1:15)



# Modify ggplot code to include team colors
ggplot(data = top_qbs, aes(x = player_name, y = adjusted_yards_per_attempt, fill = recent_team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = team_colors) +  # Assign team colors
  # Updated geom_text_repel with adjustments
  geom_text_repel(aes(label = player_name),
                  box.padding = 0.5,
                  point.padding = 0.2,
                  force = 0.5,
                  max.overlaps = 35,
                  vjust = -0.5,  # Adjust vertical position inside the bar
                  angle = 90,   # Flip the text vertically
                  color = "white",
                  position = position_stack(vjust = 0.5),
                  size = 5,  # Adjust the size of the text
                  family = "bold") +  # Set text color
  labs(title = "Top Quarterbacks by Adjusted Yards per Attempt", y = "Adjusted Yards per Attempt")

qb_QBR <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>%filter(sum(attempts) > 554) %>% 
  summarize(
    completions = sum(completions),
    attempts = sum(attempts),
    passing_yards = sum(passing_yards),
    passing_tds = sum(passing_tds),
    interceptions = sum(interceptions),
    
    # Calculate individual components
    C = (completions / attempts) * 100,
    Y = passing_yards / attempts,
    TD = (passing_tds / attempts) * 100,
    INT = (interceptions / attempts) * 100,
    
    # Calculate Passer Rating
    passer_rating = ((C - 30) / 20) + ((Y - 3) / 4) + (TD / 5) - (INT / 4),recent_team = first(recent_team)
  )

#take top 10
top_qbs_QBR <- qb_QBR %>% 
  arrange(desc(passer_rating)) %>% 
  slice(1:15)

ggplot(data = top_qbs_QBR, aes(x = player_name, y = passer_rating, fill = recent_team)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = team_colors) +  # Assign team colors
  # Updated geom_text_repel with adjustments
  geom_text_repel(aes(label = player_name),
                  box.padding = 0.5,
                  point.padding = 0.2,
                  force = 0.5,
                  max.overlaps = 35,
                  vjust = -0.5,  # Adjust vertical position inside the bar
                  angle = 90,   # Flip the text vertically
                  color = "white",
                  position = position_stack(vjust = 0.5),
                  size = 5,  # Adjust the size of the text
                  family = "bold") +  # Set text color
  labs(title = "Top Quarterbacks by Passer Rating", y = "Passer Rating")
generic <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") 




##ANY/A with records
## get win numbers per player
qb_records <- data.frame(
  player = c("P.Mahomes", "D.Brees", "J.Garoppolo", "T.Brady", "A.Rodgers", "D.Watson", "P.Rivers", "D.Prescott", "K.Cousins", "A.Luck", "M.Ryan", "J.Burrow", "R.Wilson", "L.Jackson", "G.Smith", "M.Stafford", "J.Hurts", "R.Tannehill"),
  wins = c(72, 48, 43, 79, 67, 31, 42, 61, 61, 18, 50, 24, 62, 45, 17, 47, 33, 49),
  losses = c(19, 22, 20, 31, 36, 28, 38, 36, 49, 13, 58, 18, 46, 16, 16, 50, 12, 30)
)

top_qbs_with_records <- inner_join(top_qbs, qb_records, by = c("player_name" = "player"))


ggplot(top_qbs_with_records, aes(x = adjusted_yards_per_attempt, y = (wins / (wins + losses))*100, label = player_name,fill = recent_team)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_text(aes(label = player_name), vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels above the points
  scale_fill_manual(values = team_colors) +  # Assign team colors
  labs(title = "ANY/A Impact on Quarterback Win Rates",
       x = "Adjusted Net Yards per Attempt (ANY/A)",
       y = "Win Rate (%)") +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
  )


# do it for passer rating
top_qbsQBR_with_records <- inner_join(top_qbs_QBR, qb_records, by = c("player_name" = "player"))

ggplot(top_qbsQBR_with_records, aes(x = passer_rating, y = (wins / (wins + losses))*100, label = player_name,fill = recent_team)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_text(aes(label = player_name), vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels above the points
  scale_fill_manual(values = team_colors) +  # Assign team colors
  labs(title = "Passer Rating Impact on Quarterback Win Rates",
       x = "Passer Rating",
       y = "Win Rate (%)") +
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
  )

#Completion Percentage v ANY/A 
qb_CMP <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>% summarize(CMPPercentage = ((sum(completions)/sum(attempts)))*100, average_yards_per_attempt =  (sum(passing_yards) + 20*(sum(passing_tds)) - 45*(sum(interceptions)))/sum(attempts))

combined_data_ANYA <- left_join(top_qbs_with_records, qb_CMP, by = "player_name") #any/a
combined_data_PASR <- left_join(top_qbs_with_records, qb_CMP, by = "player_name")#passer rating

ggplot(combined_data_ANYA, aes(x = CMPPercentage, y = average_yards_per_attempt)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_text(aes(label = player_name), vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels above the points
  scale_fill_manual(values = team_colors) +  # Assign team colors
  geom_density_2d() +
  labs(title = "Density Plot of Career Completion Percentage vs ANY/A for Quarterbacks",
       x = "Completion Percentage",
       y = "ANY/A")

#Completion percentage v PAsser rating


ggplot(combined_data_PASR, aes(x = CMPPercentage, y = passer_rating)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_text(aes(label = player_name), vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels above the points
  scale_fill_manual(values = team_colors) +  # Assign team colors
  geom_density_2d() +
  labs(title = "Density Plot of Completion Percentage vs Passer Rating for Top Quarterbacks",
       x = "Completion Percentage (%)",
       y = "Passer Rating")


##Rushing QBs

qbRushers <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>%
  filter(sum(attempts) > 554) %>% summarize(passing_yards = sum(passing_yards),rushing_attempts = sum(carries),
recent_team = recent_team, rushing_yards = sum(rushing_yards), rushing_tds = sum(rushing_tds))

qbRushers <- qbRushers %>% distinct(player_name, .keep_all = TRUE) #keep nonduplicates
#Scatter Plot
qbRushers <- qbRushers %>% filter(rushing_attempts >= 100 & passing_yards >= 10000)
ggplot(qbRushers, aes(x = rushing_attempts, y = passing_yards,label = player_name,fill = recent_team)) +
  geom_point(size = 3, shape = 21, color = "black") +
  geom_text(aes(label = player_name), vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels above the points
  scale_fill_manual(values = team_colors) +  # Assign team colors
  labs(title = "Impact of Rushing Attempts on Passing Yards",
       x = "Rushing Attempts",
       y = "Passing Yards")

##Box plots

qbRushers <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>%
  filter(sum(attempts) > 554)

qb_names <- c("L.Jackson", "J.Allen", "C.Newton", "K.Murray", "J.Hurts", "R.Wilson", "J.Fields",
              "D.Jones", "D.Watson", "P.Mahomes", "D.Prescott", "C.Wentz", "K.Cousins", "E.Manning",
              "M.Ryan", "D.Carr", "A.Rodgers", "T.Brady")


QBR_Rushing <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>%
  filter(sum(attempts) > 554 & player_name %in% qb_names) %>% 
  summarize(
    completions = sum(completions),
    attempts = sum(attempts),
    passing_yards = sum(passing_yards),
    passing_tds = sum(passing_tds),
    interceptions = sum(interceptions),
    
    # Calculate individual components
    C = (completions / attempts) * 100,
    Y = passing_yards / attempts,
    TD = (passing_tds / attempts) * 100,
    INT = (interceptions / attempts) * 100,
    
    # Calculate Passer Rating
    passer_rating = ((C - 30) / 20) + ((Y - 3) / 4) + (TD / 5) - (INT / 4),    # Assign rush-tendency labels
    rush_tendency = case_when(
      player_name %in% c("L.Jackson", "J.Allen", "C.Newton", "K.Murray", "J.Hurts", "R.Wilson", "J.Fields") ~ "High",
      player_name %in% c("D.Jones", "D.Watson", "P.Mahomes", "D.Prescott", "C.Wentz", "K.Cousins") ~ "Mid-tier",
      player_name %in% c("E.Manning", "M.Ryan", "D.Carr", "A.Rodgers", "T.Brady") ~ "Low-tier",
      TRUE ~ "Unknown"
    ),
    recent_team = first(recent_team)
  )

QBR_Rushing <- QBR_Rushing %>% distinct(player_name, .keep_all = TRUE) #keep nonduplicates


ggplot(QBR_Rushing, aes(x = rush_tendency, y = passer_rating, fill = rush_tendency)) +
  geom_boxplot() +
  labs(title = "Comparison of Passer Rating by Rush Tendency",
       x = "Rush Tendency",
       y = "Passer Rating") +
  theme_minimal()
#Dual Axis

qb_records <- rbind(qb_records,
                    data.frame(player = "J.Fields", wins = 5, losses = 20),
                    data.frame(player = "D.Jones", wins = 22, losses = 36),
                    data.frame(player = "C.Wentz", wins = 46, losses = 45),
                    data.frame(player = "E.Manning", wins = 20, losses = 31),
                    data.frame(player = "D.Carr", wins = 53, losses = 57)
)


qb_names <- c("L.Jackson", "J.Allen", "C.Newton", "K.Murray", "J.Hurts", "R.Wilson", "J.Fields",
              "D.Jones", "D.Watson", "P.Mahomes", "D.Prescott", "C.Wentz", "K.Cousins", "E.Manning",
              "M.Ryan", "D.Carr", "A.Rodgers", "T.Brady")
QBR_Rushing <- nflreadr::load_player_stats(seasons = 2016:2022) %>%
  filter(season_type == "REG" & position == "QB") %>% 
  group_by(player_name) %>%
  filter(sum(attempts) > 554 & player_name %in% qb_names) %>% summarize(rushing_attempts = sum(carries),    completions = sum(completions), attempts = sum(attempts), passing_yards = sum(passing_yards),
passing_tds = sum(passing_tds),
interceptions = sum(interceptions),

# Calculate individual components
C = (completions / attempts) * 100,
Y = passing_yards / attempts,
TD = (passing_tds / attempts) * 100,
INT = (interceptions / attempts) * 100,
# Calculate Passer Rating
passer_rating = ((C - 30) / 20) + ((Y - 3) / 4) + (TD / 5) - (INT / 4),recent_team = recent_team)

combinedQBRWins <- inner_join(QBR_Rushing, qb_records, by = c("player_name" = "player"))
combinedQBRWins <- combinedQBRWins %>% distinct(player_name, .keep_all = TRUE) #keep nonduplicates


ggplot(combinedQBRWins, aes(x = player_name)) +
  
  # First axis (left) for rushing attempts
  geom_bar(aes(y = rushing_attempts, fill = recent_team), stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = team_colors) +
  
  # Add player names to the bars on the left axis
  geom_text(aes(y = rushing_attempts, label = player_name),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            family = "bold",
            angle = 90,
            hjust = 0.5) +
  
  # Second axis (right) for passing yards
  geom_line(aes(y = passing_yards * 0.01), color = "red", size = 1.5, group = 1) +
  geom_point(aes(y = passing_yards * 0.01), color = "red", size = 3, shape = 19) +

  geom_point(aes(y = (wins / (wins + losses)) * 100 * 10), color = "purple", size = 3, shape = 16) +
  geom_line(aes(y = (wins / (wins + losses)) * 100 * 10), color = "purple", size = 1.5, linetype = "dashed", group = 3) +
  
  # Add win-loss percentages above the points
  geom_text(aes(y = (wins / (wins + losses)) * 100 * 10, label = paste0(round((wins / (wins + losses)) * 100, 1), "%")),
            color = "black",
            vjust = -1,
            hjust = 0.5,
            size = 3) +
  
  # Add labels and titles
  labs(
    title = "Impact of Rushing on Quarterback Quality",
    y = "Rushing Attempts",
    x = "Quarterback",
    fill = "Team",
    color = "Passing Yards (scaled) / Passer Rating (scaled) / Win Rate (scaled)"
  ) +
  
  # Set the y-axis labels for the second axis
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Passing Yards (scaled) / Passer Rating (scaled) / Win Rate (scaled)")) +
  
  # Customize appearance
  theme_minimal()+  scale_linetype_manual(name = "Metrics", values = c("solid", "dashed", "solid"), labels = c("Passing Yards", "Passer Rating", "Win Rate"))




#O-line Quality
pbp <- nflreadr::load_pbp(2022) %>%
  filter(season_type == "REG") %>%
  mutate(qbh_inc = ifelse(qb_hit == 1 & incomplete_pass == 1, 1,0),
         qb_int = ifelse(qb_hit == 1 & interception == 1, 1,0))

nfl_plays <- pbp %>%
  group_by(week, posteam) %>%
  summarize(
    total_plays = sum(play_type == "run" | play_type == "pass", na.rm = TRUE)
  )
# Calculate Total Line Value 
nfl_line_value <- pbp %>%
  filter(
    sack == 1 | qb_hit == 1 | qb_scramble == 1
  ) %>%
  group_by(posteam) %>%
  summarize(
    sacks = sum(sack),
    qb_hits = sum(qb_hit),
    qb_scrambles = sum(qb_scramble)
  )
pass_protection_long <- nfl_line_value %>%
  pivot_longer(cols = c(sacks, qb_hits, qb_scrambles), names_to = "Outcome", values_to = "Count")

team_qbr_completion <- QBR_Rushing %>%
  group_by(recent_team) %>%
  summarize(
    avg_qbr = mean(passer_rating, na.rm = TRUE),
    avg_completion_pct = mean(C, na.rm = TRUE)
  )

pass_protection_qbr_pct_team <- merge(pass_protection_long, team_qbr_completion, by.x = "posteam", by.y = "recent_team")

ggplot(pass_protection_qbr_pct_team, aes(x = posteam, y = Count, fill = Outcome, color = avg_qbr)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb"), name = "Outcome") +
  scale_color_gradient(low = "red", high = "green", name = "Average QBR") +
  geom_text(aes(label = sprintf("%.1f", avg_qbr), y = Count + 5), color = "black", vjust = -0.5, size = 3) +  # Display QBR values above the bars
  labs(
    title = "Pass Protection Outcomes and Average Passer Rating by Team",
    x = "Team",
    y = "Number of Outcomes"
  ) +
  theme_minimal()


