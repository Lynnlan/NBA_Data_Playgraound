library(tidyverse)
library(jpeg)
library(stringr)
library(grid)

# Loading data

players_info <- read.csv("./playersInfo.csv", header = TRUE)
players_info_t <- as.tibble(players_info)
players_info_t

shots_info <- read.csv("./playoff_shots.csv", header = TRUE)
shots_info_t <- as.tibble(shots_info)
shots_info_t

# Data Wrangler

shots_info_t <-
  shots_info_t %>% 
  select(PLAYER_NAME, TEAM_NAME, PERIOD, SECONDS_REMAINING, EVENT_TYPE, SHOT_ZONE_AREA, SHOT_ZONE_BASIC, LOC_X, LOC_Y) %>%
  mutate(PLAYER_NAME = tolower(shots_info_t$PLAYER_NAME))

players_info_t <-
  players_info_t %>% 
  select(Player, From, To, Pos, Ht, Wt) %>%
  mutate(Player = as.character(Player))

# Using left_join
shots_player <- 
  left_join(shots_info_t, players_info_t, by = c("PLAYER_NAME" = "Player"))

# Plot 1 : The Playoff Shots Distribution of Each player

# Prepare the background image.
courtJPEG <- readJPEG("./court.jpeg")
court_g <- rasterGrob(courtJPEG, width = unit(1,"npc"), height = unit(1,"npc"))

# Using this function by input the player_name in "function(player_name, court_g)", like "stephen curry". No need to change court_g.
shot_distribution_dashboard <- function(player_name, court_g){
  title <- str_c("The Playoff Shots Distribution of: ", toupper(player_name))
  shots_info_t %>% 
    filter(PLAYER_NAME == player_name) %>%
    ggplot(aes(x = LOC_X, y = LOC_Y)) +
    annotation_custom(court_g, -250, 250, -50, 420) +
    geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
}

shot_distribution_plot <- shot_distribution_dashboard("stephen curry", court_g)

# save plot
ggsave("shot_distribution_plot.png", width = 10, height = 8, dpi = 150, units = "in", device = 'png')

# Plot 2 : The career length of players in different playoff teams
playoff_players <- 
  shots_player %>%
  select(PLAYER_NAME, TEAM_NAME, From, To) %>%
  unique() %>%
  mutate(career_length = To - From + 1, career_length_cat = "1") %>%
  na.omit()

# For Loop: Add values to the category of career length.
for (i in seq_along(playoff_players$career_length)){
  l = playoff_players$career_length[i]
  if (l <= 4) {
    playoff_players$career_length_cat[i] = "1-4 years"
  } else if (l <= 10) {
    playoff_players$career_length_cat[i] = "5-10 years"
  } else if (l <= 15) {
    playoff_players$career_length_cat[i] = "11-15 years"
  } else {
    playoff_players$career_length_cat[i] = "Above 15 years"
  }
}

# Plot
playoff_players %>%
  group_by(TEAM_NAME, career_length_cat) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = TEAM_NAME, fill = career_length_cat)) +
  ggtitle("The career length of players in different playoff teams") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x=element_text(angle = 30, hjust = 1))

# Save
ggsave("career_length_stats.png", width = 18, height = 9, dpi = 150, units = "in", device = 'png')