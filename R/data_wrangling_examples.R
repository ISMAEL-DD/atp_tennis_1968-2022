# Summarizing the career of a player: examples of manipulations ----

#### Package loading and script sourcing ####

library("magrittr")
library("tidyverse")
source('R/data_importation.R')
source("R/data_wrangling.R")

#### Preparing data for Roger Federer ####

format_player(data = atp, player = "Roger Federer") -> player_career
enhance_player_data(data = player_career) -> roger_career

#### Results for each tourney ####

roger_career %>%
  group_by(year, tourney_name) %>% 
  summarize(tourney_result = max(tourney_result),
            last_opponent = opponent[last_match],
            score = score[last_match],
            minutes = minutes[last_match]) -> roger_tourneys

#### Summarizing head to heads ####

roger_career %>%
  group_by(opponent) %>%
  summarize(v = sum(result == "Won"),
            d = sum(result == "Lost"),
            t = n()) %>%
  arrange(desc(t)) 

#### Number of championships by category ####

roger_tourneys %>%
  left_join(y = atp_tourneys,
            by = c("year", "tourney_name")) %>%
  ungroup() %>%
  mutate(tourney_level = case_when(tourney_level == "A" & tourney_type == "Olympics" ~ "O",
                                   TRUE ~ tourney_level)) %>%
  group_by(tourney_level) %>%
  summarize(nbchamp = sum(tourney_result %in% c("W", "Gold")))


#nombre de tournois de gc de roger federer
federer <-atp%>%
  filter(winner_name=="Roger Federer" & tourney_name %in% c('Wimbledon', 'US Open', 'Roland Garros', 'Australian Open')& round == 'F')%>%
  group_by(tourney_name)%>%
  summarize(Nombre_de_matchs = n())

