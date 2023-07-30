# Pr?ambule ----

## Desc. : 

# Carrière d'un joueur

#' @title Extract and format matches for a specified player
#' @description This function returns a tibble of all ATP matches won or list by a player.
#' @param data The tibble to be filtered, as formated by Jeff Sackman.
#' @param player Character string specifying the name of the player to focus on.
#' @eval A tibble containing the matches from `data` concerning the player `player`.

format_player <- function(data, player) {
  data %>% 
    filter(winner_name == !!player | loser_name == !!player) %>%
    mutate(year = stringr::str_sub(tourney_id, start=1L, end=4L)) %>%
    select(year, tourney_name, tourney_date, tourney_level, tourney_type, surface, round, winner_name, loser_name, score, minutes) %>%
    mutate(opponent = case_when(winner_name == !!player ~ loser_name,
                                winner_name != !!player ~ winner_name),
           result =case_when(winner_name == !!player ~ "Won",
                             winner_name != !!player ~ 'Lost')) %>%
    select(-winner_name, -loser_name)
  
}

# Example
# source('R/data_importation.R')
library("dplyr")
# format_player(data = atp, player = "Roger Federer")


#' @title Enrich tibble returned by format_player by adding information at the scale of year*tourney
#' @description 
#' @param data a tibble returned by format_player
#' @eval Returns a tibble containing all matches of data except Davis cup and Laver Cup matches. It adds the following variables: tourney_result (containing the result of each tourney) and last_match (a flag indicating which is the last match in each tourney)

enhance_player_data <- function(data) {
  data %>%
    group_by(tourney_type) %>%
    nest() -> nested_data
  nested_data %>%
    filter(tourney_type %in% c("ATP", "Olympics", "Masters")) %>%
    mutate(data = case_when(tourney_type == "ATP" ~ map(data,
                                                        \(dt) {
                                                          dt %>% 
                                                            group_by(year, tourney_name) %>%
                                                            mutate(tourney_result = if_else(all(result == "Won") & "F" %in% as.character(round), 
                                                                                            "W", 
                                                                                            as.character(max(round)))) %>%
                                                            ungroup() %>%
                                                            identity()
                                                        }) ,
                            tourney_type == "Olympics" ~ map(data, 
                                                             \(dt){
                                                               dt %>%
                                                                 group_by(tourney_name) %>%
                                                                 mutate(tourney_result = case_when(all(result == "Won") & "F" %in% as.character(round) ~ "Gold",
                                                                                                   as.character(max(round)) == "F" & result[which.max(round)] == "Lost" ~ "Silver",
                                                                                                   as.character(max(round)) == "BR" & result[which.max(round)] == "Won" ~ "Bronze",
                                                                                                   as.character(max(round)) == "BR" & result[which.max(round)] == "Lost" ~ "BR cont.",
                                                                                                   TRUE ~ as.character(round))) %>%
                                                                 ungroup() %>%
                                                                 identity()
                                                             }),
                            # TRUE ~ data))
                            tourney_type == "Masters" ~ map(data, 
                                                            \(dt){
                                                              dt %>%
                                                                group_by(year) %>%
                                                                mutate(wonrr = sum(result[round == "RR"] == "Won", na.rm = TRUE),
                                                                       lostrr = sum(result[round == "RR"] == "Lost", na.rm = TRUE),
                                                                       tourney_result = case_when(all(result[round >= "SF"] == "Won") & "F" %in% as.character(round) ~ "W",
                                                                                                  "F" %in% as.character(round) | "SF" %in% as.character(round) ~ as.character(max(round)),
                                                                                                  TRUE ~ paste0("RR (", wonrr, "V/", lostrr, "D)"))) %>%
                                                                ungroup() %>%
                                                                select(-wonrr, -lostrr)
                                                            }),
                            TRUE ~ data)) %>%
    unnest(cols = c(data)) -> data_enhanced
  data_enhanced %>%
    group_by(year, tourney_name) %>%
    mutate(last_match = round == max(round))
}

# Example
# format_player(data = atp, player = "Roger Federer") -> player_career
# enhance_player_data(data = player_career) -> roger_career


#### evolution du nombre de joueurs par ann?e ####

NbrJoueurs <- function(min,max) {
                  atp %>% 
                    group_by(Année = substr(tourney_date,1,4))%>%   # Grouper par dates
                    filter(Année <= max) %>% 
                    filter(Année >= min ) %>% 
                    summarise(NombreJoueurs = length(unique(unique(loser_name),unique(winner_name))), .groups='drop')
}
#fonction pour determiner le nombre de tournois de Grand Chelem des joureurs 
NbrGc <- function(player_name) {
  atp%>%
    filter(winner_name %in% player_name & tourney_name %in% 
             c('Wimbledon', 'US Open', 'Roland Garros', 
               'Australian Open')&round == 'F')%>%
    group_by(winner_name, tourney_name)%>%
    summarize(Nombre_de_tournois = n())
}
NbrGc("Novak Djokovic")
NbrGc("Roger Federer")
NbrGc("Rafael Nadal")
NbrGc(c("Roger Federer","Rafael Nadal","Novak Djokovic"))

#Filtrage de la base de donnée
atp_big3<- atp%>%
  filter(winner_name %in% c("Roger Federer","Rafael Nadal","Novak Djokovic")&  
           loser_name %in% c("Roger Federer","Rafael Nadal","Novak Djokovic")&!winner_rank_points%in% c(4600,10010))


### Pourcentage de gagnants droitiers
DroitierWin = round(count(atp,winner_hand=='R')[2,2] / (count(atp,loser_hand=='R')[2,2] + count(atp,winner_hand=='R')[2,2]) *100,0)
GaucherWin = round(count(atp,winner_hand=='U')[2,2] / (count(atp,loser_hand=='U')[2,2] + count(atp,winner_hand=='U')[2,2]) *100,0)

### age médian des joueurs 
MedianAge <- round( median( cbind(atp$winner_age,atp$loser_age),na.rm=T),0)