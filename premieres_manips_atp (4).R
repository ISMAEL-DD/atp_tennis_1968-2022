
# Importation donnees -----------------------------------------------------

## Chargement du pckage readr
library('readr')
## Importation du fichier
atp2019 <- read_csv(file = "~/Documents/databases/tennis_atp/atp_matches_2019.csv")
## Liste des fichiers du dossier et filtrage pour ne conserver que les fichier atp_matches_xxxx.csv
# Liste des noms des fichiers du dossier
lst <- list.files(path = "/home/philippe/Documents/databases/tennis_atp")
# On filtre pour ne garder que les noms des fichiers de la forme atp_matches_xxxx.csv
lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
# Chargement des packages stringr et purrr
library("stringr")
library('purrr')
# On crée le vecteur de chaines de caractères des nom des tibbles qu'on va créer lors de l'importation
lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
# On effectue une boucle avec map (ou lapply) pour importer les donnees et stocker les tibbles dans une liste
lst_tib <- map(.x = lst_data, 
               .f = function (x) read_csv(paste("/home/philippe/Documents/databases/tennis_atp/", 
                                                          x, sep = "")))
# On renomme les composantes de la liste des tibbles
names(lst_tib) <- lst_names
# Reclloage des bases à l'aide de la fonction reduce
library("dplyr")
atp <- reduce(.x = lst_tib, .f = bind_rows)


# Résumé d'une saison -----------------------------------------------------

## On reconstitue le tableau des tournois tel que présenté sur la page wikipédia « Saison ATP 2019 » 
library("dplyr")
atp2019 %>%
  filter(round == "F") %>%
  select(tourney_id, tourney_date, tourney_name, tourney_level, surface, winner_name, loser_name, score) %>%
  mutate(tourney_date = lubridate::ymd(tourney_date)) %>%
  arrange(tourney_date) %>%
  rename(winner = winner_name, finalist = loser_name) %>%
  View()


# Résumé de la saison d'un joueur -----------------------------------------

## Importation de la base des joueurs
players <- read_csv(file = "/home/philippe/Documents/databases/tennis_atp/atp_players.csv",
                    col_names = FALSE)
names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")

## Extraction de l'identifiant de Roger Federer dans la base des joueurs
players %>%
  filter(firstname == 'Roger' & lastname == 'Federer') %>%
  select(id) %>%
  as.numeric() -> id_fed
## Extraction des matchs joués par Roger Federer en 2019
atp2019 %>%
  filter(winner_id == id_fed | loser_id == id_fed) -> fed2019

## Nombre de matchs par surface
fed2019 %>%
  mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
  group_by(surface) %>%
  summarize(`Nombre de matchs` = n()) %>%
  #Toilettage en prévision de l'inclusion dans un rapport ou un graphique
  rename(Surface = surface) %>%
  mutate(Surface = case_when(Surface == "Clay" ~ "Terre battue",
                             Surface == "Grass" ~ "Herbe",
                             Surface == "Hard" ~ "Dur")) -> surf2019
## Chargement du script outils_stat_desc.R
source(file = "../Cours_SEP0942_2021/ressources/scripts/outils_stat_desc.R")
## Diagramme circulaire des matches de la saison 2019
library("ggplot2")
surf2019 %>%
  mutate(textpos = cumsum(`Nombre de matchs`) - `Nombre de matchs`/2) %>%
  ggplot(mapping = aes(x = 1, y = `Nombre de matchs`, fill = Surface)) +
  geom_col(width = 1) +
  coord_polar(theta = 'y', start = 0) +
  scale_fill_discrete(type = c('Terre battue' = 'chocolate', 'Herbe' = 'chartreuse4', 'Dur' = "deepskyblue4")) +
  theme_void() +
  geom_text(aes(y = textpos,
                label = paste(`Nombre de matchs`, ' (', round_perc(`Nombre de matchs`/sum(`Nombre de matchs`)*100,1),
                              '%' ,')', sep = '')),
            size=5)


## Adversaires rencontrés au cours d'une saison
fed2019 %>%
  select(tourney_date, tourney_name, tourney_level, surface, round, winner_name, loser_name, score, minutes) %>%
  filter(!grepl(pattern = '^Davis', x = tourney_name)) %>%
  mutate(opponent = case_when(winner_name == 'Roger Federer' ~ loser_name,
                              winner_name != 'Roger Federer' ~ winner_name),
         result =case_when(winner_name == 'Roger Federer' ~ "Won",
                           winner_name != 'Roger Federer' ~ 'Lost')) %>%
  select(-winner_name, -loser_name) -> summary_fed2019
library("tidyr")
summary_fed2019 %>%
  group_by(opponent, result) %>%
  summarize(`Nombre de matchs` = n()) %>%
  ungroup() %>% #Penser à dégrouper pour pouvoir calculer Total par la suite
  #On arrange un peu la présentation du tableau
  pivot_wider(names_from = result, values_from  = `Nombre de matchs`) %>%
  mutate(Total = apply(select(., Won, Lost), 1, sum, na.rm = TRUE)) %>%
  arrange(desc(Total)) -> op2019
op2019 %>%
  pivot_longer(cols = c(Won, Lost), names_to = "result", values_to = "eff") %>%
  ggplot(mapping = aes(x = reorder(opponent, Total), y = eff, fill = result)) +
  geom_col() +
  scale_fill_discrete(type = c('Lost' = "tomato3", "Won" = "springgreen3")) +
  coord_flip() +
  theme_bw() +
  labs(y = "Nombre de confrontations", x = 'Adversaires') +
  geom_text(mapping = aes(y = -0.1, label = Total), size = 2) +
  labs(fill = "Résultat")


# Création fonction summary_season ----------------------------------------

summary_season <- function(data, player, year) {
  .year <- year
  data %>%
    mutate(tourney_date = lubridate::ymd(tourney_date),
           year = lubridate::year(tourney_date)) %>%
    filter(year == .year) %>%
    filter(winner_name == player | loser_name == player) %>%
    select(tourney_date, tourney_name, tourney_level, surface, round, winner_name, loser_name, score, minutes) %>%
    #  filter(!grepl(pattern = '^Davis', x = tourney_name)) %>%
    mutate(opponent = case_when(winner_name == player ~ loser_name,
                                winner_name != player ~ winner_name),
           result =case_when(winner_name == player ~ "Won",
                             winner_name != player ~ 'Lost')) %>%
    select(-winner_name, -loser_name) -> summary_season_player
  return(summary_season_player)
}

summary_fed2019 <- summary_season(data = atp, player = "Roger Federer", year = 2019) 

## Résumé des résultats sur la saison

## Attention, il faut traiter intelligemment les tournois avec des phases de poules (les tournois avec des round robin (RR pour la variable round) et des matchs de barrage (ER))
library("tidyr")
summary_fed2019 %>%
  select(-minutes, -opponent, -score) %>%
  filter(tourney_level %in% c("A", "M", "G")) %>%
  mutate(tourney_result = case_when(round == 'F' & result == 'Won' ~ 'W',
                                    result == 'Lost' ~ round)) %>%
  drop_na(tourney_result) %>%
  arrange(tourney_date)




# Quelques représentations graphiques -------------------------------------

## Chargement du script outils_stat_desc.R
source(file = "../Cours_SEP0942_2021/ressources/scripts/outils_stat_desc.R")
## Diagramme circulaire des matches de la saison 2019
library("ggplot2")
surf2019 %>%
  mutate(textpos = cumsum(`Nombre de matchs`) - `Nombre de matchs`/2) %>%
  ggplot(mapping = aes(x = 1, y = `Nombre de matchs`, fill = Surface)) +
  geom_col(width = 1) +
  coord_polar(theta = 'y', start = 0) +
  scale_fill_discrete(type = c('Terre battue' = 'chocolate', 'Herbe' = 'chartreuse4', 'Dur' = "deepskyblue4")) +
  theme_void() +
  geom_text(aes(y = textpos,
                label = paste(`Nombre de matchs`, ' (', round_perc(`Nombre de matchs`/sum(`Nombre de matchs`)*100,1),
                              '%' ,')', sep = '')),
            size=5)

# DIagramme de Pareto des adversaires de Federer en 2019
summary_fed2019 %>%
  group_by(opponent, result) %>%
  summarize(`Nombre de matchs` = n()) %>%
  ungroup() %>% #Penser à dégrouper pour pouvoir calculer Total par la suite
  #On arrange un peu la présentation du tableau
  pivot_wider(names_from = result, values_from  = `Nombre de matchs`) %>%
  mutate(Total = apply(select(., Won, Lost), 1, sum, na.rm = TRUE)) %>%
  arrange(desc(Total)) -> op2019
op2019 %>%
  pivot_longer(cols = c(Won, Lost), names_to = "result", values_to = "eff") %>%
  ggplot(mapping = aes(x = reorder(opponent, Total), y = eff, fill = result)) +
  geom_col() +
  scale_fill_discrete(type = c('Lost' = "tomato3", "Won" = "springgreen3")) +
  coord_flip() +
  theme_bw() +
  labs(y = "Nombre de confrontations", x = 'Adversaires') +
  geom_text(mapping = aes(y = -0.1, label = Total), size = 2) +
  labs(fill = "Résultat")
