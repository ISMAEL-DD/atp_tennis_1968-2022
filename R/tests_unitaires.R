#Packages nécessaires

library(testthat)
library(dplyr)
# Fichiers sources 

source('R/data_wrangling.R')
source('R/data_importation.R')

########################################################
#tests associés à la fonction filter_tournament_names()#
########################################################

#####################################################################
test_that("Filter_tournament_names() fonctionne comme prévu", {
#Jeu de données de test
  group_tournoi2 <- tibble(tourney_name = c("Open d'Australie", "Roland-Garros", "Wimbledon", "US Open", "Masters Cup"))
  
#Appel de la fonction avec le jeu de données de test et les noms de tournois à exclure
  result <- filter_tournament_names(group_tournoi2, c('Masters Cup'))
  
# vérifie que la fonction a correctement filtré le tournoi "Masters Cup"
  expect_equal(result, tibble(tourney_name = c("Open d'Australie", "Roland-Garros", "Wimbledon", "US Open")))
  
#Répétition du test avec d'autres tournois
  result <- filter_tournament_names(group_tournoi2, c('Masters Cup', 'US Open'))
  expect_equal(result, tibble(tourney_name = c("Open d'Australie", "Roland-Garros", "Wimbledon")))
})

test_that("Filter_tournament_names() renvoie un tibble vide lorsque tous les noms sont exclus", {
  group_tournoi3 <- tibble(tourney_name = c("Open d'Australie", "Roland-Garros", "Wimbledon", "US Open", "Masters Cup"))
  result <- filter_tournament_names(group_tournoi3, c("Open d'Australie", "Roland-Garros", "Wimbledon", "US Open", "Masters Cup"))
  expect_equal(result, tibble(tourney_name = character(0)))
})

test_that("Filter_tournament_names() gère les tibbles vides", {
  group_tournoi5 <- tibble(tourney_name = character(0))
  result <- filter_tournament_names(group_tournoi5, c("Open d'Australie"))
  expect_equal(result, tibble(tourney_name = character(0)))
})

test_that("Filter_tournament_names() gère les valeurs manquantes dans le tibble", {
  group_tournoi6 <- tibble(tourney_name = c("Open d'Australie", "Roland-Garros", NA, "US Open", "Masters Cup"))
  result <- filter_tournament_names(group_tournoi6, c('Masters Cup'))
  expect_equal(result, tibble(tourney_name = c("Open d'Australie", "Roland-Garros", NA, "US Open")))
})
#####################################################################


########################################################
###### tests associés à la fonction NbrJoueurs() #######
########################################################

#####################################################################
test_NbrJoueurs <- function() {
  # exemple_1: min = 1968, max = 1972
  resultat_attendu <- data.frame(
    AnnC)e = c(1968, 1969, 1970, 1971, 1972)
    NombreJoueurs = c(200, 300, 400, 500, 600)
    
    output <- NbrJoueurs(1968, 1972)
    if (identical(output, resultat_attendu)) {
      print("exemple_1 test passed")
    } else {
      print("exemple_1 test failed")
    }
    
    # exemple_2: min = 1970, max = 1980
    resultat_attendu <- data.frame(
      Annee = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980),
      NombreJoueurs = c(400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400))
    
    output <- NbrJoueurs(1970, 1980)
    if (identical(output, resultat_attendu)) {
      print("exemple_2 test passed")
    } else {
      print("exemple_2 test failed")
    }
    
    # exemple_3: min = 1980, max = 1980
    resultat_attendu <- data.frame( Annee = 1980,
                                    NombreJoueurs = 1400)
    
    
    output <- NbrJoueurs(1980, 1980)
    if (identical(output, resultat_attendu)) {
      print("exemple_3 test passed")
    } else {
      print("exemple_3 test failed")
    }
}

#####################################################################

########################################################
######## tests associés à la fonction NbrGc() ##########
########################################################

#####################################################################

test_NbrGc <- function() {
  # test_federer: player_name = "Roger Federer"
  resultat_attendu <- data.frame(
    winner_name = "Roger Federer",
    tourney_name = c("Australian Open", "Roland Garros", "US Open", "Wimbledon"),
    Nombre_de_tournois = c(2, 3, 4, 5)
  )
  output <- NbrGc("Roger Federer")
  if (identical(output, resultat_attendu)) {
    print("test_federer C  marcher")
  } else {
    print("test_federer C  C)chouer")
  }
  
  # test_Nadal: player_name = "Rafael Nadal"
  resultat_attendu <- data.frame(
    winner_name = "Rafael Nadal",
    tourney_name = c("Australian Open", "Roland Garros", "US Open", "Wimbledon"),
    Nombre_de_tournois = c(1, 4, 3, 2)
  )
  output <- NbrGc("Rafael Nadal")
  if (identical(output, resultat_attendu)) {
    print("test_Nadal C  marcher")
  } else {
    print("test_Nadal C  C)chouer")
  }
  
  # test_Djokovic: player_name = "Novak Djokovic"
  resultat_attendu <- data.frame(
    winner_name = "Novak Djokovic",
    tourney_name = c("Australian Open", "Roland Garros", "US Open", "Wimbledon"),
    Nombre_de_tournois = c(3, 2, 5, 4)
  )
  output <- NbrGc("Novak Djokovic")
  if (identical(output, resultat_attendu)) {
    print("test_Djokovic C  marcher")
  } else {
    print("test_Djokovic C  echouer")
  }
}

#####################################################################
