####En-tête####

##File: outils_stat_desc.R
##Desc.: quelques fonctions "maison" facilitant l'analyse descriptive d'une base de données
##Author: Philippe Regnault (philippe.regnault(at)univ-reims.fr)
##Date: 04/02/2019

####Arrondir un vecteur de pourcentage####
round_perc <- function(percentages, digits = 0){
  arrondis <- round(percentages, digits = digits)
  diff <- 100 - sum(arrondis)
  nb_dist <- round(abs(diff*10^digits))
  arr_ord <- order(arrondis)[nb_dist]
  arrondis[arr_ord] <- arrondis[arr_ord] + diff/nb_dist
  return(arrondis)  
}

####Diagramme en barres####
my_barplot <- function(freq, digits = 1, pareto = FALSE, stack = FALSE, margin = 1, mprof = FALSE,
                       text = TRUE,...){
  if (is.matrix(freq)){
    profils <- prop.table(freq, margin)
    if (margin != 1){profils <- t(profils); freq <- t(freq)}
    if (mprof) {
      profils <- rbind(profils, margin.table(prop.table(freq), 2))
      freq <- rbind(freq, margin.table(freq, 2))
      rownames(profils)[nrow(profils)] <- 'Ensemble'
    }
    n <- ncol(profils)
    m <- nrow(profils)
    par(mar = c(2.1, 2.1, 1.1, 0.1), ...)
    barplot(t(profils), beside = FALSE, 
            xlim = c(0.2, 1.2*m + 1), ylim = c(0,1), 
            col = paste('gray', round(seq(50, 100, length.out = n)), sep='')[n:1],
            legend.text = colnames(profils), axes = FALSE)
    axis(side = 2, at = seq(from = 0, to = 1, length.out = 5))
    if (text){
      for (x in 1:m){
        text(x = 0.7 +1.2*(x-1), y =  profils[x,]/2 + c(0, cumsum(profils[x,-n])),
             labels = paste(round_perc(profils[x,]*100, digits), ' % (', freq[x, ], ')', sep = ''), srt = 90)
      }
    }
  } 
  else{
    if (pareto){
      freq <- sort(freq)
    }
    n <- length(freq)
    perc <- freq /sum(freq )*100
    par(mar = c(2.1, 2.1, 0.1, 0.1), ...)
    if (stack){
      par(mar = c(2.1, 2.1, 2.1, 1.1))
      barplot(matrix(freq, nrow = length(freq), ncol = 1), xlim = c(0,2), ylim = c(0, sum(freq)*1.15), beside = FALSE,
              col = paste('gray', round(seq(50, 100, length.out = n)), sep='')[n:1],
              legend.text = names(freq), axes = FALSE)
      axis(side = 2, at = seq(from = 0, to = sum(freq), length.out = 5), labels = round(seq(from = 0, to = sum(freq), length.out = 5)))
      if (text){
        text(x = 0.7, y = freq/2 + c(0, cumsum(freq[-length(freq)])), 
             labels = paste(freq, ' (', round_perc(perc, digits), ' %)', sep = ''), srt=90)
        text(x = 0.3, y = sum(freq), labels = paste('Total : ', sum(freq), sep =''), pos = 3, cex = 0.7, srt=90)
      }
    }
    if (pareto & !stack) {
      barplot(freq, horiz = TRUE,  
              xlim = c(0, max(freq)*1.3), axes = FALSE,
              cex.names= 0.7)
      axis(side = 1, at = seq(from = 0, to = max(freq), length.out = 5), labels = round(seq(from = 0, to = max(freq), length.out = 5)))
      if (text){
        text(y = seq(from = 0.7, by = 1.2, length.out = length(freq)), x = freq, 
             labels = paste(freq, ' (', round_perc(perc, digits), ' %)', sep = ''),
             pos = 4, srt=90)
      }
    }
    if (!pareto & !stack) {
      barplot(freq,  ylim = c(0, max(freq)*1.15), axes = FALSE, 
              cex.names= 0.7)
      axis(side = 2 - pareto, at = seq(from = 0, to = max(freq), length.out = 5), labels = round(seq(from = 0, to = max(freq), length.out = 5)))
      if (text){
        text(x = seq(from = 0.7, by = 1.2, length.out = length(freq)), y = freq, 
             labels = paste('             ', freq, ' (', round_perc(perc,digits), ' %)', sep = ''),
             pos = 3, srt=90)
      }
    }
  }
}


####Diagramme circulaire####
my_pie <- function(freq, digits = 1, ...){
  n <- length(freq)
  perc <- freq /sum(freq )*100
  par(mar = c(.1, .1,.1,.1), ...)
  pie(freq, clockwise = TRUE,
      col = paste('gray', round(seq(50, 100, length.out = n)), sep='')[n:1]) 
  theta <- pi/2 - 2*pi*(freq/2 + c(0, cumsum(freq[-length(freq)])))/sum(freq)
  text(cos(theta)/2, sin(theta)/2,
       labels = paste(freq, ' (', round_perc(perc, digits), ' %)', sep = ''))
}



