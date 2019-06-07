#Set working directory
setwd("~/Downloads")
pathFile <- 'data_DC.csv'
#Lire le csv
ResultatSondage <- read.csv(file=pathFile, header=TRUE, sep=";")
#Afficher le csv
summary(ResultatSondage)
