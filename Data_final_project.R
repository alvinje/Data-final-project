
print('Welcome to the final project of data schedule')

# Super commentaire.
library(tidyverse)
csv <- fread("data_DC.csv")

# get column names
colnames(csv)

# Remove duplicated column by index.
csv[,32] <- NULL

# Rename columns with Dplyr package.
csv <- csv %>% rename(
    annee_naissance = "Quelle est votre année de naissance ?",
    genre = "Vous êtes ?",
    nationalite = "Vous êtes ?",
    departement_travail = "Dans quel département travaillez-vous ?",
    situation_pro = "Quelle est votre situation professionnelle ?",
    specialisation_pro = "Quelle est votre spécialisation professionnelle ?",
    diplome_actuel =  "Quelle est votre niveau de diplôme actuel ?",
    tranche_salaire = "Quelle est votre tranche de salaire ?",
    type_bac = "Quel type de Bac avez-vous obtenu ?",
    annee_dernier_diplome_avant_DC = "En quelle année avez-vous obtenu votre dernier diplôme avant Digital Campus ?",
    dernier_diplome_avant_dc = "Quel est votre dernier niveau d'étude obtenu ?",
    formation_DC = "Quelle formation avez-vous suivi à Digital Campus ?",
    ville_formation_DC = "Dans quelle ville avez-vous fait votre formation Digital Campus ?",
    annee_debut_DC = "En quelle année avez-vous rejoint Digital Campus ?",
    annee_fin_DC = "En quelle année avez-vous quitté Digital Campus ?"
  )
colnames(csv)

  csv$annee_naissance[csv$annee_naissance == 996] <- 1996

  csv$annee_naissance <- revalue(csv$annee_naissance, c("996"="1996"))
  
  
  

summary(csv)
class(csv$nationalite)

csv <- csv %>% mutate(annee_naissance = ifelse(annee_naissance == 996, 1996, annee_naissance))
head(csv)

