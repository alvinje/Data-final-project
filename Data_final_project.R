
print('Welcome to the final project of data schedule')

#super commentaire

csv <- fread("data_DC.csv", encoding = "Latin-1")
csv <- read_delim("data_DC.csv", sep=";")
setwd("/Users/sandy/Documents/Digital Campus/Cours/Data/M2/Module Projet Data/Scripts/Data-final-project")
Sys.setlocale(category = "LC_ALL", locale = "fr_FR.UTF-8")
library(data.table)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

#########

# Remove duplicated column by index.
csv[,32] <- NULL

# Rename columns with Dplyr package.
csv <- csv %>% rename(
  age = "Quelle est votre année de naissance ?",
  genre = "Vous êtes ?",
  nationalite = "Quel est votre nationalité ?",
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
  annee_fin_DC = "En quelle année avez-vous quitté Digital Campus ?",
  niveau_avt_dc_crea = "Comment évaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Créatif]",
  niveau_avt_dc_mkt_com = "Comment évaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Marketing et communication]",
  niveau_avt_dc_dev = "Comment évaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Developpement]",
  niveau_avt_dc_gest = "Comment évaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Gestion de projet]",
  niveau_apr_dc_crea = "Comment évaluez-vous votre niveau après Digital Campus dans les domaines suivants? [Créatif]",
  niveau_apr_dc_mkt_com = "Comment évaluez-vous votre niveau après Digital Campus dans les domaines suivants? [Marketing et communication]",
  niveau_apr_dc_dev = "Comment évaluez-vous votre niveau après Digital Campus dans les domaines suivants? [Developpement]",
  niveau_apr_dc_gest = "Comment évaluez-vous votre niveau après Digital Campus dans les domaines suivants? [Gestion de projet]",
  spe = "Quelle spécialité avez-vous choisi ?",
  spe_ok = "Votre spécialité a-t-elle répondu à vos attentes ?",
  tps_trouve_alt = "En combien de temps avez-vous trouvé votre alternance ?",
  sourcing_alt = "Comment avez-vous trouvé votre alternance ?",
  reco_dc = "Recommanderiez-vous Digital Campus à d'autres personnes sur une échelle de 0 à 5 ?",
  recherche_emploi_apr_dc = "Avez-vous cherché un emploi à la fin de vos études ?",
  non_pq = "Non, pourquoi ?",
  tps_recherche_emploi = "Combien de temps a duré votre recherche d\u0092emploi ?",
  spe_pro = "Quelle est votre spécialisation professionnelle ?",
  type_contrat_apr_dc = "Quel était le type de contrat ?",
  tps_plein_partiel = "Était-ce à temps plein ou à temps partiel ?",
  salaire_embauche_apr_dc = "Quel était votre salaire à l\u0092embauche ?",
  emploi_priv_pub = "Votre emploi était-il dans le privé ou dans le public ?",
  csp_apr_dc = "A quelle catégorie socio-professionnelle apparteniez-vous ?",
  type_entreprise = "Dans quel type d\u0092entreprise étiez-vous?",
  dept_premier_emploi = "Dans quel département avez-vous trouvé votre premier emploi ?",
  milieu = "Dans quel milieu ?",
  change_ville_pr_emploi = "Aviez-vous changé de ville pour décrocher cet emploi ?",
  cmt_trouve_emploi_apr_dc = "Comment aviez-vous trouvé votre travail ?",
  perception_recherche_emploi_apr_dc = "Comment aviez-vous perçu votre recherche d\u0092emploi ?",
  satisfaction_premier_emploi = "Aviez-vous été satisfait de ce premier emploi?",
  place_travail_vie = "Quelle place occupe votre travail dans votre vie?",
  estimation_importance_dc_recrutement = "A combien estimez vous l\u0092importance de digital campus dans votre recrutement?"
    )
colnames(csv)

##############
## PROFIL DES REPONDANTS ##

## AGE
#annee_naissance <- csv %>%
#  group_by('age') %>%
#  count()

#naiss <- csv$age
#evt <- 2019
#time_length(interval(naiss, evt))

# Calcul de l'âge réel et stockage du résultat dans une nouvelle colonne
csv$vrai_age <- 2019 - csv$age

data_age <- plot_ly(data = csv, x=~actual_age,type="histogram")

## SEXE
data_sexe <- plot_ly(data = csv, x=~genre,type="histogram")

## NATIONALITE
data_nat <- plot_ly(data = csv, x=~nationalite,type="histogram")

