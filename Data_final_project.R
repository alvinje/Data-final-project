
print('Welcome to the final project of data schedule')

# Patch encoding Mac.
Sys.setlocale(category = "LC_ALL", locale = "fr_FR.UTF-8")
library(tidyverse)
library(plotly)
library(ggplot2)
library(data.table)
library(readr)

install.packages("gridExtra")
library("gridExtra")
install.packages("cowplot")
library("cowplot")

library(ggplot2)
library(dplyr)

setwd("D:/Desktop/Data-final-project")

csv = read_delim("data_DC.csv", ";",
                 escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"),
                 trim_ws = TRUE)
# get column names
colnames(csv)

# Remove duplicated column by index.
#csv[,32] <- NULL
colnames(csv)[32] <- "specialisation_pro_1"


# Rename columns with Dplyr package.
csv <- csv %>% rename(
  annee_naissance = "Quelle est votre année de naissance ?",
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
  reco_dc = "Recommanderiez-vous Digital Campus à d'autres personnes sur une échelle de 1 à 5 ?",
  recherche_emploi_apr_dc = "Avez-vous cherché un emploi à la fin de vos études ?",
  non_pq = "Non, pourquoi ?",
  tps_recherche_emploi = "Combien de temps a duré votre recherche d'emploi ?",
  spe_pro = "Quelle est votre spécialisation professionnelle ?",
  type_contrat_apr_dc = "Quel était le type de contrat ?",
  tps_plein_partiel = "Était-ce à temps plein ou à temps partiel ?",
  salaire_embauche_apr_dc = "Quel était votre salaire à l'embauche ?",
  emploi_priv_pub = "Votre emploi était-il dans le privé ou dans le public ?",
  csp_apr_dc = "A quelle catégorie socio-professionnelle apparteniez-vous ?",
  type_entreprise = "Dans quel type d'entreprise étiez-vous?",
  dept_premier_emploi = "Dans quel département avez-vous trouvé votre premier emploi ?",
  milieu = "Dans quel milieu ?",
  change_ville_pr_emploi = "Aviez-vous changé de ville pour décrocher cet emploi ?",
  cmt_trouve_emploi_apr_dc = "Comment aviez-vous trouvé votre travail ?",
  perception_recherche_emploi_apr_dc = "Comment aviez-vous perçu votre recherche d'emploi ?",
  satisfaction_premier_emploi = "Aviez-vous été satisfait de ce premier emploi?",
  place_travail_vie = "Quelle place occupe votre travail dans votre vie?",
  estimation_importance_dc_recrutement = "A combien estimez vous l'importance de digital campus dans votre recrutement?"
)

colnames(csv)

# Adapt data to factor if necessary.
col_departements_is_factor = c("dept_premier_emploi", "departement_travail")
for (col in colnames(csv)){
  if ( class(csv[[col]]) == "character" || col %in% col_departements_is_factor){
    csv[[col]] = as.factor(csv[[col]])
  }
}


class(csv$nationalite)

# Change 996 to 1996 due to an error of user.
csv <- csv %>% mutate(annee_naissance = ifelse(annee_naissance == 996, 1996, annee_naissance))

# Change value of nationalite for multiple cases (ifelse imbriqued).
csv <- csv %>% mutate(nationalite = ifelse(nationalite == "ALLEMAND", "DE",
                                           ifelse(nationalite == "FRANCAISE" | nationalite == "France", "FR", as.character(nationalite))))

# Change gender corrupted data.
csv <- csv %>% mutate(genre = ifelse(genre == "femme", "f",
                                     ifelse(genre == "homme", "h", as.character(genre))))

# Change gender corrupted data.
csv <- csv %>% mutate(genre = ifelse(genre == "femme", "f",
                                     ifelse(genre == "homme", "h", as.character(genre))))

# Change formation_DC corrupted data.
csv <- csv %>% mutate(formation_DC = ifelse(formation_DC == "Bacelor", "Bachelor", as.character(formation_DC)))


# Change gender corrupted data.
csv <- csv %>% mutate(ville_formation_DC = ifelse(ville_formation_DC == "lyon", "Lyon",
                                                  ifelse(ville_formation_DC == "paris", "Paris", as.character(ville_formation_DC))))

# Delete annee_naissance = 1904
csv2 <- with(csv, (annee_naissance == "1904"))
csv <- csv[!csv2, ]

# Set data as factor.
csv$nationalite <- as.factor(csv$nationalite)
csv$genre <- as.factor(csv$genre)
csv$formation_DC <- as.factor(csv$formation_DC)
csv$ville_formation_DC <- as.factor(csv$ville_formation_DC)

# Remove NA from annee naissance.
csv <- csv %>% filter(!is.na(annee_naissance))
summary(csv)
head(csv)

############## SANDRA PART ##############
## PROFIL DES REPONDANTS ##

## AGE
#annee_naissance <- csv %>%
#  group_by('age') %>%
#  count()

#naiss <- csv$age
#evt <- 2019
#time_length(interval(naiss, evt))

# Calcul de l'âge réel et stockage du résultat dans une nouvelle colonne
csv$age <- 2019 - csv$annee_naissance

data_age <- plot_ly(data = csv, x=~age,type="histogram")
test <- plot_ly(data = csv, x=~age,type="scatter", mode = "lines")

## SEXE
data_sexe <- plot_ly(data = csv, x=~genre,type="histogram")

## NATIONALITE
data_nat <- plot_ly(data = csv, x=~nationalite,type="histogram")

########## REMI PART ##########

# init color graph
colors <- c("#0073C2FF", "#EFC000FF", "#008686FF")

# Analyse du niveau cr�atif avant/apr�s.

ComparerNiveauCreatifAvantApres <- function() {
  niveauCreatifAvantDC <- csv%>%
    filter(!is.na(niveau_avt_dc_crea)) %>%
    group_by(niveau_avt_dc_crea) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_avt_dc_crea)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauCreatifAvantDC, aes(x = "", y = pct, fill = niveau_avt_dc_crea)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauCreatifApresDC <- csv%>%
    filter(!is.na(niveau_apr_dc_crea)) %>%
    group_by(niveau_apr_dc_crea) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_apr_dc_crea)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauCreatifApresDC, aes(x = "", y = pct, fill = niveau_apr_dc_crea)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau cr�atif avant", "Niveau cr�atif apr�s"), ncol = 2, nrow = 1))
}

# Analyse du niveau Marketing et communication avant/apr�s.

ComparerNiveauMarketingAvantApres <- function() {
  niveauMarketingAvantDC <- csv%>%
    filter(!is.na(niveau_avt_dc_mkt_com)) %>%
    group_by(niveau_avt_dc_mkt_com) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_avt_dc_mkt_com)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauMarketingAvantDC, aes(x = "", y = pct, fill = niveau_avt_dc_mkt_com)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauMarketingApresDC <- csv%>%
    filter(!is.na(niveau_apr_dc_mkt_com)) %>%
    group_by(niveau_apr_dc_mkt_com) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_apr_dc_mkt_com)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauMarketingApresDC, aes(x = "", y = pct, fill = niveau_apr_dc_mkt_com)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau Marketing avant", "Niveau Marketing aprds"), ncol = 2, nrow = 1))
}

# Analyse du niveau d�veloppement avant/apr�s.

ComparerNiveauDeveloppementAvantApres <- function() {
  niveauDeveloppementAvantDC <- csv%>%
    filter(!is.na(niveau_avt_dc_dev)) %>%
    group_by(niveau_avt_dc_dev) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_avt_dc_dev)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauDeveloppementAvantDC, aes(x = "", y = pct, fill = niveau_avt_dc_dev)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauDeveloppementApresDC <- csv%>%
    filter(!is.na(niveau_apr_dc_dev)) %>%
    group_by(niveau_apr_dc_dev) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_apr_dc_dev)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauDeveloppementApresDC, aes(x = "", y = pct, fill = niveau_apr_dc_dev)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau Developpement avant", "Niveau Developpement apr�s"), ncol = 2, nrow = 1))
}

# Analyse du niveau d�veloppement avant/apr�s.

ComparerNiveauGestionProjetAvantApres <- function() {
  niveauGestionProjetAvantDC <- csv%>%
    filter(!is.na(niveau_avt_dc_gest)) %>%
    group_by(niveau_avt_dc_gest) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_avt_dc_gest)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauGestionProjetAvantDC, aes(x = "", y = pct, fill = niveau_avt_dc_gest)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauGestionProjetApresDC <- csv%>%
    filter(!is.na(niveau_apr_dc_gest)) %>%
    group_by(niveau_apr_dc_gest) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(niveau_apr_dc_gest)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauGestionProjetApresDC, aes(x = "", y = pct, fill = niveau_apr_dc_gest)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau Gestion de Projet avant", "Niveau Gestion de Projet apr�s"), ncol = 2, nrow = 1))
}

# Call functions
ComparerNiveauCreatifAvantApres()
ComparerNiveauMarketingAvantApres()
ComparerNiveauDeveloppementAvantApres()
ComparerNiveauGestionProjetAvantApres()
