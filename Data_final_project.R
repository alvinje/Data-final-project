
print('Welcome to the final project of data schedule')

# Patch encoding Mac.
Sys.setlocale(category = "LC_ALL", locale = "fr_FR.UTF-8")
setwd("D:/Desktop/Data-final-project")


Packages <- c("dplyr", "tidyverse", "plotly", "ggplot2", "data.table", "readr", "questionr", "gridExtra", "cowplot")

# Set uninstalled package.
packagesToInstall <- Packages[!(Packages %in% installed.packages()[,"Package"])]

# Install uninstalled package if true.
if(length(new.packages)) install.packages(new.packages)

# Add dependency to packages.
lapply(Packages, library, character.only = TRUE)


# Load original Data csv.
#csv <- read_delim("data_DC.csv", ";",
#                      escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"),
#                      trim_ws = TRUE)


# Load renamed Data csv.
csv <- read_delim("renamedData.csv", ",",
                      escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"),
                      trim_ws = TRUE)

# get column names
colnames(csv)

# Remove duplicated column by index.
#csv[,32] <- NULL
#colnames(csv)[32] <- "specialisation_pro_1"


# Rename columns with Dplyr package.
#csv <- csv %>% rename(
#  annee_naissance = "Quelle est votre ann?e de naissance ?",
#  genre = "Vous ?tes ?",
#  nationalite = "Quel est votre nationalit? ?",
#  departement_travail = "Dans quel d?partement travaillez-vous ?",
#  situation_pro = "Quelle est votre situation professionnelle ?",
#  specialisation_pro = "Quelle est votre sp?cialisation professionnelle ?",
#  diplome_actuel =  "Quelle est votre niveau de dipl?me actuel ?",
#  tranche_salaire = "Quelle est votre tranche de salaire ?",
#  type_bac = "Quel type de Bac avez-vous obtenu ?",
#  annee_dernier_diplome_avant_DC = "En quelle ann?e avez-vous obtenu votre dernier dipl?me avant Digital Campus ?",
#  dernier_diplome_avant_dc = "Quel est votre dernier niveau d'?tude obtenu ?",
#  formation_DC = "Quelle formation avez-vous suivi ? Digital Campus ?",
#  ville_formation_DC = "Dans quelle ville avez-vous fait votre formation Digital Campus ?",
#  annee_debut_DC = "En quelle ann?e avez-vous rejoint Digital Campus ?",
#  annee_fin_DC = "En quelle ann?e avez-vous quitt? Digital Campus ?",
#  niveau_avt_dc_crea = "Comment ?valuez-vous votre niveau avant Digital Campus dans les domaines suivants? [Cr?atif]",
#  niveau_avt_dc_mkt_com = "Comment ?valuez-vous votre niveau avant Digital Campus dans les domaines suivants? [Marketing et communication]",
#  niveau_avt_dc_dev = "Comment ?valuez-vous votre niveau avant Digital Campus dans les domaines suivants? [Developpement]",
#  niveau_avt_dc_gest = "Comment ?valuez-vous votre niveau avant Digital Campus dans les domaines suivants? [Gestion de projet]",
#  niveau_apr_dc_crea = "Comment ?valuez-vous votre niveau apr?s Digital Campus dans les domaines suivants? [Cr?atif]",
#  niveau_apr_dc_mkt_com = "Comment ?valuez-vous votre niveau apr?s Digital Campus dans les domaines suivants? [Marketing et communication]",
#  niveau_apr_dc_dev = "Comment ?valuez-vous votre niveau apr?s Digital Campus dans les domaines suivants? [Developpement]",
#  niveau_apr_dc_gest = "Comment ?valuez-vous votre niveau apr?s Digital Campus dans les domaines suivants? [Gestion de projet]",
#  spe = "Quelle sp?cialit? avez-vous choisi ?",
#  spe_ok = "Votre sp?cialit? a-t-elle r?pondu ? vos attentes ?",
#  tps_trouve_alt = "En combien de temps avez-vous trouv? votre alternance ?",
#  sourcing_alt = "Comment avez-vous trouv? votre alternance ?",
#  reco_dc = "Recommanderiez-vous Digital Campus ? d'autres personnes sur une ?chelle de 0 ? 5 ?",
#  recherche_emploi_apr_dc = "Avez-vous cherch? un emploi ? la fin de vos ?tudes ?",
#  non_pq = "Non, pourquoi ?",
#  tps_recherche_emploi = "Combien de temps a dur? votre recherche d'emploi ?",
#  spe_pro = "Quelle est votre sp?cialisation professionnelle ?",
#  type_contrat_apr_dc = "Quel ?tait le type de contrat ?",
#  tps_plein_partiel = "?tait-ce ? temps plein ou ? temps partiel ?",
#  salaire_embauche_apr_dc = "Quel ?tait votre salaire ? l'embauche ?",
#  emploi_priv_pub = "Votre emploi ?tait-il dans le priv? ou dans le public ?",
#  csp_apr_dc = "A quelle cat?gorie socio-professionnelle apparteniez-vous ?",
#  type_entreprise = "Dans quel type d'entreprise ?tiez-vous?",
#  dept_premier_emploi = "Dans quel d?partement avez-vous trouv? votre premier emploi ?",
#  milieu = "Dans quel milieu ?",
#  change_ville_pr_emploi = "Aviez-vous chang? de ville pour d?crocher cet emploi ?",
#  cmt_trouve_emploi_apr_dc = "Comment aviez-vous trouv? votre travail ?",
#  perception_recherche_emploi_apr_dc = "Comment aviez-vous per?u votre recherche d'emploi ?",
#  satisfaction_premier_emploi = "Aviez-vous ?t? satisfait de ce premier emploi?",
#  place_travail_vie = "Quelle place occupe votre travail dans votre vie?",
#  estimation_importance_dc_recrutement = "A combien estimez vous l'importance de digital campus dans votre recrutement?"
#)

# Export data to a CSV file to avoid unexcepted error on rename sentence.
#write.csv(csv, "D:/Desktop/Data-final-project/renamedData.csv", row.names = FALSE)



##### DATA TRAITMENT #####
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
temp_csv <- with(csv, (annee_naissance == "1904"))
csv <- csv[!temp_csv, ]
rm(temp_csv)
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

# Calcul de l'?ge r?el et stockage du r?sultat dans une nouvelle colonne
csv$age <- 2019 - csv$annee_naissance

data_age <- plot_ly(data = csv, x=~age,type="histogram")

#Regroupe les ages en un dataframe
data_age <- data.frame(csv[,47])
#compte le nombre de chaque age
data_age <- data_age %>%
  group_by(age)%>%
  count()
data_age <- data_age %>%
  arrange(desc(age)) %>%
  mutate(lab.ypos = cumsum(n) - 0.5*n)
#graph avec age
tb_age <- ggplot(data_age, aes(x="", y=n, fill=age))+
  geom_bar(width = 1, stat = "identity", color = "white")

# Cr?er une variable de position
age_data <- csv %>%
  filter(!is.na(age)) %>%
  group_by(age) %>%
  summarise(nb = n()) %>%
  mutate(pct = round(nb / sum(nb) * 100)) %>%
  arrange(desc(age)) %>%
  mutate(lab_ypos = cumsum(pct) - 0.5 * pct)

#camembert avec genre et labels en pourcentages
tb_age2 <- ggplot(age_data, aes(x="", y=nb, fill=age))+
  geom_bar(width = 1, stat = "identity", color = "white")+
  coord_polar("y", start=0) +
  geom_text(aes(y = lab_ypos, label = paste0(pct,'%')), color = "white") +
  labs(fill = "Age") + labs(x = "") + labs(y = "")

# histogramme répartition âge
data_test <- plot_ly(data = age_data, x=~age,y=~nb,type="bar")

## SEXE
data_sexe <- plot_ly(data = csv, x=~genre,type="histogram")

# Cr?er une variable de position
genre_data <- csv %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(nb = n()) %>%
  mutate(pct = round(nb / sum(nb) * 100)) %>%
  arrange(desc(genre)) %>%
  mutate(lab_ypos = cumsum(pct) - 0.5 * pct)

#graph avec genre et labels en pourcentages
tb_sexe2 <- ggplot(genre_data, aes(x="", y=nb, fill=genre))+
  geom_bar(width = 1, stat = "identity", color = "white")+
  coord_polar("y", start=0) +
  geom_text(aes(y = lab_ypos, label = paste0(pct,'%')), color = "white") +
  labs(fill = "Sexe") + labs(x = "") + labs(y = "")


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

  return(plot_grid(plot1, plot2, labels=c("Niveau cr?atif avant", "Niveau cr?atif apr?s"), ncol = 2, nrow = 1))
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

# Analyse du niveau d?veloppement avant/apr?s.

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

  return(plot_grid(plot1, plot2, labels=c("Niveau Developpement avant", "Niveau Developpement apr?s"), ncol = 2, nrow = 1))
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

  return(plot_grid(plot1, plot2, labels=c("Niveau Gestion de Projet avant", "Niveau Gestion de Projet apr?s"), ncol = 2, nrow = 1))
}

# Call functions
ComparerNiveauCreatifAvantApres()
ComparerNiveauMarketingAvantApres()
ComparerNiveauDeveloppementAvantApres()
ComparerNiveauGestionProjetAvantApres()

############## MARGAUX PART ##############
#II. Analyse descriptive de leur parcours scolaire
#A Description du BAC
#i Type de BAC
#Regroude les type de bac en un dataframe
typeBac <- data.frame(csv[,9])
#compte le nombre de chaque type de bac
typeBac <- typeBac %>%
  group_by(type_bac)%>%
  count()
typeBac <- typeBac %>%
  arrange(desc(type_bac)) %>%
  mutate(lab.ypos = cumsum(n) - 0.5*n)
#graph avec type de bac
tb <- ggplot(typeBac, aes(x="", y=n, fill=type_bac))+
geom_bar(width = 1, stat = "identity", color = "white")
#affichage en camembert pour que ça soit plus parlant
#on peut considerer les resultats comme des pourcentages
# comme le total est egal a 100
tb + coord_polar("y", start=0) +
  geom_text(aes(y = lab.ypos, label = paste0(n,'%')), color = "white") +
  labs(fill = "Types de bac") + labs(x = "") + labs(y = "") +
  scale_fill_manual(values=c("#2AA4AC", "#8ED1D6", "#69AFBD", "#317279", "#44959B")) +
  theme(axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid  = element_blank()) +
  theme(panel.background = element_blank())

#ii Annee d'obtention du BAC
#impossible de savoir precisemment quand ils ont eu leur bac
#alors on va considerer qu'ils l'ont eu a 18 ans
AnneeObtentionBac <-  data.frame(csv$annee_naissance + 18)
names(AnneeObtentionBac)[1] <- 'annee_obtention'
#calcul du nombre de personne ayant obtenu le bac la meme annee
AnneeObtentionBac <- AnneeObtentionBac %>%
  group_by(annee_obtention)%>%
  count()
#affichage goem bar annee obtention
ggplot(data = AnneeObtentionBac, aes(x=annee_obtention, y=n)) +
  geom_bar(stat = 'identity', width = 1, color = "#2AA4AC") +
  theme(axis.title.x = element_blank(),
  axis.title.y = element_blank()) +
  ggtitle("Nombre d'?tudiants en fonction de l'ann?e d'obtention du bac") +
  theme(plot.title = element_text(size = 15, face = "bold"))

#II b)
#i Parcours DC, type de formation suivie
typeFormation <-  data.frame(csv$formation_DC)
names(typeFormation)[1] <- 'type_formation'
#calcul du nombre de personnes en fonction du type de formation
typeFormation <- typeFormation %>%
  group_by(type_formation)%>%
  count()
#affichage goem bar type formation
ggplot(data = typeFormation, aes(x=type_formation, y=n)) +
  geom_bar(stat = 'identity', width = 1, fill = "#2AA4AC") +
  theme(axis.title.x = element_blank(),
  axis.title.y = element_blank()) +
  ggtitle("Nombre d'?tudiants en fonction du type de formation suivie ? Digital Campus") +
  theme(plot.title = element_text(size = 12, face = "bold"))