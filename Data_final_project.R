
print('Welcome to the final project of data schedule')

#super commentaire

library(readr)
csv <- read_delim("data_DC.csv", ";", 
                      escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                      trim_ws = TRUE)
View(csv)
# Remove duplicated column by index.
csv[,32] <- NULL

summary(csv)

#II) Analyse descriptive de leur parcours scolaire
#A Description du BAC
#i Type de BAC
#Regroude les type de bac en un dataframe
typeBac <- data.frame(csv[,9])
#compte le nombre de chaque type de bac
typeBac <- typeBac %>%
  group_by(Quel.type.de.Bac.avez.vous.obtenu..)%>%
  count()
typeBac <- typeBac %>%
  arrange(desc(Quel.type.de.Bac.avez.vous.obtenu..)) %>%
  mutate(lab.ypos = cumsum(freq) - 0.5*freq)
#graph avec type de bac
tb <- ggplot(typeBac, aes(x="", y=freq, fill=Quel.type.de.Bac.avez.vous.obtenu..))+
  geom_bar(width = 1, stat = "identity", color = "white")
#affichage en camembert pour que ça soit plus parlant
#on peut considerer les resultats comme des pourcentages
# comme le total est egal a 100
tb + coord_polar("y", start=0) +
  geom_text(aes(y = lab.ypos, label = paste0(freq,'%')), color = "white") +
  labs(fill = "Types de bac") + labs(x = "") + labs(y = "") +
  scale_fill_manual(values=c("#2AA4AC", "#8ED1D6", "#69AFBD", "#317279", "#44959B")) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  theme(panel.background = element_blank())

#ii Annee d'obtention du BAC
#impossible de savoir precisemment quand ils ont eu leur bac
#alors on va considerer qu'ils l'ont eu a 18 ans
AnneeObtentionBac <-  data.frame(csv$'Quelle est votre année de naissance ?' + 18)
names(AnneeObtentionBac)[1] <- 'annee_obtention'
#calcul du nombre de personne ayant obtenu le bac la meme annee
AnneeObtentionBac <- AnneeObtentionBac %>%
  group_by(annee_obtention)%>%
  count()
#affichage goem bar annee obtention
ggplot(data = AnneeObtentionBac, aes(x=annee_obtention, y=freq)) + 
  geom_bar(stat = 'identity', width = 1, color = "#2AA4AC") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Nombre d'étudiants en fonction de l'année d'obtention du bac") +
  theme(plot.title = element_text(size = 15, face = "bold"))

#II b)
#i Parcours DC, type de formation suivie
typeFormation <-  data.frame(csv$'Quelle formation avez-vous suivi à Digital Campus ?')
names(typeFormation)[1] <- 'type_formation'
#calcul du nombre de personnes en fonction du type de formation
typeFormation <- typeFormation %>%
  group_by(type_formation)%>%
  count()
#affichage goem bar type formation
ggplot(data = typeFormation, aes(x=type_formation, y=freq)) + 
  geom_bar(stat = 'identity', width = 1, fill = "#2AA4AC") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  ggtitle("Nombre d'étudiants en fonction du type de formation suivie à Digital Campus") +
  theme(plot.title = element_text(size = 12, face = "bold"))




