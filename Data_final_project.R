
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
  labs(fill = "Type de bac") + labs(x = "") + labs(y = "")

#ii Annee d'obtention du BAC



