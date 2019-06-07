csv <- fread("data_DC.csv", encoding = "Latin-1")

# Deux packages pour pouvoir visualiser plusieurs graphiques sur une même vue
install.packages("gridExtra")
library("gridExtra")
install.packages("cowplot")
library("cowplot")

library(ggplot2)
library(dplyr)

# Rename columns with Dplyr package.
csv <- csv %>% rename(
  NiveauCreatifAvantDC = "Comment..valuez.vous.votre.niveau.avant.Digital.Campus.dans.les.domaines.suivants...Cr.atif.",
  NiveauCreatifApresDC = "Comment..valuez.vous.votre.niveau.apr.s.Digital.Campus.dans.les.domaines.suivants...Cr.atif.",
  NiveauMarketingAvantDC = "Comment..valuez.vous.votre.niveau.avant.Digital.Campus.dans.les.domaines.suivants...Marketing.et.communication.",
  NiveauMarketingApresDC = "Comment..valuez.vous.votre.niveau.apr.s.Digital.Campus.dans.les.domaines.suivants...Marketing.et.communication.",
  NiveauDeveloppementAvantDC = "Comment..valuez.vous.votre.niveau.avant.Digital.Campus.dans.les.domaines.suivants...Developpement.",
  NiveauDeveloppementApresDC = "Comment..valuez.vous.votre.niveau.apr.s.Digital.Campus.dans.les.domaines.suivants...Developpement.",
  NiveauGestionProjetAvantDC = "Comment..valuez.vous.votre.niveau.avant.Digital.Campus.dans.les.domaines.suivants...Gestion.de.projet.",
  NiveauGestionProjetApresDC = "Comment..valuez.vous.votre.niveau.apr.s.Digital.Campus.dans.les.domaines.suivants...Gestion.de.projet."
)
colnames(csv)

# init color graph
colors <- c("#0073C2FF", "#EFC000FF", "#008686FF")

# Analyse du niveau créatif avant/après.

ComparerNiveauCreatifAvantApres <- function() {
  niveauCreatifAvantDC <- csv%>%
    filter(!is.na(NiveauCreatifAvantDC)) %>%
    group_by(NiveauCreatifAvantDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauCreatifAvantDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauCreatifAvantDC, aes(x = "", y = pct, fill = NiveauCreatifAvantDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauCreatifApresDC <- csv%>%
    filter(!is.na(NiveauCreatifApresDC)) %>%
    group_by(NiveauCreatifApresDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauCreatifApresDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauCreatifApresDC, aes(x = "", y = pct, fill = NiveauCreatifApresDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau créatif avant", "Niveau créatif après"), ncol = 2, nrow = 1))
}

# Analyse du niveau Marketing et communication avant/après.

ComparerNiveauMarketingAvantApres <- function() {
  niveauMarketingAvantDC <- csv%>%
    filter(!is.na(NiveauMarketingAvantDC)) %>%
    group_by(NiveauMarketingAvantDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauMarketingAvantDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauMarketingAvantDC, aes(x = "", y = pct, fill = NiveauMarketingAvantDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauMarketingApresDC <- csv%>%
    filter(!is.na(NiveauMarketingApresDC)) %>%
    group_by(NiveauMarketingApresDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauMarketingApresDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauMarketingApresDC, aes(x = "", y = pct, fill = NiveauMarketingApresDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau Marketing avant", "Niveau Marketing après"), ncol = 2, nrow = 1))
}

# Analyse du niveau développement avant/après.

ComparerNiveauDeveloppementAvantApres <- function() {
  niveauDeveloppementAvantDC <- csv%>%
    filter(!is.na(NiveauDeveloppementAvantDC)) %>%
    group_by(NiveauDeveloppementAvantDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauDeveloppementAvantDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauDeveloppementAvantDC, aes(x = "", y = pct, fill = NiveauDeveloppementAvantDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauDeveloppementApresDC <- csv%>%
    filter(!is.na(NiveauDeveloppementApresDC)) %>%
    group_by(NiveauDeveloppementApresDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauDeveloppementApresDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauDeveloppementApresDC, aes(x = "", y = pct, fill = NiveauDeveloppementApresDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau Developpement avant", "Niveau Developpement après"), ncol = 2, nrow = 1))
}

# Analyse du niveau développement avant/après.

ComparerNiveauGestionProjetAvantApres <- function() {
  niveauGestionProjetAvantDC <- csv%>%
    filter(!is.na(NiveauGestionProjetAvantDC)) %>%
    group_by(NiveauGestionProjetAvantDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauGestionProjetAvantDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot1 <- ggplot(niveauGestionProjetAvantDC, aes(x = "", y = pct, fill = NiveauGestionProjetAvantDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  niveauGestionProjetApresDC <- csv%>%
    filter(!is.na(NiveauGestionProjetApresDC)) %>%
    group_by(NiveauGestionProjetApresDC) %>%
    summarise(nb = n()) %>%
    mutate(pct = round(nb/sum(nb)*100)) %>%
    arrange(desc(NiveauGestionProjetApresDC)) %>%
    mutate(lab_ypos = cumsum(pct) - 0.5 * pct)
  
  plot2 <- ggplot(niveauGestionProjetApresDC, aes(x = "", y = pct, fill = NiveauGestionProjetApresDC)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = lab_ypos, label = pct), color = "white") +
    scale_fill_manual(values = colors) +
    theme_void()
  
  return(plot_grid(plot1, plot2, labels=c("Niveau Gestion de Projet avant", "Niveau Gestion deProjet après"), ncol = 2, nrow = 1))
}

# Call functions
ComparerNiveauCreatifAvantApres()
ComparerNiveauMarketingAvantApres()
ComparerNiveauDeveloppementAvantApres()
ComparerNiveauGestionProjetAvantApres()
