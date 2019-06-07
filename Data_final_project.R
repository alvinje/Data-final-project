
print('Welcome to the final project of data schedule')

#super commentaire

csv <- fread("data_DC.csv")

library(data.table)
library(plotly)
library(ggplot2)
library(dplyr)


#########

# Remove duplicated column by index.
csv[,32] <- NULL

# Rename columns with Dplyr package.
csv <- csv %>% rename(
  age = "Quelle est votre année de naissance ?",
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
  annee_fin_DC = "En quelle année avez-vous quitté Digital Campus ?",
  niveau_avt_dc_crea = "Comment Èvaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [CrÈatif]",
  niveau_avt_dc_mkt_com = "Comment Èvaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Marketing et communication]",
  niveau_avt_dc_dev = "Comment Èvaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Developpement]",
  niveau_avt_dc_gest = "Comment Èvaluez-vous votre niveau avant Digital Campus dans les domaines suivants? [Gestion de projet]",
  niveau_apr_dc_crea = "Comment Èvaluez-vous votre niveau aprËs Digital Campus dans les domaines suivants? [CrÈatif]",
  niveau_apr_dc_mkt_com = "Comment Èvaluez-vous votre niveau aprËs Digital Campus dans les domaines suivants? [Marketing et communication]",
  niveau_apr_dc_dev = "Comment Èvaluez-vous votre niveau aprËs Digital Campus dans les domaines suivants? [Developpement]",
  niveau_apr_dc_gest = "Comment Èvaluez-vous votre niveau aprËs Digital Campus dans les domaines suivants? [Gestion de projet]",
  spe = "Quelle spÈcialitÈ avez-vous choisi ?",
  spe_ok = "Votre spÈcialitÈ a-t-elle rÈpondu ‡ vos attentes ?",
  tps_trouve_alt = "En combien de temps avez-vous trouvÈ votre alternance ?",
  sourcing_alt = "Comment avez-vous trouvÈ votre alternance ?",
  reco_dc = "Recommanderiez-vous Digital Campus ‡ d'autres personnes sur une Èchelle de 0 ‡ 5 ?",
  recherche_emploi_apr_dc = "Avez-vous cherchÈ un emploi ‡ la fin de vos Ètudes ?",
  non_pq = "Non, pourquoi ?",
  tps_recherche_emploi = "Combien de temps a durÈ votre recherche díemploi ?",
  spe_pro = "Quelle est votre spÈcialisation professionnelle ?",
  type_contrat_apr_dc = "Quel Ètait le type de contrat ?",
  tps_plein_partiel = "…tait-ce ‡ temps plein ou ‡ temps partiel ?",
  salaire_embauche_apr_dc = "Quel Ètait votre salaire ‡ líembauche ?",
  emploi_priv_pub = "Votre emploi Ètait-il dans le privÈ ou dans le public ?",
  csp_apr_dc = "A quelle catÈgorie socio-professionnelle apparteniez-vous ?",
  type_entreprise = "Dans quel type díentreprise Ètiez-vous?",
  dept_premier_emploi = "Dans quel dÈpartement avez-vous trouvÈ votre premier emploi ?",
  milieu = "Dans quel milieu ?",
  change_ville_pr_emploi = "Aviez-vous changÈ de ville pour dÈcrocher cet emploi ?",
  cmt_trouve_emploi_apr_dc = "Comment aviez-vous trouvÈ votre travail ?",
  perception_recherche_emploi_apr_dc = "Comment aviez-vous perÁu votre recherche díemploi ?",
  satisfaction_premier_emploi = "Aviez-vous ÈtÈ satisfait de ce premier emploi?",
  place_travail_vie = "Quelle place occupe votre travail dans votre vie?",
  estimation_importance_dc_recrutement = "A combien estimez vous líimportance de digital campus dans votre recrutement?"
    )
colnames(csv)

##############
