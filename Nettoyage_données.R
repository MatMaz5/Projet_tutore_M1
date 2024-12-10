## Paramétrage du chemin de l'espace de travail :
# setwd("/Users/enzo/Documents/Enzo/Master/Projet tutoré")


## Librairies :
# Installation :
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("readxl")

# Chargement :
library(ggplot2)
library(stringr)
library(readxl)

## Importation des données :
# data_brut <- read_xlsx("donnees_CINEDESIM.xlsx", col_names = FALSE)
# Fonctionne mal par la suite, on préfère donc convertir le fichier en .csv dès le début
data_brut <- read.csv("donnees_CINEDESIM.csv", header = FALSE, encoding = "UTF-8", sep = ";")

## Stockage des dimensions du data frame
n_brut <- dim(data_brut)[1]
p_brut <- dim(data_brut)[2]


## Renommage des colonnes du data frame
# Nouveaux noms de colonne :
data_brut[2,] <- c("nom_batch", "temp", "n_demande", rep(c("epi_maj", "locus", "bille", "serologie", "MFI", "PRA", "prozone", "origine"), 2),
                   "date", "heure_d", "heure_f", "duree", "nb_util_col", "monet", "vol_pla_prescrit", "vol_pla_traite", "ddn", "poids", "taille",
                   "sexe", "grossesse", "greffe_ant", "type_greffe", "type_donneur", "transfusion", "unites", "transplant", "date_greffe")

# Recherche de l'emplacement dans lequel se trouve "CLASSE I" et "CLASSE II" :
c1 <- which(data_brut[1,] == "CLASSE I")
c2 <- which(data_brut[1,] == "CLASSE II")

# Création du préfixe permettant de différentier les 2 classes :
prefixe <- c(rep("", c1 - 1), rep("C1.", c2 - c1), rep("C2.", c2 - c1), rep("", length((c2 + c2 - c1):p_brut)))
nom_col <- paste0(prefixe, data_brut[2,])
data <- data_brut[-c(1, 2), ]

# Attribution des noms de colonne/variables :
colnames(data) <- nom_col


## Nettoyage du jeu de données
# Suppression des dernières lignes (lignes de légendes) :
data <- data[which(data$nom_batch!=""), ]

# Suppression des colonnes inutiles :
data <- data[, -c(3, 5, 6, 11, 13, 14, 19, 34)]

# Suppression des patients 1/5 et 1/10 et des Prozone :
# data <- data[-which(str_detect(data$nom_batch, '1/10') == TRUE), ]
# data <- data[-which(str_detect(data$nom_batch, '/') == TRUE && data$C1.prozone == "Oui"), ]
# Règle de décision pour le retrait des prozone ? car C1 et C2 peuvent etre differents

# Pour l'instant on retire donc toutes les dilutions
data <- data[-which(str_detect(data$nom_batch, '/') == TRUE), ]


## Structuration des données :
n <- dim(data)[1]
p <- dim(data)[2]

# Harmonisation du nom des patients :
patient10 <- which(data$nom_batch == "Patient 10 ")[1]
data$nom_batch[1:patient10-1] <- substr(data$nom_batch[1:patient10-1], 1, 9)
data$nom_batch[patient10:n] <- substr(data$nom_batch[patient10:n], 1, 10)

# Harmonisation des transfusion
data$transfusion <- gsub("oui", "Oui", data$transfusion)

# Correction de l'erreur de saisie du patient 9, t23 -> t19 :
data$temp[which(data$nom_batch == "Patient 9" & data$temp == "t23")] <- "t19"

# Mise en facteur des variables quali :
data$nom_batch <- factor(data$nom_batch)
data$temp <- factor(data$temp)
data$C1.epi_maj <- factor(data$C1.epi_maj)
data$C2.epi_maj <- factor(data$C2.epi_maj)
data$sexe <- factor(data$sexe)
data$type_donneur <- factor(data$type_donneur)
data$transfusion <- factor(data$transfusion)

# Passage des variables quanti de caractère à numérique :
data$C1.MFI <- as.double(data$C1.MFI)
data$C2.MFI <- as.double(data$C2.MFI)
data$duree <- as.double(data$duree)
tab_tempo <- as.data.frame(str_split(data$heure_d, ":"))
data$heure_d <- as.integer(tab_tempo[1,]) + round(as.integer(tab_tempo[2,]) / 60, 2)
tab_tempo <- as.data.frame(str_split(data$heure_f, ":"))
data$heure_f <- as.integer(tab_tempo[1,]) + round(as.integer(tab_tempo[2,]) / 60, 2)
data$nb_util_col <- as.integer(data$nb_util_col)
data$grossesse <- as.integer(data$grossesse)
data$greffe_ant <- as.integer(data$greffe_ant)
data$unites <- as.integer(data$unites)
data$transplant <- as.integer(data$transplant)

# Correction des valeurs manquantes génantes :
# data$nom_batch[which(is.na(data$poids))]
# C'est le patient 8 qui possède des données manquantes, nous allons donc les compléter avec la moyenne du poids du patient
poids_moy <- signif(mean(data$poids[which(data$nom_batch == "Patient 8")], na.rm = TRUE), 3)
data$poids[which(is.na(data$poids))] <- poids_moy

ind_temp_NA <- which(is.na(data$duree) & !is.na(data$heure_d) & !is.na(data$heure_f))
temp_NA <- data[ind_temp_NA, 14:16]
data[ind_temp_NA, 14] <- (temp_NA[,2] - temp_NA[,1]) * 60


# Choix entre la valeur diluée ou non (effet prozone) :
  # Idée : faire une boucle qui parcours la colonne `C1.prozone` et qui supprime soit la vraie valeur soit la 1/5 suivant la réponse ("oui" ou "non"). 
# Pbs : 
  # - 2 colonnes prozone à traiter en simultanée `C1.prozone` et `C2.prozone` ;
  # - la réponse prozone est parfois renseignée une fois et d'autres fois 2.


## Création des data frame par patient : 
p1 <- subset(data, data$nom_batch == "Patient 1")
p2 <- subset(data, data$nom_batch == "Patient 2")
p3 <- subset(data, data$nom_batch == "Patient 3")
p4 <- subset(data, data$nom_batch == "Patient 4")
p5 <- subset(data, data$nom_batch == "Patient 5")
p6 <- subset(data, data$nom_batch == "Patient 6")
p7 <- subset(data, data$nom_batch == "Patient 7")
p8 <- subset(data, data$nom_batch == "Patient 8")
p9 <- subset(data, data$nom_batch == "Patient 9")
p10 <- subset(data, data$nom_batch == "Patient 10")

## Création d'un nouveau fichier de données
write.csv(data, file = "donnees_corrigees_CINEDESIM.csv")

