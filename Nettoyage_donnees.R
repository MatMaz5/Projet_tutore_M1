## Paramétrage du chemin de l'espace de travail :
# setwd("/Users/enzo/Documents/Enzo/Master/Projet tutoré")


## Librairies :
# Installation :
# install.packages("stringr")
# install.packages("readxl")

# Chargement :
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
# On conserve les données 1/10 pour les MFI classe I du patient 2 et les données 1/5 pour les données MFI classe 2 du patient 7
data[which(data$nom_batch == "Patient 2 "), 3:7] <- data[which(data$nom_batch == "Patient 2  1/10"), 3:7]
data[which(data$nom_batch == "Patient 7 "), 8:12] <- data[which(data$nom_batch == "Patient 7 1/5"), 8:12]
data <- data[-which(str_detect(data$nom_batch, '/') == TRUE), ]


## Structuration des données :
n <- dim(data)[1]
p <- dim(data)[2]

# Harmonisation du nom des patients :
patient10 <- which(data$nom_batch == "Patient 10 ")[1]
data$nom_batch[1:patient10-1] <- substr(data$nom_batch[1:patient10-1], 1, 9)
data$nom_batch[patient10:n] <- substr(data$nom_batch[patient10:n], 1, 10)

# Harmonisation des transfusions
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
poids_moy <- round(mean(data$poids[which(data$nom_batch == "Patient 8")], na.rm = TRUE), 2)
data$poids[which(is.na(data$poids))] <- poids_moy

ind_temp_NA <- which(is.na(data$duree) & !is.na(data$heure_d) & !is.na(data$heure_f))
temp_NA <- data[ind_temp_NA, 14:16]
data[ind_temp_NA, 16] <- round((temp_NA[,2] - temp_NA[,1]) * 60, 2)

# Correction du format de la date du patient 6
data[which(data$nom_batch == "Patient 6"),]$date[c(1, 2)] <- "10-07-19"
data[which(data$nom_batch == "Patient 2"),]$date[16] <- "04-11-19"

# Mise en forme des dates (séance et date de naissance)
data$date <- as.Date(data$date, "%m-%d-%y")

ddn <- as.data.frame(strsplit(data$ddn, "-"))
ddn[3,] <- paste0("19", ddn[3,])
ddn <- paste0(ddn[1,], "-", ddn[2,], "-", ddn[3,])
ddn <- as.Date(ddn, "%m-%d-%Y")
data$ddn <- ddn

## Ajout de nouvelles variables 
semaine <- rep(c(rep(1, 10), rep(2, 10)), 10)
# dose <- rep(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10), 10)
dose <- rep(c(1, ".", 2, ".", 3, ".", 4, ".", 5, ".", 6, ".", 7, ".", 8, ".", 9, ".", 10, "."), 10)
age <- as.integer(round((data$date - data$ddn) / 365.25, 0))
data <- cbind(data, age, semaine, dose)

# Création de data frame par patient pour plus de facilité
liste_patients <- list()
for (patient in levels(data$nom_batch)){
  i <- which(levels(data$nom_batch) == patient)
  liste_patients[[i]] <- subset(data, data$nom_batch == patient)
}

nb_patients <- length(levels(data$nom_batch))

for (n_patient in 1:nb_patients){
  # Correction des dànnées temporelles afin de pouvoir calculer une variable temps
  # Correction des durées de séance manquantes
  liste_patients[[n_patient]]$duree[which(is.na(liste_patients[[n_patient]]$duree))] <- round(mean(liste_patients[[n_patient]]$duree, na.rm = TRUE), 2)
  # Ajout d'une heure de début de séance s'il n'y en a pas 
  ind_heure_d_NA <- is.na(liste_patients[[n_patient]]$heure_d)
  if (sum(ind_heure_d_NA) > 0) {
    liste_patients[[n_patient]]$heure_d[ind_heure_d_NA] <- round(mean(liste_patients[[n_patient]]$heure_d, na.rm = TRUE), 2)
  }
  # Correction des heures de fin de séance à l'aide des heures de début et de la durée de la séance
  ind_heure_f_NA <- which(is.na(liste_patients[[n_patient]]$heure_f))
  liste_patients[[n_patient]]$heure_f[ind_heure_f_NA] <- liste_patients[[n_patient]]$heure_d[ind_heure_f_NA] + round(liste_patients[[n_patient]]$duree[ind_heure_f_NA] / 60, 2)
  # Calcul de la variable temps (depuis le debut du protocole)
  ecart_date <- diff(liste_patients[[n_patient]]$date)[seq(2,18, 2)]
  temps_entre_seances <- c(0, (24 * ecart_date) + liste_patients[[n_patient]]$heure_d[seq(3, 19, 2)] - liste_patients[[n_patient]]$heure_f[seq(2, 18, 2)])
  temps_seances <- round((liste_patients[[n_patient]]$heure_f[seq(1, 19, 2)] - liste_patients[[n_patient]]$heure_d[seq(1, 19, 2)]), 2)
  temps <- c(0)
  nb_seances <- dim(liste_patients[[n_patient]])[1] / 2
  for(i in 1:nb_seances){
    temps <- c(temps, temps[length(temps)] + temps_entre_seances[i])
    temps <- c(temps, temps[length(temps)] + temps_seances[i])
  }
  temps <- temps[-1]
  liste_patients[[n_patient]] <- cbind(liste_patients[[n_patient]], temps)
}

data <- liste_patients[[1]]
for (i in 2:nb_patients){
  data <- rbind(data, liste_patients[[i]])
}

# Calcul des delta MFI
liste_patients <- list()
for (patient in levels(data$nom_batch)){
  i <- which(levels(data$nom_batch) == patient)
  liste_patients[[i]] <- subset(data, data$nom_batch == patient)
}

delta_MFI = function(df) {
  delta1 = numeric(20)
  delta2 = numeric(20)
  for (i in 1:10){
    delta1[2*i-1] <- df$C1.MFI[2*i] - df$C1.MFI[2*i-1]
    delta1[2*i] <- df$C1.MFI[2*i] - df$C1.MFI[2*i-1]
    delta2[2*i-1] <- df$C2.MFI[2*i] - df$C2.MFI[2*i-1]
    delta2[2*i] <- df$C2.MFI[2*i] - df$C2.MFI[2*i-1]
  }
  df$deltaMFI1 <- delta1
  df$deltaMFI2 <- delta2
  return(df[-seq(1,20,2),])
}

liste_delta <-lapply(liste_patients, delta_MFI)

deltaMFI1 <- rep(liste_delta[[1]][,36], each = 2)
deltaMFI2 <- rep(liste_delta[[1]][,37], each = 2)

for (i in 2:nb_patients){
  deltaMFI1 <- c(deltaMFI1, rep(liste_delta[[i]][,36], each = 2))
  deltaMFI2 <- c(deltaMFI2, rep(liste_delta[[i]][,37], each = 2))
}

data <- cbind(data, deltaMFI1, deltaMFI2)

## Création d'un nouveau fichier de données
write.csv(data, file = "donnees_corrigees_CINEDESIM.csv")


var_conservees_monolix <- c("nom_batch", "C1.MFI", "C2.MFI", "vol_pla_traite",
                            "age", "poids", "taille", "sexe", "grossesse", "greffe_ant",
                            "transfusion", "semaine", "dose", "temps")
data_monolix <- data[, var_conservees_monolix]

write.csv(data_monolix, file = "donnees_corrigees_CINEDESIM_MONOLIX.csv")
