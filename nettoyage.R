setwd("/Users/matmaz/Master/GitHub/Projet_tutore_M1")


# Librairies :
library(readxl)
library(ggplot2)



# Importation des données :
data_brut <- read_excel("/Users/matmaz/Master/M1_SSD/projet_tutore/data.xlsx", col_names = FALSE)
data <- data_brut



# Stocker les dimensions de la data frame :
n <- dim(data_brut)[1]
p <- dim(data_brut)[2]


# Trouver l'emplacement dans lequel se trouve "CLASSE I" et "CLASSE II" :
c1 <- which(data_brut[1,] == "CLASSE I")
c2 <- which(data_brut[1,] == "CLASSE II")

# Créer un préfixe à ajouter à chaque termes de la $2^e$ ligne afin de pouvoir faire une ligne de 
# noms de variables à `data` à partir des 2 $1^e$ lignes de `data_brut` :
prefixe <- c(rep("", c1 - 1), rep("C1.", c2 - c1), rep("C2.", c2 - c1), rep("", length((c2 + c2 - c1):p)))
nom_col <- paste0(prefixe, data_brut[2,])
data <- data[-c(1, 2), ]
colnames(data) <- nom_col



# Suppression des dernières lignes (lignes de légendes) :

data <- subset(data, is.na(data$`Nom du Batch`) == FALSE)


# Suppression de colonnes inutiles :
data <- data[, -3]

# Suppression des patients 1/5 et 1/10 et des Prozone :
data <- data[1:211,]
data <- data[-c(1,22,43,64,85,106,107,128,149,170,191),]


# ATTENTION : valeur manquante "t19" pour le patient 9 :
data[179,2] <- "t19"


# Structuration des données :
data$`Nom du Batch` <- factor(data$`Nom du Batch`, levels = 
                                c("Patient 1", "Patient 2", "Patient 3", "Patient 4", 
                                  "Patient 5", "Patient 6", "Patient 7", "Patient 8", 
                                  "Patient 9", "Patient 10"))
data$Time <- factor(data$Time, levels = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", 
                                          "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20"))

data$`C1.Epitope majoritaire` <- factor(data$`C1.Epitope majoritaire`)
data$`C1.Type de locus` <- factor(data$`C1.Type de locus`)
data$`C1.Type de Bille` <- factor(data$`C1.Type de Bille`)
data$C1.Sérologique <- factor(data$C1.Sérologique)
data$C1.MFI <- as.double(data$C1.MFI)
data$`C1.PRA CLASSE I` <- as.double(data$`C1.PRA CLASSE I`)
data$C1.Prozone <- factor(data$C1.Prozone)
data$`C1.Origine Immunisation` <- factor(data$`C1.Origine Immunisation`)

data$`C2.Epitope majoritaire` <- factor(data$`C2.Epitope majoritaire`)
data$`C2.Type de locus` <- factor(data$`C2.Type de locus`)
data$`C2.Type de Bille` <- factor(data$`C2.Type de Bille`)
data$C2.Sérologique <- factor(data$C2.Sérologique)
data$C2.MFI <- as.double(data$C2.MFI)
data$`C2.PRA CLASSE II` <- as.double(data$`C2.PRA CLASSE II`)
data$C2.Prozone <- factor(data$C2.Prozone)
data$`C2.Origine Immunisation` <- factor(data$`C2.Origine Immunisation`)

data$`Durée de la seance (min )` <- as.double(data$`Durée de la seance (min )`)
data$`Nombre d'utilisation de la colonne` <- as.integer(data$`Nombre d'utilisation de la colonne`)
data$`Monet : O/N` <- factor(data$`Monet : O/N`)
data$`Volume plasma prescrit` <- as.double(data$`Volume plasma prescrit`)
data$`Volume plasma traite` <- as.double(data$`Volume plasma traite`)

data$`Poids (kg)` <- as.double(data$`Poids (kg)`)
data$`Taille (cm)` <- as.double(data$`Taille (cm)`)
data$Sexe <- factor(data$Sexe)
data$Grossesses <- as.integer(data$Grossesses)
data$`Greffe anterieur` <- as.integer(data$`Greffe anterieur`)
data$Transfusion <- factor(data$Transfusion)
data$Unités <- as.integer(data$Unités)
data$Transplantectomie <- as.integer(data$Transplantectomie)

# Pbs : `NAs` introduit dans les colonnes suivantes : `C1.MFI`, `C1.PRA CLASSE I`, `C2.MFI`, 
# `C2.PRA CLASSE II`, `Nombre d'utilisation de la colonne`, `Volume plasma prescrit`, 
# `Volume plasma traite`, `Poids (kg)`, `Grossesses`, `Unités` et `Transplantectomie`.


# Pas sûr pour ceux-là :
  # data$`Date séance` <- as.integer(data$`Date séance`)
  # data$`Heure début (branchement)` <- as.double(data$`Heure début (branchement)`)
  # data$`Heure fin (débranchement)` <- as.double(data$`Heure fin (débranchement)`)
  # data$DDN <- as.integer(data$DDN)




# Choix entre la valeur diluée ou non (effet prozone) :
  # Idée : faire une boucle qui parcours la colonne `C1.prozone` et qui supprime soit la vraie valeur soit la 1/5 suivant la réponse ("oui" ou "non"). 
# Pbs : 
  # - 2 colonnes prozone à traiter en simultanée `C1.prozone` et `C2.prozone` ;
  # - la réponse prozone est parfois renseignée une fois et d'autres fois 2.



# Création des data frame par patient : 
p1 <- subset(data, data$`Nom du Batch` == "Patient 1")
p2 <- subset(data, data$`Nom du Batch` == "Patient 2")
p3 <- subset(data, data$`Nom du Batch` == "Patient 3")
p4 <- subset(data, data$`Nom du Batch` == "Patient 4")
p5 <- subset(data, data$`Nom du Batch` == "Patient 5")
p6 <- subset(data, data$`Nom du Batch` == "Patient 6")
p7 <- subset(data, data$`Nom du Batch` == "Patient 7")
p8 <- subset(data, data$`Nom du Batch` == "Patient 8")
p9 <- subset(data, data$`Nom du Batch` == "Patient 9")
p10 <- subset(data, data$`Nom du Batch` == "Patient 10")


