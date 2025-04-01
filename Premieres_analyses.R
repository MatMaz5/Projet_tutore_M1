## Paramétrage du chemin de l'espace de travail :
# setwd("/Users/enzo/Documents/Enzo/Master/Projet tutoré")


## Librairies :
# Installation :
# install.packages("ggplot2")

# Chargement :
library(ggplot2)
library(GGally)

## Importation des données
data <- read.csv("donnees_corrigees_CINEDESIM.csv", header = TRUE, encoding = "UTF-8")[-1]

# Mise en facteur des variables quali :
data$nom_batch <- factor(data$nom_batch)
data$temp <- factor(data$temp)
data$C1.epi_maj <- factor(data$C1.epi_maj)
data$C2.epi_maj <- factor(data$C2.epi_maj)
data$sexe <- factor(data$sexe)
data$type_donneur <- factor(data$type_donneur)
data$transfusion <- factor(data$transfusion)
data$semaine <- factor(data$semaine, labels = c("Semaine 1", "Semaine 2"))

# Passage des variables quanti de caractère à numérique :
data$poids <- as.double(data$poids)
data$vol_pla_prescrit <- as.integer(data$vol_pla_prescrit)
data$vol_pla_traite <- as.integer(data$vol_pla_traite)

# Séparation en jeux de données par patient
liste_patients <- list()
for (patient in levels(data$nom_batch)){
  i <- which(levels(data$nom_batch) == patient)
  liste_patients[[i]] <- subset(data, data$nom_batch == patient)
}

## Graphiques des MFI et delta MFI
# MFI
graph_MFI1 <-  ggplot(data, aes(x = temps, y = C1.MFI, group=as.factor(nom_batch), color=as.factor(nom_batch))) +
  geom_line() +
  facet_grid(~semaine, scales = "free_x") +
  geom_hline(yintercept = 3000, color = "black", linetype = "dashed") +
  labs(x = "Temps en heures",
       y = "MFI classe I",
       color="Patient")
graph_MFI1


graph_MFI2 <-  ggplot(data, aes(x = temps, y = C2.MFI, group=as.factor(nom_batch), color=as.factor(nom_batch))) +
  geom_line() +
  facet_grid(~semaine, scales = "free_x") +
  geom_hline(yintercept = 3000, color = "black", linetype = "dashed") +
  labs(x = "Temps en heures",
       y = "MFI classe I",
       color="Patient")
graph_MFI2

# Delta MFI
# Creation d'un dataset adapté
df_deltaMFI <- data[seq(1, dim(data)[1], 2),]

# Représentation graphique
graph_delta_MFI1 <- ggplot(df_deltaMFI, aes(x=temps, y = - deltaMFI1, group=as.factor(nom_batch), color=as.factor(nom_batch))) +
  geom_line() +
  facet_grid(~semaine, scales = "free_x") +
  labs(x = "Séance",
       y = "Delta MFI Classe I",
       color="Patient") + 
  ylim(-2500,10000)
graph_delta_MFI1

mean(df_deltaMFI$deltaMFI1[seq(1, length(df_deltaMFI$deltaMFI1), 10)])
rep(df_deltaMFI$deltaMFI1, each = 2)

# Boxplot des Delta MFI par séance pour voir l'évolution générale
nb_patients <- length(levels(data$nom_batch))
nb_seances <- length(levels(data$temp)) / 2
boxplot(-df_deltaMFI$deltaMFI1~rep(1:nb_seances, nb_patients), xlab = "Séance", ylab = "Delta MFI Classe 1", main = "Évolution des delta MFI1 dans le temps", col = "lightblue")
boxplot(-df_deltaMFI$deltaMFI2~rep(1:nb_seances, nb_patients), xlab = "Séance", ylab = "Delta MFI Classe 2",  main = "Évolution des delta MFI2 dans le temps", col = "lightblue")

# Variations delta MFI dependantes de la durée de séance ? Moyenne / moyenne ou mediane / mediane
plot(-df_deltaMFI$deltaMFI1~df_deltaMFI$duree)
plot(-df_deltaMFI$deltaMFI2~df_deltaMFI$duree)

moy1 <- aggregate(cbind(-df_deltaMFI$deltaMFI1, df_deltaMFI$duree) , list(df_deltaMFI$nom_batch), mean, na.rm = TRUE)
moy2 <- aggregate(cbind(-df_deltaMFI$deltaMFI2, df_deltaMFI$duree) , list(df_deltaMFI$nom_batch), mean, na.rm = TRUE)

plot(moy1[,c(2,3)], xlab = "Delta MFI Classe 1 moyen", ylab = "Durée moyenne", main = "mean(deltaMFI1)~mean(duree)")
plot(moy2[,c(2,3)], xlab = "Delta MFI Classe 2 moyen", ylab = "Durée moyenne", main = "mean(deltaMFI2)~mean(duree)")

med1 <- aggregate(cbind(-df_deltaMFI$deltaMFI1, df_deltaMFI$duree) , list(df_deltaMFI$nom_batch), median, na.rm = TRUE)
med2 <- aggregate(cbind(-df_deltaMFI$deltaMFI2, df_deltaMFI$duree) , list(df_deltaMFI$nom_batch), median, na.rm = TRUE)

plot(med1[,c(2,3)], xlab = "Delta MFI Classe 1 médian", ylab = "Durée médianne", main = "median(deltaMFI1)~median(duree)")
plot(med2[,c(2,3)], xlab = "Delta MFI Classe 2 médian", ylab = "Durée médianne", main = "median(deltaMFI2)~median(duree)")


##
ggplot(df_deltaMFI, aes(x=temps, y = - deltaMFI1/duree, group=as.factor(nom_batch), color=as.factor(nom_batch))) +
  geom_line() +
  facet_grid(~semaine, scales = "free_x") +
  labs(x = "Séance",
       y = "Delta MFI Classe I / duree seance",
       color="Patient")


graph_delta_MFI2 <- ggplot(df_deltaMFI, aes(x=temps, y = - deltaMFI2, group=as.factor(nom_batch), color=as.factor(nom_batch))) +
  geom_line() +
  facet_grid(~semaine, scales = "free_x") +
  labs(x = "Séance",
       y = "Delta MFI Classe II",
       color="Patient")
graph_delta_MFI2


## Croisements avec nos variables d'intérêt
# Croisement entre le MFI1 initial et les covariables
palette <- rainbow(nb_patients)

# MFI1 Initiaux
plot(data$C1.MFI[seq(1, 181, 20)], col = palette, pch = 19, main = "Niveau du MFI1 Initial",
     ylab = "MFI1 Initial", xlab = "Patient")
legend("topright", legend = paste("Patient", 1:nb_patients), col = palette, pch = 19)

p1 <- liste_patients[[1]]
p10 <- liste_patients[[2]]
p2 <- liste_patients[[3]]
p3 <- liste_patients[[4]]
p4 <- liste_patients[[5]]
p5 <- liste_patients[[6]]
p6 <- liste_patients[[7]]
p7 <- liste_patients[[8]]
p8 <- liste_patients[[9]]
p9 <- liste_patients[[10]]

# MFI1 Initial X Durée
duree_MFI1 <- cbind(p1$duree[seq(1, 19, 2)]/p1$C1.MFI[1], p2$duree[seq(1, 19, 2)]/p2$C1.MFI[1],
                    p3$duree[seq(1, 19, 2)]/p3$C1.MFI[1], p4$duree[seq(1, 19, 2)]/p4$C1.MFI[1],
                    p5$duree[seq(1, 19, 2)]/p5$C1.MFI[1], p6$duree[seq(1, 19, 2)]/p6$C1.MFI[1],
                    p7$duree[seq(1, 19, 2)]/p7$C1.MFI[1], p8$duree[seq(1, 19, 2)]/p8$C1.MFI[1],
                    p9$duree[seq(1, 19, 2)]/p9$C1.MFI[1], p10$duree[seq(1, 19, 2)]/p10$C1.MFI[1])

plot(duree_MFI1[,1], col = palette[1], type = "l", ylim = c(0, max(duree_MFI1)))
lines(duree_MFI1[,2], col = palette[2])
lines(duree_MFI1[,3], col = palette[3])
lines(duree_MFI1[,4], col = palette[4])
lines(duree_MFI1[,5], col = palette[5])
lines(duree_MFI1[,6], col = palette[6])
lines(duree_MFI1[,7], col = palette[7])
lines(duree_MFI1[,8], col = palette[8])
lines(duree_MFI1[,9], col = palette[9])
lines(duree_MFI1[,10], col = palette[10])

# MFI1 Initial X MFI1
MFI1XInit <- cbind(p1$C1.MFI[seq(1, 19, 2)]/p1$C1.MFI[1], p2$C1.MFI[seq(1, 19, 2)]/p2$C1.MFI[1],
                    p3$C1.MFI[seq(1, 19, 2)]/p3$C1.MFI[1], p4$C1.MFI[seq(1, 19, 2)]/p4$C1.MFI[1],
                    p5$C1.MFI[seq(1, 19, 2)]/p5$C1.MFI[1], p6$C1.MFI[seq(1, 19, 2)]/p6$C1.MFI[1],
                    p7$C1.MFI[seq(1, 19, 2)]/p7$C1.MFI[1], p8$C1.MFI[seq(1, 19, 2)]/p8$C1.MFI[1],
                    p9$C1.MFI[seq(1, 19, 2)]/p9$C1.MFI[1], p10$C1.MFI[seq(1, 19, 2)]/p10$C1.MFI[1])

plot(MFI1XInit[,1], col = palette[1], type = "l", ylim = c(0, max(MFI1XInit, na.rm = TRUE)))
lines(MFI1XInit[,2], col = palette[2])
lines(MFI1XInit[,3], col = palette[3])
lines(MFI1XInit[,4], col = palette[4])
lines(MFI1XInit[,5], col = palette[5])
lines(MFI1XInit[,6], col = palette[6])
lines(MFI1XInit[,7], col = palette[7])
lines(MFI1XInit[,8], col = palette[8])
lines(MFI1XInit[,9], col = palette[9])
lines(MFI1XInit[,10], col = palette[10])

# Croisement entre le MFI2 initial et les covariables
nb_patients <- 8
palette <- rainbow(nb_patients)

# MFI2 Initiaux
plot(c(1:3, 5:7, 9, 10), data$C2.MFI[seq(1, 181, 20)][which(!is.na(data$C2.MFI[seq(1, 181, 20)]))], col = palette, pch = 19, main = "Niveau du MFI2 Initial",
     ylab = "MFI2 Initial", xlab = "Patient")
legend("topright", legend = paste("Patient", 1:nb_patients), col = palette, pch = 19)

# MFI2 Initial X Durée
duree_MFI2 <- cbind(p1$duree[seq(1, 19, 2)]/p1$C2.MFI[1], p2$duree[seq(1, 19, 2)]/p2$C2.MFI[1],
                    p3$duree[seq(1, 19, 2)]/p3$C2.MFI[1],# p4$duree[seq(1, 19, 2)]/p4$C2.MFI[1],
                    p5$duree[seq(1, 19, 2)]/p5$C2.MFI[1], p6$duree[seq(1, 19, 2)]/p6$C2.MFI[1],
                    p7$duree[seq(1, 19, 2)]/p7$C2.MFI[1],# p8$duree[seq(1, 19, 2)]/p8$C2.MFI[1],
                    p9$duree[seq(1, 19, 2)]/p9$C2.MFI[1], p10$duree[seq(1, 19, 2)]/p10$C2.MFI[1])

plot(duree_MFI2[,1], col = palette[1], type = "l", ylim = c(0, max(duree_MFI2)))
lines(duree_MFI2[,2], col = palette[2])
lines(duree_MFI2[,3], col = palette[3])
lines(duree_MFI2[,4], col = palette[4])
lines(duree_MFI2[,5], col = palette[5])
lines(duree_MFI2[,6], col = palette[6])
lines(duree_MFI2[,7], col = palette[7])
lines(duree_MFI2[,8], col = palette[8])
# lines(duree_MFI2[,9], col = palette[9])
# lines(duree_MFI2[,10], col = palette[10])

# MFI2 Initial X MFI2
MFI2XInit <- cbind(p1$C2.MFI[seq(1, 19, 2)]/p1$C2.MFI[1], p2$C2.MFI[seq(1, 19, 2)]/p2$C2.MFI[1],
                   p3$C2.MFI[seq(1, 19, 2)]/p3$C2.MFI[1],# p4$C2.MFI[seq(1, 19, 2)]/p4$C2.MFI[1],
                   p5$C2.MFI[seq(1, 19, 2)]/p5$C2.MFI[1], p6$C2.MFI[seq(1, 19, 2)]/p6$C2.MFI[1],
                   p7$C2.MFI[seq(1, 19, 2)]/p7$C2.MFI[1],# p8$C2.MFI[seq(1, 19, 2)]/p8$C2.MFI[1],
                   p9$C2.MFI[seq(1, 19, 2)]/p9$C2.MFI[1], p10$C2.MFI[seq(1, 19, 2)]/p10$C2.MFI[1])

plot(MFI2XInit[,1], col = palette[1], type = "l", ylim = c(0, max(MFI2XInit, na.rm = TRUE)))
lines(MFI2XInit[,2], col = palette[2])
lines(MFI2XInit[,3], col = palette[3])
lines(MFI2XInit[,4], col = palette[4])
lines(MFI2XInit[,5], col = palette[5])
lines(MFI2XInit[,6], col = palette[6])
lines(MFI2XInit[,7], col = palette[7])
lines(MFI2XInit[,8], col = palette[8])
# lines(MFI2XInit[,9], col = palette[9])
# lines(MFI2XInit[,10], col = palette[10])




