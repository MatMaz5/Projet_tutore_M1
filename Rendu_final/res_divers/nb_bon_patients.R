# setwd(dir = "/Users/matmaz/Master/GitHub/Projet_tutore_M1")


    # Packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(png)
library(ggpubr)


    # Données de base
data <- read.csv("donnees_corrigees_CINEDESIM.csv")

data$nom_batch <- factor(data$nom_batch)
data$temp <- factor(
  data$temp, 
  levels = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10",
             "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")
  )


    # Barplot
      ## Données pour le barplot
df_bp3000 <- data %>%
  filter(C1.MFI < 3000) %>%
  group_by(temp) %>%
  summarise(nb_patients = n_distinct(nom_batch))

df_bp3000$seance <- factor(
  c("S1", "S1", "S2", "S2", "S3", "S3", "S4", "S4", "S5", "S5", 
    "S6", "S6", "S7", "S7", "S8", "S8", "S9", "S9", "S10", "S10"), 
  levels = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
    )
df_bp3000$moment_seance <- factor(
  df_bp3000$temp, 
  labels = rep(c("Début", "Fin"), 10),
  levels = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10",
             "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")
)

    ## Graphique
bp3000 <- ggplot(df_bp3000, aes(x = seance, y = nb_patients, fill = moment_seance)) +
  geom_col(position = "dodge") +  # Permet de séparer les temps au sein d'une séance
  scale_y_continuous(limits = c(0, 10), breaks = 0:10) +  
  scale_fill_manual(values = c("#4e757e", "#add5e0")) +
  labs(
    x = "Séance", 
    y = "Nombre de patients", 
    title = "Nombre de patients avec MFI.CI < 3000 à chaque prélèvement",
    fill = "Temps dans la séance"
    ) +
  geom_vline(xintercept = 5.5, col = "firebrick", linetype = 2, size = 0.5) +
  geom_label( 
    x = 5.5, 
    y = 8.5, 
    label = "Week-end",
    fill = "white", 
    color = "firebrick",
    size = 3,
    angle = 90
  )
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 12), 
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6),
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
    ) +

ggsave("bp3000.png", plot = bp3000, 
       width = 7, height = 4, path = "Rendu_final/res_divers/")
img_bp3000 <-  rasterGrob(readPNG("Rendu_final/res_divers/tab3000.png"), 
                           interpolate = TRUE)


### Tableau récapitulatif
data_reduit <- data[, c("temp", "nom_batch", "C1.MFI")] %>%
  filter(temp == c("t1", "t20"))

df_tab3000 <- data.frame(
  Patient = unique(data_reduit$nom_batch),
  MFI_t1 = data_reduit$C1.MFI[data_reduit$temp == "t1"],
  MFI_t20 = ifelse(
    data_reduit$C1.MFI[data_reduit$temp == "t20"] < 3000,
    "Oui", "Non"))

df_tab3000 <- df_tab3000 %>%
  arrange(desc(MFI_t1))

tab3000 <- tableGrob(df_tab3000, rows = rep("", 10),
                     cols = c("Patient", "MFI à t0", "MFI < 3000\npost-protocole ?"))
ggsave("tab3000.png", plot = tab3000, 
       width = 3.3, height = 3.3, path = "Rendu_final/res_divers/")
img_tab3000 <-  rasterGrob(readPNG("Rendu_final/res_divers/tab3000.png"), 
                           interpolate = TRUE)

ggarrange(bp3000, img_tab3000, ncol = 2, nrow = 2)











