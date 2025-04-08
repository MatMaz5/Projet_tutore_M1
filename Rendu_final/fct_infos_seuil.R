# Dans l'idéal, la fonction devrait pouvoir traiter les 2 classes mais il manque des
# données pour certains patient (4 et 8) dans C2.MFI.

infos_seuil <- function(df = data, seuil = 3000, classe = C1.MFI) {
  # Format du df
  df$nom_batch <- factor(df$nom_batch)
  df$temp <- factor(
    df$temp, 
    levels = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10",
               "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")
  )
  df <- df %>%
    arrange(nom_batch, temp)
  
  
  # Classe choisie
  nom_classe <- as_label(rlang::enquo(classe))
  titre_classe <- ifelse(nom_classe == "C1.MFI", "Classe I", "Classe II")
  
  
  # Barplot
    ## Données
  temps_complets <- data.frame(temp = levels(df$temp))
  df_bp <- df %>%
    filter({{classe}} < seuil) %>%
    group_by(temp) %>%
    summarise(nb_patients = n_distinct(nom_batch), .groups = "drop") %>%
    right_join(temps_complets, by = "temp") %>%
    mutate(nb_patients = ifelse(is.na(nb_patients), 0, nb_patients))
  df_bp$seance <- factor(
    c("S1", "S1", "S2", "S2", "S3", "S3", "S4", "S4", "S5", "S5", 
      "S6", "S6", "S7", "S7", "S8", "S8", "S9", "S9", "S10", "S10"), 
    levels = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10")
      )
  df_bp$moment_seance <- factor(
    df_bp$temp, 
    labels = rep(c("Début", "Fin"), dim(df_bp)[1]/2),
    levels = c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10",
               "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20")
  )

    ## Graphique
  bp <- ggplot(df_bp, aes(x = seance, y = nb_patients, fill = moment_seance)) +
    geom_col(position = "dodge") +
    scale_y_continuous(limits = c(0, 10), breaks = 0:10) +  
    scale_fill_manual(values = c("#4e757e", "#add5e0")) +
    labs(
      x = "Séance", 
      y = "Nombre de patients", 
      title = paste(
        "Nombre de patients avec un MFI", titre_classe, "<", 
        seuil, 
        "à chaque prélèvement"
        ),
      fill = "Temps dans la séance"
      ) +
    geom_vline(xintercept = 5.5, col = "firebrick", linetype = 2, linewidth = 0.5) +
    geom_label( 
      x = 5.5, 
      y = 8.5, 
      label = "Week-end",
      fill = "white", 
      color = "firebrick",
      size = 3,
      angle = 90
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0, size = 13), 
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 7),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
      )
  ggsave("bp_seuil.png", plot = bp, width = 7, height = 5, 
         path = "res_divers/")
  img_bp_seuil <- rasterGrob(readPNG("res_divers/bp_seuil.png"))
  
  
  # Tableau récapitulatif
    ## Premier prélèvement avec MFI < seuil
  prem_temp <- df %>%
    group_by(nom_batch) %>%
    filter({{classe}} < seuil) %>%
    summarise(prem_temp_ss_seuil = first(temp))
  
    ## Prélèvement après lequel MFI toujours < seuil
  tjrs_ss_seuil <- function(temp, mfi) {
    for (i in seq_along(mfi)) {
      if (all(mfi[i:length(mfi)] < seuil)) {
        return(temp[i])
      }
    }
    return(NA)
  }
  stable_temp <- df %>%
    group_by(nom_batch) %>%
    summarise(
      temp_stable_ss_seuil = tjrs_ss_seuil(temp, {{classe}})
    )
  
    ## Fusion des infos dans un même df
  df_tab <- df %>%
    group_by(nom_batch) %>%
    summarise(MFI_t1 = {{classe}}[temp == "t1"][1]) %>%
    left_join(prem_temp, by = "nom_batch") %>%
    left_join(stable_temp, by = "nom_batch") %>%
    mutate(
      prem_temp_ss_seuil = ifelse(
        is.na(prem_temp_ss_seuil),
        "Non atteint", 
        paste0(prem_temp_ss_seuil)
      ),
      temp_stable_ss_seuil = ifelse(
        is.na(temp_stable_ss_seuil),
        "Non atteint", 
        paste0(temp_stable_ss_seuil)
      )
    ) %>%
    arrange(desc(MFI_t1))
  
  
  tab <- tableGrob(
    df_tab, 
    rows = rep("", nrow(df_tab)),
    cols = c(
      "Patient", 
      "MFI à t1", 
      paste("Premier\nprélèvement\navec MFI <", seuil), 
      paste("Prélèvement à partir\nduquel le patient\nreste sous", seuil, "MFI")
    )
  )
  
  ggsave("tab_seuil.png", plot = tab, 
         width = 5.1, height = 3.7, path = "res_divers/")
  img_tab_seuil <- rasterGrob(readPNG("res_divers/tab_seuil.png"))
  
  ggarrange(img_bp_seuil, img_tab_seuil, ncol = 2, nrow = 1)
}
