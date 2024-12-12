library(reshape2)
library(ggplot2)
library(dplyr)
library(knitr)


# Paramètres de la matrice
n <- 5
p <- 5
mat <- matrix(NA, nrow = n, ncol = p)
rownames(mat) <- c("5", "4", "3", "2", "1")
colnames(mat) <- c("1", "2", "3", "4", "5")


# Transformer la matrice pour ggplot
sur_mat <- melt(mat)
colnames(sur_mat) <- c("Likelihood", "Consequence", "Impact")
sur_mat <- mutate(sur_mat, Likelihood = as.numeric(as.character(Likelihood)),
                  Consequence = as.numeric(as.character(Consequence)),
                  Impact = Consequence + Likelihood)


# Couleurs pour la matrice
couleurs <- c("green", "green2", "yellow2", "orange", "red3")


# Données des risques
tags <- c("Non functional study", "Legal issues (plagiarism)", "Team conflicts", "Files' loss", "Response fail", 
          "Wrong files' versions", "Missing students", "Delayed schedule", "Changing in backers' needs")
x <- c(5, 4, 4.5, 3.5, 5, 2.5, 2, 3.5, 5)
y <- c(1, 1, 1.5, 2, 2.5, 2.5, 4.5, 3.5, 5)
data <- data.frame("Tags" = tags, "Consequence" = x, "Likelihood" = y)
data <- mutate(data, Impact = Consequence + Likelihood, Numbers = 1:nrow(data))


# Création de la matrice des risques
mat_risk <- ggplot(sur_mat, aes(x = Consequence, y = Likelihood, fill = Impact)) + 
  geom_tile(color = "white") +
  scale_fill_gradientn(colours = couleurs, limits = c(0, 10)) + 
  scale_x_continuous(breaks = 1:5, expand = c(0, 0)) + 
  scale_y_continuous(breaks = 1:5, expand = c(0, 0)) + 
  coord_fixed() +
  theme_minimal() +
  ggtitle("Risk Matrix") +
  labs(x = "Consequence", y = "Likelihood") +
  geom_point(data = data, aes(x = Consequence, y = Likelihood), color = "black", size = 3, pch = 18) +
  geom_text(data = data, aes(x = Consequence, y = Likelihood, label = Numbers), 
            hjust = 0.5, vjust = 2, size = 3, color = "black")
mat_risk

risks <- data[, c(5, 1)]
kable(risks, align = "c")


