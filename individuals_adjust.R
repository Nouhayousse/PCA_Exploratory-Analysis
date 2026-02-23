

notes <- data_acp_idsi2
notes_df <- as.data.frame(notes)

rownames(notes) <- notes$Étudiant

notes <- notes[, -1]   # on supprime la colonne Etudiant

# Aperçu global
head(notes_df)
str(notes_df)
summary(notes_df)
boxplot(notes_df,
        col = "lightblue",
        main = "Boxplots des notes par matière",
        ylab = "Notes",
        las = 2)
par(mfrow = c(2,3))
for (j in 1:ncol(notes_df)) {
  hist(notes_df[, j],
       main = colnames(notes_df)[j],
       xlab = "Notes",
       col = "lightgray",
       border = "white")
}
par(mfrow = c(1,1))




# Centrage-réduction
Z <- scale(notes_df, center = TRUE, scale = TRUE)

# Affichage
Z

colMeans(Z)      # ≈ 0
apply(Z, 2, sd)  # = 1


R <- cor(Z)
R
corrplot(R,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8,
         main = "Matrice de corrélation des matières")



lambda <- c(3.8174, 1.9601, 0.1289, 0.0444, 0.0301, 0.0191)

inertie <- lambda / sum(lambda)
cumul <- cumsum(inertie)

data.frame(
  Axe = 1:6,
  Valeur_propre = lambda,
  Inertie = inertie * 100,
  Cumul = cumul * 100
)



acp <- prcomp(notes_df, center = TRUE, scale. = TRUE)

eig <- acp$sdev^2
eig



CP <- acp$x[, 1:3]
CP

apply(CP, 2, var)











