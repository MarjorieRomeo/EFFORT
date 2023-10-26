herb <- read.csv("herb_ombre_diff.csv")


# Colonnes à transformer en facteur 
# site , modalite et annee
transfo <- c("année", "modalite", "site")

# Transformer les colonnes en facteurs
herb[, transfo] <- lapply(herb[, transfo], as.factor)
str(herb)

# Pivoter la table pour avoir toutes les valeurs de chaque catégorie de tolérance à l'ombre dans une seule colonne = value
# mean_diff_all = les catégories des plantes favorisées par l'ombre, tolérante ou non tolérante à l'ombre

View(herb)
library(tidyr)
herb_pivot <- herb %>% 
    pivot_longer(
        cols = starts_with("mean_diff_"),
        names_to = "mean_diff_all",
        values_to = "value"
    )

View(herb_pivot)

# test homogeneité des residus de Ombre en fonction des modalites de gestion
model_dispersal <- lm(herb_pivot$value ~ herb_pivot$modalite)
residuals <- residuals(model_dispersal)
hist(residuals)
qqnorm(residuals)
qqline(residuals)

# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test homogeneité des residus de Ombre en fonction des années 
# Pour tous les sites confondus et toutes les années de 2016 à 2022 
model_dispersal <- lm(herb_pivot$value ~ herb_pivot$année)
residuals <- residuals(model_dispersal)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test LM pour la selection du model le plus parcimonieux
# test AIC
model_1 <- lm(value ~ année, data = herb_pivot)

model_2 <- lm(value ~ modalite, data = herb_pivot)

model_3 <- lm(value ~ année * modalite, data = herb_pivot)

model_4 <- lm(value ~ année + modalite, data = herb_pivot)

model <- AIC(model_1,model_2,model_3 ,model_4)
model$AIC

# Tri des modèles par ordre croissant de l'AIC
sorted_indices <- order(model$AIC)
sorted_indices


summary(model_4)


