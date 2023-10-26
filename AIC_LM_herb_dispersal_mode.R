herb <- read.csv("herb_dispersal_diff.csv")


# Colonnes à transformer en facteur pour l'analyse stat
# site , modalite et annee
transfo <- c("année", "modalite", "site")

# Transformer les colonnes en facteurs
herb[, transfo] <- lapply(herb[, transfo], as.factor)
str(herb)

# Pivoter les valeurs de tous les modes de dispersion dans une seule colonne (value)
# mean_diff_all correspond au type de dispersion correspondant à chaque valeur 
library(tidyr)
herb_pivot <- herb %>% 
    pivot_longer(
        cols = starts_with("mean_diff_"),
        names_to = "mean_diff_all",
        values_to = "value"
    )
View(herb_pivot)

# test homogeneité des residus des valeurs de mode de dispersion en fonction des modalites de gestion
# tous les modes de dispersion confondus
model_dispersal <- lm(herb_pivot$value ~ herb_pivot$modalite)
residuals <- residuals(model_dispersal)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test homogeneité des residus des valeurs de mode de dispersion en fonction des années 
model_dispersal <- lm(herb_pivot$value ~ herb_pivot$année)
residuals <- residuals(model_dispersal)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test LM et AIC pour la selection du model le plus parcimonieux
model_1 <- lm(value ~ année, data = herb_pivot)

model_2 <- lm(value ~ modalite, data = herb_pivot)

model_3 <- lm(value ~ année * modalite, data = herb_pivot)

model_4 <- lm(value ~ année + modalite, data = herb_pivot)

model <- AIC(model_1,model_2,model_3 ,model_4)
model$AIC

# Tri des modèles par ordre croissant de l'AIC
sorted_indices <- order(model$AIC)
sorted_indices


summary(model_2)


