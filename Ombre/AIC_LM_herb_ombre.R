herb <- read.csv("herb_ombre_diff.csv")


# Colonnes � transformer en facteur 
# site , modalite et annee
transfo <- c("ann�e", "modalite", "site")

# Transformer les colonnes en facteurs
herb[, transfo] <- lapply(herb[, transfo], as.factor)
str(herb)

# Pivoter la table pour avoir toutes les valeurs de chaque cat�gorie de tol�rance � l'ombre dans une seule colonne = value
# mean_diff_all = les cat�gories des plantes favoris�es par l'ombre, tol�rante ou non tol�rante � l'ombre

View(herb)
library(tidyr)
herb_pivot <- herb %>% 
    pivot_longer(
        cols = starts_with("mean_diff_"),
        names_to = "mean_diff_all",
        values_to = "value"
    )

View(herb_pivot)

# test homogeneit� des residus de Ombre en fonction des modalites de gestion
model_dispersal <- lm(herb_pivot$value ~ herb_pivot$modalite)
residuals <- residuals(model_dispersal)
hist(residuals)
qqnorm(residuals)
qqline(residuals)

# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test homogeneit� des residus de Ombre en fonction des ann�es 
# Pour tous les sites confondus et toutes les ann�es de 2016 � 2022 
model_dispersal <- lm(herb_pivot$value ~ herb_pivot$ann�e)
residuals <- residuals(model_dispersal)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test LM pour la selection du model le plus parcimonieux
# test AIC
model_1 <- lm(value ~ ann�e, data = herb_pivot)

model_2 <- lm(value ~ modalite, data = herb_pivot)

model_3 <- lm(value ~ ann�e * modalite, data = herb_pivot)

model_4 <- lm(value ~ ann�e + modalite, data = herb_pivot)

model <- AIC(model_1,model_2,model_3 ,model_4)
model$AIC

# Tri des mod�les par ordre croissant de l'AIC
sorted_indices <- order(model$AIC)
sorted_indices


summary(model_4)


