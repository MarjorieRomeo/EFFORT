herb <- read.csv("shannon_herb_diff.csv")

View(herb)

# Colonnes à transformer en facteur
# site , modalite et annee
transfo <- c("année", "modalite", "site")

# Transformer les colonnes en facteurs
herb[, transfo] <- lapply(herb[, transfo], as.factor)
str(herb)
View(herb)




# test homogeneité des residus des valeurs de shannon index en fonction des modalites de gestion
model_shannon <- lm(herb$diff_mean_shannon ~ herb$modalite)
residuals <- residuals(model_shannon)
hist(residuals)
qqnorm(residuals)
qqline(residuals)

# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test homogeneité des residus des valeurs de shannon index en fonction des années 
model_shannon_an <- lm(herb$diff_mean_shannon ~ herb$année)
residuals <- residuals(model_shannon_an)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
shapiro_test_arc <- shapiro.test(residuals)
shapiro_test_arc




# test LMM pour la selection du model le plus parcimonieux
model_1 <- lm(diff_mean_shannon ~ année, data = herb)

model_2 <- lm(diff_mean_shannon ~ modalite, data = herb)

model_3 <- lm(diff_mean_shannon ~ année * modalite, data = herb)

model_4 <- lm(diff_mean_shannon ~ année + modalite, data = herb)

model <- AIC(model_1,model_2,model_3 ,model_4)
model$AIC
# Tri des modèles par ordre croissant de l'AIC
sorted_indices <- order(model$AIC)
sorted_indices


summary(model_3)


