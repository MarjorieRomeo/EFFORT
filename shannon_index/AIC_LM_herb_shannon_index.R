herb <- read.csv("shannon_herb_diff.csv")

View(herb)

# Colonnes � transformer en facteur
# site , modalite et annee
transfo <- c("ann�e", "modalite", "site")

# Transformer les colonnes en facteurs
herb[, transfo] <- lapply(herb[, transfo], as.factor)
str(herb)
View(herb)




# test homogeneit� des residus des valeurs de shannon index en fonction des modalites de gestion
model_shannon <- lm(herb$diff_mean_shannon ~ herb$modalite)
residuals <- residuals(model_shannon)
hist(residuals)
qqnorm(residuals)
qqline(residuals)

# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test homogeneit� des residus des valeurs de shannon index en fonction des ann�es 
model_shannon_an <- lm(herb$diff_mean_shannon ~ herb$ann�e)
residuals <- residuals(model_shannon_an)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
shapiro_test_arc <- shapiro.test(residuals)
shapiro_test_arc




# test LMM pour la selection du model le plus parcimonieux
model_1 <- lm(diff_mean_shannon ~ ann�e, data = herb)

model_2 <- lm(diff_mean_shannon ~ modalite, data = herb)

model_3 <- lm(diff_mean_shannon ~ ann�e * modalite, data = herb)

model_4 <- lm(diff_mean_shannon ~ ann�e + modalite, data = herb)

model <- AIC(model_1,model_2,model_3 ,model_4)
model$AIC
# Tri des mod�les par ordre croissant de l'AIC
sorted_indices <- order(model$AIC)
sorted_indices


summary(model_3)


