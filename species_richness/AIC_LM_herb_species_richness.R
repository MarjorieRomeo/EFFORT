herb <- read.csv("species_richness_diff_herb.csv")
View(herb)

# Colonnes � transformer en facteur
# site , modalite et annee
transfo <- c("ann�e", "modalite", "site")

# Transformer les colonnes en facteurs
herb[, transfo] <- lapply(herb[, transfo], as.factor)
str(herb)


# test homogeneit� des residus des valeurs de species richness en fonction des modalites de gestion
model_shannon <- lm(herb$diff_mean_SR ~ herb$modalite)
residuals <- residuals(model_shannon)
hist(residuals)
qqnorm(residuals)
qqline(residuals)

# test normalite
shapiro_test <- shapiro.test(residuals)
shapiro_test



# test homogeneit� des residus des valeurs de species richness en fonction des ann�es 
model_shannon_an <- lm(herb$diff_mean_SR ~ herb$ann�e)
residuals <- residuals(model_shannon_an)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
shapiro_test_arc <- shapiro.test(residuals)
shapiro_test_arc




# test LMM pour la selection du model le plus parcimonieux
model_1 <- lm(diff_mean_SR ~ ann�e, data = herb)

model_2 <- lm(diff_mean_SR ~ modalite, data = herb)

model_3 <- lm(diff_mean_SR ~ ann�e * modalite, data = herb)

model_4 <- lm(diff_mean_SR ~ ann�e + modalite, data = herb)

model <- AIC(model_1,model_2,model_3 ,model_4)
model$AIC
# Tri des mod�les par ordre croissant de l'AIC
sorted_indices <- order(model$AIC)
sorted_indices


summary(model_1)


