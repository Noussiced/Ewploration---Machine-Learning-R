
#########################
# Chargement de l'ensemble de données Iris
#########################

# Méthode 1

library(datasets)
data(iris)

iris2 <- datasets::iris

# Méthode 2
# install.packages("RCurl")

library(RCurl)
iris3 <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv") )

# Afficher les données
View(iris)

#############################
# Afficher les statistiques sommaires
#############################

# head() / tail()
head(iris, 5)
tail(iris, 5)


# summary()
summary(iris)
summary(iris$Sepal.Length)


# Vérifier s'il y a des données manquantes ?
sum(is.na(iris))


# skimr() - étend summary() en fournissant un ensemble plus large de statistiques
#  install.packages("skimr")
# https://github.com/ropensci/skimr

library(skimr)

skim(iris) # Effectuer skim pour afficher les statistiques sommaires

# Regrouper les données par espèce puis effectuer skim
iris %>% 
  dplyr::group_by(Species) %>% 
  skim() 

#############################
# Visualisation rapide des données
#
# Tracé de base R()
#############################


# Tracés de panneau
plot(iris)
plot(iris, col = "red")

# Nuage de points
plot(iris$Sepal.Width, iris$Sepal.Length)

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red")     # Fait des cercles rouges

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",     # Fait des cercles rouges + Ajoute des étiquettes d'axe x et y
     xlab = "Largeur du sépale", ylab = "Longueur du sépale")

# Histogramme
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col = "red")   # Fait des barres rouges

# Tracés de caractéristiques
# https://www.machinelearningplus.com/machine-learning/caret-package/
featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))