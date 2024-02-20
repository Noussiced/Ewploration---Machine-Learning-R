# Importation des bibliothèques
library(datasets) # Contient l'ensemble de données Iris
library(caret) # Package pour les algorithmes d'apprentissage automatique / CARET signifie Classification And REgression Training

# Importation de l'ensemble de données Iris
data(iris)

# Vérifier s'il y a des données manquantes ?
sum(is.na(iris))

# Pour obtenir un modèle reproductible; définir le numéro de graine aléatoire
set.seed(100)

# Effectue une division aléatoire stratifiée de l'ensemble de données
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Ensemble d'entraînement
TestingSet <- iris[-TrainingIndex,] # Ensemble de test

# Comparer le nuage de points des sous-ensembles de données 80 et 20




###############################
# Modèle SVM (noyau polynomial)

# Construire le modèle d'entraînement
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Construire le modèle CV
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)


# Appliquer le modèle pour la prédiction
Model.training <-predict(Model, TrainingSet) # Appliquer le modèle pour prédire sur l'ensemble d'entraînement
Model.testing <-predict(Model, TestingSet) # Appliquer le modèle pour prédire sur l'ensemble de test
Model.cv <-predict(Model.cv, TrainingSet) # Effectuer la validation croisée

# Performance du modèle (Affiche la matrice de confusion et les statistiques)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Importance des caractéristiques
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")
