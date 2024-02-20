
# Importation des bibliothèques
library(datasets) # Contient plusieurs jeux de données
library(caret) # Package pour les algorithmes d'apprentissage automatique / CARET signifie Classification And REgression Training

# Importation de l'ensemble de données dhfr
data(dhfr)

# Vérifier s'il y a des données manquantes ?
sum(is.na(dhfr))

# Pour obtenir un modèle reproductible; définir le numéro de graine aléatoire
set.seed(100)

# Effectue une division aléatoire stratifiée de l'ensemble de données
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Ensemble d'entraînement
TestingSet <- dhfr[-TrainingIndex,] # Ensemble de test

###############################
# Modèle SVM (noyau polynomial)

# Construire le modèle d'entraînement
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Construire le modèle CV
Model.cv <- train(Y ~ ., data = TrainingSet,
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
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Importance des caractéristiques
Importance <- varImp(Model)
plot(Importance, top = 25)
plot(Importance, col = "red")
