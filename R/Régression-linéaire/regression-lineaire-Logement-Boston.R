# Importation de bibliothèques
library(mlbench) # Contient plusieurs ensembles de données de référence (en particulier l'ensemble de données sur le logement à Boston).
library(caret) # Package pour les algorithmes d'apprentissage automatique / CARET signifie Classification And REgression Training

# Importation de l'ensemble de données sur le logement à Boston
data(BostonHousing)

head(BostonHousing)

# Vérifier s'il y a des données manquantes
sum(is.na(BostonHousing))

# Pour obtenir un modèle reproductible, définir le numéro de la graine aléatoire
set.seed(100)

# Effectue un découpage aléatoire stratifié de l'ensemble des données
TrainingIndex <- createDataPartition(BostonHousing$medv, p=0.8, list = FALSE)
TrainingSet <- BostonHousing[TrainingIndex,] # Training Set
TestingSet <- BostonHousing[-TrainingIndex,] # Test Set


# Construire un modèle de formation
Model <- train(medv ~ ., data = TrainingSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none")
)

# Appliquer le modèle de prédiction
Model.training <-predict(Model, TrainingSet) # Appliquer le modèle pour prédire sur l'ensemble d'entraînement
Model.testing <-predict(Model, TestingSet) # Appliquer le modèle pour prédire sur l'ensemble de test

# Performance du modèle (Affiche le nuage de points et les mesures de performance)
# Nuage de points de l'ensemble d'entraînement
plot(TrainingSet$medv,Model.training, col = "blue" )
plot(TestingSet$medv,Model.testing, col = "blue" )
