
# Importer les libraries
library(datasets) # Contient plusieurs jeux de données
library(ggplot2)
library(lattice)
library(foreach)
library(iterators)
library(parallel)
library(caret) # Package pour les algorithmes d'apprentissage automatique / CARET signifie formation à la classification et à la régression

# Importer le Dataset dhfr
data(dhfr)

# Manque-t-il des données?
sum(is.na(dhfr))

# Pour obtenir un modèle reproductible, il faut définir le numéro de la graine aléatoire
set.seed(100)

# Découpage aléatoire stratifié de l'ensemble des données
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Set D'entrainement
TestingSet <- dhfr[-TrainingIndex,] # Set de Test



###############################
# Forêt aléatoire


# Exécution normale sans traitement parallèle
start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Construire un modèle à l'aide d'un ensemble d'entraînement
               method = "rf" # Algorithme d'apprentissage
         )
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)



# Utiliser doParallel

library(doParallel)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet,
               method = "rf"
         )
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)




##########################

# Exécution sans traitement parallèle

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
         )
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# Utiliser doParallel

library(doParallel)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, 
               method = "rf", 
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
         )
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)


##########################
# Appliquer le modèle de prédiction
Model.training <-predict(Model, TrainingSet) # Appliquer le modèle pour faire des prédictions sur l'ensemble de formation

# Performance du modèle (affiche la matrice de confusion et les statistiques)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)


# Importance de la fonction
Importance <- varImp(Model)
plot(Importance, top = 25)
plot(Importance, col = "red")
