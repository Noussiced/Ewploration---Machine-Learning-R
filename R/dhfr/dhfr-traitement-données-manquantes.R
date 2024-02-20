# 1. Chargement des données DHFR
library(RCurl)
dhfr <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/dhfr.csv") )

View(dhfr)

# 2. Vérification des données manquantes

sum(is.na(dhfr))

# 3. Si les données sont propres, introduire aléatoirement NA dans l'ensemble de données

na.gen <- function(data,n) {
  i <- 1
  while (i < n+1) {
    idx1 <- sample(1:nrow(data), 1)
    idx2 <- sample(1:ncol(data), 1)
    data[idx1,idx2] <- NA
    i = i+1
  }
  return(data)
}


# Avant d'introduire NA dans l'ensemble de données, laisser l'étiquette de classe Y (variable de sortie) de côté

dhfr <- dhfr[,-1]


# Choisissez 1 des éléments suivants à exécuter (ils produiront le même résultat)

dhfr <- na.gen(dhfr,100)

dhfr <- na.gen(n=100,data=dhfr)

dhfr <- na.gen(100,dhfr) # Cela produit une erreur, pourquoi ?


# 4. Vérifier à nouveau les données manquantes

sum(is.na(dhfr))

colSums(is.na(dhfr))

str(dhfr)


# Liste les lignes avec des données manquantes

missingdata <- dhfr[!complete.cases(dhfr), ]

sum(is.na(missingdata))


# Si la somme ci-dessus est 0, cela signifie qu'il n'y a pas de données manquantes et procéder à la modélisation.
# Si la somme ci-dessus est supérieure à 0, passez à # 5


# 5. Traitement des données manquantes. Il y a 2 options, décidez et choisissez-en seulement 1

# 5.1. Supprimer simplement toutes les entrées avec des données manquantes

clean.data <- na.omit(dhfr)

sum(is.na(clean.data))


# 5.2. Imputation : Remplacer les valeurs manquantes par la 

# MOYENNE
dhfr.impute <- dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- mean(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))


# MÉDIANE
dhfr.impute <- dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- median(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))
