##############################################################"
#' Biodiv-data
#' Groupe salicornes 
#' Bonneaudet ; Fraboulet 
#' 2026-2027
#'Exploration de la structure des données ALGUES
#'données de Valentine Anstett
#' ####################################################"
#' 

#' 01. Je charge les données nettoyées en csv créées dans 00 qui sont dans data_clean
XXX <- read.csv("", row.names = 1)
XXX <- read.csv("", row.names = 1)
XXX <- read.csv("", row.names = 1)
XXX <- read.csv("", row.names = 1)
XXX <- read.csv("", row.names = 1)
XXX <- read.csv("", row.names = 1)


#### 02. Global : tailles et NA 

# Taille : nombre de lignes, puis nombre de colonnes
dim()
dim()
dim()
dim()
dim()
dim()

### 03. Pourcentage de NA dans chaque tableau
mean(is.na()) * 100
mean(is.na()) * 100 
mean(is.na()) * 100
mean(is.na()) * 100
mean(is.na()) * 100  
mean(is.na()) * 100



#### 04. Valeurs manquantes (nombre de NA par colonne)
colSums(is.na())
colSums(is.na())  
colSums(is.na())
colSums(is.na())
colSums(is.na()) 

#### 05. Lignes en double
sum(duplicated())
sum(duplicated())
sum(duplicated())
sum(duplicated())
sum(duplicated())
sum(duplicated())

#### 06. Valeurs summary
summary()
summary()
summary()
summary()
summary()
summary()

#### 07. Moda des variables quali= plan d'échantillonnage
table(data_labo$type, useNA = "ifany") # équilibre ou pas ?
table(data_distance_sediment$id_secteur, useNA = "ifany")  

