#### 0. Préparation ####
library(openxlsx)
library(vegan)

#Important, il est néceessaire d'ouvrir le projet avec bd_algues-LTM.Rproj
setwd("data/raw")
getwd()

list.files(pattern = "\\.xlsx$")


#### 1. Sels nutritifs ####

#Enregistrement des données sur les sels nutritifs
getSheetNames("sels nutritifs 2023-2025.xlsx")
data_sel <-  read.xlsx("sels nutritifs 2023-2025.xlsx", sheet = 7)#La feuille TOTAL est déjà la compilations de toute les autres

str(data_sel)#Plusieurs variables apparaissent en character à cause du symbole "<" et "," pour les très petites valeurs
head(data_sel)

#on se débarrasse des unités dans les noms de colonnes pour que ça soit plus simple par la suite
names(data_sel)[4:8] <- c("silicates",
                          "nitrites",
                          "phosphates",
                          "nitrates",
                          "ammonium")

#on passe tout en numérique
cols_num <- 4:ncol(data_sel)

for (col in cols_num) {
  data_sel[[col]] <- as.numeric(gsub(",", ".", gsub("<", "", data_sel[[col]])))
}
str(data_sel)

#Dernier problème, les deux dernieres lignes ont des valeurs mais de site, d'ID ou quoi on les enlève donc.
data_sel <- data_sel[-c(355:356),]

#Enregistrement en .csv dans le dossier processed
write.csv(data_sel, "../processed/data_sel.csv",row.names = FALSE, fileEncoding = "UTF-8")

rm(list = ls())


#### 2. Minéraux ####

### Données du laboratoire Teyssier sur les minéraux
#Ces données sont sur deux tableurs qu'il faut remerged ensemble
list.files(pattern = "\\.xlsx$")

#on retire les lignes unités et normes
data_mineraux1 <-  read.xlsx("25077098 à 25077208 UMR AMAP - UMR MARBEC (PS1-BUREAU 06 - RECAPITULATIF ANALYSES.xlsx")[-c(1:2),]
data_mineraux2 <- read.xlsx("25097092 à 25097100 UMR 5120-AMAP-UNIVERSITE DE MONTPELLIER - RECAPITULATIF ANALYSES.xlsx")[-c(1:2),]

identical(names(data_mineraux1), names(data_mineraux2))

data_mineraux <- rbind(data_mineraux1, data_mineraux2)[,-2]#Enlève la colonne de l'identifiant donné par le laboratoire
str(data_mineraux)#c'est pas en numérique

#colonne 1 = identifiant, le reste est numérique (2:ncol car la colonne 2 d'origine vient d'être retirée)
cols_num <- 2:ncol(data_mineraux)
for (col in cols_num) {
  data_mineraux[[col]] <- as.numeric(data_mineraux[[col]])
}
str(data_mineraux)
sum(is.na(data_mineraux))
#Enregistrement en .csv dans le dossier processed
write.csv(data_mineraux, "../processed/data_mineraux.csv",row.names = FALSE, fileEncoding = "UTF-8")
rm(list = ls())


#### 3. Macrophytes ####

### Donnée macrophyte

list.files(pattern = "\\.xlsx$")
data_macrophytes <-  read.xlsx("macrophytes_2025.xlsx")
dim(data_macrophytes)
str(data_macrophytes)

colSums(data_macrophytes[,c(5:22)])
colSums(is.na(data_macrophytes[, 5:22]))   # nombre de NA par colonne

which(is.na(data_macrophytes$Ruppia.maritima_ABS_COV))   # numéros de lignes
which(is.na(data_macrophytes$Tolypella.salina_ABS_COV))

data_macrophytes[, 5:22][is.na(data_macrophytes[, 5:22])] <- 0
colSums(data_macrophytes[,c(5:22)])

#Trois espèces sur lequel ce posé des questions Zanichellia pedunculata et Riella helicophylla : non observé ? On garde ?
# Zanichellia palustris observé une fois ?


#Enregistrement en .csv dans le dossier processed
write.csv(data_macrophytes, "../processed/data_macrophytes_2025.csv", row.names = FALSE, fileEncoding = "UTF-8")
rm(list = ls())


#### 4. Hydrologie : compilation des feuilles et comparaison avec total2 ####

#Les données hydrologiques
list.files(pattern = "\\.xlsx$")
getSheetNames("hydro-2023-2025.xlsx")#feuille exemple ?

fichier <- "hydro-2023-2025.xlsx"
feuilles <- setdiff(getSheetNames(fichier), c("total", "total2","exemple"))
length(feuilles)
liste <- list()

for (f in feuilles) {
  d <- read.xlsx(fichier, sheet = f)
  d$feuille <- f
  liste[[f]] <- d
}

length(unique(lapply(liste, names)))   # 1 = mêmes colonnes partout, ici 2


ref <- names(liste[[1]])

# feuilles dont les colonnes diffèrent de la première
diff_feuilles <- names(liste)[!sapply(liste, function(d) identical(names(d), ref))]
diff_feuilles

# détail de la différence pour la première feuille concernée
d2 <- names(liste[[diff_feuilles[1]]])
setdiff(ref, d2)    # colonnes présentes dans la référence mais pas dans cette feuille
setdiff(d2, ref)    # colonnes présentes dans cette feuille mais pas dans la référence

#Bag1 est la seul feuille différente des autres

head(liste$bag1)
head(liste$bag2)

#On change les noms de bag 1 avec le reste
names(liste[["bag1"]]) <- sub("_moy$|_med$", "", names(liste[["bag1"]]))

# vérification : doit donner 1
length(unique(lapply(liste, names)))
# Tout les fichiers de la liste ont les mêmes colonnes on va en faire un seul dataframe

data_hydro_compil <- do.call(rbind, lapply(liste, tail, 1))

#Je veux comparé ce dataframe de data compilation avec la feuille total2

data_total2 <- read.xlsx(fichier, sheet = 2)

dim(data_hydro_compil)
dim(data_total2)

# Aligner les noms de colonnes sur total2 (qui reprend ceux de bag1)
ren <- c("turbidite" = "turbidite_moy",
         "temperature" = "temperature_moy",
         "hauteur_eau(cm)" = "hauteur_eau_moy(cm)",
         "salinite(g/L)" = "salinite_moy(g/L)",
         "conductivite(mS/cm)" = "conductivite_moy(mS/cm)",
         "chlorophylleA" = "chlorophylleA_med")
names(data_hydro_compil)[match(names(ren), names(data_hydro_compil))] <- ren

# Colonnes communes, triées par code pour comparer dans le même ordre
communes <- intersect(names(data_hydro_compil), names(data_total2))
a <- data_hydro_compil[order(data_hydro_compil$code), communes]
b <- data_total2[order(data_total2$code), communes]

# Résultat : 9 colonnes identiques sur 11
for (col in communes) cat(col, ":", isTRUE(all.equal(a[[col]], b[[col]])), "\n")

# Reste à regarder salinite_moy(g/L) et chlorophylleA_med
cbind(a[c("code", "salinite_moy(g/L)", "chlorophylleA_med")],
      b[c("salinite_moy(g/L)", "chlorophylleA_med")])

#Malgré les falses ses valeurs sont les mêmes la différence c'est l'arrondi, donc les données compilés
#sont les mêmes que total2 sauf que total2 est plus complet, on va donc enregister ce jeu

data_total2
write.csv(data_total2, "../processed/data_hydro_total2.csv", row.names = FALSE, fileEncoding = "UTF-8")
rm(list = ls())


#### 5. Hydrologie : série 2023-2025 (total1 + total2) et lien avec les sels ####

#On souhaite maintenant voir ce qu'il y a dans total1
#j'ai compris, total 2 est la suite de total1, c'est une série temporelle, comme les sels nutritifs sauf que total2 à merged avec les sels
#On va donc créer un jeu de donné clean de cette série avec les variables en lien avec les sels et celle en lien avec l'hydro

data_total1 <- read.xlsx("hydro-2023-2025.xlsx",sheet = 1)
colnames(data_total1)
data_2023 <- data_total1[,c(1:15)]
data_2023$prelevement_sels_nutritifs <- tolower(data_2023$prelevement_sels_nutritifs)#homogenise les Oui, oui, et OUI

data_2023_2024 <- data_2023[data_2023$prelevement_sels_nutritifs== "oui",]

data_2023_2024 <- data_2023_2024[!is.na(data_2023_2024$date_releve), ]# on enlève les NA là ou il y a pas eu de relevé
data_2023_2024 <- data_2023_2024[,-c(14:15)]
data_2023_2024$eau <- NULL
data_2023_2024$date_releve <- format(as.Date(data_2023_2024$date_releve), "%Y-%m")#pour que ça corresponde au sel nutritif



data_hydro_2023_2025 <- data_2023_2024
write.csv(data_hydro_2023_2025, "../processed/data_hydro_2023_2025.csv", row.names = FALSE, fileEncoding = "UTF-8")


data_sel <- read.csv("../processed/data_sel.csv")
mineraux <- read.csv("../processed/data_mineraux.csv")

#Ce qui est surprenant c'est que data_sel a plus de site que le data data_data_hydro avec toutes les années
# codes de sel absents de l'hydro
#Après beaucoup de vérification, il y a des codes douteux dans data_sel qui font référence à des colonnes bizarre de total


names(data_sel)[names(data_sel) == "ID_LAG"] <- "code"
names(data_sel)[names(data_sel) == "date"] <- "date_releve"

data_hydro_sel <- merge(data_hydro_2023_2025, data_sel[, -1],
                        by = c("code", "date_releve"), all.x = TRUE)

data_hydro_sel[["salinite(g/L)"]] <- as.numeric(data_hydro_sel[["salinite(g/L)"]])
data_hydro_sel[["chlorophylleA"]] <- as.numeric(data_hydro_sel[["chlorophylleA"]])



#Donnée de présence absence borel
rm(list = ls())

data_borel <- read.xlsx("Rendu données brutes Borel.xlsx",sheet = 2)
write.csv(data_borel, "../processed/data_borel", row.names = FALSE, fileEncoding = "UTF-8")


#Donnée de couverture des algues
rm(list = ls())
data_algues_TdV <- read.xlsx("données Algues TdV final.xlsx",sheet = 1)

#corrigé le nom des sites X3, X4, X5... prennent le nom du site à leur gauche
noms <- names(data_algues_TdV)
noms[1] <- "taxon"

#
for (i in 2:length(noms)) {
  if (grepl("^X[0-9]+$", noms[i])) noms[i] <- noms[i - 1]
}
noms <- names(data_algues_TdV)
noms[1] <- "taxon"

# X3, X4, X5... prennent le nom du site à leur gauche
for (i in 2:length(noms)) {
  if (grepl("^X[0-9]+$", noms[i])) noms[i] <- noms[i - 1]
}

# la première ligne contient N, S, E, W
direction <- as.character(unlist(data_algues_TdV[1, -1]))
names(data_algues_TdV) <- c("taxon", paste(noms[-1], direction, sep = "_"))
data_algues_TdV <- data_algues_TdV[-1, ]

names(data_algues_TdV)[1:9]    # taxon, BAG_PET_1_N, BAG_PET_1_S, ...
dim(data_algues_TdV)           # 10 lignes (taxons), 121 colonnes

algues <- as.data.frame(t(data_algues_TdV[, -1]))
names(algues) <- data_algues_TdV$taxon
#Enlevé les + et passé en numérique
for (col in 1:ncol(algues)) {
  algues[[col]][algues[[col]] == "+"] <- "1"
  algues[[col]] <- as.numeric(algues[[col]])
}
str(algues)
write.csv(algues, "../processed/data_algues_TdV.csv",  fileEncoding = "UTF-8")
test <- read.csv("../processed/data_algues_TdV.csv")
dim(test)

colSums(algues)



##Donnée en présence absence, on pourra peut être les ajouté au jeu de donnée "Borel"
algues__tdv_presence_absence <- read.xlsx("données Algues TdV final.xlsx",sheet = 2)
table(unlist(algues__tdv_presence_absence[, -1]), useNA = "ifany")    # doit montrer seulement x et NA

for (col in 2:ncol(algues__tdv_presence_absence)) {
  algues__tdv_presence_absence[[col]] <- ifelse(is.na(algues__tdv_presence_absence[[col]]), 0, 1)
}

algues_tdv_pa <- as.data.frame(t(algues__tdv_presence_absence[, -1]))
names(algues_tdv_pa) <- algues__tdv_presence_absence$Taxon

str(algues_tdv_pa)
write.csv(algues_tdv_pa, "../processed/algues_tdv_presence_absence.csv",  fileEncoding = "UTF-8")
