##############################################################"
#' Biodiv-data
#' Groupe salicornes 
#' Bonneaudet ; Fraboulet 
#' 2026-2027
#'Exploration de la structure des données ALGUES
#'données de Valentine Anstett
#' ####################################################"
#' 

#### 1 = macrophytes : data_macrophytes_2025

# Végétation aquatique des lagunes en 2025.
# Chaque ligne = un point d'observation dans une lagune
#Dans chaque lagune, 4 directions (N, S, E, W) + 5 points par direction 
# donc 20 lignes par lagune
# Pour chaque point, % de recouvrement (0 à 100 %) de chaque sp
# ou taxon 
#dernière colonne = additionne le % de recouvrement de toutes les sp confondues
#22 variables :
# var1 = annee = année du relevé (toujours 2025)
#var2= LAGUNE = code 
# var3=ORD = direction 
# var4= CONTACT = point ?
#var5 =algues_ABS_COV = recouvrement par des algues en %
#de var6 à var21 = % recouvrement des différentes sp 
#var22 = somme % recouvrement de toutes les sp 

##CHANGER CHEMIN ACCES
macrophytes = read.csv("data/processed/data_macrophytes_2025.csv")

dim(macrophytes)                   # 2440 lignes, 22 colonnes
colSums(is.na(macrophytes))           # Non pas de NA
sum(duplicated(macrophytes))    # lignes en double --> on aurait 20 lignes en double
which(duplicated(macrophytes))     # les lignes en doubles = 1361 1362 1363 1364 1365 1366 1367 1368 1369 1370 1371 1372 1373 1374 1375 1376 1377 1378 1379 1380
# A VERIFIER IMPORTANT

summary(macrophytes)        # summary pour chacune des colonnes, ici pas bcp d'intérêt mais pour autres tableaux  

# Plan échantillonnage 
table(macrophytes$ORD)  # équilibré OK
table(macrophytes$CONTACT)   # idem
length(unique(macrophytes$LAGUNE)) # 121 ID uniques de lagunes = 121 lagunes différentes ?
table(table(macrophytes$LAGUNE))          # on a 120 identifiants de lagunes qui ont 20 lignes et 1 lagune qui a 40 lignes PK ? 

# C'est SIG_GSA_04 qui a 40 lignes au lieu de 20 : pb de code site ? 


# Nbr de relevés où on retrouve chaque sp, pour voir un peu comment se structurent les commus globales
# sommées de  la ligne 6 à la ligne 21 pour avoir que les sp 
colSums(macrophytes[, 6:21] > 0)
# on a Althenia.filiformis qui défonce tout avec présence dans 565 relevés puis Ruppia.maritima présente dasn 325 relevés

# abondances totales des sp en nbr d'individus dans la totalité des relevés 
barplot(colSums(macrophytes[, 6:21]), las = 2) 
#héhéhé à modifier on voit pas le nom des sp !!!!!! 



#----------------------------------------------

### 2. sédiments : data_mineraux
#' analyses de sédiment faites par le labo Teysser
#' 1 ligne = 1 lagune = 1 échantillon
#' ET C'EST TROP CHOUETTE parce qu'on a tout : 
#' - chimie = calcaire, MO, C, N, P, Fe)
#' - granulométrie / texture = argile, limons, sables
#'- propriétés physiques = matière sèche, masse volumique (MVA? savoir comment mesuré)

#' C/N me semble pas mesuré direct mais par dosages du C et du N donc info redondante je pense ; à voir 
#' 
#' 17 var
#' 
#' décrire quand on sera sûrs ; voir avec VALENTINE
#' voir comment est mesurée la masse volumique
#' vor ce que signifie un refus etc 
#' 
sediment=read.csv("data/processed/data_mineraux.csv")
str(sediment)

dim(sediment)                  # on a 120 lignes et 17 colonnes : PK PAS 121 ?!!
colSums(is.na(sediment))  #aucun Na
sum(duplicated(sediment)) #pas de lignes en doubles 
summary(sediment) # A COMMENTER

# Granulométrie : on devrait arriver à 100%
summary(sediment$Argile + sediment$LimonsFins + sediment$LimonsGrossiers +
          sediment$SablesFins + sediment$SablesGrossiers)

table(sediment$Cailloux) # on en a 120 à 0 
table(sediment$Refus) # 69 avec 0 refus, 47 avec 1 refus et 4 avec 2 refus --> Refus ?


