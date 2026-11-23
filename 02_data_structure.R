##############################################################"
#' Biodiv-data
#' Groupe Algues 
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

dim(sediment)                  # on a 120 lignes et 17 colonnes : PK PAS 121 ?!! (Bagnas 11 sites au lieu de 10 donc viendrait de là)
colSums(is.na(sediment))  #aucun Na
sum(duplicated(sediment)) #pas de lignes en doubles 
summary(sediment) # A COMMENTER

# Granulométrie : on devrait arriver à 100%
summary(sediment$Argile + sediment$LimonsFins + sediment$LimonsGrossiers +
          sediment$SablesFins + sediment$SablesGrossiers)

table(sediment$Cailloux) # on en a 120 à 0 
table(sediment$Refus) # 69 avec 0 refus, 47 avec 1 refus et 4 avec 2 refus --> Refus ?

#----------------------------------------------
### 3. Nutriments en suspension/eau, avec suivi dans le temps : data_sel
#'Nutriments dissouts dans l'eau des lagunes 
#' 1 ligne = 1 lagune pour 1 date de prélèvement 
#' sur 6 campagnes
#' de 2023 à 2025 --> On prend que le 2025)
#' donc les lagunes appraissent pls fois
#' 5 nutriments : silicates, nitrites, phosphates, nitrates, ammonium --> en µM 
#' 
#' IMPORTANT : Certaines stations ont un code en "co" comme RNC_co34 MAIS elles n'existent pas
#' dans macrophytes DONC il faut vérifier ce qu'elles représentent
#' 
#' Si salinité = OFL --> signifie que c° trop haute pour être mesurée (sinon calculable via conductivité X T°C)
#' Plein de fois 0.08 pour les nitrates --> normal ou limite de détection ? 
#' 
#' var1 = Site = nom du site (8 sites, ex. Grande Camargue)
#'var2 = ID_LAG = code de la lagune ou de la station
#' var3 = date = campagne de prélèvement (année-mois)
#var4 = silicates = silicates dissous
#' var5 = nitrites = nitrites dissous
#'var6 = phosphates = phosphates dissous
#' var7 = nitrates = nitrates dissous
#' var8 = ammonium = ammonium dissous
#' 
eau <- read.csv("data/processed/data_sel.csv")
str(eau)  #ok pour les num

dim(eau)           # 354 lignes et 8 col
colSums(is.na(eau))  #pas de NA
sum(duplicated(eau)) #pas de lignes en dbl
summary(eau)    #cf les descripteurs numériques pour chaque var 

# Plan échantillonnage au fil du temps
table(eau$date)  # on a pas le même nbr d'échantillonnage à chaque campagne, va de 47 à 67
table(eau$Site)    #nbr de relevés faits dans chaque site, entre 27 et 71
table(eau$ID_LAG, eau$date)     # lagunes échantillonnées ou pas à chaq campagne

# On pense voir bcp de 0.08, compter nbr ?
sum(eau$nitrates == 0.08)  # 81 fois la valeur 0.08 pour les nitrates --> c'est une limte de détection 
#a quelles dates ? différences entre années ?
table(eau$nitrates == 0.08, eau$date) # c'est entre fin 2024 et 2025, pas avant --> changements ? + de pluies ?

#----------------------------------------------
#### 4. pour hydro total, moyennes par lagune : data_hydro_total2
###en charger un autre car j'ai pas les dates 

#' "le + gros merdier à démeller" 
#'pour résumer l'environnement de chaque lagune 
#'1 ligne = 1 lagune, var moyennées 
#' On a 
#' turbidité, température, hauteur d'eau, salinité, conductivité,
#'  chla (médiane),  données de sédiment, nutriments 
#'=moyennés sur les 6 campagnes 
#'
#' IMPORTANT = colonnes sédiments vides pour Canet St Nazaire et l'Etang de Pissevache
#' 
#' 20 var = 
#' var1  = site = nom du site
#' var2  = code = code de la lagune (même format que LAGUNE dans macrophyte)
#' var3  = num_lagune = numéro de la lagune dans son site
#' var4  = Lon = longitude (degrés décimaux)
#' var5  = Lat = latitude (degrés décimaux)
#' var6  = turbidite_moy = turbidité moyenne
#' var7  = temperature_moy = température moyenne de l'eau (°C)
#' var8  = hauteur_eau_moy.cm. = hauteur d'eau moyenne (cm)
#' var9  = salinite_moy.g.L. = salinité moyenne (g/L)
#' var10 = conductivite_moy.mS.cm. = conductivité moyenne (mS/cm)
#' var11 = chlorophylleA_med = chlorophylle a, médiane (indicateur du phytoplancton)
#' var12 = MO_TOT... = matière organique du sédiment (%)
#' var13 = AZOTE_TOT.mg.kg. = azote total du sédiment (mg/kg)
#' var14 = C.N = rapport C/N du sédiment
#' var15 = P2O5_TOT... = phosphore total du sédiment (%)
#' var16 = Silicates.µM..SiOH4. = silicates moyens (µM)
#' var17 = Nitrites.µM..NO2. = nitrites moyens (µM)
#' var18 = Phosphates.µM..PO4. = phosphates moyens (µM)
#' var19 = Nitrates.µM..NO3. = nitrates moyens (µM)
#' var20 = Ammonium.µM..NH4. = ammonium moyen (µM)

#'
hydro <- read.csv("data/processed/data_hydro_total2.csv")
str(hydro)

dim(hydro)                # 52 lignes et 20 col
colSums(is.na(hydro))   # pas mal de NA pas répartis au hasard -- relevés précis ?
mean(is.na(hydro))        #5% d NA dan sce tableau
sum(duplicated(hydro$code))     # pas de lignes en dbl
summary(hydro)

table(hydro$site) # entre 5 et 9 par site
hydro$code[is.na(hydro$C.N)]   # lagunes sans prélèvements pour C.N
#[1] "CAN_NAZ_01" "CAN_NAZ_02" "CAN_NAZ_05" "CAN_NAZ_06" "CAN_NAZ_07" "CAN_NAZ_08" "BPA_PIS_01" "BPA_PIS_02"
#[9] "BPA_PIS_03" "BPA_PIS_06" "BPA_PIS_07" "BPA_PIS_08" "BPA_PIS_09"
#c'est les 13 premières 
#et c'est idem pour les autres NA
#ca concerne 2 sites : 
# - Canet Saint Nazaire
# - Etang de pissevache

##IMPORTANT savoir quoi faire de ces deux sites et de ce manq de données 

#----------------------------------------------
#### 5. Qu'est-ce qui diffère entre les données et qui pourrait poser pb ? 
#j'utilise setdiff 
?setdiff

#' Codes lagunes dans sédiment absents dans macrophyte (codes utilisés différents ?)
setdiff(sediment$LABORATOIRE.TEYSSIER, macrophytes$LAGUNE)
#[1] "OBR_ORP_01"        "ORB_ORP_11"        "BAG_PET_09"        "PCA_01"            "PCA_02"           
#[6] "PCA_03"            "PCA_04"            "PCA_05"            "PCA_06"            "PCA_07"           
#[11] "PCA_08"            "EOR_MOT_01 Ou GM_"

# Lagunes dans macrophytes sans analyse  sédiment
setdiff(macrophytes$LAGUNE, sediment$LABORATOIRE.TEYSSIER)
#"PCA_CAP_01" "PCA_CAP_02" "PCA_CAP_03" "PCA_CAP_04" "PCA_CAP_05" "PCA_CHA_01" "PCA_CHA_03" "PCA_CHA_05"
#[9] "ORB_MAI_05" "EOR_MOT_01" "ORB_ORP_01" "ORB_ORP_09" "ORB_ORP_10"

# Lagunes de Macrophytes sans données d'eau
setdiff(macrophytes$LAGUNE, hydro$code)
#là y'en a un paquet...

# Stations prélèvements d'eau absentes de macrophytes
setdiff(eau$ID_LAG, macrophytes$LAGUNE)
#C'est les Co 
# "RNC_co34" "RNC_co37" "RNC_co30" "RNC_co31" "RNC_co32" "RNC_co89" "CAN_co21" "PIS_co23" "PIS_co30"
#[10] "PIS_co70" "EOR_co40" "ORP_co50" "BAG_co30" "CAN_co60" "ORP_co30" "SIG_co80" "SIG_co90" "PIS_co21"
###IMPORTANT = les Co ce sont des connections, on n'en a pas vraiment besoin ! 


#### 6. Quels sont les sites présents dans Algues dans lequels on va avoir toutes les données nécessaires ? 
#' Puis rassembler toutes les données dans un ou pls tableaux 
#' #'élagage entre fin 2024 et fin 2025
#' puis setdiff avec algues 
#' 
#' entre algues et macrophytes --> si macrophytes > algues on le prend comme référence plutot que algues ? 
#' 
#' créer des sous-tableau qui sont = aux setdiff 
#' --> réenregistrer en cvs dans un sous-dossier dans process --> final 
#' 
#' 
#' puis graphes 
