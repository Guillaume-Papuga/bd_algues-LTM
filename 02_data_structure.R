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


#### 6. Notes RDV = à faire = Quels sont les sites présents dans Algues dans lequels on va avoir toutes les données nécessaires ? 
#' Puis rassembler toutes les données dans un ou pls tableaux 
#' #'Prendre que les relevés de mars 2025
#' Donnera 51 lagunes avec qlq manques 
#' puis setdiff avec algues 
#' 
#' entre algues et macrophytes --> si macrophytes > algues on le prend comme référence plutot que algues ? 
#' 
#' créer des sous-tableau qui sont = aux setdiff 
#' --> réenregistrer en cvs dans un sous-dossier dans process --> final 
#' 
#' 
#' puis graphes 

#' Décisions prises avec Valentine :
#' - on garde UNE campagne : mars 2025 = début du pic de production
#'et même période que les relevés macrophytes
#' - tout passer en prés/abs
#' -macrophytes : fréquence = nbr de points où l'sp est vue / 20 points  -->  occurence
#'   (garde pas %rec car biais observateur)
#' - Refus et Cailloux : on les enlève =inutiles 
#' - les stations "co" sont des connexions = pas besoin
#'

#### 6.1 Importation tableaux qui manquent 

# Algues en présence/absence (TdV + Borel)
algues=read.csv("data/processed/algues_pres_abs_final.csv")
str(algues)
dim(algues)                 # 56 lagunes avec 20 taxons + code_site et source
sum(duplicated(algues$code))  # pas de doublon ici

# Hydro avec les dates (2023-2025)  - 1 ligne = 1 lagune à 1 date
hydro_serie=read.csv("data/processed/data_hydro_2023_2025.csv")
str(hydro_serie)      # la salinité est en texte à cause des OFL --> je les mettrai en NA
table(hydro_serie$date_releve) # Cool, on a un max de relevés en mars 2025 donc bien utilisable 


#### 6.2 Environnement en mars 2025


# Hydro de mars 2025
hydro_mars=hydro_serie[hydro_serie$date_releve == "2025-03", ]
dim(hydro_mars)             # on garde 52 lagunes et 12 var c'es bon

# Salinité : OFL = trop salé pour être mesuré --> on met NA + on passe en numérique
hydro_mars$salinite.g.L.[hydro_mars$salinite.g.L. == "OFL"]=NA
hydro_mars$salinite.g.L.= as.numeric(hydro_mars$salinite.g.L.)
hydro_mars$code[is.na(hydro_mars$salinite.g.L.)]   # SIG_GSA_04 

# Nutriments en suspens° dans eau pour 03/2025 - je dégage en + les "Co" ; ce sont des connections, pas utiles 
eau_mars=eau[eau$date == "2025-03", ]
eau_mars=eau_mars[!grepl("_co", eau_mars$ID_LAG), ]
eau_mars=eau_mars[, c("ID_LAG", "silicates", "nitrites", "phosphates", "nitrates", "ammonium")]
names(eau_mars)[1] <- "code"     # même nom de col code partout
dim(eau_mars)
#OK

# Sédiment : on dégage Refus et Cailloux= pas utiles 
sed=sediment[, !(names(sediment) %in% c("Refus", "Cailloux"))]
names(sed)[1]= "code"
# Correction de la faute de frappe cf part 5 entre OBR et ORB
sed$code[sed$code == "OBR_ORP_01"]= "ORB_ORP_01"

#' Je ne sais pas quoi en faire parcq ne sait pas à quoi ça correspond (à voir avec Valentine) :
#' "EOR_MOT_01 ou GM_" et PCA_01 à PCA_08 (PCA_CAP ou PCA_CHA ?)

#### 6.3 Macrophytes en fréquence et en prés/abs

# On enlève les 20 lignes en double de SIG_GSA_04 - celui qui fait 40
macro =macrophytes[!duplicated(macrophytes), ]
table(table(macro$LAGUNE))    # maintenant 20 lignes pour toutes les lagunes c'est OK

# TRUE si l'sp est vue sur le point, FALSE sinon (colonnes 5 à 21, sans le total)
vu=macro[, 5:21] > 0
colSums(vu) #nbr de fois où chaq sp est vue dans les données

# Fréquence d'occurence par lagune = moyenne des TRUE = nbr de points où l'sp est vue / 20 (ce que guillaume appelle point de contact si j'ai bien compris)
macro_freq =aggregate(vu, by = list(code = macro$LAGUNE), FUN = mean) #je veux regrouper toutes les lignes avec le même code lagune, 
#puis fait la moy du nbr de TRUE / 20 pour avoir la fréq d'occurence 
head(macro_freq) #permettra de vérifier 
names(macro_freq) = sub("_ABS_COV", "", names(macro_freq)) #retire la fin parce qu'on est plus en couverture abs
dim(macro_freq)               # 121 lagunes

# Prés/abs par lagune : 1 si vue au moins une fois sur les 20 points
macro_pa=macro_freq #on part de la freq d'occurence
macro_pa[, -1]=(macro_freq[, -1]>0)*1  #je prends toutes les col sauf la première, je transforme les TRUE en 1 et FALSE en 0 dans le corps de ce tableau
colSums(macro_pa[, -1])       # nombre de lagunes où chaque sp est présente sur les 121 lagunes
#' A VOIR : Riella.helicophylla et Zannichellia.pedunculata sont jamais vues,
#' et Ruppia.sp = Ruppia.maritima selon Valentine mais à confirmer


#### 6.4 Setdiff : quelles lagunes sont dans tous les tableaux ?

# Lagunes d'algues absentes des autres tableaux
setdiff(algues$code, hydro_mars$code)   # pas d'hydro en mars 2025 pour "GCA_RNC_02" "GCA_RNC_04" "GCA_RNC_05" "ORB_ORP_11"
setdiff(algues$code, eau_mars$code)  # pas de nutriments en mars 2025 pour "GCA_RNC_02" "GCA_RNC_04" "GCA_RNC_05" "GCA_RNC_07" "ORB_ORP_11"
setdiff(algues$code, sed$code)     # pas de sédiment pour "ORB_ORP_01"
setdiff(algues$code, macro_freq$code)   # pas de macrophytes pour "ORB_ORP_11"

# Lagunes de macrophytes absentes des autres tableaux
setdiff(macro_freq$code, hydro_mars$code) #tout plein
setdiff(macro_freq$code, eau_mars$code) #idem
setdiff(macro_freq$code, sed$code)  #idem 
#normal, on le savait - macrophytes est probablement le + restrictif

# Lagunes présentes partout (intersect = ce qui est commun aux deux)
sites_ok= intersect(algues$code, macro_freq$code)
sites_ok=intersect(sites_ok, hydro_mars$code)
sites_ok=intersect(sites_ok, eau_mars$code)
sites_ok=intersect(sites_ok, sed$code)
length(sites_ok)              # 50 lagunes --> on est OK c'est ce qu'on voulait !
sort(sites_ok)
#' C'est l'environnement de mars 2025 qui limite, pas algues ni macrophytes :
#' que l'on prenne algues ou macrophytes comme référence, on garde les mêmes 50 lagunes
#' Lagunes perdues côté algues : GCA_RNC_02, 04, 05 (pas d'hydro), GCA_RNC_07 (pas de nutriments
#' en mars 2025), ORB_ORP_11 (ni hydro ni macrophytes)
#' 
#' Liste des lagunes gardées : 
#' "BAG_PET_01" "BAG_PET_02" "BAG_PET_03" "BAG_PET_04" "BAG_PET_05" "BAG_PET_06" "BAG_PET_07" "BAG_PET_08"
#[9] "BAG_PET_11" "BPA_PIS_01" "BPA_PIS_02" "BPA_PIS_03" "BPA_PIS_06" "BPA_PIS_07" "BPA_PIS_08" "BPA_PIS_09"
#[17] "CAN_NAZ_01" "CAN_NAZ_02" "CAN_NAZ_05" "CAN_NAZ_06" "CAN_NAZ_07" "CAN_NAZ_08" "EOR_MOT_04" "EOR_MOT_05"
#[25] "EOR_MOT_06" "EOR_MOT_07" "EOR_MOT_08" "EOR_MOT_10" "FOS_REL_05" "FOS_REL_07" "FOS_REL_08" "FOS_REL_09"
#[33] "FOS_REL_10" "GCA_RNC_01" "GCA_RNC_03" "GCA_RNC_06" "GCA_RNC_08" "GCA_RNC_09" "GCA_RNC_10" "ORB_ORP_03"
#[41] "ORB_ORP_05" "ORB_ORP_06" "ORB_ORP_08" "SIG_GSA_01" "SIG_GSA_04" "SIG_GSA_06" "SIG_GSA_07" "SIG_GSA_08"
#[49] "SIG_GSA_09" "SIG_GSA_10"


#### 6.5 Tableaux finaux, puis j'enregistre chacun à part dans data/processed/1_Final
""
# On garde les 50 lagunes dans chaque tableau avec %in% donc "fait partie de la liste "sites_ok""
hydro_final = hydro_mars[hydro_mars$code %in% sites_ok, ] #hydrologie
sels_final = eau_mars[eau_mars$code %in% sites_ok, ] #sels en suspens° 
sediment_final = sed[sed$code %in% sites_ok, ] #sédiments
algues_final = algues[algues$code %in% sites_ok, ] #algues
macro_freq_final = macro_freq[macro_freq$code %in% sites_ok, ] #fréquence occurence macrophytes 
macro_pa_final = macro_pa[macro_pa$code %in% sites_ok, ] # prés/abs macrophytes

# Vérif°= on doit avoir 50 lignes partout, 50 lagunes
nrow(hydro_final)
nrow(sels_final)
nrow(sediment_final)
nrow(algues_final)
nrow(macro_freq_final)
nrow(macro_pa_final)
# C'est OK ! 


# Enregistrement en csv dans processed/final

write.csv(hydro_final, "data/processed/1_Final/hydro_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sels_final, "data/processed/1_Final/sels_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sediment_final, "data/processed/1_Final/sediment_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(algues_final, "data/processed/1_Final/algues_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(macro_freq_final, "data/processed/1_Final/macrophytes_freq_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(macro_pa_final, "data/processed/1_Final/macrophytes_pa_final.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Vérif°: j'essaye de lire 1 fichier
test=read.csv("data/processed/1_Final/sels_final.csv")
head(test)
dim(test)
#j'ai tout


####Le nom des col de codes est le mêmes pour tous les tableaux, donc ne devrait pas poser de pb !




#### 7. Liste des sp pour faire des groupes d'algues : fixées, en suspension, etc...
#besoin que pour algues, macrophytes toutes fixées 
liste_sp = data.frame(            #crée un tableau
  taxon_sp= c(names(algues)[2:21]),  #colonne taxon ou sp 
  groupe = ""                 # je veux laisser une col vide à remplir par un spécialiste 
)
liste_sp
write.csv(liste_sp, "data/processed/1_Final/liste_sp_pour_groupes.csv", row.names = FALSE, fileEncoding = "UTF-8")
# on pourra envoyer cette liste à guillaume
# si besoin on peut l'enregistrer en atre chose que format csv 

#### 8. Premiers graphes - à améliorer

# Nbr de lagunes où chaque algue est identifiée
barplot(colSums(algues_final[, 2:21]), las = 2)
#on pourrait aussi les ranger par ordre décroissant à voir

#Fréq moyenne de chaque macrophyte, toutes lagunes comprises
barplot(colMeans(macro_freq_final[, -1]), las = 2)

# Salinité par site (marge du bas agrandie pour lire les noms de sites)
par(mar = c(10, 4, 2, 1))
boxplot(salinite.g.L. ~ site, data = hydro_final, las = 2)
#ok, 2 valeurs extrêmes - à voir 

# CONTINUER GRAPHES avec descripteurs par sites par ex ? A discuter 