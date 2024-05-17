#------------------------------------------------------------------------------#
# SCRIPT : MANIPULER LES VECTEURS
#------------------------------------------------------------------------------#

# LIBRAIRIES ET DATA ----
## Chargement des librairies mobilisées dans le script ---- 
library("dplyr")      # Manipulation de données 
library("sf")         # Manupulation de données vectorielles
library("terra")      # Manipulation de données raster
library("mapsf")      # Cartographie thématique
library("units")      # Manipulation des unités de mesure
library("rmapshaper") # Simplification des formers vectorielles
library("readxl")     # Chargement de données excel


#------------------------------------------------------------------------------#

## Chargement des données mobilisées dans le script ---- 

# geometries des delegations
st_layers("data/geom/tun_admin.gpkg")
del <- st_read(dsn = "data/geom/tun_admin.gpkg", 
               layer = "delegation")

# geometries ponctuelles
# Consulter le contenu du géopackage "tun_osm"
st_layers("data/geom/tun_osm.gpkg")
poi <- st_read(dsn = "data/geom/tun_osm.gpkg ", 
               layer = "poi")

## Données attributaires
# Importer les fichiers locaux
del_df <- read.csv("data/don_del.csv", sep = ";", dec = ",")

# Ecrire des données ----
# st_write(obj = del, 
#          dsn = "data/mar1Vector.gpkg", 
#          layer = "delegation")
# 
# st_write(obj = del, 
#          dsn = "data/mar1Vector.gpkg", 
#          layer = "delegation", 
#          delete_layer = TRUE)
# 
#------------------------------------------------------------------------------#

# Premieres explorations ----

## Données tabulaires ----
head(del)
str(del)
colnames(del)
summary(del)


## Géométries ----

# Type de géométries
st_geometry_type(del, by_geometry = FALSE)

# Afficher toutes les champs
plot(del)

# Afficher les géométries uniquement
plot(st_geometry(del))

# ou
plot(del$geom)

#------------------------------------------------------------------------------#
# Les systemes de coordonées ----
#------------------------------------------------------------------------------#

# tout le détail du crs
st_crs(del)

# sauvegarder les parametres du crs dans un objet
crsDel <- st_crs(del, parameters = TRUE)

# acceder aux parametres un par un 
crsDel$Name

st_crs(del)$srid

## Changer la projection ----

del <- st_transform(del, crs = "EPSG:22332")

st_crs(del)$Name

# visualiser les objets dans la nouvelle projection avec  le graticule
plot(st_geometry(del), border = "lightblue", lwd = 2, col = NA,
     graticule = TRUE)


#------------------------------------------------------------------------------#
# Selections et jointure attributaire ---- 
#------------------------------------------------------------------------------#

## Selectionner des lignes ----
del[1:5,]
del[del$del_nom_fr == "Sousse Medina", ]

## Selectionner des colonnes ---- 
del[, ncol(del)-5:ncol(del)]
del[, c("del_nom_fr", "gou_nom", "reg_nom")]

## Combiner les selections ----
delSousse <- del[del$gou_nom %in% "Sousse", c("del_nom_fr", "gou_nom", "reg_nom")]

# Afficher les géométries   
# Parametre de l'affichage
par(mar = c(0, 0, 4, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")

plot(st_geometry(delSousse), col = "#5B89A3", border = "white", lwd = 2)
title(paste(unique(delSousse$gou_nom)))

#------------------------------------------------------------------------------#

## Jointure attributaire ----
# Il faut d'abord vérifier le champ de jointure
delMerge <- merge(x = del,
                  y = del_df, 
                  by.x = "del_code",
                  by.y = "del_code",
                  all.x = TRUE)


# Les deux objets ont bien été joints
head(delMerge, 3)

#------------------------------------------------------------------------------#
# Selections et jointure spatiale ----
#------------------------------------------------------------------------------#

# Verifier les projections des objets à comparer spatialement 
st_crs(poi)$srid
st_crs(del)$srid

# Changer le CRS de poi pour correspondre à EPSG:22332
poi <- st_transform(poi, crs = "EPSG:22332")

# Intersection des points dans les polygones de Sousse, 
poiSousse <- st_filter(x = poi, 
                       y = delSousse,
                       .predicate = st_within)

# Afficher le résultat
# Parametre de l'affichage
par(mar = c(0, 0, 4, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")

# Initialisation de la carte à l'emprise de Sousse
plot(st_geometry(delSousse), col = NA, border = NA)

# Fond de carte des délégations
plot(st_geometry(del), col = "gray80", border = "white", lwd = 1, add = TRUE)

# Délégations de Soussz
plot(st_geometry(delSousse), col = "#5B89A3", border = "white", lwd = 2, add = TRUE)

# Points remarquables en Tunisie
plot(st_geometry(poi), col = "red", border = "white", pch = 19, cex = .3, add = TRUE)

# Points remarquables de Souss
plot(st_geometry(poiSousse), col = "green", border = "white", pch = 19, cex = .3, add = TRUE)

# Titre
title("Points remarquables \ndu Gouvernorat de Sousse")

#------------------------------------------------------------------------------#
# GEOTRAITEMENTS - Opérations sur les geometries ----
#------------------------------------------------------------------------------#

## Extraire les centroides ----

delSousse_c <- st_centroid(delSousse)

# Afficher le résultat
# Parametre de l'affichage
par(mar = c(0, 0, 0, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")

# Délégations de Sousse
plot(st_geometry(delSousse), col = "#5B89A3", border = "white")

# Centroides des délégations de Sousse
plot(st_geometry(delSousse_c), add = TRUE, pch = 20, col = "pink")

#------------------------------------------------------------------------------#

## Agregation sans stat ----
gouSousse <- st_union(delSousse)

# Parametre de l'affichage
par(mar = c(0, 0, 0, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")

# Initialisation de la carte à l'emprise de Sousse
plot(st_geometry(delSousse), col = NA, border = NA)

# Fond de carte des délégations
plot(st_geometry(del), col = "gray80", border = "white", lwd = 1, add = TRUE)

# Délégations de Sousse
plot(st_geometry(delSousse), col = "#5B89A3", border = "white", lwd = 1, add = TRUE)

# Gouvernorat de Sousse
plot(st_geometry(gouSousse), border = "darkblue", lwd = 3, add = TRUE)

#------------------------------------------------------------------------------#

## Agregation spatiale avec résumé statistique identique ----
gou <- aggregate(
  x = delMerge[c("popto_2014", "immig_2014")], 
  by = list(gou_nom = delMerge$gou_nom.x), 
  FUN = sum
)

# Parametre de l'affichage
par(mar = c(0, 0, 0, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")

# Fond de carte des délégations
plot(st_geometry(del), col = "gray80", border = "white", lwd = 1)

# Gouvernorats
plot(st_geometry(gou), border = "#5B89A3", lwd = 2, add = TRUE)

#------------------------------------------------------------------------------#

## Agregation spatiale avec resume statistique différents ----
library(dplyr)

gou <- delMerge |> 
  group_by(gou_nom.x) |> 
  summarise(pop = sum(popto_2014),
            immig_mean = mean(immig_2014))

#------------------------------------------------------------------------------#

## Zone tampon - Buffer
# Connaitre l'unité
st_crs(delSousse_c)$units

# Sélection du centroide de Sidi Bou Ali 
sidiBou_c <- delSousse_c[delSousse_c$del_nom_fr %in% "Sid Bou Ali", ]

# Zone tampon de 5km = 5000m
sidiBou_t <- st_buffer(sidiBou_c, dist = 5000)

# AFFICHER
# Parametre de l'affichage
par(mar = c(0, 0, 0, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")
plot(st_geometry(delSousse), col = NA, border = NA)
plot(st_geometry(del), col = "gray80", border = "white", lwd = 1, add = TRUE)
plot(st_geometry(delSousse), col = "#5B89A3", border = "white", lwd = 1, add = TRUE)
plot(st_geometry(gouSousse), border = "darkblue", lwd = 3, add = TRUE)
plot(st_geometry(sidiBou_t), border = "pink", col = "#fac0cb50", lwd = 2, add = TRUE)
plot(st_geometry(sidiBou_c), col = "pink", pch = 20, cex = 2, add = TRUE)


#------------------------------------------------------------------------------#

## Intersection ----
# découpe la couche de point en fonction de la géométrie de la couche du tampon
poi_sidiBou <- st_intersection(x = sidiBou_t, y = poiSousse)

# AFFICHER
# Parametre de l'affichage
par(mar = c(0, 0, 0, 0),  xaxs='i', yaxs='i', bg = "#F1F3F5")

# Initialisation de la carte à l'emprise de Sousse
plot(st_geometry(delSousse), col = NA, border = NA)
plot(st_geometry(del), col = "gray80", border = "white", lwd = 1, add = TRUE)
plot(st_geometry(delSousse), col = "#5B89A3", border = "white", lwd = 1, add = TRUE)
plot(st_geometry(gouSousse), border = "darkblue", lwd = 3, add = TRUE)
plot(st_geometry(sidiBou_t), border = "pink", col = "#fac0cb50", lwd = 2, add = TRUE)
plot(st_geometry(sidiBou_c), col = "pink", pch = 20, cex = 2, add = TRUE)
plot(st_geometry(poi_sidiBou), col = "red", border = "white", pch = 19, cex = .5, add = TRUE)

#------------------------------------------------------------------------------#

## Compter les points

### Compter les points dans un seul objet ----
# La fonction st_intersects() compte les points sans modifier les couches
# L'objet retourné n'st pas spatial
inter <- st_intersects(x = sidiBou_t, y = poi_sidiBou)

inter
# Verifier qu'il y a autant de liste de points qu'il y a de polygones intersectés
length(inter) == nrow(sidiBou_t)

# Nombre de points comptés par polygones
lengths(inter)

# Integrer ces décomptes comme une variable de la zone tampon (nombre de points par deleg)
sidiBou_t$nb_poi <- lengths(inter)

### Compter les points dans tous les gouvernorats ----
interGou <- st_intersects(x = gou, y = poi)

interGou
# Le nombre d'intersections est-il égal aux objets de gou
length(interGou) == nrow(gou)

# combien y a t il de points par intersection
lengths(interGou)

# Ajout du nombre de points intersectés à l'objet gou
gou$nb_poi <- lengths(interGou)

### Cartographier le comptage de points ----
library(mapsf)

# intitialisation du fond de carte
mf_map(x = gou, border = "white", lwd = 0.5)

# cartographie du nombre de points en cercles proportionnels
mf_map(x = gou,
       var = "nb_poi",
       type = "prop",
       border = "white",
       col = "#FF000080",
       leg_title = "Nombre de points remarquables",
       inches   = 0.4, leg_pos  = "topright")

# Habillage
mf_layout(title = "Equipements dans les gouvernorats",
          arrow = TRUE, 
          scale = TRUE, 
          credits = "GeoUnivR 2024 - Tunisie")


#------------------------------------------------------------------------------#

## Changer le type de géométries ----

# connaitre le type de geometrie rapidement
st_geometry_type(del, by_geometry = FALSE)

# Transformer les multipolygones en multilignes
del_line <- st_cast(del, to = "MULTILINESTRING")

#------------------------------------------------------------------------------#
# UNITE ET CALCULS ----
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Connaitre l'unité 
st_crs(del, parameters = TRUE)$units_gdal

#------------------------------------------------------------------------------#

## Calculs ----

### superficie ----
# Superficie des 5 premier-es délégations
st_area(del[1:5, ])

# Nouvelle variable de superficie pour toutes les délégations
del$area <- st_area(del)

### longueur ou perimetre ----

# longueur des lignes
st_length(del_line[1:5, ])

# perimetre de toutes les lignes
del_line$perimetre <- st_length(del_line)

### Distances ----

# Automatiquement sur les centroides d'un objet polygon
# Matrice de distance entre les centroides des 5 premieres delegations
st_distance(del[1:5, ])

# Matrice de distances entre les centroides des delegations
distances <- st_distance(del[1:5, ])

#------------------------------------------------------------------------------#

## Unite et conversion 
# Chargement du package
library(units)

# unite de distance
units(distances)

# Modification de l'unité de l'objet distance
set_units(x = distances, value = km)

#------------------------------------------------------------------------------#
# ALLER PLUS LOIN ----
#------------------------------------------------------------------------------#

## Simplifier les géométries

# Fortement simplifier avec sf
del_simp_sf <- st_simplify(del, dTolerance = 5000, preserveTopology = TRUE)

# Avec rmapshaper qui est mieux
library(rmapshaper)

# simple généralisation des géométries avec mapshaper
del_simp_rmap <- ms_simplify(del)

# Forte généralisation des géométries avec mapshaper
del_simp_rmap2 <- ms_simplify(del, keep = 0.001, keep_shapes = TRUE)


# Visualiser les generalisations
# Parametrer la sortie grapihque
par(mfrow = c(1,4),
    mar = c(0, 1, 3, 1),
    xaxs='i', yaxs='i', 
    bg = "#F1F3F5")

# Cartographie des géométries
plot(del$geom, col = "#5B89A3", border = "white")
title("Géométries \ninitiales")

# Carto de la simplification avec sf
plot(del_simp_sf$geom, col = "#5B89A3", border = "white")
title("Simplification avec sf")

# Carto de la simplification avec mapshaper
plot(del_simp_rmap$geom, col = "#5B89A3", border = "white")
title("Simplification avec \nrMapshaper")

# Carto de la forte simplification avec mapshaper
plot(del_simp_rmap2$geom, col = "#5B89A3", border = "white")
title("Forte simplification \navec rMapshaper")

#------------------------------------------------------------------------------#

## Construction d'une grille réguliere ----
### Création de la grille de cellules carrées ----
grid <- st_make_grid(gou, cellsize = 35000)

# Ajout d'un identifiant unique et passage en sf 
grid <- st_sf(ID = 1:length(grid), geom = grid)

head(grid)

# Dessin des gouvernorats et de la grille
par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i', bg = "#F1F3F5")
plot(st_geometry(gou), col = "#5B89A3", border = "white", lwd = 1)
plot(st_geometry(grid), col = NA, border = "black", lwd = 1, add = TRUE)


### Création de la grille de cellules hexagonales ----

grid_hex <- st_make_grid(gou, cellsize = 35000, square = FALSE)

# Ajout d'un identifiant unique et passage en sf 
grid_hex <- st_sf(ID = 1:length(grid_hex), geom = grid_hex)

# Dessin de la grille hexagonale
par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i', bg = "#F1F3F5")
plot(st_geometry(gou), col = "#5B89A3", border = "white", lwd = 1)
plot(st_geometry(grid_hex), col = NA, border = "black", lwd = 1, add = TRUE)

### Dessin des centres et des angles d'une grille ----

par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i', bg = "#F1F3F5")

plot(st_geometry(gou), col = "#5B89A3", border = "white", lwd = 1)

# Les centres
plot(st_make_grid(gou, cellsize = 35000, what = "centers"), col = "red", pch = 20, add = TRUE)

# Les angles
plot(st_make_grid(gou, cellsize = 35000, what = "corners"), col = "pink", pch = 3, add = TRUE)

## Intersecter la grille et les points
inter <- st_intersects(grid, poi, sparse = TRUE) 

# vérifier l'intersection
length(inter) == nrow(grid)

# Jointure des résultats dans la grille
grid$nb_poi <- lengths(inter)

# Garder uniquement les cellules intersectant la tunisie
grid_f <- st_filter(grid, gou, .predicate = st_intersects)

### Cartographie du resultat ----
# intitialisation du fond de carte
mf_map(x = grid_f, border = "white", lwd = 0.5)

# cartographie du nombre de points en cercles proportionnels
mf_map(x = grid_f,
       var = "nb_poi",
       type = "prop",
       border = "white",
       col = "#FF000080",
       leg_title = "Nombre de points remarquables",
       inches   = 0.4, leg_pos  = "topright")

#------------------------------------------------------------------------------#

## Passer vers un objet vectoriel terra
library(terra)

# conversion de l'objet sf en objet terra 
grid_spatVect <- vect(grid)
class(grid_spatVect)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

