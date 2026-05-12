# Analyse spatiale des inégalités socio-économiques entre Angers et Savennières
## Intro
**Angers** est une ville moyenne située dans la région Pays de la Loire. C’est la préfecture  du Maine-et-Loire, et la deuxième ville la plus peuplée de la région (elle s’étend sur 4270 hectares pour 159 000 habitants dans la ville, et 250 000 habitants dans l’unité urbaine). La superficie de la ville est composée de 73% de terres artificialisées en 2018. 

**Savennières** est une petite ville dans la deuxième couronne de la banlieue sud d’Angers dans le Maine-et-Loire (à une quinzaine de kilomètres du centre-ville). La ville est située dans la zone du val de Loire inscrite au patrimoine mondial de l’UNESCO depuis 2000. Elle est composée à 82,8% de terres agricoles en 2018, avec 35 appellations sur le territoire. C’est un bourg rural selon la grille communale de densité, hors de l’unité urbaine, dans l’aire d’attraction d’Angers. La population de la commune est en moyenne plus âgée que la moyenne du département. 

## Problématique
Dans quelle mesure les dynamiques d’artificialisation et de pression immobilière à Angers renforcent-elles les inégalités socio-économiques entre les espaces urbains et ruraux, et comment ces inégalités se spatialisent-elles à l’échelle des communes et des carreaux ?

## La pauvreté à l'échelle du centre ville d'Angers
Nous avons rencontrés des difficultés d'affichage de notre carte sur R, nous avons donc choisi cette carte en illustration mais nous avons tous de même rédiger le code que vous trouverai ci-dessous 
<img width="1010" height="721" alt="Capture d’écran 2026-05-07 à 17 34 33" src="https://github.com/user-attachments/assets/160dc3a8-2f83-498d-a03b-e863922563e7" />

## Code 
### Récupération des géométries 
library(leaflet)
library(sf)
library(RColorBrewer)
library(happign)
library(terra)

Projet("/Users/aurianeaugereau/Documents/SIG RENDU/SIG RENDU JEANNE AURIANE.R")
communes <- sf::st_read("/Users/aurianeaugereau/Documents/SIG RENDU/contours_communes.shp")
angers <- get_apicarto_cadastre("49100", type = "commune")
tot <- get_apicarto_cadastre(commune, type = "commune")
st_write(tot, "data/projet.gpkg", "commune", delete_layer = T)

### Récupérer une orthophoto
r <- get_wms_raster(angers,
                    layer = "ORTHOIMAGERY.ORTHOPHOTOS",
                    res = 10,
                    crs = 2154,
                    rgb = TRUE,
                    filename = "data/bondy.tif",
                    overwrite = FALSE,
                    verbose = TRUE)

library(terra)
r <- rast("data/angers")
plot(r)

### Découpage du raster 
car <- st_read("data/cours5.gpkg", "carreau_manquant", quiet=T)
carBondy <- car [car$code == 93010,]
plot(carBondy$geom)
cr <- crop(r, carBondy, mask = T)
plot(cr)
carBondy <- st_cast(carAngers,"POLYGON")
carBondy <- carBondy [1,]
cr <- crop(r, carBondy, mask = T)
plot(cr)

#GT Vecteur : Centroides et tampon 

pt <- st_centroid(bondy)
longlat <- unlist(pt$geometry )
iso <- get_isochrone(pt, time = 15, profile = "pedestrian", source = "pgr")
library(rgeoservices)
iso <- gs_get_isochrone(
  longitude = longlat [1],
  latitude = longlat [2],
  cost_value = 15,
  profile = "pedestrian",
  direction = "departure",
  time_unit = "minute"
)
plot(iso)


## Description de la donnée 
Le centre-ville d'Angers est un espace hétérogène où cohabitent des situations socio-économiques très différentes. En effet, les seuils de pauvreté y sont élevés. Cela tient en partie à la structure sociale du centre, ainsi qu'au nombre élevé de petits logements à destination d'étudiants ou de jeunes actifs. Cette population se trouve pour beaucoup aux alentours du château, du quartier de la Doutre ou de Saint-Serge. On observe toutefois une tendance à la gentrification depuis quelques années, avec la réhabilitation de logements dans le centre-ville et dans les quartiers périphériques.  Toujours en lien avec les étudiants, des quartiers se dynamisent grâce à l'arrivée du tram et l'installation du campus Belle-Beille. Le quartier de Patton voit une grande quantité d'étudiants arriver, ce qui a eu tendance à augmenter le prix de l'immobilier et des loyers (logements neufs, services à la population).

## Schéma de jointure spatiale 
<img width="950" height="378" alt="Capture d’écran 2026-05-07 à 18 08 39" src="https://github.com/user-attachments/assets/e345dcf2-15a0-4558-8407-62b376519a28" />


# La pauvreté à l'échelle de l'aire d'attraction d'Angers (dont fait parti Savennière) - données vectorielles 
<img width="565" height="367" alt="Capture d’écran 2026-05-07 à 17 42 53" src="https://github.com/user-attachments/assets/983f8259-9cdc-40db-84d5-91bb7f6a5af4" />

# Code 

### Charger les bibliothèques nécessaires
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)

### 1. Charger les données
setwd("/Users/aurianeaugereau/Documents/SIG RENDU/SIG RENDU JEANNE AURIANE.R")
communes <- st_read("contours_communes.shp") %>%
  st_transform(4326)  # Reprojection en WGS84
carreaux <- st_read("carreaux_pauvrete.shp") %>%
  st_transform(4326)

### 2. Préparer les palettes de couleurs pour les taux de pauvreté
seuils_pauvrete <- c(0, 3.6, 9.5, 18.4, 29.6, 72.2)
couleurs_pauvrete <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b2df8a")
pal_pauvrete <- colorBin(
  palette = couleurs_pauvrete,
  domain = carreaux$taux_pauvrete,  # Remplace par ta colonne
  bins = seuils_pauvrete,
  na.color = "transparent"
)

### 3. Créer la carte avec leaflet
carte <- leaflet() %>%
#Ajouter un fond de carte (OpenStreetMap)
  addTiles() %>%
  addPolygons(
    data = carreaux,
    fillColor = ~pal_pauvrete(carreaux$taux_pauvrete),  
    fillOpacity = 0.8,
    color = "white",
    weight = 0.5,
    popup = ~paste(
      "<b>Taux de pauvreté :</b>", round(taux_pauvrete, 1), "%<br>",
      "<b>Commune :</b>", nom_commune  
    )
  ) %>%

Ajouter les limites communales (en noir)
  addPolygons(
    data = communes,
    fillColor = "transparent",
    color = "black",
    weight = 2,
    popup = ~nom  # Remplace par ta colonne
  ) %>%

Ajouter les quartiers politiques (en vert)
  addPolygons(
    data = quartiers,
    fillColor = "transparent",
    color = "green",
    weight = 2,
    popup = ~nom_quartier  # Remplace par ta colonne
  ) %>%

Ajouter une légende pour les taux de pauvreté
  addLegend(
    position = "topright",
    pal = pal_pauvrete,
    values = ~taux_pauvrete,  # Remplace par ta colonne
    title = "Taux de pauvreté (en %)",
    labFormat = labelFormat(
      suffix = "%",
      between = " - ",
      digits = 1
    ),
    bins = seuils_pauvrete
  ) %>%

Ajouter une légende pour les limites
  addLegend(
    position = "bottomright",
    colors = c("black", "green"),
    labels = c("Limites communales", "Quartiers Politique de la Ville"),
    title = "Limites administratives"
  ) %>%

 Ajouter un titre
  addControl(
    html = "
    <div style='text-align: center; background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);'>
      <h2 style='margin: 0; color: #2c3e50;'>Carte des taux de pauvreté à Angers et Savennières</h2>
      <p style='margin: 5px 0 0 0; color: #7f8c8d; font-size: 14px;'>
        Données : INSEE, 202X | Échelle : Carreaux de 200m
      </p>
    </div>
    ",
    position = "topcenter"
  ) %>%

Centrer la carte sur Angers et Savennières
  fitBounds(
    lng1 = min(st_bbox(communes)[["xmin"]]),
    lat1 = min(st_bbox(communes)[["ymin"]]),
    lng2 = max(st_bbox(communes)[["xmax"]]),
    lat2 = max(st_bbox(communes)[["ymax"]])
  )

## Descirption de la donnée 
Le centre-ville d'Angers, et en particulier l'est de la ville possède un taux de pauvreté allant jusque 72%. C'est un espace avec de fortes inégalités socio-spatiales, entre des populations très précaires et d'autres de la classe moyenne. Les espaces à haut taux de pauvreté sont concentrés surtout autour des axes routiers (rocade), ce sont des espaces avec de faibles aménités et donc un prix de l'immobilier plus faible. Toutefois, la commune de Savennières au sud-ouest de la ville d'Angers a elle un taux de pauvreté très faible (entre 0 et 3,6% à certains endroits). C'est une banlieue très aisée d'Angers qui contraste grandement avec le centre-ville. On peut déduire qu'il s'agit de deux types de population différentes, avec notamment une population urbaine, qui subit son lieu de vie, et une population aisée, qui s'est installée à Savennières pour le cadre et les aménités; C'est aussi une population qui a un accès facile à la voiture pour se rendre aux espaces avec des services, ou dans le centre-ville d'Angers, et qui a donc sûrement un capital financier élevé.


## Conclusion 
Angers Loire Métropole est une aire d'attraction avec une géographie complexe et multiforme de la pauvreté dans l’Aire d’Attraction d’Angers, démentant les idées reçues sur sa répartition spatiale. Les résultats révèlent que la précarité ne se limite pas aux périphéries éloignées ou aux zones rurales isolées, mais s’étend également au cœur même de la ville, où des dynamiques socio-économiques spécifiques maintiennent des populations dans des situations de fragilité.
