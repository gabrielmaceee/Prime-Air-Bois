---
output:
  html_document: default
  pdf_document: default
---
# explorePAB

ExplorePAB est une interface Shiny permettant d'explorer les données issues du suivi des dossiers de demande de primes Air-Bois, ainsi que les résultats de notre analyse des données, et ceci à travers des visualisations graphiques. Elle vise à mieux comprendre le comportement des demandeurs sur le territoire. Elle a été développée 



## Packages requis

 - shiny
 - readxl
 - writexl
 - ggplot2
 - dplyr
 - tidyr
 - janitor
 - gridExtra
 - sf
 - ggspatial
 - plotly
 - mapview
 - leafpop
 - leaflet
 - grid
 - glue
 - ggmosaic
 - epitools
 

## Origine des données

Les données sont fournies par l’ALEC et l’AGEDEN, et sont de plusieurs origines :

 - Les bases de données d’instruction présentant les données de gestion de 8542 dossiers de demande de la Prime Air Bois, qu’on peut relier, grâce aux numéros de dossier, au questionnaire en amont du projet, présentant 7447 individus et 40 variables décrivant les bénéficiaires, leurs matériels, et leurs comportements quant au chauffage au bois. 
 - Environ 550 bénéficiaires ont rempli un questionnaire facultatif en aval du projet, où les questions étaient plutôt portées  sur leur satisfaction et leur nouveau mode d’utilisation de l’appareil. 

Toutes ces données ont été collectées de novembre 2015 à mai 2024, et ceci sur trois territoires : Le Pays Voironnais (CAPV), le Grésivaudan (CCLG), et Grenoble Alpes Métropole (GAM). La base de données principale à analyser est le questionnaire en amont du projet, à laquelle nous avons ajouté plusieurs variables, telles que les différentes dates de demande et de versement de la prime, le coût total TTC des travaux, le montant d'aide demandé, le statu majoré ou non de la prime, le coût de l’appareil, ou encore la commune du demandeur.

Afin de compléter ces données, nous avons pu en récupérer d’autres, notamment au niveau communal :
- Grâce à l’outil SIDDT de INRAE, nous avons pu récupérer des variables socio-démographiques, telles que le nombre de maisons, le nombre de ménage par catégorie socioprofessionnelle, la densité de population au km², ou encore la part de forêt dans la commune. 
- Grâce à l’INSEE, nous avons pu récupérer la [médiane du niveau de vie par commune](https://www.insee.fr/fr/statistiques/7756729?sommaire=7756859), et le [type de chaque commune](https://www.insee.fr/fr/statistiques/5039991?sommaire=5040030#onglet-1).
- L’observatoire de la qualité de l’air « Atmo Auvergne-Rhône-Alpes », nous a fourni des données indiquant par commune le nombre d’appareils anciens au 31 décembre 2022, donc ceux à changer, ainsi que le nombre de ces appareils changés en 2023.


## Fonctionnement de l'interface

L'interface se veut simple et est organisé en trois parties :

### Exploration : 

Cette première partie permet d'explorer les différentes variables, une par une puis deux par deux, à l'aide de représentations graphiques adaptées et de cartographies. Aussi, il y a une couleur différente pour chaque territoire que vous représentez :

 - Gris si vous sélectionnez tous les territoires.
 - Vert pour le Pays Voironnais (CAPV)
 - Bleu pour Grenoble Alpes Métropole (GAM)
 - Rouge pour le Grésivaudan (CCLG)


### Indicateurs : 

La seconde partie contient une sélection de graphiques spécifiques, ainsi qu'une carte, permettant de mieux comprendre les données. Elle est divisée en deux sections:

 - "La prime et ses bénéficiaires" : présentant la distribution générale de la prime;
 - "Modes d'usages" : Présentant les évolutions de comportement avant et après le changement d'appareils.

### Typologies : 

La dernière partie présente les résultats que nous avons obtenus en séparant les données en cluster : les liens entre les groupes et les variables, et la répartition géographique des groupes. Nous avons fait cette analyse à deux niveaux : au niveau communal et à celui de l'individu, ils constitueront chacun une section.
