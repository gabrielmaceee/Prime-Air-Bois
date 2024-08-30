---
output:
  html_document: default
  pdf_document: default
---
# Prime Air-Bois

Scripts traitement et visualisation de données Prime Air-Bois

## Content


## Getting started

La gestion des credentials se fait en dehors de ce depot.
On utilise désormais le fichier .Renviron pour stocker ces infos et eviter d'avoir a gérer des fichiers locaux.

Pour localiser le fichier .Renviron, utiliser la commande suivante:

```r
usethis::edit_r_environ()

Message de retour :
• Modify 'C:/Users/frederic.bray/Documents/.Renviron'
• Restart R for changes to take effect
```
Ouvrir le fichier .Renviron et ajouter les lignes suivantes en mettant vos propres credentials

```bash
DB_SIDDT_HOST="siddt.inra.local"
DB_SIDDT_NAME="basedtm"
DB_SIDDT_USER="your_user_name"
DB_SIDDT_PW="your_password"
DB_SIDDT_PORT="5432"
DB_SIDDT_DBDRIVER="PostgreSQL"
```

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
 - FactoMineR
 - factoextra
 - stats
 - missMDA
 - corrplot
 - reshape2
 - stringr
 - clv
 - fastDummies
 - epitools
 - vegan
 - ggtext
 - car
 - purrr
 - forcats
 - janitor
 - wordcloud
 - tm
 - RColorBrewer
 




## Origine des données

Les données sont fournies par l’ALEC et l’AGEDEN, et sont de plusieurs origines :

 - Les bases de données d’instruction présentant les données de gestion de 8542 dossiers de demande de la Prime Air Bois, qu’on peut relier, grâce aux numéros de dossier, au questionnaire en amont du projet, présentant 7447 individus et 40 variables décrivant les bénéficiaires, leurs matériels, et leurs comportements quant au chauffage au bois. 
 - Environ 550 bénéficiaires ont rempli un questionnaire facultatif en aval du projet, où les questions étaient plutôt portées  sur leur satisfaction et leur nouveau mode d’utilisation de l’appareil. 

Toutes ces données ont été collectées de novembre 2015 à mai 2024, et ceci sur trois territoires : Le Pays Voironnais (CAPV), le Grésivaudan (CCLG), et Grenoble Alpes Métropole (GAM). La base de données principale à analyser est le questionnaire en amont du projet, à laquelle nous avons ajouté plusieurs variables, telles que les différentes dates de demande et de versement de la prime, le coût total TTC des travaux, le montant d'aide demandé, le statu majoré ou non de la prime, le coût de l’appareil, ou encore la commune du demandeur.

Ces données d'origines sont stockées dans le dossier "data/1_Source".

Afin de compléter ces données, nous avons pu en récupérer d’autres, notamment au niveau communal :
- Grâce à l’outil SIDDT de INRAE, nous avons pu récupérer des variables socio-démographiques, telles que le nombre de maisons, le nombre de ménage par catégorie socioprofessionnelle, la densité de population au km², ou encore la part de forêt dans la commune. 
- Grâce à l’INSEE, nous avons pu récupérer la [médiane du niveau de vie par commune](https://www.insee.fr/fr/statistiques/7756729?sommaire=7756859), et le [type de chaque commune](https://www.insee.fr/fr/statistiques/5039991?sommaire=5040030#onglet-1).
- L’observatoire de la qualité de l’air « Atmo Auvergne-Rhône-Alpes », nous a fourni des données indiquant par commune le nombre d’appareils anciens au 31 décembre 2022, donc ceux à changer, ainsi que le nombre de ces appareils changés en 2023.

Toutes ces données supplémentaires sont stockées dans "data".



### Organisation des dossiers : 

Dire comment est organisé le code, les analyses... quels sont les fichiers rmd d'analyse pertinent, où trouver les graphs.

Les scripts R de traitement rendant les données exploitables sont ceux stockés dans le dossier "scripts/R".

Les fichiers R markdown de notre analyse des données sont stockés dans le dossier "script R Gabriel". Les fichiers principaux sont :

 - "AFDM.Rmd" : Réduction de dimension et clustering au niveau des individus;
  - "ACP_communes.Rmd" : Réduction de dimension et clustering au niveau communal;
 - "tests.Rmd" : Tests statistiques inter-variables au niveau des individus;
 - "tests_communes.Rmd" : Tests statistiques inter-variables au niveau communal;
 - "analyse_graphique.Rmd" : Production de graphiques variés au niveau des individus;
 - "variables_communales.Rmd" :  Production de graphiques et cartographies au niveau communal.

Les résultats sont stockés dans différents dossiers :

 - Les fichiers HTML correspondant aux R markdown sont stockés dans le dossier "script R Gabriel";
 - Les cartographies sont stockées dans le dossier "carto";
 - Les différents graphiques sont stockés dans le dossier "data/3_Resultat/graphes";
 - Quelques fichiers Excel contenant des résultats des clustering sont stockés dans le fichier "data/3_Resultat";


L'interface shiny "explorePAB" contenue dans le dossier "shinyapp".



! Attention :  Ayant du réaliser mon rapport pour la moitié de mon stage, certains résultats et interprétations peuvent être erronés ou incomplets. 



