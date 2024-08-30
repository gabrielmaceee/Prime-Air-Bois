# Script de traitement de données de questionnaire
# Date : 2023-04-27
# Auteur : F. Bray
# Help : lucas.lopez-dovina@inrae.fr


library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(ggspatial)
library(plotly)
library(purrr)
library(forcats)
library(glue)
library(janitor)
library(wordcloud)
library(sf)
library(tm)
library(tidyr)
library(fastDummies)
library(RColorBrewer)

# wd_script <- "C:/Users/frederic.bray/Nextcloud/Stage bois bûche Lucas/Projet Bois-buche/scripts/R/"
# wd_data <- "C:/Users/frederic.bray/Nextcloud/Stage bois bûche Lucas/Projet Bois-buche/data/1_Source/"
wd_script <- "C:/Users/gmace/PAB/Projet Bois-buche/scripts/R"
wd_data <- "../../data/1_Source/"

# Load custom scripts to connect the database
setwd(wd_script)
con_encoding <- "UTF-8"
source("R_connect_siddt.R")
source("R_functions.R")

# Lets go to load the datas
setwd(wd_data)

#### Part 1 : Recodage Questionnaire

# Get questionnaire data
file <- "Questionnaire.xlsx"
data <- read_excel(file)

# Get Correspondance data
# Lucas produce this file to get variable correspondances

file_dico <- "Correspondances_codes_libellés.xlsx"
correspondances <- read_excel(file_dico)


# renommer toutes les colonnes :
names(data) <- c("N° dossier",                                                                            
"Territoire",                                                                            
"Sit pro demandeur",                                                                     
"Sit pro conjoint",                                                                      
"Age",                                                                                             
"Nb pers ménage",                                                                        
"Revenus",                                                                                     
"Nature logement",                                                                  
"Surf chauff logement",                                                                  
"Période de construction",                                                        
"Travaux iso depuis 2005",                                                               
"Classe énergétique logement",                                                   
"Occupation logement",                                                            
"Type Ancien appareil",                                                                
"Usage ancien matériel",                                                         
"Freq utilisation période chauffe",                          
"Période utilisation",                                                                   
"Année installation",                                                               
"Type combustible",                                                                   
"Type combustible autre",                                                                
"Qtté bois consommée",                                                                   
"Qtté bois unité",                                                                       
"Approvisionnement",                                                                     
"Appro autre",                                                                           
"Origine bois",                                                                          
"Stockage bois",                                                                         
"Durée séchage bois",                                                               
"Type de bois",                                                                           
"Ramonage installation",                                                                 
"Fréquence ramonage",                                                               
"Fréquence ramonage autre",                                                              
"Nouveau matériel",                                                                   
"Type combustible nouveau materiel",                                                     
"Type combustible nouveau matériel autre",                                               
"Usage nouveau matériel",                                                       
"Fréquence utilisation période de chauffe nouveau matériel",
"Motivation changement appareil",                                                        
"Motivation autre",                                                                      
"Connaissance aide",                                                                 
"Connaissance aide site internet",                                                       
"Connaissance aide autre")


# Get All Column names to be processed
column_names <- correspondances %>%
  distinct(nom_colonne) %>%
  pull


#### add_factor Function
# search in correspondances the column to  tranform into factor
# with levels values, and corresponding labels
# usage : add_factors(data,"Age")

add_factors <- function(df, column_name){
  # For one column, search all levels and labels
  the_codes <- correspondances %>%
    filter(nom_colonne == column_name)
  # Apply the corresponding factor
  df %>%
    mutate({{column_name}} := factor(df[[column_name]],
                                     levels = the_codes$code,
                                     labels = the_codes$libellé))
}

# Looping through the colnames
# wanted to make it purrr, but no way ...
# data <- walk( .x = column_names,
#               .f = add_factors,
#               data=data )

# Shit j'arrive pas à faire purrr !
# data2 <- pmap_dfc( list(data = data, column_names = column_names), add_factors(data, column_names))

# tant pis : une vieille loop...
# for (col in column_names){
#   try(data <- add_factors(data, col))
# }


# gerer les variables pouvant contenir plusieurs réponses par individus : 

data[data["Période utilisation"] == "en soirée,  lorsqu'il fait très froid", "Période utilisation"] <- "en soirée, lorsqu'il fait très froid"
binaires = c("Connaissance aide", "Motivation changement appareil", "Travaux iso depuis 2005", "Période utilisation")
modalités <-  c("« Bouche oreille »", "ALEC/AGEDEN","Autre","G.A.M", "Installateur", "Grésivaudan",
                "Voironnais","Mairie","Notaire/conformité","Pas de réponse", "Presse locale", "Site Internet (préciser)",
                "Qualité air de mon logement","Économiser du bois/énergie","Économiser de l'argent","Gagner en confort/chaleur",
                "Qualité de l'air extérieur","Autre (préciser)","Pas de réponse",
                "Aucun","Menuiseries", "Murs / façade","Ne sait pas","Pas de réponse","Plancher bas","Toiture",
                "intersaison", "soirée", "journée", "nuit", "matin"," très froid", "Pas réponse" 
)
data_binaire = data[,setdiff(names(data), binaires)]
nb <- c(0,12, 19,26,33) 
sep = c(";", ";", ", ", ", ")
for(i in 1:4){
  df_long <- data[, c("N° dossier", binaires[i])] %>%
    separate_rows(binaires[i], sep = sep[i])
  df_long <- dummy_cols(df_long, select_columns = binaires[i], remove_first_dummy = FALSE, remove_selected_columns = TRUE)
  df_binary <- df_long %>%
    group_by(`N° dossier`) %>%
    summarize(across(everything(), max))
  names = c(modalités[(nb[i]+1):nb[i+1]])
  names(df_binary) <- c("N° dossier",names)
  data_binaire <- data_binaire %>% left_join( df_binary, by = "N° dossier")
  print(names)
  cont = c()
  for(v in names){
    for(v2 in names){
      cont = c(cont, dim(df_binary[df_binary[,v] == 1 & df_binary[v2] == 1,])[1])
    }
  }
  contingence = matrix(cont, nrow = nb[i+1] - nb[i],  ncol = nb[i+1] - nb[i])
  rownames(contingence) <- names
  colnames(contingence) <- names
  seul = c()
  for(r in names){
    for(c in names){
      if(c==r){
        seul = c(seul,contingence[r,c])
        contingence[r,c] = 0
      }
    }
  }
  nom = binaires[i]
  coul = c(brewer.pal(n = nb[i+1] - nb[i], name = 'Paired'))
  dataf <- data.frame(names = names,seul = seul,coul = coul)
  if(i == 2){
    dataf$names <- factor(dataf$names, levels = c("Qualité air de mon logement","Qualité de l'air extérieur","Économiser du bois/énergie",
                                                  "Économiser de l'argent","Gagner en confort/chaleur", "Autre (préciser)","Pas de réponse"))
  }
  if(i == 4){
    dataf$names <- factor(dataf$names, levels = c("matin", "journée","soirée", "nuit", "intersaison", " très froid", "Pas réponse"))
  }
  myplot<- ggplot(dataf, aes(x = names, y = seul, fill = coul)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() + # Utiliser les couleurs spécifiées dans le vecteur 'coul'
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10)) + # Faire pivoter les étiquettes des axes
    labs(title = nom, x = "Noms", y = "Valeurs")
  ggsave(glue("../3_Resultat/graphes/Univariés/{nom}.png"), myplot, device = "png")
  
  png(glue("../3_Resultat/graphes/Univariés/{nom}_2.png"))
  barplot(contingence, col = coul , names = names,  las = 2)
  #axis(1, at = 1.2 * 1:(nb[i+1] - nb[i]) -0.5, labels=names,  las = 2)
  if(i == 3) {legend("top", legend = names, fill = coul, bg = "transparent")
  } else legend("topright", legend = names, fill = coul, bg = "transparent")
  dev.off()
}



# Export recoded survey
write_xlsx(data, "../2_Travail/export_questionnaire.xlsx")
write_xlsx(data_binaire, "../2_Travail/export_questionnaire_binaire.xlsx")

# Keep Data in a dataframe (to merge later)
data_questionnaire <- data


# Graph Output for all columns

# Get real colnames, and remove the first 3
column_names <- setdiff(names(data), c("Motivation changement appareil", "Connaissance aide"))
column_names <- column_names[-c(1)]


# Usage : get_count_graph("Sit pro demandeur")
# get_count_graph <- function(df, colname){
#   myplot <- ggplot(df,
#                    aes(x = fct_infreq(.data[[colname]]))) + 
#     geom_bar(stat = 'count') +  
#     coord_flip() + 
#     ggtitle(colname) +
#     theme(axis.title.x = element_blank(),axis.title.y = element_blank())
#   ggsave(glue("../3_Resultat/graphes/{colname}.png"), myplot, device = "png")
# }
# 
# walk(.x = column_names,
#      .f = get_count_graph,
#      df = data)# marche pas


#### changer le plot selon le type de variable :
#  "Qtté bois consommée" est quantitative mais contient des NA + pb d'unité de mesure
# data$`Qtté bois consommée` = as.numeric(data$`Qtté bois consommée`) # rendre numérique 

get_graph <- function(df, colname){
  
  if(colname == "Surf chauff logement"){
    png(glue("../3_Resultat/graphes/Univariés/{colname}.png"))   # Open a png file
    plot(density(df %>% pull(colname)), main = glue("Densité de : {colname}"))  # 2. Create a plot
    dev.off() # Close the png file
  }
  else if(colname %in% c("Appro autre", "Connaissance aide autre","Connaissance aide site internet", "Fréquence ramonage autre", "Motivation autre", "Fréquence ramonage autre", "Type combustible autre")){
    png(glue("../3_Resultat/graphes/Univariés/{colname}.png"))   # Open a png file
    wordcloud(df %>% pull(colname), max.words = 30, colors = brewer.pal(8, "Dark2"),  rot.per=0, scale = c(4,0.5), min.freq = 5)
    dev.off()
  }
  else{
    #barplot(table(df[, colname]), main = glue("Répartition des modalités de : {colname}"))
    myplot <- ggplot(df, aes(x = df %>% pull(colname), main = glue("Répartition des modalités de : {colname}"))) + 
      geom_bar(stat = 'count') +  
      coord_flip() + 
      ggtitle(colname) +
      theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    ggsave(glue("../3_Resultat/graphes/Univariés/{colname}.png"), myplot, device = "png")
  }
}

for(col in column_names){
  get_graph(data, col)
} # marche



#### Part 2 : Geocodage Dossiers

#### get_data_from_file function
# Read a file
# Clean names
# Get just 2 columns :numero_dossier and city name

get_data_from_file <- function(filename){
  print(filename)
  sheet <- "BD"
  data <- read_excel(filename, sheet)
  if(filename == "Traitement données FABV.xlsx"){
    names(data)[2] <- "numero_dossier"
    names(data)[29] <- "adresse_ville"
  }
  if(filename == "Traitement données FABVR.xlsx"){
    names(data)[2] <- "numero_dossier"
    names(data)[27] <- "adresse_ville"
  }

  
  extrait <- data %>%
    clean_names() %>%
    select(numero_dossier, adresse_ville)
  return(extrait)

}
# test <- read_excel("Traitement données FABV.xlsx", sheet = "BD")
# test$...2
# Get filenames to merge
files <- list.files(pattern = "Traitement")

# Get ville and numero dossier from the 3 files
data <- map_df(files, get_data_from_file)

# Clean adresse_ville
# Sould prepare a function that read in a dict, but... no
data <- data %>%
  mutate(adresse_ville = str_replace_all(adresse_ville, "_", "-")) %>%
  mutate(adresse_ville = str_replace_all(adresse_ville, " ", "-")) %>%
  mutate(adresse_ville = str_replace_all(adresse_ville, "’", "'")) %>%
  mutate(adresse_ville = toupper(adresse_ville)) %>%
  mutate(adresse_ville = str_replace_all(adresse_ville, "SAINT", "ST")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "ALLEVARD-LES-BAINS", "CRETS-EN-BELLEDONNE")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LE-PLATEAU-DES-PETITES-ROCHES", "PLATEAU-DES-PETITES-ROCHES")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "BRIGNOUD", "VILLARD-BONNOT")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LE-FONTANIL-CORNILLON", "FONTANIL-CORNILLON")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LE-GA", "LE-GUA")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "VF", "VIF")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "MURAINETTE", "MURIANETTE")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "SAPPEY-EN-CHARTREUSE", "LE-SAPPEY-EN-CHARTREUSE")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "VEUREY-VOIROIZE", "VEUREY-VOROIZE")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "VEUREY-VOROISE", "VEUREY-VOROIZE")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "SEYSINNET-PARISET", "SEYSSINET-PARISET")) %>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LANCEY", "LA-COMBE-DE-LANCEY"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "STE-AGNÈS", "STE-AGNES"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LA-BUISSIÈRE", "LA-BUISSIERE"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LA-FLACHÈRE", "LA-FLACHERE"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "HURTIÈRES", "HURTIERES"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "CRÊTS-EN-BELLEDONNE", "CRETS-EN-BELLEDONNE"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LE-CHAMP-PRÈS-FROGES", "LE-CHAMP-PRES-FROGES"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "DOMÈNE", "DOMENE"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "LA-FERRIÈRE", "LA-FERRIERE"))%>%
  mutate(adresse_ville = replace(adresse_ville, adresse_ville == "BRIGNOUD-->VILLARD-BONNOT", "VILLARD-BONNOT"))


# Get official data from our database
sql <- "SELECT depcom AS insee_com, replace(replace(nom_com_m,'SAINT','ST'),' ','-') AS nom_commune FROM bd_admin_express.geo_commune_2017
        WHERE insee_dep = '38'
        UNION
        SELECT insee_com, replace(replace(nom_m,'SAINT','ST'),' ','-') AS nom_commune FROM bd_admin_express.geo_commune_2022
        WHERE insee_dep = '38'
		    ORDER BY 1"
communes <- dbGetQuery(con, sql)

# What are the bad Town names ?
notJoined <- data %>%
  left_join(as_tibble(communes), by=c("adresse_ville" = "nom_commune")) %>%
  select(adresse_ville, insee_com) %>%
  filter(is.na(insee_com)) %>%
  distinct() %>%
  select(adresse_ville)

data <- data %>%
  left_join(as_tibble(communes), by=c("adresse_ville" = "nom_commune"))

# Quantify the bad joined data
issues <- data %>% filter(is.na(insee_com))  

# Export geocoded data 
write_xlsx(data, "../2_Travail/data_geocodes.xlsx") 


#### Mapping

# Prepare Map with new inseecom
count <- data %>%
  group_by(insee_com) %>%
  count()

# Get polygons communes data
sql <- "SELECT insee_com, nom_m AS nom_commune, the_geom 
        FROM bd_admin_express.geo_commune_2022
        WHERE insee_dep = '38'
		    ORDER BY 1"
layer_communes <- st_read(con, query = sql)

shp <- st_read(dsn = "../../data/EPCI 2024_region.shp")
shp = shp[c(27, 175, 929),]
shp$ sociale <- c("CCLG", "GAM", "CAPV")

# Join feature and counted data
layer_communes %>%
  left_join(count) %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "white",
                      high = "blue", 
                      na.value = "grey80")+
  #scale_fill_brewer(palette = "OrRd", na.value = "grey90") +
  labs(fill = "Nombre de demandes de prime") +
  annotation_scale(location = "br", bar_cols = "black", width_hint = 1/12) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm")) +
  theme_void() + 
  geom_sf(data = shp, aes(fill = Z_MOYEN), color = c("red", "blue", "green"), lwd = 1.2, fill = NA)
# Save map to png
ggsave("../../carto/carte_count_demande.png", width = 20, height = 20, units = "cm")


# Save layer to shapefile  
layer_communes %>%
  left_join(count) %>%  
  st_write("../2_Travail/layer_count.shp",
           append=F)

# Merge Questionnaire with geocoded data
data_final <- data_questionnaire %>%
  left_join(data, by = c("N° dossier"="numero_dossier"))

# Export result
write_xlsx(data_final, "../3_Resultat/data_questionnaire_geocode.xlsx")

# carte interactive :


to_plot <- layer_communes %>% left_join(count)
 
# Créer un objet ggplot
plot <- ggplot(to_plot) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey80") +
  geom_sf_text(aes(label = nom_commune, label1 = insee_com, label2 = n), size = 1, color = "transparent", na.rm = FALSE) +
  labs(fill = "Nombre de demandes de prime") +
  theme_void() + 
  geom_sf(data = shp, aes(fill = Z_MOYEN), color = c("red", "blue", "green"), lwd = 1.2, fill = NA)

# Convertir l'objet ggplot en plotly
plotly_plot <- ggplotly(plot, tooltip = c("label", "label1","label2"))

# Afficher la carte interactive
plotly_plot

htmlwidgets::saveWidget( widget = plotly_plot, file =  "../../carto/carte_count_demande_interactive.html")


#### Interface shiny :

# library(shiny)
# 
# # Créer l'interface utilisateur Shiny
# ui <- fluidPage(
#   titlePanel("Carte interactive"),
#   selectInput("info", "Choisir l'information à afficher au survol:",
#               choices = c("Nom de la commune" = "nom_commune",
#                           "Code INSEE" = "insee_com",
#                           "Nombre d'éléments" = "n")),
#   plotlyOutput("plot")
# )
# 
# # Créer le serveur Shiny
# server <- function(input, output) {
#   
#   # Joindre les données des communes avec les données du compte
#   to_plot <- left_join(layer_communes, count)
#   
#    # Convertir l'objet ggplot en plotly
#   output$plot <- renderPlotly({
# 
#   # Créer un objet ggplot
#   plot <- ggplot(to_plot) +
#     geom_sf(aes(fill = n)) +
#     scale_fill_gradient(low = "white", high = "blue", na.value = "grey80") +
#     geom_sf_text(aes_string(label = input$info), size = 1, color = "transparent", na.rm = FALSE) +
#     theme_void()
#   
#   ggplotly(plot, tooltip = c("label", "label1", "label2"))
#   })
# }
# 
# # Lancer l'application Shiny
# carte_interactive <- shinyApp(ui = ui, server = server)
# carte_interactive
# saveRDS(carte_interactive, "../../carto/app.R")



### Partie 3 : ajout de variables : 

# ajout de la consommation en kwh

data_qt <- read_excel("../3_Resultat/data_questionnaire_geocode.xlsx")

transfo <- function(qt, unite, type){
  if( is.na(qt) || is.na(unite) || is.na(type)) return(NA)
  qt = as.numeric(qt)
  if(type == "Bûche"){
    if(unite %in% c("Stères", "stère", "stères", "steres", "steres", "STERES", "Stère", "STERE", "Steres","stere", "ST", "M3", "m3")){
      return(qt * 1500)
    } # à verif 
  }
  else if(type == "Bois de récup"){
    if(unite %in% c("stere", "stère", "steres", "STERES", "stères", "Stères", "m3")) return(qt * 1500)
  }
  else if(type == "Granulés"){
    if(unite %in% c("tonne", "tonnes")) return( qt * 4800)
    else if(unite == "kg") return( qt * 4.8)
  }
  
  return(NA)
}

kwh = c()

for(i in 1:dim(data_qt)[1]){
  kwh = c(kwh,transfo(data_qt[i,"Qtté bois consommée"], data_qt[i, "Qtté bois unité"], data_qt[i, "Type combustible"]))
}
length(na.omit(kwh))


bb <- data_qt %>% filter(`Type combustible` == "Bûche")
sort(table(bb$`Qtté bois unité`))

bb <- data_qt %>% filter(`Type combustible` == "Bois de récup")
table(bb$`Qtté bois unité`)

bb <- data_qt %>% filter(`Type combustible` == "Granulés")
table(bb$`Qtté bois unité`)

data_qt <- data.frame(data_qt, kwh)
write_xlsx(data_qt, "../3_Resultat/data_questionnaire_kwh.xlsx")
write_xlsx(data_qt, "../3_Resultat/data_questionnaire_complet.xlsx")

png(glue("../3_Resultat/graphes/Univariés/kwh.png"))   # Open a png file
plot(density(na.omit(data_qt[data_qt$kwh < 50000,]$kwh)))
dev.off() # Close the png file



# Ajout du coût ttc de l'installation, du nouvel appareil, et du montant de l'aide :

data_final <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
FABG <- read_excel("Traitement données FABG.xlsx", sheet = "BD", col_names = TRUE)
FABV <- read_excel("Traitement données FABV.xlsx", sheet = "BD", col_names = TRUE)
FABVR <- read_excel("Traitement données FABVR.xlsx", sheet = "BD", col_names = TRUE)
FABM <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[1:2320, -1]
FABM2 <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[2321:3961,-1]
names(FABM)[3] <- "majoration"
names(FABV) <- FABV[3,]
FABV <- FABV[-c(1:4),]
names(FABVR) <- FABVR[2,]
FABVR <- FABVR[-c(1:3),]
names(FABG) <- FABG[1,]
FABG <- FABG[-1,]
names(FABV)[c(3, 66)] <- c("montant_aide", "cout_total_TTC")
names(FABVR)[c(3,54, 60, 67)] <- c("montant_aide","cout_appareil", "cout_total_TTC", "majoration")
FABM2 <- FABM2[,-c(5,6,9)]
names(FABM2) <- names(FABM)[1:82]
maj = c()
for(i in FABV$montant_aide){
  if(i == 400) maj = c(maj, "NM")
  else if(i == 800) maj = c(maj, "M")
  else maj = c(maj, NA)
}
FABV$majoration = maj
FABG <- FABG[,c("numero_dossier","cout_total TTC", "montant d'aide demandée", "Prime Majoré", "cout_appareil")]
FABM <- FABM[,c("numero_dossier","cout_total TTC", "Montant de la Prime", "majoration", "cout_appareil")]
FABM2 <- FABM2[,c("numero_dossier","cout_total TTC", "Montant de la Prime", "majoration", "cout_appareil")]
FABV <- FABV[,c("numero_dossier","cout_total_TTC", "montant_aide", "majoration", "cout_appareil")]
FABVR <- FABVR[,c("numero_dossier","cout_total_TTC","montant_aide", "majoration", "cout_appareil")]
names(FABG)[c(2,3, 4)] <- c("cout_total_TTC", "montant_aide", "majoration")
names(FABM)[c(2,3)] <- c("cout_total_TTC", "montant_aide")
names(FABM2)[c(2,3)] <- c("cout_total_TTC", "montant_aide")
FABG$majoration <- as.factor(FABG$majoration)
levels(FABG$majoration) <- c("-", "M", "man", "NC", "NM")
FABVR$majoration <- as.factor(FABVR$majoration)
levels(FABVR$majoration) <- c("M", "NC", "NM")

data <- rbind(FABV, FABVR, FABG, FABM, FABM2)
data[,"cout_total_TTC"] <- as.numeric(data$`cout_total_TTC`)
data[,"montant_aide"] <- as.numeric(data$montant_aide)
data$cout_appareil <- as.numeric(data$cout_appareil)
# ajouter catégories MPR : pas assez de données
# FABG <- FABG[,c("numero_dossier","cout_total HT", "Très modestes / modestes / intermédiaire / supérieurs")]
# FABM <- FABM[,c("numero_dossier","cout_total HT", "PROFIL MPR")]
# FABV[,25]
# FABV <- FABV[,c("numero_dossier","cout_total HT", )]
# FABVR <- FABVR[,c("numero_dossier","cout total HT", "Catégorie MPR")]
data[data$majoration %in% c("-", "0", "45237", "M3543", "man", "NC") ,]$majoration <- NA
data[data$majoration %in% c("nm", "NM ", "NM1600"),]$majoration <- "NM"
data[8095, "majoration"] <- "NM"

data <- left_join(data_final, data, by= c("N..dossier" = "numero_dossier")) # si besoin : data_final[,-c(45,46)]
png(glue("../3_Resultat/graphes/Univariés/cout_total_TTC.png"))   # Open a png file
plot(density(na.omit(data[data$cout_total_TTC< 20000,]$cout_total_TTC)))
dev.off() # Close the png file

png(glue("../3_Resultat/graphes/Univariés/cout_appareil.png"))   # Open a png file
plot(density(na.omit(data[data$cout_appareil <15000,]$cout_appareil)))
dev.off() # Close the png file

png(glue("../3_Resultat/graphes/Univariés/montant de l'aide.png"))   # Open a png file
plot(density(na.omit(data$montant_aide)))
dev.off() # Close the png file
myplot <- ggplot(data, aes(x = majoration, main = glue("Répartition des modalités de : majoration"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("majoration") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())
ggsave(glue("../3_Resultat/graphes/Univariés/majoration.png"), myplot, device = "png")

write_xlsx(data, "../3_Resultat/data_questionnaire_complet.xlsx")

# equivalent stères : 
data_final <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
data_final["équivalent_steres"] <- data_final$kwh / 1500

write_xlsx(data_final, "../3_Resultat/data_questionnaire_complet.xlsx")

png(glue("../3_Resultat/graphes/Univariés/équivalent_stères.png"))   # Open a png file
plot(density(na.omit(data_final[data_final$équivalent_steres < 30,]$équivalent_steres)))
dev.off() # Close the png file



### Partie 4 : Plot croisés

data <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")[, -c(1)]

quanti = c("Surf.chauff.logement", "Nb.pers.ménage", "cout_total_TTC", "montant_aide", "équivalent_steres","cout_appareil")
nq = c("kwh","dossier_reçu","Période.utilisation","Appro.autre","Connaissance.aide","Type.combustible.nouveau.matériel.autre", "Connaissance.aide.autre","Connaissance.aide.site.internet", "Fréquence.ramonage.autre", "Motivation.autre", "Motivation.changement.appareil", "Fréquence.ramonage.autre", "Type.combustible.autre", "adresse_ville", "insee_com","Qtté.bois.consommée", "Qtté.bois.unité", quanti)
quali = setdiff(names(data), nq)

get_plot_quanti_quali <- function(df, colname){
    for(q in quanti){
      png(glue("../3_Resultat/graphes/croisés/Quanti-quali/{q}_{colname}_boxplot.png"))
      boxplot((df %>% pull(q))~(df %>% pull(colname)), main = glue("{q} en fonction de {colname}"))
      dev.off()
      p <- ggplot(data=data, aes_string(x=q)) + geom_density(aes( fill=factor(factor(data %>% pull(colname)))), alpha = 0.7)
      ggsave(glue("../3_Resultat/graphes/croisés/Quanti-quali/{q}_{colname}_densité.png"), width = 20, height = 20, units = "cm")
    }
}


for(col in quali){
  get_plot_quanti_quali(data, col)
}
png(glue("../3_Resultat/graphes/croisés/Quanti-quanti/graph_corrélations.png"))
pairs(data[,quanti], cex.labels = 1.6)
dev.off()


### !!!!!!!!!!!! Très long
get_plot_quali_quali <- function(df, colname){
  for(q in quali){
    if(q != colname){
      p <- ggplot(data, aes_string(x = q, fill = colname)) + geom_bar(position = "stack")
      ggsave(glue("../3_Resultat/graphes/croisés/Quali-quali/{q}_{colname}_barplot.png"), width = 20, height = 20, units = "cm")
    }
  }
}
for(col in quali){
  get_plot_quali_quali(data, col)
}


### Partie 5 : Ajout des dates

data_final <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
FABG <- read_excel("Traitement données FABG.xlsx", sheet = "BD", col_names = TRUE)
FABV <- read_excel("Traitement données FABV.xlsx", sheet = "BD", col_names = TRUE)
FABVR <- read_excel("Traitement données FABVR.xlsx", sheet = "BD", col_names = TRUE)
FABM <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[1:2320,-1]
FABM2 <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[2321:3961,-1]
names(FABV) <- FABV[3,]
FABV <- FABV[-c(1:4),]
names(FABVR) <- FABVR[2,]
FABVR <- FABVR[-c(1:3),]
names(FABG) <- FABG[1,]
FABG <- FABG[-1,]
names(FABG)[4] <- "dossier_reçu"
names(FABV)[4] <- "dossier_reçu"
names(FABVR)[4] <- "dossier_reçu"
names(FABM)[6] <- "dossier_reçu"
names(FABM2)[8] <- "dossier_reçu"
#names(FABG)[21]<- "transfert_facture"

FABG <- FABG[,c("numero_dossier","dossier_reçu")]
FABM <- FABM[,c("numero_dossier","dossier_reçu")]
FABM2 <- FABM2[,c("numero_dossier","dossier_reçu")]
FABV <- FABV[,c("numero_dossier","dossier_reçu")]
FABVR <- FABVR[,c("numero_dossier","dossier_reçu")]


FABG[,"dossier_reçu"] <- openxlsx::convertToDate(FABG  %>% pull("dossier_reçu"))
FABM2[,"dossier_reçu"] <- openxlsx::convertToDate(FABM2  %>% pull("dossier_reçu"))
FABV[,"dossier_reçu"] <- openxlsx::convertToDate(FABV  %>% pull("dossier_reçu"))
FABVR[,"dossier_reçu"] <- openxlsx::convertToDate(FABVR  %>% pull("dossier_reçu"))


test <- rbind(FABG, FABV, FABVR, FABM, FABM2)


exp <- left_join(data_final, test, by= c("N..dossier" = "numero_dossier"))


# Export result
write_xlsx(exp, "../3_Resultat/data_questionnaire_complet.xlsx")



library(zoo)

####### Générale
# for()
# par jour :
res <- sort(test$dossier_reçu)[-1]
# res <- sort(exp$dossier_reçu)[-1]
length(table(res))
temp.zoo <- zoo(x = table(res), order.by = sort(unique(res)))
temp.sum <- zoo(x = cumsum(table(res)), order.by = sort(unique(res)))

plot(temp.zoo, type = "l")
autoplot.zoo(temp.zoo)
autoplot(temp.sum)

png(glue("../3_resultat/graphes/Analyse temporelle/Générale courbe cumulative.png"))
autoplot(temp.sum)
dev.off()

# par mois :
nb_mois <- format(res,"%m")
barplot(table(nb_mois))

png(glue("../3_Resultat/graphes/Analyse temporelle/Générale par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an : enlever 2024
nb_an <- format(res, "%Y")
barplot(table(nb_an))
plot(table(sort(nb_an))[-c(1,10)], type = "l")
png(glue("../3_Resultat/graphes/Analyse temporelle/Générale par an.png"))
plot(table(sort(nb_an))[-c(1,10)], type = "l")
dev.off()

# par mois et an :
nb_mois_an <- sort(format((res), "%Y/%m"))
plot(table(nb_mois_an), type = "l")
png(glue("../3_Resultat/graphes/Analyse temporelle/Générale par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2015, 11), end = c(2024,5), freq = 12)
plot.ts(nb_doss)
plot(decompose(nb_doss))
acf(nb_doss) # effets des mois sur lui,meme et celui d'apres, et lui meme sur un an -> par années
pacf(nb_doss) # effet des mois sur eux memes

png(glue("../3_Resultat/graphes/Analyse temporelle/Générale Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()



###### Grésivaudan : 

# res_G <- exp[exp$Territoire == "CCLG",] # pour comparer tt/ que retenu, ou bien prendre que FABG en créant la variable retenu / pas retenu 6> + de dossiers
res_G <- sort(FABG$dossier_reçu)[-1]
temp.sum <- zoo(x = cumsum(table(res_G)), order.by = sort(unique(res_G)))
png(glue("../3_resultat/graphes/Analyse temporelle/Grésivaudan courbe cumulative.png"))
autoplot(temp.sum)
dev.off()


# par mois :
nb_mois <- format(res_G,"%m")
png(glue("../3_resultat/graphes/Analyse temporelle/Grésivaudan par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an : enlever 2024
nb_an <- format(res_G, "%Y")
png(glue("../3_resultat/graphes/Analyse temporelle/Grésivaudan par an.png"))
plot(table(sort(nb_an))[-c(1,10)], type = "l")
dev.off()

# par mois et an :
nb_mois_an <- sort(format((res_G), "%Y/%m"))
png(glue("../3_resultat/graphes/Analyse temporelle/Grésivaudan par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2015, 11), end = c(2024,5), freq = 12)
png(glue("../3_Resultat/graphes/Analyse temporelle/Grésivaudan Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()





####### Voironnais : 

res_V <-sort(c(FABV$dossier_reçu, FABVR$dossier_reçu))
 
temp.sum <- zoo(x = cumsum(table(res_V)), order.by = sort(unique(res_V)))
png(glue("../3_resultat/graphes/Analyse temporelle/Voironnais courbe cumulative.png"))
autoplot(temp.sum)
dev.off()

# par mois :
nb_mois <- format(res_V,"%m")
png(glue("../3_Resultat/graphes/Analyse temporelle/Voironnais par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an : enlever 2024
nb_an <- format(res_V, "%Y")
png(glue("../3_Resultat/graphes/Analyse temporelle/Voironnais par an.png"))
plot(table(sort(nb_an))[-9], type = "l")
dev.off()

# par mois et an : enlever le dernier mois ?
nb_mois_an <- sort(format((res_V), "%Y/%m"))
png(glue("../3_Resultat/graphes/Analyse temporelle/Voironnais par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2016, 01), end = c(2024,03), freq = 12)
png(glue("../3_Resultat/graphes/Analyse temporelle/Voironnais Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()


####### Grenoble métropole

res_M <- sort(c(FABM$dossier_reçu, FABM2$dossier_reçu))

temp.sum <- zoo(x = cumsum(table(res_M)), order.by = sort(unique(res_M)))
png(glue("../3_resultat/graphes/Analyse temporelle/Grenoble courbe cumulative.png"))
autoplot(temp.sum)
dev.off()

# par mois :
nb_mois <- format(res_M,"%m")
png(glue("../3_Resultat/graphes/Analyse temporelle/Grenoble par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an :
nb_an <- format(res_M, "%Y")
png(glue("../3_Resultat/graphes/Analyse temporelle/Grenoble par an.png"))
plot(table(sort(nb_an))[-c(1,10)], type = "l")
dev.off()

# par mois et an :
nb_mois_an <- sort(format((res_M), "%Y/%m"))
png(glue("../3_Resultat/graphes/Analyse temporelle/Grenoble par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2015, 11), end = c(2024,5), freq = 12)
png(glue("../3_Resultat/graphes/Analyse temporelle/Grenoble Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()





### Partie 5 bis : Ajout des dates de fin de travaux (transfert facture)

data_final <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
FABG <- read_excel("Traitement données FABG.xlsx", sheet = "BD", col_names = TRUE)
FABV <- read_excel("Traitement données FABV.xlsx", sheet = "BD", col_names = TRUE)
FABVR <- read_excel("Traitement données FABVR.xlsx", sheet = "BD", col_names = TRUE)
FABM <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[1:2319,-1]
FABM2 <- read.csv("Traitement données FABM.csv", sep = ";", header = TRUE)[2320:3961,]
names(FABV) <- FABV[3,]
FABV <- FABV[-c(1:4),]
names(FABVR) <- FABVR[2,]
FABVR <- FABVR[-c(1:3),]
names(FABG) <- FABG[1,]
FABG <- FABG[-1,]
names(FABG)[16] <- "transfert_facture"
names(FABV)[16] <- "transfert_facture"
names(FABVR)[15] <- "transfert_facture"
names(FABM)[25] <- "transfert_facture"
names(FABM2)[28] <- "transfert_facture"


FABG <- FABG[,c("numero_dossier","transfert_facture")]
FABM <- FABM[,c("numero_dossier","transfert_facture")]
FABM2 <- FABM2[,c("numero_dossier","transfert_facture")]
FABV <- FABV[,c("numero_dossier","transfert_facture")]
FABVR <- FABVR[,c("numero_dossier","transfert_facture")]

library(chron)
FABG[,"transfert_facture"] <- openxlsx::convertToDate(FABG  %>% pull("transfert_facture"))
FABM2[,"transfert_facture"] <- as.Date(chron(FABM2$transfert_facture, format = "d/m/y"))
FABV[,"transfert_facture"] <- openxlsx::convertToDate(FABV  %>% pull("transfert_facture"))
FABVR[,"transfert_facture"] <- openxlsx::convertToDate(FABVR  %>% pull("transfert_facture"))


test <- rbind(FABG, FABV, FABVR, FABM, FABM2)


exp <- left_join(data_final, test, by= c("N..dossier" = "numero_dossier"))


# Export result
write_xlsx(exp, "../3_Resultat/data_questionnaire_complet.xlsx")



library(zoo)

####### Générale
# for()
# par jour :
res <- sort(test$transfert_facture)[-c(1:4, 6957)]
# res <- sort(exp$transfert_facture)[-1]
length(table(res))
temp.zoo <- zoo(x = table(res), order.by = sort(unique(res)))
temp.sum <- zoo(x = cumsum(table(res)), order.by = sort(unique(res)))

plot(temp.zoo, type = "l")
autoplot.zoo(temp.zoo)
autoplot(temp.sum)

png(glue("../3_resultat/graphes/Analyse temporelle facture/Générale courbe cumulative.png"))
autoplot(temp.sum)
dev.off()

# par mois :
nb_mois <- format(res,"%m")
barplot(table(nb_mois))

png(glue("../3_Resultat/graphes/Analyse temporelle facture/Générale par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an : enlever 2024
nb_an <- format(res, "%Y")
barplot(table(nb_an))
plot(table(sort(nb_an))[-c(9)], type = "l")
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Générale par an.png"))
plot(table(sort(nb_an))[-c(9)], type = "l")
dev.off()

# par mois et an :
nb_mois_an <- sort(format((res), "%Y/%m"))
plot(table(nb_mois_an), type = "l")
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Générale par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2016, 1), end = c(2024,5), freq = 12)
plot.ts(nb_doss)
plot(decompose(nb_doss))
acf(nb_doss) # effets des mois sur lui,meme et celui d'apres
pacf(nb_doss) # effet des mois sur eux memes

png(glue("../3_Resultat/graphes/Analyse temporelle facture/Générale Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()



###### Grésivaudan : 

# res_G <- exp[exp$Territoire == "CCLG",] # pour comparer tt/ que retenu, ou bien prendre que FABG en créant la variable retenu / pas retenu 6> + de dossiers
res_G <- sort(FABG$transfert_facture)
temp.sum <- zoo(x = cumsum(table(res_G)), order.by = sort(unique(res_G)))
png(glue("../3_resultat/graphes/Analyse temporelle facture/Grésivaudan courbe cumulative.png"))
autoplot(temp.sum)
dev.off()


# par mois :
nb_mois <- format(res_G,"%m")
png(glue("../3_resultat/graphes/Analyse temporelle facture/Grésivaudan par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an : enlever 2024
nb_an <- format(res_G, "%Y")
png(glue("../3_resultat/graphes/Analyse temporelle facture/Grésivaudan par an.png"))
plot(table(sort(nb_an))[-c(9)], type = "l")
dev.off()

# par mois et an :
nb_mois_an <- sort(format((res_G), "%Y/%m"))
png(glue("../3_resultat/graphes/Analyse temporelle facture/Grésivaudan par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2016, 4), end = c(2024,5), freq = 12)
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Grésivaudan Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()





####### Voironnais : 

res_V <-sort(c(FABV$transfert_facture, FABVR$transfert_facture))

temp.sum <- zoo(x = cumsum(table(res_V)), order.by = sort(unique(res_V)))
png(glue("../3_resultat/graphes/Analyse temporelle facture/Voironnais courbe cumulative.png"))
autoplot(temp.sum)
dev.off()

# par mois :
nb_mois <- format(res_V,"%m")
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Voironnais par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an : enlever 2024
nb_an <- format(res_V, "%Y")
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Voironnais par an.png"))
plot(table(sort(nb_an))[-9], type = "l")
dev.off()

# par mois et an : enlever le dernier mois ?
nb_mois_an <- sort(format((res_V), "%Y/%m"))
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Voironnais par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2016, 03), end = c(2024,05), freq = 12)
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Voironnais Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()


####### Grenoble métropole

res_M <- sort(c(FABM$transfert_facture, FABM2$transfert_facture))[-c(1:4, 3217)]

temp.sum <- zoo(x = cumsum(table(res_M)), order.by = sort(unique(res_M)))
png(glue("../3_resultat/graphes/Analyse temporelle facture/Grenoble courbe cumulative.png"))
autoplot(temp.sum)
dev.off()

# par mois :
nb_mois <- format(res_M,"%m")
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Grenoble par mois.png"))
barplot(table(nb_mois))
dev.off()

# par an :
nb_an <- format(res_M, "%Y")
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Grenoble par an.png"))
plot(table(sort(nb_an))[-9], type = "l")
dev.off()

# par mois et an :
nb_mois_an <- sort(format((res_M), "%Y/%m"))
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Grenoble par mois et an.png"))
plot(table(nb_mois_an), type = "l")
dev.off()

nb_doss <- ts(table(nb_mois_an), start = c(2016, 1), end = c(2024,5), freq = 12)
png(glue("../3_Resultat/graphes/Analyse temporelle facture/Grenoble Analyse nb dossiers.png"))
plot(decompose(nb_doss))
dev.off()











### Partie 6 : Refus du dossier

data_final <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
FABG <- read_excel("Traitement données FABG.xlsx", sheet = "BD", col_names = TRUE)
FABV <- read_excel("Traitement données FABV.xlsx", sheet = "BD", col_names = TRUE)
FABVR <- read_excel("Traitement données FABVR.xlsx", sheet = "BD", col_names = TRUE)
FABM <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[1:2320,-1]
FABM2 <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[2321:3961,-1]
names(FABV) <- FABV[3,]
FABV <- FABV[-c(1:4),]
names(FABVR) <- FABVR[2,]
FABVR <- FABVR[-c(1:3),]
names(FABG) <- FABG[1,]
FABG <- FABG[-1,]
names(FABG)[19] <- "Date_refus"
names(FABV)[19] <- "Date_refus"
names(FABVR)[17] <- "Date_refus"
names(FABM)[26] <- "Date_refus"
names(FABM2)[29] <- "Date_refus"

FABG <- FABG[,c("numero_dossier","Date_refus")]
FABM <- FABM[,c("numero_dossier","Date_refus")]
FABM2 <- FABM2[,c("numero_dossier","Date_refus")]
FABV <- FABV[,c("numero_dossier","Date_refus")]
FABVR <- FABVR[,c("numero_dossier","Date_refus")]


FABG[,"Date_refus"] <- openxlsx::convertToDate(FABG  %>% pull("Date_refus"))
FABM2[,"Date_refus"] <- openxlsx::convertToDate(FABM2  %>% pull("Date_refus"))
FABV[,"Date_refus"] <- openxlsx::convertToDate(FABV  %>% pull("Date_refus"))
FABVR[,"Date_refus"] <- openxlsx::convertToDate(FABVR  %>% pull("Date_refus"))

test <- rbind(FABG, FABV, FABVR, FABM, FABM2)
test$refusé = !(is.na(test$Date_refus))

exp <- left_join(data_final, test, by= c("N..dossier" = "numero_dossier"))


# Export result
write_xlsx(exp, "../3_Resultat/data_questionnaire_complet.xlsx")




### Partie 6 bis : Validation du dossier

data_final <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
FABG <- read_excel("Traitement données FABG.xlsx", sheet = "BD", col_names = TRUE)
FABV <- read_excel("Traitement données FABV.xlsx", sheet = "BD", col_names = TRUE)
FABVR <- read_excel("Traitement données FABVR.xlsx", sheet = "BD", col_names = TRUE)
FABM <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[1:2320,-1]
FABM2 <- read_excel("Traitement données FABM.xlsx", sheet = "BD", col_names = TRUE)[2321:3961,-1]
names(FABV) <- FABV[3,]
FABV <- FABV[-c(1:4),]
names(FABVR) <- FABVR[2,]
FABVR <- FABVR[-c(1:3),]
names(FABG) <- FABG[1,]
FABG <- FABG[-1,]
names(FABG)[8] <- "Date_dossier_complet"
names(FABV)[8] <- "Date_dossier_complet"
names(FABVR)[8] <- "Date_dossier_complet"
names(FABM)[11] <- "Date_dossier_complet"
names(FABM2)[14] <- "Date_dossier_complet"

FABG <- FABG[,c("numero_dossier","Date_dossier_complet")]
FABM <- FABM[,c("numero_dossier","Date_dossier_complet")]
FABM2 <- FABM2[,c("numero_dossier","Date_dossier_complet")]
FABV <- FABV[,c("numero_dossier","Date_dossier_complet")]
FABVR <- FABVR[,c("numero_dossier","Date_dossier_complet")]


FABG[,"Date_dossier_complet"] <- openxlsx::convertToDate(FABG  %>% pull("Date_dossier_complet"))
FABM[,"Date_dossier_complet"] <- openxlsx::convertToDate(FABM  %>% pull("Date_dossier_complet"))
FABM2[,"Date_dossier_complet"] <- openxlsx::convertToDate(FABM2  %>% pull("Date_dossier_complet"))
FABV[,"Date_dossier_complet"] <- openxlsx::convertToDate(FABV  %>% pull("Date_dossier_complet"))
FABVR[,"Date_dossier_complet"] <- openxlsx::convertToDate(FABVR  %>% pull("Date_dossier_complet"))

test <- rbind(FABG, FABV, FABVR, FABM, FABM2)

exp <- left_join(data_final, test, by= c("N..dossier" = "numero_dossier"))


# Export result
write_xlsx(exp, "../3_Resultat/data_questionnaire_complet.xlsx")



#### Partie 7 : data shiny App :

# Faire les clustering individus et communes avant

# Récuperer et trier les données que l'on veut exploiter dans l'appli R shiny
data <- read_excel("../3_Resultat/data_questionnaire_complet.xlsx")
data <- data[, setdiff(names(data), c("Travaux.iso.depuis.2005", "Période.utilisation", "Connaissance.aide", "Motivation.changement.appareil",
                                      "Type.combustible.autre", "Qtté.bois.consommée", "Qtté.bois.unité", "Appro.autre", "Fréquence.ramonage.autre",
                                      "Type.combustible.nouveau.matériel.autre", "Motivation.autre", "Connaissance.aide.site.internet",
                                      "Connaissance.aide.autre"))]
data <- data[-6774,]
data_binaire <- read_excel("../2_Travail/export_questionnaire_binaire.xlsx")
data_binaire <- data_binaire[, c(1,38:70)]

data_shiny <- cbind(data, data_binaire)
which(data_shiny$N..dossier != data_shiny$`N° dossier`)
data_shiny <- data_shiny[,setdiff(names(data_shiny), "N° dossier" )]

# Export result
write_xlsx(data_shiny, "../3_Resultat/data_shinyt.xlsx")
write_xlsx(data_shiny, "../../shinyapp/WWW/data/data.xlsx")
