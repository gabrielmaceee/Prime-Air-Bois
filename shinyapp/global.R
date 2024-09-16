library(shiny)
library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(gridExtra)
library(sf)
library(ggspatial)
library(plotly)
library(mapview)
library(leafpop)
library(leaflet)
library(grid)
library(glue)
library(ggmosaic)
library(epitools)


# T1<-Sys.time()
# data <- read_excel("www/data/data.xlsx")
# enquete <- read_excel("www/data/Enquête qualitative.xlsx")
# dico <- read_excel("www/data/dico.xlsx")
# T2<-Sys.time()
# 
# libelles <- names(data)
# data <- data %>%
#   clean_names()
# column_names <- names(data)
# df <- data.frame(
#   id = column_names,
#   name = libelles
# )
# write_xlsx(df , "www/data/dico.xlsx")
# write_xlsx(data , "www/data/data.xlsx")
# enquete <- enquete %>%
#   clean_names()
# 
# 
# data[ data$date_refus == "NaN",]$date_refus <- NA
# write.csv(data, "www/data/data.csv")
# write.csv(enquete, "www/data/Enquête qualitative.csv")
# write.csv(dico, "www/data/dico.csv")
# T1<-Sys.time()
data <- read.csv("www/data/data.csv") 
enquete <- read.csv("www/data/Enquête qualitative.csv")
dico <- read.csv("www/data/dico.csv")
# T2<-Sys.time() # 3 fois plus rapide : gain = 1/2 seconde




data$revenus <- factor(data$revenus, levels = c("Pas de réponse", "Moins de 20 000 €", "20 000 à 30 000 €", "30 000 à 40 000 €", 
                                                    "40 000 à 50 000 €","50 000 à 60 000 €", "60 000 à 70 000 €", "70 000 à 80 000 €",
                                                    "80 000 à 90 000 €", "Plus de 100 000 €"))
data$usage_ancien_materiel <- factor(data$usage_ancien_materiel, levels = c("Chauffage principal", "Chauffage d'appoint", 
                                                                            "Plaisir / agrément",  "Pas de réponse")) 
data$usage_nouveau_materiel <- factor(data$usage_nouveau_materiel, levels = c("Chauffage principal", "Chauffage d'appoint", 
                                                                            "Plaisir / agrément",  "Pas de réponse")) 
data$freq_utilisation_periode_chauffe <- factor(data$freq_utilisation_periode_chauffe, levels = c("Tous les jours", "3 à 4 jours / semaine", "1 à 2 jours / semaine", 
                                                                                                  "1 à 3 jours / mois", "moins souvent", "Jamais", "Pas de réponse"))

data$frequence_utilisation_periode_de_chauffe_nouveau_materiel <- factor(data$frequence_utilisation_periode_de_chauffe_nouveau_materiel, 
                                                                         levels = c("Tous les jours", "3 à 4 jours / semaine", "1 à 2 jours / semaine", 
                                                                                    "1 à 3 jours / mois",
                                                                                    "moins souvent", "Jamais", "Pas de réponse"))

data$duree_sechage_bois <- factor(data$duree_sechage_bois, levels = c("<1an", "1-2 ans", "> 2 ans", "Ne sait pas", "Pas de réponse"))

data$nature_logement <- factor(data$nature_logement, levels = c("Maison sans jardin", "Maison avec jardin", "Appartement", "Pas de réponse"))

data$periode_de_construction <- factor(data$periode_de_construction, levels = c("avant 1949", "1946-1975", "1975-1990", "1990-2005", "après 2005", "Pas de réponse"))

data$frequence_ramonage <- factor(data$frequence_ramonage, levels = c("2 fois par an", "1 fois par an", "1 fois tous les 2 ans", 
                                                                      "autre (colonne Questionnaire.Fréquence ramonage autre)", "Pas de réponse"))

data$dossier_recu <- as.Date(data$dossier_recu)
data$transfert_facture <- as.Date(data$transfert_facture)
data$date_dossier_complet <- as.Date(data$date_dossier_complet)
data$date_refus <- as.Date(data$date_refus)


data[data$nb_pers_menage == 51, "nb_pers_menage"] <- 5
data[data$nb_pers_menage == 41, "nb_pers_menage"] <- 4
data[data$nb_pers_menage == 30, "nb_pers_menage"] <- 3
data[data$nb_pers_menage == 21, "nb_pers_menage"] <- 2

data[data$nouveau_materiel %in% c("Poêle de masse", "Poêle hydraulique"),]$nouveau_materiel <- "Poêle"
# data[data$type_ancien_appareil  == "Poêle" & data$type_combustible %in% c("Bûche", "Bois de récup"),]$type_ancien_appareil <- "Poêle buche"
# data[data$type_ancien_appareil  == "Poêle" & data$type_combustible == "Granulés",]$type_ancien_appareil <- "Poêle granulés"
data[data$nouveau_materiel == "Poêle" & data$type_combustible_nouveau_materiel == "Bûche",]$nouveau_materiel <- "Poêle bûche"
# data[data$nouveau_materiel == "Poêle" & data$type_combustible_nouveau_materiel == "Bûche, Granulés",]$nouveau_materiel <- "Poêle mixte"
data[data$nouveau_materiel == "Poêle" & data$type_combustible_nouveau_materiel == "Granulés",]$nouveau_materiel <- "Poêle granulés"
data[data$nouveau_materiel == "Poêle",]$nouveau_materiel <- "Poêle autre"
column_names  <- dico %>% filter(to_plot == 1) 
column_names_var2 <- column_names %>% filter(type != "qcm")
column_names_var1 <- column_names_var2 %>% filter(type != "tempo")

couleur_terr <- data.frame(territoire = c("Tous", "CAPV", "GAM", "CCLG"), couleur = c("gray60", "green", "blue", "red"))

no_plot <- c("N..dossier", "adresse_ville", "insee_com", names(data[,42:74]))
quanti <- c("Nb.pers.ménage", "Surf chauff logement", "équivalent_steres", "cout_appareil", "cout_total_TTC", "kwh", "montant_aide" )
tempo <- c("dossier_reçu", "Date_dossier_complet", "Date_refus")
quali <- setdiff(names(data), c(no_plot, quanti, tempo))
qcm <- c("Motivation du changement", "Connaissance aide", "Travaux isolation depuis 2005", "Période utilisation")


# Créer une séquence de dates avec un pas d'un mois
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2023-12-01")
date_vector <- format(seq.Date(from = start_date, to = end_date, by = "month"), "%Y-%m")

# Définir l'ordre d'apparition des modalités des varaibles qcm :
ordre_bin <- list(connaissance_aide = c(8, 1, 11, 2, 7, 3, 4, 5, 9, 12, 6, 10),
                  motivation_changement = c(1, 3, 4, 5, 2, 6, 7),
                  travaux_iso_depuis_2005 = c(6, 1, 2, 5, 7, 3, 4),
                  periode_utilisation = c(5, 3, 2, 4, 1, 6, 7))
ordre_bin["connaissance_aide"]


get_graph <- function(df, colname, couleur){
  column_cara <- column_names %>% 
    filter(id == colname)
  if(column_cara$type == "quanti"){

    x = as.numeric(na.omit(df %>% pull(colname)))
    p <- ggplot(df,aes(x = df %>% pull(colname))) + geom_density(color= couleur) + 
      ggtitle(column_cara$definition) +  theme(axis.title.x=element_blank()) +
      labs(x = "Graphique de densité") 
    data_summary <- data.frame(
      Statistiques = c("Minimum", "1er quartile", "Médiane", "Moyenne", "3ème quartile", "Maximum", "Variance"),
      Valeurs = round(c(summary(x), var(x)), 2)
    )
    # Créer un tableau interactif avec plotly
    plotly_tab <- plot_ly(
      type = 'table',
      header = list(
        values = c("Statistiques", "Valeurs"),
        align = c('center'),
        line = list(width = 1, color = 'black'),
        fill = list(color = 'grey'),
        font = list(family = "Arial", size = 15, color = "white")
      ),
      cells = list(
        values = rbind(data_summary$Statistiques, data_summary$Valeurs),
        align = c('center'),
        line = list(color = "black", width = 1),
        fill = list(color = c('white', 'lightgrey')),
        font = list(family = "Arial", size = 12, color = c("black"))
      )
    )
    fig <- subplot(plotly_tab, ggplotly(p), nrows = 2, heights = c(0.35, 0.65))
    fig
  }
  else if(column_cara$type == "quali"){
    count_df = as.data.frame(table(df %>% pull(colname)))
    colnames(count_df) <- c("Item", "Count")
    
    p <- ggplot(count_df, aes(x = Item, y = Count)) +
      geom_bar(stat = 'identity', fill = couleur) +
      ggtitle(column_cara$definition) +
      ylab("Nombre de demandes") +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
      ylim(0, max(count_df$Count))
    ggplotly(p)
  }
  else if(column_cara$type == "tempo"){
    df <- df[ df %>% pull(colname) >= "2016-01-01" & df %>% pull(colname) < "2024-01-01",]
    temp = df %>% pull(colname)
    # par mois
    mois <- format(temp,"%m")
    mois <- as.data.frame(table(mois))
    colnames(mois) <- c("Mois", "Fréquence")
    Mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
    mois$Mois <- factor(Mois, levels = Mois)
    p1 <- ggplot(mois ,aes(x = Mois, y  = Fréquence)) + geom_bar(stat = 'identity', fill = couleur) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = "Mois", y = "Nombre de demandes") 
    p1 <- ggplotly(p1, tooltip = c("x", "y"))
    # par an : enlever 2024
    an <- format(temp, "%Y")
    an <- as.data.frame(table(an)) 
    colnames(an) <- c("Année", "Fréquence")
    p2 <- an %>%
      ggplot(aes(x = Année, y  = Fréquence,  group = 1)) + geom_point(color = couleur) + geom_line(color = couleur) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = "Année", y = "") 
    p2 <- ggplotly(p2, tooltip = c("x", "y"))
    
    #par mois et an 
    date = data.frame(date = date_vector)
    nb_mois_an <- data.frame(table(sort(format(temp, "%Y-%m"))))
    date = left_join(date, nb_mois_an, by = join_by("date" == "Var1"))
    date$date <- as.Date(paste0(date$date, "-01"), format = "%Y-%m-%d")
    if(sum(is.na(date$Freq)) != 0) date[is.na(date$Freq),]$Freq <- 0
    nb_doss <- ts(date$Freq, start = c(2016, 1), end = c(2023,12), freq = 12)
    p3 <- data.frame(origine = date$Freq, dec = decompose(nb_doss)$trend, date = date$date ) %>%
      ggplot() +
      geom_line(aes(x = date, y = origine, group = 1), color = couleur) +
      geom_line(aes(x = date, y = dec, group = 1), color = "black") + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Année et mois", y = "") 
    p3 <- ggplotly(p3, tooltip = c("x", "y"))
    fig <- subplot(p1, p2, p3, nrows = 1, shareX = TRUE,  shareY = FALSE, titleY = TRUE) %>% 
      layout(title = list(text = column_cara$definition, y = 0.995))
    fig
  }
  else if(column_cara$type == "qcm"){
    column_bin <- dico %>% filter(definition == colname)
    summed_values <- colSums(df[, column_bin$id])
    summed_df <- data.frame(Item = names(summed_values), Count = summed_values)
    summed_df$Item <- factor(summed_df$Item, levels = summed_df$Item[order(ordre_bin[[colname]])])
    
    p <- ggplot(summed_df, aes(x = Item, y = Count)) +
      geom_bar(stat = 'identity', fill = couleur) +
      ggtitle(column_cara$definition) +
      ylab("Nombre de demandes") +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
      ylim(0, max(summed_df$Count))
    ggplotly(p)
    }
}




get_graph_bivar <-function(df, var1, var2, couleur){
if(var1 == var2) get_graph(df, var1, couleur)
else{
column_cara1 <- column_names %>% 
    filter(id == var1)
column_cara2 <- column_names %>% 
    filter(id == var2)

if(column_cara1$type == "quanti" & column_cara2$type == "quanti"){
  p <- ggplot(df,aes_string(x = var1, y = var2)) + geom_point() +
    ggtitle(paste(column_cara1$definition, "en fonction de", column_cara2$definition) )+  
    labs(x = column_cara1$definition, y = column_cara2$definition) 
  ggplotly(p)
}
else if(column_cara1$type == "quali" & column_cara2$type == "quali"){
  p <- ggplot(df, aes_string(x = var1, fill = var2)) + geom_bar(position = "stack") +
    ggtitle(paste(column_cara1$definition, "en fonction de", column_cara2$definition) ) +  
    labs(x = column_cara1$definition, y = column_cara2$definition) 
  ggplotly(p)
  
}
else if(column_cara2$type == "tempo"){
df <- df[ df[,var2] >= "2016-01-01" &  df[,var2] < "2024-01-01" & !is.na( df[,var2]),]
df[,var2] <- format(df[,var2], "%Y")
   if(column_cara1$type == "quali"){
    df <- df %>%
       group_by(across(all_of(c(var2, var1)))) %>%
       summarise(n = n())
     p <- ggplot(df, aes_string(x = var2, y =  "n", group = var1, color = var1)) +
       geom_point() + geom_line() +
  labs(x = column_cara2$definition, color = column_cara1$definition, y = "Nombre de dossiers")
     ggplotly(p)
   }
   else{
     df <- df[!is.na(df[, var1]),] %>%
       group_by(across(all_of(c(var2, "territoire")))) %>%
       summarise(moy = mean(!!sym(var1)))
     p <- ggplot(df, aes_string(x = var2, y =  "moy", group = "territoire", color = "territoire")) +
       geom_point() + geom_line() +
       scale_color_manual(values = c("GAM" = "blue", "CCLG" = "red", "CAPV" = "green")) + 
       labs(x = column_cara2$definition,  y = paste("Moyenne de : ", column_cara1$definition))
     ggplotly(p, tooltip = c("x", "y", "group"))
   }
   }
else{
var_quali <- var2
column_cara_quali = column_cara2
var_quanti <- var1
column_cara_quanti = column_cara1
if(column_cara1$type == "quali"){
  var_quali <- var1
  column_cara_quali = column_cara1
  var_quanti <- var2
  column_cara_quanti = column_cara2
  }

p <- ggplot(df,aes(x = df %>% pull(var_quali), y = df %>% pull(var_quanti))) + geom_boxplot() + 
  ggtitle(paste(column_cara_quanti$definition, "en fonction de", column_cara_quali$definition) )+  
  labs(x = column_cara_quali$definition, y = column_cara_quanti$definition) 
ggplotly(p)
}
}  
}

### carto : 
layer_communes <- st_read("www/data/layer_count.shp")
names(layer_communes)[1:3] <- c("insee_com", "nom_commune","nombre_de_dossiers")
layer_communes$insee_com <- as.integer(layer_communes$insee_com)
layer_communes <- layer_communes[layer_communes$insee_com %in% unique(data$insee_com),]
shp <- st_read(dsn = "www/data/EPCI 2024_region.shp")
shp <- shp[c(27, 175, 929),c("geometry")]
shp$sociale <- c("CCLG", "GAM", "CAPV")           
names(shp)[2] <- "Territoire"
pop <- read.csv("www/data/population.csv", sep = ";", skip = 2)[1:123, c(1,3)]
pop$communes.depcom <- as.integer(pop$communes.depcom)

carto_var <- function(df, colname){
column_cara <- column_names %>% 
    filter(id == colname)  
if(column_cara$type == "quali"){
  count <- df[!is.na(df[, colname]),] %>%
    group_by(insee_com) %>%
    summarise(
      plus_frequent = names(sort(table(!!sym(colname)), decreasing = TRUE))[1], 
      frequence = round(max(table(!!sym(colname))) / sum(table(!!sym(colname))),2)
    )
  layer_communes = layer_communes %>% left_join(count)
  layer_communes = layer_communes %>% left_join(pop, by = join_by("insee_com" == "communes.depcom"))
  
  to_plot <- layer_communes %>%
    mutate(dossier_par_habitant = nombre_de_dossiers/Population.en.2020)
  map <- mapview(shp, col.regions = NA, col = c("red", "blue", "green"), lwd = 5, layer.name = "Territoire", legend = FALSE)
  map <- map + mapview(to_plot, zcol = "plus_frequent",
                       col.regions = colorRampPalette(c("darkgreen", "#FC4E07", "brown", "lightblue", "orange", "pink","purple", "#868686FF", "white")),
                       legend = TRUE,
                       layer.name = column_cara$definition,
                       popup = leafpop::popupTable(to_plot, 
                                                   zcol = c("nom_commune", "insee_com", "plus_frequent", "frequence",
                                                            "nombre_de_dossiers", "Population.en.2020"), row.numbers = FALSE, feature.id = FALSE),
                       hide = FALSE)
  leaflet_map <- map@map 
  leaflet_map
  
}
  else{
    count <- df[!is.na(df[, colname]),] %>%
      group_by(insee_com) %>%
      summarise(moy = mean(!!sym(colname)))
    names(count)[2] = paste("Moyenne de :", column_cara$definition)
    layer_communes = layer_communes %>% left_join(count)
    layer_communes = layer_communes %>% left_join(pop, by = join_by("insee_com" == "communes.depcom"))
    
    to_plot <- layer_communes %>%
      mutate(dossier_par_habitant = nombre_de_dossiers/Population.en.2020)
    map <- mapview(shp, col.regions = NA, col = c("red", "blue", "green"), lwd = 5, layer.name = "Territoire", legend = FALSE)
    map <- map + mapview(to_plot, zcol = names(count)[2],
                         col.regions = colorRampPalette(c("white", "blue")),
                         legend = TRUE,
                         layer.name = names(count)[2],
                         popup = leafpop::popupTable(to_plot, 
                                                     zcol = c("nom_commune", "insee_com", names(count)[2], 
                                                              "nombre_de_dossiers", "Population.en.2020"), row.numbers = FALSE, feature.id = FALSE),
                         hide = FALSE)
    leaflet_map <- map@map 
    leaflet_map
  }
}



carto <- function(var){
count <- data %>%
  group_by(insee_com) %>%
  count()
names(count)[2] = "nombre_de_dossiers"
layer_communes = layer_communes %>% left_join(count)
layer_communes = layer_communes %>% left_join(pop, by = join_by("insee_com" == "communes.depcom"))

to_plot <- layer_communes %>%
          mutate(dossier_par_habitant = nombre_de_dossiers/Population.en.2020)
names(to_plot)[c(3,4,6)] <- c("Nombre de dossiers", "Population en 2020", "Dossiers par habitant")
# Créer la carte interactive avec mapview
map <- mapview(shp, col.regions = NA, col = c("red", "blue", "green"), lwd = 5, layer.name = "Territoire", legend = FALSE)
map <- map + mapview(to_plot, zcol = var,
               col.regions = colorRampPalette(c("white", "blue")),
               legend = TRUE,
               layer.name = var,
               popup = leafpop::popupTable(to_plot, 
                                           zcol = c("nom_commune", "insee_com", "Dossiers par habitant", 
                                                    "Nombre de dossiers", "Population en 2020"), row.numbers = FALSE, feature.id = FALSE),
               hide = FALSE)



leaflet_map <- map@map 
leaflet_map
}




#### Graphiques spécifiques : 
# Sélectionner que les années d'interêt :
df <- data[data$dossier_recu >= "2016-01-01" & data$dossier_recu < "2024-01-01" & !is.na(data$dossier_recu),]
df$dossier_recu <- format(df$dossier_recu, "%Y")

# La prime et ses bénéficiaires : 

demande_territoire <- function(){
plot <- df %>%
  group_by(dossier_recu, territoire) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = dossier_recu, y =  n, group = territoire, color = territoire)) +
  geom_point() + geom_line() + labs(title = "Évolution de la demande de prime par an",
                                 x = "Année",
                                 y = "Nombre de primes",
                                 color = "Territoire") +
  scale_color_manual(values = c("GAM" = "blue", "CCLG" = "red", "CAPV" = "green"))
# Convertir l'objet ggplot en plotly
plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "group"))
plotly_plot
}



taux_majoration <- function(){
  counts_pv = table(df[df$majoration == "M" & df$territoire == "CAPV" ,]$dossier_recu)/ table(df[df$territoire == "CAPV" ,]$dossier_recu)
  counts_g = table(df[df$majoration == "M" & df$territoire == "CCLG" ,]$dossier_recu)/ table(df[df$territoire == "CCLG" ,]$dossier_recu)
  counts_gam = table(df[df$majoration == "M" & df$territoire == "GAM" ,]$dossier_recu)/ table(df[df$territoire == "GAM" ,]$dossier_recu)

  # Combinez les données en un data frame
  df <- data.frame(
    year = as.numeric(names(counts_gam)),
    GAM = as.numeric(counts_gam),
    CCLG = as.numeric(counts_g),
    CAPV = as.numeric(counts_pv))

  # Transformez les données en format long
  df_long <- pivot_longer(df, cols = -year, names_to = "Territoire", values_to = "Taux_majorees")

  plot <- ggplot(df_long, aes(x = year, y = Taux_majorees, color = Territoire)) +
    geom_line() + geom_point() +
    labs(title = "Évolution du taux de primes majorées par an",
         x = "Année",
         y = "Taux de primes majorées") +
    scale_color_manual(values = c("GAM" = "blue", "CCLG" = "red", "CAPV" = "green")) +
    theme(legend.position = "topleft")
  # Convertir l'objet ggplot en plotly
  plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "group"))
  plotly_plot

}

evo_revenus <- function(df, terr){
  if(terr == "CAPV"){
    df <- df[df$dossier_recu >= "2022-01-01" & df$dossier_recu < "2024-01-01" & !is.na(df$dossier_recu),]
  }
  else{  df <- df[df$dossier_recu >= "2016-01-01" & df$dossier_recu < "2024-01-01" & !is.na(df$dossier_recu),]}
  df$dossier_recu <- format(df$dossier_recu, "%Y")
  df <- df %>%
    filter(revenus != "Pas de réponse")
  df$revenus <- factor(df$revenus, levels = rev(levels(df$revenus)))
  plot <- df[!is.na(df$revenus),] %>%
    group_by(dossier_recu, revenus) %>%
    summarise(n = n()) %>%
   mutate(percentage = n / sum(n) * 100) %>% # pour avoir le graph en pourcent
    ggplot(aes(x = dossier_recu, y = percentage, group = revenus, fill = revenus)) +
    geom_area(stat = "identity") + guides(fill="none") +
  labs(x = "Taux de demandes en %",
       y = "",
       fill = "Revenus") +
    theme(axis.title=element_text(face = "bold"))
  # Convertir l'objet ggplot en plotly
  plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "group"))

  plot <- df[!is.na(df$revenus),] %>%
    group_by(dossier_recu, revenus) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = dossier_recu, y = n, group = revenus, fill = revenus)) +
    geom_area(stat = "identity") +
    labs(x = "Nombre de demandes",
         y = "",
         fill = "Revenus") +
    theme(axis.title=element_text(face = "bold"))
  # Convertir l'objet ggplot en plotly
  plotly_plot2 <- ggplotly(plot, tooltip = c("x", "y", "group"))

  fig <- subplot(plotly_plot, plotly_plot2, nrows = 1, shareX = TRUE,  shareY = FALSE, titleY = TRUE) %>% 
    layout(title = list(text = 'Evolution de la demande de prime par tranche de revenus', y = 0.995))
  fig
}

aide_terr <- function(){
  df <- data
  df$global <- "Population totale"
  plot <- ggplot() +
    geom_boxplot(data = df, aes(x= global, y=montant_aide), fill = "grey", alpha = 0.4)+
    geom_boxplot(data = df, aes(x=territoire, y=montant_aide, fill = df$territoire))+
    scale_fill_manual(values=c("CAPV" = "green", "CCLG" = "red", "GAM" = "blue", "Ensemble" = "grey")) + 
    labs(x = "Territoire", y = "Montant de l'aide", fill = "Territoire")

  plotly_plot <- ggplotly(plot) %>% 
    layout(title = list(text = 'Distributions des aides par territoires', y = 0.995))
  plotly_plot
}

somme_des_aides <- function(){
  df <- data[data$transfert_facture >= "2016-01-01" & data$transfert_facture < "2024-01-01" & !is.na(data$transfert_facture),]
  df$transfert_facture <- format(df$transfert_facture, "%Y")
  plot <- df %>%
    filter(!(is.na(montant_aide))) %>%
    group_by(transfert_facture, territoire) %>%
    summarise(Somme = sum(montant_aide)/ 1000) %>%
    ggplot(aes(x = transfert_facture, y =  Somme , group = territoire, color = territoire)) +
    geom_point() + geom_line() + labs(x = "Somme des montants des primes par an", y = "", color = "Territoire") +
    scale_color_manual(values = c("GAM" = "blue", "CCLG" = "red", "CAPV" = "green")) + guides(color = "none")+
    theme(axis.title=element_text(face = "bold"))
  # Convertir l'objet ggplot en plotly
  plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "group"))
  
  plot <- df %>%
    filter(!(is.na(montant_aide))) %>%
    group_by(transfert_facture, territoire) %>%
    summarise(sum = sum(montant_aide)) %>%
    group_by(territoire) %>%
    mutate(Cumul = cumsum(sum)/ 1000) %>%
    ggplot(aes(x = transfert_facture, y =  Cumul , group = territoire, color = territoire)) +
    geom_point() + geom_line() + labs(x = "Cumul des montants des primes", y = "", color = "Territoire") +
    scale_color_manual(values = c("GAM" = "blue", "CCLG" = "red", "CAPV" = "green")) +
    theme(axis.title=element_text(face = "bold"))
  # Convertir l'objet ggplot en plotly
  plotly_plot2 <- ggplotly(plot, tooltip = c("x", "y", "group"))
  fig <- subplot(plotly_plot, plotly_plot2, nrows = 1, shareX = TRUE,  shareY = FALSE, titleY = TRUE) %>% 
    layout(title = list(text = "Évolution de la somme du montant des primes par an en K€", y = 0.995))
  
  fig
} 


#### Modes d'usages :

changement_app <- function(df){
  p1 <- ggplotly(ggplot(df, aes_string(x = "type_ancien_appareil")) +  geom_bar(position = "stack")  + labs(x = "Type de l'ancien appareil") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  p2 <- ggplotly(ggplot(df, aes_string(x = "nouveau_materiel")) + geom_bar(position = "stack") + labs(x = "Type du nouvel appareil") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  p3 <- ggplotly(ggplot(df, aes_string(x = "type_ancien_appareil", fill = "nouveau_materiel")) + geom_bar(position = "stack") +
                   labs(x = "Type de l'ancien appareil", fill = "Type du nouvel appareil") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  fig <- subplot(p1, p2, p3, nrows = 1, shareX = FALSE, shareY = TRUE, titleX = TRUE) %>% 
    layout(title = list(text = "Évolution des types d'appareils", y = 0.995))
  fig
}


evo_combustible <- function(df){
  df <- df[df$dossier_recu >= "2016-01-01" & df$dossier_recu < "2024-01-01" & !is.na(df$dossier_recu),]
  df$dossier_recu <- format(df$dossier_recu, "%Y")
  plot <- df %>%
    group_by(dossier_recu, type_combustible_nouveau_materiel) %>%
    summarise(n = n())  %>%
    filter(type_combustible_nouveau_materiel %in% c("Bûche", "Granulés")) %>%
    ggplot(aes(x = dossier_recu, y =  n, group = type_combustible_nouveau_materiel, color = type_combustible_nouveau_materiel)) +
    geom_point() + geom_line() + labs(x = "Nombre de primes par combustible",
                                      y = "") + guides(color="none") +
    theme(axis.title=element_text(face = "bold"))
  p1 <- ggplotly(plot, tooltip = c("x", "y", "group"))
  plot <- df %>%
    group_by(dossier_recu, type_combustible_nouveau_materiel) %>%
    summarise(n = n()) %>%
    mutate(percentage = n / sum(n) * 100) %>% 
    filter(type_combustible_nouveau_materiel %in% c("Bûche", "Granulés")) %>%
    ggplot(aes(x = dossier_recu, y =  percentage, group = type_combustible_nouveau_materiel, color = type_combustible_nouveau_materiel)) +
    geom_point() + geom_line() + labs(x = "Taux de primes par combustible en %",
                                      y = "",
                                      color = "Type de combustible")  +
    theme(axis.title=element_text(face = "bold"))
  p2 <- ggplotly(plot, tooltip = c("x", "y", "group"))
  fig <- subplot(p1, p2, nrows = 1, shareX = TRUE,  shareY = FALSE, titleY = TRUE) %>% 
    layout(title = list(text = "Évolution de la demande de primes par combustible des nouveaux appareils", y = 0.995))
  fig
}


changement_usage <- function(df){
  p1 <- ggplotly(ggplot(df, aes_string(x = "usage_ancien_materiel")) + geom_bar(position = "stack") + labs(x = "Usage de l'ancien appareil") +
                   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  p2 <- ggplotly(ggplot(df, aes_string(x = "usage_nouveau_materiel")) + geom_bar(position = "stack") + labs(x = "Usage du nouvel appareil") +
                   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  p3 <- ggplotly(ggplot(df, aes_string(x = "usage_ancien_materiel", fill = "usage_nouveau_materiel")) + 
                   geom_bar(position = "stack") +
                   labs(x = "Usage de l'ancien appareil", fill = "Usage du nouvel appareil") +
                   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))) 
  labs(fill = "Usage du nouvel appareil")
  fig <- subplot(p1, p2, p3, nrows = 1, shareX = FALSE, shareY = TRUE, titleX = TRUE)%>% 
    layout(title = list(text = "Évolution des usages", y = 0.995))
  fig
  
}

transfo <- function(qt, unite, type){
  if( is.na(qt) || is.na(unite) || is.na(type)) return(NA)
  qt = as.numeric(qt)
  if(type == "Bûche"){
    if(unite %in% c("Stères", "Mètre cube (M3)")){
      return(qt)
    } # à verif
  }
  else if(type == "Bois de récupération"){
    if(unite %in% c("Stères")) return(qt)
  }
  else if(type == "Granulés / Pellets"){
    if(unite %in% c("Stères", "Mètre cube (M3)")) return(qt)
    else if(unite %in% c("Tonnes")) return( qt * 4800 /1500)
    else if(unite == "Sacs") return( (qt *15 * 4.8) /1500)
  }
  
  return(NA)
}

evo_conso <- function(df){
  eq_stere = c()
  for(i in 1:dim(enquete)[1]){
    eq_stere = c(eq_stere,transfo(enquete[i,"quelle_quantite_de_bois_avez_vous_utilise_lhiver_dernier_avec_votre_nouvel_appareil"], 
                                  enquete[i, "le_chiffre_de_la_question_precedente_concerne"], 
                                  enquete[i, "combustible_appareil_actuel"]))
  }
  eq_stere = na.omit(eq_stere)
  eq_stere2 = as.numeric(na.omit(df$equivalent_steres))
  eq_stere2 = eq_stere2[which(eq_stere2 >0)]
  eq_st = data.frame( Ancienneté = c(rep("Ancien appareil", length(eq_stere2)), rep("Nouvel appareil", length(eq_stere))),  
                      eq = c(eq_stere2, eq_stere) )
  plot <- ggplot(eq_st, aes(x = Ancienneté, y = eq, fill = Ancienneté)) + 
    geom_boxplot() + ylim(-1,20) + ylab("Équivalent stère") + xlab("") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14), legend.title = element_text(size=14), legend.text= element_text(size=12))
  
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1.25)),
    colhead = list(fg_params=list(cex = 1.25)),
    rowhead = list(fg_params=list(cex = 1.25)))
  
  grid.arrange(tableGrob(data.frame(Avant = round(c(summary(eq_stere2), var(eq_stere2)),2),
                                    Après = round(c(summary(eq_stere), var(eq_stere)),2),
                                    row.names = c("Minimum", "1er quartile", "Médiane", "Moyenne", "3ème quartile", 
                                                  "Maximum", "variance")), theme = mytheme),
               plot, nrow = 1, top = textGrob('Évolution de la consommation en équivalent stère',
                                              gp=gpar(fontsize=20,font=1)))

}




### Graphs des différences de moyennes inter clusters communaux :

differences_moy <-function(){
res_acp_com = read_xlsx("www/data/res_acp_communes.xlsx")
res_sup = read_xlsx("www/data/supp_acp_communes.xlsx")
def_acp_com = read_xlsx("www/data/def_var_acp.xlsx")
def_sup = read_xlsx("www/data/def_var_sup.xlsx")

res_acp_com <- left_join(res_acp_com, def_acp_com, by = "variables")
res_sup = left_join(res_sup, def_sup, by = "variables")

# Variables de l'AFDM :
res_acp_com$variables <- factor(res_acp_com$variables, levels = res_acp_com$variables[order(abs(res_acp_com$Diff_grp))])
plot <- ggplot(res_acp_com, aes(y = variables, x = abs(Diff_grp), label = Définition)) +
  geom_point(aes(color = Diff_grp), size = 3) +
  scale_color_gradient2(low = "#0073C2FF", mid = "grey90", high = "red", midpoint = 0, limits = c(-1.5, 1.5)) +
  coord_cartesian(xlim = c(0, 1.75)) +  # Étendre les limites de l'axe X
  labs(title = "Différence de moyennes",
       y = "Modalités",
       x = HTML("Différence de moyennes, <br> variables issues des dossiers"),
       color = HTML("Diff moy <br> (1 - 2)")) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +  # Masquer les labels originaux de l'axe Y
  geom_text(aes(y = variables, label = Définition), color = res_acp_com$label_colors,
            nudge_y = -0.35,  # Déplacer les étiquettes vers la droite
            hjust = 0,
            show.legend = FALSE, size = 3.5)  # Étiquettes avec couleurs définies
plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "label", "color"))

# Variables supplémentaires : 
res_sup$variables <- factor(res_sup$variables, levels = res_sup$variables[order(abs(res_sup$Diff_grp))])
plot <- ggplot(res_sup, aes(y = variables, x = abs(Diff_grp), label = Définition)) +
  geom_point(aes(color = Diff_grp), size = 3) +
  scale_color_gradient2(low = "#0073C2FF", mid = "grey90", high = "red", midpoint = 0, limits = c(-1.5, 1.5)) +
  coord_cartesian(xlim = c(0, 1)) +  # Étendre les limites de l'axe X
  labs(title = "Différence de moyennes",
       y = "",
       x = HTML("Différence de moyennes, <br> variables supplémentaires testées"),
       color = HTML("Diff moy <br> (1 - 2)")) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +  # Masquer les labels originaux de l'axe Y
  geom_text(aes(y = variables, label = Définition), color = res_sup$label_colors,
            nudge_y = -0.35,  # Déplacer les étiquettes vers la droite
            hjust = 0,
            show.legend = FALSE, size = 3.5)  # Étiquettes avec couleurs définies

plotly_plot2 <- ggplotly(plot, tooltip = c("x", "y", "label", "color"))

fig <- subplot(plotly_plot, plotly_plot2, nrows = 1, shareX = TRUE,  shareY = FALSE, titleY = TRUE, titleX = TRUE) %>%
  layout(title = list(text = 'Différences de moyennes des variables centrées réduites, entre les groupes', y = 0.995))
fig
}




### Liens type de commune / cluster

types_communes = read_excel("www/data/types_communes.xlsx", sheet = 1, skip = 2, col_names = TRUE)
names(types_communes) <- c("insee_com", "type_commune")
types_communes = types_communes[types_communes$insee_com %in% data$insee_com,]
types_communes$insee_com <- as.integer(types_communes$insee_com)
types_communes$type_commune <- factor(types_communes$type_commune, levels = c("rural autonome très peu dense", "rural autonome peu dense",
                                                                              "rural sous faible influence d'un pôle", 
                                                                              "rural sous forte influence d'un pôle", 
                                                                              "urbain densité intermédiaire", "urbain dense"))
types_communes$type_commune2 <- types_communes$type_commune
levels(types_communes$type_commune) <- c("rural", "rural", "rural","rural", "urbain", "urbain")

lien_comm_clust <- function(){
clust <- data %>%
  group_by(insee_com) %>%
  summarise(cluster = mean(cluster_com))
clust <- na.omit(clust)

clust <- clust %>% left_join(types_communes, by = "insee_com")
table <- table(clust$cluster, clust$type_commune)
t = t(addmargins(table))
t = t / t[,3]
t = t[1:2,1:2]

table2 <- table(clust$cluster, clust$type_commune2)
t2 = t(addmargins(table2))
t2 = t2 / t2[,3]
t2 = t2[1:6,1:2]

or = c()
inf = c()
sup = c()
mod = c()

clust <- na.omit(clust)
for(m in levels(clust %>% pull("type_commune2"))){
  vec = c()
  for(a in clust %>% pull("type_commune2")){
    if(a == m) vec = c(vec,1)
    else vec = c(vec,0)
  }
  mod = c(mod, m)
  res <-tryCatch(
    expr = {
      res = oddsratio(vec, clust$cluster)$measure[2,]
    },
    error = function(err){
      c(NA, NA, NA)
    }
  )
  or = c(or, res[1])
  inf = c(inf, res[2])
  sup = c(sup, res[3])
  if(length(levels(clust %>% pull("type_commune2"))) == 2) break # pour ne prendre que la première modalité des variables binaires != 0/1, (ex : majoration)
}

odr = data.frame(modalité = mod, OR = or, borne_2.5 = inf, borne_97.5 = sup)

odr = odr[order(odr$OR, decreasing =  FALSE),]
odr$log_or = log(odr$OR)
odr$log_inf = log(odr$borne_2.5)
odr$log_sup = log(odr$borne_97.5)
odr$modalité = factor(odr$modalité, levels = odr$modalité)
plot <- na.omit(odr) %>% ggplot(aes(y = modalité , x = log_or, color = log_or)) +
  geom_point(size = 3) + # Points pour les valeurs de log_or
  geom_errorbar(aes(xmin = log_inf, xmax = log_sup), width = 0.2) + # Barres d'erreur pour les intervalles de confiance
  scale_color_gradient2(low = "seagreen", mid = "grey90",  high = "coral3", midpoint = 0) +
  #coord_cartesian(xlim = c(-5, 5)) +
  labs(title = "Log Odds Ratios with Confidence Intervals",
       y = "Group",
       x = "Log Odds Ratio",
       color = "Log Odds Ratio") +
  geom_vline(xintercept = 0, linetype="dotted") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14), legend.title = element_text(size=14), legend.text= element_text(size=12))

mytheme <- gridExtra::ttheme_default(
  core = list(fg_params=list(cex = 1.1)),
  colhead = list(fg_params=list(cex = 1.1)),
  rowhead = list(fg_params=list(cex = 1.1)))

grid.arrange(
  arrangeGrob(tableGrob(t(table), theme = mytheme), tableGrob(t(table2), theme = mytheme), tableGrob(round(t,2), theme = mytheme),
              tableGrob(round(t2,2), theme = mytheme), nrow = 2, ncol = 2),
             plot, nrow = 1, top = textGrob('Lien entre les clusters et les types de communes',
                                            gp=gpar(fontsize=20,font=1)))
}

### carto groupes de communes: 

carto_cluster_com <- function(){
  count <- data %>%
    group_by(insee_com) %>%
    summarise(cluster = mean(cluster_com))
  count$cluster = as.character(count$cluster)
  layer_communes = layer_communes %>% left_join(count, by = "insee_com")
  layer_communes = layer_communes %>% left_join(pop, by = join_by("insee_com" == "communes.depcom"))
  
  to_plot <- layer_communes %>%
    mutate(dossier_par_habitant = nombre_de_dossiers/Population.en.2020)
  # Créer la carte interactive avec mapview
  map <- mapview(shp, col.regions = NA, col = c("red", "blue", "green"), lwd = 5, layer.name = "Territoire", legend = FALSE)
  map <- map + mapview(to_plot, zcol = "cluster",
                       col.regions = c("seagreen","coral3"),
                       legend = TRUE,
                       layer.name = "Les communes selon leur cluster",
                       popup = leafpop::popupTable(to_plot, 
                                                   zcol = c("nom_commune", "insee_com", "cluster", "dossier_par_habitant", 
                                                            "nombre_de_dossiers", "Population.en.2020"), row.numbers = FALSE, feature.id = FALSE),
                       hide = FALSE)
  
  to_plot <- to_plot %>% left_join(types_communes, by = "insee_com")
  map <- map + mapview(to_plot, zcol = "type_commune",
                       col.regions = c("seagreen","coral3"),
                       legend = TRUE,
                       layer.name = "Les types de communes",
                       popup = leafpop::popupTable(to_plot, 
                                                   zcol = c("nom_commune", "insee_com", "cluster", "type_commune", 
                                                            "dossier_par_habitant", "nombre_de_dossiers", "Population.en.2020"), row.numbers = FALSE, feature.id = FALSE),
                       hide = TRUE)
  map <- map + mapview(to_plot, zcol = "type_commune2",
                       col.regions = colorRampPalette(c("seagreen","coral3")),
                       legend = TRUE,
                       layer.name = "Les types de communes détaillés",
                       popup = leafpop::popupTable(to_plot, 
                                                   zcol = c("nom_commune", "insee_com", "cluster", "type_commune2", 
                                                            "dossier_par_habitant", "nombre_de_dossiers", "Population.en.2020"), row.numbers = FALSE, feature.id = FALSE),
                       hide = TRUE)
  leaflet_map <- map@map 
  leaflet_map
}



### Graphiques des odds-ratios pour les groupes au niveau de l'individu :

plot_OR <- function(){
odr <- read_excel("www/data/OR_afdm.xlsx")
def <- read_excel("www/data/def_var_afdm.xlsx")
odr <- odr %>% left_join(def, by = "modalité")
plot <- na.omit(odr) %>% ggplot(aes(y = Définition, x = log_or, color = log_or)) +
  geom_point(size = 3) + # Points pour les valeurs de log_or
  geom_errorbar(aes(xmin = log_inf, xmax = log_sup), width = 0.2) + # Barres d'erreur pour les intervalles de confiance
  scale_color_gradient2(low = "#0073C2FF", mid = "grey90",  high = "#EFC000FF", midpoint = 0) +
  coord_cartesian(xlim = c(-5, 5)) +
  labs(title = "Log Odds Ratios with Confidence Intervals",
       y = "",
       x = "Log Odds Ratio",
       color = "Log Odds Ratio") +
  geom_vline(xintercept = 0, linetype="dotted") +
  theme_minimal()
plotly_plot <- ggplotly(plot, tooltip = c("x", "y", "log_inf", "log_sup", "color"))
plotly_plot
}


### Graph évolution nb individus / cluster :

evo_clust_ind <- function(df){

df <- df[df$dossier_recu >= "2016-01-01" & df$dossier_recu < "2024-01-01" & !is.na(df$dossier_recu),] 

  plot(table(format(df[df$cluster_ind == "1",]$dossier_recu, "%Y")) / table(format(df$dossier_recu, "%Y")),
       type = "l", col ="pink", main = "évolution des taux de demandeurs par cluster", ylab = "", ylim = c(0,1))
  lines(table(format(df[df$cluster_ind == "2",]$dossier_recu, "%Y")) / table(format(df$dossier_recu, "%Y")), 
        type = "l", col ="purple")
  legend("bottomleft", legend=c("1", "2"),  col=c("pink","purple"), lty=1, cex=0.8, bg="transparent", title = "Clusters")  

}

### Graph différence par variable des clusters d'individus :


diff_cluster_ind <- function(colname, df){
  column_cara <- column_names %>% 
    filter(id == colname)
  df$global <- "Population totale"
  df$cluster_ind <- as.character(df$cluster_ind)
  df = df[!is.na(df$cluster_ind),]
  if(column_cara$type == "quanti"){
    plot <- ggplot() +
      geom_boxplot(data = df, aes(x = global, y = df %>% pull(colname)), fill = "grey", alpha = 0.4) +
      geom_boxplot(data = df, aes(x= cluster_ind, y= df %>% pull(colname), fill = df$cluster_ind))+
      scale_fill_manual(values = c("1" = "#0073C2FF",  "2" = "#EFC000FF", "Global" = "grey")) +
      labs(x = "Groupes", y = "Valeurs", fill = "Cluster") +
      theme_minimal()
    plotly_plot <- ggplotly(plot) %>% 
      layout(title = list(text = glue('Distributions de {column_cara$definition}, par cluster'), y = 0.995))
    plotly_plot
  }
  else if(column_cara$type == "quali"){
    df[, "colname"] = df[, colname]
    p <- ggplot(df) + 
      geom_mosaic(aes(x = product(colname, cluster_ind), fill = colname)) +
      labs(x = "Groupes", y = colname) +
      theme_classic() + 
      
      guides(fill="none") + 
      scale_fill_manual(values=c("#0073C2FF", "#EFC000FF", "#868686FF","#FC4E07", "green", "orange", "pink","purple", "brown", "white")) +
      theme(aspect.ratio = 1) 
    
    p2 <- ggplot(df) + 
      geom_mosaic(aes(x = product(colname, global), fill = colname)) +
      labs(x = "", y = colname) +
      scale_fill_manual(values=c("#0073C2FF", "#EFC000FF", "#868686FF","#FC4E07", "green", "orange", "pink","purple", "brown", "white")) +
      theme(aspect.ratio = 1)
    
    fig <- subplot(ggplotly(p, tooltip = c("x", "y")), ggplotly(p2, tooltip = c("x", "y")), nrows = 1, shareX = FALSE,  shareY = TRUE, titleY = TRUE, titleX = TRUE) %>%
      layout(title = list(text = glue('Proportion de: {column_cara$definition}, entre les groupes'), y = 0.995))
    fig
  }
  
  
  else if(column_cara$type == "tempo"){
    df1 <- df[ !is.na(df$cluster_ind) & df$cluster_ind == "1",]
    df1 <- df1[ df1 %>% pull(colname) >= "2016-01-01" & df1 %>% pull(colname) < "2024-01-01",]
    
    df2 <- df[ !is.na(df$cluster_ind) & df$cluster_ind == "2",]
    df2 <- df2[ df2 %>% pull(colname) >= "2016-01-01" & df2 %>% pull(colname) < "2024-01-01",]
    temp = df2 %>% pull(colname)
    
    mois <- format(temp,"%m")
    mois <- as.data.frame(table(mois))
    colnames(mois) <- c("Mois", "Fréquence")
    max1 = max(mois$Fréquence)
    
    an <- format(temp, "%Y")
    an <- as.data.frame(table(an)) 
    colnames(an) <- c("Année", "Fréquence")
    min2 = min(an$Fréquence)
    max2 = max(an$Fréquence)
    
    temp = df1 %>% pull(colname)
    # par mois
    mois <- format(temp,"%m")
    mois <- as.data.frame(table(mois))
    colnames(mois) <- c("Mois", "Fréquence")
    Mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
    mois$Mois <- factor(Mois, levels = Mois)
    p1 <- ggplot(mois ,aes(x = Mois, y  = Fréquence)) + geom_bar(stat = 'identity', fill = "#0073C2FF") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      ylim(0, max(max1,max(mois$Fréquence))) +
      labs(x = "Mois", y = "Nombre de demandes") 
    gp1 <- ggplotly(p1, tooltip = c("x", "y"))
    # par an : enlever 2024
    an <- format(temp, "%Y")
    an <- as.data.frame(table(an)) 
    colnames(an) <- c("Année", "Fréquence")
    p2 <- an %>%
      ggplot(aes(x = Année, y  = Fréquence,  group = 1)) + geom_point(color = "#0073C2FF") + geom_line(color = "#0073C2FF") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      ylim(min(min2,min(an$Fréquence)), max(max2, max(an$Fréquence))) +
      labs(x = "Année", y = "Nombre de demandes") 
    gp2 <- ggplotly(p2, tooltip = c("x", "y"))
    
    
    temp = df2 %>% pull(colname)
    # par mois
    mois <- format(temp,"%m")
    mois <- as.data.frame(table(mois))
    colnames(mois) <- c("Mois", "Fréquence")
    mois$Mois <- factor(Mois, levels = Mois)
    p12 <- ggplot(mois ,aes(x = Mois, y  = Fréquence)) + geom_bar(stat = 'identity', fill = "#EFC000FF") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = "Mois", y = "Nombre de demandes") 
    gp12 <- ggplotly(p12, tooltip = c("x", "y"))
    # par an : enlever 2024
    an <- format(temp, "%Y")
    an <- as.data.frame(table(an)) 
    colnames(an) <- c("Année", "Fréquence")
    p22 <- an %>%
      ggplot(aes(x = Année, y  = Fréquence,  group = 1)) + geom_point(color = "#EFC000FF") + geom_line(color = "#EFC000FF") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = "Année", y = "Nombre de demandes") 
    gp22 <- ggplotly(p22, tooltip = c("x", "y"))
    
    
    fig <- subplot(gp1, gp12, gp2, gp22, nrows = 2, shareX = FALSE, shareY = TRUE, titleX = TRUE, margin = c(0.01, 0.01, 0.05, 0.05)) %>% 
      layout(title = list(text = glue("{column_cara$definition}, selon le groupe"), y = 0.995))
    fig
  }
  else if(column_cara$type == "qcm"){
    df1 <- df[ !is.na(df$cluster_ind) & df$cluster_ind == "1",]
    column_bin <- dico %>% filter(definition == colname)
    summed_values <- colSums(df1[, column_bin$id])
    summed_df <- data.frame(Item = names(summed_values), Count = summed_values)
    summed_df$Item <- factor(summed_df$Item, levels = summed_df$Item[order(ordre_bin[[colname]])])
    summed_df$Groupe = "1"
    
    df2 <- df[ !is.na(df$cluster_ind) & df$cluster_ind == "2",]
    summed_values2 <- colSums(df2[, column_bin$id])
    summed_df2 <- data.frame(Item = names(summed_values2), Count = summed_values2)
    summed_df2$Item <- factor(summed_df2$Item, levels = summed_df2$Item[order(ordre_bin[[colname]])])
    summed_df2$Groupe = "2"
    
    summed_df <- rbind(summed_df, summed_df2)
    
    p <- ggplot(summed_df, aes(x = Item, y = Count, fill = Groupe)) +
      geom_bar(stat = "identity", position=position_dodge())+
      scale_fill_manual(values=c("#0073C2FF","#EFC000FF")) +
      theme_bw() +
      ggtitle(glue('Répartition de : {column_cara$definition}, selon le groupe')) +
      ylab("Nombre de demandes") +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust=1))+
      ylim(0, max(summed_df$Count))
    ggplotly(p)
  }
}




### carto groupes d'individus : 

carto_cluster_ind <- function(){
  count <- data %>%
    group_by(insee_com) %>% 
    summarise(taux_groupe2 = round(mean(na.omit(cluster_ind)) - 1,3))
  layer_communes = layer_communes %>% left_join(count)
  layer_communes = layer_communes %>% left_join(pop, by = join_by("insee_com" == "communes.depcom"))
  
  to_plot <- layer_communes %>%
    mutate(dossier_par_habitant = nombre_de_dossiers/Population.en.2020)
  # Créer la carte interactive avec mapview
  map <- mapview(shp, col.regions = NA, col = c("red", "blue", "green"), lwd = 5, layer.name = "Territoire", legend = FALSE)
  map <- map + mapview(to_plot, zcol = "taux_groupe2",
                       col.regions = colorRampPalette(c("blue", "yellow")),
                       legend = TRUE,
                       layer.name = "Taux d'individus du groupe 2",
                       popup = leafpop::popupTable(to_plot, 
                                                   zcol = c("nom_commune", "insee_com", "taux_groupe2", "dossier_par_habitant", 
                                                            "nombre_de_dossiers", "Population.en.2020"), row.numbers = FALSE, feature.id = FALSE),
                       hide = FALSE)
  

  
  leaflet_map <- map@map 
  leaflet_map
}







# Textes : 

texte_presentation <- HTML("<font face='Comic sans MS'>
<br><br> <font size='+2'><font color='#0000FF'><U> Fonctionnement</U> :</font> </font></font><br><br>
Cette interface se veut simple et est organisé en trois parties : <br><br>

<B> Exploration : </B> <br>

Cette première partie permet d'explorer les différentes variables, une par une puis deux par deux, à l'aide de représentations graphiques adaptées et de cartographies.
<br> 
Elle vous correspond si vous cherchez à explorer et analyser les données par vous même.
<br><br>


<B> Indicateurs : </B> <br> 

La seconde partie contient une sélection de graphiques spécifiques, ainsi qu'une carte, permettant de mieux comprendre les données. <br>
Elle vous permet d'observer rapidement les indicateurs pertinents.<br>
Elle est divisée en deux sections:
<br>
                   <ul>
                      <li> 'La prime et ses bénéficiaires' : présentant la distribution générale de la prime.</li>
                      <li>'Modes d'usages' : Présentant les évolutions de comportement avant et après le changement d'appareils. </li>
                   </ul>
<br>    

<B> Typologies : </B> 
<br>
La dernière partie présente les résultats que nous avons obtenus en séparant les données en cluster : les liens entre les groupes et les variables, et la répartition
géographique des groupes. Nous avons fait cette analyse à deux niveaux : au niveau communal et à celui de l'individu, ils constitueront chacun une section.<br>
Elle requiert des compétences supplémentaires en analyse de données.")


texte_donnees <- HTML("<br> <font size='+2'><font color='#0000FF'><U>Données</U> :</font> </font></font><br><br>
Les données sont fournies par l’<a href = https://www.alec-grenoble.org/> ALEC </a> et l’<a href = https://www.ageden38.org/>AGEDEN</a>, et ont été collectées de novembre
2015 à mai 2024, et ceci sur trois territoires : Le Pays Voironnais (CAPV), le Grésivaudan (CCLG), et Grenoble Alpes Métropole (GAM). <br><br>
Afin de compléter ces données, nous avons pu en récupérer d’autres, notamment au niveau communal, grâce à l'<a href = https://www.insee.fr/fr/accueil>INSEE</a>, à l'outils 
<a href = https://siddt.inrae.fr>SIDDT</a> d'<a href = https://www.inrae.fr>INRAE</a>, ou encore à 
<a href = https://www.atmo-auvergnerhonealpes.fr/> Atmo Auvergne-Rhône-Alpes</a>. 
(cf <a href = https://forgemia.inra.fr/lessem/primeairbois/-/blob/main/README.md?ref_type=heads> README</a>). <br><br>
Il est important de rappeler que ces données représentent uniquement les bénéficiaires de la Prime Air-Bois, et non l'ensemble des consommateurs du bois de chauffage.

")



texte_explo = HTML("<font face='Comic sans MS'>
<B> Aide à l'interprétation des différents graphiques : </B><br><br>

                   Tout d'abord, il y a une couleur différente pour chaque territoire que vous représentez : <br>
                   <ul>
                      <li>
                      Gris si vous sélectionnez tous les territoires.
                      </li>
                      
                      <li>
                        Vert pour le Pays Voironnais (CAPV)
                      </li>
                      
                      <li>
                        Bleu pour Grenoble Alpes Métropole (GAM)
                      </li>
                      <li>
                        Rouge pour le Grésivaudan (CCLG)
                      </li>
                      
                   </ul>
                   <br>
                   
                   <U> <font size='+1'>Variables qualitatives : </font> </U><br><br>
                   Pour ce type de variable, le graphique à barres représente le nombre de dossiers par modalité de la variable. <br>
                   Attention, pour les variables suivantes : 'Connaissance aide', 'Motivation du changement', 'Travaux isolation depuis 2005', 'Période utilisation'; 
                   Les demandeurs pouvaient cocher plusieurs réponses, il y a donc plus de réponses que de dossiers.<br><br><br>
          
                   <U> <font size='+1'>Variables quantitatives : </font> </U><br><br>
                   Pour ce type de variable, il vous est d'abord proposé un premier tableau contenant différents indicateurs concernant la variable.<br>
                   Ainsi qu'un graphique de densité : celui-ci représente la densité de dossier en fonction des différentes valeurs de la variable, 
                   c'est à dire que plus il y a d'individus ayant une valeur similaire, plus la densité de celle-ci sera forte et la courbe haute.<br><br><br>
                   
                   <U> <font size='+1'>Variables temporelles : </font> </U><br><br>
                   Pour les variables temporelles, trois graphiques vous sont proposés : <br>
                   
                   <ul>
                      <li>
                      Un graphique à barres représentant le nombre de dossier par mois, peu importe l'année.
                      </li>
                      
                      <li>
                        Une courbe représentant le nombre de dossier par année, peu importe le mois.
                      </li>
                      
                      <li>
                        Une courbe représentant le nombre de dossiers par année et par mois, ainsi qu'une estimation de la tendance (en noir), 
                        c'est à dire l'évolution du nombre de dossiers, en enlevant les effets saisonniers et les effets aléatoires.
                      </li>
                      
                   </ul>
                   </font>
                   ")

texte_carto_var = HTML("<font face='Comic sans MS'>
<B> Aide à l'interprétation des cartes : </B><br><br>
Les cartes affichent des choses différentes selon le type de variable : <br><br>

<ul>
                      <li>
                      Pour les variables qualitatives, la carte affiche pour chaque commune, la modalité de la variable la plus fréquente.<br>
                      En cliquant sur une commune, vous obtenez le détail, avec le taux d'apparition de cette modalité.
                      </li>
                      
                      <li>
                      Pour les variables quantitatives, la carte donne la moyenne communale, avec un gradient de couleur permettant de comparer visuellement les communes.
                      </li>
                      
                   </ul>

")
texte_auteur <- HTML("<B> <U>Traitement et analyse des données</U> : </B> <font size='3'> Gabriel Macé </font>")
source <- HTML("<B> <U> Données</U> : </B> <font size='3'>
                        7279 dossiers reçus de janvier 2016 à décembre 2023, source : Dossiers Prime Air-Bois : AGEDEN / ALEC </font>")

source_conso <- HTML("<B> <U> Données</U> : </B> <font size='3'>
                        7279 dossiers reçus de janvier 2016 à décembre 2023, source : Dossiers Prime Air-Bois : AGEDEN / ALEC. <br>
                            374 consommations calculées à partir des données issues des questionnaires facultatifs de juin 2023 à mai 2024,
                     source : Dossiers Prime Air-Bois : AGEDEN / ALEC <br></font>")

precaution_demande <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>
                              La date prise en compte est celle de réception du dossier.</font>")

precaution_majoration <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>
                              La date prise en compte est celle de réception du dossier.</font>")

precaution_tranche_revenu <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>
Le premier graphique représente pour chaque année le taux (en %) de demandeurs par tranche de revenu. Une augmentation du pourcentage ne siginifie pas forcément une 
augmentation du nombre de demandeurs dans cette tranche. Par exemple, de 2020 à 2021, le nombre de demandeurs ayant un revenu supérieur à 100000 € augmente mais son taux 
diminue.<br>
C'est pourquoi il faut aussi prendre en compte le deuxième graphique représentant l'évolution du nombre de demandes divisé par tranche de revenu. <br>
 La date prise en compte est celle de réception du dossier.
                                  </font>")

precaution_somme_aide <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>Le cumul étant forcément croissant, une chute se traduira par une pente moins 
                              forte.<br>
                              La date prise en compte est celle du transfert de la facture vers le bénéficiaire.</font>")

precaution_materiel <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>Le dernier graphique représente le type d'appareil qui remplace celui utilisé 
auparavant.</font>")

precaution_combustible <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'> Seuls les deux types de combustibles principaux sont représentés ici (bûche et 
granulés), le cumul n'est donc pas égal à la valeur totale. Aussi, seuls les combustibles des nouveaux appareils sont représentés, car les anciens sont principalement de la 
bûche. <br>
 La date prise en compte est celle de réception du dossier.</font>")

precaution_usage <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>Le dernier graphique représente le type d'usage du nouvel appareil selon l'usage qui a 
                         été fait auparavant.</font>")

precaution_evo_conso <- HTML("<B> <U> Précaution d'usage</U> :  </B> <font size='-0.5'>Les consommations négatives ne sont pas pris en compte, aussi, pour que l'échelle des 
boîtes à moustache reste pertinente, une limite de 20 équivalents stères est fixée sur le graphique.</font>")



methode_acp = HTML("<B> <U> Analyse au niveau communale </U> : </B><br><br>
<B> Méthodologie : </B><br><br>
Nous avons continué l’analyse au niveau communal.
<br>
Pour cela, nous avons sélectionné les variables que nous avons jugé pertinentes : celles représentant des comportements de consommation du bois, mais aussi celles 
représentant la situation économique et professionnelle des demandeurs, ainsi que leurs motivations. Puis nous les avons transformé en les regroupant par communes : en 
utilisant la moyenne communale pour les variables qualitatives, et en créant une variable représentant le taux par communes de chaque modalité des variables quantitatives 
(par exemple, le taux de primes majorées par commune). Nous n’utilisons pas les variables représentant les modalités de type 'autre', 'pas de réponse, 'NA', ni les communes
présentant moins de 10 dossiers. 
<br><br>

Afin d’illustrer la proximité à la ressource et les caractéristiques socio-économique des communes, nous avons ajouté des variables communales provenant de SIDDT ou des 
bases de données de l’INSEE, telles que le taux de forêt par commune, la médiane de niveau de vie, la densité de population, ou encore l’évolution du nombre de logements 
entre 2015 et 2020, mais aussi le nombre de dossiers, le nombre d’appareils « anciens » à changer fin 2022, et le nombre d’appareils changés en 2023, et ceci aussi ramené 
au nombre d’habitants. <br><br>

Le but de l'analyse étant de séparer les communes selon les caractéristiques des demandeurs, nous avons utilisé des méthodes d'apprentissage statistique de regroupement
(ou clustering).
<br><br><bR><br>


<B> Résultats : </B><br><br>
Le choix de séparer en deux groupes fût le plus optimal.
<br><br>
Le premier groupe contient principalement des communes rurales et une vingtaine de communes urbaines de densité intermédiaire.<br>
Le deuxième groupe contient principalement des communes urbaines, et quelques communes rurales sous influence d'un pôle.<br><br>

Les communes du premier groupe ont en moyenne un taux de prime majorées plus fort (12%), ont un taux plus fort de chauffage principal dans les anciens et nouveaux matériels
(23 et 22%), ont 13 % de plus de pôele dans les noveaux matériels, et remplissent plus de réponses dans les raisons de changement d'appareil par rapport au deuxième groupe.
<br>
Le deuxième groupe a un usage moins important des appareils (appoint, plaisir), avant comme après prime, et a un taux plus fort d'utilisation de la bûche dans les nouveaux 
appareils (15%). <br><br>

Il est possible que les usages des appareils influencent le type d'appareil et de combustible (Chauffage principal -> préférence pour un poêle à granulés). 
De même, les usages peuvent être liés à la siutation économique de l'usager (ex : majoration de la prime). <br><br>

Pour les variables supplémentaires (celles issues de SIDDT et de l'INSEE), dans le premier groupe, il semble y avoir plus de demandes de primes par habitant, ainsi qu'une 
plus grande part de maison dans les résidences principales.<br>
Dans le deuxième groupe, il semble y avoir plus de population, et une plus forte médiane de niveau de vie. Ce qui conforte la séparation rural / urbain des deux groupes.
Les différences entre les taux de forêts sont très faibles (entre 1 et 2 %), et sont plus fort tantôt pour le groupe 1, tantôt pour le 2, dépendant de la variable.
<br><br>
Il semble que les différences de comportement entre les deux groupes soient plutôt dues aux caractéristiques socio-économique des communes.

<br><br>
Les hypothèses de différences de comportements entre utilisateurs étant pour les utilisateurs du bois bûche, nous avons ensuite réalisé la même analyse mais uniquement avec 
les données des personnes se chauffant à la bûche. Les résultats furent assez similaires, mais la séparation communes urbaines / rurales est plus marquée.
")

texte_types_communes = HTML("<B> <U> Types de communes </U> : </B><br><br>
                            Les communes 'urbaines' correspondent aux types : 'urbain dense' et 'urbain densité intermédiaire'.<br>
                            Les communes 'rurales' correspondent aux types : 'rural autonome très peu dense', 'rural autonome peu dense',
                            'rural sous faible influence d'un pôle', 'rural sous forte influence d'un pôle'.
                            ")

methode_afdm = HTML("<B> <U> Analyse au niveau des individus </U> : </B><br><br>
<B> Méthodologie : </B><br><br>
Nous avons continué l'analyse au niveau de l’individu cette fois. <br>
Le but étant de différencier les individus selon leurs comportements quant à l’utilisation du bois comme combustible, tout en prenant en compte leurs caractéristiques 
socio-professionnelles, nous n'avons gardé que les variables liées à ces derniers  <br/><br/> 

Pour que les situations professionnelles des demandeurs et celles des conjoints ne représente plus qu’une seule variable, car nous ne connaissons pas le «chef.fe de 
famille»
,
nous avons procédé à la transformation suivante : Nous avons créé une variable binaire pour chaque modalité, qui sera d’un si elle correspond à au moins une personne du 
ménage.
<br/><br/>
Là aussi, le but de l'analyse étant de séparer / regrouper les demandeurs, nous avons utilisé des méthodes d'apprentissage statistique de regroupement (ou clustering).
<br/><br/>
<br/><br/>

<B> Résultats : </B><br><br>
On choisit a choisi de séparer les individus en deux groupes : <br><br> 

Globalement le groupe 1 est caractérisé par un usage d'appoint et de plaisir de leurs appareils, un ancien appareil de type foyer ouvert ou fermé, l'utilisation 
principalement de la bûche, les bénéficiaires de ce groupe semblent avoir des revenus plus importants, et a des taux plus forts de bénéficiaires cadres et de bénéficiaires
vivant dans des communes urbaines denses que le groupe 2.<br><br>
Les individus du groupe 2 eux, ont une utilisation plus importante de leurs appareils, donc utilisent souvent leurs appareils et comme chauffage principal, se chauffent un 
peu plus aux granulés qu'à la bûche, ont principalement des poêles, ont des revenus moins importants et donc plus de primes majorées.<br>
Dans ce groupe, il y a un taux plus importants de bénéficiares employés  et de bénéficiaires ouvriers, ainsi que de bénéficiaires vivant dans des communes rurales que dans
le groupe 1.
<br><br>
Le taux de demandeur du type du groupe 1 semble augmenter depuis 2022.
<br><br>
Nous n'observons pas de résultats probants ou de régularités en analysant uniquement les dossiers des personnes se chauffant maintenant à la bûche.
<br><br>
")


texte_aide_diff_com <- HTML("<B> <U> Graphiques différences de moyennes</U> : </B><br><br>
Sur ces représentation graphique, nous avons représenté les différences de moyennes entre les deux groupes, pour chaques variables (issues des dossiers et supplémentaires),
que nous avions préalablement centrées et réduites, afin qu'elles soit toutes d'échelles comparables.
<br><br>
Tous les points sont supérieurs à 0 car nous avons pris la valeur absolue des différences moyennes, afin de comparer les différences peu importe quel groupeà la moyenne la
plus forte. Les points bleus sont ceux où la moyenne du groupe 2 est plus forte que celle du groupe 1, et inversement pour les rouges.<br>

Plus une différence est forte, plus sa couleur est forte / opaque, et plus elle est haute et à droite sur le graphique.

<br><br>
Aussi, les variables pour les quelles il y a statistiquement égalité des moyennes ont leur nom écrit en gris.
                            ")

aide_OR <- HTML("<B> <U> Les Odds Ratios</U> : </B><br><br>
Afin de comprendre les caractéristiques des clusters, nous avons calculé des « odds-ratios » (rapport de côte): <br>
Les chances d’un individu d’appartenir au groupe 2 quand il présente une caractéristique, contre les chances s’il ne présente pas la caractéristique. 
<br><br>
Un OR est significatif, c’est-à-dire que la modalité amène à être dans le groupe 2 ou à ne pas être dans le groupe 2, si la borne inférieure de son intervalle de confiance 
est supérieure à 1 dans le premier cas, ou si la borne supérieure de son intervalle de confiance est inférieure à 1 pour le deuxième. <br><br>
Afin que les résultats soient symétriques, nous avons calculé le logarithme de l’OR et de ses intervalles de confiance.On compare donc non plus à 1 mais à 0.<br><br>

Donc, dans ces graphiques, les caractéristiques amenant à être dans le groupe 2 sont celles où tous l'intervale de confiance est supérieur à 0, donc à droite de l'axe 0. 
<br>
Les caractéristiques amenant à ne pas être dans le groupe 2 (donc à être dans le groupe 1) sont celles où tous l'intervale de confiance est inférieur à 0, donc à gauche 
de l'axe 0.<br><br>
Si un évenement (ici une modalité) présente trop peu de cas, le calcul du rapport de côte est impossible.
                ") 

aide_graph_clust_ind <- HTML("<font face='Comic sans MS'>
<B> Aide à l'interprétation des différents graphiques : </B><br><br>

                   Tout d'abord, il y a une couleur différente pour chaque groupe : Bleu pour le premier, jaune pour le deuxième <br><br>

                   
                   <U> <font size='+1'>Variables qualitatives : </font> </U><br><br>
                   Pour ce type de variable, le graphique à barres représente le taux d'apparition des modalités de la variable, pour le groupe 1, le 2, et 
                   la population totale. <br>
                   Attention, pour les variables suivantes : 'Connaissance aide', 'Motivation du changement', 'Travaux isolation depuis 2005', 'Période utilisation'; 
                   Les demandeurs pouvaient cocher plusieurs réponses, il y a donc plus de réponses que de dossiers, on ne peut donc pas calculer de taux par dossiers,
                   nous représentons donc le nombre total de réponses par modalité.<br><br><br>
          
                   <U> <font size='+1'>Variables quantitatives : </font> </U><br><br>
                   Pour ce type de variable, nous représentons des boites à moustaches : la répartition de la variable pour le groupe 1, le 2, et 
                   la population totale.<br><br><br>
                   
                   <U> <font size='+1'>Variables temporelles : </font> </U><br><br>
                   Pour les variables temporelles, deux graphiques vous sont proposés, pour chaque groupe : <br>
                   
                   <ul>
                      <li>
                      Un graphique à barres représentant le nombre de dossiers par mois, peu importe l'année.
                      </li>
                      
                      <li>
                        Une courbe représentant le nombre de dossiers par année, peu importe le mois.
                      </li>

                   </ul>
                   </font>
                   ")
  
aide_map_clust_ind <- HTML("<font size='-0.5'>Le gradient de couleur représente le taux d'individus du groupe 2, par communes : <br>
                           Plus une commune est jaune plus elle contient un taux important d'individus du groupe 2, et plus elle est bleue plus elle contient un taux
                           important d'individus du groupe 1.</font>")

