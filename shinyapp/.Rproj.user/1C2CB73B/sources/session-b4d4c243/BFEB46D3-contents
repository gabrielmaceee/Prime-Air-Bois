library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(purrr)
library(forcats)
library(glue)
library(janitor)
library(wordcloud)
library(tm)
library(tidyr)

install.packages("reticulate")
library(reticulate)

# Appeler le script Python
py_run_file("measure_co2.py")
# Plan analyse : 
# but à long terme : Long terme : Projet de thèse avec comparaison inter-territoriale
# but à court terme : analyse géographique, variation temporelle de la demande/nb dossier, -> de la difussion de la prime
# identification de « types » de consommateurs, liens entre les variables

# données siddt : réduction de dim / clustering : nouvel variable pour les dossiers
# 1 Graph variable par variable : barplot / density / word cloud (catégorisation des mots) / par territoire
# 2 Graph croisement : boxplot évolution selon une modalité quanti quali / graph de corrrélation quanti quanti / barplot/ modalités : quali quali
# 3 Tests : Corrélation / régression linéaire : quanti quanti
           # chi2 indépendance quali quali
           # test chi2 loi normale (mue = 100) (qqplot) Surf Chauff logement
           # anova / fisher : égalité des moyennes + tests égalité des distributons quanti quali
# correction Benjamini-Hochberg
# peut être pas essentiel, mais si pour assurer la qualité des réductions de dimensions et clustering -> calcul indicateur lien entre variables
# -> colinéarité, corrélation, VIF : VIFk ≥ 10 ⇐⇒ R2 k ≥ 0.9
# enlever les variables "doublons"
# 4 : Trouver les variables qui en explique d'autres : régression linéaire / logistique
# ACP / AFC / AFDM / tsne (que quanti) -> projection 2D : coloration selon les différentes variables : clusters ? 
                    # clustering sur données de bases / transformées 
                    # puis selon le nb de dossier / commune et les caractéristiques des communes (ex part des forets)
# clustering : transformation variables catégorielles ?
# acp communales : lien linéaire (car taux inter-dépendant) entre les variables, TSNE ne devrait pas fonctionner

# 5 analyse temporelle : selon les différents types de dates -> prévision ?
# analyse par territoire : diviser par le nb d'habitants, ou d'objectif, ou de bénéficiaire
# graph scatterplot : évolution nb bénéficiare = y, x = niveau de revenu des communes
# taux de personnes bénéficiant de la prime majorée (modeste) -> objectif 30% graph évolution cf schéma Sandrine

# changement de type de mode chauffage : buche -> buche, buche -> granulé, granulé -> granulé, granulé -> buche, en fonction des années

# 6 analyse géographique : carte variable par variable nb dossier / variable quanti / 
# nb par modalité ou la modalités la plus rpz variable quali / variable des communes aussi
# dviser par le nombre d'habitants de chaque commune
# + pareil mais carte évolutive en fonction du temps / des années

# Analyse des différences inter-variables par territoire


source <- read_excel("C:/Users/gmace/PAB/Projet Bois-buche/data/1_Source/Questionnaire.xlsx")
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





data <- read_excel("C:/Users/gmace/PAB/Projet Bois-buche/data/3_Resultat/data_questionnaire_complet.xlsx")
# des infos ont disparu ?? 10 variables que Na
# en faite dans " Correspondances_codes_libellés ", il y a des variables où il n'y a pas les facteurs, donc mets Na

data_analyse <- data[,-c("N..dossier", "ID.enregistrement", "Qtté.bois.consommée", "Qtté.bois.unité", 
                         "Appro.autre", "Fréquence.ramonage.autre","Type.combustible.nouveau.materiel.autre")]

#d <- read_excel("C:/Users/gmace/PAB/Projet Bois-buche/data/1_Source/Traitement donnees FABG.xlsx")
data <- as.data.frame(data[,-c(1:2)])
summary(data)


library(ggplot2)

# Traitement variables qualitatives : 

# Nb pers menage
plot(density(data$`Nb.pers.ménage`)) # loi normale asymétrique: longue queue à droite
summary(data$`Nb.pers.ménage`) # famille de 0 ? De 51 ?
var(data$`Nb.pers.ménage`)
# <0 à 51, moy = 2.665, var =  2.528184, loi de Poisson (moy = var) ? -> décrire un évenement dans un interval temporel fixé, voir spatial
lines(dpois(x = 1:6578, lambda = 2.6), type = "l", col = "red")

# Surf chauff logement : 
plot(density(data$`Surf.chauff.logement`)) # loi normale
summary(data$`Surf.chauff.logement`) # de 0 à 400 : 0 ?
var(data$`Surf.chauff.logement`)
# de 0 à 400, moy = 113.8, var = 1976.224

library(corrplot)
pairs(data[,c("Nb.pers.ménage","Surf.chauff.logement","kwh")]) # ne semble pas du tout corrélés
plot(data$Surf.chauff.logement, data$kwh)


library(wordcloud)
library(tm)
wordcloud(data$Motivation.autre, max.words = 50, colors = brewer.pal(8, "Dark2"),  rot.per=0, scale = c(2.5,0.5), min.freq = 3)

library(Hmisc)
describe(data$Territoire)

plot.ecdf(data$Surf.chauff.logement)

library(tidyr)
km = kmeans(na.omit(data[, c("Nb.pers.ménage", "kwh", "Surf.chauff.logement")]), center = 5)
table(km$cluster)


dist_X = dist(na.omit(data[, c("Nb.pers.ménage", "kwh", "Surf.chauff.logement")]))
hc = hclust(dist_X, method = "ward.D2")
plot(hc, na.omit(data[, c("N..dossier", "Nb.pers.ménage", "kwh", "Surf.chauff.logement")])$N..dossier)
hc_5 = cutree(hc, k=5)
contingence = table(km$cluster, hc_5)
barplot(contingence)
# pas tellement d'intérêt, à voir après ACP/AFC/T-sne







########### par années : 
plot(table(nb_mois[nb_an == "2016"]), type = "l", ylim = c(0, 170), col = "2016")
for(i in 2017:2023){
  lines(table(nb_mois[nb_an == i]), type = "l", col = i)
}
legend(1, 170, legend=c(2016:2023), col=c(2016:2023), lty=1, cex=0.8)

data_list <- lapply(2016:2023, function(year) {
  data.frame(
    month = as.integer(names(table(nb_mois[nb_an == year]))),
    count = as.integer(table(nb_mois[nb_an == year])),
    year = as.factor(year)
  )
})

# Combinez les tables de données en une seule
data_combined <- bind_rows(data_list)

# Convertir l'année en numérique pour le gradient de couleur
#data_combined$year_numeric <- as.numeric(as.character(data_combined$year))

# Trouver les points de fin pour chaque année
data_labels <- data_combined %>%
  group_by(year) %>%
  filter(month == max(month))

# Créez le graphique avec ggplot et un gradient de couleur
ggplot(data_combined, aes(x = month, y = count, color = year, group = year)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 170)) +
  #scale_color_gradient(low = "blue", high = "red", name = "Année") +
  geom_text(data = data_labels, aes(label = year), hjust = -0.2, vjust = 0.5) +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  labs(title = "Évolution mensuelle des comptes par année",
       x = "Mois",
       y = "Nombre") +
  theme_minimal()

###################


data <- read_excel("C:/Users/gmace/PAB/Projet Bois-buche/data/1_Source/Questionnaire.xlsx")
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










######## Modification de graphs de manière non automatisée : 



myplot <- ggplot(data, aes(x = Age, main = glue("Répartition des modalités de : Age"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Age") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Age.png"), myplot, device = "png")


data$Revenus <- factor(data$Revenus, levels = c("Pas de réponse", "Moins de 20 000 €", "20 000 à 30 000 €", "30 000 à 40 000 €", "40 000 à 50 000 €",
                                                "50 000 à 60 000 €", "60 000 à 70 000 €", "70 000 à 80 000 €", "80 000 à 90 000 €", "Plus de 100 000 €"))
myplot <- ggplot(data, aes(x = Revenus, main = glue("Répartition des modalités de : Revenus"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Revenu") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Revenus.png"), myplot, device = "png")



data$Freq.utilisation.période.chauffe <- factor(data$Freq.utilisation.période.chauffe, levels = c("Tous les jours", "3 à 4 jours / semaine",
                                                                                                  "1 à 2 jours / semaine",  "1 à 3 jours / mois", "moins souvent", "Jamais", "Pas de réponse"))
myplot <- ggplot(data, aes(x = Freq.utilisation.période.chauffe, main = glue("Répartition des modalités de : Freq utilisation période chauffe"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Freq utilisation période chauffe ancien appareil") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Freq utilisation période chauffe.png"), myplot, device = "png")

myplot <- ggplot(data, aes(x = fct_infreq(factor(Sit.pro.demandeur)), main = glue("Répartition des modalités de : Sit pro demandeur"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Sit pro demandeur") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Sit pro demandeur.png"), myplot, device = "png")



myplot <- ggplot(data, aes(x = Type.Ancien.appareil, main = glue("Répartition des modalités de : Type Ancien Appareil"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Type Ancien appareil") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Type Ancien Appareil.png"), myplot, device = "png")

data[data$Nouveau.matériel %in% c("Poêle hydraulique", "Poêle de masse"), ]$Nouveau.matériel <- "Poêle"
myplot <- ggplot(data, aes(x = Nouveau.matériel, main = glue("Répartition des modalités de : Nouveau matériel"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Nouveau matériel") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Nouveau matériel.png"), myplot, device = "png")

p <- ggplot(data, aes_string(x = "Type.Ancien.appareil", fill = "Nouveau.matériel")) + geom_bar(position = "stack")
ggsave(glue("../3_Resultat/graphes/croisés/Quali-quali/Type Ancien appareil_Nouveau matériel_barplot.png"), width = 20, height = 20, units = "cm")

data$Usage.ancien.matériel <- factor(data$Usage.ancien.matériel, levels = c( "Pas de réponse", "Plaisir / agrément", "Chauffage d'appoint", "Chauffage principal"))
myplot <- ggplot(data, aes(x = Usage.ancien.matériel, main = glue("Répartition des modalités de : Usage ancien matériel"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Usage réalisé ancien matériel") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Usage ancien matériel.png"), myplot, device = "png")


data$Usage.nouveau.matériel <- factor(data$Usage.nouveau.matériel, levels = c( "Pas de réponse", "Plaisir / agrément", "Chauffage d'appoint", "Chauffage principal"))
myplot <- ggplot(data, aes(x = Usage.nouveau.matériel, main = glue("Répartition des modalités de : Usage nouveau matériel"))) + 
  geom_bar(stat = 'count') +  
  coord_flip() + 
  ggtitle("Usage prévu nouveau matériel") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.y = element_text(size = 12, hjust = 1, family = "Fira Sans"))
myplot
ggsave(glue("../3_Resultat/graphes/Univariés/Usage nouveau matériel.png"), myplot, device = "png")







library(fastDummies)
 df_long <- data[, c("N..dossier", "Connaissance.aide" )] %>%
    separate_rows("Connaissance.aide", sep = ";")
  df_long <- dummy_cols(df_long, select_columns = "Connaissance.aide", remove_first_dummy = FALSE, remove_selected_columns = TRUE)
  df_binary <- df_long %>%
    group_by(`N..dossier`) %>%
    summarize(across(everything(), max))
  names = c("« Bouche oreille »", "ALEC/AGEDEN","Autre","G.A.M", "Installateur", "Grésivaudan",
            "Voironnais","Mairie","Notaire/conformité","Pas de réponse", "Presse locale", "Site Internet (préciser)")
  names(df_binary) <- c("N° dossier",names)
 
  
interco = 0 
for(i in c(1:dim(df_binary)[1])){
  if(df_binary[i, "G.A.M"] == 1) interco = interco + 1
  else if(df_binary[i, "Voironnais"] == 1) interco = interco + 1
  else if(df_binary[i, "Grésivaudan"] == 1) interco = interco + 1
} 
  
  
  cont = c()
  for(v in names){
    for(v2 in names){
      cont = c(cont, dim(df_binary[df_binary[,v] == 1 & df_binary[v2] == 1,])[1])
    }
  }
  contingence = matrix(cont, nrow = 12,  ncol = 12)
  rownames(contingence) <- names
  colnames(contingence) <- names
  
  seul = c()
  for(r in setdiff(names, c("G.A.M", "Grésivaudan", "Voironnais"))){
    for(c in setdiff(names, c("G.A.M", "Grésivaudan", "Voironnais"))){
      if(c==r){
        seul = c(seul,contingence[r,c])
      }
    }
  }
  nom = "Connaissance.aide"
  coul = c(brewer.pal(n = 10, name = 'Paired'))
  names = c("Bouche oreille", "ALEC/AGEDEN","Autre", "Installateur",
            "Mairie","Notaire/conformité","Pas de réponse", "Presse locale", "Site Internet", "intercommunalités")
  dataf <- data.frame(names = names,seul = c(seul, interco) ,coul = coul)
  dataf$names <- factor(dataf$names, levels = c("intercommunalités", "ALEC/AGEDEN", "Mairie", "Bouche oreille",  "Installateur", "Notaire/conformité", 
                                                "Presse locale", "Site Internet","Autre","Pas de réponse"))
  myplot<- ggplot(dataf, aes(x = names, y = seul, fill = coul)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() + # Utiliser les couleurs spécifiées dans le vecteur 'coul'
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12)) + # Faire pivoter les étiquettes des axes
    labs(title = nom, x = "Noms", y = "Valeurs")
  myplot
  ggsave(glue("../3_Resultat/graphes/Univariés/Connaissance aide.png"), myplot, device = "png")
  
  
  
  
data$Usage.ancien.matériel <- factor(data$Usage.ancien.matériel, levels = c("Chauffage d'appoint", "Chauffage principal",
                                                                                                    "Plaisir / agrément",  "Pas de réponse")) 
  
p <- ggplot(data, aes_string(x = "Usage.ancien.matériel", fill = "Usage.nouveau.matériel")) + geom_bar(position = "stack")
p
ggsave(glue("../3_Resultat/graphes/croisés/Quali-quali/Usage.ancien.matériel_Usage.nouveau.matériel_barplot.png"), width = 20, height = 20, units = "cm")


data$Type.Ancien.appareil <- factor(data$Type.Ancien.appareil, levels = c("Chaudière", "Cuisinière", "Foyer ouvert", "Insert / foyer fermé", "Poêle", "Pas de réponse")) 

p <- ggplot(data, aes_string(x = "Type.Ancien.appareil", fill = "Nouveau.matériel")) + geom_bar(position = "stack")
p
ggsave(glue("../3_Resultat/graphes/croisés/Quali-quali/Type.Ancien.appareil_Nouveau.matériel_barplot.png"), width = 20, height = 20, units = "cm")
