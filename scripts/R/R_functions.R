# Fonctions Generiques d'import dans la base

# install.packages('RPostgreSQL')
library(RPostgreSQL)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("R_functions_nettoie_texte.R",encoding="UTF-8")



#### Connexion a la BDD ####

# DC)claration du driver a utiliser
drv <<- dbDriver("PostgreSQL")

#PrC)paration de la chaine de connexion
con <<- dbConnect(drv, 
                  dbname = db_name , 
                  host = db_host, 
                  port = "5432", 
                  user = db_user, 
                  password=db_pw)

#Declaration de l'encodage courant (souvent diffC)rent de la base qui est en UTF 8)
#postgresqlpqExec(con, "SET client_encoding = 'windows-1252'");
con_encoding <- "UTF-8"
postgresqlpqExec(con, paste0("SET client_encoding = '", con_encoding, "'"));


####
## Fonction import depuis R dans PG

import_base <- function (thedataframe, shema_name, table_name, app = F){
  
  names(thedataframe) <- nettoie_texte(names(thedataframe))
  table_name <- nettoie_texte(table_name)
  
  # Si la table existe et qu'on ne souhaite pas y ajouter de la donner
  # La supprimer et commeter l'C)crasemenet
  if(dbExistsTable(con, c(shema_name,table_name)) & app == F){
    dbRemoveTable(con, c(shema_name,table_name))
    print (paste("remove", shema_name,table_name))
  }
  
  # InsC)rer la table (ou l'incrC)menter si besoin)
  print (paste("insert", shema_name,table_name))
  
  dbWriteTable(con, 
               name = c(shema_name,table_name), 
               append = app,
               value = thedataframe, 
               row.names=FALSE)
  
  #Deconnexion de la base
  # dbDisconnect(con)
}

addPK <- function(schema, table, pk, nom = "") {
  sql <- paste0("ALTER TABLE ", schema, ".", table, " ADD CONSTRAINT ", table, nom, "_pkey PRIMARY KEY (",pk,");")
  dbGetQuery(con, sql)  
}

addFK <- function(schema, table, fk, ref, nom = "") {
  sql <- paste0("ALTER TABLE ", schema, ".", table, "
                  ADD CONSTRAINT FK_", table, nom, "
                  FOREIGN KEY (", fk, ") REFERENCES ", ref, "; ")
  dbGetQuery(con, sql)  
}

grantpublic <- function(schema, table) {
  
  sql <- paste0("GRANT SELECT ON TABLE ", schema, ".", table," TO groupe_tout_public;")
  dbExecute(con, sql)
}
