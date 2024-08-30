# Fonction de formatage de texte
# Requis: Package de nettoyage de string
# install.packages('gsubfn')

library(gsubfn)
unwanted_array = 
  list('é'='e', 'è'='e', 'à'='a', 'ç'='c', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
       'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
       'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
       'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
       'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'ý'='y', 'þ'='b')

nettoie_texte <-function (myvecteur){
  myvecteur = tolower(myvecteur)
  myvecteur <- gsub("[[:punct:]]", "_", myvecteur) 
  myvecteur <- gsub("[[:space:]]", "_", myvecteur) 
  myvecteur <- gsub("\"", "_", myvecteur)
  #Fonction traitant les accents (ne peut pas prendre en charge ls + -, etc)       
  myvecteur <-gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,myvecteur)
  #Finition/polichage qui ne veut pas fonctionner dans la fonction... etrange non?
  myvecteur <- gsub("____", "_", myvecteur)
  myvecteur <- gsub("___", "_", myvecteur)
  myvecteur <- gsub("__", "_", myvecteur)
  myvecteur <- gsub("_$", "", myvecteur)
  myvecteur <- gsub("^_", "", myvecteur)
}