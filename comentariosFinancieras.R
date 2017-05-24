####################################
#                                                ^ ^
# Creado por Área de Data Science    <(°)      =(°-°)=         
# Fernando Dorantes Nieto              ( >)"    (   )S  
#                                       /|       w w
#                                              
library(magrittr)
c("data.table", "dplyr", "tidyr", "lubridate", "Rfacebook", "ggplot2") %>% 
  sapply(require,character.only=T)


# Realizando la conexión APIS REDES SOCIALES
# Conexión facebook API ---------------------------------------------------
fb_oauth <- fbOAuth(app_id="1611650985792093", 
                    app_secret="85db5a49077d074e84b5ce0a19659893", 
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("~/fb_oauth")
idFB_seat = 113144262054871


# Directorio --------------------------------------------------------------
setwd("~/local/Sonia/HoraPosteoFinanciera/datos")


# Datos -------------------------------------------------------------------
data <- read.csv("datosAnalisis.csv", header = T)
ids <- data$id %>%  unique %>% 
  as.character


# Comentarios -------------------------------------------------------------
numero <- 0
comments <- lapply(c(ids, "dasdasda"), function(x){
  numero <<- numero + 1 
  Y = tryCatch(
    getPost(x, comments = T, n.comments = 2000, token = fb_oauth),
    error = function(e){NULL}
  )
  if(!is.null(Y)){
  idPost  = Y$post$id
  Y = Y$comments %>%  
    mutate(idPosteo = idPost)
  }else{Y = NULL}
  print(c(numero, Y$idPosteo[1] ))
  Sys.sleep(2.5)
  return(Y)
  }) 

comments <- rbindlist(comments)
comments %>% 
  write.csv("comentariosFinancieras.csv", row.names=F)









