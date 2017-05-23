# Librerías ---------------------------------------------------------------
library(magrittr)
c("data.table", "dplyr","tidyr","lubridate","vegetarian","vegan",
  "lattice","ggplot2", "purrr","lme4", "xts","reshape2","Rfacebook",
  "dygraphs") %>% 
  sapply(require, character.only=T)

# Conexión a Facebook -----------------------------------------------------
fb_oauth <- fbOAuth(app_id = "1611650985792093", 
                    app_secret = "85db5a49077d074e84b5ce0a19659893", 
                    extended_permissions = TRUE)
save(fb_oauth, file= "fb_oauth")
load("~/fb_oauth")
idFB_seat = 113144262054871

# Directorio ---------------------------------------------------------
setwd("~/local/Sonia/AnalisisFinanciera")
# Datos ---------------------------------------------------------------
idautomotriz <- read.csv("idsAutomotriz.csv", header = T)

setwd("~/local/Sonia/AnalisisFinanciera/datos")


# Recolección de datos ----------------------------------------------------

# autosData <- lapply(idautomotriz[,2], function(x){
#   X <- getPage(x, token = fb_oauth, reactions = T, feed = F, 
#                since = "2016-01-01", until = Sys.Date(), n= 5e4)
#   Sys.sleep(60)
#   return(X)
# })
# 
# autosData <- rbindlist(autosData)
# 
# autosData1 <- lapply(idautomotriz[,2], function(x){
#   X <- getPage(x, token = fb_oauth, reactions = T, feed = F, 
#                since = "2015-01-01", until = "2015-12-31", n= 5e4)
#   Sys.sleep(60)
#   return(X)
# })
# autosData1 <- rbindlist(autosData1)
# 
# autosData  <- rbind(autosData1, autosData)
# autosData %>%  write.csv("datosAnalisis.csv", row.names=F)

autosData <- read.csv("datosAnalisis.csv", header = T)

##Manipulación de datos
autosData <- autosData %>% 
  separate(created_time, c("Fecha","Hora"), sep="T", remove = F) 

autosData <- autosData %>%
  left_join(autosData %>% 
              group_by(Fecha,from_name) %>%  
              tally %>% 
              rename(numeroPosteos=n)) %>% 
  data.table %>% 
  .[,created_time := gsub("T", " ", created_time)] %>%  
  .[,created_time := gsub("[+]\\S+", "", created_time)] %>%  
  .[,created_time := as.POSIXct(created_time, format="%Y-%m-%d %H:%M:%S")] %>% 
  .[,Hora := gsub("[:]\\S+", "", Hora)] %>%  
  .[,interaccionTotal := rowSums(.[,c(2,12:18)])] %>% 
  .[,reaccionesTotales := rowSums(.[,c(2,14:18)])] %>%
  .[,Fecha := as.Date(Fecha)] %>%
  .[,diaSemana := weekdays(Fecha)] %>% unique 

autosData %>%  
  ggplot(aes(x = created_time, y= interaccionTotal ))+
  geom_line()+
  facet_wrap(~from_name)

####Series de tiempo
tiempo <- autosData %>% 
  select(Fecha,interaccionTotal, reaccionesTotales, comments_count,
         shares_count, likes_count, love_count, haha_count, sad_count, wow_count,
         angry_count, from_name) %>%  
  gather(tipo, valor, -Fecha, - from_name) %>%  
  mutate(union = paste(from_name, tipo, sep="_")) %>%  
  select(-from_name, -tipo) %>% 
  mutate(id= 1:n()) %>% 
  spread(union, valor) 

tiempo[is.na(tiempo)]<- 0

xts(tiempo[,-1], as.Date(tiempo[,1])) %>% dygraph
