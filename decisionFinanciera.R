####################################
#                                                   ^ ^
# Creado por el Área de Data Science    <(°)      =(°-°)=         
# Fernando Dorantes Nieto                 ( >)"    (   )S  
#                                          /|       w w
#                                              


library(magrittr)
c("data.table", "dplyr", "lubridate", "ggplot2", "lattice", "purrr",
  "lme4", "tidyr", "stringi", "tree", "rpart", "rpart.plot","rattle") %>% 
  sapply(require, character.only=T)

##Sembrando directorio
setwd("~/local/Sonia/HoraPosteoFinanciera/datos/")



# Datos -------------------------------------------------------------------
comentarios <- read.csv("comentariosFinancieras.csv", header = T, 
                        stringsAsFactors = F)

data <- read.csv("datosAnalisis.csv", header = T, stringsAsFactors = F)



# Análisis ----------------------------------------------------------------
comments <- comentarios %>%  
  data.table %>% 
  .[, created_time := gsub("T", " ", created_time)] %>% 
  .[, created_time := gsub("[+]\\S+", " ", created_time)] %>% 
  .[, created_time := as.POSIXct(created_time, format ="%Y-%m-%d %H:%M:%S")] %>% 
  .[, created_time := as.numeric(created_time)] %>% 
  .[, created_time := (created_time) -(6*3600)] %>% 
  .[, created_time := as.POSIXct(created_time, origin="1970-01-01")] %>% 
  rename(tiempoComentario = created_time) %>% 
  rename(idComentario = id) %>% 
  select(idComentario, idPosteo, tiempoComentario) 

posteos <- data %>%
  data.table %>% 
  .[, created_time := gsub("T", " ", created_time)] %>% 
  .[, created_time := gsub("[+]\\S+", " ", created_time)] %>% 
  .[, created_time := as.POSIXct(created_time, format ="%Y-%m-%d %H:%M:%S")] %>%
  .[, created_time := as.numeric(created_time)] %>% 
  .[, created_time := (created_time) -(6*3600)] %>% 
  .[, created_time := as.POSIXct(created_time, origin="1970-01-01")] %>% 
    rename(idPosteo = id) %>% 
    rename(tiempoPosteo = created_time) %>% 
  select(idPosteo, tiempoPosteo) 


unionComment <- left_join(comments, posteos, by="idPosteo") 
unionComment <- unionComment %>%  
  data.table %>% 
  .[, diferencia := difftime(tiempoComentario, tiempoPosteo)] %>% 
  .[, diferencia := as.numeric(diferencia)] %>%
  .[, diferenciaHora := diferencia/3600] %>%
  .[, diaComentario := weekdays(tiempoComentario)] %>%  
  .[, diaPosteo := weekdays(tiempoPosteo)] %>%  
  .[, horaComentario := hour(tiempoComentario)] %>%  
  .[, horaPosteo := hour(tiempoPosteo)] %>%  
  rename(diferenciaSeg = diferencia)

promedioHora <- unionComment %>% 
  select(idPosteo, tiempoPosteo, horaPosteo, diaPosteo) %>% 
  left_join(
    unionComment %>%  
      group_by(idPosteo) %>% 
      summarise(mediaHora  = mean(diferenciaHora , na.rm=T))
    
  ) %>% 
  mutate(diaPosteoFactor  = as.factor(diaPosteo))

# promedioHora$diaPosteo <-factor(promedioHora$diaPosteo, 
#                                 levels(promedioHora$diaPosteo))[c(3,4,5,2,7,6,1)]
  
promedioHora$diaPosteoFactor<- factor(promedioHora$diaPosteoFactor, 
       levels =c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))



unionComment %>%  
  group_by(idPosteo) %>% 
  summarise(mediaHora  = mean(diferenciaHora)) %>% 
  filter(mediaHora == max(mediaHora))

x11()
promedioHora %>% unique %>% 
  # filter(grepl("113144262054871", idPosteo)) %>%
  data.table %>% 
  # .[,horaPosteo := as.character(horaPosteo)] %>% 
  ggplot(aes(x =horaPosteo, y = mediaHora )) +
  geom_point()+
  facet_wrap(~diaPosteoFactor)


x11()
promedioHora %>% unique %>% 
  filter(grepl("113144262054871", idPosteo)) %>%
  filter(mediaHora < 500 & mediaHora >0 ) %>% 
  data.table %>% 
  # .[,horaPosteo := as.character(horaPosteo)] %>% 
  ggplot(aes(x =horaPosteo, y = mediaHora )) +
  geom_point()+
  facet_wrap(~diaPosteoFactor)+ theme_bw()

arbolesHora <- promedioHora %>%  
  filter(grepl("113144262054871", idPosteo)) %>%
  select(mediaHora, diaPosteo, horaPosteo) %>% 
  unique %>% 
  mutate(diaPosteo = as.factor(diaPosteo)) %>% 
  rename(promedioHora = mediaHora)

boxplot(mediaHora ~diaPosteo*horaPosteo, data=arbolesHora)

arbolesHora %>%  head

arbol <- tree(diaPosteo ~ ., data = as.data.frame(arbolesHora),
             control = tree.control(1000000, mindev = 0.0002))

x11()
plot(arbol, col="steelblue")
text(arbol, cex=0.6)


# arbol1 <- rpart(diaPosteo ~ ., data = as.data.frame(arbolesHora), 
#       control= rpart.control(minsplit=1000000, cp=0.00002, xval=10))

arbol1 <- rpart(diaPosteo ~ ., data = as.data.frame(arbolesHora), 
      control= rpart.control(10, cp = 0.002))

plot(arbol1)
text(arbol1)
#fancyRpartPlot(arbol1)
rpart.plot(arbol1, type = 0, box.palette = "Greens", cex = 0.8, extra=2)
