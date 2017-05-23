####################################   
#Creado por Área de Data Science    <(°) 
# Fernando Dorantes Nieto             ( >)"
# Christian Daniel Morán Titla         /|
# Heloel Hernández Santos
####################################

# Librerías ---------------------------------------------------------------
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

##Sembrando directorio
setwd("~/local/Sonia/HoraPosteoFinanciera/datos/")

##Cargando datos
post <- read.csv("datosAnalisis.csv", header=T)

##Funciones
horaCorrecta <- function(hora){
  X <- hora - 6
  if(X<0){
    Y <- 24 + X
    return(Y)
  }else{
    return(X)
  }
}

# Análisis ----------------------------------------------------------------

post <- post %>%
  select(id, likes_count, from_name, created_time, type, 
         comments_count, shares_count, love_count, haha_count, 
         wow_count, sad_count, angry_count) %>% 
  separate(created_time,c("Fecha","Hora"),sep="T") %>% 
  mutate(Fecha= as.Date(Fecha)) %>% 
  mutate(anio = year(Fecha)) %>% 
  filter(anio==2016) %>% 
  select(-anio) %>% 
  mutate(Fecha = weekdays(Fecha)) %>% 
  mutate(Hora = gsub(":\\S+","",Hora))  %>% 
  mutate(Hora= as.numeric(Hora)) %>% 
  mutate(Hora = sapply(Hora, function(x){horaZona(x,6)})) %>% 
  mutate(Hora = as.character(Hora))



post %>% 
  select(-id, -type) %>% 
  # filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Fecha,Hora) %>% 
  summarise_each(funs(sum)) %>% 
  gather(tipo, valor, -Fecha, -Hora) %>% 
  filter(tipo=="love_count") %>% 
  ggplot(aes(x=Hora,y=valor,fill=Fecha)) +
  geom_bar(stat = "identity",colour="black") +
  facet_wrap(~Fecha,ncol=2) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "")+
  ylab("Número de interacciones")+
  scale_y_continuous(labels = scales::comma)



sumaPosteo <- post %>%
  select(-id, -type) %>% 
  # filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Fecha,Hora) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>% 
  mutate(totales = rowSums(.[,3:7])) %>% 
  select(Fecha, Hora, totales) 


conteoPosteo <- post %>%  
  select(-id, -type) %>% 
  # filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Fecha,Hora) %>%  tally 

