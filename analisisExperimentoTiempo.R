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
library(lme4)
library(car)

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
  #filter(anio==2016) %>% 
  select(-anio) %>% 
  mutate(Fecha = weekdays(Fecha)) %>% 
  mutate(Hora = gsub(":\\S+","",Hora))  %>% 
  mutate(Hora= as.numeric(Hora)) %>% 
  mutate(Hora = sapply(Hora, horaCorrecta)) %>% 
  mutate(Hora = as.numeric(Hora))

# post <- post %>% 
#   mutate(Fecha =factor(post$Fecha, levels(post$Fecha)[c(1,3,4,5,2,7,6)]))


post %>% 
  select(-id, -type) %>% 
  #filter(from_name=="SEAT México") %>%
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

post %>% 
  select(-id) %>% 
  #filter(from_name=="SEAT México") %>%
  mutate(type = as.character(type)) %>% 
  select(-from_name,-Fecha) %>% 
  group_by(type,Hora) %>% 
  summarise_each(funs(sum)) %>% 
  gather(tipo, valor, -type, -Hora) %>% 
  #filter(tipo=="love_count") %>% 
  ggplot(aes(x=type,y=valor,fill=type)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3")
  #facet_wrap(~type,ncol=2,scales="free_y")
  # theme_classic() +
  # theme(axis.text.x = element_text(angle=45, hjust=1),
  #       legend.position = "")+
  # ylab("Número de interacciones")+
  # scale_y_continuous(labels = scales::comma)


sumaPosteo <- post %>%
  select(-id,-from_name) %>% 
  group_by(Fecha,Hora,type) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(4,7:11)])) %>% 
  select(Fecha, Hora, type, reaccionesTotales, comments_count, shares_count) 

conteoPosteo <- post %>%  
  select(-id) %>% 
  group_by(Fecha,Hora, type) %>%  tally 

SumConPosteo <- merge(sumaPosteo,conteoPosteo,by=c("Fecha","Hora","type"))

SumConPosteo <- SumConPosteo %>%
  gather(reaccion,valor,-n,-Fecha,-Hora,-type)

glm(SumConPosteo$valor~SumConPosteo$n+SumConPosteo$type*SumConPosteo$reaccion,poisson) %>% summary
glm(SumConPosteo$valor~SumConPosteo$n+SumConPosteo$type*SumConPosteo$reaccion,poisson) %>% anova(test="Chisq")


x11()
SumConPosteo %>%
  ggplot(aes(x=n,y=valor,col=reaccion)) +
  geom_point() +
  facet_wrap(~type,ncol=2,scales=c("free")) +
  scale_fill_brewer(palette = "Set3") +
  theme_gray() +
  theme(axis.text.x = element_text(angle=0, hjust=0.5),
        legend.position = "top")+
  ylab("Número de Reacciones")+
  xlab("Número de posteos") +
  scale_y_continuous(labels = scales::comma)
  
### ******** Número de reacciones en función de número de posteos **************
sumaPosteo <- post %>%
  select(-id) %>% 
  group_by(Fecha,Hora,type,from_name) %>% 
  summarise_each(funs(sum(.,na.rm=T))) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(5,8:12)])) %>% 
  select(Fecha, Hora, type,from_name, 
         reaccionesTotales, 
         comments_count, shares_count) 


conteoPosteo <- post %>%  
  select(-id) %>% 
  group_by(Fecha,Hora, type,from_name) %>%  tally 

SumConPosteo <- merge(sumaPosteo,conteoPosteo,
                      by=c("Fecha","Hora","type","from_name"))

SumConPosteo <- SumConPosteo %>%
  gather(reaccion,valor,-n,-Fecha,-Hora,-type,-from_name)

glm(valor~n*reaccion*from_name,poisson, SumConPosteo) %>% summary
glm(valor~n*reaccion*from_name,poisson, SumConPosteo) %>% summary

class(SumConPosteo$valor)
glmer(valor~n*reaccion+(1|from_name), 
      data= SumConPosteo, 
      family = "poisson") %>%
  summary

glmer(valor~n*reaccion+(1|from_name), 
      data= SumConPosteo, 
      family = "poisson") %>%
  Anova(test="Chisq")
