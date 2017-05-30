####################################   
#Creado por Área de Data Science    <(°) 
# Fernando Dorantes Nieto             ( >)"
# Christian Daniel Morán Titla         /|
####################################

# Librerías ---------------------------------------------------------------
library(magrittr)
c("dplyr", "tidyr", "ggplot2", "lubridate", "bipartite",
  "lubridate", "lme4", "car", "lattice") %>% 
  sapply(require, character.only=T)

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
  separate(created_time,c("Dia","Hora"),sep="T") %>% 
  mutate(Dia= as.Date(Dia)) %>% 
  mutate(anio = year(Dia)) %>% 
  #filter(anio==2016) %>% 
  select(-anio) %>% 
  mutate(Dia = weekdays(Dia)) %>% 
  mutate(Hora = gsub(":\\S+","",Hora))  %>% 
  mutate(Hora= as.numeric(Hora)) %>% 
  mutate(Hora = sapply(Hora, horaCorrecta)) %>% 
  mutate(Hora = as.numeric(Hora))

# post <- post %>% 
#   mutate(Dia =factor(post$Dia, levels(post$Dia)[c(1,3,4,5,2,7,6)]))


post %>% 
  select(-id, -type) %>% 
  #filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Dia,Hora) %>% 
  summarise_each(funs(sum)) %>% 
  gather(tipo, valor, -Dia, -Hora) %>% 
  filter(tipo=="love_count") %>% 
  ggplot(aes(x=Hora,y=valor,fill=Dia)) +
  geom_bar(stat = "identity",colour="black") +
  facet_wrap(~Dia,ncol=2) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "")+
  ylab("Número de interacciones")+
  scale_y_continuous(labels = scales::comma)


sumaPosteo <- post %>%
  select(-id,-from_name) %>% 
  group_by(Dia,Hora,type) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(4,7:11)])) %>% 
  select(Dia, Hora, type, reaccionesTotales, comments_count, shares_count) 

conteoPosteo <- post %>%  
  select(-id) %>% 
  group_by(Dia, Hora, type) %>%  tally 

SumConPosteo <- merge(sumaPosteo,conteoPosteo,by=c("Dia","Hora","type"))

SumConPosteo <- SumConPosteo %>%
  gather(reaccion,valor,-n,-Dia,-Hora,-type)

glm(valor~n+type*reaccion,poisson,data=SumConPosteo) %>% summary
glm(valor~n+type*reaccion,poisson,data=SumConPosteo) %>% anova(test="Chisq")

# x11()
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
sumaPosteoCuenta <- post %>%
  select(-id) %>% 
  group_by(Dia,Hora,type,from_name) %>% 
  summarise_each(funs(sum(.,na.rm=T))) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(5,8:12)])) %>% 
  select(Dia, Hora, type,from_name, 
         reaccionesTotales, 
         comments_count, shares_count)


conteoPosteoCuenta <- post %>%  
  select(-id) %>% 
  group_by(Dia,Hora, type,from_name) %>%  tally 

SumConPosteoCuenta <- merge(sumaPosteoCuenta,conteoPosteoCuenta,
                      by=c("Dia","Hora","type","from_name"))

SumConPosteoCuenta <- SumConPosteoCuenta %>%
  gather(reaccion,valor,-n,-Dia,-Hora,-type,-from_name)

glm(valor~n*reaccion*from_name,poisson, SumConPosteoCuenta) %>%
  summary

glm(valor~n*reaccion*from_name,poisson, SumConPosteoCuenta) %>%
  anova(test="Chisq")

class(SumConPosteoCuenta$valor)

glmer(valor~n*reaccion+(1|from_name), 
      data= SumConPosteoCuenta, 
      family = "poisson") %>%
  summary

glmer(valor~n*reaccion+(1|from_name), 
      data= SumConPosteoCuenta, 
      family = "poisson") %>%
  Anova(test="Chisq")


SumConPosteoCuenta %>%
  # filter(from_name=="SEAT México") %>% 
  ggplot(aes(x=n,y=valor,col=reaccion)) +
  geom_point() +
  facet_wrap(~from_name,ncol=2,scales=c("free")) +
  scale_fill_brewer(palette = "Set3") +
  theme_gray() +
  theme(axis.text.x = element_text(angle=0, hjust=0.5),
        legend.position = "top")+
  ylab("Número de Reacciones")+
  xlab("Número de posteos") +
  scale_y_continuous(labels = scales::comma)

### **************** Comparar tipo de contenido ****************** ###
glm(valor~type+Hora*Dia,poisson, SumConPosteoCuenta) %>% summary
modelo <- glm(valor~type+Hora*Dia,poisson, SumConPosteoCuenta) %>%
  anova(test="Chisq")

modelo %>%
  rename(RDev = `Resid. Dev`) %>% 
  mutate(Porc = Deviance/head(RDev,1)*100)

SumConPosteo %>%
  mutate(Hora = as.character(Hora)) %>% 
  ggplot(aes(x=Hora,y=valor,col=Hora)) +
  geom_boxplot()
  # geom_point() +
  # facet_wrap(~type,ncol=2,scales=c("free")) +
  # scale_fill_brewer(palette = "Set3") +
  # theme_gray() +
  # theme(axis.text.x = element_text(angle=0, hjust=0.5),
  #       legend.position = "top")+
  # ylab("Número de Reacciones")+
  # xlab("Número de posteos") +
  # scale_y_continuous(labels = scales::comma)

post %>% 
  select(-id, -type) %>% 
  #filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Dia,Hora) %>% 
  summarise_each(funs(sum)) %>% 
  gather(tipo, valor, -Dia, -Hora) %>% 
  # filter(tipo=="love_count") %>% 
  ggplot(aes(x=Hora,y=valor,fill=Dia)) +
  geom_bar(stat = "identity",colour="black") +
  facet_wrap(~Dia,ncol=2) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "")+
  ylab("Número de interacciones")+
  scale_y_continuous(labels = scales::comma)

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

### ********* Mejor hora y día de posteo ******** ### ## Pendiente
post2 <- post %>%
  select(-id,-from_name) %>% 
  group_by(Dia,Hora,type) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(4,7:11)])) %>% 
  .[c(-4,-7:-11)]

post2 <- post2 %>% 
  mutate(Dia =factor(post2$Dia, 
                     levels(droplevels.factor(post2$Dia))[c(1,3,4,5,2,7,6)]))  
# post2 %>% select(-Dia,-type) %>%
  # mutate(Hora = as.character(Hora)) %>%
  # group_by(Dia) %>%
  # group_by(Hora) %>%
  # group_by(type) %>% 
  # summarise_each(funs(sum(.,na.rm=T))) %>% 
  # select(-Hora) %>% t %>% data.frame %>%
  # prcomp %>% biplot %>% abline(v=0,h=0,lty=2)


post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>% .[c(-1,-2)] %>%
  # data.frame %>% 
  nested(method="NODF2")


# post2 %>% select(-Hora,-type) %>%
#   group_by(Dia) %>%
#   summarise_each(funs(sum(.,na.rm=T))) %>%
#   gather(interacciones,valor,-Dia) %>% 
#   ggplot(aes(Dia,interacciones))+ 
#   geom_tile(aes(fill = valor),colour = "white") + 
#   scale_fill_gradient(low = "white",high = "steelblue")


# Mapa de calor total
post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones,Reacciones,-Dia,-Hora) %>% 
  ggplot(aes(Dia,Hora))+
  geom_tile(aes(fill = Reacciones),colour = "gray") + 
  scale_fill_gradient(low = "white",high = "red") +
  theme_classic() +
  ggtitle("Total")
  


# Mapa de calor por Reacciones Totales
post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones,Reacciones,-Dia,-Hora) %>% 
  filter(interacciones == "reaccionesTotales") %>% 
  ggplot(aes(Dia,Hora))+ 
  geom_tile(aes(fill = Reacciones),colour = "gray") + 
  scale_fill_gradient(low = "white", high = "red", 
                      name="Reacciones \n totales") +
  theme_classic()

# Mapa de calor por Comentarios

post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones ,Reacciones, -Dia, -Hora) %>% 
  filter(interacciones == "comments_count") %>% 
  ggplot(aes(Dia, Hora))+ 
  geom_tile(aes(fill = Reacciones), colour = "gray") + 
  scale_fill_gradient(low = "white", high = "red", 
                      name="Número de \n comentarios") +
  theme_classic()

# Mapa de calor por Shares
post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones,Reacciones,-Dia,-Hora) %>% 
  filter(interacciones == "shares_count") %>% 
  ggplot(aes(Dia,Hora))+ 
  geom_tile(aes(fill = Reacciones),colour = "gray") + 
  scale_fill_gradient(low = "white", high = "red", 
                      name="Número de \n veces compartido", 
                      formatter=labels::comma) +
  theme_classic()
  
  
post2 %>% select(-Dia,-type) %>%
# post %>% select(-id,-from_name) %>% 
  # group_by(Dia,Hora,type) %>%
  # mutate(Hora = as.numeric(Hora)) %>%
  # group_by(Dia) %>%
  group_by(Hora) %>%
  # group_by(type) %>% 
  summarise_each(funs(sum(.,na.rm=T))) %>% .[-1] %>%
  plotweb(method="normal",
          text.rot=90,
          low.lablength=7,
          high.lablength=5,
          col.high =  "white",
          col.low="lavender",
          col.interaction="black",
          bor.col.interaction="green4",
          y.width.low=.07,
          y.width.high=.07)


  post2 %>% select(-Hora,-type) %>%
  # mutate(Hora = as.numeric(Hora)) %>%
  group_by(Dia) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>% .[-1] %>%
  plotweb(method="normal",
          text.rot=90,
          low.lablength=7,
          high.lablength=5,
          col.high =  "white",
          col.low="lavender",
          col.interaction="black",
          bor.col.interaction="green4",
          y.width.low=.07,
          y.width.high=.07)
  

post2 %>% select(-Dia,-type) %>%
  mutate(Hora = as.numeric(Hora)) %>%
  group_by(Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>% .[-1] %>%
  graph.incidence %>%
  plot(vertex.label=NA, vertex.size=7, layout=layout.bipartite)
  


### **************** Comparar tipo de contenido ****************** ###
### ********************* Sin comentarios ************************ ###
SumConPosteoSC <- post %>% 
  select(-id,-from_name,-comments_count) %>% 
  #filter(from_name=="SEAT México") %>%
  group_by(Dia,Hora,type) %>% 
  summarise_each(funs(sum)) %>% 
  gather(reaccion, valor, -Dia, -Hora,-type)

glm(valor~type+Hora*Dia,poisson, SumConPosteoSC) %>% summary
modelo2 <- glm(valor~type+Hora*Dia,poisson, SumConPosteoSC) %>%
  anova(test="Chisq")

modelo2 %>%
  rename(RDev = `Resid. Dev`) %>% 
  mutate(Porc = Deviance/head(RDev,1)*100)


SumConPosteoSC %>% 
  ggplot(aes(x=Hora,y=valor,fill=Dia)) +
  geom_bar(stat = "identity",colour="black") +
  facet_wrap(~Dia,ncol=2) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "")+
  ylab("Número de interacciones")+
  scale_y_continuous(labels = scales::comma)

