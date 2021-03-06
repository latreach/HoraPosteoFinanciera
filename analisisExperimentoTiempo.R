####################################
#                                                   ^ ^
# Creado por el Área de Data Science    <(°)      =(°-°)=         
# Fernando Dorantes Nieto                 ( >)"    (   )S  
#                                          /|       w w
# Christian Daniel Morán Titla

# Librerías ---------------------------------------------------------------
library(magrittr)
c( "tidyr", "ggplot2", "lubridate", "bipartite",
  "lubridate", "lme4", "car", "lattice", "vegan", 
  "memisc", "dplyr", "lattice", "nlme") %>% 
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

# Función capitalizar
capitaliza=  function(x){
  x = tolower(x); x = strsplit(x, " ");
  x = sapply(x, function(c){
    paste(
      toupper(substring(c, 1,1)),
      substring(c,2), sep="", collapse=" "
    )})
  return(x)
}


### ************************ Análisis exploratorio *********************** ###

# Utilizar sólo columnas designadas a posteos y al id, cuenta, dia
# y hora de posteos
post <- post %>%
  select(id, likes_count, from_name, created_time, type, 
         comments_count, shares_count, love_count, haha_count, 
         wow_count, sad_count, angry_count) %>% 
  separate(created_time,c("Dia","Hora"),sep="T") %>% 
  mutate(Dia= as.Date(Dia)) %>% 
  mutate(anio = year(Dia)) %>% 
  # filter(type=="offer") %>%
  select(-anio) %>% 
  mutate(Dia = weekdays(Dia)) %>% 
  mutate(Dia = capitaliza(Dia)) %>% 
  mutate(Hora = gsub(":\\S+","",Hora))  %>% 
  mutate(Hora= as.numeric(Hora)) %>% 
  mutate(Hora = sapply(Hora, horaCorrecta)) %>% 
  mutate(Hora = as.numeric(Hora)) %>% 
  filter(type!="offer")
  


# Sumas de reacciones en función del Día, Hora y tipo de posteo
sumaPosteo <- post %>%
  select(-id,-from_name) %>% 
  group_by(Dia,Hora,type) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(4,7:11)])) %>% 
  select(Dia, Hora, type, reaccionesTotales, comments_count, shares_count) 

# Número de posteos en función del Día, Hora y tipo de posteo
conteoPosteo <- post %>%  
  select(-id) %>% 
  group_by(Dia, Hora, type) %>%  tally 

post %>% 
  mutate(reaccionesTotales = rowSums(.[, c(2,7:13)])) %>% 
  group_by(from_name) %>%  
  summarise(reaccionesTotales = sum(reaccionesTotales,na.rm = T)) %>% 
  data.frame %>% 
  toLatex()


post %>%
  select(from_name)
  group_by(from_name) %>%  tally %>% 
  mutate(n = floor(n)) %>% 
  mutate(n = round(n)) %>% 
  data.frame %>% 
  toLatex()


# Unir tabla de sumas de reacciones a número de posteos
SumConPosteo <- merge(sumaPosteo,conteoPosteo,by=c("Dia","Hora","type"))


# Tabla de suma de reacciones y número de posteos
SumConPosteo <- SumConPosteo %>%
  gather(reaccion,valor,-n,-Dia,-Hora,-type)

# Relación entre suma de reacciones en función del número de posteos por tipo
# y reacción
glm(valor~n+type*reaccion,poisson,data=SumConPosteo) %>% summary
glm(valor~n+type*reaccion,poisson,data=SumConPosteo) %>%
  anova(test="Chisq") %>% 
  mutate(Porc = Deviance/head(`Resid. Dev`,1)*100)

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
  
  # Existe un efecto del número de posteos

# Hay una dependencia positiva de la suma de reacciones en función del 
# número de posteos con un 41 % de explicación, el tipo de posteo
# también es significativo con un 4 % de explicación,
# la reacción también es significativamente distinto con un 41 % de 
# explicación.


### **** Número de reacciones en función del número de posteos por cuenta ***
# Suma de reacciones
sumaPosteoCuenta <- post %>%
  select(-id) %>% 
  group_by(Dia,Hora,type,from_name) %>% 
  summarise_each(funs(sum(.,na.rm=T))) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(5,8:12)])) %>% 
  select(Dia, Hora, type,from_name, 
         reaccionesTotales, 
         comments_count, shares_count)


# Número de posteos
conteoPosteoCuenta <- post %>%  
  select(-id) %>% 
  group_by(Dia,Hora, type,from_name) %>%  tally 

# Unir la tabla de suma de reacciones a número de posteos
SumConPosteoCuenta <- merge(sumaPosteoCuenta,conteoPosteoCuenta,
                      by=c("Dia","Hora","type","from_name"))

# Agrupar reacciones y número de posteos en función de Día, Hora, 
# tipo reacción y cuenta
SumConPosteoCuenta <- SumConPosteoCuenta %>%
  gather(reaccion,valor,-n,-Dia,-Hora,-type,-from_name) %>% 
  mutate(Hora = as.character(Hora))


# Relación entre suma de posteos en función del número de posteos por cuenta
glm(valor~n*reaccion*from_name,poisson, SumConPosteoCuenta) %>%
  summary


glm(valor~n*reaccion*from_name,poisson, SumConPosteoCuenta) %>%
  anova(test="Chisq") %>% 
  mutate(Porc = Deviance/head(`Resid. Dev`,1)*100)



# Cuenta como efecto aleatorio
glmer(valor~n*reaccion+(1|from_name), 
      data= SumConPosteoCuenta, 
      family = "poisson") %>%
  summary

glmer(valor~n*reaccion+(1|from_name), 
      data= SumConPosteoCuenta, 
      family = "poisson") %>%
  summary()




# SumConPosteoCuenta %>%
#   # filter(from_name=="SEAT México") %>%
#   ggplot(aes(x=n,y=valor,col=reaccion)) +
#   geom_point() +
#   facet_wrap(~from_name,ncol=2,scales=c("free")) +
#   scale_fill_brewer(palette = "Set3") +
#   theme_gray() +
#   theme(axis.text.x = element_text(angle=0, hjust=0.5),
#         legend.position = "top")+
#   ylab("Número de Reacciones")+
#   xlab("Número de posteos") +
#   scale_y_continuous(labels = scales::comma)


### *********************** Mejor hora y día de posteo ********************* ###
# Tabla de tipos de reacción por Día, Hora y tipo de contenido
post2 <- post %>%
  select(-id,-from_name) %>% 
  group_by(Dia,Hora,type) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>%
  mutate(reaccionesTotales = rowSums(.[,c(4,7:11)])) %>% 
  .[c(-4,-7:-11)]


# Ordenar los días de domingo a sábado
post2 <- post2 %>% 
  mutate(Dia =factor(post2$Dia, 
                     levels(droplevels.factor(post2$Dia))[c(1,3,4,5,2,7,6)]))  


# Anidamiento
post2 %>% select(-type) %>%
  group_by(Dia, Hora) %>%
  summarise_each(funs(sum(., na.rm=T))) %>% .[c(-1,-2)] %>%
  # data.frame %>% 
  nested(method="NODF2")


# Mapa de calor Interacciones Totales
# coloresBrew = c('#ece2f0','#a6bddb','#1c9099' )
coloresBrew = c('#ef8a62','#f7f7f7','#67a9cf')
coloresBrew = c('gray50','#a6cee3','#1f78b4','#b2df8a', "darkgreen")
setwd("~/local/Sonia/HoraPosteoFinanciera/imagenes/")
png("figura5.png",height = 800, width = 1000, res = 120)
post2 %>%
  select(-type) %>%
  group_by(Dia, Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones, Reacciones, -Dia, -Hora) %>%
  ggplot(aes(Dia,Hora))+
  xlab("") +
  geom_tile(aes(fill = Reacciones), colour = "black") + 
  scale_fill_gradientn(colours = coloresBrew,
                       labels =scales::comma,
                       breaks = seq(0, 1.5e6, by=2e5),
                       name="Interacciones \n Totales")+
  # scale_fill_gradient2(name = "Interacciones \n Totales",
  #                      low =  "white",
  #                      high = "steelblue",
  #                      mid = "white",
  #                      labels =scales::comma,
  #                      breaks = seq(0, 1.5e6, by=2e5)) +
  scale_y_continuous(limits = c(0,23),
                     breaks = seq(0,23, by = 1)) +
  theme_classic()
dev.off()


# Mapa de calor por Reacciones
png("figura6.png",height = 800, width = 1000, res = 120)
post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones,Reacciones,-Dia,-Hora) %>% 
  filter(interacciones == "reaccionesTotales") %>% 
  ggplot(aes(Dia,Hora))+
  xlab("") +
  geom_tile(aes(fill = Reacciones),colour = "black") + 
  scale_fill_gradientn(name="Reacciones",
                       colours = coloresBrew,
                       labels =scales::comma,
                       breaks = seq(0, 1.5e6, by=2e5))+
  # scale_fill_gradient(name = "Reacciones",
  #                     low = "white", 
  #                     high = "steelblue", 
  #                     labels =scales::comma,
  #                     breaks = seq(0, 1.5e6, by=2e5)) +
  scale_y_continuous(limits = c(0,23),
                     breaks = seq(0,23, by = 1)) +
  theme_classic()
dev.off()


# Mapa de calor por Comentarios
png("figura7.png",height = 800, width = 1000, res = 120)
post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones ,Reacciones, -Dia, -Hora) %>% 
  filter(interacciones == "comments_count") %>% 
  ggplot(aes(Dia, Hora)) +
  xlab("") +
  geom_tile(aes(fill = Reacciones), colour = "black") + 
  scale_fill_gradientn(name="Comentarios",
                       colours = coloresBrew,
                       labels =scales::comma)+
  # scale_fill_gradient(name = "Comentarios",
  #                     low = "white", 
  #                     high = "steelblue", 
  #                     labels =scales::comma) +
  scale_y_continuous(limits = c(0,23),
                     breaks = seq(0,23, by = 1)) +
  theme_classic()
dev.off()


# Mapa de calor por Shares
png("figura8.png",height = 800, width = 1000, res = 120)
post2 %>% select(-type) %>%
  group_by(Dia,Hora) %>%
  summarise_each(funs(sum(.,na.rm=T))) %>%
  gather(interacciones,Reacciones,-Dia,-Hora) %>% 
  filter(interacciones == "shares_count") %>% 
  ggplot(aes(Dia,Hora)) +
  xlab("") +
  geom_tile(aes(fill = Reacciones),colour = "black") + 
  scale_fill_gradientn(name="Número de veces \n compartido",
                       colours = coloresBrew,
                       labels =scales::comma,
                       breaks = seq(0, 3e5, by=5e4))+
  # scale_fill_gradient(name = "Número de veces \n compartido",
  #                     low = "white", 
  #                     high = "steelblue", 
  #                     labels =scales::comma,
  #                     breaks = seq(0,3e5,5e4)) +
  scale_y_continuous(limits = c(0,23),
                     breaks = seq(0,23, by = 1)) +
  theme_classic()
dev.off()


# Red de interacciones bipartita por hora  
post2 %>% select(-Dia,-type) %>%
  group_by(Hora) %>%
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


# Red de interacciones bipartita por día
# post2 %>% select(-Hora,-type) %>%
#   group_by(Dia) %>%
#   summarise_each(funs(sum(.,na.rm=T))) %>% .[-1] %>%
#   plotweb(method="normal",
#           text.rot=90,
#           low.lablength=7,
#           high.lablength=5,
#           col.high =  "white",
#           col.low="lavender",
#           col.interaction="black",
#           bor.col.interaction="green4",
#           y.width.low=.07,
#           y.width.high=.07)



### **************** Comparar tipo de contenido ****************** ###
glm(valor~type+Hora*Dia,poisson, SumConPosteoCuenta) %>% summary
modelo <- glm(valor~type+Hora*Dia,poisson, SumConPosteoCuenta) %>%
  anova(test="Chisq")


# Modelos -----------------------------------------------------------------
# Tabla de devianza
modelo %>%
  mutate(Porc = Deviance/head(`Resid. Dev`,1)*100)

# Error
tail(modelo$`Resid. Dev`,1)/head(modelo$`Resid. Dev`,1)*100

### Mixto
modeloMixto1 <- glmer(valor~type+Hora*Dia+(1|from_name),
                family="poisson", SumConPosteoCuenta)

modeloMixto1 <- Anova(modeloMixto1, type = 2)


toLatex(modeloMixto1)


SumConPosteo <- SumConPosteo %>% 
  mutate(Dia =factor(SumConPosteo$Dia, 
                     levels(droplevels.factor(SumConPosteo$Dia))[c(1,3,4,5,2,7,6)]))



## Gráfica para comparación de tipo de posteo
png("../imagenes/fig8.png",width = 1000, height = 800, res = 110)
quilava <- pokepal(pokemon = "quilava", spread=12)[1:12]
SumConPosteo %>%
  filter(! type %in% c("status", "event")) %>% 
  mutate(type=ifelse(type=="link", "Gif", 
                     ifelse(type=="photo", "Foto", "Video"))) %>% 
  ggplot(aes(x=type,y=valor,fill=Dia)) +
  geom_boxplot(col="black") +
  theme_classic() +
  ylab("Número de Interacciones")+
  xlab("Tipo de posteo") +
  theme(legend.position="top")+
  scale_y_continuous(labels = scales::comma)+
  scale_color_manual(values = quilava, name="Día")
dev.off()

# SumConPosteo %>%
#   ggplot(aes(x=n,y=valor,col=reaccion)) +
#   geom_point() +
#   facet_wrap(~type,ncol=2,scales=c("free")) +
#   scale_fill_brewer(palette = "Set3") +
#   theme_gray() +
#   theme(axis.text.x = element_text(angle=0, hjust=0.5),
#         legend.position = "top")+
#   ylab("Número de Reacciones")+
#   xlab("Número de posteos") +
#   scale_y_continuous(labels = scales::comma)

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
  mutate(Porc = Deviance/head(`Resid. Dev`,1)*100)


SumConPosteoSC %>% 
  ggplot(aes(x=Dia,y=valor,col=Dia)) +
  geom_boxplot() +
  facet_wrap(~type,ncol=2) +
  scale_fill_brewer(palette = "Set3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "")+
  ylab("Número de interacciones")+
  scale_y_continuous(labels = scales::comma)

### ****************** Rangos de abundancia ************************** ###

rangos <- post2 %>%
  select(-type) %>% 
  mutate(DH = paste(Dia, paste(Hora, "Hr", sep=""),  sep="_")) %>%
  select(-Dia, -Hora) %>%
  gather(tipo, valor, -DH)
  
  
rangos <- rangos %>% 
  group_by(tipo) %>% 
  mutate(rango = row_number(-valor))


# Rangos por Comentarios
png("../imagenes/fig5.png",height = 800, width = 1200, res = 120)
rangos %>%
  filter(tipo=="comments_count") %>%
  filter(rango<21) %>% 
  ggplot(aes(x= rango, y= valor ))+
  xlab("Rango") +
  ylab("Número de Comentarios") +
  geom_point(color="steelblue", size=4) +
  geom_line(color="darkred")+
  theme_classic()+
  geom_text(vjust =1, hjust=-.2, angle = 45, 
            aes(x =rango, y = valor, label=DH)) +
  scale_y_continuous(limits = c(10000,39000),
                     labels = scales::comma,
                     breaks = seq(1e4,4e4, by = .5e4)) +
  scale_x_continuous(limits = c(1,21),breaks = 1:21)
dev.off()


# Rangos por veces compartido
png("../imagenes/fig6.png",height = 800, width = 1300, res = 120)
rangos %>%
  filter(tipo=="shares_count") %>%
  filter(rango<21) %>% 
  ggplot(aes(x= rango, y= valor ))+
  xlab("Rango") +
  ylab("Número de Posteos Compartidos") +
  geom_point(color="steelblue", size=4) +
  geom_line(color="darkred")+
  theme_classic()+
  geom_text(vjust =1, hjust=-.2, angle = 45, 
            aes(x =rango, y = valor, label=DH)) +
  scale_y_continuous(limits = c(44000,212000),
                     labels = scales::comma,
                     breaks = seq(44e3,21e4, by = .2e5)) +
  scale_x_continuous(limits = c(1,21),breaks = 1:21)
dev.off()



# Rangos por Reacciones Totales
png("../imagenes/fig7.png",height = 800, width = 1400, res = 120)
rangos %>%
  filter(tipo=="reaccionesTotales") %>%
  filter(rango<21) %>% 
  ggplot(aes(x= rango, y= valor )) +
  xlab("Rango") +
  ylab("Número de Reacciones Totales") +
  geom_point(color="steelblue", size=4) +
  geom_line(color="darkred") +
  theme_classic()+
  geom_text(vjust =1, hjust=-.2, angle = 45, 
            aes(x =rango, y = valor, label=DH)) +
  scale_y_continuous(limits = c(4.8e5,12.4e5),
                     labels = scales::comma,
                     breaks = seq(4.8e5,12.2e5, by = 5.5e4)) +
  scale_x_continuous(limits = c(1,21),breaks = 1:21)
dev.off()


#  Análisis ---------------------------------------------------------------
Posteos <- post %>%
  data.table %>% 
  .[, totales := rowSums(.[, c(2,7:13)])] %>%  
  .[, reaccionesTotales := rowSums(.[, c(2, 9:13)])] %>%
  select(from_name, Dia, Hora, type, comments_count, 
         shares_count, reaccionesTotales, totales) 

mixto1 <- glmer(totales~type+Dia*Hora+(1|from_name),
      data= Posteos, family = "poisson") 

mixto2 <- glmer(reaccionesTotales~type+Dia*Hora+(1|from_name),
      data= Posteos, family = "poisson") 

mixto3 <- glmer(comments_count~type+Dia*Hora+(1|from_name),
      data= Posteos, family = "poisson") 

mixto4 <- glmer(shares_count~type+Dia*Hora+(1|from_name),
      data= Posteos, family = "poisson") 

mixto1 %>% summary %>%  .[8]
tidy(augment(mixto1))

tidy(mixto4) %>% 
  mutate(group = ifelse(group=="fixed", "Fijo", "Aleatorio")) %>% 
  select(-statistic) %>% 
  mutate(p.value = ifelse(p.value<0.01, "< 0.001", p.value)) %>% 
  mutate(term = gsub("Dia", "Día ", term)) %>% 
  toLatex()
  


Posteos %>%  
  filter(!type %in% c("status", "event") ) %>% 
  ggplot(aes(x = Dia, y = comments_count, fill=type))+
  geom_violin()+
  facet_wrap(~from_name*type, scales="free_y") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))

Posteos %>%  
  filter(!type %in% c("status", "event") ) %>% 
  ggplot(aes(x = Dia, y = reaccionesTotales, fill=type))+
  geom_violin()+
  facet_wrap(~from_name*type, scales="free_y") +
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))
  








