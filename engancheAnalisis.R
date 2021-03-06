####################################
#                                                   ^ ^
# Creado por el Área de Data Science    <(°)      =(°-°)=         
# Fernando Dorantes Nieto                 ( >)"    (   )S  
#                                          /|       w w
#

# Librerías ---------------------------------------------------------------
library(magrittr)
c("data.table", "dplyr", "lubridate", "ggplot2", "lattice", "purrr",
  "Rfacebook", "betareg", "broom", "tidyr", "xts", "dygraphs") %>% 
  sapply(require, character.only=T)

# Conexión FB -------------------------------------------------------------
fb_oauth <- fbOAuth(app_id="1611650985792093", 
                    app_secret="85db5a49077d074e84b5ce0a19659893", 
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("~/fb_oauth")
idFB_seat <- 113144262054871
idFinancieraVW <- 282547551846127

ids <- c(113144262054871, 282547551846127)


##Parámetros fijos de las gráficas
#theme_set(theme_bw())


##Funciones 
capitaliza=  function(x){ 
  x = tolower(x); x = strsplit(x, " ");
  x = sapply(x, function(c){
    paste(
      toupper(substring(c, 1,1)),
      substring(c,2), sep="", collapse=" "
    )})
  return(x)
}

porcentaje <- function(x){
  y = paste(x, "%", sep="")
  return(y)
}

# Recolección de datos ----------------------------------------------------

# datosFB <- lapply(ids, function(x){
#   Y  = getPage(x, n= 1e5, since = "2016-01-01", until = "2017-05-24",
#           token = fb_oauth, reactions = T, feed = F)
#   return(Y)
# })
# 
# datosFB <- do.call("rbind", datosFB)
# datosFB %>%
#   data.frame %>%
#   write.csv("~/local/Sonia/HoraPosteoFinanciera/datos/datosEnganche.csv",
#             row.names=F)

datosFB <- read.csv("~/local/Sonia/HoraPosteoFinanciera/datos/datosEnganche.csv",
                    header = T)

fans <- read.csv("~/local/Sonia/HoraPosteoFinanciera/datos/fansNuevo.csv",
                 header = T)
fechasFiltro <- seq.POSIXt(from = as.POSIXct("2016-01-01"),
                     to= as.POSIXct("2017-06-01"), by="month") %>% 
  as.Date

fans <- fans %>% 
  mutate(fecha= as.Date(fecha)) %>% 
  filter(fecha %in% fechasFiltro) %>% 
  mutate(mes = month(fecha), anio= year(fecha)) %>% 
  rename(from_id = idPosteo)


dataFb <- datosFB %>% 
  data.table() %>% 
  .[, created_time := gsub("T", " ", created_time)] %>% 
  .[, created_time := gsub("[+]\\S+", " ", created_time)] %>% 
  .[, created_time := as.POSIXct(created_time, format ="%Y-%m-%d %H:%M:%S")] %>% 
  .[, created_time := as.numeric(created_time)] %>% 
  .[, created_time := (created_time) - (6*3600)] %>% 
  .[, created_time := as.POSIXct(created_time, origin="1970-01-01")] %>% 
  .[, totalInteraccion := rowSums(.[, c(2,10:16)])] %>%  
  separate(created_time, c("Fecha", "Hora"), sep=" ") %>% 
  select(id, from_id, Fecha, Hora, totalInteraccion, type) 
  
dataFb <- dataFb %>%  
  data.frame %>% 
  mutate(Fecha = as.Date(Fecha)) %>% 
  mutate(mes = month(Fecha), anio= year(Fecha))


dataFb <- merge(dataFb, fans, by=c("from_id", "mes", "anio")) 

dataFb <- dataFb %>%  
  data.table %>% 
  .[, enganche := totalInteraccion/dato ] %>%  
  # .[, enganche := enganche*100 ] %>%  
  .[, enganche := as.numeric(enganche)] %>% 
  .[, enganche := ifelse(enganche==0, 1e-12, enganche)] %>% 
  .[, Hora  := gsub("[:]\\S+", "", Hora)] %>% 
  .[, Cuenta := ifelse(from_id==113144262054871, "SEAT", "VWFS")] %>%
  .[, diaSeman := weekdays(Fecha)] %>% 
  .[, diaSeman := capitaliza(diaSeman)] %>% 
  select(-fecha)

  

# Test de Normalidad ------------------------------------------------------
dataFb %>% 
  group_by(Cuenta) %>% 
  do(test = shapiro.test(.$enganche)) %>% 
  .$test


SumaPosteos <- dataFb %>% 
  group_by(diaSeman, Cuenta, anio, type, Hora) %>% 
  summarise(enganche = sum(enganche)) %>% 
  left_join(dataFb %>% 
              group_by(diaSeman, Cuenta, anio, type, Hora) %>% 
              tally %>%  rename(NumPosteos=n)) 

# SumaPosteos1 <- dataFb %>% 
#   group_by(diaSeman, Cuenta, anio, mes, Hora) %>% 
#   summarise(enganche = sum(enganche)) %>% 
#   left_join(dataFb %>% 
#               group_by(diaSeman, Cuenta, anio, mes, Hora) %>% 
#               tally %>%  rename(NumPosteos=n))



modelos1 <- SumaPosteos %>% 
  split(.$Cuenta) %>% 
  map(~ betareg(enganche~NumPosteos+type+diaSeman, data=.))

modelos2 <- SumaPosteos %>% 
  split(.$Cuenta) %>% 
  map(~ glm(enganche~NumPosteos+Hora+diaSeman, data=.,
            family = quasibinomial(link="logit"))) 


modelos2 %>% 
  lapply(function(x){
    X = anova(x, test="Chisq")
    X  = X %>% 
      mutate(Porcentaje  = Deviance/head(`Resid. Dev`,1)*100) %>% 
      mutate(Nombre = c("Nulo", "Número Posteos",
                        "Hora", "DiaSemana"))
    Z = tail(X$`Resid. Dev`,1)/head(X$`Resid. Dev`,1)*100
    DF1 <- data.frame(X1 = "", X2= "", X3 = "", X4 = "", X5 = "", X6 = Z,
                      x7 = "Error")
    names(DF1)<- names(X)
    X <- rbind(X, DF1) %>% 
      select(Nombre, Deviance, Porcentaje, Df)
    
    return(X %>%  toLatex())
    

    })

modelos2$SEAT %>%  anova(test="Chisq")
# Gráficos ----------------------------------------------------------------
dias <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", 
          "Sábado")

dataFb$diaSeman <- factor(dataFb$diaSeman,levels = dias)


dataFb<- dataFb %>%  
  left_join(dataFb %>% 
              group_by(diaSeman, Cuenta) %>% 
              tally)

setwd("~/local/Sonia/HoraPosteoFinanciera/imagenes/")

png("engancheSeat.png", width = 1200, height = 800, res = 100)
dataFb %>%  
  filter(Cuenta=="SEAT") %>% 
  data.table %>% 
  .[, enganche := enganche*100] %>% 
  ggplot(aes(x= Hora, y= enganche)) +
  geom_boxplot(fill="steelblue")+ theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  facet_wrap(~diaSeman)+
  ylab("Enganche") + xlab("Hora")+
  geom_label(aes(x= 12, y=0.4, label=paste("Número de posteos:", n, sep=" ")))
  # ggtitle("Enganche de los posteos de SEAT \n por día y hora de publicación \n (2016-01-01/2017-05-15)")+
  # theme(plot.title = element_text(hjust=0.5))
dev.off()

png("engancheVwfs.png", width = 1200, height = 800, res = 100)
dataFb %>%  
  filter(Cuenta=="VWFS") %>% 
  data.table %>% 
  .[, enganche := enganche*100] %>% 
  ggplot(aes(x= Hora, y= enganche)) +
  geom_boxplot(fill="darkred") + theme_bw()+
  theme(strip.background = element_rect(fill="white"))+
  facet_wrap(~diaSeman)+
  ylab("Enganche") + xlab("Hora")+
  geom_label(aes(x= 12, y=12, label=paste("Número de posteos:", n, sep=" ")))
  # ggtitle("Enganche de los posteos de VWFS \n por día y hora de publicación")+
  # theme(plot.title = element_text(hjust=0.5))
dev.off()


promedioEnganche <- dataFb %>%
  group_by(Hora, diaSeman, Cuenta) %>% 
  summarise(promedioEnganche = mean(enganche))


promedioEnganche$diaSeman <- factor(promedioEnganche$diaSeman, levels = dias)

# coloresBrew = c('#ef8a62','#f7f7f7','#67a9cf')
# coloresBrew = c('white', '#a6cee3','#1f78b4','#b2df8a', "darkgreen")
coloresBrew = c( 'gray50','#a6cee3','#1f78b4','#b2df8a', "darkgreen")
png("figuraEnganche1.png",height = 800, width = 1000, res = 120)
promedioEnganche %>%  
  filter(Cuenta=="SEAT") %>% 
  mutate(promedioEnganche = promedioEnganche*100) %>%
  ggplot(aes(x = diaSeman, y =Hora))+
  geom_tile(aes(fill=promedioEnganche), col="black")+
  theme_classic()+
  scale_fill_gradientn(colours = coloresBrew,
                       breaks =seq(0, 0.4, by=0.05),
                       labels = porcentaje,
                       name="Enganche") +
  xlab("") + ylab("Hora")
dev.off()

png("figuraEnganche2.png",height = 800, width = 1000, res = 120)
promedioEnganche %>%  
  filter(Cuenta=="VWFS") %>% 
  mutate(promedioEnganche = promedioEnganche*100) %>% 
  ggplot(aes(x = diaSeman, y =Hora))+
  geom_tile(aes(fill=promedioEnganche), col="black")+
  theme_classic()+
  scale_fill_gradientn(colours = coloresBrew,
                       breaks = seq(0, 6, by=1),
                       labels = porcentaje,
                       name="Enganche")+
  xlab("")
dev.off()

test1 <- dataFb %>%  
  filter(Cuenta=="SEAT") %>% 
  select(Fecha, enganche)

test2 <- dataFb %>%  
  filter(Cuenta=="VWFS") %>% 
  select(Fecha, enganche)

test1 %>%  head

x11()
xts(test1[,-1], as.Date(test1[,1])) %>%  
  # diff(diff=7) %>%
  dygraph()
x11()
xts(test2[,-1], as.Date(test2[,1])) %>%  dygraph()

