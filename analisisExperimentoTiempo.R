setwd("/home/fer-datascientist/Documentos/Chris/Posteos/AnalisisFinanciera/datos")
post <- read.csv("datosAnalisis.csv",header=T)
head(post)
library(lubridate)

horaCorrecta <- function(hora){
  temp <- hora -6
  if(temp<0){
    regresa<-24+temp
    return(regresa)
  }else{
    return(temp)
  }
}

post <- post %>% 
  .[c(1,2,4,6,7,10:16)] %>%
  separate(created_time,c("Fecha","Hora"),sep="T") %>% 
  mutate(Fecha= as.Date(Fecha)) %>% 
  mutate(anio = year(Fecha)) %>% 
  filter(anio==2016) %>% 
  select(-anio) %>% 
  mutate(Fecha = weekdays(Fecha)) %>% 
  mutate(Hora = gsub(":\\S+","",Hora))  %>% 
  mutate(Hora= as.numeric(Hora)) %>% 
  mutate(Hora = sapply(Hora, horaCorrecta)) %>% 
  mutate(Hora = as.character(Hora))



head(post)

post$from_name %>%  unique
post %>% .[c(3,4,5,2,7:13)] %>% 
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



sumaPosteo <- post %>% .[c(3,4,5,2,7:13)] %>% 
  # filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Fecha,Hora) %>% 
  summarise_each(funs(sum)) %>%
  data.frame %>% 
  mutate(totales = rowSums(.[,3:7])) %>% 
  select(Fecha, Hora, totales) 


conteoPosteo <- post %>% .[c(3,4,5,2,7:13)] %>% 
  # filter(from_name=="SEAT México") %>%
  select(-from_name) %>% 
  group_by(Fecha,Hora) %>%  tally 

plot(conteoPosteo$n, sumaPosteo$totales, type="p")