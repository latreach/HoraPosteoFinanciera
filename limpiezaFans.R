# Librerías ---------------------------------------------------------------
library(magrittr)
c("data.table", "dplyr", "tidyr", "reshape2") %>% 
  sapply(require, character.only=T)

fans <- read.csv("~/local/Sonia/HoraPosteoFinanciera/datos/fans.csv",
                 header = T)


fans <- lapply(fans, function(x){
  x = gsub("{u'end_time': u'", "", x, fixed = T)
  x = gsub("u'value': ", "", x, fixed = T)
  x = gsub("}", "", x, fixed = T)
  x = gsub("'", "", x, fixed = T)
}) %>%  do.call("cbind", .)

fans <- fans %>%  data.frame %>% 
  gather(valor, tiempo, -idPosteo) %>%  
  select(-valor) 

fans <- fans %>%  
  separate(tiempo, c("fecha", "dato"), sep=",") 

fans <- fans %>%  
  na.omit %>%  
  data.table %>% 
  .[, idPosteo := gsub("/insights/page_fans/lifetime","",
                       idPosteo, fixed=T)] %>% 
  .[, fecha := gsub("[T]\\S+","", fecha)] 

fans <- fans[with(fans, order(fecha))]

fans %>%  write.csv("~/local/Sonia/HoraPosteoFinanciera/datos/fansNuevo.csv",
                    row.names=F)
