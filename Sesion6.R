library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(scales)
library(ggthemes)
library(prettydoc)

# Link base de datos ILE CDMX
# https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/table/
# Favor de descargar la base en csv de preferencia.


ile <- read.csv("interrupcion-legal-del-embarazo.csv")
View(ile)
glimpse(ile)

#Primera variable "Año" tiene un error en el nombre
unique(ile$aÃ.o)


#Renombrar la variable de año:
names(ile)[names(ile) == "aÃ.o"] <- "año" 


ile %>% 
  select(año, entidad, ocupacion) %>% 
  filter(año != "NA") %>% 
  group_by(año) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(x=año, y=Total, fill=año))+
  geom_col(position="dodge", fill=c("plum4", "seagreen3", "slateblue", "lightblue4", "salmon1"))+ 
  ylim(c(0,20000))+ 
  geom_text (aes(x=año, y=Total, label=comma(Total)),   
             colour="grey5", hjust=.5, vjust=-1)+
  labs(title = "Mujeres que accedieron al ILE en la Ciudad de México, 2016-2020.",
       subtitle = "Año 2020 se tiene registro hasta agosto",
       x= "",
       y="",
       caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/.")+
  theme_minimal(base_size=15)+ 
  theme(legend.position="none") -> g1


ggplotly(g1, tooltip = c("Año", "Total"))



#Características de las usuarias
ile_fsex <- ile %>% filter(!is.na(fsexual))
t1 <- ile_fsex %>% summarise(Media = mean(fsexual),
                             Mediana = median(fsexual),
                             MAD = mad(fsexual),
                             IQR = IQR(fsexual))



t2 <- ile %>% 
  select(año, edocivil_descripcion) %>% 
  filter(año==2020, !is.na(edocivil_descripcion)) %>% 
  group_by(edocivil_descripcion) %>% 
  summarise(Total = n(),
            Porcentaje= (Total/6170)*100)


t3 <- ile %>% 
  select(año, nivel_edu) %>% 
  filter(año==2020, !is.na(nivel_edu)) %>% 
  group_by(nivel_edu) %>% 
  summarise(Total = n(),
            Porcentaje=(Total/6182)*100)