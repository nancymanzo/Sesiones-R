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

##Otra forma de replicar el gráfico anterior:
ile %>%
 filter(!is.na(año)) %>% #Filtro para quitar mi valor NA
 ggplot(aes(x = año, fill = año)) +
 geom_bar() + #Tipo de gráfico
 scale_fill_brewer(palette = "Set2") + #Palette para colorear y representar mi gráfico
 labs(title = "Número total de mujeres que accedieron al ILE en la CDMX.", 
      caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/.", fill = "AÑO",
      x = "", 
      y = "") +
 ggthemes::theme_tufte() ->g2 #Se necesita ggthemes.

  ggplotly(g2, tooltio= c("año"))



#Más gráficos
ile %>%
 filter(!is.na(año)) %>%
 filter(!(entidad %in% c("CIUDAD DE MEXICO", "ESTADO DE MEXICO")) & !is.na(entidad)) %>%
 ggplot(aes(x = entidad, fill = entidad, weight = año)) +
 geom_bar() +
 scale_fill_viridis_d(option = "viridis") +
 labs(x = " ", 
      y = " ", 
      title = "Total de ILE por entidades realizados en CDMX.", 
      subtitle = "Se omitió las entidades de CDMX y EDOMEX", 
      caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/.", fill = "ENTIDAD") +
 theme_void() +
 facet_wrap(vars(año)) ->g3

  ggplotly(g3)


###
ile %>%
 filter(año >= 2020L & año <= 2020L & !is.na(año)) %>%
 ggplot(aes(x = edad, 
            y = nhijos, 
            colour = edad, 
            size = nhijos) +
 geom_point() +
 scale_color_distiller(palette = "PuRd") +
 labs(x = "Edad de las mujeres", 
      y = "Número de hijxs", 
      title = "Edad de las mujeres que accedieron al ILE y número de hijxs en la CDMX, 2020.", 
      subtitle = "El rango de edad incluye desde los 11 años.", 
      caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/.", 
      color = "Edad", 
      size = "Número de hijxs (0-6)") +
 theme_minimal() ->g4

 ggplotly(g4)

        
####
ile %>%
 filter(año >= 2016L & año <= 2019L | is.na(año)) %>%
 filter(!is.na(ocupacion)) %>%
 filter(!(entidad %in% c("CIUDAD DE MEXICO", "ESTADO DE MEXICO")) & !is.na(entidad)) %>%
 ggplot(aes(x = ocupacion, fill = ocupacion)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = " ", 
       y = " ", 
       title = "Ocupación de las mujeres que accedieron al ILE en CDMX", 
       subtitle = "2017-2019", 
       caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/.", fill = "Ocupación") +
  theme_void() +
  facet_wrap(vars(año)) ->g5
        
 ggplotly(g5)

        
###
ile %>%
  filter(año >= 2016L & año <= 2016L | is.na(año)) %>%
  ggplot(aes(x = npartos, 
             y = naborto, 
             colour = naborto, 
             size = naborto)) +
  geom_point() +
  scale_color_viridis_c(option = "viridis") +
  labs(x = "Número de partos", 
       y = "Número de abortos", 
       title = "Relación de las mujeres que accedieron al ILE en CDMX con el número de partos.", 
       subtitle = "Año 2016", 
       caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/") +
  theme_bw()





#Características de las usuarias

ile_edad <- ile %>% 
  filter(!is.na(edad))
t1 <- ile_edad %>% 
  summarise(Total = n(),
            Media = mean(edad),
            Mediana = median(edad),
            MAD = mad(edad),
            IQR = IQR(edad))

        #Histograma
        ile %>% 
          filter(año==2020) %>% 
          ggplot(aes(x=edad, fill="salmon3"))+
          geom_histogram()+
            labs(title = "Edad de las mujeres que accedieron al ILE en la Ciudad de México,2020.",
                 x= "",
                 y="",
                 caption = "Fuente: Datos abiertos de la CDMX https://datos.cdmx.gob.mx/explore/dataset/interrupcion-legal-del-embarazo/export/.")+
            theme_minimal(base_size=15)+ 
            theme(legend.position="none")




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
