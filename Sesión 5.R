##Cargar bases de Star Wars####
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(RColorBrewer)
library(scales)
library(ggthemes)


#Para esta sesión no se descarga la base del archivo, sólo se llama desde la red.

starwars<- read.csv("https://raw.githubusercontent.com/rpizarrog/Curso-Titulacion-Data-Science-/master/2020/datos/starwars.csv", encoding = "UTF-8")
View(starwars)
glimpse(starwars) #Similar a str pero en este caso viene glimp en dplyr.


    #Descripción de algunas variables.
    #https://dplyr.tidyverse.org/reference/starwars.html


#Mutate, Filter, Select, Arrange


#Ejercicios básicos####
starwars %>% #Selecciono mi base 
  filter(height < 90) %>% #Filtro por altura menor a 90
  select(name, gender, height) %>%  #Selecciono las variables que quiero
  arrange(height) #Ordenamos de acuerdo a la variable especificada



#Ejercicio: contar el total de personajes de acuerdo a homeworld
starwars %>% 
  group_by(name, homeworld) %>%
  summarise(Total=n(), #Primera columna que añado
            PromedioAltura=(height)) %>% #Segunda columna 
  filter(homeworld > 1) %>% 
  arrange(Total)



#Graficar a partir de tablas
starwars %>% 
  select(species, gender, height, homeworld) %>% 
  group_by(homeworld, gender) %>% 
  summarise(Total=n(), 
            PromedioAltura=(height*.5)) %>%   
  arrange(Total) %>% 
  ggplot(aes(x=gender, y=Total, fill=gender))+ 
    geom_col()
  
  

  #Omitir o eliminar los valores NA, porque en el grafico anterior se visualiza los NA
starwars %>% 
      na.omit(height)
  
  
  

#Añadiendo detalles al grafico
  
g1<-starwars %>% 
  select(species, gender, height, name) %>% 
  group_by(name, gender, height) %>% 
  summarise(Total=n(), 
            PromedioAltura=(height)) %>%   
  arrange(Total) %>% 
  ggplot(aes(x=gender, y=height ,fill=gender))+
  geom_col(position="dodge")+
  geom_text (aes(x=gender, y=PromedioAltura, label=height), #label es valor de etiqueta de la barra        
            colour="grey5", hjust=.5, vjust=0.5)+
  labs(title = "Star Wars",
       subtitle = "Subtitle",
       x= "",
       y="",
       caption = "Fuente: https://raw.githubusercontent.com")+
  theme_economist(base_size=15)+ #Se especificó el "base_size" para aumentar el tamaño, sirve como un zoom (?)
  theme(legend.position="none") #"" bottom, top, rigth, left, none




  
ggplotly(g1)

ggplotly(g1, tooltip= c("PromedioAltura")) #Para especificar que etiquetas sí mostrar
