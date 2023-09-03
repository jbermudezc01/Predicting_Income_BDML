##########################################################
# Web scrapping y manejo de datos
# Autores: Juan Pablo Bermudez. Lina Bautista. 
##########################################################


###------------- SCRAPPING Y GENERACION DE BASE DE DATOS --------###
rm(list=ls())
cat('\014')

# Librerias ---------------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)    # Para web scrapping

# Web scrapping -----------------------------------------------------------
# Scrapear la pagina principal, donde se van a encontrar varios chunks de datos.
# Para una buena reproducibilidad, la idea es poder acceder a estos chunks desde la pagina
# principal, sin tener que escribir los url por separado
url.principal  <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/'
html.principal <- read_html(url.principal)

# Viendo la pagina web, podemos observar que la manera de acceder a los url de los chunks
# es a traves de <html_elements('li a')>
elementos.raw <- html.principal %>% 
  html_elements('li a')

# En <elementos.interes> tenemos 11 elementos, de los cuales solamente nos interesan aquellos que 
# tengan 'Data chunk' en el texto, por lo que es necesario filtrarlo
elementos.filtrados <- elementos.raw[grepl('Data chunk', html_text(elementos.raw))]

# En <elementos.filtrados> estan los objetos que nos interesan, que tienen atributo href
elementos.href <- elementos.filtrados %>% 
  html_attr('href')

# Nos salen 10 objetos tipo texto, que al juntarlos con <url.principal> nos van a dar 10 nuevos url donde
# estan los datos
url.datos <- paste0(url.principal,elementos.href)

# Creacion de tablas ------------------------------------------------------
# Ahora podemos acceder a las tablas iterando por url.datos
lista.tablas <- lapply(url.datos, function(x) read_html(x) %>% html_table())
