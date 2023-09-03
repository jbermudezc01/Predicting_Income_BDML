##########################################################
# Web scrapping
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
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
# Ahora podemos acceder a las tablas iterando por url.datos.
# En primer lugar usamos <read_html> para leer el html, posteriormente usamos <html_nodes> con el xpath donde se 
# encuentra la tabla, y por ultimo utilizamos <html_attr> para encontrar el link del archivo de cada chunk de datos
url.tablas <- unlist(lapply(url.datos, function(x) read_html(x) %>% 
                              html_nodes(xpath ='/html/body/div/div/div[2]/div') %>% 
                              html_attr("w3-include-html")))

URL <- paste0(url.principal,url.tablas)

# Ahora por cada url en <URL> vamos a leer la tabla
lista.tablas <- lapply(URL, function(x){
  df.lista <- read_html(x) %>% html_table()
  tabla    <- df.lista[[1]]
  return(tabla)
})

# Unimos todas las tablas dentro de nuestra lista <tabla>
base.datos.original <- do.call(bind_rows, lista.tablas)

# Guardar la tabla en el directorio
save(base.datos.original,file = paste0(getwd(),'/stores/base_datos_original.RData'))
