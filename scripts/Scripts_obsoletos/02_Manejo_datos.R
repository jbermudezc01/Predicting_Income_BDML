##########################################################
# Manejo de datos
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

rm(list=ls())
cat('\014')

# Cargar librerias --------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)     #Para web scrapping

# Cargar la base de datos -------------------------------------------------
# La base de datos la obtuvimos haciendo web scrapping
load(paste0(getwd(),'/stores/base_datos_original.RData'))

# filtrar la base de datos para seleccionar individuos mayores de 18 anhos
base.datos.mayor.edad <- base.datos.original %>% 
  dplyr::filter(age > 18)


