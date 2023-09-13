##########################################################
# Limpieza de datos
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################


# Limpieza area de trabajo ------------------------------------------------
rm(list=ls())
cat('\014')


# Librerias ---------------------------------------------------------------
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       ggplot2, # plotting
       skimr) # summary data 


# Base de datos -----------------------------------------------------------
load(paste0(getwd(),'/stores/base_datos_original.RData'))
# Filtrar la base de datos para seleccionar individuos mayores de 18 anhos
base.datos.mayor.edad <- base.datos.original %>% 
  dplyr::filter(age > 18)
base.datos.mayor.edad <-  as_tibble(base.datos.mayor.edad)

# Limpieza base de datos --------------------------------------------------
# En primer lugar seleccionamos las variables que nos interesan de la base de datos
bd.interes <- base.datos.mayor.edad %>% 
  select(age, urban=clase, college, cotPension, cuentaPropia, depto, directorio, dsi,estrato=estrato1, formal, 
         inac, ingtot,hoursWorkUsual, maxEducLevel, oficio, orden, salud = p6090, seguridadsocial = p6100, sex, 
         microEmpresa, sizeFirm, y_salary_m, y_salary_m_hu, sub.alimentacion = p6585s1, sub.transporte=p6585s2,
         sub.familiar = p6585s3, sub.educativo = p6585s4)

# Se tiene pensado utilizar y_total_m como variable de salario ya que tiene en cuenta a los hogares que trabajan como
# independientes. Aparte, tiene menor numero de NA
# Veamos el porcentaje de NA de cada variable
round(apply(bd.interes,MARGIN = 2, function(x) sum(is.na(x)))/nrow(bd.interes),2)
# Eliminar las observaciones que no cuentan datos para <cotPension> <formal> <hoursWorkUsual> <oficio> <microEmpresa>
# <sizeFirm>
bd.reducida <- bd.interes %>% 
  dplyr::filter(!is.na(hoursWorkUsual) & !is.na(formal) & !is.na(cotPension) & !is.na(oficio) 
                &!is.na(sizeFirm) & !is.na(microEmpresa))
# Al hacer esta transformacion no se perdio ningun dato de los salarios, pero si se asegura de contar con los datos
# de las variables explicativas, consideramos que reemplazar por la media para todas las variables explicativas 
# no es buena idea
round(apply(bd.reducida,MARGIN = 2, function(x) sum(is.na(x)))/nrow(bd.reducida),2)


# Reemplazar por la media las variables con NA  o crear base sin NA -----------------------

# Crear una base de datos sin <NA> en los salarios, porque tener 40% de NA nos parece imputar mucho
bd.sin.na <- bd.reducida %>% 
  dplyr::filter(!is.na(y_salary_m))

# <seguridadsocial> es categorica por lo que cambiamos por la moda

mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

bd.reducida <- bd.reducida %>%
  mutate(
    seguridadsocial  = ifelse(is.na(seguridadsocial), mode(seguridadsocial), seguridadsocial),
    sub.alimentacion = ifelse(is.na(sub.alimentacion), mode(sub.alimentacion), sub.alimentacion),
    sub.transporte   = ifelse(is.na(sub.transporte), mode(sub.transporte), sub.transporte),
    sub.educativo    = ifelse(is.na(sub.educativo), mode(sub.educativo), sub.educativo),
    sub.familiar     = ifelse(is.na(sub.familiar), mode(sub.familiar), sub.familiar),
    maxEducLevel     = ifelse(is.na(maxEducLevel), mode(maxEducLevel), maxEducLevel)
  )

# Segunda opcion:
# Como el resto de variables que quedan son salarios, se puede Reemplazar los NA por la media de cada columna
bd.media.imputada <- bd.reducida %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

## transformar variables de interés a logaritmo natural para reducir la influencia de valores extremos =0 
bd.sin.na <- bd.sin.na %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salary_m = log(y_salary_m))

bd.media.imputada <- bd.media.imputada %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salary_m = log(y_salary_m))

# Exportar a stores -------------------------------------------------------
# CSV
write.csv(bd.sin.na, file = paste0(getwd(),'/stores/base_datos_sin_na.csv'))
write.csv(bd.media.imputada, file = paste0(getwd(),'/stores/base_datos_imputando_na.csv'))

