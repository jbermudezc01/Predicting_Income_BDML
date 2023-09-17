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
         sub.familiar = p6585s3, sub.educativo = p6585s4, relab)
glimpse(bd.interes)
# Urban solamente tiene 1 para todas las observaciones, por lo que es removida. 
# depto solamente tiene 11 para todas las observaciones, por lo que es removida
bd.interes <- bd.interes %>% 
  select(-c('urban','depto'))
#  Por otro lado, la variable salud tiene 3 labels: 1, 2 y 9. 9 indica: no sabe, no informa. Como son apenas 9 datos, 
# o menos del 0.001% de los datos, vamos a imputarlos por la moda 
moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}
bd.interes$salud[bd.interes$salud == 9] <- moda(bd.interes$salud)
# Revisando los labels, podemos ver que 2 indica que no estan afiliados a salud. Para interpretarlo como dummy se pueden
# reemplazar estos datos por 0, de modo que la dummy seria 1 cuando esta afiliado a salud y 0 si no
bd.interes$salud[bd.interes$salud == 2] <- 0

# Para <seguridadsocial>, <sub.alimentacion>, <sub.transporte>, <sub.familiar> <sub.educativo> se elimina tambien 
# la categoria 9 y se interpola por la moda
bd.interes$seguridadsocial[bd.interes$seguridadsocial == 9]   <- moda(bd.interes$seguridadsocial)
bd.interes$sub.alimentacion[bd.interes$sub.alimentacion == 9] <- moda(bd.interes$sub.alimentacion)
bd.interes$sub.transporte[bd.interes$sub.transporte == 9]     <- moda(bd.interes$sub.transporte)
bd.interes$sub.familiar[bd.interes$sub.familiar == 9]         <- moda(bd.interes$sub.familiar)
bd.interes$sub.educativo[bd.interes$sub.educativo == 9]       <- moda(bd.interes$sub.educativo)
# Por ultimo, observamos que en la variable <relab> la categoria 8 indica jornalero. Debido a que solamente cuenta con 1 dato, 
# se le va a imputar la moda, para evitar posibles errores al momento de estimar
bd.interes$relab[bd.interes$relab == 8] <- moda(bd.interes$relab)

# Reemplazar los valores 2 para las variables subsidios a 0, de este modo 0 significa que no tuvo subsidio y 1 que si tuvo
bd.interes$sub.alimentacion[bd.interes$sub.alimentacion == 2] <- 0
bd.interes$sub.transporte[bd.interes$sub.transporte == 2]     <- 0
bd.interes$sub.familiar[bd.interes$sub.familiar == 2]         <- 0
bd.interes$sub.educativo[bd.interes$sub.educativo == 2]       <- 0

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
# Sin embargo, la transformacion deja a <dsi> e <inac> solamente con 1 valor por lo que ya no son de interes
bd.reducida <- bd.reducida %>% 
  select(-c('dsi','inac'))

# Reemplazar por la media las variables con NA  o crear base sin NA -----------------------

# Crear una base de datos sin <NA> en los salarios, porque tener 40% de NA nos parece imputar mucho
bd.sin.na <- bd.reducida %>% 
  dplyr::filter(!is.na(y_salary_m))

# Incluso asi hay dos variables que tenemos que imputar por la moda, <seguridadsocial> y <maxEducLevel>
bd.sin.na <- bd.sin.na %>% 
  mutate(
    seguridadsocial = ifelse(is.na(seguridadsocial), moda(seguridadsocial), seguridadsocial),
    maxEducLevel     = ifelse(is.na(maxEducLevel), moda(maxEducLevel), maxEducLevel)
  )

# Segunda opcion:
# <seguridadsocial> es categorica por lo que cambiamos por la moda
bd.media.imputada <- bd.reducida %>%
  mutate(
    seguridadsocial  = ifelse(is.na(seguridadsocial), moda(seguridadsocial), seguridadsocial),
    sub.alimentacion = ifelse(is.na(sub.alimentacion), moda(sub.alimentacion), sub.alimentacion),
    sub.transporte   = ifelse(is.na(sub.transporte), moda(sub.transporte), sub.transporte),
    sub.educativo    = ifelse(is.na(sub.educativo), moda(sub.educativo), sub.educativo),
    sub.familiar     = ifelse(is.na(sub.familiar), moda(sub.familiar), sub.familiar),
    maxEducLevel     = ifelse(is.na(maxEducLevel), moda(maxEducLevel), maxEducLevel)
  )

# Como el resto de variables que quedan son salarios, se puede Reemplazar los NA por la media de cada columna
bd.media.imputada <- bd.media.imputada %>%
  mutate(y_salary_m_hu = ifelse(is.na(y_salary_m_hu), mean(y_salary_m_hu, na.rm = TRUE), y_salary_m_hu),
         y_salary_m = ifelse(is.na(y_salary_m), mean(y_salary_m, na.rm = TRUE), y_salary_m))
  

## transformar variables de interés a logaritmo natural para reducir la influencia de valores extremos =0 
bd.sin.na <- bd.sin.na %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salary_m = log(y_salary_m))

bd.media.imputada <- bd.media.imputada %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salary_m = log(y_salary_m))

# Cambiar variables a factor ----------------------------------------------
variables.factor <- c('college','cotPension','cuentaPropia','estrato','formal','maxEducLevel','oficio',
                      'salud','seguridadsocial','sex','microEmpresa','sizeFirm','sub.alimentacion','sub.transporte',
                      'sub.familiar','sub.educativo', 'relab')
bd.sin.na <- bd.sin.na %>%
  mutate_at(variables.factor, as.factor)
bd.media.imputada <- bd.media.imputada %>%
  mutate_at(variables.factor, as.factor)

# Exportar a stores -------------------------------------------------------
# RData
save(bd.sin.na, bd.media.imputada, file=paste0(getwd(),'/stores/bases.tratadas.RData'))

