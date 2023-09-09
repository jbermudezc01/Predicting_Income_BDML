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
base.datos.mayor.edad <-  as.tibble(base.datos.mayor.edad)


# Limpieza base de datos --------------------------------------------------
# En primer lugar seleccionamos las variables que nos interesan de la base de datos
bd.interes <- base.datos.mayor.edad %>% 
  select(age, urbano=clase, college, cotPension, cuentaPropia, depto, directorio, dsi,estrato=estrato1, formal, 
         hoursWorkUsual, maxEducLevel, oficio, orden, salud = p6090, seguridadsocial = p6100, sex, y_total_m , 
         microEmpresa, pet, sizeFirm, y_salary_m, y_salary_m_hu, y_total_m_ha)

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


# Reemplazar por la media las variables con NA -----------------------
# <seguridadsocial> es categorica por lo que cambiamos por la moda
bd.reducida$seguridadsocial <- ifelse(is.na(bd.reducida$seguridadsocial),which.max(table(bd.reducida$seguridadsocial)),
                                      bd.reducida$seguridadsocial)

# Como el resto de variables que quedan son salarios, se puede Reemplazar los NA por la media de cada columna
bd.reducida <- bd.reducida %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

## transformar variables de interés a logaritmo natural para reducir la influencia de valores extremos =0 
bd.reducida <- bd.reducida %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salary_m = log(y_salary_m),
         log_y_total_m = log(y_total_m),
         log_y_total_h = log(y_total_m_ha))


# histograma variable logaritmo salario por hora x sex

Histrograma_salario_h <- ggplot(data=base.datos.mayor.edad) +
  geom_histogram(mapping = aes(x=log_y_salary_h , group=as.factor(sex) , fill=as.factor(sex)))
Histrograma_salario_h

# boxplot_variable ogaritmo salario por hora x estrato

box_plot <- ggplot(data=base.datos.mayor.edad , mapping = aes(as.factor(estrato1) , log_y_salary_h)) + 
  geom_boxplot() 
box_plot

# Exportar a un archivo RDS
base.datos.salario <- base.datos.mayor.edad
saveRDS(base.datos.salario, file = "base.datos.salario.rds")

# Guardar en carpeta stores 
save(base.datos.salario,file = paste0(getwd(),'/stores/base.datos.salario.RData'))
