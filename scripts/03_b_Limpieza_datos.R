##########################################################
# Manejo de datos
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################

rm(list=ls())
cat('\014')

# Cargar librerias --------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)     #Para web scrapping
library(ggplot2)
library(skimr)

load("~/Desktop/base_datos_original.rdata")
# Cargar la base de datos -------------------------------------------------
# La base de datos la obtuvimos haciendo web scrapping
load(paste0(getwd(),'/stores/base_datos_original.RData'))

# filtrar la base de datos para seleccionar individuos mayores de 18 anhos
base.datos.mayor.edad <- base.datos.original %>% 
  dplyr::filter(age > 18)

##### limpiar datos #####

# inspeccionar los datos 

glimpse(base.datos.mayor.edad) # reporte especifico de la base de datos 
skim(base.datos.mayor.edad) %>% head()

# identificar valor atipicos en variables de interés: salario e ingreso total 

## realizamos  histrogramas de las variables de interés para identificar valores extremos 
# histograma variable salario por hora
Histrograma_salario_h <- ggplot(data=base.datos.mayor.edad) +
  geom_histogram(mapping = aes(x= y_salary_m_hu , group=as.factor(sex) , fill=as.factor(sex)))
Histrograma_salario_h
# histograma varibale salario mensual
Histrograma_salario_m <- ggplot(data=base.datos.mayor.edad) +
  geom_histogram(mapping = aes(x= y_salary_m , group=as.factor(sex) , fill=as.factor(sex)))
Histrograma_salario_m

# utilizamos el rando intercuantilico para clasficar los valores (atipicos o no)
# Definir una función para etiquetar variables como atípicas o no atípicas
valores_atipicos <- function(variable) {
  q1 <- quantile(variable, 0.25, na.rm =T)
  q3 <- quantile(variable, 0.75, na.rm =T)
  iqr <- q3 - q1
  umbral_superior <- q3 + 1.5 * iqr
  umbral_inferior <- q1 - 1.5 * iqr
  etiquetas <- ifelse(variable > umbral_superior | variable < umbral_inferior, "Atípica", "No Atípica")
  return(etiquetas)
}

# Aplicar la función para etiquetar variables a todas las columnas numéricas del DataFrame

# Llamada a lapply sin los paréntesis y proporcionando la función valores_atipicos como objeto
resultado_valores_atipicos <- lapply(base.datos.mayor.edad[, c("y_salary_m_hu",
                                                               "y_salary_m",
                                                               "y_total_m",
                                                               "y_total_m_ha")],
                                     valores_atipicos)
### identificar el porcentaje de valores atipicos de cada función 

# Contar los valores "Atípicos" en cada columna
conteo_atipicos <- sapply(resultado_valores_atipicos, function(x) sum(x == "Atípica", na.rm = T))

# Calcular el porcentaje de valores atípicos para cada columna
total_filas <- nrow(base.datos.mayor.edad)
porcentaje_atipicos <- (conteo_atipicos / total_filas) * 100

# Mostrar los porcentajes de valores atípicos para cada columna
print(porcentaje_atipicos)

#### imputar media a los valores atipicos 

# Crear una función para imputar la media a los valores atípicos
imputar_media_atipicos <- function(variable) {
  etiquetas <- valores_atipicos(variable)  # Etiquetar los valores atípicos
  
  # Calcular la media de la variable
  media <- mean(variable, na.rm = TRUE)
  
  # Imputar la media a los valores etiquetados como "Atípicos"
  variable <- ifelse(etiquetas == "Atípica", media, variable)
  
  return(variable)
}

# Aplicar la función para imputar la media a todas las columnas numéricas del DataFrame
base.datos.mayor.edad[, c("y_salary_m_hu", "y_salary_m", "y_total_m", "y_total_m_ha")] <- lapply(base.datos.mayor.edad[, c("y_salary_m_hu", "y_salary_m", "y_total_m", "y_total_m_ha")], imputar_media_atipicos)


# se evidencio presencia de valores extremos cercanos e iguales a cero en las cuatro variables
# en ese sentido, se procede a: 1. imputar la media  suavizar su efecto transformando a log_natural de los valores 

## transformar variables de interés a logaritmo natural para reducir la influencia de valores extremos =0 
base.datos.mayor.edad= base.datos.mayor.edad %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salaty_m = log(y_salary_m),
         log_y_total_m = log(y_total_m),
         log_y_total_h = log(y_total_m_ha))

### Identificar y tratar valores faltantes NA

# calcular el porcentjar de por cada variable
missing_values <- base.datos.mayor.edad %>%
  summarise_all(~ sum(is.na(.)) / n()) %>%
  gather(variable, porcentaje_faltantes)

# trata valores NA 

# Regla de tratamiento: tratar los valores NA se imputa con la media de la variables
# si la cantidad de missing values no supera el 10% de las observaciones para cada vairables
# para este caso, la cantidad de NA para todas las variables esta en un rango de 0 a 9,1%.

## imputar media a variables numericas 
#identficar las variables numericas
variables_numericas <-  sapply(base.datos.mayor.edad, is.numeric)
# calcular la media de los variables, se omite los valores faltantes
medias <- colMeans(base.datos.mayor.edad[, variables_numericas], na.rm = TRUE)
# imputar la media a los valores falntes en las variables numericas 
base.datos.mayor.edad[, variables_numericas] <- lapply(base.datos.mayor.edad[, variables_numericas], function(x) {
  ifelse(is.na(x), medias , x)
})
## Revisamos el procedimiento anterior
MS <- colSums(is.na(base.datos.mayor.edad))
MS 

### crear un subset con variables de interes 

bd_income <- base.datos.mayor.edad %>%
  select(-p6050:-p6240)%>%
  select(-p6426)%>%
  select(-p6500:-p7510s7a1)

# Exportar a un archivo RDS
saveRDS(bd_income, file = "bd_income.rds")

# Guardar en carpeta stores 
save(bd_m_18age,file = paste0(getwd(),'/stores/bd_income.RData'))
