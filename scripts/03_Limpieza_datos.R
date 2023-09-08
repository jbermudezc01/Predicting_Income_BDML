### limpieza de datos ####

# limpiar espacio de trabajo 
rm(list=ls())
cat('\014')

# Cargar librerias 
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       ggplot2, # plotting
       skimr) # summary data 

# Cargar la base de datos 
load(paste0(getwd(),'/stores/base_datos_original.RData'))

# filtrar la base de datos para seleccionar individuos mayores de 18 anhos
base.datos.mayor.edad <- base.datos.original %>% 
dplyr::filter(age > 18)
base.datos.mayor.edad <-  as.tibble(base.datos.mayor.edad)

#### limpieza de datos

## inspeccionar datos
skim(base.datos.mayor.edad) %>% head()
## inspeccionar variable de interes 
summary(base.datos.mayor.edad$y_salary_m_hu)

#### tratar los NA 

## identificar los  NA

### creamos vector con variables de interés para identificar la magnitud NA 

variables_interes <- c("estrato1","sex","age",
                      "totalHoursWorked","pet","college","maxEducLevel",
                      "sizeFirm","oficio","microEmpresa","y_salary_m","y_salary_m_hu",
                       "y_salarySec_m","y_total_m","y_total_m_ha")

#Crear una lista para almacenar los resultados
resultados <-  list()

# Iterar a través de las variables

for (variable in variables_interes) {
  resultado <- is.na(base.datos.mayor.edad[[variable]]) %>% table()
  resultados[[variable]] <- resultado
}
resultados

### tratar los valores NA imputando con la media 

# Reemplaza los NA por la media de cada columna
base.datos.mayor.edad <- base.datos.mayor.edad %>%
  mutate_all(~ ifelse(is.na(.), mean(., na.rm = TRUE), .))

### convertir variables de interés a logaritmo natural 


## transformar variables de interés a logaritmo natural para reducir la influencia de valores extremos =0 
base.datos.mayor.edad= base.datos.mayor.edad %>% 
  mutate(log_y_salary_h = log(y_salary_m_hu),
         log_y_salary_m = log(y_salary_m),
         log_y_total_m = log(y_total_m),
         log_y_total_h = log(y_total_m_ha))

### varificar la transformación de las variables de interés

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
