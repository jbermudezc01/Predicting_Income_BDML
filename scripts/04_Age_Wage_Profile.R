##########################################################
# Perfil salario edad
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################


# Limpieza area de trabajo ------------------------------------------------
rm(list=ls())
cat('\014')

# Cargar librerias --------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)     #Para web scrapping
library(ggplot2)
library(skimr)
library(stargazer)

# Cargar la base de datos -------------------------------------------------
# Las bases de datos que obtuvimos al filtrar
# bd.sin.na <- read_csv("stores/bd.sin.na.csv")
# bd.media.imputada <- read_csv("stores/bd.media.imputada.csv")
load(file=paste0(getwd(),'/stores/bases.tratadas.RData'))

#--------------Estimación de la regresión log(w)=b1+b2age+b3age^2
#creamos una variable de edad al cuadrado
bd.media.imputada$age2<- (bd.media.imputada$age)^2
bd.sin.na$age2<- (bd.sin.na$age)^2

#Para cada base de datos estimaremos dos regresiones con el fin de contrastar que la construcción
#de nuestra variable objetivo haya sido la adecuada

#Estimación de los modelos para la base de datos en que imputamos la media  a los NA
###1. Estimando con log_y_salary_h
modelo1_imputando <- lm(log_y_salary_h ~age+age2, data = bd.media.imputada, x = TRUE)
##2. Estimando con log(y_salary_m_hu)
modelo2_imputando<-lm(log(y_salary_m_hu)~age+age2, data = bd.media.imputada, x = TRUE)


#Estimación de los modelos para la base de datos en que eliminamos los NA.
###1. Estimando con log_y_salary_h
modelo1_sin_na <- lm(log_y_salary_h ~age+age2, data = bd.sin.na, x = TRUE)
##2. Estimando con log(y_salary_m_hu)
modelo2_sin_na<-lm(log(y_salary_m_hu)~age+age2, data = bd.sin.na, x = TRUE)


#Tablas de resultados de las regresiones
stargazer(modelo1_imputando,modelo2_imputando, modelo1_sin_na, modelo2_sin_na, type='text')
stargazer(modelo1_imputando,modelo1_sin_na, type='text')


#Estimación del modelo
coef_imputando<-modelo1_imputando$coefficients
coef_sin_na<-modelo1_sin_na$coefficients

summary_imputado<-summary(modelo1_imputando)$coefficients
summary_sin_na<-summary(modelo1_sin_na)$coefficients


#Gráfico Automático
ggplot(bd.media.imputada, aes(y = log_y_salary_h, x = age)) +
  geom_point() + # add points
  stat_smooth(formula = 'y ~ x+x^2', method = lm, se = FALSE, 
              linewidth = 1) +  #fit the linear model in the plot
  theme_bw() + #black and white theme
  labs(x = "Age",  
       y = "Log(wage_h)",
       title = "Valores predichos del salario imputando la media en NA") # labels

ggplot(bd.sin.na, aes(y = log_y_salary_h, x = age)) +
  geom_point() + # add points
  stat_smooth(formula = 'y ~ x+x^2', method = lm, se = FALSE, 
              linewidth = 1) +  #fit the linear model in the plot
  theme_bw() + #black and white theme
  labs(x = "Age",  
       y = "Log(wage_h)",
       title = "Valores predichos del salario eliminando NAs") # labels

#--------------Bootstrap para la "peak age" con Intervalos de confianza

#sabemos que podemos encontrar la peak age al derivar nuestra regresión respecto a la edad
#dln(w)/dage=b1+2b2edad=0
#edad=-b1/(2*b2)

#Obtenemos los coeficientes y los extraemos a escalares
#----Para base de datos con media imputada-----
coef_imputando
b1_i<-coef_imputando[2]
b2_i<-coef_imputando[3]

peak_age_i=(-b1_i/(2*b2_i))
peak_age_i

B<-2000
peak_age_i_vector<-rep(0,B)
lower_i<-rep(0,B)
upper_i<-rep(0,B)

# Generar semilla para reproducibilidad
set.seed(123)
for(i in 1:B){
  db_sample<- sample_frac(bd.media.imputada,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  f<-lm(log_y_salary_h~age+age2,db_sample)# estimates the models
  coefs<-f$coefficients # gets the coefficient of interest that coincides with the elasticity of demand
  b1<-coefs[2]
  b2<-coefs[3]
  peak_age_i_vector[i]<-(-b1/(2*b2)) #saves it in the above vector
}

mean(peak_age_i_vector)
SE_peakage_i<-sqrt(var(peak_age_i_vector));SE_peakage_i

#Con el error estándar que sacamos del bootstrap podemos crear intervalos de 
#confianza para cada submuestra generada por el bootstrap
for(i in 1:B){
  lower_i[i]<- peak_age_i_vector[i]-qnorm(0.05/2)*SE_peakage_i
  upper_i[i]<- peak_age_i_vector[i]+qnorm(0.05/2)*SE_peakage_i
}

#----Para base de datos eliminando NA-----
coef_sin_na
b1_s<-coef_sin_na[2]
b2_s<-coef_sin_na[3]

peak_age_s=(-b1_s/(2*b2_s))
peak_age_s

B<-2000
peak_age_s_vector<-rep(0,B)
lower_s<-rep(0,B)
upper_s<-rep(0,B)

for(i in 1:B){
  db_sample<- sample_frac(bd.sin.na,size=1,replace=TRUE) #takes a sample with replacement of the same size of the original sample (1 or 100%)
  f<-lm(log_y_salary_h~age+age2,db_sample)# estimates the models
  coefs<-f$coefficients # gets the coefficient of interest that coincides with the elasticity of demand
  b1<-coefs[2]
  b2<-coefs[3]
  peak_age_s_vector[i]<-(-b1/(2*b2)) #saves it in the above vector
}

mean(peak_age_s_vector)
SE_peakage_s<-sqrt(var(peak_age_s_vector));SE_peakage_s

#Con el error estándar que sacamos del bootstrap podemos crear intervalos de 
#confianza para cada submuestra generada por el bootstrap
for(i in 1:B){
  lower_s[i]<- peak_age_s_vector[i]-qnorm(0.05/2)*0.8302136
  upper_s[i]<- peak_age_s_vector[i]+qnorm(0.05/2)*SE_peakage_s
}

##------------Bootstrap automático:
p_load("boot")
#boot(data, statistic, R)

funcion_pa<-function(data,index){
  b1<-coef(lm(log_y_salary_h~age+age2, data = data, subset = index))[2]
  b2<-coef(lm(log_y_salary_h~age+age2, data = data, subset = index))[3]
  return((-b1/(2*b2))) 
}

funcion_pa(bd.media.imputada, 1:nrow(bd.media.imputada))
#si es la misma que teníamos al principio

#Hacemos el boot de forma automática con nuestra función
boot_i<-boot(bd.media.imputada, funcion_pa, R=2000)
boot_s<-boot(bd.sin.na, funcion_pa, R=2000)

#Creación del dataframe para generar los intervalos de confianza:
mat = matrix(ncol = 0, nrow = 2000)
IC<-data.frame(mat)

#Intervalos para datos con media inputada a los NA
SE_i<-as.numeric(sqrt(var(boot_i[["t"]])))
IC$bootstrap_i<-boot_i[["t"]]
IC$LI_i<-boot_i[["t"]]-qnorm(0.05/2)*SE_i
IC$LS_i<-boot_i[["t"]]+qnorm(0.05/2)*SE_i

#Intervalos para datos a los que se les quitaron los NA
SE_s<-as.numeric(sqrt(var(boot_s[["t"]])))
IC$bootstrap_s<-boot_s[["t"]]
IC$LI_s<-boot_s[["t"]]-qnorm(0.05/2)*SE_s
IC$LS_s<-boot_s[["t"]]+qnorm(0.05/2)*SE_s








