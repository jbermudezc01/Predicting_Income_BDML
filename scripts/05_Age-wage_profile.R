

# Cargar librerias --------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)     #Para web scrapping
library(ggplot2)
library(skimr)
library(stargazer)
# Cargar la base de datos -------------------------------------------------
# Las bases de datos que obtuvimos al filtrar
base_datos_sin_na <- read_csv("stores/base_datos_sin_na.csv")
base_datos_imputando_na <- read_csv("stores/base_datos_imputando_na.csv")


#--------------Estimación de la regresión log(w)=b1+b2age+b3age^2
#creamos una variable de edad al cuadrado
base_datos_imputando_na$age2<- (base_datos_imputando_na$age)^2
base_datos_sin_na$age2<- (base_datos_sin_na$age)^2

#Para cada base de datos estimaremos dos regresiones con el fin de contrastar que la construcción
#de nuestra variable objetivo haya sido la adecuada

#Estimación de los modelos para la base de datos en que imputamos la media  a los NA
###1. Estimando con log_y_salary_h
modelo1_imputando <- lm(log_y_salary_h ~age+age2, data = base_datos_imputando_na, x = TRUE)
##2. Estimando con log(y_salary_m_hu)
modelo2_imputando<-lm(log(y_salary_m_hu)~age+age2, data = base_datos_imputando_na, x = TRUE)


#Estimación de los modelos para la base de datos en que eliminamos los NA.
###1. Estimando con log_y_salary_h
modelo1_sin_na <- lm(log_y_salary_h ~age+age2, data = base_datos_sin_na, x = TRUE)
##2. Estimando con log(y_salary_m_hu)
modelo2_sin_na<-lm(log(y_salary_m_hu)~age+age2, data = base_datos_sin_na, x = TRUE)


#Tablas de resultados de las regresiones
stargazer(modelo1_imputando,modelo2_imputando, modelo1_sin_na, modelo2_sin_na, type='text')
stargazer(modelo1_imputando,modelo1_sin_na, type='text')


#Estimación del modelo
coef_imputando<-modelo1_imputando$coefficients
coef_sin_na<-modelo1_sin_na$coefficients

summary_imputado<-summary(modelo1_imputando)$coefficients
summary_sin_na<-summary(modelo1_sin_na)$coefficients


#Gráfico Automático
ggplot(base_datos_imputando_na, aes(y = log_y_salary_h, x = age)) +
  geom_point() + # add points
  stat_smooth(formula = 'y ~ x+x^2', method = lm, se = FALSE, 
              size = 1) +  #fit the linear model in the plot
  theme_bw() + #black and white theme
  labs(x = "Age",  
       y = "Log(wage_h)",
       title = "Valores predichos del salario imputando la media en NA") # labels

ggplot(base_datos_sin_na, aes(y = log_y_salary_h, x = age)) +
  geom_point() + # add points
  stat_smooth(formula = 'y ~ x+x^2', method = lm, se = FALSE, 
              size = 1) +  #fit the linear model in the plot
  theme_bw() + #black and white theme
  labs(x = "Age",  
       y = "Log(wage_h)",
       title = "Valores predichos del salario eliminando NAs") # labels





#Gráfico manual
# Building the plot by ourselves
preplot = data.frame(
  Features = rownames(summary_imputado),
  Estimate = summary_imputado[,'Estimate'],
  std_error = summary_imputado[,'Std. Error'])

# The function qnorm() find the boundary value that determines the area 
# under the normal density curve before alpha/2.
alpha = 0.05 # 95% Confidence Interval
preplot$lower = preplot$Estimate - qnorm(alpha/2) * preplot$std_error
preplot$upper = preplot$Estimate + qnorm(alpha/2) * preplot$std_error
preplot = preplot[!(preplot$Features == '(Intercept)'),]


ggplot(preplot) +
  geom_vline(xintercept = 0, linetype = 4) + #adds a vertical line at zero
  geom_point(aes(x = Estimate, y = Features)) + #point estimate
  geom_segment(aes(y = Features, yend = Features, x = lower, xend = upper),
               arrow = arrow(angle = 90, ends = 'both', 
                             length = unit(0.1, 'cm'))) + #segment representing the CI
  labs(x = 'Coeffienient estimate') +
  theme_bw() 