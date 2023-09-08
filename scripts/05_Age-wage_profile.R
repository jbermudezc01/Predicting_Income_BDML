

# Cargar librerias --------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)     #Para web scrapping
library(ggplot2)
library(skimr)
library(stargazer)
# Cargar la base de datos -------------------------------------------------
# La base de datos la obtuvimos haciendo web scrapping
load(paste0(getwd(),'/stores/bd_income.RData'))

#Estimación de la regresión log(w)=b1+b2age+b3age^2
bd_income$age2=(bd_income$age)^2

###Estimando con log_y_salary_h
modelo1 <- lm(log_y_salary_h ~age+age2, data = bd_income, x = TRUE)

##Estimando con y_salary_m_hu
modelo2<-lm(y_salary_m_hu ~ age+age2, data = bd_income, x = TRUE)

##Estimando con log(y_salary_m_hu)
modelo3<-lm(log(y_salary_m_hu)~age+age2, data = bd_income, x = TRUE)

bd_income$x<-log(bd_income$y_salary_m_hu)
modelo4<-lm(x~ age+age2, data = bd_income, x = TRUE)
stargazer(modelo1,modelo2, modelo3,modelo4, type='text')






