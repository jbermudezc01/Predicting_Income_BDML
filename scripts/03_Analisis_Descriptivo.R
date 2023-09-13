# Cargar librerias --------------------------------------------------------
library(tidyverse) # Librerias del tidyverse, principalmente dplyr para manejo de datos
library(rvest)     #Para web scrapping
library(ggplot2)
library(skimr)
library(stargazer)
# Cargar la base de datos -------------------------------------------------
# La base de datos la obtuvimos haciendo web scrapping
load(paste0(getwd(),'/stores/bd_income.RData'))




##Gráficos de dispersión
ggplot(data = bd_income , mapping = aes(x = age , y = y_salary_m_hu,  color=as.factor(sex)))+
  geom_point()
ggplot(data = bd_income , mapping = aes(x = age , y = log_y_salary_h,  color=as.factor(sex)))+
  geom_point()
ggplot(data = bd_income , mapping = aes(x = age , y = log(y_salary_m_hu),  color=as.factor(sex)))+
  geom_point()

#histogramas
ggplot(bd_income, aes(x = y_salary_m_hu)) + 
  geom_histogram()

ggplot(bd_income, aes(x = log_y_salary_h)) + 
  geom_histogram()

ggplot(bd_income,aes(x = log(y_salary_m_hu))) + 
  geom_histogram()
