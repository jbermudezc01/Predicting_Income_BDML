##########################################################
# Brechas salariales de genero
# Autores: Juan Pablo Bermudez. Lina Bautista. Esteban Meza. Pharad Sebastian Escobar
##########################################################


# Limpieza area de trabajo ------------------------------------------------
rm(list=ls())
cat('\014')

# Librerias ---------------------------------------------------------------
require(pacman)
p_load("tidyverse","rio","stargazer","rvest", "ggplot2","skimr", "boot")

# Base de datos -----------------------------------------------------------
# bd.sin.na <- read_csv("stores/bd.sin.na.csv")
# bd.media.imputada <- read_csv("stores/bd.media.imputada.csv")
load(file=paste0(getwd(),'/stores/bases.tratadas.RData'))

# Cambio nombre base datos para mejor manejo ------------------------------
# Antes creamos la variable de edad al cuadrado
bd.media.imputada <- bd.media.imputada %>% 
  mutate(age2 = age^2)
bd.sin.na <- bd.sin.na %>% 
  mutate(age2 = age^2)
df_gap_na <- bd.media.imputada
df_gap_no <- bd.sin.na


# Regresion brecha de genero no condicional -------------------------------
# Imputando NA
reg1.1 <- lm(log_y_salary_h ~sex, data = df_gap_na, x = TRUE)
stargazer(reg1.1, type = "text", digits = 2)  
# Eliminando NA
reg1.2 <- lm(log_y_salary_h ~sex, data = df_gap_no, x = TRUE)
stargazer(reg1.2, type = "text", digits = 2) 

# Estimacion brecha salarial condicional ----------------------------------
# Imputando NA
reg2.1 <- lm(log_y_salary_h ~sex+age+maxEducLevel+
             cuentaPropia+estrato+
             hoursWorkUsual+microEmpresa,
           data = df_gap_na, x= T)
stargazer(reg2.1, type = "text", digits = 2)
# Eliminando Na
reg2.2 <- lm(log_y_salary_h ~sex+age+maxEducLevel+estrato+
               hoursWorkUsual+microEmpresa,
             data = df_gap_no, x= T)


### estimar la brecha salarial condicional por el teorema FWL ########

# convertir las variables de explicativas a variables numericas 
df_gap_na <- df_gap_na %>% mutate(sex=as.numeric(sex),
                                  cuentaPropia=as.numeric(cuentaPropia),
                                  microEmpresa=as.numeric(microEmpresa))
# utilizar base de datos imputando NA 

# hacer una regresión con  la variable sexo  en X2-X6  y tomar los residuos 

df_gap_na <- df_gap_na %>%
  mutate(sex_residual = lm(sex ~ age+maxEducLevel+estrato+cuentaPropia+
                             hoursWorkUsual+microEmpresa , data = .)$residuals)


# hacer una regresión con la variable y=log_y_salary_h en X2-X6 y tomar los residuos

df_gap_na <- df_gap_na %>%
  mutate(salary_residual = lm(log_y_salary_h ~ age+maxEducLevel+estrato+cuentaPropia+
                             hoursWorkUsual+microEmpresa , data = .)$residuals)

# hacer una regresión entre salary_residual y sex_residual 
reg3.1 <- lm(salary_residual ~ sex_residual, data = df_gap_na )
stargazer(reg2.1, reg3.1, type = "text", digits = 7)

# estimaciones del modelo FWL 

coef_FWL <- reg3.1$coefficients
coef_FWL

# comprobamos residuos SS
sum(resid(reg2.1)^2) 
sum(resid(reg3.1)^2) # residuos iguales 

# comprobamos el error estándar 
sqrt(diag(vcov(reg2.1)))[2] 
sqrt(diag(vcov(reg3.1)))[2]


#### FWL con boostrap #####

# sembrar semilla 
set.seed(123)
B <- 2000 # Número de repeticiones 
e_reg3.1 <- rep(0,B) # vector donde gurdaremos las estimaciones 

# crear bucle que tome una muestra de tamaño n con reemplazo, estimar el coficiente de interés
# y guardar en el vector vacío

for(i in 1:B){
  db_sample<- sample_frac(df_gap_na,size=1,replace=TRUE) 
  m1<-lm(salary_residual ~ sex_residual,data= db_sample)
  coefs<-m1$coefficients[2] 
  e_reg3.1[i]<-coefs 
}
# check b=2000
length(e_reg3.1)
# histograma para evaluar distribución muestral de la estimación 
plot(hist(e_reg3.1))
# obtenemos la media 
mean(e_reg3.1)
# error estandar
sqrt(var(e_reg3.1))


##### estimar edad-salario pico por género 

# crear variable age^2 en base de datos 
bd.media.imputada$age2 <- (bd.media.imputada$age)^2
bd.sin.na$age2 <- (bd.sin.na$age)^2

# traer etimaciones edad-salario del punto 3 
# estimados la edad pico para mujeres 

# base datos con na imputados 
sub_set_sex <- subset(df_gap_na, sex == "1")
edad_salario_w <- lm(log_y_salary_h ~age+age2, data = sub_set_sex, x = TRUE)

# Estimar con Na eliminados
su_set_sex <- subset(df_gap_no, sex == "1")
edad_salario_w_no <- lm(log_y_salary_h ~age+age2, data = su_set_sex, x = TRUE)

# creamos lista de coeficientes para la estimación con bd con na imputados 

coef_na <- edad_salario_w$coefficients
coef_no <- edad_salario_w_no$coefficients

# estimamos edad pico  con base de datos con na imputados 
coef_na
b1_i <-coef_na[2]
b2_i <-coef_na[3]
edad_pico <-(-b1_i/(2*b2_i))
edad_pico # 51.7

# estimamos edad pico  con base de datos con na eliminados 
coef_no
b1_i <-coef_no[2]
b2_i <-coef_no[3]
edad_pico_w <-(-b1_i/(2*b2_i))
edad_pico_w # 49.5 

# estimamos edad pico para los hombres 

sub_set_m <- subset(bd.media.imputada, sex == "0")
edad_salario_m <- lm(log_y_salary_h ~age+age2, data = sub_set_m, x = TRUE)

# Estimar con Na eliminados
su_set_m <- subset(bd.sin.na, sex == "0")
edad_salario_m_no <- lm(log_y_salary_h ~age+age2, data = su_set_m, x = TRUE)

# creamos lista de coeficientes para la estimación con bd con na imputados 

coef_na_m <- edad_salario_m$coefficients
coef_no_m<- edad_salario_m_no$coefficients

# estimamos edad pico  con base de datos con na imputados 
coef_na_m
b1_i_m <-coef_na_m[2]
b2_i_m <-coef_na_m[3]
edad_pico_m <-(-b1_i_m/(2*b2_i_m))
edad_pico_m # 51.7

# estimamos edad pico  con base de datos con na eliminados 
coef_no_m
b1_i_m_n <-coef_no_m[2]
b2_i_m_n<-coef_no_m[3]
edad_pico_m_n <-(-b1_i_m_n/(2*b2_i_m_n))
edad_pico_m_n # 49.5 

df_gap_na$sex



  