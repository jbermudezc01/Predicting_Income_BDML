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


# Antes creamos la variable de edad al cuadrado
bd.media.imputada <-bd.media.imputada %>%
  mutate(age2 = age^2)
bd.sin.na <- bd.sin.na %>%
  mutate(age2 = age^2)

# ajustar variable maxEduc
# Calcula la media de la variable
media <- mean(df_gap_no$maxEducLevel, na.rm = TRUE)
# Imputa la media a los valores faltantes en la columna
df_gap_no$maxEducLevel[is.na(df_gap_no$maxEducLevel)] <- media
# ajustar estrato 
estrato_na<- model.matrix(~ bd.media.imputada$estrato - 1, data = bd.media.imputada)
bd.media.imputada <- cbind(bd.media.imputada, estrato_na)
colnames(bd.media.imputada)[colnames(bd.media.imputada) == "bd.media.imputada$estrato1"] <- "estrato_socioeco"

estrato_no<- model.matrix(~ bd.sin.na$estrato - 1, data = bd.sin.na)
bd.sin.na <- cbind(bd.sin.na, estrato_no)
colnames(bd.sin.na)[colnames(bd.sin.na) == "bd.sin.na$estrato1"] <- "estrato_socioeco"

# Cambio nombre base datos para mejor manejo ------------------------------
df_gap_na <- bd.media.imputada
df_gap_no <- bd.sin.na



# Regresion brecha de genero no condicional -------------------------------

# Imputando NA
reg.1.1 <- lm(log_y_salary_h ~sex, data = df_gap_na, x = TRUE)
stargazer(reg.1.1, type = "text", digits = 4)  
# Eliminando NA
reg.1.2 <- lm(log_y_salary_h ~sex, data = df_gap_no, x = TRUE)
stargazer(reg.1.2, type = "text", digits = 4)
residuos.1.2 <- residuals(reg.1.2)

# grafico valores predichos y residuos 
plot(predict(reg.1.2), residuos.1.2, xlab = "Valores Predichos", ylab = "Residuos", main = "Gráfico de Residuos vs. Valores Predichos")
abline(h = 0, col = "red", lty = 2)  # Línea horizontal en y = 0
# distribución de los residuos
qqnorm(residuos.1.2)
qqline(residuos.1.2)

# Estimacion brecha salarial condicional ----------------------------------
# Imputando NA
reg.2.1 <- lm(log_y_salary_h ~sex+age+age2+college+
             formal+estrato_socioeco+hoursWorkUsual+microEmpresa,
           data = df_gap_na, x= T)
stargazer(reg.2.1, type = "text", digits = 4)
# Eliminando Na
reg.2.2 <- lm(log_y_salary_h ~sex+age+age2+college+estrato_socioeco+
               formal+hoursWorkUsual+microEmpresa,
             data = df_gap_no, x= T)
residuos.2.2 <- residuals(reg.2.2)
stargazer(reg.2.2, type = "text", digits = 4)

# gráficos de residuos
plot(predict(reg.2.2), residuos.2.2, xlab = "Valores Predichos", ylab = "Residuos", main = "Gráfico de Residuos vs. Valores Predichos")
abline(h = 0, col = "red", lty = 2)  # Línea horizontal en y = 0
# distribución de los residuos
qqnorm(residuos.2.2)
qqline(residuos.2.2)


# TABLA DE REG PARA BRECHA NO Y CONDICIONAL 

stargazer(reg.2.2, reg.1.2, title = "Brecha salarial de género 
          condicional y no condicional", out = "Gender_gap.doc", type = "text", digits = 4)



#estimar la brecha salarial condicional por el teorema FWL-------------------

# base de datos imputando NA --------------

### estimar la brecha salarial condicional por el teorema FWL ########

# convertir las variables de explicativas a variables numericas 
df_gap_no <- df_gap_no %>% mutate(sex=as.numeric(sex),
                                  cuentaPropia=as.numeric(formal),
                                  microEmpresa=as.numeric(microEmpresa),
                                  estrato_socioeco=as.numeric(estrato_socioeco),
                                  college=as.numeric(college))

# utilizar base de datos Sin Na ---------------

# hacer una regresión con  la variable sexo  en X2-X8  y tomar los residuos 

df_gap_no <- df_gap_no %>%
  mutate(sex_residual = lm(sex ~ sex+age+age2+college+
                             formal+estrato_socioeco+hoursWorkUsual+microEmpresa, , data = .)$residuals)


# hacer una regresión con la variable y=log_y_salary_h en X2-X8 y tomar los residuos

df_gap_no <- df_gap_no %>%
  mutate(salary_residual = lm(log_y_salary_h ~ age+age2+college+
                                formal+estrato_socioeco+hoursWorkUsual+microEmpresa, , data = .)$residuals)

# hacer una regresión entre salary_residual y sex_residual 
reg.3.1 <- lm(salary_residual ~ sex_residual, data = df_gap_no )
stargazer(reg.2.2, reg.3.1, type = "text", digits = 7)

# estimaciones del modelo FWL 

coef_FWL <- reg.3.1$coefficients
coef_FWL

# comprobamos residuos SS
sum(resid(reg.2.2)^2) 
sum(resid(reg.3.1)^2) # residuos iguales 

# comprobamos el error estándar 
sqrt(diag(vcov(reg.2.2)))[2] 
sqrt(diag(vcov(reg.3.1)))[2]


#### FWL con boostrap #####

# sembrar semilla 
set.seed(123)
B <- 2000 # Número de repeticiones 
e_reg3.1 <- rep(0,B) # vector donde gurdaremos las estimaciones 

# crear bucle que tome una muestra de tamaño n con reemplazo, estimar el coficiente de interés
# y guardar en el vector vacío

for(i in 1:B){
  db_sample<- sample_frac(df_gap_no,size=1,replace=TRUE) 
  m1<-lm(salary_residual~sex_residual,data= db_sample)
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


##### estimar edad-salario pico por género ---------------------

# traer etimaciones edad-salario del punto (3) 

# estimados la edad pico para mujeres ---------------------


# base datos con na imputados 
sub_set_sex <- subset(df_gap_na, sex == "0")
edad_salario_w <- lm(log_y_salary_h ~age+age2+college+
                       formal+estrato_socioeco+hoursWorkUsual+microEmpresa, data = sub_set_sex, x = TRUE)

# Estimar con Na eliminados
su_set_sex <- subset(df_gap_no, sex == "1")
edad_salario_w_no <- lm(log_y_salary_h ~age+age2+college+
                          formal+estrato_socioeco+hoursWorkUsual+microEmpresa,, data = su_set_sex, x = TRUE)

# creamos lista de coeficientes para la estimación con bd con na imputados 

coef_na <- edad_salario_w$coefficients
coef_no <- edad_salario_w_no$coefficients

# estimamos edad pico  con base de datos con na imputados 
coef_na
b1_i <-coef_na[2]
b2_i <-coef_na[3]
edad_pico <-(-b1_i/(2*b2_i))
edad_pico # 50.9

# estimamos edad pico  con base de datos con na eliminados 
coef_no
b1_i <-coef_no[2]
b2_i <-coef_no[3]
edad_pico_w <-(-b1_i/(2*b2_i))
edad_pico_w # 44.6

# estimamos edad pico para los hombres ------------------------

# Estimar para media imputada

sub_set_m <- subset(df_gap_na, sex == "1")
edad_salario_m <- lm(log_y_salary_h ~age+age2+college+
                       formal+estrato_socioeco+hoursWorkUsual+microEmpresa,, data = sub_set_m, x = TRUE)

# Estimar con Na eliminados
su_set_m <- subset(df_gap_no, sex == "2")
edad_salario_m_no <- lm(log_y_salary_h ~age+age2+college+
                          formal+estrato_socioeco+hoursWorkUsual+microEmpresa,, data = su_set_m, x = TRUE)

# creamos lista de coeficientes para la estimación con bd con na imputados 

coef_na_m <- edad_salario_m$coefficients
coef_no_m<- edad_salario_m_no$coefficients

# estimamos edad pico  con base de datos con na imputados 
coef_na_m
b1_i_m <-coef_na_m[2]
b2_i_m <-coef_na_m[3]
edad_pico_m <-(-b1_i_m/(2*b2_i_m))
edad_pico_m # 52.6


# estimamos edad pico  con base de datos con na eliminados 
coef_no_m
b1_i_m_n <-coef_no_m[2]
b2_i_m_n<-coef_no_m[3]
edad_pico_m_n <-(-b1_i_m_n/(2*b2_i_m_n))
edad_pico_m_n # 47.6


### Estimar los intervalos 

### intervalos para edad pico mujeres 

p_load("boot")
#boot(data, statistic, R)

funcion_pa_sex<-function(data,index){
  b1<-coef(lm(log_y_salary_h~age+age2+college+
                formal+estrato_socioeco+hoursWorkUsual+microEmpresa, data = data, subset = index))[2]
  b2<-coef(lm(log_y_salary_h~age+age2+college+
                formal+estrato_socioeco+hoursWorkUsual+microEmpresa, data = data, subset = index))[3]
  (-b1/(2*b2)) 
}
# confirmamos la validez  de la función 
funcion_pa_sex(sub_set_sex, 1:nrow(sub_set_sex))


#Hacemos el boot de forma automática con nuestra función
boot_s<-boot(su_set_sex, funcion_pa_sex, R=2000) # base de datos sin na

#Creación del dataframe para generar los intervalos de confianza:
matrix.1 = matrix(ncol = 0, nrow = 2000)
i_c_w<-data.frame(matrix.1)

#Intervalos para base de datos sin NA 
se_s<-as.numeric(sqrt(var(boot_s[["t"]])))
i_c_w$bootstrap_s<-boot_s[["t"]]
i_c_w$L_i_s<-boot_s[["t"]]-qnorm(0.05/2)*se_s
i_c_w$l_s_s<-boot_s[["t"]]+qnorm(0.05/2)*se_s

# intervalos de confianza para los hombres

#Hacemos el boot de forma automática con nuestra función
boot_y<-boot(su_set_m, funcion_pa_sex, R=2000) # base de datos sin na

#Creación del dataframe para generar los intervalos de confianza:
matrix.2 = matrix(ncol = 0, nrow = 2000)
i_c_m<-data.frame(matrix.2)

#Intervalos para base de datos sin NA 
se_y<-as.numeric(sqrt(var(boot_y[["t"]])))
i_c_m$bootstrap_y<-boot_y[["t"]]
i_c_m$L_i_y<-boot_y[["t"]]-qnorm(0.05/2)*se_y
i_c_m$l_s_y<-boot_y[["t"]]+qnorm(0.05/2)*se_y







  