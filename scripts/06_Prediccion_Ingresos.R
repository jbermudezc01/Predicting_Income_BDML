##########################################################
# Predecir los ingresos
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
library(tidymodels)

# Cargar la base de datos -------------------------------------------------
# Las bases de datos que obtuvimos al filtrar
# bd.sin.na <- read_csv("stores/bd.sin.na.csv")
# bd.media.imputada <- read_csv("stores/bd.media.imputada.csv")
load(file=paste0(getwd(),'/stores/bases.tratadas.RData'))

# Por el momento el analisis se realizara sobre la base sin na
bd.interes <- bd.sin.na
# Primero creamos algunas variables de interes para las especificaciones
factonum <- function(x){
  if(!inherits(x, 'factor')) stop('Variable no es factor')
  return(as.numeric(as.character(x)))
}
bd.interes <- bd.interes %>% 
  mutate(age2 = age^2, age3 = age^3, age4 = age^4, age5 = age^5, age6 = age^6, age7 = age^7, age8 = age^8, 
         age9 = age^9, age10 = age^10, hoursWorkUsual2 = hoursWorkUsual^2, hoursWorkUsual3 = hoursWorkUsual^3 , 
         hoursWorkUsual4 = hoursWorkUsual^4, hoursWorkUsual5 = hoursWorkUsual^5, hoursWorkUsual6 =hoursWorkUsual^6,
         hoursWorkUsual7 = hoursWorkUsual^7, hoursWorkUsual8 = hoursWorkUsual^8, hoursWorkUsual9 = hoursWorkUsual^9,
         hoursWorkUsual10 = hoursWorkUsual^10,
         sexeduc = factonum(sex)*factonum(college), sexsalud = factonum(sex)*factonum(salud),
         sexempresa = factonum(sex)*factonum(microEmpresa), sexformal = factonum(sex)*factonum(formal)) %>% 
  mutate_at(c('sexeduc','sexsalud','sexempresa'),as.factor)

# Creacion de recipes -----------------------------------------------------
# Para poder utilizar correctamente las funciones de tidymodels debemos crear las recipes, donde especificaremos
# los modelos que se van a estimar. Vamos a crear en primer lugar las anteriores especificaciones
# rec.age,   salario ~ edad + edad^2
# rec.sex,    salario ~ sexo
# rec.sexage, salario ~ sexo + edad + edad^2
# rec.1,      salario ~ sexo + edad + edad^2 + maxEducLevel + estrato + hoursWorkUsual + microEmpresa
# rec.2,      salario ~ sexo + edad + edad^2 + maxEducLevel + estrato + hoursWorkUsual + microEmpresa +
#                       salud + seguridadsocial
# rec.3,      salario ~ sexo + edad + edad^2 + maxEducLevel + estrato + hoursWorkUsual + microEmpresa +
#                       sub.transporte + sub.familiar + sub.educativo + sub.alimentacion
# rec.4,      salario ~ sexo + edad + edad^2 + maxEducLevel + estrato + hoursWorkUsual + microEmpresa +
#                       oficio + sexo*college + sexo*salud + sexo*microEmpresa + college + salud
# rec.5,      salario ~ sexo + edad + edad^2 + edad^3 + edad^4 + maxEducLevel  + estrato + hoursWorkUsual + 
#                       microEmpresa + sub.transporte + sub.familiar + sub.educativo + sub.alimentacion + relab
# rec.6,      salario ~ todas las variables anteriores + interacciones + potencia 3 y 4 de edad

rec.age    <- recipe(log_y_salary_h ~ age + age2, data= bd.interes)

rec.sex    <- recipe(log_y_salary_h ~ sex, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.sexage <- recipe(log_y_salary_h ~ age + age2 + sex, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.1      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.2      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.3      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion , data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.4      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal, 
                       data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.5      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal + relab,
                     data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.6      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal + relab + 
                       sizeFirm + formal, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.7      <- recipe(log_y_salary_h ~ age + age2 + age3 + age4 + age5 + sex + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal + relab + 
                       sizeFirm + formal, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.8      <- recipe(log_y_salary_h ~ age + age2 + age3 + age4 + age5 + age6 + age7 + age8 + age9 + age10 + sex + maxEducLevel +
                       estrato + hoursWorkUsual + hoursWorkUsual2 +hoursWorkUsual3 +hoursWorkUsual4+hoursWorkUsual5 +hoursWorkUsual6 +
                       hoursWorkUsual7 + hoursWorkUsual8 + hoursWorkUsual9 + hoursWorkUsual10 + microEmpresa + salud + seguridadsocial + 
                       sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion  + sexeduc + sexsalud + sexempresa + sexformal + relab +
                       sizeFirm + formal, data= bd.interes) %>%
  step_dummy(all_nominal_predictors())

# El modelo de estimacion es lineal
lm.mod <- linear_reg()

# Creacion de flujos de trabajo -------------------------------------------

wf.age <- workflow() %>%
  add_recipe(rec.age) %>%
  add_model(lm.mod)

wf.sex <- workflow() %>% 
  add_recipe(rec.sex) %>% 
  add_model(lm.mod)

wf.sexage <- workflow() %>% 
  add_recipe(rec.sexage) %>% 
  add_model(lm.mod)

wf.1 <- workflow() %>% 
  add_recipe(rec.1) %>% 
  add_model(lm.mod)

wf.2 <- workflow() %>% 
  add_recipe(rec.2) %>% 
  add_model(lm.mod)

wf.3 <- workflow() %>% 
  add_recipe(rec.3) %>% 
  add_model(lm.mod)

wf.4 <- workflow() %>% 
  add_recipe(rec.4) %>% 
  add_model(lm.mod)

wf.5 <- workflow() %>% 
  add_recipe(rec.5) %>% 
  add_model(lm.mod)

wf.6 <- workflow() %>% 
  add_recipe(rec.6) %>% 
  add_model(lm.mod)

wf.7 <- workflow() %>% 
  add_recipe(rec.7) %>% 
  add_model(lm.mod)

wf.8 <- workflow() %>% 
  add_recipe(rec.8) %>% 
  add_model(lm.mod)
# Simple Cross-Validation -------------------------------------------------
# Semilla para reproducibilidad
set.seed(10101)
# El conjunto de entrenamiento tendra el 70% de los datos
data_split <- initial_split(bd.interes, prop = .7)
# Crear dos dataframes, uno para train y otro para test
train <- training(data_split)
test  <- testing(data_split)

# Entrenar y evaluar cada modelo
fit.age <- wf.age %>%
  fit(data = train)
test.age <- predict(fit.age , new_data = test) %>% 
  bind_cols(test)
test_rmse.age <- rmse(test.age, truth = log_y_salary_h, estimate = .pred)
rmse.age <- test_rmse.age$.estimate

fit.sex <- wf.sex %>%
  fit(data = train)
test.sex <- predict(fit.sex , new_data = test) %>% 
  bind_cols(test)
test_rmse.sex <- rmse(test.sex, truth = log_y_salary_h, estimate = .pred)
rmse.sex <- test_rmse.sex$.estimate

fit.sexage <- wf.sexage %>%
  fit(data = train)
test.sexage <- predict(fit.sexage , new_data = test) %>% 
  bind_cols(test)
test_rmse.sexage <- rmse(test.sexage, truth = log_y_salary_h, estimate = .pred)
rmse.sexage <- test_rmse.sexage$.estimate

fit.1 <- wf.1 %>%
  fit(data = train)
test.1<- predict(fit.1, new_data = test) %>% 
  bind_cols(test)
test_rmse.1 <- rmse(test.1, truth = log_y_salary_h, estimate = .pred)
rmse.1 <- test_rmse.1$.estimate

fit.2 <- wf.2 %>%
  fit(data = train)
test.2<- predict(fit.2, new_data = test) %>% 
  bind_cols(test)
test_rmse.2 <- rmse(test.2, truth = log_y_salary_h, estimate = .pred)
rmse.2 <- test_rmse.2$.estimate

fit.3 <- wf.3 %>%
  fit(data = train)
test.3<- predict(fit.3, new_data = test) %>% 
  bind_cols(test)
test_rmse.3 <- rmse(test.3, truth = log_y_salary_h, estimate = .pred)
rmse.3 <- test_rmse.3$.estimate

fit.4 <- wf.4 %>%
  fit(data = train)
test.4<- predict(fit.4, new_data = test) %>% 
  bind_cols(test)
test_rmse.4 <- rmse(test.4, truth = log_y_salary_h, estimate = .pred)
rmse.4 <-test_rmse.4$.estimate

fit.5 <- wf.5 %>%
  fit(data = train)
test.5<- predict(fit.5, new_data = test) %>% 
  bind_cols(test)
test_rmse.5 <- rmse(test.5, truth = log_y_salary_h, estimate = .pred)
rmse.5 <- test_rmse.5$.estimate

fit.6 <- wf.6 %>%
  fit(data = train)
test.6<- predict(fit.6, new_data = test) %>% 
  bind_cols(test)
test_rmse.6 <- rmse(test.6, truth = log_y_salary_h, estimate = .pred)
rmse.6 <- test_rmse.6$.estimate

fit.7 <- wf.7 %>%
  fit(data = train)
test.7 <- predict(fit.7, new_data = test) %>% 
  bind_cols(test)
test_rmse.7 <- rmse(test.7, truth = log_y_salary_h, estimate = .pred)
rmse.7 <- test_rmse.7$.estimate

fit.8 <- wf.8 %>%
  fit(data = train)
test.8 <- predict(fit.8, new_data = test) %>% 
  bind_cols(test)
test_rmse.8 <- rmse(test.8, truth = log_y_salary_h, estimate = .pred)
rmse.8 <- test_rmse.8$.estimate

# Dataframe con RMSE 
df.rmse <- data.frame('Workflow' = c('wf.sex','wf.age','wf.agesex',paste0('wf.',1:8)),
                      'RMSE' = c(rmse.sex, rmse.age, rmse.sexage,unlist(mget(paste0('rmse.',1:8))) ))
df.rmse <- df.rmse %>% mutate(MSE = RMSE^2)


# Las dos recetas que minimizan el RMSE son :
workflows.min <- df.rmse$Workflow[order(df.rmse$RMSE)[1:2]]

# LOOCV -------------------------------------------------------------------
# Para las dos recetas que minimizaron el simple CV vamos a realizar LOOCV
# Primero para el que minimizo el RMSE en el CV
loocv.preds.min <- vector("numeric", length = nrow(bd.interes))
for(i in seq_len(nrow(bd.interes))) loocv.preds.min[i] <- predict(((get(workflows.min[1])) %>% fit(data = bd.interes[-i, ])), new_data = slice(bd.interes, i))$.pred
loocv.rmse.min <- rmse(bind_cols(bd.interes$log_y_salary_h, loocv.preds.min), truth = ...1, estimate = ...2)
loocv.rmse.min$.estimate

# Ahora con el segundo minimo
loocv.preds.2 <- vector("numeric", length = nrow(bd.interes))
for(i in seq_len(nrow(bd.interes))) loocv.preds.2[i] <- predict(((get(workflows.min[2])) %>% fit(data = bd.interes[-i, ])), new_data = slice(bd.interes, i))$.pred
loocv.rmse.2 <- rmse(bind_cols(bd.interes$log_y_salary_h, loocv.preds.2), truth = ...1, estimate = ...2)
loocv.rmse.2$.estimate

#____________________________________________________________________-

# USANDO LM y no tidymodels
##--------------------------------------------------
#Modelos de entrenamiento
mod.sex    <- lm(log_y_salary_h ~ sex, data= train)
mod.age    <- lm(log_y_salary_h ~ age + age2, data= train)
mod.sexage <- lm(log_y_salary_h ~ age + age2 + sex, data= train) 
mod.1      <- lm(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                   estrato + hoursWorkUsual + microEmpresa, data= train) 
mod.2      <- lm(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                   estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial, data=train)

mod.3      <- lm(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                   estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                   sub.educativo + sub.alimentacion , data= train) 
mod.4      <- lm(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                   estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                   sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal, data= train)

mod.5      <- lm(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                   estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                   sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal + relab, data= train)

mod.6      <- lm(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  
                   estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                   sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal + relab + 
                   sizeFirm + formal, data= train)
mod.7     <-lm(log_y_salary_h ~ age + age2 + age3 + age4 + age5 + sex + maxEducLevel +  
                 estrato + hoursWorkUsual + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                 sub.educativo + sub.alimentacion + sexeduc + sexsalud + sexempresa + sexformal + relab + 
                 sizeFirm + formal, data= train)

mod.9   <- lm(log_y_salary_h ~ poly(age,10, raw=T) + sex + maxEducLevel +
                estrato + poly(hoursWorkUsual,10,raw=T) + microEmpresa + salud + seguridadsocial + sub.transporte + sub.familiar +
                sub.educativo + sub.alimentacion  + sexeduc + sexsalud + sexempresa + sexformal + relab +
                sizeFirm + formal, data= train)
  


# Modelos de prueba prediccion --------------------------------------------
test$mod.sex<-predict(mod.sex,newdata = test)
test$mod.age<-predict(mod.age,newdata = test)
test$mod.sexage<-predict(mod.sexage,newdata = test)
test$mod.1<-predict(mod.1,newdata = test)
test$mod.2<-predict(mod.2,newdata = test)
test$mod.3<-predict(mod.3,newdata = test)
test$mod.4<-predict(mod.4,newdata = test)
test$mod.5<-predict(mod.5,newdata = test)
test$mod.6<-predict(mod.6,newdata = test)
test$mod.7<-predict(mod.7,newdata = test)
test$mod.9<-predict(mod.9,newdata = test)

#Calculemos los MSE
mse.sex<-with(test,mean((log_y_salary_h-mod.sex)^2))
mse.age<-with(test,mean((log_y_salary_h-mod.age)^2))
mse.sexage<-with(test,mean((log_y_salary_h-mod.sexage)^2))
mse.1<-with(test,mean((log_y_salary_h-mod.1)^2))
mse.2<-with(test,mean((log_y_salary_h-mod.2)^2))
mse.3<-with(test,mean((log_y_salary_h-mod.3)^2))
mse.4<-with(test,mean((log_y_salary_h-mod.4)^2))
mse.5<-with(test,mean((log_y_salary_h-mod.5)^2))
mse.6<-with(test,mean((log_y_salary_h-mod.6)^2))
mse.7<-with(test,mean((log_y_salary_h-mod.7)^2))
mse.9<-with(test,mean((log_y_salary_h-mod.9)^2))

mse<-c(mse.sex,mse.age,mse.sexage,mse.1,mse.2, mse.3,mse.4,mse.5,mse.6, mse.7, mse.9)
df<-data.frame(model=factor(c("modelage","modelsex", "modelsexage","model 1","model 2", "model 3", "model 4",
                              "model 5","model 6", "model 7", "model 8"), ordered=TRUE), MSE=mse)
df$RMSE<-sqrt(df$MSE)

R2<-c(r2.sex<-summary(mod.sex)$r.squared,
      r2.age<-summary(mod.age)$r.squared,
      r2.sexage<-summary(mod.sexage)$r.squared,
      r2.mod1<-summary(mod.1)$r.squared,
      r2.mod2<-summary(mod.2)$r.squared,
      r2.mod3<-summary(mod.3)$r.squared,
      r2.mod4<-summary(mod.4)$r.squared,
      r2.mod5<-summary(mod.5)$r.squared,
      r2.mod6<-summary(mod.6)$r.squared,
      r2.mod7<-summary(mod.7)$r.squared,
      r2.mod9<-summary(mod.9)$r.squared)
complejidad<-c(1:11)
df<-data.frame(complejidad,df,R2)

# Errores  ----------------------------------------------------------------
# El modelo con menor RMSE fue el modelo <mod6> por lo que vamos a ver la distribución de los errores
errores.mod6 <- (test$log_y_salary_h - test$mod.6)
plot(density(errores.mod6),lwd=3, main='Errores de predicción modelo 6', ylab='Densidad')

# RMSE para dato de entrenamiento -----------------------------------------

train$mod.sex<-predict(mod.sex,newdata = train)
train$mod.age<-predict(mod.age,newdata = train)
train$mod.sexage<-predict(mod.sexage,newdata = train)
train$mod.1<-predict(mod.1,newdata = train)
train$mod.2<-predict(mod.2,newdata = train)
train$mod.3<-predict(mod.3,newdata = train)
train$mod.4<-predict(mod.4,newdata = train)
train$mod.5<-predict(mod.5,newdata = train)
train$mod.6<-predict(mod.6,newdata = train)
train$mod.7<-predict(mod.7,newdata = train)
train$mod.9<-predict(mod.9,newdata = train)


#Calculemos los MSE
train.mse.sex    <-with(train,mean((log_y_salary_h-mod.sex)^2))
train.mse.age    <-with(train,mean((log_y_salary_h-mod.age)^2))
train.mse.sexage <-with(train,mean((log_y_salary_h-mod.sexage)^2))
train.mse.1      <-with(train,mean((log_y_salary_h-mod.1)^2))
train.mse.2      <-with(train,mean((log_y_salary_h-mod.2)^2))
train.mse.3      <-with(train,mean((log_y_salary_h-mod.3)^2))
train.mse.4      <-with(train,mean((log_y_salary_h-mod.4)^2))
train.mse.5      <-with(train,mean((log_y_salary_h-mod.5)^2))
train.mse.6      <-with(train,mean((log_y_salary_h-mod.6)^2))
train.mse.7      <-with(train,mean((log_y_salary_h-mod.7)^2))
train.mse.9      <-with(train,mean((log_y_salary_h-mod.9)^2))

train.mse <- c(train.mse.sex,train.mse.age,train.mse.sexage,train.mse.1,train.mse.2, train.mse.3,train.mse.4,train.mse.5,train.mse.6, train.mse.7, train.mse.9)
df.train  <-data.frame(model=factor(c("modelage","modelsex", "modelsexage","model 1","model 2", "model 3", "model 4",
                              "model 5","model 6", "model 7", "model 8"), ordered=TRUE), MSE= train.mse)
df.train$RMSE<-sqrt(df.train$MSE)
df.train$complejidad <- complejidad

# Graficas MSE  -----------------------------------------------------------

plot(df.train$RMSE, type='lines',col='red', xlab = 'Complejidad', ylab='RMSE', main = 'RMSE train vs RMSE test'); lines(df$RMSE, type='lines', col='blue')
legend("topright", legend=c("Train RMSE", "Test RMSE"), col=c("red", "blue"), lty=1)
axis(1, at = 1:11)

