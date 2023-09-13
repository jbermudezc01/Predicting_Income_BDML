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
  mutate(age2 = age^2, age3 = age^3, age4 = age^4,sexeduc = factonum(sex)*factonum(college), 
         sexsalud = factonum(sex)*factonum(salud),sexempresa = factonum(sex)*factonum(microEmpresa)) %>% 
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
#                       microEmpresa + sub.transporte + sub.familiar + sub.educativo + sub.alimentacion
# rec.6,      salario ~ todas las variables anteriores

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
                       estrato + hoursWorkUsual + microEmpresa + sub.transporte + sub.familiar +
                       sub.educativo + sub.alimentacion, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.4      <- recipe(log_y_salary_h ~ age + age2 + sex + maxEducLevel +  college + salud +
                       estrato + hoursWorkUsual + microEmpresa + oficio + sexeduc + sexsalud + sexempresa, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.5      <- recipe(log_y_salary_h ~ age + age2 + sex + age3 + age4 + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + sub.transporte + sub.familiar + sub.educativo + 
                       sub.alimentacion, data= bd.interes) %>% 
  step_dummy(all_nominal_predictors())

rec.6      <- recipe(log_y_salary_h ~ age + age2 + sex + age3 + age4 + maxEducLevel +  
                       estrato + hoursWorkUsual + microEmpresa + sub.transporte + sub.familiar + sub.educativo + sub.alimentacion +
                       salud + seguridadsocial + college + oficio + sexeduc + sexsalud + sexempresa+
                       sizeFirm  + ingtot + formal, data= bd.interes) %>% 
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

# Dataframe con RMSE 
df.rmse <- data.frame('Workflow' = c('wf.age','wf.sex','wf.agesex',paste0('wf.',1:6)),
                      'RMSE' = c(rmse.age, rmse.sex, rmse.sexage,unlist(mget(paste0('rmse.',1:6))) ))
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
