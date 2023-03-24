
#------------------------------------------------------------------------------#
#
#                       3. REGRESSION INCOMES
#
#------------------------------------------------------------------------------#

#Predicción de ingresos - Intento de predecir primero los ingresos

#Ajuste de datos y base --------      
  
train_h2 <- train_h %>% mutate(Pobre=factor(Pobre_Pobre,levels=c(0,1),labels=c("No_Pobre","Pobre")))
train_h2 <- train_h2 %>% select( -Nper, -num_oc_hogar, -Pobre_No_Pobre, -Pobre_Pobre, -Clase_Urbano, -Vivienda_Propia_paga, -sexo_jefe_hogar_Hombre, -nivel_edu_jefe_hogar_Ninguno, -jefe_hogar_des_No, -jefe_hogar_oc_No, -jefe_hogar_ina_No, -Hacinamiento_No, -jefe_hogar_oc_Si)
train_h2$ingresos_no <- if_else(train_h2$Ingtotugarr==0, 1, 0) ##detectamos hogares sin ingresos 
train_h2 <- train_h2 %>% subset(ingresos_no==0) #nos quedamos con hogares con ingresos 
train_h2 <- train_h2 %>% mutate(Log_ing = log(Ingtotugarr),
                                Log_ing2 = log(Ingpcug),
                                edad_2 = edad_jefe_hogar^2)

train_h2$N_personas_hog <- train_h2$Npersug

c <- sum(train_h2$ingresos_no)

glimpse(train_h2)

#Train/test/eval (70/20/10) - BD Hogares -------

set.seed(10110)
index_3 <- createDataPartition(y = train_h2$Log_ing , p = 0.7)[[1]]
train_hh2<- train_h2[index_3,]
other_2 <- train_h2[-index_3,]


set.seed(10110)
index_4<- createDataPartition(y = other_2$Log_ing , p = 1/3)[[1]]
test_hh2 <- other_2[index_4,]
eval_hh2 <- other_2[-index_4,]


dim(train_h2)   
dim(train_hh2)
dim(test_hh2)
dim(eval_hh2)

dim(train_h2)[1] - dim(train_hh2)[1] - dim(test_hh2)[1] - dim(eval_hh2)[1]

#Estandarizacion-----------

train_hhs2 <- train_hh2 #Guardamos las tres BD Originales aparte
test_hhs2  <- test_hh2  
eval_hhs2  <- eval_hh2
glimpse(train_hhs2)

variables_numericas <- c("num_cuartos", "num_cuartos_dormir", "Npersug",
                         "edad_jefe_hogar", "num_Menores_edad", "num_adulto_mayor", 
                         "Numper_por_dor", "Ocupados_por_perhog", "edad_2")



escalador <- preProcess(train_hhs2[, variables_numericas],
                        method = c("center", "scale"))

train_hhs2[, variables_numericas] <- predict(escalador, train_hh2[, variables_numericas])
test_hhs2[, variables_numericas] <- predict(escalador, test_hh2[, variables_numericas])
eval_hhs2[, variables_numericas] <- predict(escalador, eval_hh2[, variables_numericas])  


#Control------------------------------------------------------------------------

grilla2 <- 10^seq(10, -1, length = 100)


control2 <- trainControl(method = "cv", number = 5)


#1 - LM ---------------------------------------------------------

set.seed(10110)
modelo_1 <-   train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + Ocupados_por_perhog +
                      Clase_Rural + sexo_jefe_hogar_Mujer + nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si,
                    data = train_hhs2,
                    method = "lm",
                    metric = "RMSE",
                    trControl = control2,
                    preProcess = NULL)
modelo_1


train_hhs2$modelo_1 <- predict(modelo_1, newdata = train_hhs2)
test_hhs2$modelo_1  <- predict(modelo_1, newdata = test_hhs2)
eval_hhs2$modelo_1  <- predict(modelo_1, newdata = eval_hhs2)


train_hhs2$y_hat_1 <- exp(train_hhs2$modelo_1)/train_hhs2$N_personas_hog
test_hhs2$y_hat_1  <- exp(test_hhs2$modelo_1)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_1  <- exp(eval_hhs2$modelo_1)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_1 <- if_else(train_hhs2$y_hat_1<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_1  <- if_else(test_hhs2$y_hat_1<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_1  <- if_else(eval_hhs2$y_hat_1<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_1=factor(Pobre_1,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_1=factor(Pobre_1,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_1=factor(Pobre_1,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_1  <- Accuracy(y_pred = train_hhs2$Pobre_1, y_true = train_hhs2$Pobre)
acc_test_1   <- Accuracy(y_pred = test_hhs2$Pobre_1, y_true = test_hhs2$Pobre)
acc_eval_1   <- Accuracy(y_pred = eval_hhs2$Pobre_1, y_true = eval_hhs2$Pobre)

rec_train_1 <- Recall(y_pred = train_hhs2$Pobre_1, y_true = train_hhs2$Pobre)
rec_test_1  <- Recall(y_pred = test_hhs2$Pobre_1, y_true = test_hhs2$Pobre)
rec_eval_1  <- Recall(y_pred = eval_hhs2$Pobre_1, y_true = eval_hhs2$Pobre)

f1_train_1 <- F1_Score(y_pred = train_hhs2$Pobre_1, y_true = train_hhs2$Pobre)
f1_test_1  <- F1_Score(y_pred = test_hhs2$Pobre_1, y_true = test_hhs2$Pobre)
f1_eval_1  <- F1_Score(y_pred = eval_hhs2$Pobre_1, y_true = eval_hhs2$Pobre)

metricas_train_1 <- data.frame(Modelo = "LM simple", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_1,
                               "Accuracy" = acc_train_1,
                               "F1" = f1_train_1)

metricas_test_1 <- data.frame(Modelo = "LM simple", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_1,
                              "Accuracy" = acc_test_1,
                              "F1" = f1_test_1)

metricas_eval_1 <- data.frame(Modelo = "LM simple", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_1,
                              "Accuracy" = acc_eval_1,
                              "F1" = f1_eval_1)

metricas_1 <- bind_rows(metricas_train_1, metricas_test_1, metricas_eval_1)
metricas <- bind_rows(metricas_1)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)



#2 - LM mayor complejidad ---------------------------------------------------------

set.seed(10110)
modelo_2 <-   train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + edad_jefe_hogar + edad_2 + 
                      num_Menores_edad + num_adulto_mayor + Numper_por_dor + Ocupados_por_perhog +
                      Clase_Rural + Vivienda_Propia_No_Paga + Vivienda_Arriendo + Vivienda_Usufructo +
                      Vivienda_Ocupante_No_Dueño + Vivienda_Otra + sexo_jefe_hogar_Mujer + 
                      nivel_edu_jefe_hogar_Basica_primaria + nivel_edu_jefe_hogar_Basica_secundaria + nivel_edu_jefe_hogar_Media+
                      nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si + jefe_hogar_ina_Si + Hacinamiento_Si + Npersug*Hacinamiento_Si + 
                      sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Media + sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Superior + Clase_Rural*sexo_jefe_hogar_Mujer + 
                      Clase_Rural*nivel_edu_jefe_hogar_Basica_primaria + Clase_Rural*nivel_edu_jefe_hogar_Basica_secundaria + Clase_Rural*nivel_edu_jefe_hogar_Superior + 
                      edad_2*sexo_jefe_hogar_Mujer + Vivienda_Arriendo*Hacinamiento_Si,
                    data = train_hhs2,
                    method = "lm",
                    metric = "RMSE",
                    trControl = control2,
                    preProcess = NULL)
summary(modelo_2)
modelo_2


train_hhs2$modelo_2 <- predict(modelo_2, newdata = train_hhs2)
test_hhs2$modelo_2  <- predict(modelo_2, newdata = test_hhs2)
eval_hhs2$modelo_2  <- predict(modelo_2, newdata = eval_hhs2)


train_hhs2$y_hat_2 <- exp(train_hhs2$modelo_2)/train_hhs2$N_personas_hog
test_hhs2$y_hat_2  <- exp(test_hhs2$modelo_2)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_2  <- exp(eval_hhs2$modelo_2)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_2 <- if_else(train_hhs2$y_hat_2<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_2  <- if_else(test_hhs2$y_hat_2<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_2  <- if_else(eval_hhs2$y_hat_2<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_2=factor(Pobre_2,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_2=factor(Pobre_2,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_2=factor(Pobre_2,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_2  <- Accuracy(y_pred = train_hhs2$Pobre_2, y_true = train_hhs2$Pobre)
acc_test_2   <- Accuracy(y_pred = test_hhs2$Pobre_2, y_true = test_hhs2$Pobre)
acc_eval_2   <- Accuracy(y_pred = eval_hhs2$Pobre_2, y_true = eval_hhs2$Pobre)

rec_train_2 <- Recall(y_pred = train_hhs2$Pobre_2, y_true = train_hhs2$Pobre)
rec_test_2  <- Recall(y_pred = test_hhs2$Pobre_2, y_true = test_hhs2$Pobre)
rec_eval_2  <- Recall(y_pred = eval_hhs2$Pobre_2, y_true = eval_hhs2$Pobre)

f1_train_2 <- F1_Score(y_pred = train_hhs2$Pobre_2, y_true = train_hhs2$Pobre)
f1_test_2  <- F1_Score(y_pred = test_hhs2$Pobre_2, y_true = test_hhs2$Pobre)
f1_eval_2  <- F1_Score(y_pred = eval_hhs2$Pobre_2, y_true = eval_hhs2$Pobre)


metricas_train_2 <- data.frame(Modelo = "LM Complejo", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_2,
                               "Accuracy" = acc_train_2,
                               "F1" = f1_train_2)

metricas_test_2 <- data.frame(Modelo = "LM Complejo", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_2,
                              "Accuracy" = acc_test_2,
                              "F1" = f1_test_2)

metricas_eval_2 <- data.frame(Modelo = "LM Complejo", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_2,
                              "Accuracy" = acc_eval_2,
                              "F1" = f1_eval_2)

metricas_2 <- bind_rows(metricas_train_2, metricas_test_2, metricas_eval_2)
metricas <- bind_rows(metricas_1, metricas_2)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)


#3 - RIDGE---------------------------------------------------------

modelo_3<-train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + Ocupados_por_perhog +
                  Clase_Rural + sexo_jefe_hogar_Mujer + nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si,
                data=train_hhs2,
                method = 'glmnet',
                metric = "RMSE",
                trControl = control2,
                tuneGrid = expand.grid(alpha = 0, lambda = grilla2),
                preProcess = NULL)
 


summary(Modelo_3)

coef_ridge <- coef(Modelo_3$finalModel,Modelo_3$bestTune$lambda)
coef_ridge

train_hhs2$modelo_3 <- predict(modelo_3, newdata = train_hhs2)
test_hhs2$modelo_3  <- predict(modelo_3, newdata = test_hhs2)
eval_hhs2$modelo_3  <- predict(modelo_3, newdata = eval_hhs2)

train_hhs2$y_hat_3 <- exp(train_hhs2$modelo_3)/train_hhs2$N_personas_hog
test_hhs2$y_hat_3  <- exp(test_hhs2$modelo_3)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_3  <- exp(eval_hhs2$modelo_3)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_3 <- if_else(train_hhs2$y_hat_3<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_3  <- if_else(test_hhs2$y_hat_3<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_3  <- if_else(eval_hhs2$y_hat_3<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_3=factor(Pobre_3,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_3=factor(Pobre_3,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_3=factor(Pobre_3,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_3  <- Accuracy(y_pred = train_hhs2$Pobre_3, y_true = train_hhs2$Pobre)
acc_test_3   <- Accuracy(y_pred = test_hhs2$Pobre_3, y_true = test_hhs2$Pobre)
acc_eval_3   <- Accuracy(y_pred = eval_hhs2$Pobre_3, y_true = eval_hhs2$Pobre)

rec_train_3 <- Recall(y_pred = train_hhs2$Pobre_3, y_true = train_hhs2$Pobre)
rec_test_3  <- Recall(y_pred = test_hhs2$Pobre_3, y_true = test_hhs2$Pobre)
rec_eval_3  <- Recall(y_pred = eval_hhs2$Pobre_3, y_true = eval_hhs2$Pobre)

f1_train_3 <- F1_Score(y_pred = train_hhs2$Pobre_3, y_true = train_hhs2$Pobre)
f1_test_3  <- F1_Score(y_pred = test_hhs2$Pobre_3, y_true = test_hhs2$Pobre)
f1_eval_3  <- F1_Score(y_pred = eval_hhs2$Pobre_3, y_true = eval_hhs2$Pobre)


metricas_train_3 <- data.frame(Modelo = "RIDGE simple", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_3,
                               "Accuracy" = acc_train_3,
                               "F1" = f1_train_3)

metricas_test_3 <- data.frame(Modelo = "RIDGE simple", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_3,
                              "Accuracy" = acc_test_3,
                              "F1" = f1_test_3)

metricas_eval_3 <- data.frame(Modelo = "RIDGE simple", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_3,
                              "Accuracy" = acc_eval_3,
                              "F1" = f1_eval_3)

metricas_3 <- bind_rows(metricas_train_3, metricas_test_3, metricas_eval_3)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)


#4 - RIDGE Mayor complejidad---------------------------------------------------------

modelo_4<-train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + edad_jefe_hogar + edad_2 + 
                  num_Menores_edad + num_adulto_mayor + Numper_por_dor + Ocupados_por_perhog +
                  Clase_Rural + Vivienda_Propia_No_Paga + Vivienda_Arriendo + Vivienda_Usufructo +
                  Vivienda_Ocupante_No_Dueño + Vivienda_Otra + sexo_jefe_hogar_Mujer + 
                  nivel_edu_jefe_hogar_Basica_primaria + nivel_edu_jefe_hogar_Basica_secundaria + nivel_edu_jefe_hogar_Media+
                  nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si + jefe_hogar_ina_Si + Hacinamiento_Si + Npersug*Hacinamiento_Si + 
                  sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Media + sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Superior + Clase_Rural*sexo_jefe_hogar_Mujer + 
                  Clase_Rural*nivel_edu_jefe_hogar_Basica_primaria + Clase_Rural*nivel_edu_jefe_hogar_Basica_secundaria + Clase_Rural*nivel_edu_jefe_hogar_Superior + 
                  edad_2*sexo_jefe_hogar_Mujer + Vivienda_Arriendo*Hacinamiento_Si,
                data=train_hhs2,
                method = 'glmnet',
                metric = "RMSE",
                trControl = control2,
                tuneGrid = expand.grid(alpha = 0, lambda = grilla2),
                preProcess = NULL)



summary(Modelo_4)

coef_ridge <- coef(Modelo_4$finalModel,Modelo_4$bestTune$lambda)
coef_ridge

train_hhs2$modelo_4 <- predict(modelo_4, newdata = train_hhs2)
test_hhs2$modelo_4  <- predict(modelo_4, newdata = test_hhs2)
eval_hhs2$modelo_4  <- predict(modelo_4, newdata = eval_hhs2)

train_hhs2$y_hat_4 <- exp(train_hhs2$modelo_4)/train_hhs2$N_personas_hog
test_hhs2$y_hat_4  <- exp(test_hhs2$modelo_4)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_4  <- exp(eval_hhs2$modelo_4)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_4 <- if_else(train_hhs2$y_hat_4<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_4  <- if_else(test_hhs2$y_hat_4<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_4  <- if_else(eval_hhs2$y_hat_4<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_4=factor(Pobre_4,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_4=factor(Pobre_4,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_4=factor(Pobre_4,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_4  <- Accuracy(y_pred = train_hhs2$Pobre_4, y_true = train_hhs2$Pobre)
acc_test_4   <- Accuracy(y_pred = test_hhs2$Pobre_4, y_true = test_hhs2$Pobre)
acc_eval_4   <- Accuracy(y_pred = eval_hhs2$Pobre_4, y_true = eval_hhs2$Pobre)

rec_train_4 <- Recall(y_pred = train_hhs2$Pobre_4, y_true = train_hhs2$Pobre)
rec_test_4  <- Recall(y_pred = test_hhs2$Pobre_4, y_true = test_hhs2$Pobre)
rec_eval_4  <- Recall(y_pred = eval_hhs2$Pobre_4, y_true = eval_hhs2$Pobre)

f1_train_4 <- F1_Score(y_pred = train_hhs2$Pobre_4, y_true = train_hhs2$Pobre)
f1_test_4  <- F1_Score(y_pred = test_hhs2$Pobre_4, y_true = test_hhs2$Pobre)
f1_eval_4  <- F1_Score(y_pred = eval_hhs2$Pobre_4, y_true = eval_hhs2$Pobre)


metricas_train_4 <- data.frame(Modelo = "RIDGE Complejo", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_4,
                               "Accuracy" = acc_train_4,
                               "F1" = f1_train_4)

metricas_test_4 <- data.frame(Modelo = "RIDGE Complejo", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_4,
                              "Accuracy" = acc_test_4,
                              "F1" = f1_test_4)

metricas_eval_4 <- data.frame(Modelo = "RIDGE Complejo", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_4,
                              "Accuracy" = acc_eval_4,
                              "F1" = f1_eval_4)

metricas_4 <- bind_rows(metricas_train_4, metricas_test_4, metricas_eval_4)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

#5 - LASSO---------------------------------------------------------

modelo_5<-train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + Ocupados_por_perhog +
                  Clase_Rural + sexo_jefe_hogar_Mujer + nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si,
                data=train_hhs2,
                method = 'glmnet',
                metric = "RMSE",
                trControl = control2,
                tuneGrid = expand.grid(alpha = 1, lambda = grilla2),
                preProcess = NULL)



summary(modelo_5)

coef_lasso <- coef(modelo_5$finalModel,modelo_5$bestTune$lambda)
coef_lasso

train_hhs2$modelo_5 <- predict(modelo_5, newdata = train_hhs2)
test_hhs2$modelo_5  <- predict(modelo_5, newdata = test_hhs2)
eval_hhs2$modelo_5  <- predict(modelo_5, newdata = eval_hhs2)

train_hhs2$y_hat_5 <- exp(train_hhs2$modelo_5)/train_hhs2$N_personas_hog
test_hhs2$y_hat_5  <- exp(test_hhs2$modelo_5)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_5  <- exp(eval_hhs2$modelo_5)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_5 <- if_else(train_hhs2$y_hat_5<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_5  <- if_else(test_hhs2$y_hat_5<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_5  <- if_else(eval_hhs2$y_hat_5<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_5=factor(Pobre_5,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_5=factor(Pobre_5,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_5=factor(Pobre_5,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_5  <- Accuracy(y_pred = train_hhs2$Pobre_5, y_true = train_hhs2$Pobre)
acc_test_5   <- Accuracy(y_pred = test_hhs2$Pobre_5, y_true = test_hhs2$Pobre)
acc_eval_5   <- Accuracy(y_pred = eval_hhs2$Pobre_5, y_true = eval_hhs2$Pobre)

rec_train_5 <- Recall(y_pred = train_hhs2$Pobre_5, y_true = train_hhs2$Pobre)
rec_test_5  <- Recall(y_pred = test_hhs2$Pobre_5, y_true = test_hhs2$Pobre)
rec_eval_5  <- Recall(y_pred = eval_hhs2$Pobre_5, y_true = eval_hhs2$Pobre)

f1_train_5 <- F1_Score(y_pred = train_hhs2$Pobre_5, y_true = train_hhs2$Pobre)
f1_test_5  <- F1_Score(y_pred = test_hhs2$Pobre_5, y_true = test_hhs2$Pobre)
f1_eval_5  <- F1_Score(y_pred = eval_hhs2$Pobre_5, y_true = eval_hhs2$Pobre)


metricas_train_5 <- data.frame(Modelo = "LASSO", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_5,
                               "Accuracy" = acc_train_5,
                               "F1" = f1_train_5)

metricas_test_5 <- data.frame(Modelo = "LASSO", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_5,
                              "Accuracy" = acc_test_5,
                              "F1" = f1_test_5)

metricas_eval_5 <- data.frame(Modelo = "LASSO", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_5,
                              "Accuracy" = acc_eval_5,
                              "F1" = f1_eval_5)

metricas_5 <- bind_rows(metricas_train_5, metricas_test_5, metricas_eval_5)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)



#6 - ELASTIC NET---------------------------------------------------------

modelo_6<-train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + Ocupados_por_perhog +
                  Clase_Rural + sexo_jefe_hogar_Mujer + nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si,
                data=train_hhs2,
                method = 'glmnet',
                metric = "RMSE",
                trControl = control2,
                tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), lambda = grilla2),
                preProcess = NULL)



summary(modelo_6)

coef_lasso <- coef(modelo_6$finalModel,modelo_6$bestTune$lambda)
coef_lasso

train_hhs2$modelo_6 <- predict(modelo_6, newdata = train_hhs2)
test_hhs2$modelo_6  <- predict(modelo_6, newdata = test_hhs2)
eval_hhs2$modelo_6  <- predict(modelo_6, newdata = eval_hhs2)

train_hhs2$y_hat_6 <- exp(train_hhs2$modelo_6)/train_hhs2$N_personas_hog
test_hhs2$y_hat_6  <- exp(test_hhs2$modelo_6)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_6  <- exp(eval_hhs2$modelo_6)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_6 <- if_else(train_hhs2$y_hat_6<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_6  <- if_else(test_hhs2$y_hat_6<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_6  <- if_else(eval_hhs2$y_hat_6<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_6=factor(Pobre_6,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_6=factor(Pobre_6,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_6=factor(Pobre_6,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_6  <- Accuracy(y_pred = train_hhs2$Pobre_6, y_true = train_hhs2$Pobre)
acc_test_6   <- Accuracy(y_pred = test_hhs2$Pobre_6, y_true = test_hhs2$Pobre)
acc_eval_6   <- Accuracy(y_pred = eval_hhs2$Pobre_6, y_true = eval_hhs2$Pobre)

rec_train_6 <- Recall(y_pred = train_hhs2$Pobre_6, y_true = train_hhs2$Pobre)
rec_test_6  <- Recall(y_pred = test_hhs2$Pobre_6, y_true = test_hhs2$Pobre)
rec_eval_6  <- Recall(y_pred = eval_hhs2$Pobre_6, y_true = eval_hhs2$Pobre)

f1_train_6 <- F1_Score(y_pred = train_hhs2$Pobre_6, y_true = train_hhs2$Pobre)
f1_test_6  <- F1_Score(y_pred = test_hhs2$Pobre_6, y_true = test_hhs2$Pobre)
f1_eval_6  <- F1_Score(y_pred = eval_hhs2$Pobre_6, y_true = eval_hhs2$Pobre)


metricas_train_6 <- data.frame(Modelo = "EN Simple", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_6,
                               "Accuracy" = acc_train_6,
                               "F1" = f1_train_6)

metricas_test_6 <- data.frame(Modelo = "EN Simple", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_6,
                              "Accuracy" = acc_test_6,
                              "F1" = f1_test_6)

metricas_eval_6 <- data.frame(Modelo = "EN Simple", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_6,
                              "Accuracy" = acc_eval_6,
                              "F1" = f1_eval_6)

metricas_6 <- bind_rows(metricas_train_6, metricas_test_6, metricas_eval_6)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5, metricas_6)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

#7 - ELASTIC NET Mayor complejidad----------------------------------------------------------

modelo_7<-train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + edad_jefe_hogar + edad_2 + 
                  num_Menores_edad + num_adulto_mayor + Numper_por_dor + Ocupados_por_perhog +
                  Clase_Rural + Vivienda_Propia_No_Paga + Vivienda_Arriendo + Vivienda_Usufructo +
                  Vivienda_Ocupante_No_Dueño + Vivienda_Otra + sexo_jefe_hogar_Mujer + 
                  nivel_edu_jefe_hogar_Basica_primaria + nivel_edu_jefe_hogar_Basica_secundaria + nivel_edu_jefe_hogar_Media+
                  nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si + jefe_hogar_ina_Si + Hacinamiento_Si + Npersug*Hacinamiento_Si + 
                  sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Media + sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Superior + Clase_Rural*sexo_jefe_hogar_Mujer + 
                  Clase_Rural*nivel_edu_jefe_hogar_Basica_primaria + Clase_Rural*nivel_edu_jefe_hogar_Basica_secundaria + Clase_Rural*nivel_edu_jefe_hogar_Superior + 
                  edad_2*sexo_jefe_hogar_Mujer + Vivienda_Arriendo*Hacinamiento_Si,
                data=train_hhs2,
                method = 'glmnet',
                metric = "RMSE",
                trControl = control2,
                tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), lambda = grilla2),
                preProcess = NULL)



summary(modelo_7)

coef_lasso <- coef(modelo_7$finalModel,modelo_7$bestTune$lambda)
coef_lasso

train_hhs2$modelo_7 <- predict(modelo_7, newdata = train_hhs2)
test_hhs2$modelo_7  <- predict(modelo_7, newdata = test_hhs2)
eval_hhs2$modelo_7  <- predict(modelo_7, newdata = eval_hhs2)

train_hhs2$y_hat_7 <- exp(train_hhs2$modelo_7)/train_hhs2$N_personas_hog
test_hhs2$y_hat_7  <- exp(test_hhs2$modelo_7)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_7  <- exp(eval_hhs2$modelo_7)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_7 <- if_else(train_hhs2$y_hat_7<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_7  <- if_else(test_hhs2$y_hat_7<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_7  <- if_else(eval_hhs2$y_hat_7<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_7=factor(Pobre_7,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_7=factor(Pobre_7,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_7=factor(Pobre_7,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_7  <- Accuracy(y_pred = train_hhs2$Pobre_7, y_true = train_hhs2$Pobre)
acc_test_7   <- Accuracy(y_pred = test_hhs2$Pobre_7, y_true = test_hhs2$Pobre)
acc_eval_7   <- Accuracy(y_pred = eval_hhs2$Pobre_7, y_true = eval_hhs2$Pobre)

rec_train_7 <- Recall(y_pred = train_hhs2$Pobre_7, y_true = train_hhs2$Pobre)
rec_test_7  <- Recall(y_pred = test_hhs2$Pobre_7, y_true = test_hhs2$Pobre)
rec_eval_7  <- Recall(y_pred = eval_hhs2$Pobre_7, y_true = eval_hhs2$Pobre)

f1_train_7 <- F1_Score(y_pred = train_hhs2$Pobre_7, y_true = train_hhs2$Pobre)
f1_test_7  <- F1_Score(y_pred = test_hhs2$Pobre_7, y_true = test_hhs2$Pobre)
f1_eval_7  <- F1_Score(y_pred = eval_hhs2$Pobre_7, y_true = eval_hhs2$Pobre)


metricas_train_7 <- data.frame(Modelo = "EN Complejo", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_7,
                               "Accuracy" = acc_train_7,
                               "F1" = f1_train_7)

metricas_test_7 <- data.frame(Modelo = "EN Complejo", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_7,
                              "Accuracy" = acc_test_7,
                              "F1" = f1_test_7)

metricas_eval_7 <- data.frame(Modelo = "EN Complejo", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_7,
                              "Accuracy" = acc_eval_7,
                              "F1" = f1_eval_7)

metricas_7 <- bind_rows(metricas_train_7, metricas_test_7, metricas_eval_7)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5, metricas_6, metricas_7)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

#8 - ARBOL de decisión----------------------------------------------------------

p_load(rattle)
modelo_8 <- train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + edad_jefe_hogar + edad_2 + 
                    num_Menores_edad + num_adulto_mayor + Numper_por_dor + Ocupados_por_perhog +
                    Clase_Rural + Vivienda_Propia_No_Paga + Vivienda_Arriendo + Vivienda_Usufructo +
                    Vivienda_Ocupante_No_Dueño + Vivienda_Otra + sexo_jefe_hogar_Mujer + 
                    nivel_edu_jefe_hogar_Basica_primaria + nivel_edu_jefe_hogar_Basica_secundaria + nivel_edu_jefe_hogar_Media+
                    nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si + jefe_hogar_ina_Si + Hacinamiento_Si + Npersug*Hacinamiento_Si + 
                    sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Media + sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Superior + Clase_Rural*sexo_jefe_hogar_Mujer + 
                    Clase_Rural*nivel_edu_jefe_hogar_Basica_primaria + Clase_Rural*nivel_edu_jefe_hogar_Basica_secundaria + Clase_Rural*nivel_edu_jefe_hogar_Superior + 
                    edad_2*sexo_jefe_hogar_Mujer + Vivienda_Arriendo*Hacinamiento_Si,
                 data = train_hh2, 
                 method = "rpart", 
                 trControl = control2)


fancyRpartPlot(modelo_8$finalModel)


train_hhs2$modelo_8 <- predict(modelo_8, newdata = train_hhs2)
test_hhs2$modelo_8  <- predict(modelo_8, newdata = test_hhs2)
eval_hhs2$modelo_8  <- predict(modelo_8, newdata = eval_hhs2)

train_hhs2$y_hat_8 <- exp(train_hhs2$modelo_8)/train_hhs2$N_personas_hog
test_hhs2$y_hat_8  <- exp(test_hhs2$modelo_8)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_8  <- exp(eval_hhs2$modelo_8)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_8 <- if_else(train_hhs2$y_hat_8<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_8  <- if_else(test_hhs2$y_hat_8<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_8  <- if_else(eval_hhs2$y_hat_8<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_8=factor(Pobre_8,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_8=factor(Pobre_8,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_8=factor(Pobre_8,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_8  <- Accuracy(y_pred = train_hhs2$Pobre_8, y_true = train_hhs2$Pobre)
acc_test_8   <- Accuracy(y_pred = test_hhs2$Pobre_8, y_true = test_hhs2$Pobre)
acc_eval_8   <- Accuracy(y_pred = eval_hhs2$Pobre_8, y_true = eval_hhs2$Pobre)

rec_train_8 <- Recall(y_pred = train_hhs2$Pobre_8, y_true = train_hhs2$Pobre)
rec_test_8  <- Recall(y_pred = test_hhs2$Pobre_8, y_true = test_hhs2$Pobre)
rec_eval_8  <- Recall(y_pred = eval_hhs2$Pobre_8, y_true = eval_hhs2$Pobre)

f1_train_8 <- F1_Score(y_pred = train_hhs2$Pobre_8, y_true = train_hhs2$Pobre)
f1_test_8  <- F1_Score(y_pred = test_hhs2$Pobre_8, y_true = test_hhs2$Pobre)
f1_eval_8  <- F1_Score(y_pred = eval_hhs2$Pobre_8, y_true = eval_hhs2$Pobre)


metricas_train_8 <- data.frame(Modelo = "ARBOL", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_8,
                               "Accuracy" = acc_train_8,
                               "F1" = f1_train_8)

metricas_test_8 <- data.frame(Modelo = "ARBOL", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_8,
                              "Accuracy" = acc_test_8,
                              "F1" = f1_test_8)

metricas_eval_8 <- data.frame(Modelo = "ARBOL", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_8,
                              "Accuracy" = acc_eval_8,
                              "F1" = f1_eval_8)

metricas_8 <- bind_rows(metricas_train_8, metricas_test_8, metricas_eval_8)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5, metricas_6, metricas_7, metricas_8)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

#Random forest----------------------------------------------------------

##Grilla ramdom forest 

tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                           min.node.size = c(10, 30, 50,
                                             70, 100),
                           splitrule = "variance")

tunegrid_rf2 <- expand.grid(mtry = c(10, 15), 
                           min.node.size = c(50, 70, 80),
                           splitrule = "variance")



#9 - Primer grilla para tunear --------

modelo_9 <- train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + edad_jefe_hogar + edad_2 + 
                   num_Menores_edad + num_adulto_mayor + Numper_por_dor + Ocupados_por_perhog +
                   Clase_Rural + Vivienda_Propia_No_Paga + Vivienda_Arriendo + Vivienda_Usufructo +
                   Vivienda_Ocupante_No_Dueño + Vivienda_Otra + sexo_jefe_hogar_Mujer + 
                   nivel_edu_jefe_hogar_Basica_primaria + nivel_edu_jefe_hogar_Basica_secundaria + nivel_edu_jefe_hogar_Media+
                   nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si + jefe_hogar_ina_Si + Hacinamiento_Si + Npersug*Hacinamiento_Si + 
                   sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Media + sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Superior + Clase_Rural*sexo_jefe_hogar_Mujer + 
                   Clase_Rural*nivel_edu_jefe_hogar_Basica_primaria + Clase_Rural*nivel_edu_jefe_hogar_Basica_secundaria + Clase_Rural*nivel_edu_jefe_hogar_Superior + 
                   edad_2*sexo_jefe_hogar_Mujer + Vivienda_Arriendo*Hacinamiento_Si,
                 data = train_hh2, 
                 method = "ranger", 
                 trControl = control2,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_rf)

Grilla_1 <- ggplot(modelo_9$results, aes(x = min.node.size, y = RMSE, 
                             color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "Resultados del grid search",
       x = "Mínima cantidad de observaciones por hoja",
       y = "RMSE (Cross-Validation)") +
  scale_color_discrete("Número de predictores seleccionados al azar") +
  theme_bw() +
  theme(legend.position = "bottom")

train_hhs2$modelo_9 <- predict(modelo_9, newdata = train_hhs2)
test_hhs2$modelo_9  <- predict(modelo_9, newdata = test_hhs2)
eval_hhs2$modelo_9  <- predict(modelo_9, newdata = eval_hhs2)

train_hhs2$y_hat_9 <- exp(train_hhs2$modelo_9)/train_hhs2$N_personas_hog
test_hhs2$y_hat_9  <- exp(test_hhs2$modelo_9)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_9  <- exp(eval_hhs2$modelo_9)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_9 <- if_else(train_hhs2$y_hat_9<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_9  <- if_else(test_hhs2$y_hat_9<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_9  <- if_else(eval_hhs2$y_hat_9<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_9=factor(Pobre_9,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_9=factor(Pobre_9,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_9=factor(Pobre_9,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_9  <- Accuracy(y_pred = train_hhs2$Pobre_9, y_true = train_hhs2$Pobre)
acc_test_9   <- Accuracy(y_pred = test_hhs2$Pobre_9, y_true = test_hhs2$Pobre)
acc_eval_9   <- Accuracy(y_pred = eval_hhs2$Pobre_9, y_true = eval_hhs2$Pobre)

rec_train_9 <- Recall(y_pred = train_hhs2$Pobre_9, y_true = train_hhs2$Pobre)
rec_test_9  <- Recall(y_pred = test_hhs2$Pobre_9, y_true = test_hhs2$Pobre)
rec_eval_9  <- Recall(y_pred = eval_hhs2$Pobre_9, y_true = eval_hhs2$Pobre)

f1_train_9 <- F1_Score(y_pred = train_hhs2$Pobre_9, y_true = train_hhs2$Pobre)
f1_test_9  <- F1_Score(y_pred = test_hhs2$Pobre_9, y_true = test_hhs2$Pobre)
f1_eval_9  <- F1_Score(y_pred = eval_hhs2$Pobre_9, y_true = eval_hhs2$Pobre)


metricas_train_9 <- data.frame(Modelo = "RAMDOM FOREST", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_9,
                               "Accuracy" = acc_train_9,
                               "F1" = f1_train_9)

metricas_test_9 <- data.frame(Modelo = "RAMDOM FOREST", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_9,
                              "Accuracy" = acc_test_9,
                              "F1" = f1_test_9)

metricas_eval_9 <- data.frame(Modelo = "RAMDOM FOREST", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_9,
                              "Accuracy" = acc_eval_9,
                              "F1" = f1_eval_9)

metricas_9 <- bind_rows(metricas_train_9, metricas_test_9, metricas_eval_9)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5, metricas_6, metricas_7, metricas_8, metricas_9)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

#10 - Segunda grilla para tunear --------

modelo_10 <- train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + edad_jefe_hogar + edad_2 + 
                     num_Menores_edad + num_adulto_mayor + Numper_por_dor + Ocupados_por_perhog +
                     Clase_Rural + Vivienda_Propia_No_Paga + Vivienda_Arriendo + Vivienda_Usufructo +
                     Vivienda_Ocupante_No_Dueño + Vivienda_Otra + sexo_jefe_hogar_Mujer + 
                     nivel_edu_jefe_hogar_Basica_primaria + nivel_edu_jefe_hogar_Basica_secundaria + nivel_edu_jefe_hogar_Media+
                     nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si + jefe_hogar_ina_Si + Hacinamiento_Si + Npersug*Hacinamiento_Si + 
                     sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Media + sexo_jefe_hogar_Mujer*nivel_edu_jefe_hogar_Superior + Clase_Rural*sexo_jefe_hogar_Mujer + 
                     Clase_Rural*nivel_edu_jefe_hogar_Basica_primaria + Clase_Rural*nivel_edu_jefe_hogar_Basica_secundaria + Clase_Rural*nivel_edu_jefe_hogar_Superior + 
                     edad_2*sexo_jefe_hogar_Mujer + Vivienda_Arriendo*Hacinamiento_Si,
                   data = train_hh2, 
                  method = "ranger", 
                  trControl = control2,
                  metric = 'RMSE', 
                  tuneGrid = tunegrid_rf2)

Grilla_2 <- ggplot(modelo_10$results, aes(x = min.node.size, y = RMSE, 
                                         color = as.factor(mtry))) +
  geom_line() +
  geom_point() +
  labs(title = "Resultados del grid search",
       x = "Mínima cantidad de observaciones por hoja",
       y = "RMSE (Cross-Validation)") +
  scale_color_discrete("Número de predictores seleccionados al azar") +
  theme_bw() +
  theme(legend.position = "bottom")

train_hhs2$modelo_10 <- predict(modelo_10, newdata = train_hhs2)
test_hhs2$modelo_10  <- predict(modelo_10, newdata = test_hhs2)
eval_hhs2$modelo_10  <- predict(modelo_10, newdata = eval_hhs2)

train_hhs2$y_hat_10 <- exp(train_hhs2$modelo_10)/train_hhs2$N_personas_hog
test_hhs2$y_hat_10  <- exp(test_hhs2$modelo_10)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_10  <- exp(eval_hhs2$modelo_10)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_10 <- if_else(train_hhs2$y_hat_10<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_10  <- if_else(test_hhs2$y_hat_10<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_10  <- if_else(eval_hhs2$y_hat_10<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_10=factor(Pobre_10,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_10=factor(Pobre_10,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_10=factor(Pobre_10,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_10  <- Accuracy(y_pred = train_hhs2$Pobre_10, y_true = train_hhs2$Pobre)
acc_test_10   <- Accuracy(y_pred = test_hhs2$Pobre_10, y_true = test_hhs2$Pobre)
acc_eval_10   <- Accuracy(y_pred = eval_hhs2$Pobre_10, y_true = eval_hhs2$Pobre)

rec_train_10 <- Recall(y_pred = train_hhs2$Pobre_10, y_true = train_hhs2$Pobre)
rec_test_10  <- Recall(y_pred = test_hhs2$Pobre_10, y_true = test_hhs2$Pobre)
rec_eval_10  <- Recall(y_pred = eval_hhs2$Pobre_10, y_true = eval_hhs2$Pobre)

f1_train_10 <- F1_Score(y_pred = train_hhs2$Pobre_10, y_true = train_hhs2$Pobre)
f1_test_10  <- F1_Score(y_pred = test_hhs2$Pobre_10, y_true = test_hhs2$Pobre)
f1_eval_10  <- F1_Score(y_pred = eval_hhs2$Pobre_10, y_true = eval_hhs2$Pobre)


metricas_train_10 <- data.frame(Modelo = "RAMDOM FOREST Tuning", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_10,
                               "Accuracy" = acc_train_10,
                               "F1" = f1_train_10)

metricas_test_10 <- data.frame(Modelo = "RAMDOM FOREST Tuning", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_10,
                              "Accuracy" = acc_test_10,
                              "F1" = f1_test_10)

metricas_eval_10 <- data.frame(Modelo = "RAMDOM FOREST Tuning", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_10,
                              "Accuracy" = acc_eval_10,
                              "F1" = f1_eval_10)

metricas_10 <- bind_rows(metricas_train_10, metricas_test_10, metricas_eval_10)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5, metricas_6, metricas_7, metricas_8, metricas_9, metricas_10)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

#Gradient Boosting tree----------------------------------------------------------

##Grilla Boosting tree

tunegrid_gbm3 <- expand.grid(learn_rate = c(0.1, 0.01, 0.001),
                            ntrees = 100,
                            max_depth = 10,
                            col_sample_rate = 1,
                            min_rows = 100) 


#instalación
if ("package:h2o" %in% search()) {detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) {remove.packages("h2o")}
library(pacman)
p_load("RCurl","jsonlite") 
install.packages("h2o", type = "source",
                 repos = (c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

library(h2o)
h2o.init(nthreads = 2)
library(caret)

#11 - BOOSTING----------------------------------------------------------

set.seed(10110)
modelo_11 <- train(Log_ing ~ num_cuartos + num_cuartos_dormir + Npersug + Ocupados_por_perhog +
                     Clase_Rural + sexo_jefe_hogar_Mujer + nivel_edu_jefe_hogar_Superior + jefe_hogar_des_Si,
                 data = train_hh2, 
                 method = "gbm_h2o", 
                 trControl = control2,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_gbm3) 
plot(modelo_11)

train_hhs2$modelo_11 <- predict(modelo_11, newdata = train_hhs2)
test_hhs2$modelo_11  <- predict(modelo_11, newdata = test_hhs2)
eval_hhs2$modelo_11  <- predict(modelo_11, newdata = eval_hhs2)

train_hhs2$y_hat_11 <- exp(train_hhs2$modelo_11)/train_hhs2$N_personas_hog
test_hhs2$y_hat_11  <- exp(test_hhs2$modelo_11)/test_hhs2$N_personas_hog
eval_hhs2$y_hat_11  <- exp(eval_hhs2$modelo_11)/eval_hhs2$N_personas_hog

train_hhs2$Pobre_11 <- if_else(train_hhs2$y_hat_11<=train_hhs2$Lp, 1, 0)
test_hhs2$Pobre_11  <- if_else(test_hhs2$y_hat_11<=test_hhs2$Lp, 1, 0)
eval_hhs2$Pobre_11  <- if_else(eval_hhs2$y_hat_11<=eval_hhs2$Lp, 1, 0)

train_hhs2 <- train_hhs2 %>% mutate(Pobre_11=factor(Pobre_11,levels=c(0,1),labels=c("No_Pobre","Pobre")))
test_hhs2 <- test_hhs2 %>% mutate(Pobre_11=factor(Pobre_11,levels=c(0,1),labels=c("No_Pobre","Pobre")))
eval_hhs2 <- eval_hhs2 %>% mutate(Pobre_11=factor(Pobre_11,levels=c(0,1),labels=c("No_Pobre","Pobre")))

acc_train_11  <- Accuracy(y_pred = train_hhs2$Pobre_11, y_true = train_hhs2$Pobre)
acc_test_11   <- Accuracy(y_pred = test_hhs2$Pobre_11, y_true = test_hhs2$Pobre)
acc_eval_11   <- Accuracy(y_pred = eval_hhs2$Pobre_11, y_true = eval_hhs2$Pobre)

rec_train_11 <- Recall(y_pred = train_hhs2$Pobre_11, y_true = train_hhs2$Pobre)
rec_test_11  <- Recall(y_pred = test_hhs2$Pobre_11, y_true = test_hhs2$Pobre)
rec_eval_11  <- Recall(y_pred = eval_hhs2$Pobre_11, y_true = eval_hhs2$Pobre)

f1_train_11 <- F1_Score(y_pred = train_hhs2$Pobre_11, y_true = train_hhs2$Pobre)
f1_test_11  <- F1_Score(y_pred = test_hhs2$Pobre_11, y_true = test_hhs2$Pobre)
f1_eval_11  <- F1_Score(y_pred = eval_hhs2$Pobre_11, y_true = eval_hhs2$Pobre)


metricas_train_11 <- data.frame(Modelo = "BOOSTING", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Entrenamiento",
                               "Sensitivity" = rec_train_11,
                               "Accuracy" = acc_train_11,
                               "F1" = f1_train_11)

metricas_test_11 <- data.frame(Modelo = "BOOSTING", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Test",
                              "Sensitivity" = rec_test_11,
                              "Accuracy" = acc_test_11,
                              "F1" = f1_test_11)

metricas_eval_11 <- data.frame(Modelo = "BOOSTING", 
                              "Muestreo" = "---", 
                              "Evaluación" = "Evaluación",
                              "Sensitivity" = rec_eval_11,
                              "Accuracy" = acc_eval_11,
                              "F1" = f1_eval_11)

metricas_11 <- bind_rows(metricas_train_11, metricas_test_11, metricas_eval_11)
metricas <- bind_rows(metricas_1, metricas_2, metricas_3, metricas_4, metricas_5, metricas_6, metricas_7, metricas_8, metricas_9, metricas_10, metricas_11)
metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)


metricas_test <- metricas %>% filter(Evaluación == "Test") %>% as.data.frame()
print(xtable(metricas_test, digits = 4), include.rownames = FALSE)
