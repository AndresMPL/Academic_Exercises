
#------------------------------------------------------------------------------#
#
#                         2. POVERTY CLASSIFICATION
#
#------------------------------------------------------------------------------#

#Enfoque de clasificación - Intento de predecir directamente 0 (no pobre) y 1 (pobre)

#Pasos previos ----------------------------------------------------------------

  train_h <- train_h %>% mutate(Pobre=factor(Pobre_Pobre,levels=c(0,1),labels=c("No_Pobre","Pobre")))
  train_h <- train_h %>% select(-id, -Nper, -Lp,-Ingtotugarr, -Ingpcug, -num_oc_hogar, -Pobre_No_Pobre, -Pobre_Pobre, -Clase_Urbano, -Vivienda_Propia_paga, -sexo_jefe_hogar_Hombre, -nivel_edu_jefe_hogar_Ninguno, -jefe_hogar_des_No, -jefe_hogar_oc_No, -jefe_hogar_ina_No, -Hacinamiento_No, -jefe_hogar_oc_Si)
  
  glimpse(train_h)
  

#Evaluación desbalance

  prop.table(table(train_h$Pobre)) #1-Pobre, 0-No Pobre
  #Grado de desbalance Moderado
  

#Dividimos train/test/eval (70%/20%/10%) - BD Hogares

  set.seed(10110)
  index_1 <- createDataPartition(y=train_h$Pobre, p = 0.7, list = FALSE)
  train_hh  <- train_h[index_1,]
  other     <- train_h[-index_1,]
  
  set.seed(10110)
  index2  <- createDataPartition(other$Pobre, p = 1/3, list = FALSE)
  test_hh <- other[-index2,]
  eval_hh <- other[ index2,]
  
  dim(train_hh)   
  dim(test_hh)
  dim(eval_hh)
  
  dim(train_h)[1] - dim(train_hh)[1] - dim(test_hh)[1] - dim(eval_hh)[1] #Cero para verificar que las particiones hayan quedado bien
  
  prop.table(table(train_h$Pobre))    #Verificamos que las particiones conserven las mismas proporciones
  prop.table(table(train_hh$Pobre))
  prop.table(table(test_hh$Pobre))
  prop.table(table(eval_hh$Pobre))  


#Estandarizamos

  train_hhs <- train_hh #Guardamos las tres BD Originales aparte
  test_hhs  <- test_hh  
  eval_hhs  <- eval_hh

  glimpse(train_hhs)

  variables_numericas <- c("num_cuartos", "num_cuartos_dormir", "Npersug",
                           "edad_jefe_hogar", "num_Menores_edad", "num_adulto_mayor", 
                           "Numper_por_dor", "Ocupados_por_perhog")
  
  escalador <- preProcess(train_hh[, variables_numericas],
                          method = c("center", "scale"))
  
  train_hhs[, variables_numericas] <- predict(escalador, train_hh[, variables_numericas])
  test_hhs[, variables_numericas] <- predict(escalador, test_hh[, variables_numericas])
  eval_hhs[, variables_numericas] <- predict(escalador, eval_hh[, variables_numericas])  


#Control de CV-------------------------------------------------------------------

  grilla <- 10^seq(10, -1, length = 100)
  
  fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  
  control <- trainControl(method = "cv",
                          number = 5,
                          summaryFunction = fiveStats,
                          classProbs = TRUE,
                          verbose=FALSE,
                          savePredictions = T)


#1 - Logit ---------------------------------------------------------

  set.seed(10110)
  modelo1 <-   train(Pobre ~ . , 
                     data = train_hhs,
                     method = "glm",
                     trControl = control,
                     family = "binomial",
                     preProcess = NULL,
                     metric = 'Accuracy')
  modelo1
  
  y_hat_train1 <- predict(modelo1, newdata = train_hhs)
  y_hat_test1  <- predict(modelo1, newdata = test_hhs)
  y_hat_eval1  <- predict(modelo1, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval1, eval_hhs$Pobre)
  
  probs_train1 <- predict(modelo1, train_hhs, type = "prob")
  probs_test1  <- predict(modelo1, test_hhs, type = "prob")
  probs_eval1  <- predict(modelo1, eval_hhs, type = "prob")
  
  acc_train1  <- Accuracy(y_pred = y_hat_train1, y_true = train_hhs$Pobre)
  acc_test1   <- Accuracy(y_pred = y_hat_test1, y_true = test_hhs$Pobre)
  acc_eval1   <- Accuracy(y_pred = y_hat_eval1, y_true = eval_hhs$Pobre)
  
  rec_train1 <- Recall(y_pred = y_hat_train1, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test1  <- Recall(y_pred = y_hat_test1, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval1  <- Recall(y_pred = y_hat_eval1, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train1 <- F1_Score(y_pred = y_hat_train1, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test1  <- F1_Score(y_pred = y_hat_test1, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval1  <- F1_Score(y_pred = y_hat_eval1, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train1 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Sensitivity" = rec_train1,
                                "Accuracy" = acc_train1,
                                "F1" = f1_train1)
  
  metricas_test1 <- data.frame(Modelo = "Logit", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Sensitivity" = rec_test1,
                               "Accuracy" = acc_test1,
                               "F1" = f1_eval1)
  
  metricas_eval1 <- data.frame(Modelo = "Logit", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Sensitivity" = rec_eval1,
                               "Accuracy" = acc_eval1,
                               "F1" = f1_eval1)
  
  
  metricas1 <- bind_rows(metricas_train1, metricas_test1, metricas_eval1)
  metricas1 %>% kbl(digits = 4) %>% kable_styling(full_width = T)

###1.1 Logit - Upsampling ----
  
  train_hhs11 <- upSample(x = select(train_hhs, -Pobre),
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs11$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs11) 
  
  glimpse(train_hhs11) 
  
  set.seed(10110)
  modelo11 <- train(Pobre~., 
                    data = train_hhs11,
                    method = "glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  probs_train11  <- predict(modelo11, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train11[probs_train11 < 0] <- 0
  #probs_train11[probs_train11 > 1] <- 1
  
  probs_test11   <- predict(modelo11, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test11[probs_train11 < 0] <- 0
  #probs_test11[probs_train11 > 1] <- 1
  
  probs_eval11   <- predict(modelo11, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval11[probs_train11 < 0] <- 0
  #probs_eval11[probs_train11 > 1] <- 1
  
  y_hat_train11  <- as.numeric(probs_train11 > 0.5)
  y_hat_test11   <- as.numeric(probs_test11 > 0.5)
  y_hat_eval11   <- as.numeric(probs_eval11 > 0.5)
  
  acc_train11  <- Accuracy(y_pred = y_hat_train11, y_true = as.numeric(train_hhs$Pobre))
  acc_test11   <- Accuracy(y_pred = y_hat_test11, y_true = as.numeric(test_hhs$Pobre))
  acc_eval11   <- Accuracy(y_pred = y_hat_eval11, y_true = as.numeric(eval_hhs$Pobre))
  
  rec_train11 <- Recall(y_pred = y_hat_train11, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  rec_test11  <- Recall(y_pred = y_hat_test11, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  rec_eval11  <- Recall(y_pred = y_hat_eval11, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  f1_train11 <- F1_Score(y_pred = y_hat_train11, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  f1_test11  <- F1_Score(y_pred = y_hat_test11, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  f1_eval11  <- F1_Score(y_pred = y_hat_eval11, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  metricas_train11 <- data.frame(Modelo = "Logit", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train11,
                                 "Accuracy" = acc_train11,
                                 "F1" = f1_train11)
  
  metricas_test11 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test11,
                                "Accuracy" = acc_test11,
                                "F1" = f1_test11)
  
  metricas_eval11 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval11,
                                "Accuracy" = acc_eval11,
                                "F1" = f1_eval11)
  
  metricas11 <- bind_rows(metricas_train11, metricas_test11, metricas_eval11)
  metricas <- bind_rows(metricas1, metricas11)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
###1.2 Logit - Downsampling ----

  train_hhs12 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs12$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs12) 
  
  set.seed(10110)
  modelo12 <- train(Pobre~., 
                    data = train_hhs12,
                    method = "glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train12  <- predict(modelo12, train_hhs)
  y_hat_test12   <- predict(modelo12, test_hhs)
  y_hat_eval12   <- predict(modelo12, eval_hhs)
  
  confusionMatrix(y_hat_eval12, eval_hhs$Pobre)
  
  acc_train12  <- Accuracy(y_pred = y_hat_train12, y_true = train_hhs$Pobre)
  acc_test12   <- Accuracy(y_pred = y_hat_test12, y_true = test_hhs$Pobre)
  acc_eval12   <- Accuracy(y_pred = y_hat_eval12, y_true = eval_hhs$Pobre)
  
  rec_train12 <- Recall(y_pred = y_hat_train12, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test12  <- Recall(y_pred = y_hat_test12, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval12  <- Recall(y_pred = y_hat_eval12, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train12 <- F1_Score(y_pred = y_hat_train12, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test12  <- F1_Score(y_pred = y_hat_test12, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval12  <- F1_Score(y_pred = y_hat_eval12, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train12 <- data.frame(Modelo = "Logit", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train12,
                                 "Accuracy" = acc_train12,
                                 "F1" = f1_train12)
  
  metricas_test12 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test12,
                                "Accuracy" = acc_test12,
                                "F1" = f1_test12)
  
  metricas_eval12 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval12,
                                "Accuracy" = acc_eval12,
                                "F1" = f1_eval12)
  
  metricas12 <- bind_rows(metricas_train12, metricas_test12, metricas_eval12)
  metricas <- bind_rows(metricas, metricas12)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)


###1.3 Logit - Oversamplig (ROSE) ----

  rose_train13 <- ROSE(Pobre ~ ., data = train_hhs, N = nrow(train_hhs) + 69239, p = 0.5)$data
  nrow(rose_train13)
  table(rose_train13$Pobre)
  prop.table(table(rose_train13$Pobre))
  
  prop.table(table(train_hhs$Pobre))
  nrow(train_hhs)

  set.seed(10110)
  modelo13 <- train(Pobre~., 
                    data = rose_train13,
                    method = "glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train_rose13 <- predict(modelo13, newdata = train_hhs)
  y_hat_test_rose13  <- predict(modelo13, newdata = test_hhs)
  y_hat_eval_rose13  <- predict(modelo13, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval_rose13, eval_hhs$Pobre)
  
  acc_train_rose13 <- Accuracy(y_pred = y_hat_train_rose13, y_true = train_hhs$Pobre)
  acc_test_rose13  <- Accuracy(y_pred = y_hat_test_rose13, y_true = test_hhs$Pobre)
  acc_eval_rose13  <- Accuracy(y_pred = y_hat_eval_rose13, y_true = eval_hhs$Pobre)
  
  rec_train_rose13 <- Recall(y_pred = y_hat_train_rose13, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test_rose13  <- Recall(y_pred = y_hat_test_rose13, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval_rose13  <- Recall(y_pred = y_hat_eval_rose13, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train13 <- F1_Score(y_pred = y_hat_train_rose13, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test13  <- F1_Score(y_pred = y_hat_test_rose13, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval13  <- F1_Score(y_pred = y_hat_eval_rose13, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train13 <- data.frame(Modelo = "Logit", 
                                 "Muestreo" = "Oversamplig (ROSE)", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train_rose13,
                                 "Accuracy" = acc_train_rose13,
                                 "F1" = f1_train13)
  
  metricas_test13 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test_rose13,
                                "Accuracy" = acc_test_rose13,
                                "F1" = f1_test13)
  
  metricas_eval13 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval_rose13,
                                "Accuracy" = acc_eval_rose13,
                                "F1" = f1_eval13)
  
  metricas13 <- bind_rows(metricas_train13, metricas_test13, metricas_eval13)
  metricas <- bind_rows(metricas, metricas13)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
#2 - Logit - Lasso (1)------------------------------------------------------------

  set.seed(1010)
  modelo2 <- train(Pobre ~ . , 
                   data = train_hhs, 
                   method = "glmnet",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = 1,lambda=grilla))
  modelo2
  
  y_hat_train2 <- predict(modelo2, train_hhs)
  y_hat_test2  <- predict(modelo2, test_hhs)
  y_hat_eval2  <- predict(modelo2, eval_hhs)
  
  confusionMatrix(y_hat_eval2, eval_hhs$Pobre)
  
  acc_train2  <- Accuracy(y_pred = y_hat_train2, y_true = train_hhs$Pobre)
  acc_test2   <- Accuracy(y_pred = y_hat_test2, y_true = test_hhs$Pobre)
  acc_eval2   <- Accuracy(y_pred = y_hat_eval2, y_true = eval_hhs$Pobre)
  
  rec_train2 <- Recall(y_pred = y_hat_train2, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test2  <- Recall(y_pred = y_hat_test2, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval2  <- Recall(y_pred = y_hat_eval2, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train2 <- F1_Score(y_pred = y_hat_train2, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test2  <- F1_Score(y_pred = y_hat_test2, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval2  <- F1_Score(y_pred = y_hat_eval2, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train2 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Sensitivity" = rec_train2,
                                "Accuracy" = acc_train2,
                                "F1" = f1_train2)
  
  metricas_test2 <- data.frame(Modelo = "Logit - Lasso", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Sensitivity" = rec_test2,
                               "Accuracy" = acc_test2,
                               "F1" = f1_test2)
  
  metricas_eval2 <- data.frame(Modelo = "Logit - Lasso", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Sensitivity" = rec_eval2,
                               "Accuracy" = acc_eval2,
                               "F1" = f1_eval2)
  
  
  metricas2 <- bind_rows(metricas_train2, metricas_test2, metricas_eval2)
  metricas <- bind_rows(metricas, metricas2)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
  ###2.1 Logit - Lasso - Upsampling ----
  
  train_hhs21 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs21$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs21) 
  
  glimpse(train_hhs21) 
  
  set.seed(10110)
  modelo21 <- train(Pobre ~ . , 
                    data = train_hhs, 
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 1,lambda=grilla))
  
  probs_train21  <- predict(modelo21, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train21[probs_train21 < 0] <- 0
  #probs_train21[probs_train21 > 1] <- 1
  
  probs_test21   <- predict(modelo21, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test21[probs_test21 < 0] <- 0
  #probs_test21[probs_test21 > 1] <- 1
  
  probs_eval21   <- predict(modelo21, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval21[probs_eval21 < 0] <- 0
  #probs_eval21[probs_eval21 > 1] <- 1
  
  y_hat_train21  <- as.numeric(probs_train21 > 0.5)
  y_hat_test21   <- as.numeric(probs_test21 > 0.5)
  y_hat_eval21   <- as.numeric(probs_eval21 > 0.5)
  
  acc_train21  <- Accuracy(y_pred = y_hat_train21, y_true = as.numeric(train_hhs$Pobre))
  acc_test21   <- Accuracy(y_pred = y_hat_test21, y_true = as.numeric(test_hhs$Pobre))
  acc_eval21   <- Accuracy(y_pred = y_hat_eval21, y_true = as.numeric(eval_hhs$Pobre))
  
  rec_train21 <- Recall(y_pred = y_hat_train21, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  rec_test21  <- Recall(y_pred = y_hat_test21, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  rec_eval21  <- Recall(y_pred = y_hat_eval21, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  f1_train21 <- F1_Score(y_pred = y_hat_train21, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  f1_test21  <- F1_Score(y_pred = y_hat_test21, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  f1_eval21  <- F1_Score(y_pred = y_hat_eval21, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  metricas_train21 <- data.frame(Modelo = "Logit - Lasso", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train21,
                                 "Accuracy" = acc_train21,
                                 "F1" = f1_train21)
  
  metricas_test21 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test21,
                                "Accuracy" = acc_test21,
                                "F1" = f1_test21)
  
  metricas_eval21 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval21,
                                "Accuracy" = acc_eval21,
                                "F1" = f1_eval21)
  
  metricas21 <- bind_rows(metricas_train21, metricas_test21, metricas_eval21)
  metricas <- bind_rows(metricas, metricas21)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###2.2 Logit - Lasso - Downsampling ----
  
  train_hhs22 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs22$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs22) 
  
  set.seed(10110)
  modelo22 <- train(Pobre~., 
                    data = train_hhs22,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 1,lambda=grilla))
  
  y_hat_train22  <- predict(modelo22, train_hhs)
  y_hat_test22   <- predict(modelo22, test_hhs)
  y_hat_eval22   <- predict(modelo22, eval_hhs)
  
  confusionMatrix(y_hat_eval22, eval_hhs$Pobre)
  
  acc_train22  <- Accuracy(y_pred = y_hat_train22, y_true = train_hhs$Pobre)
  acc_test22   <- Accuracy(y_pred = y_hat_test22, y_true = test_hhs$Pobre)
  acc_eval22   <- Accuracy(y_pred = y_hat_eval22, y_true = eval_hhs$Pobre)
  
  rec_train22 <- Recall(y_pred = y_hat_train22, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test22  <- Recall(y_pred = y_hat_test22, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval22  <- Recall(y_pred = y_hat_eval22, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train22 <- F1_Score(y_pred = y_hat_train22, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test22  <- F1_Score(y_pred = y_hat_test22, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval22  <- F1_Score(y_pred = y_hat_eval22, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train22 <- data.frame(Modelo = "Logit - Lasso", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train22,
                                 "Accuracy" = acc_train22,
                                 "F1" = f1_train22)
  
  metricas_test22 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test22,
                                "Accuracy" = acc_test22,
                                "F1" = f1_test22)
  
  metricas_eval22 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval22,
                                "Accuracy" = acc_eval22,
                                "F1" = f1_eval22)
  
  metricas22 <- bind_rows(metricas_train22, metricas_test22, metricas_eval22)
  metricas <- bind_rows(metricas, metricas22)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
###2.3 Logit - Lasso - Oversamplig (ROSE) ----

  rose_train23 <- ROSE(Pobre ~ ., data = train_hhs, N = nrow(train_hhs) + 69239, p = 0.5)$data
  nrow(rose_train23)
  table(rose_train23$Pobre)
  prop.table(table(rose_train23$Pobre))
  
  prop.table(table(train_hhs$Pobre))
  nrow(train_hhs)
  
  set.seed(10110)
  modelo23 <- train(Pobre ~ . , 
                    data = rose_train23,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 1,lambda=grilla))
  
  y_hat_train_rose23 <- predict(modelo23, newdata = train_hhs)
  y_hat_test_rose23  <- predict(modelo23, newdata = test_hhs)
  y_hat_eval_rose23  <- predict(modelo23, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval_rose23, eval_hhs$Pobre)
  
  acc_train_rose23 <- Accuracy(y_pred = y_hat_train_rose23, y_true = train_hhs$Pobre)
  acc_test_rose23  <- Accuracy(y_pred = y_hat_test_rose23, y_true = test_hhs$Pobre)
  acc_eval_rose23  <- Accuracy(y_pred = y_hat_eval_rose23, y_true = eval_hhs$Pobre)
  
  rec_train_rose23 <- Recall(y_pred = y_hat_train_rose23, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test_rose23  <- Recall(y_pred = y_hat_test_rose23, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval_rose23  <- Recall(y_pred = y_hat_eval_rose23, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train23 <- F1_Score(y_pred = y_hat_train_rose23, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test23  <- F1_Score(y_pred = y_hat_test_rose23, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval23  <- F1_Score(y_pred = y_hat_eval_rose23, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train23 <- data.frame(Modelo = "Logit - Lasso", 
                                 "Muestreo" = "Oversamplig (ROSE)", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train_rose23,
                                 "Accuracy" = acc_train_rose23,
                                 "F1" = f1_train23)
  
  metricas_test23 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test_rose23,
                                "Accuracy" = acc_test_rose23,
                                "F1" = f1_test23)
  
  metricas_eval23 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval_rose23,
                                "Accuracy" = acc_eval_rose23,
                                "F1" = f1_eval23)
  
  metricas23 <- bind_rows(metricas_train23, metricas_test23, metricas_eval23)
  metricas <- bind_rows(metricas, metricas23)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  

#3 - Logit - Ridge (0)------------------------------------------------------------

  set.seed(10110)
  modelo3 <- train(Pobre ~ . , 
                   data = train_hhs,
                   method = "glmnet",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = 0,lambda=grilla))
  
  modelo3
  
  y_hat_train3 <- predict(modelo3, train_hhs)
  y_hat_test3  <- predict(modelo3, test_hhs)
  y_hat_eval3  <- predict(modelo3, eval_hhs)
  
  confusionMatrix(y_hat_eval3, eval_hhs$Pobre)
  
  acc_train3  <- Accuracy(y_pred = y_hat_train3, y_true = train_hhs$Pobre)
  acc_test3   <- Accuracy(y_pred = y_hat_test3, y_true = test_hhs$Pobre)
  acc_eval3   <- Accuracy(y_pred = y_hat_eval3, y_true = eval_hhs$Pobre)
  
  rec_train3 <- Recall(y_pred = y_hat_train3, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test3  <- Recall(y_pred = y_hat_test3, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval3  <- Recall(y_pred = y_hat_eval3, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train3 <- F1_Score(y_pred = y_hat_train3, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test3  <- F1_Score(y_pred = y_hat_test3, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval3  <- F1_Score(y_pred = y_hat_eval3, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train3 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Sensitivity" = rec_train3,
                                "Accuracy" = acc_train3,
                                "F1" = f1_train3)
  
  metricas_test3 <- data.frame(Modelo = "Logit - Ridge", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Sensitivity" = rec_test3,
                               "Accuracy" = acc_test3,
                               "F1" = f1_test3)
  
  metricas_eval3 <- data.frame(Modelo = "Logit - Ridge", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Sensitivity" = rec_eval3,
                               "Accuracy" = acc_eval3,
                               "F1" = f1_eval3)
  
  metricas3 <- bind_rows(metricas_train3, metricas_test3, metricas_eval3)
  metricas <- bind_rows(metricas, metricas3)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
  ###3.1 Logit - Ridge - Upsampling ----
  
  set.seed(10110)
  train_hhs31 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs31$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs31) 
  
  glimpse(train_hhs31) 
  
  set.seed(10110)
  modelo31 <- train(Pobre ~ . , 
                    data = train_hhs31,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 0,lambda=grilla))
  
  probs_train31  <- predict(modelo31, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train31[probs_train31 < 0] <- 0
  #probs_train31[probs_train31 > 1] <- 1
  
  probs_test31   <- predict(modelo31, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test31[probs_test31 < 0] <- 0
  #probs_test31[probs_test31 > 1] <- 1
  
  probs_eval31   <- predict(modelo31, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval31[probs_eval31 < 0] <- 0
  #probs_eval31[probs_eval31 > 1] <- 1
  
  y_hat_train31  <- as.numeric(probs_train31 > 0.5)
  y_hat_test31   <- as.numeric(probs_test31 > 0.5)
  y_hat_eval31   <- as.numeric(probs_eval31 > 0.5)
  
  acc_train31  <- Accuracy(y_pred = y_hat_train31, y_true = as.numeric(train_hhs$Pobre))
  acc_test31   <- Accuracy(y_pred = y_hat_test31, y_true = as.numeric(test_hhs$Pobre))
  acc_eval31   <- Accuracy(y_pred = y_hat_eval31, y_true = as.numeric(eval_hhs$Pobre))
  
  rec_train31 <- Recall(y_pred = y_hat_train31, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  rec_test31  <- Recall(y_pred = y_hat_test31, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  rec_eval31  <- Recall(y_pred = y_hat_eval31, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  f1_train31 <- F1_Score(y_pred = y_hat_train31, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  f1_test31  <- F1_Score(y_pred = y_hat_test31, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  f1_eval31  <- F1_Score(y_pred = y_hat_eval31, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  metricas_train31 <- data.frame(Modelo = "Logit - Ridge", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train31,
                                 "Accuracy" = acc_train31,
                                 "F1" = f1_train31)
  
  metricas_test31 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test31,
                                "Accuracy" = acc_test31,
                                "F1" = f1_test31)
  
  metricas_eval31 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval31,
                                "Accuracy" = acc_eval31,
                                "F1" = f1_eval31)
  
  metricas31 <- bind_rows(metricas_train31, metricas_test31, metricas_eval31)
  metricas <- bind_rows(metricas, metricas31)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###3.2 Logit - Ridge - Downsampling ----
  
  train_hhs32 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs32$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs32) 
  
  set.seed(10110)
  modelo32 <- train(Pobre~., 
                    data = train_hhs32,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 0,lambda=grilla))
  
  y_hat_train32  <- predict(modelo32, train_hhs)
  y_hat_test32   <- predict(modelo32, test_hhs)
  y_hat_eval32   <- predict(modelo32, eval_hhs)
  
  confusionMatrix(y_hat_eval32, eval_hhs$Pobre)
  
  acc_train32  <- Accuracy(y_pred = y_hat_train32, y_true = train_hhs$Pobre)
  acc_test32   <- Accuracy(y_pred = y_hat_test32, y_true = test_hhs$Pobre)
  acc_eval32   <- Accuracy(y_pred = y_hat_eval32, y_true = eval_hhs$Pobre)
  
  rec_train32 <- Recall(y_pred = y_hat_train32, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test32  <- Recall(y_pred = y_hat_test32, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval32  <- Recall(y_pred = y_hat_eval32, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train32 <- F1_Score(y_pred = y_hat_train32, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test32  <- F1_Score(y_pred = y_hat_test32, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval32  <- F1_Score(y_pred = y_hat_eval32, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train32 <- data.frame(Modelo = "Logit - Ridge", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train32,
                                 "Accuracy" = acc_train32,
                                 "F1" = f1_train32)
  
  metricas_test32 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test32,
                                "Accuracy" = acc_test32,
                                "F1" = f1_test32)
  
  metricas_eval32 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval32,
                                "Accuracy" = acc_eval32,
                                "F1" = f1_eval32)
  
  metricas32 <- bind_rows(metricas_train32, metricas_test32, metricas_eval32)
  metricas <- bind_rows(metricas, metricas32)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)


###3.3 Logit - Ridge - Oversamplig (ROSE) ----

  rose_train33 <- ROSE(Pobre ~ ., data = train_hhs, N = nrow(train_hhs) + 69339, p = 0.5)$data
  nrow(rose_train33)
  table(rose_train33$Pobre)
  prop.table(table(rose_train33$Pobre))
  
  prop.table(table(train_hhs$Pobre))
  nrow(train_hhs)
  
  set.seed(10110)
  modelo33 <- train(Pobre ~ . , 
                    data = rose_train33,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 0,lambda=grilla))
  
  y_hat_train_rose33 <- predict(modelo33, newdata = train_hhs)
  y_hat_test_rose33  <- predict(modelo33, newdata = test_hhs)
  y_hat_eval_rose33  <- predict(modelo33, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval_rose33, eval_hhs$Pobre)
  
  acc_train_rose33 <- Accuracy(y_pred = y_hat_train_rose33, y_true = train_hhs$Pobre)
  acc_test_rose33  <- Accuracy(y_pred = y_hat_test_rose33, y_true = test_hhs$Pobre)
  acc_eval_rose33  <- Accuracy(y_pred = y_hat_eval_rose33, y_true = eval_hhs$Pobre)
  
  rec_train_rose33 <- Recall(y_pred = y_hat_train_rose33, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test_rose33  <- Recall(y_pred = y_hat_test_rose33, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval_rose33  <- Recall(y_pred = y_hat_eval_rose33, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train33 <- F1_Score(y_pred = y_hat_train_rose33, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test33  <- F1_Score(y_pred = y_hat_test_rose33, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval33  <- F1_Score(y_pred = y_hat_eval_rose33, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train33 <- data.frame(Modelo = "Logit - Ridge", 
                                 "Muestreo" = "Oversamplig (ROSE)", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train_rose33,
                                 "Accuracy" = acc_train_rose33,
                                 "F1" = f1_train33)
  
  metricas_test33 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test_rose33,
                                "Accuracy" = acc_test_rose33,
                                "F1" = f1_test33)
  
  metricas_eval33 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval_rose33,
                                "Accuracy" = acc_eval_rose33,
                                "F1" = f1_eval33)
  
  metricas33 <- bind_rows(metricas_train33, metricas_test33, metricas_eval33)
  metricas <- bind_rows(metricas, metricas33)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)  
  
#4 - Logit - EN-------------------------------------------------------------------

  set.seed(10110)
  modelo4 <- train(Pobre ~ . , 
                   data = train_hhs,
                   method = "glmnet",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))
  
  modelo4
  
  y_hat_train4 <- predict(modelo4, train_hhs)
  y_hat_test4  <- predict(modelo4, test_hhs)
  y_hat_eval4  <- predict(modelo4, eval_hhs)
  
  confusionMatrix(y_hat_eval4, eval_hhs$Pobre)
  
  acc_train4  <- Accuracy(y_pred = y_hat_train4, y_true = train_hhs$Pobre)
  acc_test4   <- Accuracy(y_pred = y_hat_test4, y_true = test_hhs$Pobre)
  acc_eval4   <- Accuracy(y_pred = y_hat_eval4, y_true = eval_hhs$Pobre)
  
  rec_train4 <- Recall(y_pred = y_hat_train4, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test4  <- Recall(y_pred = y_hat_test4, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval4  <- Recall(y_pred = y_hat_eval4, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train4 <- F1_Score(y_pred = y_hat_train4, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test4  <- F1_Score(y_pred = y_hat_test4, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval4  <- F1_Score(y_pred = y_hat_eval4, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train4 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Sensitivity" = rec_train4,
                                "Accuracy" = acc_train4,
                                "F1" = f1_train4)
  
  metricas_test4 <- data.frame(Modelo = "Logit - EN", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Sensitivity" = rec_test4,
                               "Accuracy" = acc_test4,
                               "F1" = f1_test4)
  
  metricas_eval4 <- data.frame(Modelo = "Logit - EN", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Sensitivity" = rec_eval4,
                               "Accuracy" = acc_eval4,
                               "F1" = f1_eval4)
  
  metricas4 <- bind_rows(metricas_train4, metricas_test4, metricas_eval4)
  metricas <- bind_rows(metricas, metricas4)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
  ###4.1 Logit - EN - Upsampling ----
  
  train_hhs41 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs41$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs41) 
  
  glimpse(train_hhs41) 
  
  set.seed(10110)
  modelo41 <- train(Pobre ~ . , 
                    data = train_hhs41,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))
  
  probs_train41  <- predict(modelo41, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train41[probs_train41 < 0] <- 0
  #probs_train41[probs_train41 > 1] <- 1
  
  probs_test41   <- predict(modelo41, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test41[probs_test41 < 0] <- 0
  #probs_test41[probs_test41 > 1] <- 1
  
  probs_eval41   <- predict(modelo41, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval41[probs_eval41 < 0] <- 0
  #probs_eval41[probs_eval41 > 1] <- 1
  
  y_hat_train41  <- as.numeric(probs_train41 > 0.5)
  y_hat_test41   <- as.numeric(probs_test41 > 0.5)
  y_hat_eval41   <- as.numeric(probs_eval41 > 0.5)
  
  acc_train41  <- Accuracy(y_pred = y_hat_train41, y_true = as.numeric(train_hhs$Pobre))
  acc_test41   <- Accuracy(y_pred = y_hat_test41, y_true = as.numeric(test_hhs$Pobre))
  acc_eval41   <- Accuracy(y_pred = y_hat_eval41, y_true = as.numeric(eval_hhs$Pobre))
  
  rec_train41 <- Recall(y_pred = y_hat_train41, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  rec_test41  <- Recall(y_pred = y_hat_test41, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  rec_eval41  <- Recall(y_pred = y_hat_eval41, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  f1_train41 <- F1_Score(y_pred = y_hat_train41, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  f1_test41  <- F1_Score(y_pred = y_hat_test41, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  f1_eval41  <- F1_Score(y_pred = y_hat_eval41, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  metricas_train41 <- data.frame(Modelo = "Logit - EN", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train41,
                                 "Accuracy" = acc_train41,
                                 "F1" = f1_train41)
  
  metricas_test41 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test41,
                                "Accuracy" = acc_test41,
                                "F1" = f1_test41)
  
  metricas_eval41 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval41,
                                "Accuracy" = acc_eval41,
                                "F1" = f1_eval41)
  
  metricas41 <- bind_rows(metricas_train41, metricas_test41, metricas_eval41)
  metricas <- bind_rows(metricas, metricas41)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###4.2 Logit - EN - Downsampling ----
  
  train_hhs42 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs42$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs42) 
  
  set.seed(10110)
  modelo42 <- train(Pobre~., 
                    data = train_hhs42,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))
  
  y_hat_train42  <- predict(modelo42, train_hhs)
  y_hat_test42   <- predict(modelo42, test_hhs)
  y_hat_eval42   <- predict(modelo42, eval_hhs)
  
  confusionMatrix(y_hat_eval42, eval_hhs$Pobre)
  
  acc_train42  <- Accuracy(y_pred = y_hat_train42, y_true = train_hhs$Pobre)
  acc_test42   <- Accuracy(y_pred = y_hat_test42, y_true = test_hhs$Pobre)
  acc_eval42   <- Accuracy(y_pred = y_hat_eval42, y_true = eval_hhs$Pobre)
  
  rec_train42 <- Recall(y_pred = y_hat_train42, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test42  <- Recall(y_pred = y_hat_test42, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval42  <- Recall(y_pred = y_hat_eval42, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train42 <- F1_Score(y_pred = y_hat_train42, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test42  <- F1_Score(y_pred = y_hat_test42, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval42  <- F1_Score(y_pred = y_hat_eval42, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train42 <- data.frame(Modelo = "Logit - EN", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train42,
                                 "Accuracy" = acc_train42,
                                 "F1" = f1_train42)
  
  metricas_test42 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test42,
                                "Accuracy" = acc_test42,
                                "F1" = f1_test42)
  
  metricas_eval42 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval42,
                                "Accuracy" = acc_eval42,
                                "F1" = f1_eval42)
  
  metricas42 <- bind_rows(metricas_train42, metricas_test42, metricas_eval42)
  metricas <- bind_rows(metricas, metricas42)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
###4.3 Logit - EN - Oversamplig (ROSE) ----

  rose_train43 <- ROSE(Pobre ~ ., data = train_hhs, N = nrow(train_hhs) + 69439, p = 0.5)$data
  nrow(rose_train43)
  table(rose_train43$Pobre)
  prop.table(table(rose_train43$Pobre))
  
  prop.table(table(train_hhs$Pobre))
  nrow(train_hhs)
  
  set.seed(10110)
  modelo43 <- train(Pobre ~ . , 
                    data = rose_train43,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))
  
  y_hat_train_rose43 <- predict(modelo43, newdata = train_hhs)
  y_hat_test_rose43  <- predict(modelo43, newdata = test_hhs)
  y_hat_eval_rose43  <- predict(modelo43, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval_rose43, eval_hhs$Pobre)
  
  acc_train_rose43 <- Accuracy(y_pred = y_hat_train_rose43, y_true = train_hhs$Pobre)
  acc_test_rose43  <- Accuracy(y_pred = y_hat_test_rose43, y_true = test_hhs$Pobre)
  acc_eval_rose43  <- Accuracy(y_pred = y_hat_eval_rose43, y_true = eval_hhs$Pobre)
  
  rec_train_rose43 <- Recall(y_pred = y_hat_train_rose43, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test_rose43  <- Recall(y_pred = y_hat_test_rose43, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval_rose43  <- Recall(y_pred = y_hat_eval_rose43, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train43 <- F1_Score(y_pred = y_hat_train_rose43, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test43  <- F1_Score(y_pred = y_hat_test_rose43, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval43  <- F1_Score(y_pred = y_hat_eval_rose43, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train43 <- data.frame(Modelo = "Logit - EN", 
                                 "Muestreo" = "Oversamplig (ROSE)", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train_rose43,
                                 "Accuracy" = acc_train_rose43,
                                 "F1" = f1_train43)
  
  metricas_test43 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test_rose43,
                                "Accuracy" = acc_test_rose43,
                                "F1" = f1_test43)
  
  metricas_eval43 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval_rose43,
                                "Accuracy" = acc_eval_rose43,
                                "F1" = f1_eval43)
  
  metricas43 <- bind_rows(metricas_train43, metricas_test43, metricas_eval43)
  metricas <- bind_rows(metricas, metricas43)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

    
#5 - LDA ---------------------------------------------------------------------
  
  set.seed(10110)
  modelo5 <- train(Pobre ~ . , 
                   data = train_hhs,
                   method = "lda",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy')
  
  modelo5
  
  y_hat_train5 <- predict(modelo5, train_hhs)
  y_hat_test5  <- predict(modelo5, test_hhs)
  y_hat_eval5  <- predict(modelo5, eval_hhs)
  
  confusionMatrix(y_hat_eval5, eval_hhs$Pobre)
  
  acc_train5  <- Accuracy(y_pred = y_hat_train5, y_true = train_hhs$Pobre)
  acc_test5   <- Accuracy(y_pred = y_hat_test5, y_true = test_hhs$Pobre)
  acc_eval5   <- Accuracy(y_pred = y_hat_eval5, y_true = eval_hhs$Pobre)
  
  rec_train5 <- Recall(y_pred = y_hat_train5, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test5  <- Recall(y_pred = y_hat_test5, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval5  <- Recall(y_pred = y_hat_eval5, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train5 <- F1_Score(y_pred = y_hat_train5, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test5  <- F1_Score(y_pred = y_hat_test5, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval5  <- F1_Score(y_pred = y_hat_eval5, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train5 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Sensitivity" = rec_train5,
                                "Accuracy" = acc_train5,
                                "F1" = f1_train5)
  
  metricas_test5 <- data.frame(Modelo = "LDA", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Sensitivity" = rec_test5,
                               "Accuracy" = acc_test5,
                               "F1" = f1_test5)
  
  metricas_eval5 <- data.frame(Modelo = "LDA", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Sensitivity" = rec_eval5,
                               "Accuracy" = acc_eval5,
                               "F1" = f1_eval5)
  
  metricas5 <- bind_rows(metricas_train5, metricas_test5, metricas_eval5)
  metricas <- bind_rows(metricas, metricas5)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###5.1 LDA - Upsampling ----
  
  train_hhs51 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs51$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs51) 
  
  glimpse(train_hhs51) 
  
  set.seed(10110)
  modelo51 <- train(Pobre ~ . , 
                    data = train_hhs51,
                    method = "lda",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  probs_train51  <- predict(modelo51, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train51[probs_train51 < 0] <- 0
  #probs_train51[probs_train51 > 1] <- 1
  
  probs_test51   <- predict(modelo51, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test51[probs_test51 < 0] <- 0
  #probs_test51[probs_test51 > 1] <- 1
  
  probs_eval51   <- predict(modelo51, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval51[probs_eval51 < 0] <- 0
  #probs_eval51[probs_eval51 > 1] <- 1
  
  y_hat_train51  <- as.numeric(probs_train51 > 0.5)
  y_hat_test51   <- as.numeric(probs_test51 > 0.5)
  y_hat_eval51   <- as.numeric(probs_eval51 > 0.5)
  
  acc_train51  <- Accuracy(y_pred = y_hat_train51, y_true = as.numeric(train_hhs$Pobre))
  acc_test51   <- Accuracy(y_pred = y_hat_test51, y_true = as.numeric(test_hhs$Pobre))
  acc_eval51   <- Accuracy(y_pred = y_hat_eval51, y_true = as.numeric(eval_hhs$Pobre))
  
  rec_train51 <- Recall(y_pred = y_hat_train51, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  rec_test51  <- Recall(y_pred = y_hat_test51, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  rec_eval51  <- Recall(y_pred = y_hat_eval51, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  f1_train51 <- F1_Score(y_pred = y_hat_train51, y_true = as.numeric(train_hhs$Pobre), positive = 1)
  f1_test51  <- F1_Score(y_pred = y_hat_test51, y_true = as.numeric(test_hhs$Pobre), positive = 1)
  f1_eval51  <- F1_Score(y_pred = y_hat_eval51, y_true = as.numeric(eval_hhs$Pobre), positive = 1)
  
  metricas_train51 <- data.frame(Modelo = "LDA", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train51,
                                 "Accuracy" = acc_train51,
                                 "F1" = f1_train51)
  
  metricas_test51 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test51,
                                "Accuracy" = acc_test51,
                                "F1" = f1_test51)
  
  metricas_eval51 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval51,
                                "Accuracy" = acc_eval51,
                                "F1" = f1_eval51)
  
  metricas51 <- bind_rows(metricas_train51, metricas_test51, metricas_eval51)
  metricas <- bind_rows(metricas, metricas51)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###5.2 LDA - Downsampling ----
  
  train_hhs52 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs52$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs52) 

  set.seed(10110)
  modelo52 <- train(Pobre~., 
                    data = train_hhs52,
                    method = "lda",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train52  <- predict(modelo52, train_hhs)
  y_hat_test52   <- predict(modelo52, test_hhs)
  y_hat_eval52   <- predict(modelo52, eval_hhs)
  
  confusionMatrix(y_hat_eval52, eval_hhs$Pobre)
  
  acc_train52  <- Accuracy(y_pred = y_hat_train52, y_true = train_hhs$Pobre)
  acc_test52   <- Accuracy(y_pred = y_hat_test52, y_true = test_hhs$Pobre)
  acc_eval52   <- Accuracy(y_pred = y_hat_eval52, y_true = eval_hhs$Pobre)
  
  rec_train52 <- Recall(y_pred = y_hat_train52, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test52  <- Recall(y_pred = y_hat_test52, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval52  <- Recall(y_pred = y_hat_eval52, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train52 <- F1_Score(y_pred = y_hat_train52, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test52  <- F1_Score(y_pred = y_hat_test52, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval52  <- F1_Score(y_pred = y_hat_eval52, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train52 <- data.frame(Modelo = "LDA", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train52,
                                 "Accuracy" = acc_train52,
                                 "F1" = f1_train52)
  
  metricas_test52 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test52,
                                "Accuracy" = acc_test52,
                                "F1" = f1_test52)
  
  metricas_eval52 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval52,
                                "Accuracy" = acc_eval52,
                                "F1" = f1_eval52)
  
  metricas52 <- bind_rows(metricas_train52, metricas_test52, metricas_eval52)
  metricas <- bind_rows(metricas, metricas52)
  
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)

  
###5.3 LDA - Oversamplig (ROSE) ----
  
  rose_train53 <- ROSE(Pobre ~ ., data = train_hhs, N = nrow(train_hhs) + 69539, p = 0.5)$data
  nrow(rose_train53)
  table(rose_train53$Pobre)
  prop.table(table(rose_train53$Pobre))
  
  prop.table(table(train_hhs$Pobre))
  nrow(train_hhs)
  
  set.seed(10110)
  modelo53 <- train(Pobre ~ . , 
                    data = rose_train53,
                    method = "lda",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train_rose53 <- predict(modelo53, newdata = train_hhs)
  y_hat_test_rose53  <- predict(modelo53, newdata = test_hhs)
  y_hat_eval_rose53  <- predict(modelo53, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval_rose53, eval_hhs$Pobre)
  
  acc_train_rose53 <- Accuracy(y_pred = y_hat_train_rose53, y_true = train_hhs$Pobre)
  acc_test_rose53  <- Accuracy(y_pred = y_hat_test_rose53, y_true = test_hhs$Pobre)
  acc_eval_rose53  <- Accuracy(y_pred = y_hat_eval_rose53, y_true = eval_hhs$Pobre)
  
  rec_train_rose53 <- Recall(y_pred = y_hat_train_rose53, y_true = train_hhs$Pobre, positive = "Pobre")
  rec_test_rose53  <- Recall(y_pred = y_hat_test_rose53, y_true = test_hhs$Pobre, positive = "Pobre")
  rec_eval_rose53  <- Recall(y_pred = y_hat_eval_rose53, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  f1_train53 <- F1_Score(y_pred = y_hat_train_rose53, y_true = train_hhs$Pobre, positive = "Pobre")
  f1_test53  <- F1_Score(y_pred = y_hat_test_rose53, y_true = test_hhs$Pobre, positive = "Pobre")
  f1_eval53  <- F1_Score(y_pred = y_hat_eval_rose53, y_true = eval_hhs$Pobre, positive = "Pobre")
  
  metricas_train53 <- data.frame(Modelo = "LDA", 
                                 "Muestreo" = "Oversamplig (ROSE)", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train_rose53,
                                 "Accuracy" = acc_train_rose53,
                                 "F1" = f1_train53)
  
  metricas_test53 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test_rose53,
                                "Accuracy" = acc_test_rose53,
                                "F1" = f1_test53)
  
  metricas_eval53 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval_rose53,
                                "Accuracy" = acc_eval_rose53,
                                "F1" = f1_eval53)
  
  metricas53 <- bind_rows(metricas_train53, metricas_test53, metricas_eval53)
  metricas <- bind_rows(metricas, metricas53)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  #6 - Árboles -------------------------------------------------------------------
  
  #Vamos a usar la BD sin dummys - "train_h_factores"
  glimpse(train_h_factores)
  
  train_h_factores <- train_h_factores %>% select(-Nper, -Lp,-Ingtotugarr, -Ingpcug, -num_oc_hogar)
  
  
  #Dividimos train/test/eval (70%/20%/10%)
  
  set.seed(10110)
  index_3 <- createDataPartition(y=train_h_factores$Pobre, p = 0.7, list = FALSE)
  train_arboles  <- train_h_factores[index_3,]
  other_arboles  <- train_h_factores[-index_3,]
  
  set.seed(10110)
  index4  <- createDataPartition(other_arboles$Pobre, p = 1/3, list = FALSE)
  test_arboles <- other_arboles[-index4,]
  eval_arboles <- other_arboles[ index4,]
  
  dim(train_arboles)   
  dim(test_arboles)
  dim(eval_arboles)
  
  dim(train_h_factores)[1] - dim(train_arboles)[1] - dim(test_arboles)[1] - dim(eval_arboles)[1] #Cero para verificar que las particiones hayan quedado bien
  
  prop.table(table(train_h_factores$Pobre))    #Verificamos que las particiones conserven las mismas proporciones
  prop.table(table(train_arboles$Pobre))       #Verificamos que las muestras se encuentran desbalanceadas
  prop.table(table(test_arboles$Pobre))
  prop.table(table(eval_arboles$Pobre))  
  
  #Ejecutamos el modelo de árbol de decisión
  
  set.seed(10101)
  modelo6 <- train(Pobre ~ .,
                   data = train_arboles, 
                   method = "rpart", 
                   trControl = control)
  
  library(rattle)
  fancyRpartPlot(modelo6$finalModel)
  
  arbol6 <- rpart(Pobre ~ ., 
                  data = train_arboles,
                  method = "class")
  
  arbol6
  
  prp(arbol6, under = TRUE, branch.lty = 3, yesno = 2, faclen = 0, varlen=0, box.palette = "-RdYlGn")
  
  y_hat_train6 = predict(modelo6, newdata = train_arboles)
  y_hat_test6 = predict(modelo6, newdata = test_arboles)
  y_hat_eval6 = predict(modelo6, newdata = eval_arboles)
  
  acc_train6 <- Accuracy(y_pred = y_hat_train6, y_true = train_arboles$Pobre)
  acc_test6 <- Accuracy(y_pred = y_hat_test6, y_true = test_arboles$Pobre)
  acc_eval6 <- Accuracy(y_pred = y_hat_eval6, y_true = eval_arboles$Pobre)
  
  rec_train6 <- Recall(y_pred = y_hat_train6, y_true = train_arboles$Pobre, positive = "Pobre")
  rec_test6 <- Recall(y_pred = y_hat_test6, y_true = test_arboles$Pobre, positive = "Pobre")
  rec_eval6 <- Recall(y_pred = y_hat_eval6, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  f1_train6 <- F1_Score(y_pred = y_hat_train6, y_true = train_arboles$Pobre, positive = "Pobre")
  f1_test6 <- F1_Score(y_pred = y_hat_test6, y_true = test_arboles$Pobre, positive = "Pobre")
  f1_eval6 <- F1_Score(y_pred = y_hat_eval6, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  metricas_train6 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Sensitivity" = rec_train6,
                                "Accuracy" = acc_train6,
                                "F1" = f1_train6)
  
  metricas_test6 <- data.frame(Modelo = "Árbol de decisión", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Sensitivity" = rec_test6,
                               "Accuracy" = acc_test6,
                               "F1" = f1_test6)
  
  metricas_eval6 <- data.frame(Modelo = "Árbol de decisión", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Sensitivity" = rec_eval6,
                               "Accuracy" = acc_eval6,
                               "F1" = f1_eval6)
  
  metricas6 <- bind_rows(metricas_train6, metricas_test6, metricas_eval6)
  metricas <- bind_rows(metricas, metricas6)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###6.1 Árbol de decisión - Upsampling ----
  
  train_arbol61 <- upSample(x = select(train_arboles, -Pobre), 
                            y = train_arboles$Pobre, yname = "Pobre")
  
  prop.table(table(train_arboles$Pobre)) #BD inicial
  nrow(train_arboles) 
  
  prop.table(table(train_arbol61$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_arbol61) 
  
  glimpse(train_arbol61)
  
  set.seed(10101)
  modelo61 <- train(Pobre ~ .,
                    data = train_arbol61, 
                    method = "rpart", 
                    trControl = control)
  
  fancyRpartPlot(modelo61$finalModel)
  
  arbol61 <- rpart(Pobre ~ ., 
                   data = train_arbol61,
                   method = "class")
  
  arbol61
  
  prp(arbol61, under = TRUE, branch.lty = 3, yesno = 2, faclen = 0, varlen=0, box.palette = "-RdYlGn")
  
  y_hat_train61 = predict(modelo61, newdata = train_arboles)
  y_hat_test61 = predict(modelo61, newdata = test_arboles)
  y_hat_eval61 = predict(modelo61, newdata = eval_arboles)
  
  acc_train61 <- Accuracy(y_pred = y_hat_train61, y_true = train_arboles$Pobre)
  acc_test61 <- Accuracy(y_pred = y_hat_test61, y_true = test_arboles$Pobre)
  acc_eval61 <- Accuracy(y_pred = y_hat_eval61, y_true = eval_arboles$Pobre)
  
  rec_train61 <- Recall(y_pred = y_hat_train61, y_true = train_arboles$Pobre, positive = "Pobre")
  rec_test61 <- Recall(y_pred = y_hat_test61, y_true = test_arboles$Pobre, positive = "Pobre")
  rec_eval61 <- Recall(y_pred = y_hat_eval61, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  f1_train61 <- F1_Score(y_pred = y_hat_train61, y_true = train_arboles$Pobre, positive = "Pobre")
  f1_test61 <- F1_Score(y_pred = y_hat_test61, y_true = test_arboles$Pobre, positive = "Pobre")
  f1_eval61 <- F1_Score(y_pred = y_hat_eval61, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  metricas_train61 <- data.frame(Modelo = "Árbol de decisión", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train61,
                                 "Accuracy" = acc_train61,
                                 "F1" = f1_train61)
  
  metricas_test61 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test61,
                                "Accuracy" = acc_test61,
                                "F1" = f1_test61)
  
  metricas_eval61 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval61,
                                "Accuracy" = acc_eval61,
                                "F1" = f1_eval61)
  
  metricas61 <- bind_rows(metricas_train61, metricas_test61, metricas_eval61)
  metricas <- bind_rows(metricas, metricas61)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  ###6.2 Árbol de decisión - Downsampling ----
  
  train_arbol62 <- downSample(x = select(train_arboles, -Pobre), 
                              y = train_arboles$Pobre, yname = "Pobre")
  
  prop.table(table(train_arboles$Pobre)) #BD inicial
  nrow(train_arboles) 
  
  prop.table(table(train_arbol62$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_arbol62) 
  
  glimpse(train_arbol62)
  
  
  set.seed(10101)
  modelo62 <- train(Pobre ~ .,
                    data = train_arbol62, 
                    method = "rpart", 
                    trControl = control)
  
  fancyRpartPlot(modelo62$finalModel)
  
  arbol62 <- rpart(Pobre ~ ., 
                   data = train_arbol62,
                   method = "class")
  
  arbol62
  
  prp(arbol62, under = TRUE, branch.lty = 3, yesno = 2, faclen = 0, varlen=0, box.palette = "-RdYlGn")
  
  y_hat_train62 = predict(modelo62, newdata = train_arboles)
  y_hat_test62 = predict(modelo62, newdata = test_arboles)
  y_hat_eval62 = predict(modelo62, newdata = eval_arboles)
  
  acc_train62 <- Accuracy(y_pred = y_hat_train62, y_true = train_arboles$Pobre)
  acc_test62 <- Accuracy(y_pred = y_hat_test62, y_true = test_arboles$Pobre)
  acc_eval62 <- Accuracy(y_pred = y_hat_eval62, y_true = eval_arboles$Pobre)
  
  rec_train62 <- Recall(y_pred = y_hat_train62, y_true = train_arboles$Pobre, positive = "Pobre")
  rec_test62 <- Recall(y_pred = y_hat_test62, y_true = test_arboles$Pobre, positive = "Pobre")
  rec_eval62 <- Recall(y_pred = y_hat_eval62, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  f1_train62 <- F1_Score(y_pred = y_hat_train62, y_true = train_arboles$Pobre, positive = "Pobre")
  f1_test62 <- F1_Score(y_pred = y_hat_test62, y_true = test_arboles$Pobre, positive = "Pobre")
  f1_eval62 <- F1_Score(y_pred = y_hat_eval62, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  metricas_train62 <- data.frame(Modelo = "Árbol de decisión", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train62,
                                 "Accuracy" = acc_train62,
                                 "F1" = f1_train62)
  
  metricas_test62 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test62,
                                "Accuracy" = acc_test62,
                                "F1" = f1_test62)
  
  metricas_eval62 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval62,
                                "Accuracy" = acc_eval62,
                                "F1" = f1_eval62)
  
  metricas62 <- bind_rows(metricas_train62, metricas_test62, metricas_eval62)
  metricas <- bind_rows(metricas, metricas62)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  
  ###6.3 Árbol de decisión - Oversamplig (ROSE) ----
  
  table(train_arboles$Pobre)
  rose_train63 <- ROSE(Pobre ~ ., data = train_arboles, N = nrow(train_arboles) + 69539, p = 0.5)$data
  
  prop.table(table(rose_train63$Pobre))  #BD remuestreo - Verificamos proporciones de cada clase
  nrow(rose_train63)
  table(rose_train63$Pobre)
  
  prop.table(table(train_arboles$Pobre)) #BD inicial
  nrow(train_arboles)
  
  glimpse(rose_train63)
  
  
  set.seed(10101)
  modelo63 <- train(Pobre ~ .,
                    data = rose_train63, 
                    method = "rpart", 
                    trControl = control)
  
  fancyRpartPlot(modelo63$finalModel)
  
  arbol63 <- rpart(Pobre ~ ., 
                   data = rose_train63,
                   method = "class")
  
  arbol63
  
  prp(arbol63, under = TRUE, branch.lty = 3, yesno = 2, faclen = 0, varlen=0, box.palette = "-RdYlGn")
  
  y_hat_train63 = predict(modelo63, newdata = train_arboles)
  y_hat_test63 = predict(modelo63, newdata = test_arboles)
  y_hat_eval63 = predict(modelo63, newdata = eval_arboles)
  
  acc_train63 <- Accuracy(y_pred = y_hat_train63, y_true = train_arboles$Pobre)
  acc_test63 <- Accuracy(y_pred = y_hat_test63, y_true = test_arboles$Pobre)
  acc_eval63 <- Accuracy(y_pred = y_hat_eval63, y_true = eval_arboles$Pobre)
  
  rec_train63 <- Recall(y_pred = y_hat_train63, y_true = train_arboles$Pobre, positive = "Pobre")
  rec_test63 <- Recall(y_pred = y_hat_test63, y_true = test_arboles$Pobre, positive = "Pobre")
  rec_eval63 <- Recall(y_pred = y_hat_eval63, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  f1_train63 <- F1_Score(y_pred = y_hat_train63, y_true = train_arboles$Pobre, positive = "Pobre")
  f1_test63 <- F1_Score(y_pred = y_hat_test63, y_true = test_arboles$Pobre, positive = "Pobre")
  f1_eval63 <- F1_Score(y_pred = y_hat_eval63, y_true = eval_arboles$Pobre, positive = "Pobre")
  
  metricas_train63 <- data.frame(Modelo = "Árbol de decisión", 
                                 "Muestreo" = "Oversamplig (ROSE)", 
                                 "Evaluación" = "Entrenamiento",
                                 "Sensitivity" = rec_train63,
                                 "Accuracy" = acc_train63,
                                 "F1" = f1_train63)
  
  metricas_test63 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Test",
                                "Sensitivity" = rec_test63,
                                "Accuracy" = acc_test63,
                                "F1" = f1_test63)
  
  metricas_eval63 <- data.frame(Modelo = "Árbol de decisión", 
                                "Muestreo" = "Oversamplig (ROSE)", 
                                "Evaluación" = "Evaluación",
                                "Sensitivity" = rec_eval63,
                                "Accuracy" = acc_eval63,
                                "F1" = f1_eval63)
  
  metricas63 <- bind_rows(metricas_train63, metricas_test63, metricas_eval63)
  metricas <- bind_rows(metricas, metricas63)
  metricas %>% kbl(digits = 4) %>% kable_styling(full_width = T)
  
  