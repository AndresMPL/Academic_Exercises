
#------------------------------------------------------------------------------#
#
#                                4 - SUPER LEARNER
#
#------------------------------------------------------------------------------#

#Particiones Train-Test----

  p_load(SuperLearner, caret)
  set.seed(1011)
  
  inTrain <- createDataPartition(y = train$price, p = .7, list = FALSE)
  
  train_7 <- train[ inTrain,]
  test_3  <- train[-inTrain,]
  

#Regresiones----

  glimpse(train)
  
  ##Regresion 1----
  reg1 <- lm(price~distancia_parque+distancia_museo+distancia_ips+distancia_ese +distancia_colegios+distancia_cai+ 
               distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
               total_eventos_2022, data = train_7)
  
  stargazer(reg1, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg1)
  
  test_3$y_hat1 <- predict(reg1, newdata = test_3)
  MAE_model1 <- with(test_3, mean(abs(price - y_hat1))) #Calculating the MAE
  MAE_model1
  
  
  ##Regresion 2----
  reg2 <- lm(price~surface_total_imp+surface_covered_imp+rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
             distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
             distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
             total_eventos_2022, data = train_7)
  
  stargazer(reg2, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg2)
  
  test_3$y_hat2 <- predict(reg2, newdata = test_3)
  MAE_model2 <- with(test_3, mean(abs(price - y_hat2))) #Calculating the MSE
  MAE_model2

  reg21 <- lm(price~surface_total_imp+surface_covered_imp+rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
               distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
               distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
               total_eventos_2022+I(total_eventos_2022^2)+I(distancia_cai^2)+I(distancia_colegios^2), data = train_7)
  
  stargazer(reg21, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg21)
  
  test_3$y_hat21 <- predict(reg21, newdata = test_3)
  MAE_model21 <- with(test_3, mean(abs(price - y_hat21))) #Calculating the MSE
  MAE_model21
  
  
  ##Regresion 3----
  reg22 <- lm(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                total_eventos_2022+I(total_eventos_2022^2)+I(distancia_cai^2)+I(distancia_colegios^2), data = train_7)
  
  stargazer(reg22, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg22)
  
  test_3$y_hat22 <- predict(reg22, newdata = test_3)
  MAE_model22 <- with(test_3, mean(abs(price - y_hat22))) #Calculating the MSE
  MAE_model22
  
  
#Elastic Net ----

  set.seed(10101)
  fitControl <- trainControl(method = "cv", number = 10)

  ##EN1----
  EN <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+distancia_parque+distancia_museo+
                 distancia_ips+distancia_ese+distancia_colegios+
                 distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                 total_eventos_2022+I(total_eventos_2022^2)+
                 I(distancia_colegios^2), data = train_7, 
                 method = 'glmnet', 
                 trControl = fitControl,
                 tuneGrid = expand.grid(alpha = 0.5,lambda = seq(0.001,0.02,by = 0.001)),
                 preProcess = c("center", "scale")) 
  
  coef_EN <- coef(EN$finalModel, EN$bestTune$lambda)
  coef_EN
  
  test_3$y_hat3 <- predict(EN, newdata = test_3)
  MAE_model3 <- with(test_3, mean(abs(price - y_hat3))) #Calculating the MSE
  MAE_model3

  
  ##EN2----
  
  set.seed(10101)
  EN2 <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                  distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022+I(total_eventos_2022^2), data = train_7, 
               method = 'glmnet', 
               trControl = fitControl,
               tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
               preProcess = c("center", "scale")) 
  
  coef_EN2 <- coef(EN2$finalModel, EN2$bestTune$lambda)
  coef_EN2
  
  test_3$y_hat4 <- predict(EN2, newdata = test_3)
  MAE_model4 <- with(test_3, mean(abs(price - y_hat4))) #Calculating the MSE
  MAE_model4
  
  ##EN3----
  set.seed(10101)
  EN3 <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                  distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022+I(total_eventos_2022^2)+ I(distancia_cai^2)+(distancia_colegios^2), data = train_7, 
                method = 'glmnet', 
                trControl = fitControl,
                tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
                preProcess = c("center", "scale")) 
  
  coef_EN3 <- coef(EN3$finalModel, EN3$bestTune$lambda)
  coef_EN3
  
  test_3$y_hat5 <- predict(EN3, newdata = test_3)
  MAE_model5 <- with(test_3, mean(abs(price - y_hat5))) #Calculating the MSE
  MAE_model5
  
  
  ##EN4----
  set.seed(10101)
  EN4 <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                  distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022+I(total_eventos_2022^2)+ I(distancia_cai^2)+I(distancia_colegios^2), data = train_7, 
                method = 'glmnet', 
                trControl = fitControl,
                tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
                preProcess = c("center", "scale")) 
  
  coef_EN4 <- coef(EN4$finalModel, EN4$bestTune$lambda)
  coef_EN4
  
  test_3$y_hat6 <- predict(EN4, newdata = test_3)
  MAE_model6 <- with(test_3, mean(abs(price - y_hat6))) #Calculating the MSE
  MAE_model6


  set.seed(10101)
  fitControl <- trainControl(method = "cv", number = 10)
  
  ##EN5----
  set.seed(10101)
  EN5 <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                  distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                  I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                  I(distancia_ips*distancia_ese) + I(distancia_parque^2),
                data = train_7, 
                method = 'glmnet', 
                trControl = fitControl,
                tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
                preProcess = c("center", "scale")) 
  
  coef_EN5 <- coef(EN5$finalModel , EN5$bestTune$lambda)
  coef_EN5
  
  test_3$y_hat7 <- predict(EN5, newdata = test_3)
  MAE_model7 <- with(test_3, mean(abs(price - y_hat7))) #Calculating the MSE
  MAE_model7
  
  ##EN6----
  set.seed(10101)
  EN6 <-  train(price~property_type+
                  distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                  I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                  I(distancia_ips*distancia_ese) + I(distancia_parque^2) + rooms_imp +bedrooms_imp +bathrooms_imp,
                data = train_7, 
                method = 'glmnet', 
                trControl = fitControl,
                tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
                preProcess = c("center", "scale")) 
  
  coef_EN6 <- coef(EN6$finalModel , EN6$bestTune$lambda)
  coef_EN6
  
  test_3$y_hat8 <- predict(EN6, newdata = test_3)
  MAE_model8 <- with(test_3, mean(abs(price - y_hat8))) #Calculating the MSE
  MAE_model8
  
#Random forest----------------------------------------------------------
  
  tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                             min.node.size = c(10, 30, 50, 70, 100),
                             splitrule = "variance")
  
  control_rf <- trainControl(method = "cv", number = 10)
  
  modelo_rf <- train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                       distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                       distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                       total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                       I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                       I(distancia_ips*distancia_ese) + I(distancia_parque^2),
                       data = train_7, 
                       method = "ranger", 
                       trControl = control_rf,
                       metric = 'RMSE', 
                       tuneGrid = tunegrid_rf)
  
  Grilla_1 <- ggplot(modelo_rf$results, 
                     aes(x = min.node.size, y = RMSE, 
                     color = as.factor(mtry))) +
                     geom_line() +
                     geom_point() +
                     labs(title = "Resultados del grid search",
                     x = "Mínima cantidad de observaciones por hoja",
                     y = "RMSE (Cross-Validation)") +
                     scale_color_discrete("Número de predictores seleccionados al azar") +
                     theme_bw() +
                     theme(legend.position = "bottom")
      
  Grilla_1
  
  test_3$y_hat9 <- predict(modelo_rf, newdata = test_3)
  MAE_model9 <- with(test_3, mean(abs(price - y_hat9))) #Calculating the MSE
  MAE_model9
  
  
#Random forest2----------------------------------------------------------
  
  tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                             min.node.size = c(10, 30, 50, 70, 100),
                             splitrule = "variance")
  
  control_rf <- trainControl(method = "cv", number = 10)
  
  modelo_rf2 <- train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                       distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                       distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                       total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                       I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                       I(distancia_ips*distancia_ese) + I(distancia_parque^2),
                     data = train_7, 
                     method = "ranger", 
                     trControl = control_rf,
                     metric = 'RMSE', 
                     tuneGrid = tunegrid_rf)
  
  Grilla_12 <- ggplot(modelo_rf2$results, 
                     aes(x = min.node.size, y = RMSE, 
                         color = as.factor(mtry))) +
    geom_line() +
    geom_point() +
    labs(title = "Resultados del grid search",
         x = "Mínima cantidad de observaciones por hoja",
         y = "RMSE (Cross-Validation)") +
    scale_color_discrete("Número de predictores seleccionados al azar") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  Grilla_12
  
  test_3$y_hat10 <- predict(modelo_rf2, newdata = test_3)
  MAE_model10 <- with(test_3, mean(abs(price - y_hat10))) #Calculating the MSE
  MAE_model10
  
  
  
  # imputación de datos  --------
  
  id_test <- test %>% 
    select(property_id) #extraer identificador para volver a dividir 
  
  #variables a usar (se usa la base despues de aplicarle las modificaciones del script 5_prediction)
  
  test_imp <- test %>% select(property_id, price, surface_total, surface_covered, 
                              rooms, bedrooms, bathrooms, property_type, ESoEstrato, 
                              mts2, total_eventos_2022, CMNOMLOCAL, distancia_parque,
                              distancia_museo, distancia_ips, distancia_ese, distancia_colegios, 
                              distancia_cai, distancia_best, distancia_centrof, distancia_cuadrantes, 
                              distancia_buses, distancia_tm)
  
  train_imp <- train %>% select(property_id, price, surface_total, surface_covered, 
                              rooms, bedrooms, bathrooms, property_type, ESoEstrato, 
                              mts2, total_eventos_2022, CMNOMLOCAL, distancia_parque,
                              distancia_museo, distancia_ips, distancia_ese, distancia_colegios, 
                              distancia_cai, distancia_best, distancia_centrof, distancia_cuadrantes, 
                              distancia_buses, distancia_tm)
  
  total_base <- rbind(train_imp, test_imp) %>% as.data.frame()
  
  #Imputación 
  
  filtro <- is.na(total_base$surface_total) 
  sum(filtro)
  total_base$surface_total_imp[filtro] <- mean(total_base$surface_total, na.rm = T)
  
  filtro <- is.na(total_base$surface_covered) 
  sum(filtro)
  total_base$surface_covered_imp[filtro] <- mean(total_base$surface_covered, na.rm = T)
  
  filtro <- is.na(total_base$bedrooms) 
  sum(filtro)
  total_base$bedrooms_imp[filtro] <- mean(total_base$bedrooms, na.rm = T)
  
  filtro <- is.na(total_base$bathrooms) 
  sum(filtro)
  total_base$bathrooms_imp[filtro] <- mean(total_base$bathrooms, na.rm = T)
  
  filtro <- is.na(total_base$rooms)
  sum(filtro)
  total_base$rooms_imp[filtro] <- mean(total_base$rooms, na.rm = T)
  
  
  #estandarizacion  
  
  variables_numericas <- c("surface_total_imp2", "surface_covered_imp", "rooms_imp",
                           "bedrooms_imp", "bathrooms_imp",  
                           "total_eventos_2022", "distancia_parque",
                           "distancia_museo", "distancia_ips", "distancia_ese", "distancia_colegios", 
                           "distancia_cai", "distancia_best", "distancia_centrof", "distancia_cuadrantes", 
                           "distancia_buses", "distancia_tm")
  
  escalador <- preProcess(total_base[, variables_numericas],
                          method = c("center", "scale"), na.remove = TRUE)
  
  total_base[, variables_numericas] <- predict(escalador, total_base[, variables_numericas])
  
  #definicion de variables categoricas como factores
  
  total_base$property_type <- ifelse(total_base$property_type == "Casa", 1, 2) 
  total_base <- total_base %>% mutate(property_type=factor(property_type,levels=c(1,2),labels=c("Casa","Apartamento")))
  
  
  total_base <- total_base %>% 
    mutate(ESoEstrato=factor(ESoEstrato, levels=c(1,2,3,4,5,6), 
                             labels= c("Estrato_1","Estrato_2", "Estrato_3", "Estrato_4", "Estrato_5", "Estrato_6"))) 
  
  
  total_base$CMNOMLOCAL <- factor(total_base$CMNOMLOCAL)
  
  #dividir base en test y train nuevamente 
  
  test_imp2<- total_base %>% filter(property_id==id_test)
  
  train_imp2 <- total_base %>% filter(property_id!=id_test)
  
  
  #Particiones nuevas Train-Test----
  
  set.seed(1011)
  
  inTrain <- createDataPartition(y = train_imp2$price, p = .7, list = FALSE)
  
  train_nw <- train[ inTrain,]
  test_nw  <- train[-inTrain,]
  
  ##Regresion nueva 1----
  reg1_nw <- lm(price~distancia_parque+distancia_museo+distancia_ips+distancia_ese +distancia_colegios+distancia_cai+ 
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022 + CMNOMLOCAL, data = train_nw)
  
  stargazer(reg1_nw, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg1_nw)
  
  test_nw$y_hat1 <- predict(reg1_nw, newdata = test_nw)
  MAE_model1_nw <- with(test_nw, mean(abs(price - y_hat1))) #Calculating the MAE
  MAE_model1_nw
  
  
  ##Regresion nueva 2----
  reg2_nw <- lm(price~surface_total_imp+surface_covered_imp+rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                  distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                  distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                  total_eventos_2022 +  CMNOMLOCAL, data = train_nw)
  
  stargazer(reg2_nw, type = "text", dep.var.labels = "Precio de venta", digits = 4)
  summary(reg2_nw)
  
  test_nw$y_hat2 <- predict(reg2_nw, newdata = test_nw)
  MAE_model2_nw <- with(test_nw, mean(abs(price - y_hat2))) #Calculating the MSE
  MAE_model2_nw
  
  #Elastic Net nuevos ----
  
  set.seed(10101)
  fitControl <- trainControl(method = "cv", number = 10)
  
  ##EN1 nuevos----
  EN_nw <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+distancia_parque+distancia_museo+
                    distancia_ips+distancia_ese+distancia_colegios+
                    distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                    total_eventos_2022 + CMNOMLOCAL, data = train_nw, 
                  method = 'glmnet', 
                  trControl = fitControl,
                  tuneGrid = expand.grid(alpha = 0.5,lambda = seq(0.001,0.02,by = 0.001)),
                  preProcess = NULL) 
  
  coef_EN_nw <- coef(EN_nw$finalModel, EN_nw$bestTune$lambda)
  coef_EN_nw
  
  test_nw$y_hat3 <- predict(EN_nw, newdata = test_nw)
  MAE_model3_nw <- with(test_nw, mean(abs(price - y_hat3))) #Calculating the MSE
  MAE_model3_nw
  
  ##EN2----
  set.seed(10101)
  EN2_nw <-  train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                     distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                     distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                     total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                     I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                     I(distancia_ips*distancia_ese) + I(distancia_parque^2)+ CMNOMLOCAL,
                   data = train_nw, 
                   method = 'glmnet', 
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda = seq(0.001,0.02,by = 0.001)),
                   preProcess = NULL) 
  
  coef_EN2_nw <- coef(EN2_nw$finalModel , EN2_nw$bestTune$lambda)
  coef_EN2_nw
  
  test_nw$y_hat7 <- predict(EN2_nw, newdata = test_nw)
  MAE_model7_nw <- with(test_nw, mean(abs(price - y_hat7))) #Calculating the MSE
  MAE_model7_nw
  
  #Random forest2----------------------------------------------------------
  
  tunegrid_rf_nw <- expand.grid(mtry = c(3, 5, 10), 
                                min.node.size = c(10, 30, 50, 70, 100),
                                splitrule = "variance")
  
  control_rf_nw <- trainControl(method = "cv", number = 10)
  
  modelo_rf2_nw <- train(price~rooms_imp+bedrooms_imp+bathrooms_imp+property_type+
                           distancia_parque+distancia_museo+distancia_ips+distancia_ese+distancia_colegios+distancia_cai+
                           distancia_best+distancia_centrof+distancia_cuadrantes+distancia_buses+distancia_tm+
                           total_eventos_2022+I(total_eventos_2022^2)+I(total_eventos_2022^3) + I(distancia_cai^2)+I(distancia_colegios^2)+
                           I(distancia_parque*distancia_buses) + I(total_eventos_2022*distancia_cai) + I(distancia_tm*distancia_buses)+
                           I(distancia_ips*distancia_ese) + I(distancia_parque^2) + CMNOMLOCAL,
                         data = train_nw, 
                         method = "ranger", 
                         trControl = control_rf_nw,
                         metric = 'RMSE', 
                         tuneGrid = tunegrid_rf_nw)
  
  Grilla_12_nw <- ggplot(modelo_rf2_nw$results, 
                         aes(x = min.node.size, y = RMSE, 
                             color = as.factor(mtry))) +
    geom_line() +
    geom_point() +
    labs(title = "Resultados del grid search",
         x = "Mínima cantidad de observaciones por hoja",
         y = "RMSE (Cross-Validation)") +
    scale_color_discrete("Número de predictores seleccionados al azar") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  Grilla_12_nw
  
  test_nw$y_hat10 <- predict(modelo_rf2_nw, newdata = test_nw)
  MAE_model10_nw <- with(test_nw, mean(abs(price - y_hat10))) #Calculating the MSE
  MAE_model10_nw
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  imputar <- c("surface_total", "surface_covered", "rooms",
               "bedrooms", "bathrooms", "mts2", "total_eventos_2022", 
               "distancia_parque","distancia_museo", "distancia_ips", 
               "distancia_ese", "distancia_colegios", "distancia_cai", 
               "distancia_best", "distancia_centrof", "distancia_cuadrantes", 
               "distancia_buses", "distancia_tm", "property_type")
  
  p_load(VIM)
  
  datos_imp <- kNN(data = total_base[, imputar], k = 5)
  
  summary(datos_imp)
  
  