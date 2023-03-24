#---------------------------------------------------------------------------
#                           
#                               Taller 1
#
#   Grupo 5:  Isabella Mendez Pedraza.
#             Manuela Ojeda Ojeda.
#             Juan Sebastian Tellez Melo.
#             Andres Mauricio Palacio Lugo.
#
#---------------------------------------------------------------------------

#---------------------------------------------------------------------------
#                      5. Predicting earnings
#---------------------------------------------------------------------------

set.seed(10101) 

#seleccionar las variables que se usaran 

dt_final_P5 <- dt_final %>%
                select(age, cuentaPropia, informal, maxEducLevel3, maxEducLevel4, maxEducLevel5, maxEducLevel6, maxEducLevel7, 
                microEmpresa, experiencia, Ingresos_laborales, female, age_squred, oficio2:oficio99)

#Dividir la base 70% - 30%

dt_final_P5$id <- 1:nrow(dt_final_P5)

train  <- dt_final_P5 %>% dplyr::sample_frac(0.70) ## train sample
test   <- dplyr::anti_join(dt_final_P5, train, by = 'id') ## test sample

train <- select(train, -id)
test <- select(test, -id)

#calculamos los modelos y los MSE

##modelos ya usados  

model1<-lm(Ingresos_laborales ~ female ,data=train)
        test$model1<-predict(model1,newdata = test)
        MSE_model1 <- with(test,mean((Ingresos_laborales-model1)^2))#Calculating the MSE
        MSE_model1

model2<-lm(Ingresos_laborales ~ age + age_squred ,data=train)
        test$model2<-predict(model2,newdata = test)
        MSE_model2 <-with(test,mean((Ingresos_laborales-model2)^2))#Calculating the MSE

model3<-lm(Ingresos_laborales ~ female + age + age_squred ,data=train)
        test$model3<-predict(model3,newdata = test)
        MSE_model3 <-with(test,mean((Ingresos_laborales-model3)^2))#Calculating the MSE

model4<-lm(Ingresos_laborales ~ female + age + cuentaPropia + informal + 
        maxEducLevel3 + maxEducLevel4 + maxEducLevel5 + maxEducLevel6 + maxEducLevel7 + 
        microEmpresa + experiencia +  age_squred ,data=train)
        test$model4<-predict(model4,newdata = test)
        MSE_model4 <-with(test,mean((Ingresos_laborales-model4)^2))#Calculating the MSE

##nuevos modelos 

model5<-lm(Ingresos_laborales ~ . ,data=train)
        test$model5<-predict(model5,newdata = test)
        MSE_model5 <-with(test,mean((Ingresos_laborales-model5)^2)) #Calculating the MSE

model6<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal ,data=train)
        test$model6<-predict(model6,newdata = test)
        MSE_model6 <-with(test,mean((Ingresos_laborales-model6)^2))#Calculating the MSE

model7<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal + poly(experiencia,4,raw=TRUE),data=train)
        test$model7<-predict(model7,newdata = test)
        MSE_model7 <-with(test,mean((Ingresos_laborales-model7)^2))#Calculating the MSE

model8<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE),data=train)
        test$model8<-predict(model8,newdata = test)
        MSE_model8 <-with(test,mean((Ingresos_laborales-model8)^2))#Calculating the MSE

model9<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal*poly(experiencia,4,raw=TRUE)*microEmpresa ,data=train)
        test$model9<-predict(model9,newdata = test)
        MSE_model9 <-with(test,mean((Ingresos_laborales-model9)^2))#Calculating the MSE


#Tabla de comparación de los MSE

test_compar <- cbind(MSE = c(MSE_model1, MSE_model2, MSE_model3, MSE_model4, MSE_model5, MSE_model6, MSE_model7, MSE_model8, MSE_model9)) %>%
  cbind(Modelo = c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09"))

test_compar <- test_compar %>% as.data.frame() %>%arrange(Modelo)
View(test_compar)

test_compar <- test_compar %>% mutate(MSE = as.numeric(MSE))
test_compar <- test_compar %>% mutate(MSE = round(test_compar$MSE,4))

modelos <-  ggplot(data=test_compar, mapping = aes(x=Modelo , y = MSE)) +
  geom_point() + 
  labs(title = 'Figura 10. Comparación de MSE y Complejidad de los Modelos', x = 'Modelo', y = 'MSE') + 
  theme_bw()

modelos #aquí observamos la distribuación de los modelos de acuerdo con su complejidad y MSE de predicción estimado



##Errores de predicción

test <- test %>%
  mutate(pre_errors_model5 = (Ingresos_laborales-model5))  

test <- test %>%
  mutate(pre_errors_model6 = (Ingresos_laborales-model6))


dist_pre_errors_m5 <- test %>%
  ggplot(mapping = aes(x=Ingresos_laborales , y = pre_errors_model5)) +
  geom_point() + 
  stat_smooth(method = lm,formula= y ~ poly(x, 2), se = TRUE, level=0.95) + 
  labs(title = 'Figura 11. Distribución de los errores modelo 5', x = 'Ingresos laborales', y = 'Errores') + 
  theme_bw()

dist_pre_errors_m5

dist_pre_errors_m6 <- test %>%
  ggplot(mapping = aes(x=Ingresos_laborales , y = pre_errors_model6)) +
  geom_point() + 
  stat_smooth(method = lm,formula= y ~ poly(x, 2), se = TRUE, level=0.95) + 
  labs(title = 'Figura 12. Distribución de los errores modelo 6', x = 'Ingresos laborales', y = 'Errores') + 
  theme_bw()

dist_pre_errors_m6

# LOOCV: Los modelos con menor error de predicción son los modelos 5 y 6 
  
#Leave-One-Out Cross-Validation (LOOCV)-----------------------------------------

  #Calculamos los MSE - utilizando "High Leverage Points"--------------------------------------------------
  #Modelo 5
  
  model5_L<-lm(Ingresos_laborales ~ . ,data=dt_final_P5) #calculamos el modelo con todas las observaciones 
  
  dt_final_P5$hats_P_M5 <- hatvalues(model5_L)#encontramos los "High Leverage Points"
  summary(dt_final_P5$hats_P_M5) #encontramos una observacion que genera h = 1, entonces la eliminamos 
  
  dt_final_P5_M5 <- dt_final_P5 %>%
  filter(hats_P_M5 != 1)
  
  model5_L<-lm(Ingresos_laborales ~ . ,data=dt_final_P5_M5) #volvemos a calcular el modelo sin las observaciones eliminadas 
  
  dt_final_P5_M5$model5_L<-predict(model5_L, data=dt_final_P5_M5) #Calculamos y_hat
  
  dt_final_P5_M5 <- dt_final_P5_M5 %>%
    mutate(least_squares_M5 = (Ingresos_laborales - model5_L)^2) #Calculamos errores al cuadrado
  
  dt_final_P5_M5$hats_P_M5 <- hatvalues(model5_L) #encontramos los "High Leverage Points""
  
  dt_final_P5_M5$hats_M5 <- (1- dt_final_P5_M5$hats_P_M5)^2 
  
  dt_final_P5_M5$lq_hat_M5 <- (dt_final_P5_M5$least_squares_M5/dt_final_P5_M5$hats_M5) 
  
  CV_M5 <- (1/nrow(dt_final_P5_M5) * colSums(as.matrix(dt_final_P5_M5$lq_hat_M5), dims = 1)) # calculamos CV: aproximacion del MSE de LOOCV ajustando los errores por la importancia que tiene cada observacion dentro del ajsute del modelo original  
  
  #Modelo 6
  
  model6_L<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal ,data=dt_final_P5) #calculamos el modelo con todas las observaciones 
  
  dt_final_P5$hats_P_M6 <- hatvalues(model6_L)#encontramos los "High Leverage Points""
  summary(dt_final_P5$hats_P_M6) #encontramos una observacion que genera h = 1, entonces la eliminamos 
  
  dt_final_P5_M6 <- dt_final_P5 %>%
    filter(hats_P_M6 != 1)
  
  model6_L<-lm(Ingresos_laborales ~ . + female*cuentaPropia*informal ,data=dt_final_P5_M6) #volvemos a calcular el modelo sin las observaciones eliminadas 
  
  dt_final_P5_M6$model6_L<-predict(model6_L, data=dt_final_P5_M6) #Calculamos y_hat
  
  dt_final_P5_M6 <- dt_final_P5_M6 %>%
    mutate(least_squares_M6 = (Ingresos_laborales - model6_L)^2) #Calculamos errores al cuadrado
  
  dt_final_P5_M6$hats_P_M6 <- hatvalues(model6_L) #encontramos los "High Leverage Points""
  
  dt_final_P5_M6$hats_M6 <- (1- dt_final_P5_M6$hats_P_M6)^2 
  
  dt_final_P5_M6$lq_hat_M6 <- (dt_final_P5_M6$least_squares_M6/dt_final_P5_M6$hats_M6) 
  
  CV_M6 <- (1/nrow(dt_final_P5_M6) * colSums(as.matrix(dt_final_P5_M6$lq_hat_M6), dims = 1)) # calculamos CV: aproximacion del MSE de LOOCV ajustando los errores por la importancia que tiene cada observacion dentro del ajsute del modelo original  
  

  #LOOCV------------------------------------------------------------------------
  
  #Generamos las partes para evaluar los modelos
  
  #Datos para hacer el modelo reproducible
  set.seed(01010)
  
  # Evaluaremos en K partes
  K <- nrow(dt_final_P5)
  
  #Dividimos el conjunto de datos en K partes
  index <- split(1:nrow(dt_final), 1: K)
  
  head(index[[1]])
  lapply(index,length)
  
  #Aplicamos la lista de partes al conjunto de datos
  splt <- lapply(1:K, function(ind) dt_final[index[[ind]], ])
  head(splt[[1]])
  
  #Evaluamos los modelos dejando fuera una de las partes
  
  #Modelo 5 - model5
  
  m1 <- lapply(1:K, function(ii) lm(Ingresos_laborales ~ . , data = rbindlist(splt[-ii]))) 
  p1 <- lapply(1:K, function(ii) data.frame(predict(m1[[ii]], newdata = rbindlist(splt[ii]))))
  p1[1] #Comprobamos el vector creado con predicciones
  
  for (i in 1:K) {
    colnames(p1[[i]])<-"yhat_1" #agregamos la predicción
    splt[[i]] <- cbind(splt[[i]], p1[[i]])
  }
  
  head(splt[[1]])
  
  #Calculamos el MSE
  
  mse2_k5 <- lapply(1:K, function(ii) mean((splt[[ii]]$Ingresos_laborales - splt[[ii]]$yhat)^2))
  mse2_k5 #MSE en cada parte evaluada
  mse2_model5 <- mean(unlist(mse2_k5))
  
  #Modelo 6 - model6
  
  m2 <- lapply(1:K, function(ii) lm(Ingresos_laborales ~ . + female*cuentaPropia*informal, data = rbindlist(splt[-ii]))) 
  p2 <- lapply(1:K, function(ii) data.frame(predict(m2[[ii]], newdata = rbindlist(splt[ii]))))
  p2[1] #Comprobamos el vector creado con predicciones
  
  for (i in 1:K) {
    colnames(p2[[i]])<-"yhat_2" #agregamos la predicción
    splt[[i]] <- cbind(splt[[i]], p2[[i]])
  }
  
  head(splt[[1]])
  
  #Calculamos el MSE
  
  mse2_k6 <- lapply(1:K, function(ii) mean((splt[[ii]]$Ingresos_laborales - splt[[ii]]$yhat_2)^2))
  mse2_k6 #MSE en cada parte evaluada
  mse2_model6 <- mean(unlist(mse2_k6))
 

#Tabla de comparación de los MSE
  
  test_compar_L <- cbind(MSE = c( MSE_model5, MSE_model6, CV_M5, CV_M6)) %>%
                  cbind(Modelo = c("M05", "M06", "M05-L","M06-L"))
  
  test_compar_L <- test_compar_L %>% as.data.frame() %>%arrange(Modelo)
 
  test_compar_L <- test_compar_L %>% mutate(MSE = as.numeric(MSE))
  test_compar_L <- test_compar_L %>% mutate(MSE = round(test_compar_L$MSE,4))
  
  modelos_L <-  ggplot(data=test_compar_L, mapping = aes(x=Modelo , y = MSE)) +
    geom_point() + 
    labs(title = 'Figura 13. Comparación MSE de LOOCV y conjunto de validación', x = 'Modelo', y = 'MSE') + 
    theme_bw()
  
  modelos_L #aquí observamos la distribuación de los modelos de acuerdo con su complejidad y MSE de predicción estimado
  
  