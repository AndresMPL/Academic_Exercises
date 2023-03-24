
#------------------------------------------------------------------------------#
#
#                             4 - NEURAL NETWORK
#
#------------------------------------------------------------------------------#

library(keras)

#-----------------------------------------------------
  
  #Instalación de TF
      #install.packages("tensorflow")
      library(reticulate)
      #path_to_python <- install_python()
      #virtualenv_create("r-reticulate", python = path_to_python)
      library(tensorflow)
      #install_tensorflow(envname = "r-reticulate")
      #install.packages("keras")
      library(keras)
      #install_keras(envname = "r-reticulate")
      library(tensorflow)
      #tf$constant("Hello Tensorflow!")

#------------------------------------------------------

  sum(is.na(train_clean_2$name)) #Validamos que todos los registros están diligenciados
  
  train_clean_2$name2 <- train_clean_2$name  #Guardamos el nombre en una variable nueva para generar factores
  
  train_clean_2$name2 <- ifelse(train_clean_2$name2 == "Lopez", 1, 
                         ifelse(train_clean_2$name2 == "Petro", 2, 3))
  
  train_clean_2$name2 <- as.factor(train_clean_2$name2)
  table(train_clean_2$name2)
  
  Y <- train_clean_2$name2
  Y <- to_categorical(Y)
       class(Y)
       head(Y)
       dim(Y)
       colSums(Y) #Verificamos que coincida con los valores de la tabla de nombres
  
  X <- as.matrix(tf_idf_reducido)
  class(X)
  dim(X)
  
  set.seed(10101)
  
  n <- nrow(train_clean_2)
  data_rows <- floor(0.70 * n)
  train_indices <- sample(1:n, data_rows)
  X_train <- X[train_indices, ]
  X_test <- X[-train_indices, ]
  y_train <- Y[train_indices, ]
  y_test <- Y[-train_indices, ]
  
  n_h = nrow(X_train)/(2*(ncol(X_train) + 5))


##Modelo 1---------------------------------------------------------------------

##1500 palabras

  rm(model_1)
  model_1 <- keras_model_sequential() 
  model_1 %>% 
          layer_dense(units = 2, activation = 'relu', input_shape = ncol(X_train)) %>% 
          layer_dropout(rate = 0.5) %>%
          layer_dense(units = 4, activation = 'softmax')
  
  summary(model_1)
  
  model_1 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('accuracy'))
  
  history_1 <- model_1 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_1 <- plot(history_1)
  history_plot_1
  
  model_1 %>% evaluate(X_test, y_test)
  
  y_hat_1 <- model_1 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_1), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))


##Modelo 2---------------------------------------------------------------------

  #layer_dense units = 2
  #epochs = 200
  #batch_size = 2^8
  #2296
  
  rm(model_2)
  model_2 <- keras_model_sequential() 
  model_2 %>% 
    layer_dense(units = 2, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_2)
  
  model_2 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('accuracy'))
  
  history2 <- model_2 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_2 <- plot(history_2)
  history_plot_2
  
  model_2 %>% evaluate(X_test, y_test)
  
  y_hat_2 <- model_2 %>% predict(X_test) %>% k_argmax()
  
  
  #library(caret)
  
  confusionMatrix(data = factor(as.numeric(y_hat_2), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))


##Modelo 3---------------------------------------------------------------------

  rm(model_3)
  model_3 <- keras_model_sequential() 
  model_3 %>% 
    layer_dense(units = 10, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_3)
  
  model_3 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_3 <- model_3 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_3 <- plot(history_3)
  history_plot_3
  
  model_3 %>% evaluate(X_test, y_test)
  
  y_hat_3 <- model_3 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_3), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))


##Modelo 4---------------------------------------------------------------------

  rm(model_4)
  model_4 <- keras_model_sequential() 
  model_4 %>% 
    layer_dense(units = 1, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_4)
  
  model_4 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_4 <- model_4 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.2)
  
  history_plot_4 <- plot(history_4)
  history_plot_4
  
  model_4 %>% evaluate(X_test, y_test)
  
  y_hat_4 <- model_4 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_4), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 5---------------------------------------------------------------------
  
  rm(model_5)
  model_5 <- keras_model_sequential() 
  model_5 %>% 
    layer_dense(units = 15, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 6, activation = 'softmax')  %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_5)
  
  model_5 %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_5 <- model_5 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.3)
  
  history_plot_5 <- plot(history_5)
  history_plot_5
  
  model_5 %>% evaluate(X_test, y_test)
  
  y_hat_5 <- model_5 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_5), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
##Modelo 6---------------------------------------------------------------------
  
  rm(model_6)
  model_6 <- keras_model_sequential() 
  model_6 %>% 
    layer_dense(units = 64, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dense(units = 30, activation = 'relu') %>%
    layer_dense(units = 6, activation = 'softmax')  %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_6)
  
  model_6 %>% compile(optimizer = optimizer_adam(learning_rate = 0.01), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_6 <- model_6 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.3)
  
  history_plot_6 <- plot(history_6)
  history_plot_6
  
  model_6 %>% evaluate(X_test, y_test)
  
  y_hat_6 <- model_6 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_6), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 7---------------------------------------------------------------------
  
  rm(model_7)
  model_7 <- keras_model_sequential() 
  model_7 %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 32, activation = 'softmax')  %>%
    layer_dense(units = 16, activation = 'relu')  %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_7)
  
  model_7 %>% compile(optimizer = optimizer_adam(learning_rate = 0.01), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_7 <- model_7 %>% fit(X_train, y_train, epochs = 100, batch_size = 2^8, validation_split = 0.3)
  
  history_plot_7 <- plot(history_7)
  history_plot_7
  
  model_7 %>% evaluate(X_test, y_test)
  
  y_hat_7 <- model_7 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_7), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 8---------------------------------------------------------------------
  
  rm(model_8)
  model_8 <- keras_model_sequential() 
  model_8 %>% 
    layer_dense(units = 100, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 32, activation = 'softmax')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 16, activation = 'relu')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_8)
  
  model_8 %>% compile(optimizer = optimizer_adam(learning_rate = 0.02), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_8 <- model_8 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.3)
  
  history_plot_8 <- plot(history_8)
  history_plot_8
  
  model_8 %>% evaluate(X_test, y_test)
  
  y_hat_8 <- model_8 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_8), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 9---------------------------------------------------------------------
  
  #Modelo con todas las columnas - eliminando palabras con n < 10 en la limpieza
  
  rm(model_9)
  model_9 <- keras_model_sequential() 
  model_9 %>% 
    layer_dense(units = 512, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 128, activation = 'softmax')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_9)
  
  model_9 %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_9 <- model_9 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.3, callbacks = list(
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5), callback_early_stopping(patience = 10)))
  
  history_plot_9 <- plot(history_9) + theme_bw()
  history_plot_9
  
  model_9 %>% evaluate(X_test, y_test)
  
  y_hat_9 <- model_9 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_9), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  tabla_modelo <- xtable(summary(model_9))
  stargazer::stargazer(model_9)
  
  
  print(model_9)
  
##Modelo 10---------------------------------------------------------------------
  
  rm(model_10)
  model_10 <- keras_model_sequential() 
  model_10 %>% 
    layer_dense(units = 1024, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 512, activation = 'relu') %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 128, activation = 'softmax')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_10)
  
  model_10 %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_10 <- model_10 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.3, callbacks = list(
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5), callback_early_stopping(patience = 10)))
  
  history_plot_10 <- plot(history_10) + theme_bw()
  history_plot_10
  
  model_10 %>% evaluate(X_test, y_test)
  
  y_hat_10 <- model_10 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_10), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 11---------------------------------------------------------------------
  
  rm(model_11)
  model_11 <- keras_model_sequential() 
  model_11 %>% 
    layer_dense(units = 1024, activation = 'relu', input_shape = ncol(X_train)) %>% 
    #layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = 'relu') %>%
    #layer_dropout(rate = 0.5) %>%
    layer_dense(units = 128, activation = 'softmax')  %>%
    #layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_11)
  
  model_11 %>% compile(optimizer = optimizer_adam(learning_rate = 0.0001), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_11 <- model_11 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.3)
  
  history_plot_11 <- plot(history_11) + theme_bw()
  history_plot_11
  
  model_11 %>% evaluate(X_test, y_test)
  
  y_hat_11 <- model_11 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_11), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 12---------------------------------------------------------------------
#Modelo 5245 palabras
  
  rm(model_12)
  model_12 <- keras_model_sequential() 
  model_12 %>% 
    layer_dense(units = 512, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 128, activation = 'softmax')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_12)
  
  model_12 %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_12 <- model_12 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.3, callbacks = list(
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5), callback_early_stopping(patience = 10)))
  
  history_plot_12 <- plot(history_12) + theme_bw()
  history_plot_12
  
  model_12 %>% evaluate(X_test, y_test)
  
  y_hat_12 <- model_12 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_12), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))
  
  
##Modelo 13---------------------------------------------------------------------
#Modelo 5245 palabras
  
  rm(model_13)
  model_13 <- keras_model_sequential() 
  model_13 %>% 
    layer_dense(units = 1024, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 512, activation = 'softmax') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = 'softmax') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 128, activation = 'relu')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_13)
  
  model_13 %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_13 <- model_13 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^10, validation_split = 0.3, callbacks = list(
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5), callback_early_stopping(patience = 10)))
  
  history_plot_13 <- plot(history_13) + theme_bw()
  history_plot_13
  
  model_13 %>% evaluate(X_test, y_test)
  
  y_hat_13 <- model_13 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_13), levels = 1:3), 
                  reference = factor(train_clean_2$name2[-train_indices], levels = 1:3))

