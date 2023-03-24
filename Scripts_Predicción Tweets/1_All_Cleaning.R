
library(pacman)

p_load(dplyr, tidyverse, tm, textir, tidytext, wordcloud, SentimentAnalysis, 
       udpipe, syuzhet,stringi,stopwords,textstem,topicmodels, rio, caret, sentimentr,
       janitor, wordcloud2, udpipe,ggcorrplot)
library(keras)

#Unir las bases----

  test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/test.csv")
  train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/train.csv")


#Extraer id y text----

  test_id  <- test  %>% select(id, text)
  train_id  <- train  %>% select(id, text)
  
  text_clean <- bind_rows(test_id, train_id)
  
  nrow(test_id)+nrow(train_id)-nrow(text_clean) #Debe dar cero


#Limpieza----

  text_clean_1 <- text_clean %>% 
    mutate(corpus = iconv(text, to = "ASCII//TRANSLIT")) %>%     #Acentos
    mutate(corpus = removeNumbers(text)) %>%                              #Números
    mutate(corpus = removePunctuation(corpus)) %>%                        #Puntuación
    mutate(corpus = tolower(corpus)) %>%                                  #Minúsculas
    mutate(corpus = stripWhitespace(corpus)) %>%                          #Espacios sobrantes
    mutate(corpus = str_replace_all(corpus, "[^[:alnum:]\\s]", "")) %>%   #Caracteres que no sean letras o espacios
    mutate(corpus = gsub("[^[:alpha:]#\\s]", " ", corpus)) %>%            #Caracteres que no sean alfanuméricos
    mutate(corpus = removeWords(corpus, stopwords("es"))) %>%             #Stopword
    mutate(corpus = gsub("\\d+", " ", corpus)) %>%                        #Reemplazar números
    mutate(corpus = gsub("http\\S+", " ", corpus)) %>%                    #Reemplazar URLs
    mutate(corpus = gsub("www\\S+", " ", corpus)) %>%                     #Reemplazar URLs
    mutate(corpus = gsub("\\b\\w{1,2}\\b", " ", corpus)) %>%              #Reemplzar palabras de 1 o 2 letras
    mutate(corpus = gsub("\\s+", " ", corpus))                            #Dobles espacios
  
  text_clean_1 <- text_clean_1 %>% mutate(n_palabras_i = str_count(text, "\\S+")) #contamos el número de palabras iniciales

  text_token_1 <- text_clean_1 %>% unnest_tokens("word", corpus)
  text_token_1 %>% count(word, sort = TRUE) %>% head()
  
  sw <- c()
  for (s in c("snowball", "stopwords-iso", "nltk")) {
    temp <- get_stopwords("spanish", source = s)$word
    sw <- c(sw, temp)
            }
  sw <- unique(sw)
  sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
  sw <- data.frame(word = sw)
  
  nrow(text_token_1)
  
  text_token_1 <- text_token_1 %>% anti_join(sw, by = "word")
  
  nrow(text_token_1)
  
  #Inicio Lemma
  
  model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
  
  palabras_unicas_1 <- text_token_1 %>% distinct(word = text_token_1$word)
  
  udpipe_results <- udpipe_annotate(model, x = palabras_unicas_1$word)
  
  udpipe_results <- as_tibble(udpipe_results)
  
  udpipe_results <- udpipe_results %>% select(token, lemma) %>% rename("word" = "token")
  
  text_token_1 <- text_token_1 %>% left_join(udpipe_results, by = "word", multiple = "all")
  
  text_token_1[is.na(text_token_1$lemma), "lemma"] <- text_token_1[is.na(text_token_1$lemma), "word"]
  
  #data <- test_token %>% group_by(lemma) %>% summarise(n = n()) %>% arrange(desc(-n)) %>% as.data.frame()
  #palabras_eliminar <- train_token %>% count(lemma) %>% filter(n < 20)
  
  #train_token <- train_token %>% anti_join(palabras_eliminar, by = "lemma") 
  
  repetidas <- text_token_1 %>% select(id, word = lemma)
  train_name <- train %>% select(id, name)
  repetidas <- left_join(repetidas, train_name, by = "id")
  table(repetidas$name)
  repetidas <- repetidas[complete.cases(repetidas$name),]
  repetidas <- repetidas %>% select(name, word)
  table(repetidas$name)
  repetidas_conteo <- repetidas %>% group_by(word) %>% summarise(n=n())
  
  Lopez <- repetidas$word[repetidas$name=="Lopez"]
  Petro <- repetidas$word[repetidas$name=="Petro"]
  Uribe <- repetidas$word[repetidas$name=="Uribe"]
  palabras_comunes <- intersect(intersect(Lopez, Petro), Uribe)
  contador <- sapply(palabras_comunes, function(x) sum(repetidas$word==x))
  resultado <- data.frame(word=palabras_comunes, freq=contador) %>% arrange(desc(freq)) %>% head(100)
  resultado_eliminar <- resultado %>% select(lemma = word)
  
  text_token_1 <- text_token_1 %>% anti_join(resultado_eliminar, by = "lemma")
  
  #Fin Lemma
  
  
  text_clean_2 <- text_token_1 %>% #Generamos la BD con el texto limpio
    group_by(id, n_palabras_i) %>% 
    summarise(text = str_c(lemma, collapse = " ")) %>%
    ungroup()
  
  text_clean_2 <- text_clean_2 %>% rename(text_ii=text)
  
  diferencia <- setdiff(text_clean_1$id, text_clean_2$id) %>% as.data.frame()
  
  dif <- nrow(diferencia)
  inicial <- nrow(text_clean_1)
  final <- nrow(text_clean_2)
  
  inicial - final #Debe ser igual a "dif" para comprobar lo que se eliminó
  
  text_clean_1 %>% filter (id == "bfa354b62f85d73f203c689d") #Ejemplo para validar una cadena de texto de una palabra que fue eliminada
  

#Separar las bases----

  text_clean_2 <- text_clean_2 %>% mutate(n_palabras_ii = str_count(text_ii, "\\S+")) #contamos el número de palabras iniciales
  test_id_1 <- left_join(test_id, text_clean_2, by ="id")
  train_id_1 <- left_join(train, text_clean_2, by = "id")

table(train$name)
table(train_id_1$name)

#Matrices para los modelos----
  
  #Matriz de Términos TRAIN
  
  tm_corpus <- Corpus(VectorSource(x = train_id_1$text_ii))
  str(tm_corpus)
  
  tf_idf <- TermDocumentMatrix(tm_corpus, control = list(weighting = weightTfIdf))
  tf_idf <- as.matrix(tf_idf) %>% t() %>% as.data.frame()
  
  train_id_1$text_ii[1]
  tf_idf[1, 1:10]
  head(tf_idf)
  dim(tf_idf)
  
  
  #Matriz de Términos TEST
  
  tm_corpus2 <- Corpus(VectorSource(x = test_id_1$text_ii))
  str(tm_corpus2)
  
  tf_idf2 <- TermDocumentMatrix(tm_corpus2, control = list(weighting = weightTfIdf))
  tf_idf2 <- as.matrix(tf_idf2) %>% t() %>% as.data.frame()
  
  test_id_1$text_ii[1]
  tf_idf2[1, 1:10]
  head(tf_idf2)
  dim(tf_idf2)
  
  
  #Matriz TRAIN
  
  columnas_seleccionadas <- intersect(colnames(tf_idf), colnames(tf_idf2))
  
  tf_idf <- tf_idf %>% select(all_of(columnas_seleccionadas))
  dim(tf_idf)
  
  #Ejecutamos si queremos todas las columnas comunes
  tf_idf_reducido <- tf_idf
  
  #Ejecutamos si queremos un número específico de columnas 
  columnas_seleccionadas_2 <- colSums(tf_idf) %>%
    data.frame() %>%
    arrange(desc(.)) %>%
    head(2000) %>%
    rownames()
  
  tf_idf_reducido <- tf_idf %>% select(all_of(columnas_seleccionadas_2)) 
  dim(tf_idf_reducido)
  
  
  #Matriz Test
  
  #Ejecutamos si queremos todas las columnas comunes
  tf_idf_reducido2 <- tf_idf2 %>% select(all_of(columnas_seleccionadas))
  dim(tf_idf_reducido2)
  
  #Ejecutamos si queremos un número específico de columnas 
  tf_idf_reducido2 <- tf_idf2 %>% select(all_of(columnas_seleccionadas_2))
  dim(tf_idf_reducido2)
  
  
  ##Modelo 14---------------------------------------------------------------------
  
  sum(is.na(train_id_1$name)) #Validamos que todos los registros están diligenciados
  
  train_id_1$name2 <- train_id_1$name  #Guardamos el nombre en una variable nueva para generar factores
  
  train_id_1$name2 <- ifelse(train_id_1$name2 == "Lopez", 1, 
                             ifelse(train_id_1$name2 == "Petro", 2, 3))
  
  train_id_1$name2 <- as.factor(train_id_1$name2)
  table(train_id_1$name2)
  
  Y <- train_id_1$name2
  Y <- to_categorical(Y)
  class(Y)
  head(Y)
  dim(Y)
  colSums(Y) #Verificamos que coincida con los valores de la tabla de nombres
  
  X <- as.matrix(tf_idf_reducido)
  class(X)
  dim(X)
  
  X1 <- scale(X[,1:5145])
  
  set.seed(10101)
  
  n <- nrow(train_id_1)
  data_rows <- floor(0.70 * n)
  train_indices <- sample(1:n, data_rows)
  X_train <- X1[train_indices, ]
  X_test <- X1[-train_indices, ]
  y_train <- Y[train_indices, ]
  y_test <- Y[-train_indices, ]
  
  n_h = nrow(X_train)/(2*(ncol(X_train) + 5))
  
  
#Modelo con todas las columnas - eliminando palabras con n < 10 en la limpieza
  
  rm(model_14)
  model_14 <- keras_model_sequential() 
  model_14 %>% 
    layer_dense(units = 5145, activation = 'relu', input_shape = ncol(X_train)) %>% 
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 512, activation = 'relu',  kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 256, activation = 'relu',  kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 128, activation = 'softmax')  %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')
  
  summary(model_14)
  
  model_14 %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = 'categorical_crossentropy', metrics = c('CategoricalAccuracy'))
  
  history_14 <- model_14 %>% fit(X_train, y_train, epochs = 200, batch_size = 2^8, validation_split = 0.3, callbacks = list(
    callback_reduce_lr_on_plateau(factor = 0.5, patience = 5), callback_early_stopping(patience = 10)))
  
  history_plot_14 <- plot(history_14) + theme_bw()
  history_plot_14
  
  model_14 %>% evaluate(X_test, y_test)
  
  y_hat_14 <- model_14 %>% predict(X_test) %>% k_argmax()
  
  confusionMatrix(data = factor(as.numeric(y_hat_14), levels = 1:3), 
                  reference = factor(train_id_1$name2[-train_indices], levels = 1:3))
  
  #Predicciones
  
  x_prueba <- as.matrix(tf_idf_reducido2)
  dim(tf_idf_reducido2)
  y_hat_test <- model_14 %>% predict(x_prueba) %>% k_argmax()
  
  resultados <- data.frame(id = test_id_1$id, name = factor(as.numeric(y_hat_test)))
  resultados$name <- ifelse(resultados$name == 1, "Lopez", ifelse(resultados$name == 2, "Petro", "Uribe"))
  write.table(resultados, "submission.csv", row.names = FALSE, quote=FALSE, sep = ",")