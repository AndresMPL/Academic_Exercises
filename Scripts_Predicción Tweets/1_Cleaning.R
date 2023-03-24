
#------------------------------------------------------------------------------#
#
#                                PROBLEM SET 4
#
#                            "Predicting Tweets"
#
#       Grupo 5:  Isabella Mendez Pedraza.
#                 Manuela Ojeda Ojeda.
#                 Juan Sebastian Tellez Melo.
#                 Andres Mauricio Palacio Lugo.
#
#------------------------------------------------------------------------------#

#Cargamos librerías y verificamos que no generen conflictos

  library(pacman)
  
  p_load(dplyr, tidyverse, tm, textir, tidytext, wordcloud, SentimentAnalysis, 
         udpipe, syuzhet,stringi,stopwords,textstem,topicmodels, rio, caret, sentimentr,
         janitor, wordcloud2, udpipe,ggcorrplot)

  rm(list=ls())

  
#Importamos los datos desde GitHub
  
  test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/test.csv")
  train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS4/main/datasets/train.csv")
  
  glimpse(train)
  glimpse(test)
  

#Limpieza BD Train--------------------------------------------------------
  
  train_clean <- train %>% 
    mutate(corpus = stri_trans_general(text, id = "Latin-ASCII")) %>%     #Acentos
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

  train_clean <- train_clean %>% mutate(n_palabras_i = str_count(text, "\\S+")) #contamos el número de palabras iniciales
 
   n_tweets_i = train_clean %>% group_by(name) %>% 
    summarise(n_tweets_inicial = n()) %>% 
                  as.data.frame()
  
#Tokens
#Generamos lo tokens de las expresiones y corregimos mas stopwords
  
  train_token <- train_clean %>% unnest_tokens("word", corpus)
  train_token %>% count(word, sort = TRUE) %>% head()
  
  sw <- c()
  for (s in c("snowball", "stopwords-iso", "nltk")) {
       temp <- get_stopwords("spanish", source = s)$word
       sw <- c(sw, temp)
        }
      sw <- unique(sw)
      sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
      sw <- data.frame(word = sw)
  
  nrow(train_token)
  
  train_token <- train_token %>% anti_join(sw, by = "word")
  
  nrow(train_token)
  
  
#Lemma
#Lematización de los textos
  
  udpipe::udpipe_download_model('spanish') #Descargar en caso que no se tenga
  
  model <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
  
  palabras_unicas <- train_token %>% distinct(word = train_token$word)
  
  udpipe_results <- udpipe_annotate(model, x = palabras_unicas$word)
  
  udpipe_results <- as_tibble(udpipe_results)
  
  udpipe_results <- udpipe_results %>% select(token, lemma) %>% rename("word" = "token")
  
  train_token <- train_token %>% left_join(udpipe_results, by = "word", multiple = "all")
  
  train_token[is.na(train_token$lemma), "lemma"] <- train_token[is.na(train_token$lemma), "word"]
  
  data <- train_token %>% group_by(lemma) %>% summarise(n = n()) %>% arrange(desc(-n)) %>% as.data.frame()
  palabras_eliminar <- train_token %>% count(lemma) %>% filter(n < 10)
  
  train_token <- train_token %>% anti_join(palabras_eliminar, by = "lemma") 
  
  train_clean_2 <- train_token %>% #Generamos la BD con el texto limpio
    group_by(name, id, n_palabras_i) %>% 
    summarise(text = str_c(lemma, collapse = " ")) %>%
    ungroup()
  
  diferencia <- setdiff(train_clean$id, train_clean_2$id) %>% as.data.frame()
  
  dif <- nrow(diferencia)
  inicial <- nrow(train_clean)
  final <- nrow(train_clean_2)
  
  inicial - final #Debe ser igual a "dif" para comprobar lo que se eliminó
  
  train_clean %>% filter (id == "ce1464da0f03a61f2659947b") #Ejemplo para validar una cadena de texto de una palabra que fue eliminada

  
#Matriz de Términos
  
  tm_corpus <- Corpus(VectorSource(x = train_clean_2$text))
  str(tm_corpus)
  
  tf_idf <- TermDocumentMatrix(tm_corpus, control = list(weighting = weightTfIdf))
  tf_idf <- as.matrix(tf_idf) %>% t() %>% as.data.frame()
  
  train_clean_2$text[1]
  tf_idf[1, 1:10]
  head(tf_idf)
  dim(tf_idf)
  
  
#Limpieza BD Test--------------------------------------------------------
#Considerar los comentarios de limpieza del script 1_Cleaning
  
  test_clean <- test %>% 
    mutate(corpus = stri_trans_general(text, id = "Latin-ASCII")) %>%    
    mutate(corpus = removeNumbers(text)) %>%
    mutate(corpus = removePunctuation(corpus)) %>%
    mutate(corpus = tolower(corpus)) %>% 
    mutate(corpus = stripWhitespace(corpus)) %>%
    mutate(corpus = str_replace_all(corpus, "[^[:alnum:]\\s]", ""))  %>%
    mutate(corpus = gsub("[^[:alpha:]#\\s]", " ", corpus)) %>% 
    mutate(corpus = removeWords(corpus, stopwords("es"))) %>%
    mutate(corpus = gsub("\\d+", " ", corpus)) %>%
    mutate(corpus = gsub("http\\S+", " ", corpus)) %>% 
    mutate(corpus = gsub("www\\S+", " ", corpus)) %>%
    mutate(corpus = gsub("\\b\\w{1,2}\\b", " ", corpus)) %>% 
    mutate(corpus = gsub("\\s+", " ", corpus)) 
  
  test_clean <- test_clean %>% mutate(n_palabras_i = str_count(text, "\\S+"))
  
#Tokens
  
  test_token <- test_clean %>% unnest_tokens("word", corpus)
  test_token %>% count(word, sort = TRUE) %>% head()
  
  sw <- c()
  for (s in c("snowball", "stopwords-iso", "nltk")) {
    temp <- get_stopwords("spanish", source = s)$word
    sw <- c(sw, temp)
  }
  sw <- unique(sw)
  sw <- unique(stri_trans_general(str = sw, id = "Latin-ASCII"))
  sw <- data.frame(word = sw)
  
  nrow(test_token)
  
  test_token <- test_token %>% anti_join(sw, by = "word")
  
  nrow(test_token)
  
#Lemma
  
  palabras_unicas2 <- test_token %>% distinct(word = test_token$word)
  
  udpipe_results2 <- udpipe_annotate(model, x = palabras_unicas2$word)
  
  udpipe_results2 <- as_tibble(udpipe_results2)
  
  udpipe_results2 <- udpipe_results2 %>% select(token, lemma) %>% rename("word" = "token")
  
  test_token <- test_token %>% left_join(udpipe_results2, by = "word", multiple = "all")
  
  test_token[is.na(test_token$lemma), "lemma"] <- test_token[is.na(test_token$lemma), "word"]
  
  conteo2 <- test_token %>% count(lemma) %>% arrange(desc(n)) %>% tail(1000)
  
  test_clean_2 <- test_token %>%
    group_by(id, n_palabras_i) %>% 
    summarise(text = str_c(lemma, collapse = " ")) %>%
    ungroup()
  
  test_clean_2 <- test_clean_2 %>% mutate(n_palabras_i2 = str_count(text, "\\S+"))
  
  diferencia2 <- setdiff(test_clean$id, test_clean_2$id) %>% as.data.frame()
  
  dif2 <- nrow(diferencia2)
  inicial2 <- nrow(test_clean)
  final2 <- nrow(test_clean_2)
  
  inicial2 - final2
  
  test_clean %>% filter (id == "cb9ac947c675464803342fc9") #Ejemplo para validar
  
  test_final <- test %>% select(id)
  test_final <- test_final %>% left_join(test_clean_2)
  
  #Matriz de Términos
  
  tm_corpus2 <- Corpus(VectorSource(x = test_final$text))
  str(tm_corpus2)
  
  tf_idf2 <- TermDocumentMatrix(tm_corpus2, control = list(weighting = weightTfIdf))
  tf_idf2 <- as.matrix(tf_idf2) %>% t() %>% as.data.frame()
  
  test_clean_2$text[1]
  tf_idf2[1, 1:10]
  head(tf_idf2)
  dim(tf_idf2)
  
  
#Matrices para los modelos----
  
#Matriz Train
  
  columnas_seleccionadas <- intersect(colnames(tf_idf), colnames(tf_idf2)) 
  
  tf_idf <- tf_idf %>% select(all_of(columnas_seleccionadas))
  dim(tf_idf)
  
  #Ejecutamos si queremos todas las columnas comunes
  tf_idf_reducido <- tf_idf
  
  #Ejecutamos si queremos un número específico de columnas 
  columnas_seleccionadas_2 <- colSums(tf_idf) %>%
    data.frame() %>%
    arrange(desc(.)) %>%
    head(1000) %>%
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
  