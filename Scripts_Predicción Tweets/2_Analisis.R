
#------------------------------------------------------------------------------#
#
#                              2 - ANALISIS
#
#------------------------------------------------------------------------------#

#BD TRAIN----

#Gráfica del número tweets por autor

  train_clean_2 <- train_clean_2 %>% mutate(n_palabras_ii = str_count(text, "\\S+")) #contamos el número de palabras iniciales
  
  n_tweets = train_clean_2 %>% group_by(name) %>% 
             summarise(n_tweets_final = n(), n_palabras_1 = sum(n_palabras_i), 
                       n_palabras_2 = sum(n_palabras_ii)) %>% mutate(Limpieza = 1- (n_palabras_2/n_palabras_1)) %>% as.data.frame()
  
  n_tweets <- inner_join(n_tweets_i, n_tweets, by = "name") 
  
  n_tweets
  library(xtable)
  tabla <- xtable(n_tweets)
  print(tabla)
  
  autor_tweets <- ggplot(n_tweets, aes(name, n_tweets_inicial, fill = name)) + 
                  geom_col() + geom_text(aes(label = n_tweets_inicial), vjust = -1, colour = "black") + 
                  ylim(c(0, 4000)) + theme_bw() + 
                  scale_fill_manual(values = c("cadetblue3", "#CCEDB1", "#FFB90F")) + 
                  theme(legend.position = "none") +
                  labs(x = "Autor", y = "Número de tweets", title = "Número de tweets por autor") +
                  theme(axis.text = element_text(size=12)) + 
                  theme(axis.title.x = element_text(size=rel(1.2))) +
                  theme(axis.title.y = element_text(size=rel(1.2))) 
  
  autor_tweets

  
#Palabras más usadas por autor

  train_cloud <- train_token %>% count(name, word) %>% group_by(name) %>% ungroup()
  
  autor_top_word <- train_cloud %>%
                    group_by(name) %>%
                    slice_max(n, n = 10) %>% 
                    ungroup() %>%
                    arrange(name, -n)

  imagen_top_autor <- ggplot(autor_top_word, aes(y = reorder(word, n), x = n, fill = factor(name))) +
                      geom_bar(stat = "identity") +
                      ggtitle("Términos más frecuentes") +
                      ylab("Términos") +
                      facet_wrap(~ name, scales = "free") +
                      xlab("Frecuencia") + theme_bw() +
                      scale_fill_manual(values = c("cadetblue3", "#CCEDB1", "#FFB90F")) +
                      theme(legend.position = "none") +
                      theme(axis.text = element_text(size=10), axis.title.x = element_text(size=rel(1.2)), axis.title.y = element_text(size=rel(1.2)))
  
  imagen_top_autor


#Nube de palabras por autor

  train_cloud <- train_token %>% count(name, word) %>% group_by(name) %>% ungroup()
  
  table(train_cloud$name) %>% as.data.frame()
  
  cloud_lopez <- train_cloud  %>% filter(name == "Lopez") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  cloud_uribe <- train_cloud %>% filter(name == "Uribe") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  cloud_petro <- train_cloud %>% filter(name == "Petro") %>% select(word, n) %>% arrange(desc(n)) %>% head(50)
  
  wordcloud(train_cloud$word, freq = train_cloud$n, min.freq = 70, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_lopez$word, freq = cloud_lopez$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_uribe$word, freq = cloud_uribe$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  wordcloud(cloud_petro$word, freq = cloud_petro$n, colors= brewer.pal(8, "Dark2"),random.order = FALSE)
  
