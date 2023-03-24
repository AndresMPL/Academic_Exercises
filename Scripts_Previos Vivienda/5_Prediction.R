
#------------------------------------------------------------------------------#
#
#                             5 - PREDICTION
#
#------------------------------------------------------------------------------#

#Limpieza de la BD----

  id <- test$property_id
  descripcion <- test$description
  titulo <- test$title
  
  df <- data.frame(id,descripcion,titulo)

#Ponemos todo en minúscula, quitamos espacios en blanco sobrante y signos de puntuación

test$descripcion <- removePunctuation(test$description)
test$descripcion <- tolower(test$description)
test$descripcion <- stripWhitespace(test$description)

#Generamos bigramas

bigrams <- test %>% ungroup() %>%
  unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)


#Eliminamos los bigramas que no contengan información de metros cuadrados

descripcion_keep <- c("mts2", "m", "mts", "metros", "m2", "mt2")
bigrams_keep <- data.frame(word2 = descripcion_keep)

bigrams2 <- bigrams %>% ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  semi_join(bigrams_keep, by = "word2") %>%
  unite(bigram, word1, sep = " ")


#Se borra la plabra relacionada a metros cuadrados

test2 <- bigrams2 %>% select(-c("word2"))
colnames(test2)

test2 <- transform(test2,bigram = as.numeric(bigram))

sapply(test2, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables

filtro <- is.na(test2$bigram) #Transformamos los NA a ceros
sum(filtro)
test2$bigram[filtro] <- 0

test2 <- test2 %>% rename(mts2=bigram)

#Buscamos informacion sobre si las propiedades tienen parqueaderos / garajes o no a partir de la descripción

library(tokenizers)
test2$parqueadero <- tokenize_words(test2$description)
descripcion_keep2 <- c("parqueadero?", "garaje?")
test2$parqueadero <- as.logical(grepl(paste(descripcion_keep2, collapse = "|"), test2$parqueadero))
test2$parqueadero <- as.integer(as.logical(test2$parqueadero))

test_area <- test2 %>% select(property_id, mts2, parqueadero)

test_area2 <- test_area %>% group_by(property_id) %>% slice(1)

test <- left_join(test, test_area2)

#Limpiamos la BD----

test <- test %>% mutate(latp=lat,longp=lon, ln_price = log(price))
test <- sf::st_as_sf(test,coords=c('longp','latp'),crs = 4686)
class(test)
head(test)


#Imputamos las variables surface_total, surfaced_covered, bedrooms, bathrooms, rooms y guardamos las originales por separado
  
  test$surface_total_imp <- test$surface_total
  test$surface_covered_imp <- test$surface_covered
  test$bedrooms_imp <- test$bedrooms
  test$bathrooms_imp <- test$bathrooms
  test$rooms_imp <- test$rooms
  
  filtro <- is.na(test$surface_total) 
  sum(filtro)
  test$surface_total_imp[filtro] <- mean(test$surface_total, na.rm = T)
  
  filtro <- is.na(test$surface_covered) 
  sum(filtro)
  test$surface_covered_imp[filtro] <- mean(test$surface_covered, na.rm = T)
  
  filtro <- is.na(test$bedrooms) 
  sum(filtro)
  test$bedrooms_imp[filtro] <- mean(test$bedrooms, na.rm = T)
  
  filtro <- is.na(test$bathrooms) 
  sum(filtro)
  test$bathrooms_imp[filtro] <- mean(test$bathrooms, na.rm = T)
  
  filtro <- is.na(test$rooms)
  sum(filtro)
  test$rooms_imp[filtro] <- mean(test$rooms, na.rm = T)
  
  filtro <- is.na(test$lat) | is.na(test$lon) #| is.na(test$rooms) | is.na(test$bathrooms) #| is.na(test$surface_total) | is.na(test$surface_covered) 
  test <- test[!filtro, ] #Filtramos los registros sin lat y lon
  
  limites <- getbb("Bogota Colombia") #Aseguramos que sean inmuebles en Bogotá
  filtro1 <- between(test$lon, limites[1, "min"], limites[1, "max"])
  filtro2 <- between(test$lat, limites[2, "min"], limites[2, "max"])
  filtro <- filtro1 & filtro2
  test <- test[filtro,] 
  
  test <- test %>% mutate(area_maxima = ifelse(surface_total_imp>surface_covered_imp, surface_total_imp, surface_covered_imp)) #Seleccionamos el área máxima

#Geografía----
  
  test <- st_join(test, delitos) #En cada inmueble agregamos el número de delitos de la localidad
  
  
#Distancias---- 


  test_sf <- st_as_sf(test, coords = c("lon", "lat"))
  test_sf <- st_transform(test_sf,4686)

  ##Parques----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_parques_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_parque <- dist_min
  test_sf$distancia_parque <- dist_min
  
  ##Museos----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_museos_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_museo <- dist_min
  test_sf$distancia_museo <- dist_min
  
  ##IPS----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_ips_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_ips <- dist_min
  test_sf$distancia_ips <- dist_min
  
  ##ESE----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_ese_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_ese <- dist_min
  test_sf$distancia_ese <- dist_min
  
  ##Colegios----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_colegios_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_colegios <- dist_min
  test_sf$distancia_colegios <- dist_min
  
  ##CAI----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_cai_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_cai <- dist_min
  test_sf$distancia_cai <- dist_min
  
  ##Biblo Estaciones----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_best_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_best <- dist_min
  test_sf$distancia_best <- dist_min
  
  ##Centros Financieros----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_centrof_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_centrof <- dist_min
  test_sf$distancia_centrof <- dist_min
  
  ##Cuadrante Policía----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_cuadrantes_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_cuadrantes <- dist_min
  test_sf$distancia_cuadrantes <- dist_min
  
  ##Estaciones de buses----
  
  dist_matrix <- st_distance(x = test_sf, y = buses_puntos_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_buses <- dist_min
  test_sf$distancia_buses <- dist_min
  
  ##Estaciones de Transmilenio----
  
  dist_matrix <- st_distance(x = test_sf, y = centroides_tm)
  dist_min <- apply(dist_matrix, 1, min)
  
  test$distancia_tm <- dist_min
  test_sf$distancia_tm <- dist_min
  
  
#Predicciones para Kaggle----

test$price <- predict(modelo_rf, newdata = test)
exportar <- test %>% select(property_id, price, -geometry) %>% as.data.frame()
exportar <- exportar %>% select(property_id, price) %>% as.data.frame()
write.csv(exportar, "submission.csv", row.names = FALSE)

