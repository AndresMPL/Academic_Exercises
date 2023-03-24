
#------------------------------------------------------------------------------#
#
#                            3 - DISTANCES
#
#------------------------------------------------------------------------------#

#Distancias----


train_sf <- st_as_sf(train, coords = c("lon", "lat"))
train_sf <- st_transform(train_sf,4686)

head(train)
head(train_sf)

##Parques----
  
  centroides_parques_d <- st_as_sf(centroides_parques, coords = c("x", "y"))
  dist_matrix <- st_distance(x = train_sf, y = centroides_parques_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_parque <- dist_min
  train_sf$distancia_parque <- dist_min
  
  
##Museos----
  
  centroides_museos_d <- st_as_sf(centroides_museos, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_museos_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_museo <- dist_min
  train_sf$distancia_museo <- dist_min
  
  
##IPS----

  centroides_ips_d <- st_as_sf(centroides_ips, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_ips_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_ips <- dist_min
  train_sf$distancia_ips <- dist_min
 
  
##ESE----
  
  centroides_ese_d <- st_as_sf(centroides_ese, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_ese_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_ese <- dist_min
  train_sf$distancia_ese <- dist_min
 
  
##Colegios----
  
  centroides_colegios_d <- st_as_sf(centroides_colegios, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_colegios_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_colegios <- dist_min
  train_sf$distancia_colegios <- dist_min

  
##CAI----

  centroides_cai_d <- st_as_sf(centroides_cai, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_cai_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_cai <- dist_min
  train_sf$distancia_cai <- dist_min

  
##Biblo Estaciones----
  
  centroides_best_d <- st_as_sf(centroides_best, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_best_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_best <- dist_min
  train_sf$distancia_best <- dist_min
  
  
##Centros Financieros----
  
  centroides_centrof_d <- st_as_sf(centroides_centrof, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_centrof_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_centrof <- dist_min
  train_sf$distancia_centrof <- dist_min
  
  
##Cuadrante Policía----
  
  centroides_cuadrantes_d  <- st_as_sf(centroides_cuadrantes , coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_cuadrantes_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_cuadrantes <- dist_min
  train_sf$distancia_cuadrantes <- dist_min
  
  
##Estaciones de buses----
  
  buses_puntos_d <- st_as_sf(buses_puntos, coords = c("x", "y"))
  st_crs(buses_puntos_d) <- 4686
  dist_matrix <- st_distance(x = train_sf, y = buses_puntos_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_buses <- dist_min
  train_sf$distancia_buses <- dist_min
  
##Estaciones de Transmilenio----
  
  centroides_tm <- st_as_sf(estaciones_tm_puntos, coords = c("x", "y"))
  st_crs(centroides_tm) <- 4686
  dist_matrix <- st_distance(x = train_sf, y = centroides_tm)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_tm <- dist_min
  train_sf$distancia_tm <- dist_min
  
  
  #Tabla de Estadísticas Descriptivas - Distancias
  
  glimpse(train)
  
  estadisticas2 <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms,
                                   distancia_parque, distancia_museo, distancia_ips, distancia_ese, 
                                   distancia_colegios, distancia_cai, distancia_best, distancia_centrof, 
                                   distancia_cuadrantes, distancia_buses, distancia_tm, mts2, surface_total_imp,
                                   surface_covered_imp, bedrooms_imp, bathrooms_imp, rooms_imp) %>% as.data.frame()
  
  estadisticas2 <- estadisticas2 %>% select(-geometry) %>% as.data.frame()
  stargazer(round(estadisticas2), digits = 2, title="Tabla de Estadísticas descriptivas", type='text')
  stargazer(round(estadisticas2), digits = 2, title="Tabla de Estadísticas descriptivas", type='latex')
  
  #Matriz de Correlaciones
  
  stargazer(cor(round(estadisticas2, 4)), title="Tabla de Correlaciones", type='text')
  stargazer(cor(round(estadisticas2, 4)), title="Tabla de Correlaciones", type='latex')
  
  