
#------------------------------------------------------------------------------#
#
#                        2 - GEOGRAPHICAL ENVIRONMENT
#
#------------------------------------------------------------------------------#

#Leemos los datos geográficos
#Datos descargados de la página Datos Abiertos de Bogotá - 6 e marzo de 2023

  #Para ejecutar el código es necesario descargar todas las carpetas que se encuentran en el siguiente repositorio:
  #https://github.com/AndresMPL/Repositorio_PS3/tree/main/datasets/localidades
  #Guardar estas carpetas en una carpeta llamada "datos" y fijar el directorio de trabajo en esta última carpeta contenedora

  parques <-st_read("datos/parques")
  museos <-st_read("datos/museos")
  ips <-st_read("datos/ips")
  ese <-st_read("datos/ese")
  colegios <-st_read("datos/colegios")
  cai <-st_read("datos/cai")
  biblioestacion <-st_read("datos/bibloestacion")
  centro_financiero <-st_read("datos/centro_financiero")
  delitos <-st_read("datos/delitos")
  cuadrantepolicia <-st_read("datos/cuadrantepolicia")
 
  #En "Delitos" generamos un total de casos para el año 2022
  #En el siguiente enlace se encuentra la descripción de las etiquetas de Delitos:
  #https://oaiee.scj.gov.co/agc/rest/services/Tematicos_Pub/CifrasSCJ/MapServer/layers
  
  delitos <- delitos %>% mutate(total_eventos_2022 = CMH22CONT+CMLP22CONT+CMHP22CONT+CMHR22CONT+CMHA22CONT+CMHB22CONT+
                                                     CMHCE22CON+CMHM22CONT+CMHC22CONT+CMDS22CONT+CMVI22CONT)
  
  delitos <- delitos %>% filter(total_eventos_2022 > 0)
  delitos <- delitos %>% select(CMNOMLOCAL, total_eventos_2022, geometry) #Nos quedamos solo con estos tres datos para unirlos a la BD Train-Test
  train <- st_join(train, delitos) #En cada inmueble agregamos el número de delitos de la localidad
  #train <- train %>% select(-CMNOMLOCAL.y, -total_eventos_2022.y)
  #train <- train %>% rename(CMNOMLOCAL = CMNOMLOCAL.x, total_eventos_2022 = total_eventos_2022.x)
  

  #Datos descargados de Open Street Map
  
  chapinero <- getbb(place_name = "UPZ Chapinero, Bogotá",
                     featuretype = "boundary:administrative",
                     format_out = "sf_polygon") %>% .$multipolygon
  
  bogota <- getbb(place_name = "Bogotá", 
                  featuretype = "boundary:administrative", 
                  format_out = "sf_polygon") %>% .$multipolygon
  
  bogota <- st_transform(bogota, 4686)

  bogota_mapa <- opq(bbox = getbb("Bogotá Colombia"))
  
#Transformamos los archivos de cada variable
  
  #Parques
  parques <- st_transform(parques,4686)
  sf_parques = parques %>% st_sf
  
  #Museos
  museos <- st_transform(museos,4686)
  sf_museos = museos %>% st_sf
  
  #IPS
  ips <- st_transform(ips,4686)
  sf_ips = ips %>% st_sf
  
  #ESE
  ese <- st_transform(ese,4686)
  sf_ese = ese %>% st_sf
  
  #Colegios
  colegios <- st_transform(colegios,4686)
  sf_colegios = colegios %>% st_sf
  
  #CAI
  cai <- st_transform(cai,4686)
  sf_cai = cai %>% st_sf
  
  #Biblioestacion
  biblioestacion <- st_transform(biblioestacion,4686)
  sf_biblioestacion = biblioestacion %>% st_sf
  
  #Centro financiero
  centro_financiero <- st_transform(centro_financiero,4686)
  sf_centro_financiero = centro_financiero %>% st_sf

  #Cuadrantes Policia
  centro_cuadrantepolicia <- st_transform(cuadrantepolicia,4686)
  sf_centro_cuadrantepolicia = centro_cuadrantepolicia %>% st_sf
  
  #Paradas de buses
  
  buses <- bogota_mapa %>% add_osm_feature(key="highway",value="bus_stop") %>% osmdata_sf()
  buses_puntos <- buses$osm_points
  
  #Transmilenio (Buscamos estaciones de TM)
  
  estaciones_tm <- bogota_mapa %>% add_osm_feature(key="amenity", value = "bus_station") %>% osmdata_sf()
  estaciones_tm_puntos <- estaciones_tm$osm_points

  #Delitos
  
  delitos <- st_transform(delitos,4686)
  sf_delitos = delitos %>% st_sf
  
  
#Generamos los centroides de cada variable
  
  centroides_parques  <- gCentroid(as(sf_parques$geometry, "Spatial"), byid = T)
  centroides_museos   <- gCentroid(as(sf_museos$geometry, "Spatial"), byid = T)
  centroides_ips      <- gCentroid(as(sf_ips$geometry, "Spatial"), byid = T)
  centroides_ese      <- gCentroid(as(sf_ese$geometry, "Spatial"), byid = T)
  centroides_colegios <- gCentroid(as(sf_colegios$geometry, "Spatial"), byid = T)
  centroides_cai      <- gCentroid(as(sf_cai$geometry, "Spatial"), byid = T)
  centroides_best     <- gCentroid(as(sf_biblioestacion$geometry, "Spatial"), byid = T)
  centroides_centrof  <- gCentroid(as(sf_centro_financiero$geometry, "Spatial"), byid = T)
  centroides_cuadrantes  <- gCentroid(as(sf_centro_cuadrantepolicia$geometry, "Spatial"), byid = T)
  
  
#Mapa Completo

  leaflet() %>%
    addTiles() %>%
    addPolygons(data=bogota, color = "#5CACEE", opacity = 0.4) %>%
    addCircles(data=train, radius = 0.1, color = "#DAA520", opacity = 2) %>%
    addPolygons(data=chapinero, color = "#008B45", opacity = 0.4) #%>%
    #addCircles(lng = centroides_colegios$x, lat = centroides_colegios$y, col = "#00CDCD", opacity = 0.8, radius = 0.5, popup = sf_colegios$NOMBRE_EST)%>%
    #addCircles(lng = centroides_parques$x, lat = centroides_parques$y, col = "#00EE00", opacity = 0.8, radius = 0.5, popup = sf_parques$NOMBRE)%>%
    #addCircles(lng = centroides_ips$x, lat = centroides_ips$y, col = "#FF3030", opacity = 0.8, radius = 0.5, popup = sf_ips$nombre_pre)%>%
    #addCircles(lng = centroides_ese$x, lat = centroides_ese$y, col = "#FF3030", opacity = 0.8, radius = 0.5, popup = sf_ese$RSOENTADSC)%>%
    #addCircles(lng = centroides_museos$x, lat = centroides_museos$y, col = "#0000FF", opacity = 0.8, radius = 0.5, popup = sf_museos$LecNombre)%>%
    #addCircles(lng = centroides_cai$x, lat = centroides_cai$y, col = "#008B00", opacity = 0.8, radius = 0.5, popup = sf_cai$CAIDESCRIP) %>%
    #addCircles(lng = centroides_best$x, lat = centroides_best$y, col = "#008B00", opacity = 0.8, radius = 0.5, popup = sf_biblioestacion$LecNombre) %>%
    #addCircles(lng = centroides_centrof$x, lat = centroides_centrof$y, col = "#008B00", opacity = 0.8, radius = 0.5)  #%>%
    #addPolygons(data = sf_delitos, fillColor  = sf_delitos$total_eventos_2022) %>%
    #addCircles(lng = centroides_cuadrantes$x, lat = centroides_cuadrantes$y, col = "#008B00", opacity = 0.8, radius = 0.5)
    
  
  