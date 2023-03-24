
#------------------------------------------------------------------------------#
#
#                                PROBLEM SET 2
#
#   Grupo 5:  Isabella Mendez Pedraza.
#             Manuela Ojeda Ojeda.
#             Juan Sebastian Tellez Melo.
#             Andres Mauricio Palacio Lugo.
#
#------------------------------------------------------------------------------#

#Pasos previos

  rm(list=ls())


#Cargar librerías
  
  library(pacman)
  p_load(glmnet, skimr, stargazer, dplyr, kableExtra, AER, MLmetrics, tidymodels, rpart.plot, rpart, rattle,
         themis, smotefamily, ROSE, fastDummies, tidyverse, caret, xtable) #Verificamos que ninguna librería genere conflicto

  
#Leer los datos - 
  
  setwd("/Users/manuelaojeda/Desktop/Universidad /MAESTRIA") #Por tamaño de los archivos, seleecionar el directorio local

  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")
  train_hogares     <- read.csv("train_hogares.csv")
  train_personas    <- read.csv("train_personas.csv")

  prop.table(table(train_hogares$Pobre))
  
  
#Organización de las BD

    #test_hogares      <- para la prueba final del modelo seleccionado
    #test_personas     <- para la prueba final del modelo seleccionado
    
    #train_hogares     <- BD Original
       #train_h        <- BD ajustada para trabajar
          #train_hh    <- 70% Train hogares - para entreamiento | Las BD estabndarizadas tendrán (s) al final
          #test_hh     <- 20% Train hogares - para pruebas      |
          #eval_hh     <- 10% Train hogares - para evaluacion   | 
    
    #train_personas    <- BD Original
       #train_p        <- BD ajustada para trabajar
          #train_pp    <- 70% Train personas - para entreamiento
          #test_pp     <- 20% Train personas - para pruebas
          #eval_pp     <- 10% Train personas - para evaluacion  
  
#Selección de Variables de interés
  
  train_p <- train_personas %>% 
    select(id, Clase, Orden, P6020, P6040, P6050, P6090, P6100, P6210, P7472, P7422, P7500s2, P7500s3, P7505, P7510s1, P7510s2, P7510s3, P7510s5, P7510s6, P7510s7,  Pet, Oc, Des, Ina, Ingtot)
  
  train_h <- train_hogares %>% 
    select(Pobre, id, Clase, P5000, P5010, P5090, Nper, Npersug, Lp, Ingtotugarr, Ingpcug)
  
  glimpse(test_hogares)
  glimpse(test_personas)
  glimpse(train_h)
  glimpse(train_p)
  
  
#Crear variables de interes  
  
  train_p$Genero <- ifelse(train_p$P6020 == 2, 1, 0) %>% as.numeric()
  train_p$Menores_edad <- if_else(train_p$P6040<=14, 1, 0 , missing = NULL)
  train_p$adulto_mayor <- if_else(train_p$P6040>=65, 1, 0 , missing = NULL)
  train_p <- train_p %>%
                    mutate(Desempleado = replace_na(Des, 0),
                    Inactivo = replace_na(Ina, 0),
                    Ocupado = replace_na(Oc, 0),
                    Pet = replace_na(Pet,0))
  
  train_p$Clase <- if_else(train_p$Clase== 2, 1, 0 , missing = NULL)
  train_h$Clase <- if_else(train_h$Clase== 2, 1, 0 , missing = NULL)
  
  train_personas_hog <- train_p %>%
                        group_by(id) %>%
                        summarize(edad_jefe_hogar = (P6040)[P6050==1], 
                        sexo_jefe_hogar = (Genero)[P6050==1],
                        nivel_edu_jefe_hogar= (P6210)[P6050==1],
                        num_Menores_edad = sum(Menores_edad, na.rm = TRUE),
                        num_adulto_mayor = sum(adulto_mayor, na.rm = TRUE),
                        jefe_hogar_des = (Desempleado)[P6050==1],
                        jefe_hogar_ina = (Inactivo)[P6050==1],
                        jefe_hogar_oc = (Ocupado)[P6050==1],
                        num_oc_hogar = sum(Ocupado))
  
  train_h <- train_h %>% rename(num_cuartos = P5000, num_cuartos_dormir = P5010) #Se renombran variables
  train_p <- train_p %>% rename(edad = P6040) #se renombran variables
  
#Uniendo bases
  
  train_h <- left_join(train_h,train_personas_hog)
  colnames(train_h) 
  
  #Creacion de variables en base pegada de hogares
  
  train_h$jefe_hogar_ina2 <- if_else(train_h$edad_jefe_hogar==11, as.integer(1), train_h$jefe_hogar_ina) ##observacion de 11 años que no clasifica como inactivo, desempleado, ocupado se asigna como inactiva
  train_h <- train_h %>% select(-jefe_hogar_ina)# se retira la variable antigua
  train_h <- train_h %>% rename(jefe_hogar_ina = jefe_hogar_ina2)#se renombra la nueva

  train_h <- train_h %>% mutate(Numper_por_dor= Nper/num_cuartos_dormir,
                                Hacinamiento = if_else(Numper_por_dor>3, 1 , 0),
                                Ocupados_por_perhog = if_else(num_oc_hogar>0, Npersug/num_oc_hogar, as.double(Npersug)))

#Variables con NA´s

  #Personas
    missing_percentage <-sapply(train_p, function(y) sum(length(which(is.na(y))))/length(train_p$id))
    data_x <- as.data.frame(missing_percentage)
    var <- cbind(Var_name = rownames(data_x), data_x)
    rownames(var) <- 1:nrow(var)
    var_delete <- var[var$missing_percentage>0.5,]
    var_keep <- var[var$missing_percentage<=0.5,]
    count(var) # Contamos cuantas variables tenemos en total 
    count(var_keep) # Contamos cuantas variables tienen % missing menor o igual a 50%
    count(var_delete) # Contamos cuantas variables tienen % missing mayor a 50% 
    
    conteo_variables <- data.frame( total_var = nrow(var),
                                    keep_bar = nrow(var_keep),
                                    delete_var = nrow(var_delete))
    
    conteo_variables #Tabla de clasificación de variables
   
    vector_var <- as.vector(var_keep[1]) #Llevamos las variables que trabajaremos a un vector
    
    train_p <- train_p %>% select(all_of(vector_var$Var_name))
    
    sapply(train_p, function(x) sum(is.na(x))) %>% as.data.frame()
    
    #Hogares
    missing_percentage2 <-sapply(train_h, function(y) sum(length(which(is.na(y))))/length(train_h$id))
    data_x2 <- as.data.frame(missing_percentage2)
    var2 <- cbind(Var_name = rownames(data_x2), data_x2)
    rownames(var2) <- 1:nrow(var2)
    var_delete2 <- var2[var2$missing_percentage2>0.5,]
    var_keep2 <- var2[var2$missing_percentage2<=0.5,]
    count(var2) # Contamos cuantas variables tenemos en total 
    count(var_keep2) # Contamos cuantas variables tienen % missing menor o igual a 50%
    count(var_delete2) # Contamos cuantas variables tienen % missing mayor a 50% 
    
    conteo_variables2 <- data.frame( total_var = nrow(var2),
                                    keep_bar = nrow(var_keep2),
                                    delete_var = nrow(var_delete2))
    
    conteo_variables2 #Tabla de clasificación de variables
    
    vector_var2 <- as.vector(var_keep2[1]) #Llevamos las variables que trabajaremos a un vector
    
    train_h <- train_h %>% select(all_of(vector_var2$Var_name))
    
    sapply(train_h, function(x) sum(is.na(x))) %>% as.data.frame() #no se excluyen variables
    
#Mutamos factores
    
    names(train_p)
    names(train_h)
    
  #Factores de Personas
      
    train_p <- train_p %>% mutate(Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                                  Genero=factor(Genero,levels=c(0,1),labels=c("Hombre","Mujer")),
                                  Parentesco_con_jefe=factor(P6050,levels=c(1,2,3,4,5,6,7,8,9),labels=c("jefe", "pareja", "Hijo/hija", "nieto", "otro", "emplead_servicio", "pensionista", "trabajador", "otro_no_pariente")),
                                  nivel_edu=factor(P6210,levels=c(1,2,3,4,5,6,9),labels=c("Ninguno", "Preescolar", "Basica_primaria", "Basica_secundaria", "Media", "Superior", "No_Saber")),
                                  Desempleado=factor(Desempleado,levels=c(0,1),labels=c("No","Si")),
                                  Inactivo=factor(Inactivo,levels=c(0,1),labels=c("No","Si")),
                                  Ocupado=factor(Ocupado,levels=c(0,1),labels=c("No","Si")),
                                  Pet=factor(Pet,levels=c(0,1),labels=c("No","Si")))
    
    train_p <- train_p %>% select(-P6020, -P6050, -P6210) # se creo una nueva variable con factores y se elimino la anterior
    
    head(train_p)
    
    
  #Factores de Hogares

      train_h <- train_h %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No_Pobre","Pobre")),
                                    Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                                    Vivienda=factor(P5090,levels=c(1,2,3,4,5,6),labels=c("Propia_paga","Propia_No_Paga", "Arriendo","Usufructo", "Ocupante_No_Dueño", "Otra")),
                                    sexo_jefe_hogar=factor(sexo_jefe_hogar,levels=c(0,1),labels=c("Hombre","Mujer")),
                                    nivel_edu_jefe_hogar=factor(nivel_edu_jefe_hogar,levels=c(1,2,3,4,5,6,9),labels=c("Ninguno", "Preescolar", "Basica_primaria", "Basica_secundaria", "Media", "Superior", "No_Saber")),
                                    jefe_hogar_des=factor(jefe_hogar_des,levels=c(0,1),labels=c("No","Si")),
                                    jefe_hogar_ina=factor(jefe_hogar_ina,levels=c(0,1),labels=c("No","Si")),
                                    jefe_hogar_oc=factor(jefe_hogar_oc,levels=c(0,1),labels=c("No","Si")),
                                    Hacinamiento = factor(Hacinamiento, levels = c(0,1), labels = c("No","Si")))
  ##Estadisticas descriptivas con variables originales 
      
      trainh_ed <- train_h %>% select(Nper, Ingpcug, num_oc_hogar, sexo_jefe_hogar)
      p_load(GGally)
      ggpairs(trainh_ed, columns = 1:3, ggplot2::aes(colour = sexo_jefe_hogar)) + theme_bw()
      
      train_h <- train_h %>% select(-P5090) # se creo una nueva variable con factores y se elimino la anterior
      
      glimpse(train_h)

      #Copia de la BD ajustada - Para usar sin Dummys
      train_h_factores <- train_h
      
#Generamos dummys en Train_Personas
  
      train_p <- dummy_cols(train_p, 
                            select_columns = c("Clase", "Pet", "Genero", "Desempleado", "Inactivo", "Ocupado", "Parentesco_con_jefe", "nivel_edu", "no_ingresos"), 
                            remove_selected_columns = TRUE)
      
      glimpse(train_p)
    
#Generamos dummys en Train_Hogares
      
      train_h <- dummy_cols(train_h, 
                            select_columns = c("Pobre", "Clase", "Vivienda", "sexo_jefe_hogar", "nivel_edu_jefe_hogar", "jefe_hogar_des", "jefe_hogar_oc", "jefe_hogar_ina", "Hacinamiento"), 
                            remove_selected_columns = TRUE)
     
      glimpse(train_h)
      
      train_hp3 <-train_h
      
##Estadisticas descriptivas---------------------------------------------
      
#install.packages("GGally")  
#p_load(GGally)
#ggpairs(train_h, columns = 2:5, ggplot2::aes(colour = train_h$sexo_jefe_hogar_mujer))

      train_h2<- train_h %>% 
        select(num_cuartos_dormir,Nper, Ingpcug, edad_jefe_hogar, sexo_jefe_hogar_Hombre, Clase_Urbano, Vivienda_Propia_paga, Vivienda_Arriendo, nivel_edu_jefe_hogar_Media) 
      
      summary(train_h2)
      
      stargazer(train_h2, title="Estadísticas descriptivas", type='latex')
      
      dist_ingreso <- ggplot(data = train_h,
                          mapping = aes(x = Ingpcug))  + 
                          geom_histogram(aes(y =after_stat(density)),
                          bins = 9,
                          position = 'identity',
                          color="#424242", fill="#E3E3E3") +
                          stat_function(fun = dnorm, xlim = c(min(train_h$Ingpcug),max(train_h$Ingpcug)), colour="#1C86EE", linewidth=1,
                          args = list(mean = mean(train_h$Ingpcug), 
                          sd = sd(train_h$Ingpcug))) + 
                          labs(title = 'Figura 4: Distribución de ingreso',
                          x = 'Ingreso',
                          y = 'Frecuencia') + 
                          theme_bw()
      
      dist_ingreso
      
      
      
      imagen_1 <- ggplot(train_h, aes(x = Pobre_Pobre)) +
                  geom_bar(fill = "#B3B3B3") +
                  theme_bw() +
                  scale_y_continuous(labels = label_number()) +
                  labs(title = "Figura 1: Distribución de la Clasificación de Pobreza por hogares",
                       y = "Número de hogares",
                       x = "Clasificación")
      
      imagen_1
      install.packages("lessR")
      p_load(lessR)
      PieChart(Pobre_Pobre, hole=0, values="%", data=train_h, fill=1:2, weights=train_hogares$fex_c, radius=1, main="")
      