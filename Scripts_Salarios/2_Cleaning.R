
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
#                      2. Datos y pasos previos
#---------------------------------------------------------------------------


#Limpieza de area de trabajo --------------------------------------------

rm(list = ls())

#Seleccionar directorio---------------------------------------------------

setwd("/Users/manuelaojeda/Desktop/Universidad /MAESTRIA")

#Cargar paquetes---------------------------------------------------

library(pacman)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest,
       stargazer,
       knitr,
       boot,
       data.table)

library(dplyr)
library(tidyr)

##############################################################################
#       1.1 Scrapping (obtener datos)
##############################################################################

#Creamos un vector con los diferentes links ---------------------------------

link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")


#Se crea un loop para agrupar las diferentes tablas --------------------------

base <- data.frame()
for(i in link){
  print(i)
  GEIH<- read_html(i) %>% 
    html_table() %>%
    as.data.frame()
  base <- rbind(base, GEIH)
}

#Visualizar base -------------------------

View(base)

#Exportar base ---------------------------

write.csv(base, "GEIH.csv")


##############################################################################
#       1.2 Limpieza de datos
##############################################################################


#Selección de Variables de interés ----------------------------

dt_total <- base %>% 
  select(directorio, age, college, cuentaPropia, dsi, estrato1, formal, hoursWorkUsual, informal, ingtot, maxEducLevel, microEmpresa, p6426, ocu, oficio, relab, sex, sizeFirm, y_total_m_ha, y_total_m)


#Filtrar los individuos empleados y mayores de edad -------------------

base_fin <- dt_total %>% 
  subset(age>=18) %>% 
  subset(ocu==1)


#Sacamos el porcentaje de missing values por variable ---------------------------

missing_percentage <-sapply(base_fin, function(y) sum(length(which(is.na(y))))/length(base_fin$directorio))

#creo una función para identificar el porcetnaje de NA´s hay por columna ------------------------

data_x <- as.data.frame(missing_percentage)
View(data_x)

#Se eliminan las variables con un alto porcentaje de missing value (>50%)----------------------

var <- cbind(Var_name = rownames(data_x), data_x)
rownames(var) <- 1:nrow(var)
var_delete <- var[var$missing_percentage>=0.5,]
var_keep <- var[var$missing_percentage<0.5,]
count(var) # Contamos cuantas variables tenemos en total ----------------------
count(var_keep) # Contamos cuantas variables tienen % missing menor o igual a 50% -----------------
count(var_delete) # Contamos cuantas variables tienen % missing mayor a 50% ---------------------

View(var_keep)
conteo_variables <- data.frame( total_var = nrow(var),
                                keep_bar = nrow(var_keep),
                                delete_var = nrow(var_delete))

conteo_variables #comprobamos el número de variables
vector_var <- as.vector(var_keep[1]) #Llevamos las variables que trabajaremos a un vector

#Limpiamos los datos de NA´s

filas_total <- nrow(base_fin)    #contamos las filas
na_total <- sum(is.na(base_fin$maxEducLevel))   #agregamos el número filas que queden con NA en Educ Level
base_fin <- base_fin %>% filter(maxEducLevel != "NA")   #eliminamos las filas con NA en Educ Level
na_total <- na_total + sum(is.na(base_fin$y_total_m_ha))   #agregamos el número filas que queden con NA en y_total_m_ha
base_fin <- base_fin %>% filter(y_total_m_ha != "NA")   #eliminamos las filas con NA en Educ Level
filas_final <- nrow(base_fin)  #contamos las filas que quedaron
filas_total - na_total - filas_final    #debe dar 0 cuando se cumpla la diferencia después de las eliminaciones

conteo_na <- sum(is.na(base_fin$female)) +
  sum(is.na(base_fin$maxEducLevel)) +
  sum(is.na(base_fin$y_total_m_ha)) +
  sum(is.na(base_fin$age))
conteo_na #cero es correcto

#Seleccionamos las variables que cumplen con el requisito y generamos y validamos las variables que necesitamos

dt_final <- base_fin %>% select(all_of(vector_var$Var_name))

dt_final <- dt_final %>% 
  mutate(Ingresos_laborales = log(y_total_m_ha))

dt_final <- dt_final %>% 
  mutate(age_squred = age^2)


colnames(dt_final)[13]="experiencia"

str(dt_final)
head(dt_final)

dt_final$female <- ifelse(dt_final$sex == 0, 1, 0) %>% as.numeric()

#Definimos las variables categoricas

Variables_categoricas <- c("maxEducLevel", "relab", "sizeFirm", "oficio" )

for (v in Variables_categoricas) 
{dt_final[, v] <- as.factor(dt_final[, v, drop = T])}

#Dummyficar la base 

dt_final <- model.matrix(~ ., dt_final) %>%
  as.data.frame()


################################################################################
#       1.3 Estadisticas descriptivas
################################################################################
dt_final2<- dt_final %>% 
  select(age, college, cuentaPropia, dsi, estrato1, formal, hoursWorkUsual, informal, ingtot, microEmpresa, experiencia, ocu, sex, y_total_m_ha, y_total_m, Ingresos_laborales, age_squred, female, maxEducLevel3, maxEducLevel4, maxEducLevel5, maxEducLevel6, maxEducLevel7) 
summary(dt_final2)
stargazer(dt_final2, title="Estadísticas descriptivas", type='latex')

################################################################################
#              1.4 Gráficas
################################################################################


#Distribución de las edades

dist_edad <- ggplot(data = dt_final,
                    mapping = aes(x = age))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 9,
                 position = 'identity',
                 color="#424242", fill="#E3E3E3") +
  stat_function(fun = dnorm, xlim = c(min(dt_final$age),max(dt_final$age)), colour="#1C86EE", linewidth=1,
                args = list(mean = mean(dt_final$age), 
                            sd = sd(dt_final$age))) + 
  labs(title = 'Figura 1: Distribución de edad',
       x = 'Edad',
       y = 'Frecuencia') + 
  theme_bw()

dist_edad

#Distribución de los salarios


#Salario
dist_salario <- ggplot(data = dt_final,
                       mapping = aes(x = y_total_m_ha))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 9,
                 position = 'identity',
                 color="#424242", fill="#E3E3E3") +
  stat_function(fun = dnorm, xlim = c(min(dt_final$y_total_m_ha),max(dt_final$y_total_m_ha)),
                colour="red", linewidth=1,
                args = list(mean = mean(dt_final$y_total_m_ha), 
                            sd = sd(dt_final$y_total_m_ha))) + 
  labs(title = 'Figura 2: Distribución Salario',
       x = 'Salarios',
       y = 'Frecuencia') + 
  theme_bw()

dist_salario

#LogSalario
dist_lnsalario <- ggplot(data = dt_final,
                         mapping = aes(x = Ingresos_laborales))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 9,
                 position = 'identity',
                 color="#424242", fill="#E3E3E3") +
  stat_function(fun = dnorm, xlim = c(min(dt_final$Ingresos_laborales),max(dt_final$Ingresos_laborales)),
                colour="red", linewidth=1,
                args = list(mean = mean(dt_final$Ingresos_laborales), 
                            sd = sd(dt_final$Ingresos_laborales))) + 
  labs(title = 'Figura 3: Distribución Log Salario',
       x = 'Salarios',
       y = 'Frecuencia') + 
  theme_bw()
dist_lnsalario

#Distribución de horas semanales habituales trabajadas

dist_horastrabajadas <- ggplot(data = dt_final,
                               mapping = aes(x = hoursWorkUsual))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 9,
                 position = 'identity',
                 color="#424242", fill="#E3E3E3") +
  stat_function(fun = dnorm, xlim = c(min(dt_final$hoursWorkUsual),max(dt_final$hoursWorkUsual)),
                colour="red", linewidth=1,
                args = list(mean = mean(dt_final$hoursWorkUsual), 
                            sd = sd(dt_final$hoursWorkUsual))) + 
  labs(title = 'Figura 4:Distribución de horas semanales habituales trabajadas',
       x = 'Horas semanales habituales trabajadas',
       y = 'Frecuencia') + 
  theme_bw()

dist_horastrabajadas


