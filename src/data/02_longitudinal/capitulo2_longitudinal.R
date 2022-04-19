# Librerias
library(xlsx)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(gt)
library(scales)
library(readr)
library(paletteer)
library(sbpiper)
library(kableExtra)
library(wesanderson)
options(ztable.type="html")
# Carpeta de referencia
setwd('D:/CONSULTORIAS/UGPP/2022/Proyectos/Boletin_UGPP/report/')
source(file = file.path('../src/utils/utils_02_longitudinal.R'), encoding = 'utf8')

###### Lectura de datos 

### Matrices
matriz_12_21<-fun_panel("Matriz_12_21.csv")
matriz_11_21<-fun_panel("Matriz_11_21.csv")
matriz_10_21<-fun_panel("Matriz_10_21.csv")
matriz_9_21<-fun_panel("Matriz_9_21.csv")
matriz_8_21<-fun_panel("Matriz_8_21.csv")
matriz_7_21<-fun_panel("Matriz_7_21.csv")

matriz_12_20<-fun_panel("Matriz_12_20.csv")
matriz_11_20<-fun_panel("Matriz_11_20.csv")
matriz_10_20<-fun_panel("Matriz_10_20.csv")
matriz_9_20<-fun_panel("Matriz_9_20.csv")
matriz_8_20<-fun_panel("Matriz_8_20.csv")
matriz_7_20<-fun_panel("Matriz_7_20.csv")

matriz_12_19<-fun_panel("Matriz_12_19.csv")
matriz_11_19<-fun_panel("Matriz_11_19.csv")
matriz_10_19<-fun_panel("Matriz_10_19.csv")
matriz_9_19<-fun_panel("Matriz_9_19.csv")
matriz_8_19<-fun_panel("Matriz_8_19.csv")
matriz_7_19<-fun_panel("Matriz_7_19.csv")

matriz_12_18<-fun_panel("Matriz_12_18.csv")

matriz_names_21 <- c('matriz_12_21', 'matriz_11_21', 
                     'matriz_10_21', 'matriz_9_21', 
                     'matriz_8_21', 'matriz_7_21')

vect_meses_21 <- seq.Date(as.Date('2021-12-01'), as.Date('2021-07-01'), by = "-1 month")
vect_meses_20 <- seq.Date(as.Date('2020-12-01'), as.Date('2020-07-01'), by = "-1 month")
vect_meses_19 <- seq.Date(as.Date('2019-12-01'), as.Date('2019-07-01'), by = "-1 month")

matriz_names_20 <- c('matriz_12_20', 'matriz_11_20', 
                     'matriz_10_20', 'matriz_9_20', 
                     'matriz_8_20', 'matriz_7_20')

matriz_names_19 <- c('matriz_12_19', 'matriz_11_19', 
                     'matriz_10_19', 'matriz_9_19', 
                     'matriz_8_19', 'matriz_7_19')
meses_names <- c('Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')
matriz_dependientes_19_21 <- NULL
matriz_independientes_19_21 <- NULL
for(ii in 1:6){
  sub_21 <- get(matriz_names_21[ii])
  sub_20 <- get(matriz_names_20[ii])
  sub_19 <- get(matriz_names_19[ii])
  
  matriz_dependientes_21 <- sub_21$matriz_dep
  matriz_dependientes_21$mes <- vect_meses_21[ii]
  
  matriz_dependientes_20 <- sub_20$matriz_dep
  matriz_dependientes_20$mes <- vect_meses_20[ii]
  
  matriz_dependientes_19 <- sub_19$matriz_dep
  matriz_dependientes_19$mes <- vect_meses_19[ii]
  
  matriz_dependientes_19_21 <- rbind(matriz_dependientes_19_21, matriz_dependientes_21, matriz_dependientes_20, matriz_dependientes_19)
  
  
  matriz_independientes_21 <- sub_21$matriz_ind
  matriz_independientes_21$mes <- vect_meses_21[ii]
  
  matriz_independientes_20 <- sub_20$matriz_ind
  matriz_independientes_20$mes <- vect_meses_20[ii]
  
  matriz_independientes_19 <- sub_19$matriz_ind
  matriz_independientes_19$mes <- vect_meses_19[ii]
  
  matriz_independientes_19_21 <- rbind(matriz_independientes_19_21, matriz_independientes_21, matriz_independientes_20, matriz_independientes_19)
}

## SECCION - Matrices resumen (DEPENDIENTES)

fun_output_resumen(matriz_12_21$matriz_dep, title = "salida_resumen_dependientes_interes_21.png")
fun_output_resumen(matriz_11_21$matriz_dep, title = "salida_resumen_dependientes_referencia_21.png")

fun_output_resumen(matriz_12_20$matriz_ind, title = "salida_resumen_dependientes_interes_20.png")
fun_output_resumen(matriz_12_19$matriz_ind, title = "salida_resumen_dependientes_interes_19.png")


## SECCION - Relaciones laborales que permanecieron (DEPENDIENTES)

column_names_IBC <- c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV")
column_names_complemento <- c("Incrementan","Disminuyen")

tabla_dependientes_21 <- data.frame(matriz_12_21$transision_dep)
colnames(tabla_dependientes_21)<-c(column_names_IBC, column_names_complemento)
tabla_dependientes_21$rownames_1 <- column_names_IBC

tabla_dependientes_20 <- data.frame(matriz_12_20$transision_dep)
colnames(tabla_dependientes_20)<-c(column_names_IBC, column_names_complemento)
tabla_dependientes_20$rownames_1 <- column_names_IBC

tabla_dependientes_19 <- data.frame(matriz_12_19$transision_dep)
colnames(tabla_dependientes_19)<-c(column_names_IBC, column_names_complemento)
tabla_dependientes_19$rownames_1 <- column_names_IBC

fun_output_matriz_transicion(tabla_dependientes_21, "salida_matriz_transicion_dependientes_21.png")
fun_output_matriz_transicion(tabla_dependientes_20, "salida_matriz_transicion_dependientes_20.png")
fun_output_matriz_transicion(tabla_dependientes_19, "salida_matriz_transicion_dependientes_19.png")

####### Salidas por actividad económica (DEPENDIENTES)
date_interes <- seq(as.Date("2021-12-01"), length = 4, by = "-1 month")
ano_interes <- year(date_interes)
mes_interes <- month(date_interes)       
ano_referencia <- 2018:2021
label_mes_interes <- 'Dic-21'
label_mes_referencia_1 <- 'Nov-21'
label_mes_referencia_2 <- 'Nov-21'

tabla_actividad_econ_4 <- "Salidas_generales_Aeconomica9_12_2021.csv"
tabla_actividad_econ_3 <- "Salidas_generales_Aeconomica9_12_2020.csv"
tabla_actividad_econ_2 <- "Salidas_generales_Aeconomica9_12_2019.csv"
tabla_actividad_econ_1 <- "Salidas_generales_Aeconomica9_12_2018.csv"

fun_output_resumen_act_econ(tabla_actividad_econ_1, tabla_actividad_econ_2, tabla_actividad_econ_3, tabla_actividad_econ_4, 
                            ano_interes, mes_interes, ano_referencia, 
                            label_mes_interes, label_mes_referencia_1, label_mes_referencia_2, 
                            tipologia = 'Dep_sec_priv', title = 'salida_act_econ_dependientes_21.png')


## SECCION - Matrices resumen (INDEPENDIENTES)

fun_output_resumen(matriz_12_21$matriz_ind, title = "salida_resumen_independientes_interes_21.png")
fun_output_resumen(matriz_11_21$matriz_ind, title = "salida_resumen_independientes_referencia_21.png")

fun_output_resumen(matriz_12_20$matriz_ind, title = "salida_resumen_independientes_interes_20.png")
fun_output_resumen(matriz_12_19$matriz_ind, title = "salida_resumen_independientes_interes_19.png")


## SECCION - Relaciones laborales que permanecieron (INDEPENDIENTES)

column_names_IBC <- c("<=1SMMLV","1-2SMMLV","2-3SMMLV","3-4SMMLV","4-5SMMLV",">5SMMLV")
column_names_complemento <- c("Incrementan","Disminuyen")

tabla_independientes_21 <- data.frame(matriz_12_21$transision_ind)
colnames(tabla_independientes_21)<-c(column_names_IBC, column_names_complemento)
tabla_independientes_21$rownames_1 <- column_names_IBC

tabla_independientes_20 <- data.frame(matriz_12_20$transision_ind)
colnames(tabla_independientes_20)<-c(column_names_IBC, column_names_complemento)
tabla_independientes_20$rownames_1 <- column_names_IBC

tabla_independientes_19 <- data.frame(matriz_12_19$transision_ind)
colnames(tabla_independientes_19)<-c(column_names_IBC, column_names_complemento)
tabla_independientes_19$rownames_1 <- column_names_IBC

fun_output_matriz_transicion(tabla_independientes_21, "salida_matriz_transicion_independientes_21.png")
fun_output_matriz_transicion(tabla_independientes_20, "salida_matriz_transicion_independientes_20.png")
fun_output_matriz_transicion(tabla_independientes_19, "salida_matriz_transicion_independientes_19.png")

####### Salidas por actividad económica (INDEPENDIENTES)
date_interes <- seq(as.Date("2021-12-01"), length = 4, by = "-1 month")
ano_interes <- year(date_interes)
mes_interes <- month(date_interes)       
ano_referencia <- 2018:2021
label_mes_interes <- 'Dic-21'
label_mes_referencia_1 <- 'Nov-21'
label_mes_referencia_2 <- 'Nov-21'

tabla_actividad_econ_4 <- "Salidas_generales_Aeconomica9_12_2021.csv"
tabla_actividad_econ_3 <- "Salidas_generales_Aeconomica9_12_2020.csv"
tabla_actividad_econ_2 <- "Salidas_generales_Aeconomica9_12_2019.csv"
tabla_actividad_econ_1 <- "Salidas_generales_Aeconomica9_12_2018.csv"

fun_output_resumen_act_econ(tabla_actividad_econ_1, tabla_actividad_econ_2, tabla_actividad_econ_3, tabla_actividad_econ_4, 
                            ano_interes, mes_interes, ano_referencia, 
                            label_mes_interes, label_mes_referencia_1, label_mes_referencia_2, 
                            tipologia = 'Independiente' , title = 'salida_act_econ_independientes_21.png')


### Comparativo (INDEPENDIENTES Y DEPENDIENTES) (ENTRADAS y SALLIDAS gráfico lineas)

dinamica_serie_1 <- matriz_dependientes_19_21 %>% filter(Rango_agrupa_2 == 'Total') %>% select(Por_entran, Por_salen, mes) 
dinamica_serie_1$Tipo <- 'Dependientes'
dinamica_serie_2 <- matriz_independientes_19_21 %>% filter(Rango_agrupa_2 == 'Total') %>% select(Por_entran, Por_salen, mes) 
dinamica_serie_2$Tipo <- 'Independientes'

dinamica_serie <- rbind(dinamica_serie_1, dinamica_serie_2)

dinamica_serie <- rbind(dinamica_serie)
dinamica_serie$mes_format <- factor(month(dinamica_serie$mes), labels = meses_names)
dinamica_serie$Año <- factor(year(dinamica_serie$mes))

fun_salidas_entradas_anual(dinamica_serie)

#### Salidas demográfico (INDEPENDIENTES Y DEPENDIENTES)

tabla_demografico_1 <- "Salidas_generales_edad_sexo9_12_2021.csv"
tabla_demografico_2 <- "Salidas_generales_edad_sexo9_12_2020.csv"
tabla_demografico_3 <- "Salidas_generales_edad_sexo9_12_2019.csv"
mes_interes <- 12
ano_interes <- 2021
label_meses <- c("Sep","Oct","Nov","Dic")
resultados_demografico <- fun_salida_demografico(tabla_demografico_1, 
                                                 tabla_demografico_2, 
                                                 tabla_demografico_3, 
                                                 mes_interes, ano_interes, label_meses)
fun_output_sexo_resumen(resultados_demografico$salida_sexo, title = 'salida_resumen_demog_dependientes_independientes_21.png')
datos_estrutura <- resultados_demografico$salida_estructura
fun_grafica_piramides(resultados_demografico$salida_estructura)



