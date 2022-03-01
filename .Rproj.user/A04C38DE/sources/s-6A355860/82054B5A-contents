# Librerias
library(xlsx)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(janitor)
library(lubridate)
# Carpeta de referencia
#source(file = file.path('../src/utils/utils_capitulo2.R'))

# Datos de entrada
# # Lectura del master de salidas
dat_master_meses <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Meses')
dat_master_rangoIBC <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Rango IBC')
dat_master_periodos <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'periodos_datos')
dat_master_anomes <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Ano_Mes')

## Definición de variables generales 
mes_interes <- 6
mes_referencia <- mes_interes
ano_interes <- 2021
ano_referencia <- 2020

meses_variacion <- 5
tabla3 <- fun_tabla3(mes_interes, mes_referencia, 
           ano_interes, ano_referencia,
           dat_master_anomes, dat_master_rangoIBC, meses_variacion)

