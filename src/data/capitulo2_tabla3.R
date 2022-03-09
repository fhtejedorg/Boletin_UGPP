# Librerias
library(xlsx)
library(dplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(janitor)
library(lubridate)
library(janitor)
library(sjmisc)
library(gt)
library(magrittr)
library(gt)
library(scales)
library(readr)
library(paletteer)
library(sbpiper)
library(wesanderson)
options(ztable.type="html")
# Carpeta de referencia
setwd('D:/CONSULTORIAS/UGPP/2022/Proyectos/Boletin_UGPP/report/')
source(file = file.path('../src/utils/utils_capitulo2.R'))

# Datos de entrada
# # Lectura del master de salidas
dat_master_meses <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Meses')
dat_master_rangoIBC <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Rango IBC')
dat_master_periodos <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'periodos_datos')
dat_master_anomes <- read.xlsx('../data/external/master_salidas.xlsx', sheetName = 'Ano_Mes')

## Definición de variables generales 
mes_interes <- 12
mes_referencia <- mes_interes
ano_interes <- 2021
ano_referencia <- 2020
meses_variacion <- 5
tabla3 <- fun_tabla3(mes_interes, mes_referencia, 
           ano_interes, ano_referencia,
           dat_master_anomes, dat_master_rangoIBC, meses_variacion)

tabla3$total <- tabla3$total %>% big_mark(big.mark ='.')

salida_table3_total<- tabla3$total %>% 
  gt(groupname_col = 'Tipologia', rowname_col = 'RangoIBC') %>% 
  data_color(
    columns = 6, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material", direction = -1) %>% as.character(),
      domain = NULL)) %>% 
  fmt_percent(columns = 6, scale_values = TRUE, decimals = 1)  %>% 
  fmt_number(columns = 3:5, sep_mark =  '.', dec_mark = ',', decimals = 0)  %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>%
  cols_label(
    Tipologia = 'Tipología',
    RangoIBC = 'Rango IBC',
    dif = 'Diferencia',
    porc = '%'
  )

salida_table3_total %>%
  gtsave("salida_table3_total.png", path = '../results/Resumen/')

tabla3_variaciones <- merge(tabla3$variaciones_referencia, tabla3$variaciones_interes)
tabla3_variaciones <- tabla3_variaciones %>% arrange(Tipologia, RangoIBC)

salida_table3_variaciones <- tabla3_variaciones %>% 
  gt(groupname_col = 'Tipologia', rowname_col = 'RangoIBC') %>% 
  data_color(
    columns = 3:12, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::red_material", direction = -1) %>% as.character(),
      domain = NULL)) %>%
  fmt_percent(columns = 3:12, scale_values = TRUE, decimals = 1)  %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>%
  cols_label(
    Tipologia = 'Tipología',
    RangoIBC = 'Rango IBC'
  )

salida_table3_variaciones %>%
  gtsave("salida_table3_variaciones.png", path = '../results/Resumen/')






library(tidyverse)
library(gt)
url_in <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv'
raw_yields <- readr::read_csv(url_in)
yield_data <- raw_yields %>% 
  janitor::clean_names() %>% 
  rename_with(~str_remove(., "_tonnes_per_hectare")) %>% 
  select(entity:beans, -code) %>% 
  pivot_longer(cols = wheat:beans, names_to = "crop", values_to = "yield") %>% 
  rename(Country = entity)
yield_data


country_sel <- c(
  "Germany", "Brazil", "Ireland", "Lebanon", "Italy", 
  "Netherlands", "France", "Denmark", "El Salvador", "Denmark"
)
rule10_data <- yield_data %>% 
  filter(
    year %in% c(2013,2017), 
    crop == "potatoes", 
    Country %in% country_sel
  ) %>% 
  pivot_wider(names_from = year, values_from = yield)
rule10_data


rule10_wide <- yield_data %>% 
  filter(
    year %in% c(2013:2017), 
    crop == "potatoes", 
    Country %in% c(
       "Germany", "Brazil", "Ireland", "Lebanon", "Italy", 
      "Netherlands", "France", "Denmark", "El Salvador", "Denmark"
    )
  ) %>% 
  pivot_wider(names_from = year, values_from = yield) %>% 
  arrange(desc(`2013`)) %>% 
  select(-crop)

small_yield <- yield_data %>% 
  filter(
    year %in% c(2013:2017), 
    crop == "potatoes", 
    Country %in% country_sel
  ) 
split_yield <- split(small_yield$yield, small_yield$Country)

rule10_spark <- rule10_data %>% 
  mutate(spark = map(split_yield, kableExtra::spec_plot),
         spark = map(spark, "svg_text"),
         spark = map(spark, ~html(as.character(.x)))) %>% 
  select(-crop) %>% 
  gt() %>% 
  cols_label(
    spark = "2013-2017"
  ) %>% 
  fmt_number(2:3) %>% 
  tab_spanner(
    label = md("Potato Yield in<br>Tonnes/Hectare"),
    columns = c(2,3)
  ) %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )
  ) %>%  
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
  )


tab_1 <-
  gtcars %>%
  dplyr::select(model, year, hp, trq) %>%
  dplyr::slice(1:5) %>%
  gt(rowname_col = "model") %>%
  tab_stubhead(label = "car")
  basic_theme()
  
  
tab_1 %>%
    gtsave("tab_1.tex", path = '../results/')

             