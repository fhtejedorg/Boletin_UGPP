fun_crea_nombre_datos <- function(dat_master_periodos, ano_interes, mes_interes, tipo){
  dat_periodo_interes <- dat_master_periodos %>% filter(Ano == ano_interes)
  for(ii in 1:nrow(dat_periodo_interes)){
    mes_inicial <- dat_periodo_interes[ii, 'Mes_inicial']
    mes_final <- dat_periodo_interes[ii, 'Mes_final']
    if(mes_inicial <= mes_interes & mes_interes <= mes_final){
      referencia_raw_dat <- paste(mes_inicial, '_', mes_final, sep = '')
    }
  }
  nombre_datos_referencia <- paste('Salidas_generales_', tipo, referencia_raw_dat, '_', ano_interes, '.csv', sep = '')
  return(nombre_datos_referencia)
}


fun_tabla3 <- function(mes_interes, mes_referencia, 
                       ano_interes, ano_referencia,
                       dat_master_anomes, dat_master_rangoIBC, meses_variacion = 5){
  tipo <- 'rangoIBC'
  mes_variaciones <- mes_interes - meses_variacion
  mes_interes_mes5 <- ifelse(mes_variaciones < 0, mes_variaciones%%12, mes_variaciones)
  mes_referencia_mes5 <- mes_interes_mes5
  ano_interes_mes5 <- ifelse(mes_variaciones < 0, ano_interes - 1, ano_interes)
  ano_referencia_mes5 <- ifelse(mes_variaciones < 0, ano_referencia - 1, ano_referencia)
  
  limites_rangoIBC <- c(dat_master_rangoIBC$Limite_inferior, 100)
  nombres_rangoIBC <- dat_master_rangoIBC$Nombre
  
  nombre_datos_interes <- fun_crea_nombre_datos(dat_master_periodos, ano_interes, mes_interes, tipo)
  nombre_datos_referencia <- fun_crea_nombre_datos(dat_master_periodos, ano_referencia, mes_interes, tipo)
  nombre_datos_interes_mes5 <- fun_crea_nombre_datos(dat_master_periodos, ano_interes_mes5, mes_interes_mes5, tipo)
  nombre_datos_referencia_mes5 <- fun_crea_nombre_datos(dat_master_periodos, ano_referencia_mes5, mes_referencia_mes5, tipo)
  
  dat_interes <- fread(file = file.path('../data/raw/', nombre_datos_interes))
  dat_referencia <- fread(file = file.path('../data/raw/', nombre_datos_referencia))
  
  dat_interes_mes5 <- fread(file = file.path('../data/raw/', nombre_datos_interes_mes5))
  dat_referencia_mes5 <- fread(file = file.path('../data/raw/', nombre_datos_referencia_mes5))
  
  if(nombre_datos_interes != nombre_datos_interes_mes5){
    dat_consolidado <- rbind(dat_referencia_mes5, 
                             dat_referencia, 
                             dat_interes, 
                             dat_interes_mes5)
  }else{
    stop('Error: Revisar datos coomparativos por mes')
  }
  dat_consolidado$tipologia_salida <- with(dat_consolidado, 
                                           ifelse(Tipologia == 'Independiente', 'Independiente', 'Dependiente'))
  
  dat_consolidado$rangoIBC_salida <- with(dat_consolidado, 
                                          cut(Rango_IBC, breaks = limites_rangoIBC, labels = nombres_rangoIBC))
  dat_consolidado$ano_mes <- with(dat_consolidado, paste(YEAR,MONTH,'1', sep = '/'))
  dat_consolidado$ano_mes <- as.Date(dat_consolidado$ano_mes)
  
  dat_consolidado <- dat_consolidado %>% arrange(ano_mes)
  salida_tabla3_total <- dat_consolidado %>%
    filter(MONTH %in% mes_interes & YEAR %in% c(ano_referencia, ano_interes)) %>%
    mutate_at(vars(c(rangoIBC_salida)), funs(as.character(.))) %>%
    bind_rows(mutate(., rangoIBC_salida = "Total")) %>%
    group_by(tipologia_salida, YEAR, rangoIBC_salida) %>%
    summarise(total_cotizantes = sum(Total_relaciones_laborales))
  salida_tabla3_total <- salida_tabla3_total %>% spread(YEAR, total_cotizantes)
  salida_tabla3_total$rangoIBC_salida <- factor(salida_tabla3_total$rangoIBC_salida, 
                                                levels = c(nombres_rangoIBC, 'Total'))
  salida_tabla3_total <- salida_tabla3_total %>% 
    arrange(tipologia_salida, rangoIBC_salida)
  colnames(salida_tabla3_total)[c(3,4)] <- c('referencia','interes')
  
  salida_tabla3_total <- salida_tabla3_total %>%
    mutate(diferencia = interes - referencia, 
           variacion = interes/referencia - 1)
  
  nombres_salida_total <- dat_master_anomes %>% filter(Codigo == mes_interes & Ano %in% c(ano_interes, ano_referencia))
  setnames(salida_tabla3_total, old = c('referencia','interes', 'diferencia', 'variacion'), 
           new = c(nombres_salida_total$Nombre, 'dif.', 'porc.'))
  # Variaciones en meses
  
  ano_mes_interes <- paste(ano_interes, mes_interes, '1', sep ='/')
  ano_mes_interes <- as.Date(ano_mes_interes)
  ano_mes_interes_mes5 <- paste(ano_interes_mes5, mes_interes_mes5, '1', sep ='/')
  ano_mes_interes_mes5 <- as.Date(ano_mes_interes_mes5)
  sequencia_interes <- seq(ano_mes_interes_mes5, ano_mes_interes, by = 'month')
  
  ano_mes_referencia <- paste(ano_referencia, mes_referencia, '1', sep ='/')
  ano_mes_referencia <- as.Date(ano_mes_referencia)
  ano_mes_referencia_mes5 <- paste(ano_referencia_mes5, mes_referencia_mes5, '1', sep ='/')
  ano_mes_referencia_mes5 <- as.Date(ano_mes_referencia_mes5)
  sequencia_referencia <- seq(ano_mes_referencia_mes5, ano_mes_referencia, by = 'month')
  
  salida_tabla3_interes_var <- dat_consolidado %>%
    filter(ano_mes %in% sequencia_interes) %>%
    group_by(tipologia_salida, rangoIBC_salida, ano_mes) %>%
    summarise(total_cotizantes = sum(Total_relaciones_laborales))
  salida_tabla3_interes_var <- salida_tabla3_interes_var %>%
    mutate(lag_total = lag(total_cotizantes)) %>%
    mutate(variacion = (total_cotizantes - lag_total) / lag_total) 
  salida_tabla3_interes_var$mes <- month(salida_tabla3_interes_var$ano_mes)
  salida_tabla3_interes_var$ano <- year(salida_tabla3_interes_var$ano_mes)
  
  salida_tabla3_interes_var <- merge(salida_tabla3_interes_var, 
                                     dat_master_anomes[, c('Codigo', 'Ano','Nombre')],
                                     by.x = c('mes', 'ano'),
                                     by.y = c('Codigo', 'Ano'))
  salida_tabla3_interes_var$Nombre <- factor(salida_tabla3_interes_var$Nombre, dat_master_anomes$Nombre)
  salida_tabla3_interes_var <- salida_tabla3_interes_var %>%
    filter(ano_mes != ano_mes_interes_mes5) %>%
    select(-one_of(c('mes', 'ano', 'ano_mes', 'total_cotizantes', 'lag_total'))) %>%
    arrange(tipologia_salida, Nombre, rangoIBC_salida) %>%
    spread(Nombre, variacion)
  
  
  salida_tabla3_referencia_var <- dat_consolidado %>%
    filter(ano_mes %in% sequencia_referencia) %>%
    group_by(tipologia_salida, rangoIBC_salida, ano_mes) %>%
    summarise(total_cotizantes = sum(Total_relaciones_laborales))
  salida_tabla3_referencia_var <- salida_tabla3_referencia_var %>%
    mutate(lag_total = lag(total_cotizantes)) %>%
    mutate(variacion = (total_cotizantes - lag_total) / lag_total) 
  salida_tabla3_referencia_var$mes <- month(salida_tabla3_referencia_var$ano_mes)
  salida_tabla3_referencia_var$ano <- year(salida_tabla3_referencia_var$ano_mes)
  
  salida_tabla3_referencia_var <- merge(salida_tabla3_referencia_var, 
                                        dat_master_anomes[, c('Codigo', 'Ano','Nombre')],
                                        by.x = c('mes', 'ano'),
                                        by.y = c('Codigo', 'Ano'))
  salida_tabla3_referencia_var$Nombre <- factor(salida_tabla3_referencia_var$Nombre, dat_master_anomes$Nombre)
  salida_tabla3_referencia_var <- salida_tabla3_referencia_var %>%
    filter(ano_mes != ano_mes_referencia_mes5) %>%
    select(-one_of(c('mes', 'ano', 'ano_mes', 'total_cotizantes', 'lag_total'))) %>%
    arrange(tipologia_salida, Nombre, rangoIBC_salida) %>%
    spread(Nombre, variacion)
  
  colnames(salida_tabla3_total)[c(1:2)] <- c('Tipologia', 'RangoIBC')
  colnames(salida_tabla3_interes_var)[c(1:2)] <- c('Tipologia', 'RangoIBC')
  colnames(salida_tabla3_referencia_var)[c(1:2)] <- c('Tipologia', 'RangoIBC')
  
  return(list(total = salida_tabla3_total, 
              variaciones_interes = salida_tabla3_interes_var,
              variaciones_referencia = salida_tabla3_referencia_var))
}



fun_tabla4 <- function(mes_interes, mes_referencia, 
                       ano_interes, ano_referencia,
                       ano_referencia_2,
                       dat_master_anomes){
  string_all <- 'Salidas_generales_rangoIBC'
  list_csv_files <- dir(path = '../data/raw/')
  list_datos <- list_csv_files[grep(string_all, list_csv_files)]
  dat_consolidado_all <- NULL
  for(ii in 1:length(list_datos)){
    datos_lectura <- fread(file = file.path('../data/raw/', list_datos[ii]))
    dat_consolidado_all <- rbind(dat_consolidado_all, datos_lectura)
  } 
  
  dat_consolidado_all$ano_mes <- with(dat_consolidado_all, paste(YEAR,MONTH,'1', sep = '/'))
  dat_consolidado_all$ano_mes <- as.Date(dat_consolidado_all$ano_mes)
  
  salida_tabla4A_total <- dat_consolidado_all %>%
    filter(MONTH %in% c(mes_interes, mes_referencia) & 
             YEAR %in% c(ano_interes, ano_referencia, ano_referencia_2)) %>%
    group_by(MONTH, YEAR) %>%
    summarise(Ingreso = sum(ingreso_Max_Sum),
              Retiro = sum(retiro_Max_Sum),
              Suspension = sum(suspension_temporal_Max_Sum),
              Incapacidades = sum(incapacidas_por_trabajo_Max_Sum),
              Vacaciones = sum(vacaciones_Max_Sum))
  
  salida_tabla4A_total <- melt(setDT(salida_tabla4A_total), id.vars = c('MONTH', 'YEAR'), 
                               variable.name = 'Novedad', value.name = 'Total')
  salida_tabla4A_total <- merge(salida_tabla4A_total, 
                                dat_master_anomes[, c('Codigo', 'Ano','Nombre')],
                                by.x = c('MONTH', 'YEAR'),
                                by.y = c('Codigo', 'Ano'))
  salida_tabla4A_total <- salida_tabla4A_total %>% arrange(YEAR,Novedad, MONTH)
  salida_tabla4A_total <- salida_tabla4A_total %>% 
    mutate(lag_total_mes = lag(Total)) %>%
    mutate(variacion_mes = (Total - lag_total_mes) / lag_total_mes)
  
  salida_tabla4A_total <- salida_tabla4A_total %>% arrange(MONTH, Novedad, YEAR) %>% 
    mutate(lag_total_year = lag(Total)) %>%
    mutate(variacion_ano = (Total - lag_total_year) / lag_total_year)  
  
  salida_tabla4A_total$Nombre <- factor(salida_tabla4A_total$Nombre, dat_master_anomes$Nombre)
  tabla4_1 <- salida_tabla4A_total %>% select(Novedad, Total, Nombre) %>% 
    spread(Nombre, Total)
  tabla4_2 <- salida_tabla4A_total %>% select(Novedad, variacion_mes , Nombre) %>% 
    spread(Nombre, variacion_mes )
  tabla4_3 <- salida_tabla4A_total %>% 
    filter(MONTH == 6) %>% 
    select(Novedad, variacion_ano , Nombre) %>% 
    spread(Nombre, variacion_ano )
  
  return(salida_tabla4A_total)
  
}
