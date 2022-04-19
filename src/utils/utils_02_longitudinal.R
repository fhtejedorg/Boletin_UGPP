
fun_panel<-function(tabla, path = '../data/interim/Matriz/'){
  Matriz<-read.csv2(file.path(path, tabla), dec = ".")
  Entran_privados<-Matriz[is.na(Matriz$Rango_IBC_pre) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Dep_sec_priv"), ]
  Permanecen_privados<-Matriz[(!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Dep_sec_priv" ) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Dep_sec_priv"), ]
  salen_privados<-Matriz[((!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Dep_sec_priv")) & Matriz$Tipologia_pos != "Dep_sec_priv", ]
  
  res_entran<-data.frame(Entran_privados%>%group_by(Rango_IBC_pos)%>%summarise(Privados_entran = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_permanecen<-data.frame(Permanecen_privados%>%group_by(Rango_IBC_pos)%>%summarise(Privados_permanecen = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_salen<-data.frame(salen_privados%>%group_by(Rango_IBC_pre)%>%summarise(Privados_salen = sum(Total_relaciones_laborales, na.rm = TRUE), Privados_salen_retiro = sum(retiro_Max_pre_Sum, na.rm = TRUE)))
  matriz_salida <- merge(res_entran,res_permanecen, by = "Rango_IBC_pos", all =TRUE)
  matriz_salida <- merge(matriz_salida,res_salen, by.x = "Rango_IBC_pos", by.y = "Rango_IBC_pre", all =TRUE)
  matriz_salida$Privados_salen_NO_retiro <- matriz_salida$Privados_salen-matriz_salida$Privados_salen_retiro
  
  matriz_salida$Rango_agrupa<-rep(0,nrow(matriz_salida))
  matriz_salida[matriz_salida$Rango_IBC_pos <= 1 ,"Rango_agrupa"] <- rep(1,nrow(matriz_salida[matriz_salida$Rango_IBC_pos <= 1,]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa"] <- rep(2,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(1.5,2),]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa"] <- rep(3,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(2.5,3),]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa"] <- rep(4,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(3.5,4),]))
  matriz_salida[matriz_salida$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa"] <- rep(5,nrow(matriz_salida[matriz_salida$Rango_IBC_pos %in% c(4.5,5),]))
  matriz_salida[matriz_salida$Rango_IBC_pos > 5 ,"Rango_agrupa"] <- rep(6,nrow(matriz_salida[matriz_salida$Rango_IBC_pos> 5,]))
  
  matriz_salida<-data.frame(matriz_salida%>%group_by(Rango_agrupa)%>%summarise(Privados_entran = sum(Privados_entran,na.rm=TRUE),
                                                                               Privados_permanecen = sum(Privados_permanecen, na.rm =TRUE), Privados_salen = sum(Privados_salen, na.rm =TRUE), Privados_salen_retiro = sum(Privados_salen_retiro, na.rm =TRUE),Privados_salen_NO_retiro = sum(Privados_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_tot<-data.frame(matriz_salida%>%summarise(Privados_entran = sum(Privados_entran,na.rm=TRUE),
                                                          Privados_permanecen = sum(Privados_permanecen, na.rm =TRUE), Privados_salen = sum(Privados_salen, na.rm =TRUE), Privados_salen_retiro = sum(Privados_salen_retiro, na.rm =TRUE),Privados_salen_NO_retiro = sum(Privados_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_tot$Rango_agrupa<-"Total"
  matriz_salida<-rbind(matriz_salida,matriz_salida_tot)
  matriz_salida$Por_entran<-matriz_salida$Privados_entran/(matriz_salida$Privados_entran+matriz_salida$Privados_permanecen)
  matriz_salida$Por_salen<-matriz_salida$Privados_salen/(matriz_salida$Privados_salen +matriz_salida$Privados_permanecen)
  
  matriz_salida<-matriz_salida[order(matriz_salida$Rango_agrupa ),]
  matriz_salida$Rango_agrupa_2<-c("<=1SMMLV","1-2_SMMLV","2-3_SMMLV","3-4_SMMLV","4-5_SMMLV",">5_SMMLV", "Total")
  
  Permanecen_privados$Rango_agrupa_pre<-rep(0,nrow(Permanecen_privados))
  Permanecen_privados$Rango_agrupa_pos<-rep(0,nrow(Permanecen_privados))
  
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre <= 1 ,"Rango_agrupa_pre"] <- rep(1,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre <= 1,]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(1.5,2) ,"Rango_agrupa_pre"] <- rep(2,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(1.5,2),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(2.5,3) ,"Rango_agrupa_pre"] <- rep(3,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(2.5,3),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(3.5,4) ,"Rango_agrupa_pre"] <- rep(4,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(3.5,4),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(4.5,5) ,"Rango_agrupa_pre"] <- rep(5,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre %in% c(4.5,5),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pre > 5 ,"Rango_agrupa_pre"] <- rep(6,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pre> 5,]))
  
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos <= 1 ,"Rango_agrupa_pos"] <- rep(1,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos <= 1,]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa_pos"] <- rep(2,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(1.5,2),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa_pos"] <- rep(3,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(2.5,3),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa_pos"] <- rep(4,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(3.5,4),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa_pos"] <- rep(5,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos %in% c(4.5,5),]))
  Permanecen_privados[Permanecen_privados$Rango_IBC_pos > 5 ,"Rango_agrupa_pos"] <- rep(6,nrow(Permanecen_privados[Permanecen_privados$Rango_IBC_pos> 5,]))
  
  matriz_transision<-matrix(0,nrow = 6, ncol = 8)
  
  for(i in 1:6){
    for(j in 1:6){
      matriz_transision[i,j]<-(sum(Permanecen_privados[Permanecen_privados$Rango_agrupa_pre == i & Permanecen_privados$Rango_agrupa_pos == j, "Total_relaciones_laborales"])/sum(Permanecen_privados[Permanecen_privados$Rango_agrupa_pre == i , "Total_relaciones_laborales"]))
    }
  }
  for(i in 2:6){matriz_transision[i,7]<-sum(matriz_transision[i,1:(i-1)])}
  for(i in 1:5){matriz_transision[i,8]<-sum(matriz_transision[i,(i+1):6])}
  
  Entran_independientes<-Matriz[is.na(Matriz$Rango_IBC_pre) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Independiente"), ]
  Permanecen_independientes<-Matriz[(!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Independiente" ) & (!is.na(Matriz$Rango_IBC_pos) & Matriz$Tipologia_pos == "Independiente"), ]
  salen_independientes<-Matriz[((!is.na(Matriz$Rango_IBC_pre) & Matriz$Tipologia_pre == "Independiente")) & Matriz$Tipologia_pos != "Independiente", ]
  
  res_entran<-data.frame(Entran_independientes%>%group_by(Rango_IBC_pos)%>%summarise(independientes_entran = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_permanecen<-data.frame(Permanecen_independientes%>%group_by(Rango_IBC_pos)%>%summarise(independientes_permanecen = sum(Total_relaciones_laborales, na.rm = TRUE)))
  res_salen<-data.frame(salen_independientes%>%group_by(Rango_IBC_pre)%>%summarise(independientes_salen = sum(Total_relaciones_laborales, na.rm = TRUE), independientes_salen_retiro = sum(retiro_Max_pre_Sum, na.rm = TRUE)))
  matriz_salida_ind <- merge(res_entran,res_permanecen, by = "Rango_IBC_pos", all =TRUE)
  matriz_salida_ind <- merge(matriz_salida_ind,res_salen, by.x = "Rango_IBC_pos", by.y = "Rango_IBC_pre", all =TRUE)
  matriz_salida_ind$independientes_salen_NO_retiro <- matriz_salida_ind$independientes_salen-matriz_salida_ind$independientes_salen_retiro
  
  matriz_salida_ind$Rango_agrupa<-rep(0,nrow(matriz_salida_ind))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos <= 1 ,"Rango_agrupa"] <- rep(1,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos <= 1,]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa"] <- rep(2,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(1.5,2),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa"] <- rep(3,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(2.5,3),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa"] <- rep(4,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(3.5,4),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa"] <- rep(5,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos %in% c(4.5,5),]))
  matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos > 5 ,"Rango_agrupa"] <- rep(6,nrow(matriz_salida_ind[matriz_salida_ind$Rango_IBC_pos> 5,]))
  
  matriz_salida_ind<-data.frame(matriz_salida_ind%>%group_by(Rango_agrupa)%>%summarise(independientes_entran = sum(independientes_entran,na.rm=TRUE),
                                                                                       independientes_permanecen = sum(independientes_permanecen, na.rm =TRUE), independientes_salen = sum(independientes_salen, na.rm =TRUE), independientes_salen_retiro = sum(independientes_salen_retiro, na.rm =TRUE),independientes_salen_NO_retiro = sum(independientes_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_ind_tot<-data.frame(matriz_salida_ind%>%summarise(independientes_entran = sum(independientes_entran,na.rm=TRUE),
                                                                  independientes_permanecen = sum(independientes_permanecen, na.rm =TRUE), independientes_salen = sum(independientes_salen, na.rm =TRUE), independientes_salen_retiro = sum(independientes_salen_retiro, na.rm =TRUE),independientes_salen_NO_retiro = sum(independientes_salen_NO_retiro, na.rm =TRUE) ))
  matriz_salida_ind_tot$Rango_agrupa<-"Total"
  matriz_salida_ind<-rbind(matriz_salida_ind,matriz_salida_ind_tot)
  matriz_salida_ind$Por_entran<-matriz_salida_ind$independientes_entran/(matriz_salida_ind$independientes_entran+matriz_salida_ind$independientes_permanecen)
  matriz_salida_ind$Por_salen<-matriz_salida_ind$independientes_salen/(matriz_salida_ind$independientes_salen +matriz_salida_ind$independientes_permanecen)
  
  matriz_salida_ind<-matriz_salida_ind[order(matriz_salida_ind$Rango_agrupa ),]
  matriz_salida_ind$Rango_agrupa_2<-c("<=1SMMLV","1-2_SMMLV","2-3_SMMLV","3-4_SMMLV","4-5_SMMLV",">5_SMMLV", "Total")
  
  Permanecen_independientes$Rango_agrupa_pre<-rep(0,nrow(Permanecen_independientes))
  Permanecen_independientes$Rango_agrupa_pos<-rep(0,nrow(Permanecen_independientes))
  
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre <= 1 ,"Rango_agrupa_pre"] <- rep(1,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre <= 1,]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(1.5,2) ,"Rango_agrupa_pre"] <- rep(2,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(1.5,2),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(2.5,3) ,"Rango_agrupa_pre"] <- rep(3,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(2.5,3),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(3.5,4) ,"Rango_agrupa_pre"] <- rep(4,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(3.5,4),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(4.5,5) ,"Rango_agrupa_pre"] <- rep(5,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre %in% c(4.5,5),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre > 5 ,"Rango_agrupa_pre"] <- rep(6,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pre> 5,]))
  
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos <= 1 ,"Rango_agrupa_pos"] <- rep(1,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos <= 1,]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(1.5,2) ,"Rango_agrupa_pos"] <- rep(2,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(1.5,2),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(2.5,3) ,"Rango_agrupa_pos"] <- rep(3,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(2.5,3),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(3.5,4) ,"Rango_agrupa_pos"] <- rep(4,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(3.5,4),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(4.5,5) ,"Rango_agrupa_pos"] <- rep(5,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos %in% c(4.5,5),]))
  Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos > 5 ,"Rango_agrupa_pos"] <- rep(6,nrow(Permanecen_independientes[Permanecen_independientes$Rango_IBC_pos> 5,]))
  
  matriz_transision_ind<-matrix(0,nrow = 6, ncol = 8)
  
  for(i in 1:6){
    for(j in 1:6){
      matriz_transision_ind[i,j]<-(sum(Permanecen_independientes[Permanecen_independientes$Rango_agrupa_pre == i & Permanecen_independientes$Rango_agrupa_pos == j, "Total_relaciones_laborales"])/sum(Permanecen_independientes[Permanecen_independientes$Rango_agrupa_pre == i , "Total_relaciones_laborales"]))
    }
  }
  for(i in 2:6){matriz_transision_ind[i,7]<-sum(matriz_transision_ind[i,1:(i-1)])}
  for(i in 1:5){matriz_transision_ind[i,8]<-sum(matriz_transision_ind[i,(i+1):6])}
  
  salida_panel = list(matriz_dep = matriz_salida, transision_dep = matriz_transision, matriz_ind = matriz_salida_ind, transision_ind = matriz_transision_ind)
  salida_panel
} 



fun_output_matriz_transicion <- function(x, title){
  salida_matriz_transicion <- x %>% 
    gt(rowname_col = 'rownames_1') %>% 
    fmt_percent(columns = 1:8, scale_values = TRUE, decimals = 1)  %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("left", "right"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        columns = 7:8
      )
    ) %>%
    cols_align(
      align = "center"
    ) %>%
    cols_width(
      everything() ~ px(100)
    )
  
  salida_matriz_transicion %>%
    gtsave(title, path = '../results/02_longitudinal/', )
}


fun_output_resumen <- function(x, title){
  nombres_columnas <- colnames(x)
  nombres_columnas <- gsub('Privados_|independientes_', '', nombres_columnas)
  colnames(x) <- nombres_columnas
  tabla_matriz_resumen <- x %>% select(-Rango_agrupa) %>% 
    gt(rowname_col = 'Rango_agrupa_2') %>% 
    fmt_percent(columns = 6:7, scale_values = TRUE, decimals = 1)  %>% 
    fmt_number(columns = 1:5, sep_mark =  '.', dec_mark = ',', decimals = 0)  %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("left", "right"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        columns = 6:7
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("top"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        rows = 7
      )
    ) %>%
    tab_style(
      style = cell_text(color = "black", weight = "bold"),
      locations = cells_body(rows = Rango_agrupa_2=='Total')) %>%
    cols_align(
      align = "center"
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#228B22", style = "italic", weight = 'bold')
      ),
      locations = list(cells_body(columns = 6), cells_column_labels(columns = 6))
    ) %>%  
    tab_style(
      style = list(
        cell_text(color = "#B22222", style = "italic", weight = 'bold')
      ),
      locations = list(cells_body(columns = 7), cells_column_labels(columns = 7))
    ) %>%  
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = list(cells_column_labels(columns = 1:5))
    ) %>%  
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = cells_stub()
    ) %>% 
    cols_label(
      entran = 'Entran',
      permanecen = 'Permanecen',
      salen = 'Salen',
      salen_retiro = 'Salen con Retiro', 
      salen_NO_retiro = 'Salen sin Retiro', 
      Por_entran = 'Entran (%)',
      Por_salen = 'Salen (%)'
    ) %>%
    cols_width(
      everything() ~ px(100)
    ) 
  
  tabla_matriz_resumen %>%
    gtsave(title, path = '../results/02_longitudinal/')
}



fun_salidas_entradas_anual <- function(dinamica_serie){
  gg_entran <- ggplot(dinamica_serie, aes(x=mes_format, y= Por_entran*100, group = Año, color=Año))+ 
    geom_line(aes(linetype=Año), size = 1) + 
    geom_point(aes(shape=Año), size = 3)+
    labs(x="Meses") + 
    theme_classic() +
    scale_y_continuous(name = '%', breaks = breaks_pretty(n = 5),limits = c(4.5, 9),
                       labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) +
    scale_color_manual(values=c('#999999','#999999', '#228B22')) +
    scale_fill_manual(name = "none") + 
    scale_linetype_manual(values=c("twodash", "dashed", 'solid'))+
    theme(legend.position="bottom") + 
    facet_wrap(~Tipo,scales = 'free') 
  
  gg_salen <- ggplot(dinamica_serie, aes(x=mes_format, y= Por_salen*100, group = Año, color=Año))+ 
    geom_line(aes(linetype=Año), size = 1) + 
    geom_point(aes(shape=Año), size = 3)+
    labs(x="Meses") + 
    theme_classic() +
    scale_y_continuous(name = '%', breaks = breaks_pretty(n = 6),limits = c(4.5, 11),
                       labels = scales:::number_format(big.mark = '.', decimal.mark = ',')) +
    scale_color_manual(values=c('#999999','#999999', '#B22222')) +
    scale_fill_manual(name = "none") + 
    scale_linetype_manual(values=c("twodash", "dashed", 'solid'))+
    theme(legend.position="bottom") + 
    facet_wrap(~Tipo,scales = 'free')
  
  ggsave(filename = '../figures/02_longitudinal/entradas_anual_dependiente_independiente.png',
         width = 10, height = 6,
         plot = gg_entran)
  
  ggsave(filename = '../figures/02_longitudinal/salidas_anual_dependiente_independiente.png',
         width = 10, height = 6,
         plot = gg_salen)
}


fun_output_resumen_act_econ <- function(tabla1, tabla2, tabla3, tabla4, ano_interes, mes_interes, ano_referencia,  
                                         label_mes_interes, label_mes_referencia_1, label_mes_referencia_2, 
                                         tipologia, title){
  path_1 = '../data/raw'
  path_2 = '../data/external/'
  
  base4<-read.csv2(file.path(path_1,tabla4),dec = ".")
  base3<-read.csv2(file.path(path_1,tabla3),dec = ".")
  base2<-read.csv2(file.path(path_1,tabla2),dec = ".")
  base1<-read.csv2(file.path(path_1,tabla1),dec = ".")
  
  
  nom_seccion <- readxl::read_xlsx(file.path(path_2, 'CRUCE_DIAN_CIIU_2020.xlsx'), sheet = 'Seccion')
  nom_seccion <- nom_seccion[, c('Seccion_fn', 'Seccion_nom')]

  base <- rbind(base4, base3, base2, base1)
  base$Seccion_fn <- ifelse(base$Seccion_fn == '', 'Otras', base$Seccion_fn)

  lll <- base %>% 
    mutate_at(vars(c(Seccion_fn)), funs(as.character(.))) %>%
    bind_rows(mutate(., Seccion_fn = "Total")) %>%
    group_by(Tipologia, YEAR, MONTH, Seccion_fn)%>%
    summarise(Total_cotizantes4 = sum(Total_relaciones_laborales, na.rm =TRUE))
  
  lll$Seccion_fn <- factor(lll$Seccion_fn, levels = c(nom_seccion$Seccion_fn, 'Total'))
  lll <- lll %>% arrange(Tipologia, YEAR, MONTH, Seccion_fn)
  data.frame(lll)
  
  lll_2 <- lll %>% filter(YEAR %in% ano_referencia & MONTH %in% mes_interes & Tipologia == tipologia)
  
  # # Variación mensual 
  lll_2 <- data.frame(lll_2)
  lll_3 <- lll_2 %>% arrange(Seccion_fn, YEAR, MONTH) %>% group_by(Seccion_fn, YEAR) %>% 
    mutate(lag_total = lag(Total_cotizantes4)) %>%
    mutate(variacion_mensual = (Total_cotizantes4 - lag_total) / lag_total) %>% 
    select(-one_of(c('lag_total')))
  data.frame(lll_3)
  
  # # Variación anual
  lll_4 <- lll_2 %>% arrange(Seccion_fn, MONTH, YEAR) %>% group_by(Seccion_fn, MONTH) %>% 
    mutate(lag_total = lag(Total_cotizantes4)) %>%
    mutate(variacion_anual = (Total_cotizantes4 - lag_total) / lag_total)  %>% 
    select(-one_of(c('lag_total', 'Total_cotizantes4')))
  data.frame(lll_4)
  
  lll_5 <- merge(lll_3, lll_4)
  
  salida_temp_1 <- lll_5 %>% filter(YEAR %in% ano_interes[4], MONTH %in% mes_interes[1]) %>% select(Seccion_fn, YEAR, MONTH, Total_cotizantes4, variacion_mensual, variacion_anual)
  salida_temp_2 <- lll_5 %>% filter(YEAR %in% ano_interes[3], MONTH %in% mes_interes[2]) %>% select(Seccion_fn, YEAR, MONTH, Total_cotizantes4)
  salida_temp_3 <- lll_5 %>% filter(YEAR %in% ano_referencia[3], MONTH %in% mes_interes[1]) %>% select(Seccion_fn, YEAR, MONTH, Total_cotizantes4)
  salida_temp_A <- merge(salida_temp_1, salida_temp_2, by = c('Seccion_fn'), all.x = T)
  salida_temp_B <- merge(salida_temp_A, salida_temp_3, by = c('Seccion_fn'), all.x = T)
  salida_temp_B$distribucion <- salida_temp_B$Total_cotizantes4.x/salida_temp_B[salida_temp_B$Seccion_fn == 'Total', 'Total_cotizantes4.x']
  
  salida_base_general_publica <- merge(salida_temp_B, nom_seccion, all.x = T)
  salida_base_general_publica <- salida_base_general_publica %>% select(Seccion_fn, Seccion_nom, Total_cotizantes4, Total_cotizantes4.y, Total_cotizantes4.x, distribucion, variacion_mensual, variacion_anual)
  salida_base_general_publica$Seccion_nom <- ifelse(is.na(salida_base_general_publica$Seccion_nom), 'Total', salida_base_general_publica$Seccion_nom)
  
  salida_publica <- salida_base_general_publica %>% 
    gt() %>% 
    fmt_percent(columns = 6:8, scale_values = TRUE, decimals = 1)  %>% 
    fmt_number(columns = 3:5, sep_mark =  '.', dec_mark = ',', decimals = 0)  %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("left", "right"), 
          color = "black",
          weight = px(1)
        )
      ),
      locations = cells_body(
        columns = 6:8
      )
    ) %>% 
    tab_style(
      style = list(
        cell_borders(
          side = c("top"), 
          color = "black",
          weight = px(1)
        ), 
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_body(
        rows = 23
      )
    ) %>%   tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = list(cells_column_labels())
    ) %>% 
    cols_label(
      Seccion_fn = 'Sec.',
      Seccion_nom = 'Actividad(sección CIIU Rev.4)',
      Total_cotizantes4 = label_mes_interes,
      Total_cotizantes4.y = label_mes_referencia_1, 
      Total_cotizantes4.x = label_mes_referencia_2, 
      distribucion = 'Distribución(%)',
      variacion_mensual = 'Var.Mensual(%)', 
      variacion_anual = 'Var.Anual(%)'
    ) %>%
    cols_width(
      Seccion_nom ~ px(500)
    )
  
  salida_publica %>%
    gtsave(title, path = '../results/02_longitudinal/', )
}



fun_demografico <- function(tabla,mes,año){
  path_1 = '../data/raw'
  
  base<-read.csv2(file.path(path_1,tabla))
  base_NA<-base[!is.na(base$Edad_fin),]
  base_res_m_4_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m4 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m4 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_4_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",mes),paste0("Aporte_priv_M_",año,"_",mes))
  base_res_f_4_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f4 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f4 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_4_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",mes),paste0("Aporte_priv_f_",año,"_",mes))
  base_res_m_3_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m3 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m3 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_3_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",(mes-1)),paste0("Aporte_priv_M_",año,"_",(mes-1)))
  base_res_f_3_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f3 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f3 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_3_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",(mes-1)),paste0("Aporte_priv_f_",año,"_",(mes-1)))
  base_res_m_2_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m2 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m2 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_2_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",(mes-2)),paste0("Aporte_priv_M_",año,"_",(mes-2)))
  base_res_f_2_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f2 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f2 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_2_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",(mes-2)),paste0("Aporte_priv_f_",año,"_",(mes-2)))
  base_res_m_1_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "M", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m1 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m1 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_1_21_priv)<-c("Edad",paste0("Tot_priv_M_",año,"_",(mes-3)),paste0("Aporte_priv_M_",año,"_",(mes-3)))
  base_res_f_1_21_priv<-data.frame(base_NA%>%filter(Tipologia == "Dep_sec_priv",Sexo_fin == "F", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f1 = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f1 = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_1_21_priv)<-c("Edad",paste0("Tot_priv_f_",año,"_",(mes-3)),paste0("Aporte_priv_f_",año,"_",(mes-3)))
  
  base_res_m_4_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m4i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m4i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_4_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",mes),paste0("Aporte_ind_M_",año,"_",mes))
  base_res_f_4_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f4i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f4i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_4_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",mes),paste0("Aporte_ind_f_",año,"_",mes))
  base_res_m_3_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m3i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m3i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_3_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",(mes-1)),paste0("Aporte_ind_M_",año,"_",(mes-1)))
  base_res_f_3_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == (mes-1),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f3i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f3i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_3_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",(mes-1)),paste0("Aporte_ind_f_",año,"_",(mes-1)))
  base_res_m_2_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m2i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m2i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_2_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",(mes-2)),paste0("Aporte_ind_M_",año,"_",(mes-2)))
  base_res_f_2_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == (mes-2),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f2i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f2i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_2_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",(mes-2)),paste0("Aporte_ind_f_",año,"_",(mes-2)))
  base_res_m_1_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "M", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m1i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_m1i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_m_1_21_ind)<-c("Edad",paste0("Tot_ind_M_",año,"_",(mes-3)),paste0("Aporte_ind_M_",año,"_",(mes-3)))
  base_res_f_1_21_ind<-data.frame(base_NA%>%filter(Tipologia == "Independiente",Sexo_fin == "F", MONTH == (mes-3),YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f1i = sum(Total_relaciones_laborales, na.rm = TRUE),Pago_Medio_f1i = sum(cot_obligatoria_salud_Max_Sum ,na.rm = TRUE)/sum(cot_obligatoria_salud_Max_Count ,na.rm = TRUE)));#colnames(base_res_f_1_21_ind)<-c("Edad",paste0("Tot_ind_f_",año,"_",(mes-3)),paste0("Aporte_ind_f_",año,"_",(mes-3)))
  
  base_res_m_4_21_tot<-data.frame(base_NA%>%filter(Sexo_fin == "M", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_m4tot = sum(Total_relaciones_laborales, na.rm = TRUE)));#colnames(base_res_m_4_21_tot)<-c("Edad",paste0("Tot_tot_M_",año,"_",mes),paste0("Aporte_tot_M_",año,"_",mes))
  base_res_f_4_21_tot<-data.frame(base_NA%>%filter(Sexo_fin == "F", MONTH == mes,YEAR  == año )%>%group_by(Edad_fin)%>%summarise(Total_cot_f4tot = sum(Total_relaciones_laborales, na.rm = TRUE)));#colnames(base_res_f_4_21_tot)<-c("Edad",paste0("Tot_tot_f_",año,"_",mes),paste0("Aporte_tot_f_",año,"_",mes))
  
  tabla_demo_tot<-merge(base_res_f_4_21_tot,base_res_m_4_21_tot,by = "Edad_fin", all = TRUE)
  
  tabla_demo<-merge(base_res_m_4_21_priv,base_res_m_3_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_2_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_1_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_4_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_3_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_2_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_1_21_priv, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_4_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_3_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_2_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_m_1_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_4_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_3_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_2_21_ind, by = "Edad_fin", all = TRUE)
  tabla_demo<-merge(tabla_demo,base_res_f_1_21_ind, by = "Edad_fin", all = TRUE)
  
  tabla_demo$Edad_cat<-rep(0,nrow(tabla_demo))
  tabla_demo[tabla_demo$Edad_fin>=0 & tabla_demo$Edad_fin<=4, "Edad_cat"]<-rep(1,nrow(tabla_demo[tabla_demo$Edad_fin>=0 & tabla_demo$Edad_fin<=4, ]))
  tabla_demo[tabla_demo$Edad_fin>=5 & tabla_demo$Edad_fin<=9, "Edad_cat"]<-rep(2,nrow(tabla_demo[tabla_demo$Edad_fin>=5 & tabla_demo$Edad_fin<=9, ]))
  tabla_demo[tabla_demo$Edad_fin>=10 & tabla_demo$Edad_fin<=14, "Edad_cat"]<-rep(3,nrow(tabla_demo[tabla_demo$Edad_fin>=10 & tabla_demo$Edad_fin<=14, ]))
  tabla_demo[tabla_demo$Edad_fin>=15 & tabla_demo$Edad_fin<=19, "Edad_cat"]<-rep(4,nrow(tabla_demo[tabla_demo$Edad_fin>=15 & tabla_demo$Edad_fin<=19, ]))
  tabla_demo[tabla_demo$Edad_fin>=20 & tabla_demo$Edad_fin<=24, "Edad_cat"]<-rep(5,nrow(tabla_demo[tabla_demo$Edad_fin>=20 & tabla_demo$Edad_fin<=24, ]))
  tabla_demo[tabla_demo$Edad_fin>=25 & tabla_demo$Edad_fin<=29, "Edad_cat"]<-rep(6,nrow(tabla_demo[tabla_demo$Edad_fin>=25 & tabla_demo$Edad_fin<=29, ]))
  tabla_demo[tabla_demo$Edad_fin>=30 & tabla_demo$Edad_fin<=34, "Edad_cat"]<-rep(7,nrow(tabla_demo[tabla_demo$Edad_fin>=30 & tabla_demo$Edad_fin<=34, ]))
  tabla_demo[tabla_demo$Edad_fin>=35 & tabla_demo$Edad_fin<=39, "Edad_cat"]<-rep(8,nrow(tabla_demo[tabla_demo$Edad_fin>=35 & tabla_demo$Edad_fin<=39, ]))
  tabla_demo[tabla_demo$Edad_fin>=40 & tabla_demo$Edad_fin<=44, "Edad_cat"]<-rep(9,nrow(tabla_demo[tabla_demo$Edad_fin>=40 & tabla_demo$Edad_fin<=44, ]))
  tabla_demo[tabla_demo$Edad_fin>=45 & tabla_demo$Edad_fin<=49, "Edad_cat"]<-rep(10,nrow(tabla_demo[tabla_demo$Edad_fin>=45 & tabla_demo$Edad_fin<=49, ]))
  tabla_demo[tabla_demo$Edad_fin>=50 & tabla_demo$Edad_fin<=54, "Edad_cat"]<-rep(11,nrow(tabla_demo[tabla_demo$Edad_fin>=50 & tabla_demo$Edad_fin<=54, ]))
  tabla_demo[tabla_demo$Edad_fin>=55 & tabla_demo$Edad_fin<=59, "Edad_cat"]<-rep(12,nrow(tabla_demo[tabla_demo$Edad_fin>=55 & tabla_demo$Edad_fin<=59, ]))
  tabla_demo[tabla_demo$Edad_fin>=60 & tabla_demo$Edad_fin<=64, "Edad_cat"]<-rep(13,nrow(tabla_demo[tabla_demo$Edad_fin>=60 & tabla_demo$Edad_fin<=64, ]))
  tabla_demo[tabla_demo$Edad_fin>=65 & tabla_demo$Edad_fin<=69, "Edad_cat"]<-rep(14,nrow(tabla_demo[tabla_demo$Edad_fin>=65 & tabla_demo$Edad_fin<=69, ]))
  tabla_demo[tabla_demo$Edad_fin>=70 & tabla_demo$Edad_fin<=74, "Edad_cat"]<-rep(15,nrow(tabla_demo[tabla_demo$Edad_fin>=70 & tabla_demo$Edad_fin<=74, ]))
  tabla_demo[tabla_demo$Edad_fin>=75 & tabla_demo$Edad_fin<=79, "Edad_cat"]<-rep(16,nrow(tabla_demo[tabla_demo$Edad_fin>=75 & tabla_demo$Edad_fin<=79, ]))
  tabla_demo[tabla_demo$Edad_fin>=80, "Edad_cat"]<-rep(17,nrow(tabla_demo[tabla_demo$Edad_fin>=80, ]))
  
  tabla_demo2<-data.frame(tabla_demo%>%group_by(Edad_cat)%>%summarise(
    Total_cot_m4 = sum(Total_cot_m4,na.rm = TRUE),
    Total_cot_f4 = sum(Total_cot_f4,na.rm = TRUE),
    Total_cot_m3 = sum(Total_cot_m3,na.rm = TRUE),
    Total_cot_f3 = sum(Total_cot_f3,na.rm = TRUE),
    Total_cot_m2 = sum(Total_cot_m2,na.rm = TRUE),
    Total_cot_f2 = sum(Total_cot_f2,na.rm = TRUE),
    Total_cot_m1 = sum(Total_cot_m1,na.rm = TRUE),
    Total_cot_f1 = sum(Total_cot_f1,na.rm = TRUE),
    Total_cot_m4i = sum(Total_cot_m4i,na.rm = TRUE),
    Total_cot_f4i = sum(Total_cot_f4i,na.rm = TRUE),
    Total_cot_m3i = sum(Total_cot_m3i,na.rm = TRUE),
    Total_cot_f3i = sum(Total_cot_f3i,na.rm = TRUE),
    Total_cot_m2i = sum(Total_cot_m2i,na.rm = TRUE),
    Total_cot_f2i = sum(Total_cot_f2i,na.rm = TRUE),
    Total_cot_m1i = sum(Total_cot_m1i,na.rm = TRUE),
    Total_cot_f1i = sum(Total_cot_f1i,na.rm = TRUE),
    Pago_Medio_m4 = sum(Pago_Medio_m4,na.rm = TRUE),
    Pago_Medio_f4 = sum(Pago_Medio_f4,na.rm = TRUE),
    Pago_Medio_m3 = sum(Pago_Medio_m3,na.rm = TRUE),
    Pago_Medio_f3 = sum(Pago_Medio_f3,na.rm = TRUE),
    Pago_Medio_m2 = sum(Pago_Medio_m2,na.rm = TRUE),
    Pago_Medio_f2 = sum(Pago_Medio_f2,na.rm = TRUE),
    Pago_Medio_m1 = sum(Pago_Medio_m1,na.rm = TRUE),
    Pago_Medio_f1 = sum(Pago_Medio_f1,na.rm = TRUE),
    Pago_Medio_m4i = sum(Pago_Medio_m4i,na.rm = TRUE),
    Pago_Medio_f4i = sum(Pago_Medio_f4i,na.rm = TRUE),
    Pago_Medio_m3i = sum(Pago_Medio_m3i,na.rm = TRUE),
    Pago_Medio_f3i = sum(Pago_Medio_f3i,na.rm = TRUE),
    Pago_Medio_m2i = sum(Pago_Medio_m2i,na.rm = TRUE),
    Pago_Medio_f2i = sum(Pago_Medio_f2i,na.rm = TRUE),
    Pago_Medio_m1i = sum(Pago_Medio_m1i,na.rm = TRUE),
    Pago_Medio_f1i = sum(Pago_Medio_f1i,na.rm = TRUE)))
  
  tabla_demo2_tot<-data.frame(tabla_demo%>%summarise(
    Total_cot_m4 = sum(Total_cot_m4,na.rm = TRUE),
    Total_cot_f4 = sum(Total_cot_f4,na.rm = TRUE),
    Total_cot_m3 = sum(Total_cot_m3,na.rm = TRUE),
    Total_cot_f3 = sum(Total_cot_f3,na.rm = TRUE),
    Total_cot_m2 = sum(Total_cot_m2,na.rm = TRUE),
    Total_cot_f2 = sum(Total_cot_f2,na.rm = TRUE),
    Total_cot_m1 = sum(Total_cot_m1,na.rm = TRUE),
    Total_cot_f1 = sum(Total_cot_f1,na.rm = TRUE),
    Total_cot_m4i = sum(Total_cot_m4i,na.rm = TRUE),
    Total_cot_f4i = sum(Total_cot_f4i,na.rm = TRUE),
    Total_cot_m3i = sum(Total_cot_m3i,na.rm = TRUE),
    Total_cot_f3i = sum(Total_cot_f3i,na.rm = TRUE),
    Total_cot_m2i = sum(Total_cot_m2i,na.rm = TRUE),
    Total_cot_f2i = sum(Total_cot_f2i,na.rm = TRUE),
    Total_cot_m1i = sum(Total_cot_m1i,na.rm = TRUE),
    Total_cot_f1i = sum(Total_cot_f1i,na.rm = TRUE),
    Pago_Medio_m4 = sum(Pago_Medio_m4,na.rm = TRUE),
    Pago_Medio_f4 = sum(Pago_Medio_f4,na.rm = TRUE),
    Pago_Medio_m3 = sum(Pago_Medio_m3,na.rm = TRUE),
    Pago_Medio_f3 = sum(Pago_Medio_f3,na.rm = TRUE),
    Pago_Medio_m2 = sum(Pago_Medio_m2,na.rm = TRUE),
    Pago_Medio_f2 = sum(Pago_Medio_f2,na.rm = TRUE),
    Pago_Medio_m1 = sum(Pago_Medio_m1,na.rm = TRUE),
    Pago_Medio_f1 = sum(Pago_Medio_f1,na.rm = TRUE),
    Pago_Medio_m4i = sum(Pago_Medio_m4i,na.rm = TRUE),
    Pago_Medio_f4i = sum(Pago_Medio_f4i,na.rm = TRUE),
    Pago_Medio_m3i = sum(Pago_Medio_m3i,na.rm = TRUE),
    Pago_Medio_f3i = sum(Pago_Medio_f3i,na.rm = TRUE),
    Pago_Medio_m2i = sum(Pago_Medio_m2i,na.rm = TRUE),
    Pago_Medio_f2i = sum(Pago_Medio_f2i,na.rm = TRUE),
    Pago_Medio_m1i = sum(Pago_Medio_m1i,na.rm = TRUE),
    Pago_Medio_f1i = sum(Pago_Medio_f1i,na.rm = TRUE)))
  
  tabla_demo2_tot$Edad_cat<-"Total"
  tabla_demo2<-rbind(tabla_demo2,tabla_demo2_tot)
  tabla_demo2$edad_rango<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+","Total")
  colnames(tabla_demo)<-c("Edad",paste0(colnames(tabla_demo[2:ncol(tabla_demo)]),"_",mes,"_",año))
  colnames(tabla_demo2)<-c("Edad",paste0(colnames(tabla_demo2[2:ncol(tabla_demo2)]),"_",mes,"_",año))
  salida = list(tabla_demo = tabla_demo, tabla_demo_quiquenales = tabla_demo2, tabla_demo_tot = tabla_demo_tot)
  salida
}


fun_salida_demografico <- function(tabla1, tabla2, tabla3, mes, ano, label_meses){
  
  salida_interes_21 <- fun_demografico(tabla1, mes, ano)
  salida_interes_20 <- fun_demografico(tabla2, mes, ano-1)
  salida_interes_19 <- fun_demografico(tabla3, mes, ano-2)
  
  salida_demo_general <- merge(salida_interes_19$tabla_demo,salida_interes_20$tabla_demo,by  = "Edad", all = TRUE)
  salida_demo_general <- merge(salida_demo_general,salida_interes_21$tabla_demo,by  = "Edad", all = TRUE)
  salida_demo_general$rango_interes <- rep(0,nrow(salida_demo_general))
  
  salida_demo_general[salida_demo_general$Edad>=18 & salida_demo_general$Edad<=28,"rango_interes"] <- 
    rep(1,nrow(salida_demo_general[salida_demo_general$Edad>=18 & salida_demo_general$Edad<=28,]))
  
  tabla_demo_quiquenales <- merge(salida_interes_19$tabla_demo_quiquenales,salida_interes_20$tabla_demo_quiquenales,by  = "Edad", all = TRUE)
  tabla_demo_quiquenales <- merge(tabla_demo_quiquenales,salida_interes_21$tabla_demo_quiquenales,by  = "Edad", all = TRUE)
  
  Total_hombres<-c(sum(salida_demo_general[,8], na.rm = TRUE),sum(salida_demo_general[,6], na.rm = TRUE),sum(salida_demo_general[,4], na.rm = TRUE),sum(salida_demo_general[,2], na.rm = TRUE),
                   sum(salida_demo_general[,41], na.rm = TRUE),sum(salida_demo_general[,39], na.rm = TRUE),sum(salida_demo_general[,37], na.rm = TRUE),sum(salida_demo_general[,35], na.rm = TRUE),
                   sum(salida_demo_general[,74], na.rm = TRUE),sum(salida_demo_general[,72], na.rm = TRUE),sum(salida_demo_general[,70], na.rm = TRUE),sum(salida_demo_general[,68], na.rm = TRUE),
                   sum(salida_demo_general[,24], na.rm = TRUE),sum(salida_demo_general[,22], na.rm = TRUE),sum(salida_demo_general[,20], na.rm = TRUE),sum(salida_demo_general[,18], na.rm = TRUE),
                   sum(salida_demo_general[,57], na.rm = TRUE),sum(salida_demo_general[,55], na.rm = TRUE),sum(salida_demo_general[,53], na.rm = TRUE),sum(salida_demo_general[,51], na.rm = TRUE),
                   sum(salida_demo_general[,90], na.rm = TRUE),sum(salida_demo_general[,88], na.rm = TRUE),sum(salida_demo_general[,86], na.rm = TRUE),sum(salida_demo_general[,84], na.rm = TRUE))
  Total_mujeres<-c(sum(salida_demo_general[,16], na.rm = TRUE),sum(salida_demo_general[,14], na.rm = TRUE),sum(salida_demo_general[,12], na.rm = TRUE),sum(salida_demo_general[,10], na.rm = TRUE),
                   sum(salida_demo_general[,49], na.rm = TRUE),sum(salida_demo_general[,47], na.rm = TRUE),sum(salida_demo_general[,45], na.rm = TRUE),sum(salida_demo_general[,43], na.rm = TRUE),
                   sum(salida_demo_general[,82], na.rm = TRUE),sum(salida_demo_general[,80], na.rm = TRUE),sum(salida_demo_general[,78], na.rm = TRUE),sum(salida_demo_general[,76], na.rm = TRUE),
                   sum(salida_demo_general[,32], na.rm = TRUE),sum(salida_demo_general[,30], na.rm = TRUE),sum(salida_demo_general[,28], na.rm = TRUE),sum(salida_demo_general[,26], na.rm = TRUE),
                   sum(salida_demo_general[,65], na.rm = TRUE),sum(salida_demo_general[,63], na.rm = TRUE),sum(salida_demo_general[,61], na.rm = TRUE),sum(salida_demo_general[,59], na.rm = TRUE),
                   sum(salida_demo_general[,98], na.rm = TRUE),sum(salida_demo_general[,96], na.rm = TRUE),sum(salida_demo_general[,94], na.rm = TRUE),sum(salida_demo_general[,92], na.rm = TRUE))
  Total_jovenes_M<-c(sum(salida_demo_general[salida_demo_general$rango_interes == 1,8], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,6], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,4], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,2], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,41], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,39], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,37], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,35], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,74], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,72], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,70], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,68], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,24], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,22], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,20], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,18], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,57], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,55], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,53], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,51], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,90], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,88], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,86], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,84], na.rm = TRUE))
  Total_jovenes_F<-c(sum(salida_demo_general[salida_demo_general$rango_interes == 1,16], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,14], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,12], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,10], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,49], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,47], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,45], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,43], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,82], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,80], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,78], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,76], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,32], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,30], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,28], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,26], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,65], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,63], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,61], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,59], na.rm = TRUE),
                     sum(salida_demo_general[salida_demo_general$rango_interes == 1,98], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,96], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,94], na.rm = TRUE),sum(salida_demo_general[salida_demo_general$rango_interes == 1,92], na.rm = TRUE))
  Total_jovenes<-Total_jovenes_M+Total_jovenes_F
  Por_jovenes<-(Total_jovenes_M+Total_jovenes_F)/(Total_hombres+Total_mujeres)
  
  salida_sexo<-data.frame(año =rep(rep(c(2019,2020,2021),c(4,4,4)),2),mes = rep(label_meses,6),Tipologia = rep(c("Dep.Privado","Independiente"),c(12,12)),
                          Hombres = formatC(Total_hombres,format = "f",digits = 0,big.mark = ".",decimal.mark = ","), 
                          Mujeres = formatC(Total_mujeres,format = "f",digits = 0,big.mark = ".",decimal.mark = ","), 
                          Razon_sexo = round(Total_mujeres/Total_hombres*100,2),
                          Por_hombres = paste0(round(Total_hombres/(Total_hombres+Total_mujeres)*100,2),"%"),
                          Por_mujeres = paste0(round(Total_mujeres/(Total_hombres+Total_mujeres)*100,2),"%"), 
                          Jovenes = formatC(Total_jovenes,format = "f",digits = 0,big.mark = ".",decimal.mark = ","),
                          por_jovenes = paste0(round(Por_jovenes*100,2),"%"))
  
  
  
  tabla_demo_quiquenales_g<-tabla_demo_quiquenales[tabla_demo_quiquenales$Edad %in% c("4","5","6","7","8","9","10","11","12","13","14","15","16"),]
  datos_estrutura<-data.frame(Rango_edad = rep(tabla_demo_quiquenales_g$Edad,8), Sexo = rep(c("M","F"),c(4*nrow(tabla_demo_quiquenales_g),4*nrow(tabla_demo_quiquenales_g))),
                              #cambiar por los meses de referecia                  
                              Total_cotizantes_dep = c(tabla_demo_quiquenales_g$Total_cot_m4_12_2019,
                                                       tabla_demo_quiquenales_g$Total_cot_m4_12_2020,
                                                       tabla_demo_quiquenales_g$Total_cot_m3_12_2021,
                                                       tabla_demo_quiquenales_g$Total_cot_m4_12_2021,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f4_12_2019,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f4_12_2020,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f3_12_2021,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f4_12_2021),
                              Total_cotizantes_ind = c(tabla_demo_quiquenales_g$Total_cot_m4i_12_2019,
                                                       tabla_demo_quiquenales_g$Total_cot_m4i_12_2020,
                                                       tabla_demo_quiquenales_g$Total_cot_m3i_12_2021,
                                                       tabla_demo_quiquenales_g$Total_cot_m4i_12_2021,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f4i_12_2019,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f4i_12_2020,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f3i_12_2021,
                                                       -1*tabla_demo_quiquenales_g$Total_cot_f4i_12_2021),
                              periodo = c(rep("M_12_2019",nrow(tabla_demo_quiquenales_g)),rep("M_12_2020",nrow(tabla_demo_quiquenales_g)),
                                          rep("M_11_2021",nrow(tabla_demo_quiquenales_g)),rep("M_12_2021",nrow(tabla_demo_quiquenales_g)),
                                          rep("F_12_2019",nrow(tabla_demo_quiquenales_g)),rep("F_12_2020",nrow(tabla_demo_quiquenales_g)),
                                          rep("F_11_2021",nrow(tabla_demo_quiquenales_g)),rep("F_12_2021",nrow(tabla_demo_quiquenales_g))))
  valores_nombre<-data.frame(Rango_edad = c("4","5","6","7","8","9","10","11","12","13","14","15","16"),Rangos = c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-75","76-79"))
  datos_estrutura<-merge(datos_estrutura,valores_nombre, by = "Rango_edad", all = TRUE)
  datos_estrutura$Rango_edad<-as.numeric(datos_estrutura$Rango_edad)
  salida_estructura <- datos_estrutura[order(datos_estrutura$periodo,datos_estrutura$Rango_edad),]
  
  return(list(salida_sexo = salida_sexo, salida_estructura = salida_estructura))
}

fun_output_sexo_resumen <- function(x, title){
  x$Tipologia <- factor(x$Tipologia, levels = c("Dep.Privado", "Independiente"))
  salida_final_sexo <- x %>% 
    gt(groupname_col = 'Tipologia') %>% 
    tab_style(
      style = list(
        cell_text(weight = 'bold')
      ),
      locations = list(cells_column_labels())
    ) %>% 
    tab_style(
      style = list(
          cell_borders(
            side = c("bottom"), 
            color = "black",
            weight = px(1)
          ),
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_body(
        rows = seq(4, 24, by = 4)
      )
    ) %>%   
    cols_label(
      año = 'Año',
      mes = 'Mes',
      Razon_sexo = 'Razon sexo',
      Por_hombres = 'Hombres(%)', 
      Por_mujeres = 'Mujeres(%)',
      por_jovenes = 'Jovenes(%)'
    ) %>%
    cols_width(
      c(año) ~ px(150)
    ) 
salida_final_sexo %>%
    gtsave(title, path = '../results/02_longitudinal/', )
}

fun_grafica_piramides <- function(datos_estrutura){
  outout_piramide_dependientes <- ggplot(data = datos_estrutura, 
                                         aes(x = Rangos, y = Total_cotizantes_dep, fill = periodo, width = 5))+
    geom_bar(stat = "identity",position = position_dodge(0.6), alpha = 0.6)+
    coord_flip()+theme_minimal()+
    labs(title = "Total cotizantes por grupos de edad",
         subtitle = "Dependientes de sector privado", 
         x = "Rangos de edad", y = "Total cotizantes") + 
    ylim(-max(abs(datos_estrutura$Total_cotizantes_dep))-5000,max(abs(datos_estrutura$Total_cotizantes_dep))+5000)+
    scale_fill_manual(values = c("orchid1","orchid1","orchid1","orchid4","slateblue1","slateblue1","slateblue1","slateblue4"))
  
  ggsave(filename = '../figures/02_longitudinal/salidas_piramide_dependientes.png',
         width = 10, height = 6,
         plot = outout_piramide_dependientes)
  
  outout_piramide_independientes <- ggplot(data = datos_estrutura, aes(x = Rangos, y = Total_cotizantes_ind, fill = periodo, width = 5))+
    geom_bar(stat = "identity",position = position_dodge(0.6), alpha = 0.6)+
    coord_flip()+theme_minimal()+
    labs(title = "Total cotizantes por grupos de edad",subtitle = "Independientes",x = "Rangos de edad", y = "Total cotizantes")+
    ylim(-max(abs(datos_estrutura$Total_cotizantes_ind))-500,max(abs(datos_estrutura$Total_cotizantes_ind))+500)+
    scale_fill_manual(values = c("orchid1","orchid1","orchid1","orchid4","slateblue1","slateblue1","slateblue1","slateblue4"))
  
  ggsave(filename = '../figures/02_longitudinal/salidas_piramide_independientes.png',
         width = 10, height = 6,
         plot = outout_piramide_independientes)
}
