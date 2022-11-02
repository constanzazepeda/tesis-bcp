


# Funciones ---------------------------------------------------------------


# delitos por zona censal
# del_x_zc_prev<- function(input, freq, tipo = c("cantidad", "densidad")){
#   n_del<- input %>% 
#     mutate(date = ymd(fecha), 
#            date_seq = floor_date(date, unit = freq, week_start = 1)) %>% 
#     group_by(gcod, abrev_tipo, date_seq) %>% 
#     summarise(n_del = n(), .groups = "keep") %>% 
#     ungroup()
#   
#   if(tipo == "cantidad"){
#     output<- n_del %>%  
#       pivot_wider(names_from = abrev_tipo, values_from = n_del)
#     
#   } else if (tipo == "densidad"){
#     
#     area<- input %>% 
#       select(gcod, area_m) %>% 
#       distinct()
#     
#     output<- n_del %>%  
#       left_join(area, by = "gcod") %>% 
#       mutate(dens_del = n_del*1000000/area_m) %>%
#       pivot_wider(names_from = abrev_tipo, values_from = dens_del)
#     
#   }
#   
#   return(output)
# }


# delitos por zona censal
del_x_zc<- function(input, freq, tipo = c("cantidad", "densidad")){
  
  input<- input %>%  
    filter(!is.na(gcod))
  
  area<- input %>% 
    dplyr::select(gcod, area_m) %>% 
    distinct()
  
  n_deli_tot<- input %>% 
    mutate(date = ymd(fecha), 
           date_seq = floor_date(date, unit = freq, week_start = 1)) %>% 
    #group_by(gcod, date_seq) %>% 
    #summarise(n_del = n(), .groups = "keep") %>% 
    group_by(gcod, date_seq, title) %>% 
    summarise(cant_del = n(), .groups = "keep") %>% 
    ungroup() %>%  
    pivot_wider(names_from = title, values_from = cant_del) %>%  
    dplyr::rename(cant_del = delito, 
           cant_inc = incivilidad#, 
           # cant_dah = drogas_armas_hom
           ) %>% 
    left_join(area, by = "gcod") %>% 
    mutate(dens_del = cant_del*10000/area_m,
           dens_inc = cant_inc*10000/area_m#, 
          # dens_dah = cant_dah*10000/area_m
          ) %>%  
    dplyr::select(-area_m)
  # n_deli<- n_deli_tipo %>%  
  #   left_join(n_deli_tot, by = c("gcod", "date_seq"))
  
  n_deli_tipo<- input %>% 
  #cant_deli<- input %>% 
    mutate(date = ymd(fecha), 
           date_seq = floor_date(date, unit = freq, week_start = 1)) %>% 
    group_by(gcod, abrev_tipo, date_seq) %>% 
    summarise(n_del_tipo = n(), .groups = "keep") %>% 
    ungroup()
  
  cant_deli<- n_deli_tipo %>%  
    mutate(abrev_tipo = paste0("cant_", abrev_tipo)) %>% 
    #mutate(cant_deli_tipo) %>% 
    pivot_wider(names_from = abrev_tipo, values_from = n_del_tipo) #%>% 
    #left_join(area, by = "gcod") #%>% 
    #mutate(dens_del_tipo = n_del_tipo*10000/area_m) %>% 
    #mutate(abrev_tipo = paste0("dens_", abrev_tipo))
  
  dens_deli<- n_deli_tipo %>%  
    left_join(area, by = "gcod") %>% 
    mutate(dens_del_tipo = n_del_tipo*10000/area_m) %>% 
    dplyr::select(-n_del_tipo) %>% 
    mutate(abrev_tipo = paste0("dens_", abrev_tipo)) %>% 
    pivot_wider(names_from = abrev_tipo, values_from = dens_del_tipo) 
    
  # dens_deli<- n_deli_tipo %>%  
  #   left_join(area, by = "gcod") %>% 
  #   mutate(# dens_del = cant_del*10000/area_m,
  #          # dens_inc = cant_inc*10000/area_m,
  #          dens_del_tipo = n_del_tipo*10000/area_m) %>%
  #   mutate(abrev_tipo = paste0("dens_", abrev_tipo)) %>%
  #   ungroup() %>% 
  #   pivot_wider(names_from = abrev_tipo, values_from = dens_del_tipo) #%>%  
  #   #dplyr::distinct()#%>%  
  #   #dplyr::select(-c(n_del_tipo, cant_del, cant_inc))
  #   
  output<- cant_deli %>%  
    left_join(dens_deli, by = c("gcod", "date_seq")) %>%  
    left_join(n_deli_tot, by = c("gcod", "date_seq"))
    
  return(output)
}

cuarentena_floor<- function(data = cuarentena_urb, 
                            cuarentena_col = cuarentena,
                            fecha_col = fecha,
                            freq = freq){
  
  if (freq == "week"){
    d = 4
  } else if (freq == "month") {
    d = 15
  } else {
    print("Expresion no valida para cuarentenas")
  }
  
  output<- data %>%  
    mutate(date_seq = floor_date({{fecha_col}}, unit = freq, week_start = 1)) %>%  
    group_by(gcod, date_seq) %>% 
    summarise(n_dias_cuarentena = sum({{cuarentena_col}}), .groups = "keep") %>% 
    mutate(cuarentena = ifelse(n_dias_cuarentena >= d, 1 ,0))
  
  return(output)
}

# hace panel de zonas censales
# make_panel_zc
make_panel_zc<- function(input , fecha_inicio, fecha_fin, freq){
  dates <- seq(ymd(fecha_inicio), ymd(fecha_fin), "day")
  date_seq<- floor_date(dates, unit = freq, week_start = 1) %>%
    unique()
  
  new_cols<- colnames(input %>% dplyr::select(-gcod))
  
  output<- input %>% 
    expand_grid(date_seq) %>%  
    group_by(gcod, date_seq) %>% 
    summarise(across(all_of(new_cols), ~sum(.x)), .groups = "keep") %>% 
    ungroup()
  
  return(output)
}

# make_panel_barrio
make_panel_barrio<- function(input, id_col , fecha_inicio, fecha_fin, freq){
  dates <- seq(ymd(fecha_inicio), ymd(fecha_fin), "day")
  date_seq<- floor_date(dates, unit = freq, week_start = 1) %>%
    unique()
  
  #new_cols<- colnames(input %>% select(-{{ id_col }}))
  
  output<- input %>% 
    expand_grid(date_seq) # %>%  
  # group_by(gcod, date_seq) %>% 
  # summarise(across(all_of(new_cols), ~sum(.x)), .groups = "keep") %>% 
  # ungroup()
  
  return(output)
}

make_panel_cuarent<- function(input = cuarentena_urb, 
                              panel = panel_zc, 
                              cuarentena_col = cuarentena,
                              fecha_col = fecha,
                              fecha_inicio = fecha_inicio, 
                              fecha_fin = fecha_fin, 
                              freq = freq){
  
  data_floor<- cuarentena_floor(data = input, 
                                cuarentena_col = cuarentena,
                                fecha_col = fecha,
                                freq = freq) %>%
    distinct()
  
  data_window<- data_floor %>%  
    filter(date_seq >= fecha_inicio & date_seq <= fecha_fin)
  
  resultado<- bind2panels(panel = panel_zc, 
                          data = data_window,  
                          i = "gcod", 
                          t = "date_seq")
  
  return(resultado)
  
}







make_panel_trat<- function(input = barrios_df, 
                           panel_base = panel_zc, 
                           #id_col = gcod,
                           fecha_inicio = fecha_inicio, 
                           fecha_fin = fecha_fin, 
                           freq = freq){
  
  
  # panel de zonas censales # make_panel_zc
  panel_barrios<- make_panel_barrio(input = input, 
                                    #id_col = gcod,
                                    fecha_inicio = fecha_inicio, 
                                    fecha_fin = fecha_fin, 
                                    freq = freq)
  
  panel_trat<- panel_barrios %>% 
    mutate(trat = ifelse(date_seq>=fecha_trat, 1,0))
  
  result<- bind2panels(data = panel_trat, panel = panel_base, i = "gcod", t = "date_seq" )
  
  return(result)
  
}

make_panel_del<- function(input = del_df, 
                          panel = panel_zc, 
                          #fecha = fecha,
                          #fecha_inicio = fecha_inicio, 
                          #fecha_fin = fecha_fin, 
                          freq = freq){
  
  # densidad de delitos por zona censal
  del_zc<- del_x_zc(input = input,  freq = freq, tipo = "densidad")
  
  
  resultado<- bind2panels(panel = panel, data = del_zc,  i = "gcod", t = "date_seq")
  
  
  return(resultado)
  
}


make_panel<- function(zonas_df      = zonas_df, 
                      barrios_df    = barrios_df, 
                      del_df        = del_df, 
                      cuarentena_df = cuarentena_df,
                      fecha_inicio  = fecha_inicio, 
                      fecha_fin     = fecha_fin, 
                      freq          = freq){
  
  # 1. Panel base zonas censales 
  panel_zc <<- make_panel_zc(input = zonas_df, 
                           #id_col = gcod, 
                           fecha_inicio = fecha_inicio, 
                           fecha_fin = fecha_fin, 
                           freq = freq)
  
  # 2. Panel tratamiento
  panel_tratamiento<- make_panel_trat(input = barrios_df, 
                                      panel = panel_zc,  
                                      fecha_inicio = fecha_inicio, 
                                      fecha_fin = fecha_fin, 
                                      freq = freq)
  
  # 3. Panel delitos 
  panel_delitos<- make_panel_del(input = del_df, 
                                 panel = panel_zc, 
                                 freq = freq)
  
  # 4. Panel cuarentenas
  panel_cuarent<- make_panel_cuarent(input = cuarentena_df, 
                                     panel = panel_zc, 
                                     cuarentena_col = cuarentena, # por defecto
                                     fecha_col = fecha, # por defecto
                                     fecha_inicio = fecha_inicio, 
                                     fecha_fin = fecha_fin, 
                                     freq = freq)
  
  # 4. Unir paneles 
  panel<- panel_zc %>%  
    left_join(panel_tratamiento , 
              by = c("gcod", "date_seq")) %>%  
    left_join(panel_delitos, 
              by = c("gcod", "date_seq")) %>% 
    left_join(panel_cuarent, 
              by = c("gcod", "date_seq"))
  
  return(panel)
  
}


# Une panel de data con zona censal
bind2panels<- function(panel, data,  i = "gcod", t = "date_seq"){
  
  c<- !colnames(data)%in%c(i,t)
  d1<-data[,c]
  d2<- d1[,colnames(data[,c])%in%colnames(panel)]
  
  data_opt<- data %>% 
    dplyr::select(-c(colnames(d2)))
  
  new_cols<- colnames(data[,!colnames(data)%in%colnames(panel)])
  #new_cols<- colnames(data_opt) ?revisar?
  output<- panel %>% 
    left_join(data_opt, by = c(i, t)) %>% 
    mutate(across(.cols = all_of(new_cols), 
                  .fns  = ~ifelse(is.na(.x)==TRUE, 0,.))) %>% 
    group_by(gcod, date_seq) %>% 
    summarise(across(all_of(new_cols), ~sum(.x)), .groups = "keep") %>% 
    ungroup()
  
  
  return(output)
  
}


adjust_floor_dates<-function(data, fecha_inicio = fecha_inicio, fecha_fin = fecha_fin, freq = freq ){
  fechas_data<- data$fecha
  output<- data %>% 
    mutate(fecha = floor_date(fechas_data, unit = freq)) %>%  
    filter(fecha >= fecha_inicio & fecha <= fecha_fin)
  
  return(output)
}



kdensity<- function(points, base, par = 100){
  
  delitos_utm<- points %>%  
    as("Spatial")
  
  # Definir bounding box ampliado
  x_min <- delitos_utm@bbox[1] - 100
  x_max <- delitos_utm@bbox[3] + 100
  y_min <- delitos_utm@bbox[2] - 100
  y_max <- delitos_utm@bbox[4] + 100
  
  # Crear ventana de trabajo y coordenadas para kernel
  w <- as.owin(c(x_min,x_max, y_min, y_max))
  #px <- pixellate(w, eps=0.5)
  # This can be converted to raster as desired
  #rLength <- raster(px)
  pts <- coordinates(delitos_utm)
  p <- ppp(pts[,1], pts[,2], window = w)
  
  # Calculo de kernel
  kde_del <- density(p, sigma = par) #ajustar parámetro adj
  
  # generar mascara
  maskCom <- base %>%
    st_union() %>%
    st_transform(32719) %>%
    as("Spatial")
  
  # Recortar
  kde_del <- mask(raster(kde_del), maskCom)
  kde_del <- kde_del  * 10000 # transformar a delitos/m
  proj4string(kde_del) <- proj4string(delitos_utm) # Asignar CRS
  
  #writeRaster(kde_del, filename = "otros/kde_del_100.tif")
  
  test_spdf <- as(kde_del, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  
  return(test_df)
}



combine_geom_by_feature<- function(data, feature){
  
  united_geom<- data %>% 
    group_by({{feature}}) %>% 
    dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep")
  
  cols_data<- colnames(data)
  cols_base<- c(colnames(united_geom), "area_m")
  new_cols <- cols_data[!cols_data %in% cols_base]
  
  attributes<- data %>%  
    st_drop_geometry() %>% 
    group_by({{feature}}) %>% 
    summarise(area_m = sum(area_m),
              across(all_of(new_cols), ~mean(.x)), .groups = "keep") %>% 
    ungroup()
  
  output<- attributes %>%  
    left_join(united_geom, by = names(dplyr::select(., {{feature}})) )
  
  return(output)
  
}


ind_logro_prop<- function(data = del_df, 
                          col_cat = c("abrev_tipo", "title"), 
                          t1_inicio = t1_inicio, 
                          t1_fin = t1_fin, 
                          t2_inicio = t2_inicio, 
                          t2_fin = t2_fin){
  
  output<- data %>%  
    dplyr::select(-area_m) %>% 
    mutate(periodo = case_when(fecha >= ymd(t1_inicio) & fecha <= ymd(t1_fin) ~ "t1",
                               fecha >= ymd(t2_inicio) & fecha <= ymd(t2_fin) ~ "t2")) %>%  
    filter(periodo %in% c("t1","t2")) %>%  
    left_join(barrios_df, by  = "gcod") %>% 
    filter(!is.na(barrio_id)) %>%  
    group_by(barrio_id, periodo, {{col_cat}}) %>%  
    summarise(cant_del = n(), .groups = "keep") %>%  
    pivot_wider(values_from  = cant_del, names_from = periodo) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(dif = t1 - t2, 
           dism = ifelse(dif > 0 , 1, 0 )) %>% 
    group_by({{col_cat}}) %>%  
    summarise(ind_prop = sum(dism / 12))
  
  return(output)
  
}



cant_deli<- function(data = del_df, 
                     col_cat = c("abrev_tipo", "title"), 
                     t1_inicio = t1_inicio, 
                     t1_fin = t1_fin, 
                     t2_inicio = t2_inicio, 
                     t2_fin = t2_fin){
  
  output<- data %>%  
    dplyr::select(-area_m) %>% 
    mutate(periodo = case_when(fecha >= ymd(t1_inicio) & fecha <= ymd(t1_fin) ~ "t1",
                               fecha >= ymd(t2_inicio) & fecha <= ymd(t2_fin) ~ "t2",
                               fecha >= ymd(t3_inicio) & fecha <= ymd(t3_fin) ~ "t3",
                               fecha >= ymd(t4_inicio) & fecha <= ymd(t4_fin) ~ "t4")) %>%  
    filter(periodo %in% c("t1","t2", "t3", "t4")) %>%  
    left_join(barrios_df, by  = "gcod") %>% 
    filter(!is.na(barrio_id)) %>%  
    group_by(periodo, {{col_cat}}) %>%  
    summarise(cant_del = n(), .groups = "keep") %>%  
    pivot_wider(values_from  = cant_del, names_from = periodo) 
  
  return(output)
  
}



