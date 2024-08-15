# Data frames de mortalidad y poblacion
{
  edad_grouping = 25:90 #rango de edades y hasta donde dejamos la ultima edad
  
  # Hombres 1992
  {
    defs_h_baja <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(1991, 1992, 1993), tramo_esc == "baja") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_h_media <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(1991, 1992, 1993), tramo_esc == "media") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_h_alta <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(1991, 1992, 1993), tramo_esc == "alta") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    smot_h_92 <- create_joined_dataframe(defs_h_baja, defs_h_media, defs_h_alta, df_hombre_92, edad_grouping, "tab_h_92")
  }
  
  # Mujeres 1992
  {
    defs_m_baja <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(1991, 1992, 1993), tramo_esc == "baja") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_m_media <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(1991, 1992, 1993), tramo_esc == "media") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_m_alta <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(1991, 1992, 1993), tramo_esc == "alta") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    smot_m_92 <- create_joined_dataframe(defs_m_baja, defs_m_media, defs_m_alta, df_mujer_92, edad_grouping, "tab_m_92")
  }
  
  # Hombres 2002
  {
    defs_h_baja <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(2001, 2002, 2003), tramo_esc == "baja") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_h_media <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(2001, 2002, 2003), tramo_esc == "media") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_h_alta <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(2001, 2002, 2003), tramo_esc == "alta") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    smot_h_02 <- create_joined_dataframe(defs_h_baja, defs_h_media, defs_h_alta, df_hombre_02, edad_grouping, "tab_h_02")
  }
  
  # Mujeres 2002
  {
    defs_m_baja <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(2001, 2002, 2003), tramo_esc == "baja") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_m_media <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(2001, 2002, 2003), tramo_esc == "media") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_m_alta <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(2001, 2002, 2003), tramo_esc == "alta") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    smot_m_02 <- create_joined_dataframe(defs_m_baja, defs_m_media, defs_m_alta, df_mujer_02, edad_grouping, "tab_m_02")
  }
  
  # Hombres 2017
  {
    defs_h_baja <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(2016, 2017, 2018), tramo_esc == "baja") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_h_media <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(2016, 2017, 2018), tramo_esc == "media") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_h_alta <- defs %>%
      filter(Sexo == 1, Ano_fa %in% c(2016, 2017, 2018), tramo_esc == "alta") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    smot_h_17 <- create_joined_dataframe(defs_h_baja, defs_h_media, defs_h_alta, df_hombre_17, edad_grouping, "tab_h_17")
  }
  
  # Mujeres 2017
  {
    defs_m_baja <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(2016, 2017, 2018), tramo_esc == "baja") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_m_media <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(2016, 2017, 2018), tramo_esc == "media") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    defs_m_alta <- defs %>%
      filter(Sexo == 2, Ano_fa %in% c(2016, 2017, 2018), tramo_esc == "alta") %>%
      group_by(Edad, tramo_esc) %>%
      summarise(Count = round(n() / 3), .groups = 'drop') %>%
      pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
    
    smot_m_17 <- create_joined_dataframe(defs_m_baja, defs_m_media, defs_m_alta, df_mujer_17, edad_grouping, "tab_m_17")
  } 
}

# Data frames con mortalidad suavizada y exposiciones suavizadas y observadas:
{
  smot_h_92 <- smooth_mortality(tab_h_92)
  smot_m_92 <- smooth_mortality(tab_m_92)
  
  smot_h_02 <- smooth_mortality(tab_h_02)
  smot_m_02 <- smooth_mortality(tab_m_02)
  
  smot_h_17 <- smooth_mortality(tab_h_17)
  smot_m_17 <- smooth_mortality(tab_m_17)
  
}
# Data frames con mortalidad extendida por Kannisto:
{
  extended_smot_h_92 <- extend_mortality_rates(smot_h_92)
  extended_smot_m_92 <- extend_mortality_rates(smot_m_92)
  
  extended_smot_h_02 <- extend_mortality_rates(smot_h_02)
  extended_smot_m_02 <- extend_mortality_rates(smot_m_02)
  
  extended_smot_h_17 <- extend_mortality_rates(smot_h_17)
  extended_smot_m_17 <- extend_mortality_rates(smot_m_17)
}

# Tablas de vida por metodo y año: escoger si con o sin extension.
{
  # Sin extension
  generate_life_tables(smot_h_92, "h", "92")
  generate_life_tables(smot_m_92, "m", "92")
  
  generate_life_tables(smot_h_02, "h", "02")
  generate_life_tables(smot_m_02, "m", "02")
  
  generate_life_tables(smot_h_17, "h", "17")
  generate_life_tables(smot_m_17, "m", "17")
  
  # Con extension:
  generate_life_tables(extended_smot_h_92, "h", "92")
  generate_life_tables(extended_smot_m_92, "m", "92")
  
  generate_life_tables(extended_smot_h_02, "h", "02")
  generate_life_tables(extended_smot_m_02, "m", "02")
  
  generate_life_tables(extended_smot_h_17, "h", "17")
  generate_life_tables(extended_smot_m_17, "m", "17")
  
}

# Ahora plotiemos:
{
  # Donde guardamos las cosas:
  {
    #setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results/Grouping/99") #para edad maxima 99
    setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results/Grouping/90") #para edad maxima 90
    #setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results/Grouping/88") #para edad maxima 88
    
  }
  
  # Plots de Mxs:
  {
    plot_mortality_comparison(smot_h_92, "1992", "hombres")
    plot_mortality_comparison(smot_m_92, "1992", "mujeres")
    
    plot_mortality_comparison(smot_h_02, "2002", "hombres")
    plot_mortality_comparison(smot_m_02, "2002", "mujeres")
    
    plot_mortality_comparison(smot_h_17, "2017", "hombres")
    plot_mortality_comparison(smot_m_17, "2017", "mujeres")
    
    ### Extension Kannisto
    plot_mortality_comparison(extended_smot_h_92, "1992", "hombres")
    plot_mortality_comparison(extended_smot_m_92, "1992", "mujeres")
    
    plot_mortality_comparison(extended_smot_h_02, "2002", "hombres")
    plot_mortality_comparison(extended_smot_m_02, "2002", "mujeres")
    
    plot_mortality_comparison(extended_smot_h_17, "2017", "hombres")
    plot_mortality_comparison(extended_smot_m_17, "2017", "mujeres")
    
    
    
  }
  
  # Plots de e_0s
  {
    {
      life_table_data <- list(
        # 1992 - Hombres
        lfh_alta_92_observed = lfh_alta_92_observed,
        lfh_alta_92_WH = lfh_alta_92_wh,
        lfh_alta_92_Splines = lfh_alta_92_splines,
        
        lfh_media_92_observed = lfh_media_92_observed,
        lfh_media_92_WH = lfh_media_92_wh,
        lfh_media_92_Splines = lfh_media_92_splines,
        
        lfh_baja_92_observed = lfh_baja_92_observed,
        lfh_baja_92_WH = lfh_baja_92_wh,
        lfh_baja_92_Splines = lfh_baja_92_splines,
        
        lfh_totales_92_observed = lfh_totales_92_observed,
        lfh_totales_92_WH = lfh_totales_92_wh,
        lfh_totales_92_Splines = lfh_totales_92_splines,
        
        # 1992 - Mujeres
        lfm_alta_92_observed = lfm_alta_92_observed,
        lfm_alta_92_WH = lfm_alta_92_wh,
        lfm_alta_92_Splines = lfm_alta_92_splines,
        
        lfm_media_92_observed = lfm_media_92_observed,
        lfm_media_92_WH = lfm_media_92_wh,
        lfm_media_92_Splines = lfm_media_92_splines,
        
        lfm_baja_92_observed = lfm_baja_92_observed,
        lfm_baja_92_WH = lfm_baja_92_wh,
        lfm_baja_92_Splines = lfm_baja_92_splines,
        
        lfm_totales_92_observed = lfm_totales_92_observed,
        lfm_totales_92_WH = lfm_totales_92_wh,
        lfm_totales_92_Splines = lfm_totales_92_splines,
        
        # 2002 - Hombres
        lfh_alta_02_observed = lfh_alta_02_observed,
        lfh_alta_02_WH = lfh_alta_02_wh,
        lfh_alta_02_Splines = lfh_alta_02_splines,
        
        lfh_media_02_observed = lfh_media_02_observed,
        lfh_media_02_WH = lfh_media_02_wh,
        lfh_media_02_Splines = lfh_media_02_splines,
        
        lfh_baja_02_observed = lfh_baja_02_observed,
        lfh_baja_02_WH = lfh_baja_02_wh,
        lfh_baja_02_Splines = lfh_baja_02_splines,
        
        lfh_totales_02_observed = lfh_totales_02_observed,
        lfh_totales_02_WH = lfh_totales_02_wh,
        lfh_totales_02_Splines = lfh_totales_02_splines,
        
        # 2002 - Mujeres
        lfm_alta_02_observed = lfm_alta_02_observed,
        lfm_alta_02_WH = lfm_alta_02_wh,
        lfm_alta_02_Splines = lfm_alta_02_splines,
        
        lfm_media_02_observed = lfm_media_02_observed,
        lfm_media_02_WH = lfm_media_02_wh,
        lfm_media_02_Splines = lfm_media_02_splines,
        
        lfm_baja_02_observed = lfm_baja_02_observed,
        lfm_baja_02_WH = lfm_baja_02_wh,
        lfm_baja_02_Splines = lfm_baja_02_splines,
        
        lfm_totales_02_observed = lfm_totales_02_observed,
        lfm_totales_02_WH = lfm_totales_02_wh,
        lfm_totales_02_Splines = lfm_totales_02_splines,
        
        # 2017 - Hombres
        lfh_alta_17_observed = lfh_alta_17_observed,
        lfh_alta_17_WH = lfh_alta_17_wh,
        lfh_alta_17_Splines = lfh_alta_17_splines,
        
        lfh_media_17_observed = lfh_media_17_observed,
        lfh_media_17_WH = lfh_media_17_wh,
        lfh_media_17_Splines = lfh_media_17_splines,
        
        lfh_baja_17_observed = lfh_baja_17_observed,
        lfh_baja_17_WH = lfh_baja_17_wh,
        lfh_baja_17_Splines = lfh_baja_17_splines,
        
        lfh_totales_17_observed = lfh_totales_17_observed,
        lfh_totales_17_WH = lfh_totales_17_wh,
        lfh_totales_17_Splines = lfh_totales_17_splines,
        
        # 2017 - Mujeres
        lfm_alta_17_observed = lfm_alta_17_observed,
        lfm_alta_17_WH = lfm_alta_17_wh,
        lfm_alta_17_Splines = lfm_alta_17_splines,
        
        lfm_media_17_observed = lfm_media_17_observed,
        lfm_media_17_WH = lfm_media_17_wh,
        lfm_media_17_Splines = lfm_media_17_splines,
        
        lfm_baja_17_observed = lfm_baja_17_observed,
        lfm_baja_17_WH = lfm_baja_17_wh,
        lfm_baja_17_Splines = lfm_baja_17_splines,
        
        lfm_totales_17_observed = lfm_totales_17_observed,
        lfm_totales_17_WH = lfm_totales_17_wh,
        lfm_totales_17_Splines = lfm_totales_17_splines
      ) 
    } # Lista de informacion de las tablas de vida
    
    
    # Apply the function to your data
    plot_life_expectancy_comparison(life_table_data, "h", "92")
    plot_life_expectancy_comparison(life_table_data, "m", "92")
    
    plot_life_expectancy_comparison(life_table_data, "h", "02")
    plot_life_expectancy_comparison(life_table_data, "m", "02")
    
    plot_life_expectancy_comparison(life_table_data, "h", "17")
    plot_life_expectancy_comparison(life_table_data, "m", "17")
    
    # Apply the function to your data
    plot_method_comparison(life_table_data, "h", "92")
    plot_method_comparison(life_table_data, "m", "92")
    plot_method_comparison(life_table_data, "h", "02")
    plot_method_comparison(life_table_data, "m", "02")
    plot_method_comparison(life_table_data, "h", "17")
    plot_method_comparison(life_table_data, "m", "17")
  }
  
  # Plots de qxs
  {
    plot_qx_comparison(life_table_data, "h", "92")
    plot_qx_comparison(life_table_data, "m", "92")
    
    plot_qx_comparison(life_table_data, "h", "02")
    plot_qx_comparison(life_table_data, "m", "02")
    
    plot_qx_comparison(life_table_data, "h", "17")
    plot_qx_comparison(life_table_data, "m", "17")
  }
  # Guardar los csv y imagenes
  { #csvs
    save_life_tables_to_csv(life_table_data, "h", "92")
    save_life_tables_to_csv(life_table_data, "m", "92")
    
    save_life_tables_to_csv(life_table_data, "h", "02")
    save_life_tables_to_csv(life_table_data, "m", "02")
    
    save_life_tables_to_csv(life_table_data, "h", "17")
    save_life_tables_to_csv(life_table_data, "m", "17")
    # tablas, salio pesimo
    #save_life_tables_as_images(life_table_data, "h", "92")
    #save_life_tables_as_images(life_table_data, "m", "92")
    
    #save_life_tables_as_images(life_table_data, "h", "02")
    #save_life_tables_as_images(life_table_data, "m", "02")
    
    #save_life_tables_as_images(life_table_data, "h", "17")
    #save_life_tables_as_images(life_table_data, "m", "17")
  }
  
}

head(lfm_alta_02_observed)