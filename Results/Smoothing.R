################# Mortalidad:
{
  
  #Librerias
  install.packages("readxl")
  install.packages("tidyverse")
  {
    library(haven)
    library(readr)
    library(lubridate)
    library(dplyr)
    library(tidyr)  
    library(ggplot2)   
    library(stringr)
    library(readxl)
    library(tidyverse)
  }
  # Setiar directorio de trabajo donde estaremos trabajando y estan los datos
  setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/DATOS_INE_ADM/RAW/AH007T0012008/SAIP AH007T0012008")
  file_list <- list.files(path = getwd(), pattern = "\\.xlsx$", full.names = TRUE)
  
  # Leer Excels proveidos por el INE
  data_list <- lapply(file_list, read_excel)
  names(data_list) <- basename(file_list)
  
  # Quedemosnos con las variables que nos interesan
  columnas_si <- c(1, 3, 4, 5, 6, 7, 8, 9)
  data_list <- lapply(data_list, function(df) df[, columnas_si])
  
  # Saquemos lo necesario
  main_list <- list(
    data_list[2:4],  # Años que necesitamos de toda la lista 1990:2021
    data_list[12:14],
    data_list[27:29]
  )
  
  # Pongamosle nombre a las columnas
  nombres_columnas <- c("Sexo", "Edad", "Curso", "Nivel", "Dia_fa", "Mes_fa", "Ano_fa", "Comuna")
  
  # Creemos funcion para ir cambiando nombres rapidamente
  rename_columns <- function(df, new_names) {
    if (is.data.frame(df)) {
      colnames(df) <- new_names
    }
    return(df)
  }
  
  # Cambiemos los nombres de las columnas
  for (i in seq_along(main_list)) {
    for (j in seq_along(main_list[[i]])) {
      main_list[[i]][[j]] <- rename_columns(main_list[[i]][[j]], nombres_columnas)
    }
  }
  names(main_list) <- c("1992", "2002", "2017") #cambiemosle nombres para sus centros
  
  # Appendiarlos todos a un data frame de cada uno.
  defs <- do.call(rbind, lapply(main_list, function(sublist) {
    do.call(rbind, sublist)
  }))
  
  # Botemos todo excepto el df.
  rm(file_list, data_list, main_list, nombres_columnas, rename_columns)
  rm(calendario1,calendario2,calendario3,i,j,def_01_03,def_16_18,def_91_93,combined_data_frame,combined_list)
  rm(columnas_si)
  
  # Exploremos un poco
  anos_f_unique <- unique(defs$Ano_fa)
  nivel_f_unique <- unique(defs$Nivel)
  curso_f_unique <- unique(defs$Curso)
  sexo_f_unique <- unique(defs$Sexo)
  
  print(anos_f_unique) # 3 tripletas de años alrededor de los censos : 1991 1992 1993 2001 2002 2003 2016 2017 2018
  print(nivel_f_unique) # 1,2,3,4,5,9,0 #hay que chequiar que es el 9, #nivel 2 corresponde a media: 5 4 3 1 2 9 0
  print(curso_f_unique) # 0:9 , corresponde mejor la utilizacion de niveles: 0 4 2 6 7 5 8 1 3 9
  
  # Para estudio INE, se arma esc_baja como 0 a 8vo, esc_media como 9-12, alta como 13+
  # Para nosotros sera: 0-6to (por regimen antiguo de escolaridad, pre reforma Juan Gomez Milla), de 7 a 12 para media, y 13+ para esc_alta
  # Para armar estas combinaciones hay que hacer algunos arreglos de los valores de curso & nivel para que conversen estos. AKA, armar columna de tramo de escolaridad.
  # Sexo = 1 = Hombre, 2 = mujer, 9 = indeterminado
  # Armemos las columnas para comparar poblaciones entre INE y yo:
  
  # Columna escalones propios:
  
  defs <- defs %>%
    mutate(
      tramo_esc = case_when(
        (Nivel == 2 & Curso %in% c(5)) | (Nivel == 1 & Curso %in% 1:9) ~ "alta",
        (Nivel == 3 & Curso %in% 1:6) | (Nivel == 4 & Curso %in% 7:8) | (Nivel == 2 & Curso %in% 1:4) ~ "media",
        (Nivel == 4 & Curso %in% 1:6) | (Nivel == 5 & Curso %in% 0) ~ "baja",
        TRUE ~ NA_character_  # Resto seran NAs
      )
    )
  # Columna escalones INE:
  defs <- defs %>%
    mutate(
      tramo_esc_INE = case_when(
        (Nivel == 2 & Curso %in% c(5)) | (Nivel == 1 & Curso %in% 1:9)  ~ "alta",
        (Nivel == 3 & Curso %in% 3:6) | (Nivel == 4 & Curso %in% 7:8) | (Nivel == 2 & Curso %in% 1:4) ~ "media",
        (Nivel == 3 & Curso %in% 1:2) | (Nivel == 4 & Curso %in% 1:8) | (Nivel == 5 & Curso %in% 0) ~ "baja", #se agrega 7mo y 8vo a esta categoria
        TRUE ~ NA_character_  # Resto seran NAs
      )
    )
  
  # Veamos proporciones de cada una de los escalones para toda la muestra (todos los años)
  proporciones_propio <- defs %>% #proporciones de los tramos segun nuestros parametros
    count(tramo_esc) %>%
    mutate(proportion = n / sum(n)) #baja: 63,58%, media: 31,21% , alta: 4,784%, otros: 0,413%
  
  proporciones_INE <- defs %>% #proporciones de los tramos segun INE
    count(tramo_esc_INE) %>%
    mutate(proportion = n / sum(n)) #baja: 69,40% , media: 25,40% , alta: 4,784%
  
  # Plotiemos las proporciones:
  plot <- ggplot(proporciones_propio, aes(x = tramo_esc, y = proportion, fill = tramo_esc)) +
    geom_bar(stat = "identity") +
    labs(title = "Proporcion de defunciones por tramos de escolaridad: Propio",
         x = "Tramos",
         y = "Proporciones") +
    theme_minimal()
  
  plot_INE <- ggplot(proporciones_INE, aes(x = tramo_esc_INE, y = proportion, fill = tramo_esc_INE)) +
    geom_bar(stat = "identity") +
    labs(title = "Proporcion de defunciones por tramos de escolaridad: INE",
         x = "Tramos",
         y = "Proporciones") +
    theme_minimal()
  
  plot #print el plot
  plot_INE #print el plot
  
  # Proporciones por año:
  proportions_by_year <- defs %>%
    group_by(Ano_fa, tramo_esc) %>%
    count() %>%
    group_by(Ano_fa) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup()
  
  plot_stack <- ggplot(proportions_by_year, aes(x = factor(Ano_fa), y = proportion, fill = tramo_esc)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Proporciones defunciones por escolaridad en muestra",
         x = "Años",
         y = "Proporcion",
         fill = "Value") +
    theme_minimal()
  plot_stack
  
  # Ahora para el INE
  proportions_by_year_INE <- defs %>%
    group_by(Ano_fa, tramo_esc_INE) %>%
    count() %>%
    group_by(Ano_fa) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup()
  
  plot_stack <- ggplot(proportions_by_year_INE, aes(x = factor(Ano_fa), y = proportion, fill = tramo_esc_INE)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Proporciones defunciones por escolaridad en muestra (INE)",
         x = "Años",
         y = "Proporcion",
         fill = "Value") +
    theme_minimal()
  plot_stack
  
  # Pasemos los datos de proporciones y counts a tablas de datos para guardar:
  setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results")
  write.csv(proportions_by_year, "proportions_by_year_table.csv", row.names = FALSE)
  write.csv(proportions_by_year_INE, "proportions_by_year_INE_table.csv", row.names = FALSE)
  
  # Volvamos al inicial
  setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/DATOS_INE_ADM/RAW/AH007T0012008/SAIP AH007T0012008")
  
  # Borremos todas la informacion creada para generar resultados:
  rm(plot,plot_INE,plot_stack,proporciones_INE,proporciones_propio) #poner o sacar dependiendo de que se quiere
  
}
# Generemos los datos de Censo: 1992
{
  #Fijemos directorio donde estan las tablas del Censo 1992
  setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/Datos/Censo/Censo 1992")
  getwd()
  
  #Instalar/usar paquetes necesarios:
  {
    library(haven)
    library(readr)
    library(lubridate)
    library(dplyr)
    library(tidyr)  
    library(ggplot2) 
  }
  # Leer los csv de las tablas del censo por genero
  {
    df_hombre_92 <- read.csv("Hombre_92.csv", sep = ";")
    df_mujer_92 <- read.csv("Mujer_92.csv", sep = ";")
  }
  # Trunquemos los df para que tengan rango de edad de 25:110 y los que no tengan sean 0s
  {
    rango_edad_min_max <- 25:110
    df_rango_edades <- data.frame(Edad = rango_edad_min_max)
    df_mujer_92 <- df_rango_edades %>%
      left_join(df_mujer_92, by = "Edad") %>%
      replace_na(list(Cantidad = 0)) %>%
      arrange(Edad)
    df_hombre_92 <- df_rango_edades %>%
      left_join(df_hombre_92, by = "Edad") %>%
      replace_na(list(Cantidad = 0)) %>%
      arrange(Edad)
  }
  # Borremos las columnas de categorias previamente hechas;
  {
    df_mujer_92 <- df_mujer_92 %>% select(-esc_bajo)
    df_mujer_92 <- df_mujer_92 %>% select(-esc_alto)
    df_hombre_92 <- df_hombre_92 %>% select(-esc_bajo)
    df_hombre_92 <- df_hombre_92 %>% select(-esc_alto)
  }
  # La categoria de años de estudio es de 0 a 20 , 0 es nunca asistio y 20 es el maximo valor posible.
  # X0 a X6(2:8); sin instruccion a 6to, X7 a X11(9:14) (hasta 4to medio), X12 a X20(15:22) escolaridad alta.
  {
    set.seed(123)
    df_mujer_92$esc_bajo <- rowSums(df_mujer_92[,2:8])
    df_mujer_92$esc_medio <- rowSums(df_mujer_92[,9:14])
    df_mujer_92$esc_alto <- rowSums(df_mujer_92[,15:22])
    
    df_hombre_92$esc_bajo <- rowSums(df_hombre_92[,2:8])
    df_hombre_92$esc_medio <- rowSums(df_hombre_92[,9:14])
    df_hombre_92$esc_alto <- rowSums(df_hombre_92[,15:22])
  }
  #Trasvasijemos a totales
  {
    df_totales_92 <- df_hombre_92 %>%
      inner_join(df_mujer_92, by = "Edad", suffix = c("_hombre","_mujer")) %>%
      mutate(esc_bajo = esc_bajo_hombre + esc_bajo_mujer, esc_medio = esc_medio_hombre + esc_medio_mujer, esc_alto = esc_alto_hombre + esc_alto_mujer) %>%
      select(Edad, esc_bajo,esc_medio, esc_alto) %>%
      arrange(Edad)
    
  }
  # Limpiemos las columnas con las que construimos los escalones de escolaridad:
  {
    df_hombre_92 <- df_hombre_92 %>%
      select(-c(2:23))
    df_mujer_92 <- df_mujer_92 %>%
      select(-c(2:23))
  }
  # Agreguemos una columna de año para dejar ID para los modelos
  {
    df_hombre_92$year <- 1992
    df_mujer_92$year <- 1992
    df_totales_92$year <- 1992
    
    # Saquemos los resultados a tablas para revision:
    # Pasemos los datos de proporciones y counts a tablas de datos para guardar:
    setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results")
    #write.csv(df_hombre_92, "df_hombre_92_table.csv", row.names = FALSE)
    #write.csv(df_mujer_92, "df_mujer_92_table.csv", row.names = FALSE)
    #write.csv(df_totales_92, "df_totales_92_table.csv", row.names = FALSE)
    
    # Volvamos al inicial
    setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/DATOS_INE_ADM/RAW/AH007T0012008/SAIP AH007T0012008")
    
  }
}


defs_h_baja <- defs %>%
  filter(Sexo == 1, Ano_fa %in% c(1991, 1992,1993),tramo_esc == "baja") %>%
  group_by(Edad, tramo_esc) %>%
  summarise(Count = round(n() / 3), .groups = 'drop') %>%
  pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
defs_h_media <- defs %>%
  filter(Sexo == 1, Ano_fa %in% c(1991, 1992,1993),tramo_esc == "media") %>%
  group_by(Edad, tramo_esc) %>%
  summarise(Count = round(n() / 3), .groups = 'drop') %>%
  pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
defs_h_alta <- defs %>%
  filter(Sexo == 1, Ano_fa %in% c(1991, 1992,1993),tramo_esc == "alta") %>%
  group_by(Edad, tramo_esc) %>%
  summarise(Count = round(n() / 3), .groups = 'drop') %>%
  pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = list(Count = 0))
  
smooths_hombre_92 <- df_hombre_92 %>%
  left_join(defs_h_baja, by = "Edad") %>%
  left_join(defs_h_media, by = "Edad") %>%
  left_join(defs_h_alta, by ="Edad")
#smooths_hombre_92 <- smooths_hombre_92 %>%
 # select(-year.x,-c(6:9))

tabla_sub_99 <- smooths_hombre_92 %>%
  filter(Edad < 99)

tabla_99_y_mas <- smooths_hombre_92 %>%
  filter(Edad >= 99) %>%
  summarise(
    Edad = 99,
    alta = sum(alta, na.rm = TRUE),
    media = sum(media, na.rm = TRUE),
    baja = sum(baja, na.rm = TRUE),
    esc_bajo.x = sum(esc_bajo.x, na.rm = TRUE),
    esc_medio.x = sum(esc_medio.x, na.rm = TRUE),
    esc_alto.x = sum(esc_alto.x, na.rm = TRUE)
  )
tabla_sub_99_agg <- tabla_sub_99 %>%
  group_by(Edad) %>%
  summarise(
    alta = sum(alta, na.rm = TRUE),
    media = sum(media, na.rm = TRUE),
    baja = sum(baja, na.rm = TRUE),
    esc_bajo.x = sum(esc_bajo.x, na.rm = TRUE),
    esc_medio.x = sum(esc_medio.x, na.rm = TRUE),
    esc_alto.x = sum(esc_alto.x, na.rm = TRUE)
  )
tabla_final <- tabla_sub_99_agg %>%
  bind_rows(tabla_99_y_mas) %>%
  arrange(Edad)
#########################################################






# Whittaker-Henderson Smoothing: hay que robarlo de diferentes bibliotecas.
{
  {
    library(devtools)
    install.packages("mgcv")
    library(mgcv)
    install.packages("svcm")
    library(svcm)
  }
  # WH Smooth:
  fit_wh <- Mort1Dsmooth(x = tabla_final$Edad, y = tabla_final$baja, offset = log(tabla_final$esc_bajo.x))
  smoothed_h_b <- fitted(fit_wh)
  
  # Splines:
  data <- data.frame(age = tabla_final$Edad, deaths = tabla_final$baja, exposures = tabla_final$esc_bajo.x)
  fit_ps <- gam(deaths ~ s(age, bs = "ps") + offset(log(exposures)), family = poisson, data = data)
  smoothed_deaths_ps <- fitted(fit_ps)
  
  plot(data$age, data$deaths, type = "p", main = "Smoothed Mortality Rates")
  lines(data$age, smoothed_h_b, col = "blue", lwd = 2, lty = 2)
  lines(data$age, smoothed_deaths_ps, col = "red", lwd = 2, lty = 1)
  legend("topleft", legend = c("Whittaker-Henderson", "Penalized B-Splines"), col = c("blue", "red"), lty = c(2, 1), lwd = 2)
  
  revision <- data.frame(age = tabla_final$Edad, splines = smoothed_deaths_ps, WH = smoothed_h_b, observado = data$deaths, exposure = data$exposures)
  revision <- revision %>%
    mutate(mx_o = observado/exposure,
           mx_s = splines/exposure,
           mx_WH = WH/exposure)
  ggplot()+
    geom_line(revision, mapping = aes(x=age, y= splines), color ="blue")+
    geom_point(revision, mapping = aes(x=age, y= splines), color ="blue")+
    geom_line(revision, mapping = aes(x=age, y= WH), color ="red")+
    geom_point(revision, mapping = aes(x=age, y= WH), color ="red")+
    geom_line(data, mapping = aes(x=age, y= deaths), color ="black")+
    labs(x="Edad",y="Mortalidad", title = "Mortalidad suavizada (H/B/92)")+
    theme_minimal()
  
  ggplot() + #Muertes
    geom_line(data = revision, aes(x = age, y = splines, color = "Splines")) +
    geom_point(data = revision, aes(x = age, y = splines, color = "Splines")) +
    geom_line(data = revision, aes(x = age, y = WH, color = "WH")) +
    geom_point(data = revision, aes(x = age, y = WH, color = "WH")) +
    geom_line(data = data, aes(x = age, y = deaths, color = "Deaths")) +
    scale_color_manual(values = c("Splines" = "blue", "WH" = "red", "Deaths" = "black"),
                       labels = c("Deaths", "Splines", "WH"),
                       name = "Legend") +
    labs(x = "Edad", y = "Mortalidad", title = "Mortalidad suavizada (H/B/92)") +
    theme_minimal()
  ggplot() + #Mortalidad
    geom_line(data = revision, aes(x = age, y = mx_s, color = "Splines")) +
    geom_point(data = revision, aes(x = age, y = mx_s, color = "Splines")) +
    geom_line(data = revision, aes(x = age, y = mx_WH, color = "WH")) +
    geom_point(data = revision, aes(x = age, y = mx_WH, color = "WH")) +
    geom_line(data = revision, aes(x = age, y = mx_o, color = "Deaths")) +
    scale_color_manual(values = c("Splines" = "blue", "WH" = "red", "Deaths" = "black"),
                       labels = c("Deaths", "Splines", "WH"),
                       name = "Legend") +
    labs(x = "Edad", y = "Mortalidad", title = "Mortalidad suavizada (H/B/92)") +
    theme_minimal()
  
  
  
  }

  


?inrange