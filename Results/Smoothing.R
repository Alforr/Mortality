################# Mortalidad:
{
  
  #Librerias
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
        (Nivel == 1 & Curso %in% 1:9) ~ "alta",
        (Nivel == 2 & Curso %in% c(5)) | (Nivel == 3 & Curso %in% 3:6) | (Nivel == 2 & Curso %in% 1:4) ~ "media",
        (Nivel == 3 & Curso %in% 1:2) | (Nivel == 4 & Curso %in% 1:6) | (Nivel == 5 & Curso %in% 0) | (Nivel == 4 & Curso %in% 7:8) ~ "baja",
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
    df_mujer_92 <- df_mujer_92 %>% select(-Total)
    
    #df_hombre_92 <- df_hombre_92 %>% select(-esc_bajo)
    #df_hombre_92 <- df_hombre_92 %>% select(-esc_alto)
  }
  # La categoria de años de estudio es de 0 a 20 , 0 es nunca asistio y 20 es el maximo valor posible.
  # X0 a X6(2:8); sin instruccion a 6to, X7 a X11(9:14) (hasta 4to medio), X12 a X20(15:22) escolaridad alta.
  {
    set.seed(123)
    df_mujer_92$esc_bajo <- rowSums(df_mujer_92[,2:10])
    df_mujer_92$esc_medio <- rowSums(df_mujer_92[,11:14])
    df_mujer_92$esc_alto <- rowSums(df_mujer_92[,15:22])
    df_mujer_92$total <- rowSums(df_mujer_92[,2:22])
    
    df_hombre_92$esc_bajo <- rowSums(df_hombre_92[,2:10])
    df_hombre_92$esc_medio <- rowSums(df_hombre_92[,11:14])
    df_hombre_92$esc_alto <- rowSums(df_hombre_92[,15:22])
    df_hombre_92$total <- rowSums(df_mujer_92[,2:22])
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
      select(-c(2:22))
    df_mujer_92 <- df_mujer_92 %>%
      select(-c(2:22))
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
# Ahora al Censo 2002:
{
  #Fijemos directorio donde esten los archivos del censo 2002
  {
    setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/Datos/Censo/Censo 2002")
    getwd()
  }
  #Instalar/usar paquetes necesarios:
  {
    
  }
  # Leer los csv de las tablas del censo por genero: reciclemos codigo del 92
  {
    df_hombre_02 <- read.csv("Hombres_2002.csv", sep = ";")
    df_mujer_02 <- read.csv("Mujeres_2002.csv", sep = ";")
  }
  # Trunquemos los df para que tengan rango de edad de 25:110 y los que no tengan sean 0s
  {
    rango_edad_min_max <- 25:110
    df_rango_edades <- data.frame(Edad = rango_edad_min_max)
    df_mujer_02 <- df_rango_edades %>%
      left_join(df_mujer_02, by = "Edad") %>%
      replace_na(list(Cantidad = 0)) %>%
      arrange(Edad)
    df_hombre_02 <- df_rango_edades %>%
      left_join(df_hombre_02, by = "Edad") %>%
      replace_na(list(Cantidad = 0)) %>%
      arrange(Edad)
  }
  #Definir categorias para el censo 2002 igual que el censo 92 y 17, 2:6 basica, 7:12 media, 13:16 alta, educacion tecnica femenina nunca ha sido tomado
  {
    df_mujer_02$esc_bajo <- rowSums(df_mujer_02[,2:5])
    df_mujer_02$esc_medio <- rowSums(df_mujer_02[,6:12])
    df_mujer_02$esc_alto <- rowSums(df_mujer_02[,13:16])
    
    df_hombre_02$esc_bajo <- rowSums(df_hombre_02[,2:5])
    df_hombre_02$esc_medio <- rowSums(df_hombre_02[,6:12])
    df_hombre_02$esc_alto <- rowSums(df_hombre_02[,13:16])
  }
  #Trasvasijemos a totales
  {
    df_totales_02 <- df_hombre_02 %>%
      inner_join(df_mujer_02, by = "Edad", suffix = c("_hombre","_mujer")) %>%
      mutate(esc_bajo = esc_bajo_hombre + esc_bajo_mujer, esc_medio = esc_medio_hombre + esc_medio_mujer, esc_alto = esc_alto_hombre + esc_alto_mujer) %>%
      select(Edad, esc_bajo,esc_medio, esc_alto) %>%
      arrange(Edad)
  }
  # Limpiemos las columnas con las que construimos los escalones de escolaridad:
  {
    df_hombre_02 <- df_hombre_02 %>%
      select(-c(2:17))
    df_mujer_02 <- df_mujer_02 %>%
      select(-c(2:17))
  }
  # Agreguemos una columna de año para dejar ID para los modelos
  {
    df_hombre_02$year <- 2002
    df_mujer_02$year <- 2002
    df_totales_02$year <- 2002
  }
  
  # Saquemos los resultados a tablas para revision:
  # Pasemos los datos de proporciones y counts a tablas de datos para guardar:
  #setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results")
  #write.csv(df_hombre_02, "df_hombre_02_table.csv", row.names = FALSE)
  #write.csv(df_mujer_02, "df_mujer_02_table.csv", row.names = FALSE)
  #write.csv(df_totales_02, "df_totales_02_table.csv", row.names = FALSE)
  
}
# Limpieza informacion innecesaria:
rm(df_rango_edades)
# Ahora el Censo 2017:
{
  
  #Manipulemos datos
  setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/Datos/Censo/Censo 2017")
  getwd() #chequear que este bien el directorio para guardar el trabajo
  data <- read_delim("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/Datos/Censo/Censo 2017/Microdato_Censo2017-Personas.csv", delim = ";")
  
  #Quedemosnos con las columnas que necesitamos:
  data <- data %>%
    select(12, 13, 27, 26)
  
  data <- data %>%
    rename(
      sexo = P08,
      edad = P09,
      nivel = P15,
      curso = P14
    )
  niveles <- unique(data$nivel)
  print(niveles)
  curso <- unique(data$curso)
  print(curso)
  data <- data %>% # tramos escolaridad INE
    mutate(
      tramo_esc_INE = case_when(
        (nivel >= 11 & nivel < 15)  ~ "alta",
        (nivel > 6 & nivel < 11) ~ "media",
        (nivel <= 6) ~ "baja", # se agrega 7mo y 8vo a esta categoria
        TRUE ~ NA_character_  # Resto seran NAs
      )
    )
  data <- data %>% # tramos escolaridad INE
    mutate(
      tramo_esc = case_when(
        (nivel >= 11 & nivel < 15)  ~ "alta",
        (nivel > 6 & nivel < 11) ~ "media",
        (nivel <= 6) ~ "baja", # se agrega 7mo y 8vo a esta categoria
        TRUE ~ NA_character_  # Resto seran NAs
      )
    )
  
  #Proporciones escolaridad censo 2017:
  proporciones_propio <- data %>% #proporciones de los tramos segun nuestros parametros
    count(tramo_esc) %>%
    mutate(proportion = n / sum(n)) 
  setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results")
  write.csv(proporciones_propio, "proporciones_propio_censo2017_table.csv", row.names = FALSE)
  
  setwd("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/Datos/Censo/Censo 2017")
  getwd() #chequear que este bien el directorio para guardar el trabajo
  
  #creemos las tablas de Q por sexo:
  df_total_17_sexos <- data %>%
    group_by(edad, sexo) %>%
    summarise(Count = n()) %>%
    pivot_wider(names_from = sexo, values_from = Count, values_fill = 0)
  
  df_total_17 <- data %>%
    group_by(edad, tramo_esc) %>%
    summarise(Count = n()) %>%
    pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = 0)
  
  df_hombre_17 <- data %>%
    filter(sexo == 1) %>%
    group_by(edad, tramo_esc) %>%
    summarise(Count = n()) %>%
    pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = 0)
  
  df_mujer_17 <- data %>%
    filter(sexo == 2) %>%
    group_by(edad, tramo_esc) %>%
    summarise(Count = n()) %>%
    pivot_wider(names_from = tramo_esc, values_from = Count, values_fill = 0)
  
  
  df_proportions <- data %>%
    filter(sexo == 1) %>%
    group_by(edad, tramo_esc) %>%
    summarise(Count = n()) %>%
    mutate(Proportion = Count / sum(Count))
  
  
  ggplot(df_proportions, aes(x = as.factor(edad), y = Proportion, fill = tramo_esc)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Edad", y = "Proportion", fill = "Tramo Esc") +
    scale_fill_manual(values = c("alta" = "blue", "media" = "orange", "baja" = "green")) +  # colores
    theme_minimal() +
    ggtitle("Proporciones de tramo de escolaridad por edad: Hombres")
  
  df_proportions <- data %>%
    filter(sexo == 2) %>%
    group_by(edad, tramo_esc) %>%
    summarise(Count = n()) %>%
    mutate(Proportion = Count / sum(Count))
  
  
  ggplot(df_proportions, aes(x = as.factor(edad), y = Proportion, fill = tramo_esc)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Edad", y = "Proportion", fill = "Tramo Esc") +
    scale_fill_manual(values = c("alta" = "blue", "media" = "orange", "baja" = "green")) +  # colores
    theme_minimal() +
    ggtitle("Proporciones de tramo de escolaridad por edad: Mujeres")
  
  
  # Guardemos las tablas de cantidad de personas por genero por edad por tramo:
  setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results")
  #write.csv(df_hombre_17, "df_hombre_17_table.csv", row.names = FALSE)
  #write.csv(df_mujer_17, "df_mujer_17_table.csv", row.names = FALSE)
  #write.csv(df_total_17, "df_total_17_table.csv", row.names = FALSE)
  #write.csv(df_proportions, "proporciones_edad_2017.csv", row.names = FALSE)
  
  rm(data, df_proportions, df_total_17_sexos)
}  
# Limpieza informacion innecesaria:
rm(anos_f_unique,curso,curso_f_unique,niveles,nivel_f_unique,sexo_f_unique)
# Truncar los df del censo 2017:
{
  df_rango_edades <- data.frame(edad = rango_edad_min_max)
  nombres_columnas <- c("Edad", "esc_bajo","NA", "esc_medio","esc_alto")
  df_mujer_17 <- df_rango_edades %>%
    left_join(df_mujer_17, by = "edad") %>%
    replace_na(list(Cantidad = 0)) %>%
    arrange(edad)
  df_hombre_17 <- df_rango_edades %>%
    left_join(df_hombre_17, by = "edad") %>%
    replace_na(list(Cantidad = 0)) %>%
    arrange(edad)
  df_total_17 <- df_rango_edades %>%
    left_join(df_total_17, by = "edad") %>%
    replace_na(list(Cantidad = 0)) %>%
    arrange(edad)
  colnames(df_mujer_17) <- nombres_columnas
  colnames(df_hombre_17) <- nombres_columnas
  colnames(df_total_17) <- nombres_columnas
  df_mujer_17$year <- 2017
  df_hombre_17$year <- 2017
  df_total_17$year <- 2017
  df_mujer_17 <- df_mujer_17 %>%
    select(Edad, esc_bajo,esc_medio,esc_alto,year,"NA")
  df_hombre_17 <- df_hombre_17 %>%
    select(Edad, esc_bajo,esc_medio,esc_alto,year,"NA")
  df_total_17 <- df_total_17 %>%
    select(Edad, esc_bajo,esc_medio,esc_alto,year,"NA")
  rm(nombres_columnas)
}
#########################################################



  