# Ahora el Censo 2017:
{
  #Usemos los paquetes
  {
    library(haven)
    library(readr)
    library(lubridate)
    library(dplyr)
    library(tidyr)  
    library(ggplot2) 
  }
  
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
  write.csv(df_hombre_17, "df_hombre_17_table.csv", row.names = FALSE)
  write.csv(df_mujer_17, "df_mujer_17_table.csv", row.names = FALSE)
  write.csv(df_total_17, "df_total_17_table.csv", row.names = FALSE)
  write.csv(df_proportions, "proporciones_edad_2017.csv", row.names = FALSE)
  
}  
  