library(haven)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)  
library(ggplot2)
library(psych)

# Censo 2017
data <- read.csv("C:/Users/Alfonso Orrego/OneDrive - Universidad Adolfo Ibanez/Simplex/UAI/Tesis/Tablas de vida/Longevidad y educacion/Datos/Censo/Censo 2017/Microdato_Censo2017-Personas.csv", sep = ";")
{
  data <- data %>%
    select(P08,P09,P14,P15,P15A,ESCOLARIDAD)
  
  data <- data %>%
    rename(
      Sexo = P08,
      Edad = P09,
      Curso = P14,
      Nivel = P15,
      Binario_nivel = P15A
    )
  data <- data %>% # tramos escolaridad INE
    mutate(
      tramo_esc = case_when(
        (Nivel >= 12 & Nivel < 15)  ~ "alta",
        (Nivel > 7 & Nivel < 12) ~ "media",
        (Nivel <= 7) ~ "baja", # se agrega 7mo y 8vo a esta categoria
        TRUE ~ NA_character_  # Resto seran NAs
      )
    )
  
  #describe(data$Nivel) # Estadisticas descriptivas de Nivel
  
  # Rompamos un poco la muestra completa para generos
  
  data_male <- data %>% filter(Sexo == 1)
  data_female <- data %>% filter(Sexo == 2)
  
  # Ahora para tramos etarios : >25, >60, >85
  data_male_25_up <- data_male %>% filter(Edad >= 25)
  data_female_25_up <- data_female %>% filter(Edad >= 25)
  data_male_60_up <- data_male %>% filter(Edad >= 60)
  data_female_60_up <- data_female %>% filter(Edad >= 60)
  data_male_85_up <- data_male %>% filter(Edad >= 85)
  data_female_85_up <- data_female %>% filter(Edad >= 85)
  # Empezemos con las visualizaciones de como se comporta la sociedad:
  nombres <- c("Sala cuna", "Pre-K", "Kinde", "Especial/Diferencial", "Basica", "Primaria", 
               "Cientifico Humanista", "Tecnica Profesional", "Humanidades", 
               "Tecnica comercial/industrial", "Tecnica Superior", "Profesional", 
               "Magister", "Doctor", "No aplica", "Missing")
  nombres_df <- data.frame(attribute = 1:16, names = nombres)
  nombres_tramos <- c("Escolaridad baja","Escolaridad media","Escolaridad alta")
  
  
  distribucion_nivel <- data %>% # muestra completa
    count(tramo_esc) %>%
    mutate(percentage = n / sum(n) * 100)
  
  
  result <- distribucion_nivel %>%
    left_join(nombres_df, by = c("Nivel_aprobado" = "attribute"))
  result <- result[-((nrow(result)-1):nrow(result)), ] # Borrar los NA (6% de la muestra)
  names(result)[names(result) == "names"] <- "Nivel_escolar"
  
  # Grafico: Distribucion Muestra completa
  {
    ggplot(result, aes(x = Nivel_aprobado, y = percentage, fill = Nivel_escolar)) +
      geom_bar(stat = "identity") +  
      geom_text(aes(label = Nivel_escolar), position = position_stack(vjust = 0.5)) +  
      labs(title = "Composicion escolaridad: muestra completa (Censo 2017)", x = "Nivel Aprobado", y = "% Relativo") +
      theme_minimal()  
  }
  # Funcion Plot: distribucion niveles y tramos:
  plot_distribution <- function(data_subset, title_suffix) {
    distribucion_nivel <- data_subset %>%
      count(Nivel) %>%
      mutate(percentage = n / sum(n) * 100)
    
    result <- distribucion_nivel %>%
      left_join(nombres_df, by = c("Nivel" = "attribute"))
    
    result <- result[-((nrow(result)-1):nrow(result)), ] # Remove NAs (if needed)
    names(result)[names(result) == "names"] <- "Nivel_escolar"
    
    ggplot(result, aes(x = Nivel, y = percentage, fill = Nivel_escolar)) +
      geom_bar(stat = "identity") +  
      geom_text(aes(label = Nivel_escolar), position = position_stack(vjust = 0.5)) +  
      labs(title = paste("Composicion escolaridad:", title_suffix), x = "Nivel Aprobado", y = "% Relativo") +
      theme_minimal()}
  
  plot_tramos <- function(data_subset, title_suffix) {
    distribucion_nivel <- data_subset %>%
      count(tramo_esc) %>%
      mutate(percentage = n / sum(n) * 100)
    names(distribucion_nivel)[names(distribucion_nivel) == "tramo_esc"] <- "Tramos"
    distribucion_nivel <- distribucion_nivel[-nrow(distribucion_nivel), ]
    ggplot(distribucion_nivel, aes(x = Tramos, y = percentage, fill = Tramos)) +
      geom_bar(stat = "identity") +  
      geom_text(aes(label = Tramos), position = position_stack(vjust = 0.5)) +  
      labs(title = paste("Composicion escolaridad:", title_suffix), x = "Tramos escolaridad", y = "% Relativo") +
      theme_minimal()}
  
  
  # Funcion a los data frames: distribucion niveles y tramos
  {
    setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results/Images/Composicion escolaridad (CENSO 2017)/Niveles")
    plot_distribution(data_male, "Hombres")
    plot_distribution(data_female, "Mujeres")
    plot_distribution(data_male_25_up, "Hombres +25")
    plot_distribution(data_female_25_up, "Mujeres +25")
    plot_distribution(data_male_60_up, "Hombres +60")
    plot_distribution(data_female_60_up, "Mujeres +60")
    plot_distribution(data_male_85_up, "Hombres +85")
    plot_distribution(data_female_85_up, "Mujeres +85")
  }
  {
    setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Results/Images/Composicion escolaridad (CENSO 2017)/Tramos")
    plot_tramos(data_male, "Hombres")
    plot_tramos(data_male_25_up, "Hombres +25")
    plot_tramos(data_male_60_up, "Hombres +60")
    plot_tramos(data_male_85_up, "Hombres +85")
    plot_tramos(data_female, "Mujeres")
    plot_tramos(data_female_25_up, "Mujeres +25")
    plot_tramos(data_female_60_up, "Mujeres +60")
    plot_tramos(data_female_85_up, "Mujeres +85")
    
    
    
  }
  
  
}

# Defunciones:
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
{
  
  
  
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
  
  
}
