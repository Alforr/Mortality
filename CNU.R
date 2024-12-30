# LLamar las diferentes fuentes de datos:
{
  # llamar la tabla de tasas de descuento # llamar la tabla de factores de mejora
  setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Data Wrangling/CNU")
  VTI <- read.csv("VTI.csv",sep = ";")
  FM <- read.csv("FM.csv",sep = ";")
  clean_improvement_factors <- function(FM) {
    # Loop through the columns H_X and M_X (which contain the improvement factors)
    FM$H_X <- as.numeric(sub("%", "", gsub(",", ".", FM$H_X))) / 100
    FM$M_X <- as.numeric(sub("%", "", gsub(",", ".", FM$M_X))) / 100
    
    return(FM)
  }
  FM <- clean_improvement_factors(FM)
  
  # llamar las tablas de vida 2017
  setwd("C:/Users/Alfonso Orrego/Documents/GitHub/Mortality/Data Wrangling/CNU/TM-ORREGO/17") #17,02 o 92 para diferentes TM
  path <- getwd()
  csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)
  TM_list <- list()
  for (file in csv_files) { # Se extraen todas las TM de la carpeta y se separan las columnas
    file_name <- tools::file_path_sans_ext(basename(file))
    TM_read <- read.csv(file,sep = ",")
    TM_list[[file_name]] <- TM_read} # Todas guardadas en esta lista de tablas TM_list
  life_table <- TM_list[["H_A"]]  # Example life table for males
  
  # funcion para determinar q(x) con factores de mejora a traves de los años
  adjust_qx_with_improvements <- function(gender, retirement_year, life_table, start_age) {
    # Extract the initial qx values from the life table for the starting age onward, excluding age 110
    qx_values <- life_table$qx[life_table$Edad >= start_age & life_table$Edad < 110]
    
    # Extract improvement factors based on gender, excluding age 110
    if (gender == "male") {
      improvement_factors <- FM$H_X[FM$Edad >= start_age & FM$Edad < 110]
    } else {
      improvement_factors <- FM$M_X[FM$Edad >= start_age & FM$Edad < 110]
    }
    
    # Convert the improvement factors from percentages (if needed)
    improvement_factors <- as.numeric(improvement_factors)
    
    # Warning in case of NA values in improvement factors
    if (any(is.na(improvement_factors))) {
      #print("Warning: NA values found in improvement factors!")
    }
    
    #print(paste("Improvement factors:", improvement_factors))
    
    # Initialize the data frame
    adjusted_qx_df <- data.frame(Edad = integer(), qx_adjusted = numeric())
    
    # Iterate from 1 to length - 1 (excluding age 110)
    for (i in 1:length(qx_values)) {
      age <- start_age + (i - 1)
      years_since_retirement <- age - start_age
      # Apply the improvement factor to the current qx value
      adjusted_qx <- qx_values[i] * (1 - improvement_factors[i])^(years_since_retirement)
      
      # Append the current age and adjusted qx to the data frame
      adjusted_qx_df <- rbind(adjusted_qx_df, data.frame(Edad = age, qx_adjusted = adjusted_qx))
      
      # Print intermediate results for debugging
      #print(paste("Iteration:", i, " Age:", age, " Years since retirement:", years_since_retirement))
      #print(paste("Adjusted qx for age", age, ":", adjusted_qx))
    }
    
    # Append the final row for age 110 where qx = 1
    adjusted_qx_df <- rbind(adjusted_qx_df, data.frame(Edad = 110, qx_adjusted = 1))
    
    return(adjusted_qx_df)
  }
  
  # Probemos la funcion
  adjusted_qx_df_male <- adjust_qx_with_improvements(gender = "female", retirement_year = 2017, life_table = life_table, start_age = 60)
  
  # Veamos resultado: (paso)
  #print(adjusted_qx_df_male)
  
  # Funcion para determinar lx de manera dinamica tambien:
  calculate_lx <- function(qx_adjusted_df) {
    # Initialize lx, with lx[1] = 1
    n <- nrow(qx_adjusted_df)
    lx <- numeric(n)
    lx[1] <- 1  # The first age starts with lx = 1
    
    # Calculate lx for each age by accumulating the survival probabilities
    for (i in 2:n) {
      lx[i] <- lx[i - 1] * (1 - qx_adjusted_df$qx_adjusted[i - 1])
    }
    
    # Append lx values to the data frame
    qx_adjusted_df$lx <- lx
    
    return(qx_adjusted_df)
  }
  
  adjusted_qx_df_male_with_lx <- calculate_lx(adjusted_qx_df_male)
  
  #print(adjusted_qx_df_male_with_lx)
  
  
  
}

# Armar el algoritmo de CNU para Vejez sin conyuge/hijos (modelo base, todo el resto solo resta)
{
  calculate_CNU <- function(gender, life_table, start_age, retirement_year, mode) {
    # Checkeo de modalidad de producto de pension.
    if (mode == "RP") {
      interest_rate_column <- VTI$TITRP
    } else if (mode == "RV") {
      interest_rate_column <- VTI$TMRV
    } else {
      stop("Invalid mode. Use 'RP' for TITRP or 'RV' for TMRV.")
    }
    
    # Checkeo numerico
    interest_rate_column <- as.numeric(interest_rate_column)
    
    # Checkeo de fecha
    if (!inherits(VTI$Periodo, "Date")) {
      VTI$Periodo <- as.Date(VTI$Periodo, format = "%d-%m-%Y")
    }
    
    # Filtros de año
    VTI_filtered <- VTI[format(VTI$Periodo, "%Y") == as.character(retirement_year), ]
    
    # Checkeo numerico
    interest_rate_column_filtered <- interest_rate_column[format(VTI$Periodo, "%Y") == as.character(retirement_year)] / 100
    
    # Checkeo numerico
    if (any(is.na(interest_rate_column_filtered))) {
      stop("Error: Non-numeric values found in the interest rate column.")
    }
    
    # Calcular la tasa de interes de mercado o RP para año/mes
    avg_interest_rate <- mean(interest_rate_column_filtered, na.rm = TRUE)
    
    # Ajustar probabilidades de muerte/supervivencia
    adjusted_qx_df <- adjust_qx_with_improvements(gender, retirement_year, life_table, start_age)
    lx_df <- calculate_lx(adjusted_qx_df)
    
    # Valores iniciales: CNU = 0, edad maxima = 110
    CNU <- 0
    max_age <- 110
    t_values <- seq(0, max_age - start_age)
    
    # Iterar de edad inicial a edad maxima 
    for (t in t_values) {
      age_at_t <- start_age + t
      lx_value <- lx_df$lx[lx_df$Edad == age_at_t]
      lx_start <- lx_df$lx[lx_df$Edad == start_age]
      
      # Suma componentes tipo Vega (2014)
      CNU <- CNU + (lx_value / lx_start) / ((1 + avg_interest_rate) ^ t)
    }
    
    # Restarle el factor de formula
    CNU <- CNU - (11 / 24)
    
    return(CNU)
  }
  
  life_table <- TM_list[["M_T"]]
  CNU_value <- calculate_CNU(gender = "female", life_table = life_table, start_age = 60, retirement_year = 2017, mode = "RP")  
  print(100000000/(CNU_value*12))
  print(CNU_value*12)
  
  #Ejemplo:
  {
    print(100000000/(CNU_value*12))
  }
}

# Armar el algoritmo de desacumulacion de fondos por RP
{
  # algoritmo deacumulacion RP:
  {
    calculate_deaccumulation <- function(initial_funds, gender, life_table, start_age, retirement_year, interest_rate) {
      # Initialize variables
      age_range <- start_age:110
      remaining_funds <- numeric(length(age_range))
      max_withdrawal <- numeric(length(age_range))
      CNU_values <- numeric(length(age_range))
      bequest <- numeric(length(age_range))
      
      # Initial funds
      remaining_funds[1] <- initial_funds
      
      # Calculate the adjusted qx and lx for the entire age range
      adjusted_qx_df <- adjust_qx_with_improvements(gender, retirement_year, life_table, start_age)
      lx_df <- calculate_lx(adjusted_qx_df)
      
      # Iterate through the age range to calculate deaccumulation
      for (i in seq_along(age_range)) {
        current_age <- age_range[i]
        lx_current <- lx_df$lx[lx_df$Edad == current_age]
        lx_start <- lx_df$lx[lx_df$Edad == start_age]
        
        # Recalculate the CNU for the current age
        t_values <- seq(0, 110 - current_age)
        CNU_values[i] <- sum((lx_df$lx[lx_df$Edad >= current_age] / lx_current) / ((1 + interest_rate) ^ t_values))
        CNU_values[i] <- CNU_values[i] - (11 / 24)  # Subtract the constant factor
        
        # Calculate the annual withdrawal for the current age
        max_withdrawal[i] <- remaining_funds[i] / CNU_values[i]
        
        # Update remaining funds after withdrawal and interest
        if (i < length(age_range)) {
          remaining_funds[i + 1] <- (remaining_funds[i] - max_withdrawal[i]) * (1 + interest_rate)
        }
        
        # Calculate the bequest (remaining funds after withdrawal for the year)
        bequest[i] <- max(remaining_funds[i] - max_withdrawal[i], 0)
        
        # Ensure no negative funds
        if (remaining_funds[i] < 0) {
          remaining_funds[i] <- 0
          max_withdrawal[i] <- 0
          CNU_values[i] <- 0
          bequest[i] <- 0
        }
      }
      
      # Create a data frame with the results
      deaccumulation_results <- data.frame(
        Age = age_range,
        Remaining_Funds = remaining_funds,
        Max_Withdrawal = max_withdrawal,
        Monthly_Pension = max_withdrawal / 12,
        Bequest = bequest,
        CNU = CNU_values
      )
      
      return(deaccumulation_results)
    }
    
  }
  
  
  # supuesto de rentabilidad basado en Fondo E 3% LP
  calculate_CNU <- function(gender, life_table, start_age, retirement_year, mode) {
    # Select the correct interest rate column based on the mode
    interest_rate_column <- ifelse(mode == "RP", "TITRP", "TMRV")
    
    # Extract and average the interest rate for the retirement year
    annual_rates <- VTI[[interest_rate_column]][format(as.Date(VTI$Periodo, "%d-%m-%Y"), "%Y") == as.character(retirement_year)]
    avg_interest_rate <- mean(annual_rates, na.rm = TRUE) / 100
    
    # Debug: Check interest rate calculation
    print(paste("Average interest rate for", retirement_year, "is:", avg_interest_rate))
    
    # Ensure interest rate is valid
    if (is.na(avg_interest_rate) || avg_interest_rate <= 0) {
      stop("Error: Invalid average interest rate.")
    }
    
    # Get adjusted qx values and calculate lx
    adjusted_qx_df <- adjust_qx_with_improvements(gender, retirement_year, life_table, start_age)
    adjusted_qx <- adjusted_qx_df$qx_adjusted
    lx_values <- cumprod(1 - adjusted_qx)
    
    # Debug: Check lx values
    print("lx values:")
    print(head(lx_values))
    
    # Ensure lx values are valid
    if (any(is.na(lx_values)) || lx_values[1] <= 0) {
      stop("Error: Invalid lx values.")
    }
    
    # Calculate the CNU as the sum of discounted lx values
    CNU <- sum(lx_values * (1 / (1 + avg_interest_rate))^(0:(length(lx_values) - 1)))
    
    # Debug: Check final CNU
    print(paste("Calculated CNU:", CNU))
    
    # Ensure CNU is valid
    if (is.na(CNU) || CNU <= 0) {
      stop("Error in CNU calculation: CNU is NA or non-positive.")
    }
    
    return(CNU)
  }
  
  
  # Ejemplo de uso 3% (0.03):
  {
    deaccumulation_results_HA <- calculate_deaccumulation(
      initial_funds = 100000000,
      gender = "male",
      life_table = TM_list[["H_A"]],
      start_age = 65,
      retirement_year = 2017,
      interest_rate = 0.03
    )
    deaccumulation_results_HM <- calculate_deaccumulation(
      initial_funds = 100000000,
      gender = "male",
      life_table = TM_list[["H_M"]],
      start_age = 65,
      retirement_year = 2017,
      interest_rate = 0.03
    )
    deaccumulation_results_HB <- calculate_deaccumulation(
      initial_funds = 100000000,
      gender = "male",
      life_table = TM_list[["H_B"]],
      start_age = 65,
      retirement_year = 2017,
      interest_rate = 0.03
    )
    deaccumulation_results_HT <- calculate_deaccumulation( # Este es el que uso en general 
      initial_funds = 100000000,
      gender = "male",
      life_table = TM_list[["M_T"]],
      start_age = 60,
      retirement_year = 2017,
      interest_rate = 0.03
    )
  }
  
  # Plot de las desacumulaciones:
  {
    # Combine data for all subcategories
    library(ggplot2)
    library(dplyr)
    library(scales)
    # Add a category column to each data frame
    deaccumulation_results_HA$Category <- "High"
    deaccumulation_results_HB$Category <- "Low"
    deaccumulation_results_HM$Category <- "Medium"
    deaccumulation_results_HT$Category <- "Total"
    
    # Combine the data frames
    combined_data <- bind_rows(deaccumulation_results_HM,deaccumulation_results_HB,deaccumulation_results_HA)
    combined_data <- bind_rows(deaccumulation_results_HT)
    # Plot
    ggplot(combined_data, aes(x = Age, y = Monthly_Pension, color = Category)) +
      geom_line(size = 1) +
      labs(title = "Group payments under scheduled withdrawal",
           x = "Age of survival",
           y = "Proportion of initial balance paid out as monthly benefit ($)",
           caption = "Note: Male, 65 years old, single & no offsprings, funds = 100MM") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            plot.caption = element_text(hjust = 0, size = 8),
            plot.title = element_text(size = 8),
            axis.title.y = element_text(size = 8))
    
    
    
    deaccumulation_results_HT$Age_Group <- cut(deaccumulation_results_HT$Age,
                                               breaks = c(59, 82,89,92, Inf),  # Age intervals # EXPECTATIVAS DE VIDA: HOMBRES: 80, 85, 88 MUJERES: 82,89,92
                                               labels = c("Low", "Mid", "High", "Tail"),  # Labels for the intervals
                                               right = TRUE)
   
    likely_death_ages <- data.frame(
      Age = c(86, 93, 95),  # Most likely age of death for Low, Mid, and High categories, Male: 81, 88, 91, Female: 86, 93, 95
      Age_Group = c("Low", "Mid", "High")  # Categories corresponding to the ages
    )
    
    # Merge Monthly_Pension values into the likely_death_ages data frame
    likely_death_ages <- merge(likely_death_ages, deaccumulation_results_HT[, c("Age", "Monthly_Pension")], by = "Age")
    
    {
      ggplot(deaccumulation_results_HT, aes(x = Age, y = Monthly_Pension, color = Age_Group)) +
        geom_line(size = 1.5) +
        scale_color_manual(values = c("Low" = "blue", "Mid" = "orange", "High" = "green", "Tail" = "gray")) +
        labs(title = "Withdrawals by Age, survival by group",
             x = "Age",
             y = "Monthly Pension",
             caption = "Note: Female, 60 years old, single & no offsprings, funds = 100MM") +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        theme(legend.title = element_blank(),
              plot.caption = element_text(hjust = 0, size = 10)) 
    }
    
    
    
    # Para la curva de retiros:
    
    ggplot(deaccumulation_results_HT, aes(x = Age, y = Monthly_Pension, color = Age_Group)) +
      geom_line(size = 1.5) +
      scale_color_manual(values = c("Low" = "blue", "Mid" = "orange", "High" = "green", "Tail" = "gray")) +
      labs(title = "Programmed withdrawal by expected age of survival",
           x = "Age",
           y = "Monthly withdrawal",
           caption = "Note: male, 65 years old, single & no offsprings, funds = 100MM, dots represent most likely age of death") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            plot.caption = element_text(hjust = 0, size = 10)) +
      
      # Add circles for the most likely age of death for each category
      geom_point(data = likely_death_ages, aes(x = Age, y = Monthly_Pension), 
                 color = "black", size = 4, shape = 21, fill = "red") +  # Circles for age of death
      
      # Optionally, add labels for the ages
      geom_text(data = likely_death_ages, aes(x = Age, y = Monthly_Pension, label = Age), 
                color = "black", vjust = -1.5)  # Position the labels above the circles
    
    #
    {
      ggplot(deaccumulation_results_HT, aes(x = Age, y = Monthly_Pension, fill = Age_Group)) +
        # Fill the area under the curve with colors based on Age_Group
        geom_area(alpha = 0.6) +  # Fill under the curve with transparency
        scale_fill_manual(values = c("Low" = "blue", "Mid" = "orange", "High" = "green", "Tail" = "gray")) +  # Set fill colors for each category
        geom_line(size = 1.5, color = "black") +  # Plot the line over the filled area
        labs(title = "Programmed Withdrawal Expected Survival",
             x = "Age",
             y = "Monthly Pension",
             caption = "Note: Male, 65 years old, single & no offsprings, funds = 100MM, dots represent most likely age of death") +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        theme(legend.title = element_blank(),
              plot.caption = element_text(hjust = 0, size = 10)) +
        
        # Add circles for the most likely age of death for each category
        geom_point(data = likely_death_ages, aes(x = Age, y = Monthly_Pension, color = Age_Group), 
                   size = 4, shape = 21, fill = "red") +  # Circles for age of death
        
        # Optionally, add labels for the ages
        geom_text(data = likely_death_ages, aes(x = Age, y = Monthly_Pension, label = Age), 
                  color = "black", vjust = -1.5)  # Position the labels above the circles
      
      }
    
    
    
    # Para la curva de herencia:
    {
      # Merge 'likely_death_ages' with the 'deaccumulation_results_HT' to get 'Remaining_Funds' for the likely death ages
      likely_death_ages <- data.frame(
        Age = c(86, 93, 95),  # Most likely age of death for Low, Mid, and High categories
        Age_Group = c("Low", "Mid", "High")  # Categories corresponding to the ages
      )
      
      # Merge to get the Remaining_Funds for those ages
      likely_death_ages <- merge(likely_death_ages, deaccumulation_results_HT[, c("Age", "Remaining_Funds")], by = "Age")
      
      # Plotting the line graph with colored areas for different age ranges
      ggplot(deaccumulation_results_HT, aes(x = Age, y = Remaining_Funds, fill = Age_Group)) +
        # Fill the area under the curve with colors based on Age_Group
        geom_area(alpha = 0.6) +  # Fill under the curve with transparency
        scale_fill_manual(values = c("Low" = "blue", "Mid" = "orange", "High" = "green", "Tail" = "gray")) +  # Set fill colors for each category
        geom_line(size = 1.5, color = "black") +  # Plot the line over the filled area
        labs(title = "Bequests by age of death",
             x = "Age",
             y = "Proportion of funds paid as bequests ($)",
             caption = "Line breaks show LE, circles show most likely age of death") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(legend.title = element_blank(),
              plot.caption = element_text(hjust = 0, size = 10)) +
        
        # Add circles for the most likely age of death for each category
        geom_point(data = likely_death_ages, aes(x = Age, y = Remaining_Funds, color = Age_Group), 
                   size = 4, shape = 21, fill = "red") +  # Circles for age of death
        
        # Optionally, add labels for the ages
        geom_text(data = likely_death_ages, aes(x = Age, y = Remaining_Funds, label = Age), 
                  color = "black", vjust = -1.5)  # Position the labels above the circles
      
      
      
      # Veamos los retiros acumulados:
      deaccumulation_results_HT$Cumulative_Withdrawal <- cumsum(deaccumulation_results_HT$Max_Withdrawal)
      
      # Ahora metamosle un factor de descuento y variable
      discount_rate <- 0.03  # 3%
      pension_start_age_male<- 65 #60 mujer, 65 hombre
      pension_start_age_female<- 60
      deaccumulation_results_HT$t <- deaccumulation_results_HT$Age - pension_start_age_female
      deaccumulation_results_HT$Discounted_Max_Withdrawal <- deaccumulation_results_HT$Max_Withdrawal * (1 + discount_rate)^(-deaccumulation_results_HT$t)
      deaccumulation_results_HT$Discounted_Cumulative_Withdrawal <- cumsum(deaccumulation_results_HT$Discounted_Max_Withdrawal)
      
      # Ahora agreguemos variables de sobrevivencia
      
      # Mujeres:
      {
        end_age <- 110
        female_list <- TM_list[c("M_A", "M_M", "M_B", "M_T")]
        # Get the starting age for females
        pension_start_age_female <- 60
        pension_start_age<- 65
        age_range_female <- pension_start_age_female:end_age
        # Extract lx values for each education category for females and ensure they match the age range
        lx_high_female <- female_list$M_A$lx[age_range_female - min(female_list$M_A$Edad) + 1]
        lx_mid_female <- female_list$M_M$lx[age_range_female - min(female_list$M_M$Edad) + 1]
        lx_low_female <- female_list$M_B$lx[age_range_female - min(female_list$M_B$Edad) + 1]
        
        # Now create the data frame for females with matching lengths
        female_lx <- data.frame(
          Age = age_range_female,
          lx_high = lx_high_female,
          lx_mid = lx_mid_female,
          lx_low = lx_low_female
        )
        
        # Metamosle la operacion para mujeres:
        lx_high_at_start <- female_lx$lx_high[female_lx$Age == pension_start_age]
        lx_mid_at_start <- female_lx$lx_mid[female_lx$Age == pension_start_age]
        lx_low_at_start <- female_lx$lx_low[female_lx$Age == pension_start_age]
        
        female_lx$lx_high_ratio <- female_lx$lx_high / lx_high_at_start
        female_lx$lx_mid_ratio <- female_lx$lx_mid / lx_mid_at_start
        female_lx$lx_low_ratio <- female_lx$lx_low / lx_low_at_start
        
      }
      
      # En este caso es para hombres: 
      {
        male_list <- TM_list[c("H_A", "H_M", "H_B", "H_T")]
        
        # Get the starting age for males
        pension_start_age_male <- 65
        age_range_male <- pension_start_age_male:end_age
        
        # Extract lx values for each education category for males and ensure they match the age range
        lx_high_male <- male_list$H_A$lx[age_range_male - min(male_list$H_A$Edad) + 1]
        lx_mid_male <- male_list$H_M$lx[age_range_male - min(male_list$H_M$Edad) + 1]
        lx_low_male <- male_list$H_B$lx[age_range_male - min(male_list$H_B$Edad) + 1]
        
        # Now create the data frame for males with matching lengths
        male_lx <- data.frame(
          Age = age_range_male,
          lx_high = lx_high_male,
          lx_mid = lx_mid_male,
          lx_low = lx_low_male
        )
        
        # Metemosle la operacion para hombres:
        lx_high_at_start_male <- male_lx$lx_high[male_lx$Age == pension_start_age_male]
        lx_mid_at_start_male <- male_lx$lx_mid[male_lx$Age == pension_start_age_male]
        lx_low_at_start_male <- male_lx$lx_low[male_lx$Age == pension_start_age_male]
        
        # Calculate the lx[t] / lx[pension_start_age] ratio for each education category
        male_lx$lx_high_ratio <- male_lx$lx_high / lx_high_at_start_male
        male_lx$lx_mid_ratio <- male_lx$lx_mid / lx_mid_at_start_male
        male_lx$lx_low_ratio <- male_lx$lx_low / lx_low_at_start_male
      }
      
      # Merge:
      {
        # Si es para hombres:
        
        deaccumulation_results_HT <- merge(deaccumulation_results_HT, male_lx[, c("Age", "lx_high_ratio", "lx_mid_ratio", "lx_low_ratio")], by = "Age", all.x = TRUE)
        
        
        
        # Si es para mujeres:
        deaccumulation_results_HT <- merge(deaccumulation_results_HT, female_lx[, c("Age", "lx_high_ratio", "lx_mid_ratio", "lx_low_ratio")], by = "Age", all.x = TRUE)
      
        
        }
      
        # Operacion sobrevivencia*valor_descontado
      {
        deaccumulation_results_HT$Discounted_Max_Withdrawal_high <- deaccumulation_results_HT$Discounted_Max_Withdrawal * deaccumulation_results_HT$lx_high_ratio
        deaccumulation_results_HT$Discounted_Max_Withdrawal_mid <- deaccumulation_results_HT$Discounted_Max_Withdrawal * deaccumulation_results_HT$lx_mid_ratio
        deaccumulation_results_HT$Discounted_Max_Withdrawal_low <- deaccumulation_results_HT$Discounted_Max_Withdrawal * deaccumulation_results_HT$lx_low_ratio
        
        deaccumulation_results_HT$Cumulative_Discounted_Max_Withdrawal_high <- cumsum(deaccumulation_results_HT$Discounted_Max_Withdrawal_high)
        deaccumulation_results_HT$Cumulative_Discounted_Max_Withdrawal_mid <- cumsum(deaccumulation_results_HT$Discounted_Max_Withdrawal_mid)
        deaccumulation_results_HT$Cumulative_Discounted_Max_Withdrawal_low <- cumsum(deaccumulation_results_HT$Discounted_Max_Withdrawal_low)
        
        # Create the plot with custom y-axis range and number formatting
        ggplot(deaccumulation_results_HT, aes(x = Age)) +
          geom_line(aes(y = Cumulative_Discounted_Max_Withdrawal_high, color = "High"), size = 1) +
          geom_point(aes(y = Cumulative_Discounted_Max_Withdrawal_high, color = "High"), size = 2) +
          geom_line(aes(y = Cumulative_Discounted_Max_Withdrawal_mid, color = "Mid"), size = 1) +
          geom_point(aes(y = Cumulative_Discounted_Max_Withdrawal_mid, color = "Mid"), size = 2) +
          geom_line(aes(y = Cumulative_Discounted_Max_Withdrawal_low, color = "Low"), size = 1) +
          geom_point(aes(y = Cumulative_Discounted_Max_Withdrawal_low, color = "Low"), size = 2) +
          labs(
            title = "Expected present value of cumulative pension payments until each date of death",
            x = "Age",
            y = "Cumulative Discounted Max Withdrawals ($)",
            color = "Group",
            caption = "Note: Discount rate equal to the RP interest rate."
          ) +
          scale_y_continuous(
            limits = c(25000000, 100000000),  # Set y-axis range from 25 million to 100 million
            labels = scales::comma  # Format numbers with commas
          ) +
          scale_color_manual(
            values = c("High" = "blue", "Mid" = "green", "Low" = "red"),  # Assign colors to the groups
            breaks = c("High", "Mid", "Low")  # Specify the order of the legend
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 8)  # Adjust the title size (smaller than default)
          )
        
        max_values_df <- data.frame(
          Category = c("High", "Mid", "Low"),
          Max_Value = c(
            max(deaccumulation_results_HT$Cumulative_Discounted_Max_Withdrawal_high),
            max(deaccumulation_results_HT$Cumulative_Discounted_Max_Withdrawal_mid),
            max(deaccumulation_results_HT$Cumulative_Discounted_Max_Withdrawal_low)
          )
        )
        
        # Set the order of the categories to be High, Mid, Low
        max_values_df$Category <- factor(max_values_df$Category, levels = c("High", "Mid", "Low"))
        
        
        ggplot(max_values_df, aes(x = Category, y = Max_Value, fill = Category)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = comma(Max_Value)), vjust = -0.5, size = 3) +  # Add data labels above the bars
          labs(title = "Male Expected Discounted Withdrawals by group",
               x = "Category",
               y = "Expected Discounted Value of Cumulative Withdrawals",
               caption = "Note: Discount rate equal to the RP interest rate, same initial funds.") +
          scale_fill_manual(values = c("High" = "blue", "Mid" = "green", "Low" = "red")) +
          scale_y_continuous(labels = comma, limits = c(0, 90000000)) +  # Rescale y-axis to a maximum of 100 million
          theme_minimal() +
          theme(
            plot.title = element_text(size = 9),  # Adjust the title size (smaller than default)
            axis.title.y = element_text(size = 10)  # Adjust the y-axis title size
          )
        
        
        ######################### Ahora agregando la edad mas probable de muerte:
        # Create likely_death_ages for males (you can adjust for females similarly if needed)
        likely_death_ages <- data.frame(
          Age = c(86, 93, 95),  # Most likely age of death for Low, Mid, and High categories (adjust as per your data)
          Age_Group = c("Low", "Mid", "High")  # Categories corresponding to the ages
        )
        
        # Merge Monthly_Pension values into the likely_death_ages data frame
        likely_death_ages <- merge(likely_death_ages, deaccumulation_results_HT[, c("Age", "Cumulative_Discounted_Max_Withdrawal_high", "Cumulative_Discounted_Max_Withdrawal_mid", "Cumulative_Discounted_Max_Withdrawal_low")], by = "Age")
        
        # Now create the plot and add the circles for the most likely age of death
        ggplot(deaccumulation_results_HT, aes(x = Age)) +
          geom_line(aes(y = Cumulative_Discounted_Max_Withdrawal_high, color = "High"), size = 1) +
          geom_point(aes(y = Cumulative_Discounted_Max_Withdrawal_high, color = "High"), size = 2) +
          geom_line(aes(y = Cumulative_Discounted_Max_Withdrawal_mid, color = "Mid"), size = 1) +
          geom_point(aes(y = Cumulative_Discounted_Max_Withdrawal_mid, color = "Mid"), size = 2) +
          geom_line(aes(y = Cumulative_Discounted_Max_Withdrawal_low, color = "Low"), size = 1) +
          geom_point(aes(y = Cumulative_Discounted_Max_Withdrawal_low, color = "Low"), size = 2) +
          labs(
            title = "Actuarial Present Value of Withdrawals by Groups",
            x = "Age",
            y = "Cumulative Discounted Max Withdrawals ($)",
            color = "Group",
            caption = "Note: Discount rate equal to the RP interest rate."
          ) +
          scale_y_continuous(
            limits = c(25000000, 100000000),  # Set y-axis range from 25 million to 100 million
            labels = scales::comma  # Format numbers with commas
          ) +
          scale_color_manual(
            values = c("High" = "blue", "Mid" = "green", "Low" = "red"),  # Assign colors to the groups
            breaks = c("High", "Mid", "Low")  # Specify the order of the legend
          ) +
          theme_minimal()
        
        # Merge likely_death_ages with Remaining_Funds
        likely_death_ages <- merge(likely_death_ages, deaccumulation_results_HT[, c("Age", "Remaining_Funds")], by = "Age")
        
        ggplot(deaccumulation_results_HT, aes(x = Age)) +
          # Plot each line for the different Age Groups with different colors
          geom_line(aes(y = Remaining_Funds, color = Age_Group), size = 1.5) +  # Line for remaining funds
          
          # Customize the colors for the different Age Groups
          scale_color_manual(values = c("Low" = "blue", "Mid" = "orange", "High" = "green", "Tail" = "gray")) +  # Set line colors
          
          labs(title = "Proportion of initial accumulation paid as a bequest",
               x = "Age of death",
               y = "Remaining Funds ($)",
               caption = "Note: Female, same initial funds") +
          
          # Format the y-axis with commas for thousands
          scale_y_continuous(labels = scales::comma) +
          
          # Theme settings
          theme_minimal() +
          theme(
            plot.title = element_text(size = 9),  # Adjust the title size (smaller than default)
            axis.title.y = element_text(size = 10)  # Adjust the y-axis title size
          ) +
          
          # Add circles for the most likely age of death for each category
          geom_point(data = likely_death_ages, aes(x = Age, y = Remaining_Funds, color = Age_Group), 
                     size = 4, shape = 21, fill = "red") +  # Circles for age of death
          
          # Optionally, add labels for the ages
          geom_text(data = likely_death_ages, aes(x = Age, y = Remaining_Funds, label = Age), 
                    color = "black", vjust = -1.5)  # Position the labels above the circles
        
        
        
        
        
      }
      
      
      }
      
    
    
    
    
    # Diferencias entre niveles:
    {
      min_rows <- min(
        nrow(deaccumulation_results_HA),
        nrow(deaccumulation_results_HB),
        nrow(deaccumulation_results_HM),
        nrow(deaccumulation_results_HT)
      )
      monthly_diff_df <- data.frame(
        Age = deaccumulation_results_HT$Age[1:min_rows],
        HA_Diff = deaccumulation_results_HA$Monthly_Pension[1:min_rows] - deaccumulation_results_HT$Monthly_Pension[1:min_rows],
        HB_Diff = deaccumulation_results_HB$Monthly_Pension[1:min_rows] - deaccumulation_results_HT$Monthly_Pension[1:min_rows],
        HM_Diff = deaccumulation_results_HM$Monthly_Pension[1:min_rows] - deaccumulation_results_HT$Monthly_Pension[1:min_rows]
      )
      ggplot(monthly_diff_df, aes(x = Age)) +
        geom_line(aes(y = HA_Diff, color = "High - Total")) +
        geom_line(aes(y = HB_Diff, color = "Low - Total")) +
        geom_line(aes(y = HM_Diff, color = "Medium - Total")) +
        geom_vline(xintercept = c(80.71, 85.91, 89.32), linetype = "dashed", color = "red") + # Add vertical dashed red lines on age + e(65)
        labs(
          title = "Difference in Monthly Payments due to mortality",
          x = "Age",
          y = " Monthly Payment Difference ($)",
          color = "Category",
          caption = "Note: red vertical lines represent e(65) for each SES, negative values imply a tax on high mortality"
        ) +
        scale_y_continuous(labels = scales::comma) + # Format y-axis with commas for thousands
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, size = 10)
        )
      # Valor presente de los flujos de RP
      {
        calculate_present_value <- function(difference_column, discount_rate) {
          age_range <- 65:110  # Age range
          years_since_65 <- age_range - 65  # Time in years since age 65
          
          # Calculate the present value for each year
          PV <- difference_column / (1 + discount_rate)^years_since_65
          
          # Return the total present value
          sum(PV, na.rm = TRUE)
        }
        
        # Define discount rate
        discount_rate <- 0.03  # 3%
        
        # Calculate PV for each SES category
        pv_HA <- calculate_present_value(monthly_diff_df$HA_Diff, discount_rate)
        pv_HB <- calculate_present_value(monthly_diff_df$HB_Diff, discount_rate)
        pv_HM <- calculate_present_value(monthly_diff_df$HM_Diff, discount_rate)
        
        # Display the results
        cat("Present Value of Differences (3% Discount Rate):\n")
        cat("High SES (HA):", pv_HA, "\n")
        cat("Low SES (HB):", pv_HB, "\n")
        cat("Medium SES (HM):", pv_HM, "\n")
        }
      
      
      
      
      
      }
    
  }
  

  
}


# Armar sum J:
{
  J_values_df <- data.frame(SES_Level = character(), J_Value = numeric(), stringsAsFactors = FALSE) # Para guardar valores
  life_table <- TM_list[["H_B"]] # este es el que muevo para SES
  adjusted_qx_df_male <- adjust_qx_with_improvements(gender = "male", retirement_year = 2017, life_table = life_table, start_age = 65)
  adjusted_qx_df_male_with_lx <- calculate_lx(adjusted_qx_df_male)
  
  calculate_J <- function(lx_df, start_age) {
    # Extract the lx value at the starting age
    lx_start <- lx_df$lx[lx_df$Edad == start_age]
    
    # Calculate the sum of lx+t / lx_start for each age in the data frame
    J <- sum(lx_df$lx / lx_start)
    
    return(J)
  }
  
  J_value <- calculate_J(adjusted_qx_df_male_with_lx, start_age = 65)
  print(J_value)
  
  J_values_df <- rbind(J_values_df, data.frame(SES_Level = "HB", J_Value = J_value))
  
  # Ensure the order of SES_Level factor levels
  J_values_df$SES_Level <- factor(J_values_df$SES_Level, levels = c("HA", "HM", "HB"))
  
  # Create the bar chart with the correct order
  ggplot(J_values_df, aes(x = SES_Level, y = J_Value, fill = SES_Level)) +
    geom_bar(stat = "identity", width = 0.6) +
    labs(x = "SES Level", y = "J Value", title = "J Values by SES Level") +
    theme_minimal() +
    theme(legend.position = "none") + 
    scale_y_continuous(labels = scales::comma)
  
  # Check PV de diferentes tasas y periodos
  {
    
    # Load necessary library for data manipulation (optional but recommended)
    library(dplyr)
    
    # Function to calculate present value for a range of discount rates
    calculate_present_value <- function(payment, periods, discount_rates) {
      
      # Step 1: Create a list of equal payments
      payments <- rep(payment, periods + 1)  # Including period 0
      
      # Step 2: Create a sequence of periods
      periods_seq <- seq(0, periods, by = 1)
      
      # Step 3: Initialize an empty data frame to store results
      pv_df <- data.frame(Period = periods_seq, Payment = payments)
      
      # Step 4: Iterate over each discount rate and calculate PV
      for (r in discount_rates) {
        # Initialize a vector to store PVs for the current discount rate
        pv_values <- numeric(length = periods + 1)
        
        # Calculate PV for each period using a for loop
        for (t in periods_seq) {
          pv_values[t + 1] <- payment / (1 + r)^t
        }
        
        # Add the PV values as a new column in the data frame
        # Column name reflects the discount rate (e.g., "PV_0.0003" for 3%)
        pv_column_name <- paste0("PV_", sprintf("%.4f", r))
        pv_df[[pv_column_name]] <- pv_values
      }
      
      # Step 5: Return the data frame with all calculations
      return(pv_df)
    }
    # Define inputs
    payment <- 1  # Example payment amount
    periods <- 26     # Example number of periods
    discount_rates <- c(0.06,0.0668,0.0702,0.0779)  # Example range of discount rates
    
    # Calculate present values
    pv_results <- calculate_present_value(payment, periods, discount_rates)
    
    # Display the results
    print(pv_results)
    
    # Define the range of periods to sum
    start_period <- 0
    end_period <- 26
    
    # Calculate the sum of PVs for each discount rate within the specified range
    sum_pv <- pv_results %>%
      filter(Period >= start_period & Period <= end_period) %>%
      summarise(across(starts_with("PV_"), sum))
    
    print(sum_pv)
   
}

}




