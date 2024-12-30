
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
  
  
