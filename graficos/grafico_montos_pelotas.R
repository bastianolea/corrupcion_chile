library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(forcats)
library(glue)
library(colorspace)

source("app/colores.R")
color_derecha = "#294a66" |> lighten(0.25)
color_izquierda = "#722a2a" |> lighten(0.2)
color_neutro = "black" |> lighten(0.5)
color_ninguno = "#662953"|> lighten(0.3)

color_fundaciones = "#c88d2c"

options(scipen = 99999)


#datos ----
corrupcion <- readRDS("app/corrupcion_datos.rds") |> 
  mutate(alcaldes_sector = case_when(alcalde == "Alcaldías" & sector == "Derecha" ~ "Alcalde de derecha",
                                     alcalde == "Alcaldías" & sector == "Izquierda" ~ "Alcalde de izquierda",
                                     alcalde == "Alcaldías" & sector == "Ninguno" ~ "Alcalde independiente",
                                     .default = "Otros casos"))
# corrupcion_escalados <- readRDS("app/corrupcion_datos_escalados.rds")

# escalar montos ----


# dividir montos grandes ----

cantidad_circulos = 50 #15
corte_enormes = 30000 #millones
corte_ultra = 50000
divisor_montos_enormes = 3 #cantidad de columnas en montos enormes

corrupcion_dividido <- corrupcion |>
  arrange(desc(monto)) |> 
  #identificar si los montos de los mayores casos son enormes en comparación con el resto
  select(caso, monto) |> 
  mutate(magnitud = case_when(monto >= corte_ultra*1000000 ~ "ultra",
                              monto >= corte_enormes*1000000 ~ "enorme",
                              .default = "normal")) |> 
  #dividir los montos grandes en tres partes iguales, los normales dejarlos como están
  group_by(caso) |> 
  # mutate(monto1 = ifelse(magnitud == "enorme", monto/divisor_montos_enormes, monto),
  #        monto2 = ifelse(magnitud == "enorme", monto/divisor_montos_enormes, NA),
  #        monto3 = ifelse(magnitud == "enorme", monto/divisor_montos_enormes, NA),
  # ) |>
  mutate(monto1 = ifelse(magnitud == "enorme", (monto/10)*2, monto),
         monto2 = ifelse(magnitud == "enorme", (monto/10)*3, NA),
         monto3 = ifelse(magnitud == "enorme", (monto/10)*5, NA),
  ) |>
  mutate(monto1 = ifelse(magnitud == "ultra", (monto/10)*1, monto1),
         monto2 = ifelse(magnitud == "ultra", (monto/10)*1.5, monto2),
         monto3 = ifelse(magnitud == "ultra", (monto/10)*2, monto3),
         monto4 = ifelse(magnitud == "ultra", (monto/10)*2.5, NA),
         monto5 = ifelse(magnitud == "ultra", (monto/10)*3, NA),
  ) |>
  # mutate(monto1 = ifelse(magnitud == "ultra", (monto/10)*(0.357*1), monto1),
  #        monto2 = ifelse(magnitud == "ultra", (monto/10)*(0.357*2), monto2),
  #        monto3 = ifelse(magnitud == "ultra", (monto/10)*(0.357*3), monto3),
  #        monto4 = ifelse(magnitud == "ultra", (monto/10)*(0.357*4), NA),
  #        monto5 = ifelse(magnitud == "ultra", (monto/10)*(0.357*5), NA),
  #        monto6 = ifelse(magnitud == "ultra", (monto/10)*(0.357*6), NA),
  #        monto7 = ifelse(magnitud == "ultra", (monto/10)*(0.357*7), NA),
  # ) |>
  select(-monto) |> 
  pivot_longer(cols = starts_with("monto"), names_to = "division_monto", values_to = "monto_dividido") |> 
  filter(!is.na(monto_dividido))

#escalar datos de 1 a 15, donde el monto maximo es 15
corrupcion_dividido_escalado <- corrupcion_dividido |> 
  ungroup() |> 
  mutate(x = monto_dividido,
         monto_escalado = (x - min(x)) / (max(x) - min(x)) * cantidad_circulos,
         monto_escalado = ceiling(monto_escalado)) |> 
  select(-x)

#el gráfico de pictogramas se basa en que una imagen se repita representando varias partes del valor total,
#por lo tanto hay que hacer una "escalera" de datos hasta llegar al valor escalado, 
#es decir, si el valor es 4, hay qe crear filas que sean 4, 3, 2, 1.
corrupcion_dividido_escalado_escalera <- corrupcion_dividido_escalado |> 
  #repetir las filas en base a el valor escalado; es decir, si el monto escalado es 4, crear 4 filas iguales
  tidyr::uncount(monto_escalado, .remove = FALSE) |> 
  #hacer que las filas iguales sean una sucesión de numeros decreciente; es decir, convertir 4 4 4 4 en 4 3 2 1
  group_by(caso, division_monto) |> 
  mutate(posicion = 1:n(), #hacer un 1 2 3 4 en cada grupo
         posicion = posicion - 1, #hacer un 0 1 2 3 en cada grupo
         monto_escalera = monto_escalado - posicion) |> 
  ungroup() |> 
  select(-posicion)

corrupcion_escalado_0 <- corrupcion_dividido_escalado_escalera |> 
  #volver a incluir el monto original de cada caso como una nueva columna
  left_join(corrupcion |> 
              select(caso, monto, sector, año, partido, perjudicado, alcalde, alcaldes_sector, caso_fundaciones), 
            join_by(caso)) |> 
  #crear una variable que sirva como etiqueta (porque los montos son muy largos)
  mutate(#caso_etiqueta = str_wrap(caso, 40),
    caso_etiqueta = fct_reorder(caso, monto_escalera)) |> 
  #como los valores grandes se van a dividir en 3 columnas, no queremos tener 3 etiquetas iguales, por lo que hacemos que solo sea visible la del medio, y la ordenamos para que quede en el medio
  mutate(division_monto_n = str_remove(division_monto, "monto") |> as.integer()) |> 
  mutate(caso_central = case_when(magnitud == "ultra" & division_monto == "monto3" ~ as.character(caso_etiqueta),
                                  magnitud == "enorme" & division_monto == "monto2" ~ as.character(caso_etiqueta),
                                  magnitud == "normal" ~ as.character(caso_etiqueta), 
                                  .default = "")) |> 
  mutate(division_monto_etiqueta = paste(caso_central, division_monto_n),
         division_monto_etiqueta = fct_reorder(division_monto_etiqueta, division_monto_n)) |> 
  #convertir monto total a una etiqueta
  mutate(monto_etiqueta = as.integer(monto/1000000),
         monto_etiqueta = format(monto_etiqueta, big.mark = ".", decimal.mark = ","),
         monto_etiqueta = paste(monto_etiqueta, "millones")) |> 
  ungroup() |> 
  mutate(ninguno = "ninguno") #variable falsa para desagrupar todos los casos en el gráfico



# variables del gráfico ----
corrupcion_escalado <- function() {
  corrupcion_escalados_años <- corrupcion_escalado_0 |> 
    filter(!is.na(año)) |> 
    filter(año >= 2014) |> 
    filter(sector != "Ninguno") |> 
    arrange(desc(monto))
  
  #filtrar top de casos
  casos <- unique(corrupcion_escalados_años$caso)
  
  corrupcion_escalado_filtrado <- corrupcion_escalados_años |>
    filter(caso %in% casos[1:30])
  
  return(corrupcion_escalado_filtrado)
}


alto_grafico_montos <- function(){
  casos = length(unique(corrupcion_escalado()$caso))
  # message("casos en gráfico montos: ", casos)
  # alto = 58 * casos
  alto = 60 * casos
  message("gráfico montos: alto gráfico montos: ", alto)
  return(alto)
}

# alto_grafico_montos()

#etiquetas de texto para los millones 
monto_etiqueta <- function() {
  corrupcion_escalado() |> 
    group_by(caso) |> 
    filter(monto_escalera == monto_escalado) |> 
    filter(nchar(as.character(division_monto_etiqueta)) > 3) |> 
    select(caso, monto, monto_etiqueta, monto_escalera, division_monto_etiqueta) |> 
    mutate(monto_etiqueta = str_trim(monto_etiqueta)
           # monto_etiqueta = ifelse(monto >= 10000000000,
           #                         str_wrap(monto_etiqueta, 6),
           #                         monto_etiqueta)
    )
}


## gráfico ----
variables_posibles = c("ninguno", "sector", "caso_fundaciones", "alcaldes_sector")

for (variable_color in variables_posibles) {
  # variable_color = "ninguno"
  # # variable_color = "sector"
  # # variable_color = "caso_fundaciones"
  # # variable_color = "alcaldes_sector"
  
  ### opciones
  opt_texto_geom = 3
  opt_texto_plot = 16
  opt_texto_axis = 13
  expansion_y = 0.17 #espacio entre borde de cada faceta de cada caso y sus valores (si fuera aditivo y no multiplicatvo se arregla espaciado distinto entre enormes y normales?
  expansion_x = 0.05 #espacio entre valor máximo y borde derecho del gráfico
  espaciado_y = 5 #espacio entre facetas
  texto_eje_y = 9 #texto casos
  texto_montos = 3 #texto montos
  tamaño_punto = 6 #tamaño de círculos
  corte_etiqueta_casos = 45 #caracteres antes del corte de línea de etiquetas y
  espaciado_etiquetas_x = 0.005 #espacio entre etiquetas eje y y puntos
  espaciado_etiquetas_millones = 0.7 #espacio entre ultimo punto y etiqueta de montos
  
  color_texto = "black"
  color_detalle = "white"
  
  
  grafico <- corrupcion_escalado() |> 
    ggplot(aes(x = monto_escalera, y = division_monto_etiqueta,
               color = .data[[variable_color]])) + 
    #puntos
    geom_point(size = tamaño_punto) +
    #signo peso en puntos
    geom_text(aes(label = "$"), color = color_detalle, size = tamaño_punto*0.6, vjust = 0.48) +
    #etiquetas millones
    geom_text(data = monto_etiqueta(),
              aes(label = monto_etiqueta, x = monto_escalera+espaciado_etiquetas_millones), 
              color = color_texto, size = texto_montos, hjust = 0, lineheight = 0.85) +
    #escala vertical
    scale_y_discrete(labels = ~str_remove(.x, " \\d+") |> str_wrap(corte_etiqueta_casos), #cortar línea de etiquetas eje y
                     # labels = ~str_remove(.x, " \\d+") |> str_replace(" \\(", "\\\n\\("), #cortar línea de etiquetas eje y}
                     # labels = ~str_remove(.x, " \\d+"), #cortar línea de etiquetas eje y
                     expand = expansion(expansion_y)) + #apretar columnas horizontales de puntos
    scale_x_continuous(expand = expansion(c(espaciado_etiquetas_x, expansion_x))) +
    coord_cartesian(clip = "off") +
    facet_grid(rows = vars(caso),
               scales = "free_y", space = 'free', switch = "y", as.table = FALSE) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_text(face = "bold", lineheight = 1,
                                     size = texto_eje_y, color = color_texto,
                                     margin = margin(r = 7)),
          axis.text.x = element_blank(),
          panel.background = element_blank(), #element_rect(fill = color_detalle, linewidth = 0),
          panel.grid = element_blank(),
          panel.spacing.y = unit(espaciado_y, "mm"),
          legend.text = element_text(color = color_texto, size = 18, hjust = 0, margin = margin(t = 0, b = 0, r = 10)),
          plot.margin = unit(c(0.1, 0, 0,  0.1), "cm")) +
    theme(legend.position = c(.9, .1), legend.direction = "vertical", 
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 10, margin = margin(l = 3))) +
    labs(title = "Casos de corrupción más grandes de Chile",
         caption = glue("Fuentes disponibles en https://github.com/bastianolea/corrupcion_chile (Última actualización de datos: {format(today(), '%d/%m/%Y')})")) +
    theme(plot.title.position = "plot", 
          plot.title = element_text(margin = margin(t=2, b = 4)),
          plot.subtitle = element_text(margin = margin(b = 8))
    )
  
  #sin escala si no se divide el gráfico por colores
  if (variable_color == "ninguno") {
    grafico <- grafico +
      scale_color_manual(values = color_destacado) +
      theme(legend.position = "none")
  }
  
  #color si es por sector político
  if (variable_color == "sector") {
    grafico <- grafico +
      scale_color_manual(values = c("Derecha" = color_derecha, "Izquierda" = color_izquierda,
                                    "Centro" = color_neutro,
                                    "Ninguno" = color_neutro)) +
      labs(color = "Sector político") +
      guides(colour = guide_legend(override.aes = list(size = 5))) +
      labs(subtitle = glue("Casos según sector político y ordenados por monto, de 2014 a 2024"))
  }
  
  
  #color si es por fundaciones
  if (variable_color == "caso_fundaciones") {
    grafico <- grafico +
      scale_color_manual(values = c("Caso fundaciones" = color_fundaciones,
                                    "Otros casos" = color_neutro)) +
      labs(color = "Caso fundaciones") +
      guides(colour = guide_legend(override.aes = list(size = 5))) +
      labs(subtitle = glue("Según correspondan a fundaciones u otros casos, y ordenados por monto"))
  }
  
  
  #color si es por alcaldes
  if (variable_color == "alcaldes_sector") {
    grafico <- grafico +
      scale_color_manual(values = c("Alcalde de derecha" = color_derecha,
                                    "Alcalde de izquierda" = color_izquierda,
                                    "Alcalde independiente" = color_ninguno,
                                    "Otros casos" = color_neutro)) +
      labs(color = "Alcaldías") +
      guides(colour = guide_legend(override.aes = list(size = 5))) +
      labs(subtitle = glue("Según correspondan a alcaldías u otros casos, y ordenados por monto"))
  }
  
  plot(grafico)
  
  ## guardar ----
  grafico |> 
    ggsave(filename = paste0("graficos/grafico_corrupcion_montos_", variable_color, "_", today(), ".png"),
         width = 15, height = 11)
  
  grafico |> 
    ggsave(filename = paste0("graficos/grafico_corrupcion_montos_", variable_color, ".png"),
         width = 15, height = 11)
}


#revisar ----

# #circulos por caso
# corrupcion_escalado_0 |> 
#   select(caso, monto_escalado) |> 
#   distinct() |> 
#   group_by(caso) |>
#   summarize(pelotas = sum(monto_escalado)) |> 
#   print(n=Inf)
# 
# 
# corrupcion_escalado_0 |> 
#   select(-monto_escalera) |> 
#   distinct() |> 
#   select(1:6) |> 
#   #cada columna dividida por sus pelotitas
#   mutate(monto2 = monto_dividido/monto_escalado) |> 
#   group_by(caso) |> 
#   #sumar todas las pelotitas, a ver si da lo mismo que da el monto total
#   summarize(monto_sumado = sum(monto_dividido),
#             monto = mean(monto))
#   
# 
# # conteos ----
# corrupcion |> 
#   filter(año >= 2014) |> 
#   count(caso_fundaciones) |> 
#   mutate(p = n/sum(n)*100)
# 
# corrupcion |> 
#   filter(año >= 2014) |> 
#   count(caso_fundaciones, sector) |> 
#   filter(caso_fundaciones != "Otros casos") |> 
#   mutate(p = n/sum(n)*100)
# 
# corrupcion |> 
#   filter(año >= 2014) |> 
#   count(alcaldes_sector) |> 
#   filter(alcaldes_sector != "Otros casos") |> 
#   mutate(p = n/sum(n)*100)
# 
# corrupcion |> 
#   filter(año >= 2014) |> 
#   count(sector) |> 
#   mutate(p = n/sum(n)*100)
# 
# # De los casos de corrupción de los últimos 10 años,
# # del total, 62% han sido desde partidos de derecha, y 19% corresponden a fundaciones.
# # Entre los casos de fundaciones, 40% son de izquierda y 20% de derecha.
# # Entre los casos de alcaldes y municipios corruptos, 81% de ellos han sido de derecha.
# 
# corrupcion |> 
#   filter(año >= 2014) |> 
#   group_by(sector) |> 
#   summarize(n = sum(monto)) |> 
#   mutate(p = n/sum(n)*100)
# 
# # Si sumamos todos los montos de casos de corrupción, obtenemos que 5% son atribuibles a la izquierda, y 66% a la derecha
# 
