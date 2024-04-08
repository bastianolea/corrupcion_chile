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
color_neutro = "black" |> lighten(0.3)
color_ninguno = "#662953"|> lighten(0.3)

color_fundaciones = "#c88d2c"

options(scipen = 99999)


#datos ----
corrupcion <- readRDS("app/corrupcion_datos.rds") |> 
  mutate(alcaldes_sector = case_when(alcalde == "Alcaldías" & sector == "Derecha" ~ "Alcalde de derecha",
                                     alcalde == "Alcaldías" & sector == "Izquierda" ~ "Alcalde de izquierda",
                                     alcalde == "Alcaldías" & sector == "Ninguno" ~ "Alcalde independiente",
                                     .default = "Otros casos"))


datos_barras <- corrupcion |> 
  filter(año >= 2010) |> 
  filter(comuna != "Viña del Mar",
         responsable != "Cathy Barriga") |> 
  mutate(sector = if_else(sector %in% c("Derecha", "Izquierda"), sector, "Otros"))

escala_barras_horizontales <- scale_x_continuous(n.breaks = 6, expand = expansion(c(0, 0.01)),
                                                 labels = scales::unit_format(unit = "mill.", big.mark = ".", decimal.mark = ",", scale = 1e-6))

escala_y_barras_horizontales <- scale_y_discrete(labels = ~str_trunc(as.character(.x), 40, ellipsis = "…"))

ancho_barras = 0.5

tema_barras_horizontales <- theme(axis.title = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  panel.grid.major.y = element_blank(),
                                  legend.key.size = unit(3, "mm"),
                                  legend.position = "top",
                                  legend.margin = margin(b = -6), 
                                  legend.text = element_text(margin = margin(l = 2, r = 4))
)


# gráfico sector ----
datos_barras |> 
  ggplot(aes(y = caso, x = monto, fill = sector)) +
  geom_col(width = ancho_barras) +
  # geom_vline(data = datos_barras |> group_by(sector) |> slice_max(monto) |> ungroup() |> select(-sector), 
  #            aes(xintercept = monto)) +
  facet_grid(rows = vars(sector), scales = "free_y", space = "free_y", axes = "all") +
  escala_barras_horizontales +
  escala_y_barras_horizontales +
  tema_barras_horizontales +
  scale_fill_manual(values = c("Derecha" = color_derecha, 
                               "Izquierda" = color_izquierda,
                               "Otros" = color_destacado)) +
  labs(fill = "Sector político")


# gráfico tipo ----
datos_barras |> 
  ggplot(aes(y = caso, x = monto, fill = caso_fundaciones)) +
  geom_col(width = ancho_barras) +
  facet_grid(rows = vars(caso_fundaciones), scales = "free_y", space = "free_y", axes = "all") +
  escala_barras_horizontales +
  escala_y_barras_horizontales +
  tema_barras_horizontales +
  scale_fill_manual(values = c("Caso fundaciones" = color_fundaciones,
                               "Otros casos" = color_destacado)) +
  labs(fill = "Tipo de caso")

# 
# datos_barras |> 
#   mutate(fundaciones_sector = case_when(caso_fundaciones == "Caso fundaciones" ~ "Fundaciones",
#                                         sector == "Derecha" ~ "Derecha",
#                                         # sector == "Izquierda" ~ "Izquierda",
#                                         .default = "Izquierda y otros")) |> 
#   ggplot(aes(y = caso, x = monto, fill = fundaciones_sector)) +
#   geom_col(width = ancho_barras) +
#   facet_grid(rows = vars(fundaciones_sector), scales = "free_y", space = "free_y", axes = "all") +
#   escala_barras_horizontales + 
#   escala_y_barras_horizontales +
#   tema_barras_horizontales +
#   scale_fill_manual(values = c("Derecha" = color_derecha, 
#                                "Fundaciones" = color_fundaciones,
#                                "Izquierda y otros" = color_destacado)) +
#   labs(fill = "Tipo de caso") +
#   theme(strip.text = element_text(hjust = 0, face = "bold"))
