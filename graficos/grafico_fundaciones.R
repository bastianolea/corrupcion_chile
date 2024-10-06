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
corrupcion <- readRDS("app/corrupcion_datos.rds")


datos_fundaciones <- corrupcion |> 
  filter(año >= 2010) |> 
  filter(!is.na(fundacion)) |> 
  filter(!is.na(monto)) |> 
  mutate(sector = if_else(sector %in% c("Derecha", "Izquierda"), sector, "Otros")) |> 
  mutate(caso = str_remove(caso, "Caso Convenios\\:|Caso Convenios\\,|Caso Convenios"),
         caso = str_remove(caso, "\\("),
         caso = str_remove(caso, "\\)"),
         caso = str_trim(caso),
         caso = gsub("^([a-z])", "\\U\\1", caso, perl=TRUE) #primera letra a mayúscula
         ) |> 
  mutate(caso = fct_reorder(caso, monto))


# gráfico sector ----
datos_fundaciones |> 
  ggplot(aes(y = caso, x = monto, fill = sector)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(monto,
                                      prefix = " ", suffix = " mill.", big.mark = ".", decimal.mark = ",", 
                                      scale = 1e-6)),
            hjust = 0) +
  escala_barras_horizontales +
  escala_y_barras_horizontales +
  tema_barras_horizontales +
  scale_fill_manual(values = c("Derecha" = color_derecha, 
                               "Izquierda" = color_izquierda,
                               "Otros" = color_destacado)) +
  scale_x_continuous(n.breaks = 6, expand = expansion(c(0, 0.3)),
                     labels = scales::unit_format(unit = "mill.", big.mark = ".", decimal.mark = ",", 
                                                  scale = 1e-6)) +
  scale_y_discrete(labels = ~str_trunc(as.character(.x), 60, ellipsis = "…")) +
  labs(fill = "Sector político", y = "Fundaciones según su sector político") +
  theme(axis.title.y = element_text(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.position = "top",
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.margin = margin(b = -6), 
        legend.text = element_text(margin = margin(l = 2, r = 4))
  )


