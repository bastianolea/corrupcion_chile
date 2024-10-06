library(dplyr)
library(ggplot2)
library(showtext)
library(lubridate)
library(stringr)
library(glue)
library(gt)
library(colorspace)
library(ggtext)

# datos ----
corrupcion <- readRDS("app/corrupcion_datos.rds")
# cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

options(scipen = 99999)

# colores
color_derecha = "#294a66" |> lighten(0.4)
color_izquierda = "#722a2a" |> lighten(0.4)
color_centro = "white" #"#D3B556" |> lighten(0.2)
# color_derecha = "#294a66" |> lighten(0.25)
# color_izquierda = "#722a2a" |> lighten(0.2)
color_neutro = "black" |> lighten(0.5)
color_ninguno = "#662953"|> lighten(0.4)
color_fundaciones = "#D39552" #"#c88d2c" |> lighten(0.2)


# casos sector ----
datos_casos_sector <- corrupcion |> 
  filter(año >= 2014) |> 
  filter(sector != "Ninguno") |> 
  count(sector) |> 
  mutate(p = n/sum(n)) |> 
  mutate(sector = as.factor(sector))

torta_casos_sector <- datos_casos_sector |> 
  ggplot(aes(x = n, y = factor(1), fill = sector)) +
  geom_col(width = 1) +
  geom_text(aes(label = sector), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5, fontface = "bold", color = "white") + 
  geom_text(aes(label = n, y = 0.3, color = sector), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0, fontface = "bold") + 
  scale_y_discrete(guide = "none", name = NULL) +
  guides(fill = "none", color = "none") +
  coord_radial(expand = FALSE, rotate_angle = TRUE, theta = "x",
               start = 1.24, inner.radius = 0.4) +
  scale_fill_manual(values = c("Derecha" = color_derecha, 
                               "Izquierda" = color_izquierda,
                               "Centro" = color_neutro), aesthetics = c("fill", "color")) +
  theme_void() +
  labs(title = "Casos de corrupción en Chile", 
       subtitle = "Casos según sector político, de 2014 a 2024",
       caption = "Fuente: Visualizador de datos de corrupción: https://bastianoleah.shinyapps.io/corrupcion_chile\nDatos disponibles en https://github.com/bastianolea/corrupcion_chile") +
  theme(plot.title = element_text(margin = margin(t = 6, l = 10, b = 6)),
        plot.subtitle = element_text(margin = margin(l= 10, b =-20)),
        plot.caption = element_text(lineheight = 1.2, margin = margin(t = -10, r = 6, b = 6)))

# guardar
torta_casos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_sector_", today(), ".png"),
       width = 6, height = 6)

torta_casos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_sector.png"),
         width = 6, height = 6)



# montos sector ----
datos_montos_sector <- corrupcion |> 
  filter(año >= 2014) |> 
  filter(sector != "Ninguno") |> 
  # filter(responsable != "Virginia Reginato") |> 
  summarize(n = sum(monto, na.rm = TRUE), .by = sector) |> 
  mutate(p = n/sum(n),
         n = n/1000000) |> 
  mutate(sector = as.factor(sector))

torta_montos_sector <- datos_montos_sector |> 
  mutate(cifra = n |> round(digits = 0) |> signif(4) |> format(big.mark = ".", trim = T) |> paste0("\n", "millones")) |> 
  ggplot(aes(x = n, y = factor(1), fill = sector)) +
  geom_col(width = 1, color = "white", linewidth = 0) +
  geom_text(aes(label = sector), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5, size = 3.6, fontface = "bold", color = "white") + 
  geom_text(aes(label = ifelse(sector == "Derecha", cifra, ""), 
                y = 2.1, color = sector), lineheight = 0.9, position = position_stack(vjust = 0.5), angle = 90, fontface = "bold") + 
  geom_text(aes(label = ifelse(sector != "Derecha", cifra, ""), 
                y = 1.7, color = sector), lineheight = 0.9, position = position_stack(vjust = 0.5), hjust = 0, angle = 90, fontface = "bold") + 
  scale_y_discrete(guide = "none", name = NULL) +
  guides(fill = "none", color = "none") +
  coord_radial(expand = FALSE, rotate_angle = TRUE, theta = "x",
               start = 1.66, inner.radius = 0.4) +
  scale_fill_manual(values = c("Derecha" = color_derecha, 
                               "Izquierda" = color_izquierda,
                               "Centro" = color_neutro), aesthetics = c("fill", "color")) +
  theme_void() +
  labs(title = "Casos de corrupción en Chile", 
       subtitle = "Montos totales defraudados según sector político, de 2014 a 2024",
       caption = "Fuente: Visualizador de datos de corrupción: https://bastianoleah.shinyapps.io/corrupcion_chile\nDatos disponibles en https://github.com/bastianolea/corrupcion_chile") +
  theme(plot.title = element_text(margin = margin(t = 6, l = 10, b = 6)),
        plot.subtitle = element_text(margin = margin(l= 10, b =-20)),
        plot.caption = element_text(lineheight = 1.2, margin = margin(t = -10, r = 6, b = 6)))

# guardar
torta_montos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_montos_sector_", today(), ".png"),
       width = 6, height = 6)

torta_montos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_montos_sector.png"),
         width = 6, height = 6)
