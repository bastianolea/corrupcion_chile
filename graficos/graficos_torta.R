library(dplyr)
library(ggplot2)
library(showtext)
library(lubridate)
library(stringr)
library(glue)
library(gt)
library(colorspace)
library(ggtext)
library(scales)

# datos ----
corrupcion <- readRDS("app/corrupcion_datos.rds")
# cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

options(scipen = 99999)

source("app/colores.R")

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
  ggplot(aes(x = p, y = factor(1), fill = sector)) +
  geom_col(width = 1) +
  geom_text(aes(label = sector), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5, fontface = "bold", color = "white") + 
  geom_text(aes(label = percent(p, accuracy = 1), y = 0.25, color = sector), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5, fontface = "bold") + 
  scale_y_discrete(guide = "none", name = NULL) +
  guides(fill = "none", color = "none") +
  coord_radial(expand = FALSE, rotate.angle = TRUE, theta = "x",
               # start = 0.8, 
               start = 1.2, 
               inner.radius = 0.4) +
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

torta_casos_sector

# guardar
torta_casos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_sector_", today(), ".png"),
       width = 6, height = 6)

torta_casos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_sector.png"),
         width = 6, height = 6)



# casos partido ----
datos_casos_partido <- corrupcion |> 
  filter(año >= 2014) |> 
  count(partido) |>
  tidyr::separate(partido, into = c("partido1", "partido2", "partido3"), sep = ", ") |> 
  tidyr::pivot_longer(cols = starts_with("partido"), values_to = "partido") |> 
  filter(!is.na(partido) & partido != "") |> 
  select(-name) |> 
  tidyr::uncount(weights = n) |> 
  group_by(partido) |> 
  # mutate(n = n()) |>
  summarize(n = n()) |>
  arrange(desc(n)) |> 
  mutate(p = n/sum(n)) |> 
  rowwise() |> 
  # mutate(partido_reduc = if_else(p <= 0.055, "Otros", partido),
  mutate(partido_reduc = if_else(p <= 0.03, "Otros", partido),
         partido_reduc = if_else(partido_reduc == "Independiente", "Ind.", partido_reduc)) |> 
  group_by(partido_reduc) |>
  summarize(n = sum(n),
            p = sum(p)) |> 
  rename(partido = partido_reduc)

torta_casos_partido <- datos_casos_partido |> 
  ggplot(aes(x = p, y = factor(1), fill = partido)) +
  geom_col(width = 1, linewidth = 0.6, color = "white") +
  geom_text(aes(label = partido), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5, fontface = "bold", color = "white") + 
  geom_text(aes(label = percent(p, accuracy = 1), y = 0.25, color = partido), position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5, fontface = "bold") + 
  scale_y_discrete(guide = "none", name = NULL) +
  guides(fill = "none", color = "none") +
  coord_radial(expand = FALSE, rotate.angle = TRUE, theta = "x",
               start = 0.7, 
               inner.radius = 0.4) +
  scale_fill_manual(values = degradado_verde(length(datos_casos_partido$partido)), aesthetics = c("fill", "color")) +
  theme_void() +
  labs(title = "Casos de corrupción en Chile", 
       subtitle = "Casos según partido político, de 2014 a 2024",
       caption = "Fuente: Visualizador de datos de corrupción: https://bastianoleah.shinyapps.io/corrupcion_chile\nDatos disponibles en https://github.com/bastianolea/corrupcion_chile") +
  theme(plot.title = element_text(margin = margin(t = 6, l = 10, b = 6)),
        plot.subtitle = element_text(margin = margin(l= 10, b =-20)),
        plot.caption = element_text(lineheight = 1.2, margin = margin(t = -10, r = 6, b = 6)))

torta_casos_partido

# guardar
torta_casos_partido |> 
  ggsave(filename = paste0("graficos/grafico_torta_partido_", today(), ".png"),
         width = 6, height = 6)

torta_casos_partido |> 
  ggsave(filename = paste0("graficos/grafico_torta_partido.png"),
         width = 6, height = 6)



# montos sector ----
datos_montos_sector <- corrupcion |> 
  filter(año >= 2014) |> 
  filter(sector != "Ninguno") |> 
  # add_row(sector = "Izquierda", monto = 10000000000) |>
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
               # start = 1.3,
               start = 1.62,
               inner.radius = 0.4) +
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


torta_montos_sector

# guardar
torta_montos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_montos_sector_", today(), ".png"),
       width = 6, height = 6)

torta_montos_sector |> 
  ggsave(filename = paste0("graficos/grafico_torta_montos_sector.png"),
         width = 6, height = 6)
