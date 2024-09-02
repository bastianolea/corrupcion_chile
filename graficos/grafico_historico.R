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


datos_suma <- corrupcion |> 
  group_by(año, sector) |> 
  summarize(n = n(),
            monto = sum(monto)) |> 
  group_by(sector) |> 
  mutate(suma = cumsum(n))

datos_suma |> 
  ggplot(aes(x = año, y = suma, fill = sector, color = sector)) +
  geom_point(aes(size = monto)) +
  geom_line()
  # geom_line(aes(año, monto, color = sector),  stat = "smooth", method = "lm", inherit.aes = F)
  # scale_y_continuous(trans = "log10") +
  # coord_cartesian(ylim = c(1000000, 300*1000000))

datos_suma |> 
  ggplot(aes(x = año, y = suma, fill = sector, color = sector)) +
  geom_area(position = position_stack())


datos_suma2 <- datos_suma |> 
  arrange(sector, año, monto) |> 
  complete(año = 1980:2024) |> 
  mutate(n = replace_na(n, 0),
         monto = replace_na(monto, 0)) |> 
  mutate(suma_n = cumsum(n),
         suma_monto = cumsum(monto))

datos_suma2 |> filter(sector == "Izquierda")

datos_suma2 |> 
  filter(sector %in% c("Izquierda", "Derecha")) |> 
  ggplot(aes(x = año, y = suma_n, fill = sector, color = sector)) +
  geom_area(position = position_stack())

datos_suma2 |> 
  filter(sector %in% c("Izquierda", "Derecha")) |> 
  ggplot(aes(x = año, y = suma_n, fill = sector, color = sector)) +
  geom_ribbon(aes(ymin = 0, ymax = suma_n), alpha = .5)

datos_suma2 |> 
  filter(sector %in% c("Izquierda", "Derecha")) |> 
  ggplot(aes(x = año, y = suma_n, fill = sector, color = sector)) +
  geom_step() + 
  theme()

datos_suma2 |> 
  filter(sector %in% c("Izquierda", "Derecha")) |> 
  ggplot(aes(x = año, y = suma_n, fill = sector, color = sector)) +
  geom_point(aes(size = monto)) +
  geom_line()
