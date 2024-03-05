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
# corrupcion_escalados <- readRDS("app/corrupcion_datos_escalados.rds")


read.csv2("datos/comunas_chile_cut.csv")