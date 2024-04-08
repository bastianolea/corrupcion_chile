library(dplyr)
library(ggplot2)
library(showtext)
library(lubridate)
library(stringr)
library(glue)
library(gt)
library(colorspace)

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
color_neutro = "black" |> lighten(0.4)
color_ninguno = "#662953"|> lighten(0.4)
color_fundaciones = "#D39552" #"#c88d2c" |> lighten(0.2)

color_na = "grey80"

titulo = "Casos de corrupción en Chile"
subtitulo = glue("Lista de casos de corrupción donde estén implicados partidos políticos, ordenados por monto, de 2014 a 2024
                             
                                _Última actualización de datos:_ {format(today(), '%d/%m/%Y')}")

# datos ----
datos <- corrupcion |> 
  filter(año >= 2014) |> 
  filter(sector != "Ninguno") |>
  mutate(tipo = case_when(alcalde == "Alcaldías" ~ "Municipalidades",
                          caso_fundaciones == "Caso fundaciones" ~ "Fundaciones", .default = "Otros casos")) |> 
  mutate(caso = if_else(alcalde == "Alcaldías", str_remove(caso, " \\(.*\\)"), caso)) |>
  mutate(responsable = if_else(is.na(responsable), "No individualizado", responsable)) |>
  mutate(comuna = if_else(is.na(comuna), "No aplica", comuna)) |>
  mutate(delitos = if_else(is.na(delitos), "Sin información", delitos)) |>
  mutate(partido = if_else(partido == "Independiente", "Ind.", partido)) |>
  mutate(monto = monto/1000000) |> 
  mutate(sector = factor(sector, c("Izquierda", "Derecha", "Centro", "Ninguno"))) |> 
  select(caso, responsable, monto, sector, partido, tipo, comuna, año, delitos) |> 
  arrange(desc(monto)) |> 
  print(n = Inf)

# tabla ----
tabla <- datos |> 
  # slice(1:26) |>
  # slice(27:99) |>
  gt() |> 
  tab_header(titulo, subtitle = md(subtitulo)) |> 
  #alineación columnas
  cols_align(columns = c(partido, sector, año), align = "center") |>
  #estilos
  tab_style(locations = cells_body(columns = caso), style = cell_text(weight = "bold")) |>
  tab_style(locations = cells_column_labels(), style = cell_text(weight = "bold")) |> 
  tab_style(locations = cells_body(columns = monto), style = "padding-right: 10px") |> 
  tab_style(locations = cells_body(columns = comuna), style = "padding-left: 10px") |> 
  #colorizar datos
  data_color(columns = c(sector), 
             method = "factor", domain = c("Derecha", "Izquierda", "Centro", "Ninguno"), ordered = T, 
             levels = c("Derecha", "Izquierda", "Centro", "Ninguno"),
             palette = c("Derecha" = color_derecha, "Izquierda" = color_izquierda, "Centro" = color_centro, "Ninguno" = "white")) |> 
  data_color(columns = c(tipo), 
             method = "factor", domain = c("Municipalidades", "Fundaciones", "Otros casos"), ordered = T, 
             levels = c("Municipalidades", "Fundaciones", "Otros casos"),
             palette = c("Municipalidades" = color_ninguno, "Fundaciones" = color_fundaciones, "Otros casos" = "white")) |> 
  #color de casos sin información
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = comuna, rows = comuna == "No aplica")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = responsable, rows = responsable == "No individualizado")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = sector, rows = sector == "Ninguno")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = partido, rows = partido == "Ninguno")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = delitos, rows = delitos == "Sin información")) |> 
  #formatear números
  fmt_number(columns = monto, sep_mark = ".", pattern = "${x}", decimals = 0) |> 
  #nombres de columnas
  cols_label(
    caso = "Caso de corrupción",
    comuna = "Comuna",
    año = "Año",
    partido = "Partido",
    sector = "Sector político",
    tipo = "Tipo de caso",
    responsable = "Responsable",
    monto = "Monto (millones)",
    delitos = "Delitos"
  ) |> 
  tab_source_note(
    html("<b>Fuentes:</b> Visualizador de datos de corrupción: <u>https://bastianoleah.shinyapps.io/corrupcion_chile</u> <br>
                       Puedes encontrar los datos y fuentes de prensa en: <u>https://github.com/bastianolea/corrupcion_chile")) |> 
  tab_options(table.border.top.color = "white",
              table.border.bottom.color = "white"); print(tabla)

# guardar ----

#guardar tabla como imagen
tabla |> gtsave(filename = "tablas/tabla_corrupcion_partidos_chile.png")
# tabla |> gtsave(filename = "tablas/tabla_corrupcion_partidos_chile_b.png")

# #tabla solo rm y guardarla
# tabla |> 
#   tab_header("Corrupción en municipios de la Región Metropolitana",
#              md(glue("Lista de casos de corrupción en municipalidades de la Región Metropolitana, ordenados por monto, con datos de afiliación política.
#                              
#                                 _Última actualización:_ {format(today(), '%d/%m/%Y')}"))) |> 
#   gtsave(filename = "tablas/tabla_corrupcion_municipalidades_rm.png")


# conteos ----
datos |> count(partido)
datos |> count(sector) |> mutate(p = n/sum(n))
