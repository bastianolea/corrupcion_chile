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
cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

corrupcion_municipios <- corrupcion |> 
  filter(alcalde == "Alcaldías") |> 
  left_join(cut_comunas)

options(scipen = 99999)

# colores
# color_derecha = "#294a66" |> lighten(0.9)
# color_izquierda = "#722a2a" |> lighten(0.9)
color_derecha = "#294a66" |> lighten(0.4)
color_izquierda = "#722a2a" |> lighten(0.4)

titulo = "Corrupción en municipios"
subtitulo = glue("Lista de casos de corrupción en municipalidades de Chile, ordenados por monto, con datos de afiliación política.
                             
                                _Última actualización:_ {format(today(), '%d/%m/%Y')}")
# tabla ----
tabla <- corrupcion_municipios |> 
  filter(region == "Metropolitana de Santiago") |> 
  select(comuna, responsable, monto, sector, partido, año) |> 
  mutate(monto = monto/1000000) |> 
  mutate(sector = factor(sector, c("Izquierda", "Derecha", "Ninguno"))) |> 
  arrange(desc(monto)) |> 
  #tabla
  gt() |> 
  tab_header(titulo,
             subtitle = md(subtitulo)) |> 
  #alineación columnas
  cols_align(columns = where(is.numeric), align = "left") |> 
  cols_align(columns = comuna, align = "right") |> 
  #estilos
  tab_style(locations = cells_body(columns = responsable), 
            style = cell_text(weight = "bold")) |> 
  tab_style(locations = cells_column_labels(),
            style = cell_text(weight = "bold")) |> 
  tab_style(locations = cells_body(columns = comuna),
            style = cell_text(style = "italic")) |> 
  #colorizar datos
  data_color(columns = c(sector), 
             method = "factor", domain = c("Derecha", "Izquierda", "Ninguno"), ordered = T, 
             levels = c("Derecha", "Izquierda", "Ninguno"),
             palette = c("Derecha" = color_derecha, "Izquierda" = color_izquierda, "Ninguno" = "white")) |> 
  #formatear números
  fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
  #nombres de columnas
  cols_label(
    comuna = "Municipio",
    año = "Año",
    partido = "Partido",
    sector = "Sector político",
    responsable = "Alcalde/alcaldesa",
    monto = "Monto (millones)"
  ) |> 
  tab_source_note("Fuente: Visualizador de datos de corrupción, en https://github.com/bastianolea/corrupcion_chile") |> 
  tab_options(table.border.top.color = "white", table.border.bottom.color = "white"); print(tabla)
 
# guardar ----

#guardar tabla como imagen
tabla |> gtsave(filename = "tablas/tabla_corrupcion_municipalidades_chile.png")

#tabla solo rm y guardarla
tabla |> 
  tab_header("Corrupción en municipios de la Región Metropolitana",
             md(glue("Lista de casos de corrupción en municipalidades de la Región Metropolitana, ordenados por monto, con datos de afiliación política.
                             
                                _Última actualización:_ {format(today(), '%d/%m/%Y')}"))) |> 
gtsave(filename = "tablas/tabla_corrupcion_municipalidades_rm.png")


# conteos ----

corrupcion_municipios |> count(partido) |> arrange(desc(n))
corrupcion_municipios |> count(sector) |> arrange(desc(n))

corrupcion_municipios |> 
  filter(region == "Metropolitana de Santiago") |> 
  count(partido) |> arrange(desc(n))

corrupcion_municipios |> 
  filter(region == "Metropolitana de Santiago") |> 
  count(sector) |> arrange(desc(n))

corrupcion_municipios |> 
  filter(region == "Metropolitana de Santiago") |> 
  filter(sector == "Derecha") |> 
  summarize(sum(monto))
