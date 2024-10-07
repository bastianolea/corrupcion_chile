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


# datos ----
datos_municipios <- corrupcion_municipios |> 
  filter(sector %in% c("Izquierda", "Derecha", "Centro")) |> 
  mutate(monto = monto/1000000) |> 
  mutate(sector = factor(sector, c("Izquierda", "Derecha", "Centro"))) |> 
  arrange(desc(monto))


# generar dos tablas, una para todo chile y otra para la región metropolitana
for (opcion in c("chile", "rm")) {
  
  if (opcion == "chile") {
    titulo = "Corrupción en municipalidades"
    subtitulo = glue("Casos de corrupción en municipios de Chile, ordenados por monto, con datos de afiliación política.
                             
                                _Última actualización:_ {format(today(), '%d/%m/%Y')}")
    
  } else if (opcion == "rm") {
    titulo = "Corrupción en municipios de la Región Metropolitana"
    subtitulo = glue("Lista de casos de corrupción en municipalidades de la Región Metropolitana, ordenados por monto, con datos de afiliación política.

                                  _Última actualización:_ {format(today(), '%d/%m/%Y')}")
  }
  
  # tabla ----
  if (opcion == "rm") {
    # filtrar rm
    datos_municipios <- datos_municipios |> 
      filter(region == "Metropolitana de Santiago")
  }
  
  tabla_municipios <- datos_municipios |> 
    select(comuna, responsable, monto, sector, partido, año) |> 
    gt() |> 
    tab_header(titulo,
               subtitle = md(subtitulo)) |> 
    #alineación columnas
    cols_align(columns = where(is.numeric), align = "left") |> 
    cols_align(columns = comuna, align = "right") |> 
    cols_align(columns = partido, align = "center") |> 
    cols_align(columns = sector, align = "center") |> 
    #estilos
    tab_style(locations = cells_body(columns = responsable), 
              style = cell_text(weight = "bold")) |> 
    tab_style(locations = cells_column_labels(),
              style = cell_text(weight = "bold")) |> 
    tab_style(locations = cells_body(columns = comuna),
              style = cell_text(style = "italic")) |> 
    #colorizar datos
    data_color(columns = c(sector), 
               method = "factor", domain = c("Derecha", "Izquierda", "Centro"), ordered = T, 
               levels = c("Derecha", "Izquierda", "Centro"),
               palette = c("Derecha" = color_derecha, "Izquierda" = color_izquierda, "Centro" = "white")) |> 
    # sin información
    tab_style(style = cell_text(color = "grey80"),
              locations = cells_body(columns = monto, 
                                     rows = is.na(monto))) |> 
    sub_missing(
      columns = monto,
      rows = everything(),
      missing_text = "Sin datos"
    ) |> 
    #formatear números
    fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
    #nombres de columnas
    cols_label(
      comuna = "Municipio",
      año = "Año",
      partido = "Partido",
      sector = "Sector político",
      responsable = "Alcalde/responsable",
      monto = "Monto (millones)"
    ) |> 
    tab_source_note("Fuente: Visualizador de datos de corrupción, en https://github.com/bastianolea/corrupcion_chile") |> 
    tab_options(table.border.top.color = "white", table.border.bottom.color = "white")
  
  tabla_municipios
  
  
  # guardar ----
  
  #guardar tabla como imagen
  tabla_municipios |> 
    gtsave(filename = paste0("tablas/tabla_corrupcion_municipalidades_", opcion, "_", today(), ".png"),
           vwidth = 840)
  
  tabla_municipios |> 
    gtsave(filename = paste0("tablas/tabla_corrupcion_municipalidades_", opcion, ".png"),
           vwidth = 840)
}
# #tabla solo rm y guardarla
# tabla_municipios_rm <- tabla_municipios |> 
#   tab_header("Corrupción en municipios de la Región Metropolitana",
#              md(glue("Lista de casos de corrupción en municipalidades de la Región Metropolitana, ordenados por monto, con datos de afiliación política.
#                              
#                                 _Última actualización:_ {format(today(), '%d/%m/%Y')}")))
# 
# tabla_municipios_rm |> 
#   gtsave(filename = paste0("tablas/tabla_corrupcion_municipalidades_rm_", today(), ".png"))
# 
# tabla_municipios_rm |> 
#   gtsave(filename = paste0("tablas/tabla_corrupcion_municipalidades_rm.png"))


# conteos ----

corrupcion_municipios |> count(partido) |> arrange(desc(n))
corrupcion_municipios |> count(sector) |> arrange(desc(n)) |> mutate(p = n/sum(n)*100)



corrupcion_municipios |>
  filter(region == "Metropolitana de Santiago") |>
  count(partido) |> arrange(desc(n))

corrupcion_municipios |>
  filter(region == "Metropolitana de Santiago") |>
  count(sector) |> arrange(desc(n)) |> 
  mutate(p = n/sum(n))

corrupcion_municipios |>
  filter(region == "Metropolitana de Santiago") |>
  filter(sector == "Derecha") |>
  summarize(sum(monto))


# textos ----

porcentaje_muni_rm_derecha <- corrupcion_municipios |>
  filter(region == "Metropolitana de Santiago") |>
  count(sector) |> arrange(desc(n)) |> 
  mutate(p = n/sum(n)*100) |> 
  filter(sector == "Derecha") |> 
  pull(p) |> round(1)



casos_muni_udi <- corrupcion_municipios |> 
  count(partido) |> filter(partido == "UDI") |> pull(n)

casos_muni_rn <- corrupcion_municipios |> 
  count(partido) |> filter(partido == "RN") |> pull(n)

porcentaje_muni_derecha <- corrupcion_municipios |> count(sector) |> 
  mutate(p = n/sum(n)*100) |> filter(sector == "Derecha") |> pull(p) |> round(1)

glue("En casos de municipalidades, la derecha lidera con {casos_muni_udi} municipios UDI y {casos_muni_rn} municipios RN. Un {porcentaje_muni_derecha}% de los casos de corrupción municipal en el país son de derecha.")

glue("En la Región Metropolitana, el {porcentaje_muni_rm_derecha}% de las municipalidades con casos de corrupción son de derecha.")
