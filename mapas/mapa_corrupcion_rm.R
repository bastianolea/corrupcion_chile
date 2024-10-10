library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(forcats)
library(glue)
library(colorspace)
library(ggrepel)
library(sf)

# source("mapas/obtener_mapas.R")

corrupcion <- readRDS("app/corrupcion_datos.rds") |> 
  mutate(alcaldes_sector = case_when(alcalde == "Alcaldías" & sector == "Derecha" ~ "Alcalde de derecha",
                                     alcalde == "Alcaldías" & sector == "Izquierda" ~ "Alcalde de izquierda",
                                     alcalde == "Alcaldías" & sector == "Ninguno" ~ "Alcalde independiente",
                                     .default = "Otros casos"))

cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

calles_principales <- readr::read_rds("mapas/osm_calles_principales.rds")
estructuras <- readr::read_rds("mapas/osm_estructuras.rds")
mapa <- readr::read_rds("mapas/mapa_comunas.rds")
mapa_urbano <- readr::read_rds("mapas/mapa_rm_urbano.rds")
region <- readr::read_rds("mapas/mapa_region.rds")

# colores
color_oscuro = "grey20"
color_fondo = "white"
color_negro = "black"
color_agua = "lightblue"
color_estructuras = "grey40"

color_derecha = "#294a66" |> lighten(0.4)
color_izquierda = "#722a2a" |> lighten(0.4)
color_neutro = "black" |> lighten(0.3)
color_ninguno = "#662953"|> lighten(0.3)



# recortar mapas para que solo sean las de dentro de la región
estructuras_region <- st_intersection(estructuras$osm_polygons, region)
# rios_region <- st_intersection(rios$osm_lines, region)
calles_region <- st_intersection(calles_principales$osm_lines, region)

# areas_pobladas <- st_read("mapas/Areas_Pobladas/")
# areas_pobladas$geometry
# st_crs(areas_pobladas$geometry) <- 4326
# areas_pobladas_2 <- areas_pobladas |> st_transform(crs = 4326)
# areas_pobladas |> st_simplify()
# areas_pobladas_region <- st_intersection(areas_pobladas_2$geometry, region)

# primer mapa ----
ggplot() +
  #mapa regional
  geom_sf(data = mapa, 
          aes(geometry = geometry),
          fill = color_oscuro, col = color_fondo, 
          alpha = 0.2) +
  #urbano
  geom_sf(data = mapa_urbano,
          aes(geometry = geometry),
          fill = color_oscuro, color = color_oscuro, alpha = 0.5) +
  #carreteras
  geom_sf(data = calles_region |> filter(highway %in% c("motorway")),
          color = color_negro, size = .1, alpha = 0.4) +
  geom_sf(data = calles_region |> filter(highway %in% c("primary")),
          color = color_negro, size = .1, alpha = 0.2) +
  # estructruras
  geom_sf(data = estructuras_region,
          fill = color_estructuras, color = NA, alpha = 0) +
  #borde comunas
  geom_sf(data = mapa, 
          aes(geometry = geometry),
          fill = NA, col = color_fondo, alpha = 1, linewidth = .3) +
  #temas
  theme_void() +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo), panel.background = element_rect(fill = color_fondo, color = color_fondo))






# segundo mapa, sólo límite urbano de la RM ----

# poblacion_censo <- chilemapas::censo_2017_comunas |> 
#   group_by(codigo_comuna) |> 
#   summarize(poblacion = sum(poblacion))

# seleccionar solo comunas urbanas 
mapa_filtrado <- mapa |> 
  # left_join(poblacion_censo) |> 
  # filter(poblacion > 100000) |> 
  # filter(!nombre_comuna %in% c("Lampa", "Colina", "Melipilla")) |> 
  filter(nombre_comuna %in% c("Pudahuel", "Cerro Navia", "Conchali", "La Pintana", "El Bosque", 
                              "Estacion Central", "Pedro Aguirre Cerda", "Recoleta", "Independencia", 
                              "La Florida", "Penalolen", "Las Condes", "Lo Barnechea", "Quinta Normal", 
                              "Maipu", "Macul", "Nunoa", "Puente Alto", "Quilicura", "Renca", 
                              "San Bernardo", "San Miguel", "La Granja", "Providencia", "Santiago",
                              "San Joaquin", "Lo Espejo", "La Reina", "San Ramon", "La Cisterna", "Lo Prado", "Cerrillos", "Vitacura", "Huechuraba"
  ))


# mapa_filtrado$nombre_comuna |> dput()
# mapa_filtrado
# st_crs(mapa_filtrado$geometry) <- 4326

# mapa_filtrado_2 <- mapa_filtrado$geometry |> st_transform(crs = 4326)

# transformar mapas
mapa_urbano_2 <- mapa_urbano |> 
  st_as_sf() |> 
  st_union() |> 
  st_transform(crs = 4326)

estructuras_region_2 <- estructuras_region |> 
  st_as_sf() |> 
  st_union() |> 
  st_transform(crs = 4326) |> 
  st_simplify() |> 
  st_make_valid()

sf_use_s2(FALSE)

# filtrar el mapa de comunas urbanas para dejar solo sectores urbanos de esas comunas
mapa_filtrado_urbano <- st_intersection(st_as_sf(mapa_filtrado), 
                                        mapa_urbano |> 
                                          st_as_sf() |> 
                                          st_union())

mapa_filtrado_urbano

# estructuras_urbano <- st_intersection(estructuras_region_2, 
#                                       mapa_urbano |> 
#                                         st_as_sf() |> 
#                                         st_union() |> 
#                                         st_transform(crs = 4326))

# filtrar mapa de estructuras (uso de suelo) para que sea solo dentro del límite urbano
estructuras_urbano <- st_intersection(estructuras_region_2 |> 
                                        st_union(), 
                                      mapa_filtrado_urbano |> 
                                        st_as_sf() |> 
                                        st_union() |>
                                        st_transform(crs = 4326))

# calles_urbano <- st_intersection(st_as_sf(calles_region) |> st_transform(crs = 4326), 
#                                  st_as_sf(mapa_urbano) |> st_union() |> st_transform(crs = 4326))

# filtrar datos de corrupción de municipios
corrupcion_comunas <- corrupcion |> 
  filter(!is.na(comuna)) |> 
  filter(alcalde == "Alcaldías") |> 
  left_join(cut_comunas, by = join_by(comuna)) |> 
  filter(!is.na(cut_comuna)) |> 
  select(sector, alcaldes_sector, año, partido, comuna, cut_comuna) |> 
  mutate(cut_comuna = as.character(cut_comuna)) |> 
  mutate(sector = if_else(is.na(sector), "Ninguno", sector))

# agregar datos de municipios a mapa de sector urbano
mapa_datos <- mapa_filtrado_urbano |> 
  left_join(corrupcion_comunas, by = c("codigo_comuna" = "cut_comuna")) |> 
  mutate(alcaldes_sector = if_else(is.na(alcaldes_sector), "Sin casos conocidos", alcaldes_sector)) |> 
  group_by(comuna) |> 
  slice_max(año) |> 
  mutate(etiqueta = glue("{comuna} ({partido})"),
         etiqueta = if_else(is.na(partido), NA, etiqueta))

# mapa_datos |> 
#   select(comuna, partido, etiqueta) |> 
#   print(n=Inf)


# graficar mapa
mapa_corrupcion_municipios <- mapa_datos |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill = alcaldes_sector),
          col = color_fondo,
          alpha = 1, linewidth = 0.5) +
  geom_sf(data = estructuras_urbano,
          fill = color_estructuras, fill = "black", color = NA, alpha = 0.15) +
  geom_label_repel(aes(label = etiqueta),
                  color = "black", alpha = 0.7, size = 3, fontface = "bold", 
                  label.size = 0, seed = 1000,
                  stat = "sf_coordinates") +
  geom_label_repel(aes(label = etiqueta),
                   color = "black", alpha = 1, size = 3, fontface = "bold", 
                   label.size = 0, seed = 1000,
                   fill = NA,
                   stat = "sf_coordinates") +
  scale_fill_manual(values = c("Alcalde de derecha" = color_derecha,
                               "Alcalde de izquierda" = color_izquierda,
                               "Alcalde independiente" = color_ninguno,
                               "Sin casos conocidos" = "gray70")) +
  labs(fill = "Municipalidades") +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  coord_sf(xlim = c(-70.81, -70.44213), ylim = c(-33.66, -33.31), expand = F) +
  #temas
  theme_void() +
  theme(legend.position = c(.1, .12)) +
  theme(legend.title = element_text(face = "bold"),
        plot.margin = unit(c(5, 5, 5, 15), "mm"),
        plot.background = element_rect(fill = color_fondo, color = color_fondo), 
        panel.background = element_rect(fill = color_fondo, color = color_fondo)) +
  theme(plot.title.position = "plot", 
        plot.title = element_text(margin = margin(t=0, b = 6)),
        plot.subtitle = element_text(margin = margin(b = 8))) +
  labs(title = "Casos de corrupción en municipalidades",
       subtitle = "Región Metropolitana, por sector político del alcalde",
       caption = "Fuentes disponibles en https://github.com/bastianolea/corrupcion_chile")

# guardar ----
mapa_corrupcion_municipios |> 
  ggsave(filename = paste0("mapas/mapa_corrupcion_municipios_rm_", today(), ".png"),
       width = 10, height = 10)

mapa_corrupcion_municipios |> 
  ggsave(filename = paste0("mapas/mapa_corrupcion_municipios_rm.png"),
       width = 10, height = 10)
