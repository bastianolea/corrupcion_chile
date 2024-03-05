#remotes::install_github("ropensci/osmdata")
library(dplyr)
library(ggplot2)
library(osmdata)
library(chilemapas)
library(ggmap)
library(rvest)
library(sf)

# obtener calles y otros desde OpenStreetMap ----
available_features()

#explorar etiquetas disponibles
available_tags("place")
available_tags("waterway")
available_tags("landuse")

#definir ciudad a obtener
# ciudad = "Santiago"
ciudad = "Región Metropolitana"
getbb("Santiago")
getbb("Región Metropolitana")
?getbb

#obtener calles y carreteras
calles_principales <- getbb(ciudad) %>%
  opq(timeout = 500) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link",
                            "trunk")) %>%
  osmdata_sf()

# calles_secundarias <- getbb(ciudad) %>%
#   opq() %>%
#   add_osm_feature(key = "highway",
#                   value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
#   osmdata_sf()

# rios <- getbb(ciudad) %>%
#   opq(timeout = 500) %>%
#   add_osm_feature(key = "waterway", 
#                         value = c("river", "canal")) %>%
#   osmdata_sf()

estructuras <- getbb(ciudad) %>%
  opq(timeout = 500) %>%
  add_osm_feature(key = "landuse", 
                  value = c("residential", "commercial")) %>%
  osmdata_sf()



# estas no porque son muy chicas
# small_streets <- getbb(ciudad)%>%
#   opq()%>%
#   add_osm_feature(key = "highway", 
#                   value = c("residential", "living_street",
#                             "unclassified",
#                             "service", "footway"
#                   )) %>%
#   osmdata_sf()


#obtener puntos de las comunas
# places <- available_tags("place")
# places |> print(n=Inf)

lugares <- getbb(ciudad) %>%
  opq() %>%
  add_osm_feature(key = "place", 
                  value = c("province", "region", "town", "village", "district", "county", "city")) %>%
  osmdata_sf()

# lugares$osm_points %>% count(place)
# 
# lugares$osm_points %>% filter(place == "town")
# lugares$osm_points$name
# 
# #filtrar puntos (ejemplo)
# # lugares_tarapaca$osm_points %>% filter(name %in% c("Pica", "Huara", "Colchane", "Camiña",
# #                                                    "Alto Hospicio", "Iquique", "Pozo Almonte")) %>%
# #   filter(place != "isolated_dwelling") #porque colchane sale dos veces
# 
# #filtrar lugares relevantes
# lugares_tarapaca$osm_points <- lugares_tarapaca$osm_points %>% 
#   filter(name %in% c("Pica", "Huara", "Colchane", "Camiña",
#                      "Alto Hospicio", "Iquique", "Pozo Almonte")) %>%
#   filter(place != "isolated_dwelling")
# 



# asignar crs
st_crs(calles_principales$osm_lines) <- 4326
# st_crs(rios$osm_lines) <- 4326
st_crs(estructuras$osm_polygons) <- 4326


# obtener mapas de chile ----

#cargar polígono regional
mapa <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))) %>%
  filter(codigo_region=="13")

mapa_urbano <- chilemapas::mapa_zonas |> 
  filter(codigo_region == 13)

#solo el borde de la región
region <- chilemapas::mapa_comunas |> 
  filter(codigo_region == 13) |> 
  pull(geometry) |> 
  st_transform(crs = 4326) |>
  st_union()