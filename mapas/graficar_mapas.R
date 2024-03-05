


#graficar ----
color_oscuro = "grey20"
color_fondo = "white"
color_negro = "black"
color_agua = "lightblue"
color_estructuras = "grey40"


estructuras$osm_polygons |> 
  filter(!is.na(`addr:city`))

# mapa_2 <- chilemapas::mapa_comunas

# st_crs(estructuras$osm_polygons)
# st_crs(chilemapas::mapa_comunas)
# st_crs(mapa_2$geometry) <- 4326

# mapa_3 <- st_transform(mapa_2$geometry, crs = 4326)

#solo el borde de la región
region <- chilemapas::mapa_comunas |> 
  filter(codigo_region == 13) |> 
  pull(geometry) |> 
  st_transform(crs = 4326) |>
  # st_cast("POLYGON") |> 
  st_union()

#recortar shapes para que solo sean las de dentro de la región
estructuras_region <- st_intersection(estructuras$osm_polygons, region)
rios_region <- st_intersection(rios$osm_lines, region)
calles_region <- st_intersection(calles_principales$osm_lines, region)

# areas_pobladas <- st_read("mapas/Areas_Pobladas/")


# areas_pobladas$geometry

# st_crs(areas_pobladas$geometry) <- 4326
# areas_pobladas_2 <- areas_pobladas |> st_transform(crs = 4326)

# areas_pobladas |> st_simplify()

# areas_pobladas_region <- st_intersection(areas_pobladas_2$geometry, region)



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








poblacion_censo <- chilemapas::censo_2017_comunas |> 
  group_by(codigo_comuna) |> 
  summarize(poblacion = sum(poblacion))

#sacar comunas chicas
mapa_filtrado <- mapa |> 
  left_join(poblacion_censo) |> 
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

mapa_filtrado_urbano <- st_intersection(st_as_sf(mapa_filtrado), 
                                        st_as_sf(mapa_urbano) |> st_union())
# calles_urbano <- st_intersection(st_as_sf(calles_region) |> st_transform(crs = 4326), 
#                                  st_as_sf(mapa_urbano) |> st_union() |> st_transform(crs = 4326))

ggplot() +
  # geom_sf(data = mapa,
  #         aes(geometry = geometry),
  #         fill = color_oscuro, col = color_fondo,
  #         alpha = 0.6) +
  #mapa regional
  geom_sf(data = mapa_filtrado_urbano,
          aes(geometry = geometry),
          fill = color_oscuro, col = color_fondo,
          alpha = 0.6) +
  geom_text(data = mapa_filtrado_urbano,
            aes(geometry = geometry, label = nombre_comuna),
            color = "white", alpha = 1, size = 3,
            stat = "sf_coordinates") +
  # #carreteras
  # geom_sf(data = calles_urbano |> filter(highway %in% c("motorway")),
  #         color = color_negro, size = .1, alpha = 0.4) +
  #urbano
  # geom_sf(data = mapa_urbano,
  #         aes(geometry = geometry),
  #         fill = color_oscuro, color = color_oscuro, alpha = 1) +
  # #carreteras
  # geom_sf(data = calles_region |> filter(highway %in% c("motorway")),
  #         color = color_negro, size = .1, alpha = 0.4) +
  # geom_sf(data = calles_region |> filter(highway %in% c("primary")),
#         color = color_negro, size = .1, alpha = 0.2) +
# # estructruras
# geom_sf(data = estructuras_region,
#         fill = color_estructuras, color = NA, alpha = 0) +
#borde comunas
# geom_sf(data = mapa, 
#         aes(geometry = geometry),
#         fill = NA, col = color_fondo, alpha = 1, linewidth = .3) +
#temas
theme_void() +
  theme(plot.background = element_rect(fill = color_fondo, color = color_fondo), panel.background = element_rect(fill = color_fondo, color = color_fondo))

