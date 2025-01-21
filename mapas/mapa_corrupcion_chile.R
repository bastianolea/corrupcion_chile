library(dplyr)
library(ggplot2)
library(sf)
library(showtext)
library(lubridate)
library(stringr)
library(glue)
library(gt)
library(colorspace)
library(ggtext)
library(scales)
library(ggiraph)

# datos ----
corrupcion <- readRDS("app/corrupcion_datos.rds")
# cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

options(scipen = 99999)

source("app/colores.R")


corrupcion_comunas <- corrupcion |> 
  filter(!is.na(comuna))

corrupcion_comunas_conteo <- corrupcion_comunas |> 
  group_by(comuna) |> 
  summarize(n = n(),
            casos = list(caso),
            responsables = list(responsable),
            montos = list(monto),
            monto = sum(monto),
            delitos = list(delitos),
            años = list(año)
  )


# mapas ----
mapa_pais <- chilemapas::mapa_comunas %>%
  left_join(chilemapas::codigos_territoriales %>%
              select(matches("comuna"), matches("region"))) |> 
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.2, keep_shapes = TRUE))

mapa_region <- mapa_pais |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry)) |> 
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.5, keep_shapes = TRUE))

# readr::write_rds(mapa_pais,
#                  "mapas/mapa_pais.rds")
# 
# readr::write_rds(mapa_region,
#                  "mapas/mapa_region_2.rds")



# mapa_pais |> 
#   ggplot(aes(geometry = geometry)) +
#   geom_sf() +
#   coord_sf(xlim = c(-76, -66))
# 
# mapa_region |> 
#   ggplot(aes(geometry = geometry)) +
#   geom_sf() +
#   coord_sf(xlim = c(-76, -66))


# unión datos con mapas ----

corrupcion_comunas_conteo_join <- corrupcion_comunas_conteo |> 
  mutate(comuna = case_match(comuna, 
                             "Puerto Natales" ~ "Natales",
                             "Atacama" ~ "Copiapó",
                             "Biobío" ~ "Concepción",
                             "La Araucanía" ~ "Temuco",
                             .default = comuna)) |> 
  mutate(comuna_match = tolower(comuna),
         comuna_match = stringi::stri_trans_general(comuna_match, "latin-ascii"))


mapa_pais_join <- mapa_pais |>
  mutate(comuna_match = tolower(nombre_comuna))


## datos para el gráfico ----
corrupcion_comunas_mapa <- left_join(corrupcion_comunas_conteo_join,
                                     mapa_pais_join,
                                     by = "comuna_match") |> 
  mutate(punto = geometry |> st_simplify() |> st_centroid(of_largest_polygon = TRUE)) |> 
  # etiquetas
  rowwise() |> 
  mutate(monto = ifelse(is.na(monto), 0, monto),
         caso_t = ifelse(n == 1, "caso", "casos"),
         casos_t = glue("{paste0(unlist(casos), collapse = ';\n')}"),
         monto_t = scales::comma(monto, big.mark = ".", suffix = " millones", scale = 1e-6, accuracy = 1),
         monto_t = ifelse(monto == 0, "Sin información", monto_t)) |> 
  # mutate(etiqueta = glue("**{comuna}:** {n} {caso_t}\n\n**Monto total:** {monto_t}\n\n**Casos:** {casos_t}"))
  mutate(etiqueta = glue("{comuna}: {n} {caso_t}\nMonto total: {monto_t}\n\n{casos_t}"))


corrupcion_comunas_mapa |> glimpse()


mapa <- corrupcion_comunas_mapa |>
  # mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON")) |> 
  ggplot() +
  aes(text = etiqueta) +
  geom_sf(data = mapa_region |> 
            mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON")), aes(geometry = geometry),
          fill = color_barras,
          color = color_fondo2, alpha = 0.6,
          inherit.aes = F) +
  # puntos
  geom_sf(aes(geometry = punto,
              size = monto, alpha = monto,
              text = etiqueta),
          color = color_complementario) +
  # borde de puntos
  geom_sf(aes(geometry = punto, size = monto), shape = 1, color = color_fondo2, alpha = .4) +
  coord_sf(xlim = c(-76, -66)) +
  scale_size_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                    range = c(3, 13),
                    labels = scales::label_comma(scale = 1e-6, suffix = " millones", big.mark = ".", decimal.mark = ","))+
  scale_alpha_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                     range = c(1, .6)) +
  guides(alpha = guide_none(),
         size = guide_none()) +
  # tema_corrupcion +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

# cortar zonas ----
# calcular coordenadas de corte para dividir chile en 3
limite_sup = 17.5 # coordenada y del extremo norte de chile
limite_inf = 56.1 # coordenada y del extremo sur de chile
rango = limite_inf-limite_sup # cantidad de grados entre norte y sur
partes = 3
alto_y = rango/partes # altura que debiera tener cada una de las partes del mapa

inicios_y <- c(limite_sup, limite_sup + alto_y, limite_sup + alto_y*2)
finales_y <- inicios_y + alto_y

# norte
mapa_norte <- mapa +
  coord_sf(xlim = c(-76, -66),
           ylim = c(-inicios_y[1], -finales_y[1]),
           expand = F)
# centro
mapa_centro <- mapa +
  coord_sf(xlim = c(-76, -66),
           ylim = c(-inicios_y[2], -finales_y[2]), 
           expand = F)

# # rm
# mapa_rm_zoom <- mapa +
#   # geom_sf(data = mapa_pais, aes(geometry = geometry), fill = NA, alpha = .1) +
#   coord_sf(xlim = c(-72.1, -69.7),
#            ylim = c(-32.7, -34.3), 
#            expand = F)

# sur
mapa_sur <- mapa +
  coord_sf(xlim = c(-76, -66),
           ylim = c(-inicios_y[3], -finales_y[3]), 
           expand = F)



# región metropolitana ----

mapa_filtrado_urbano_2 <- mapa_filtrado_urbano |> 
  mutate(comuna_match = tolower(nombre_comuna)) |> 
  mutate(geometry = st_simplify(geometry) |> st_cast('MULTIPOLYGON')) |> 
  # simplificar mapa
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = 0.5, keep_shapes = TRUE))

# readr::write_rds(mapa_filtrado_urbano_2,
#                  "mapas/mapa_rm_urbano_2.rds")



corrupcion_comunas_rm_mapa <- corrupcion_comunas_conteo_join |> 
  unnest(c(montos, responsables, casos, delitos, años)) |> 
  left_join(mapa_filtrado_urbano_2,
            by = "comuna_match") |> 
  mutate(punto = geometry |> st_simplify() |> st_centroid(of_largest_polygon = TRUE),
         punto_jitter = punto |> st_jitter(amount = 0.015)) |> 
  filter(!is.na(codigo_comuna)) |> 
  # corrupcion_comunas_rm_mapa |> select(1:7) |> 
  rowwise() |> 
  mutate(monto_t = scales::comma(monto, big.mark = ".", suffix = " millones", scale = 1e-6, accuracy = 1),
         etiqueta = case_when(is.na(delitos) ~ markdown(glue("**{comuna}**: {casos} ({años})\n\n{monto_t}")),
                              !is.na(delitos) ~ markdown(glue("**{comuna}**: {casos} ({años})\n\n{monto_t}\n\n_{delitos}_")))
  )


mapa_rm <- corrupcion_comunas_rm_mapa |> 
  filter(!is.na(montos)) |> 
  ggplot(aes(geometry = geometry)) +
  # fondo
  geom_sf(data = mapa_filtrado_urbano_2,
          aes(geometry = geometry),
          color = color_fondo) +
  # puntos
  geom_sf_interactive(aes(geometry = punto_jitter, 
                          size = montos, alpha = montos,
                          data_id = casos, 
                          tooltip = etiqueta)) +
  # geom_sf(aes(geometry = punto_jitter, size = montos, alpha = montos)) +
  coord_sf(xlim = c(-70.81, -70.44213), ylim = c(-33.66, -33.31), expand = F) +
  scale_size_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                    range = c(4, 15),
                    labels = scales::label_comma(scale = 1e-6, suffix = " millones", big.mark = "."))+
  scale_alpha_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                     range = c(.6, .4)) +
  guides(alpha = guide_none(),
         size = guide_none())




# interactivos ----

girafear <- function(objeto) {
  girafe(ggobj = objeto,
         options = list(opts_hover(css = paste0("fill: ", color_destacado, ";")),
                        opts_tooltip(opacity = 0.8, css = paste0("background-color: ", color_fondo, "; color: ", color_texto, "; padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")),
                        opts_sizing(rescale = TRUE), opts_toolbar(hidden = "selection", saveaspng = FALSE)))
}

mapa_norte |> girafear()

mapa_centro |> girafear()

mapa_sur |> girafear()

mapa_rm |> girafear()


## plotly ----
# install.packages("plotly")
library(plotly)

mapa_norte +
  geom_text(aes(label = texto)) |> 
  plotly::ggplotly(tooltip = c("text"))

plotly::ggplotly()



corrupcion_comunas_mapa |>
  ungroup() |> 
  # mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON")) |> 
  # mutate(etiqueta = as.character(etiqueta)) |> 
  ggplot() +
  geom_sf(data = mapa_region |> 
            mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON")), 
          aes(geometry = geometry),
          fill = color_barras,
          color = color_fondo2, alpha = 0.6,
          inherit.aes = F) +
  aes(text = etiqueta) +
  # puntos
  geom_sf(aes(geometry = punto,
              size = monto, 
              alpha = monto
              ),
          color = color_complementario) +
  # borde de puntos
  # geom_sf(aes(geometry = punto, size = monto), shape = 1, color = color_fondo2, alpha = .4) +
  coord_sf(xlim = c(-76, -66)) +
  scale_size_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                    range = c(3, 13),
                    labels = scales::label_comma(scale = 1e-6, suffix = " millones", big.mark = ".", decimal.mark = ","))+
  scale_alpha_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                     range = c(1, .6)) +
  guides(alpha = guide_none(),
         size = guide_none()) +
  # tema_corrupcion +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) -> mapa2


mapa2

corrupcion_comunas_mapa |> 
  ungroup() |> 
  slice(4) |> pull(etiqueta)


mapa2 |> 
  ggplotly(tooltip = "text") |> 
  style(hoverlabel = list(bgcolor = color_fondo2,
                          bordercolor = color_fondo2,
                          font = list(family = "Monaco", color  = color_texto),
                          align = "left",
                          max_width = 200
  )
  )


mapa_norte |> 
  ggplotly(tooltip = "text") |> 
  style(hoverlabel = list(bgcolor = color_fondo2,
                          bordercolor = color_fondo2,
                          font = list(family = "Monaco", color  = color_texto),
                          align = "left",
                          max_width = 200
  )
  )
