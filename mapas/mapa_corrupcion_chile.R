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

# datos ----
corrupcion <- readRDS("app/corrupcion_datos.rds")
# cut_comunas <- read.csv2("datos/comunas_chile_cut.csv")

options(scipen = 99999)

source("app/colores.R")


corrupcion_comunas <- corrupcion |> 
  filter(!is.na(comuna))

corrupcion_comunas_conteo <- corrupcion_comunas |> 
  group_by(comuna) |> 
  summarize(monto = sum(monto),
            n = n(),
            casos = list(caso),
            responsables = list(responsable),
            montos = list(monto)
  )


# mapas ----
mapa_pais <- chilemapas::mapa_comunas %>%
  left_join(chilemapas::codigos_territoriales %>%
              select(matches("comuna"), matches("region")))

mapa_region <- mapa_pais |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry))

mapa_pais |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf() +
  coord_sf(xlim = c(-76, -66))

mapa_region |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf() +
  coord_sf(xlim = c(-76, -66))


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

corrupcion_comunas_mapa <- left_join(corrupcion_comunas_conteo_join,
                                       mapa_pais_join,
                                       by = "comuna_match") |> 
  mutate(punto = geometry |> st_simplify() |> st_centroid(of_largest_polygon = TRUE))


corrupcion_comunas_mapa |> glimpse()


mapa <- corrupcion_comunas_mapa |> 
  ggplot() +
  geom_sf(data = mapa_region, aes(geometry = geometry)) +
  geom_sf(aes(geometry = punto,
              size = monto, alpha = monto)) +
  coord_sf(xlim = c(-76, -66)) +
  scale_size_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                    range = c(3, 13),
                    labels = scales::label_comma(scale = 1e-6, suffix = " millones", big.mark = "."))+
  scale_alpha_binned(breaks = c(0, 100*1e6, 1000*1e6, 10000*1e6, 100000*1e6),
                     range = c(.7, .4)) +
  guides(alpha = guide_none(),
         size = guide_none())
  
# calcular coordenadas de corte para dividir chile en 3
limite_sup = 17.5 # coordenada y del extremo norte de chile
limite_inf = 56.1 # coordenada y del extremo sur de chile
rango = limite_inf-limite_sup # cantidad de grados entre norte y sur
partes = 3
alto_y = rango/partes # altura que debiera tener cada una de las partes del mapa

inicios_y <- c(limite_sup, limite_sup + alto_y, limite_sup + alto_y*2)
finales_y <- inicios_y + alto_y

# norte
mapa +
  coord_sf(xlim = c(-76, -66),
           ylim = c(-inicios_y[1], -finales_y[1]),
           expand = F)
# centro
mapa +
  coord_sf(xlim = c(-76, -66),
           ylim = c(-inicios_y[2], -finales_y[2]), 
           expand = F)

# rm
mapa +
  # geom_sf(data = mapa_pais, aes(geometry = geometry), fill = NA, alpha = .1) +
  coord_sf(xlim = c(-72.1, -69.7),
           ylim = c(-32.7, -34.3), 
           expand = F)

# sur
mapa +
  coord_sf(xlim = c(-76, -66),
           ylim = c(-inicios_y[3], -finales_y[3]), 
           expand = F)



