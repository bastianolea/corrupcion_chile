library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(forcats)
library(ggimage)

options(scipen = 9999)

#cargar ----
#cargar base de datos de casos de corrupción en chile
corrupcion_0 <- read.csv2("casos_corrupcion_chile.csv")

corrupcion <- corrupcion_0 |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  rename(año = ano) |> 
  #excluir casos sin fuentes
  filter(!is.na(fuente1) & fuente1 != "") |> 
  #convertir monto a numérico
  mutate(monto = str_remove_all(monto, "\\."),
         monto = as.numeric(monto)) |> 
  #ordenar casos en base al monto
  mutate(caso = as.factor(caso),
         caso = forcats::fct_reorder(caso, monto)) |> 
  rowwise() |> 
  mutate(partido = ifelse(partido == "" | is.na(partido), "Ninguno", partido),
         caso_fundaciones = ifelse(!is.na(fundacion), "Caso fundaciones", "Otros casos"),
         perjudicado = ifelse(perjudicado == "" | is.na(perjudicado), "Otros", perjudicado),
         )

# readr::write_rds(corrupcion, "app/corrupcion_datos.rds")

# corrupcion_2 <- corrupcion |> 
#   mutate(monto_escalado = monto/1000000) |> 
#   mutate(monto_etiqueta = monto_escalado |> 
#            round() |> 
#            format(big.mark = ".", decimal.mark = ",") |> 
#            str_trim()
#          )
# 
# #casos en barras
# corrupcion_2 |> 
#   filter(año >= 2019) |> 
#   ggplot(aes(x = monto_escalado, y = caso)) +
#   geom_col() +
#   geom_text(aes(label = monto_etiqueta), hjust = -0.2) +
#   scale_x_continuous(labels = ~format(.x, big.mark = ".") |> as.character(), 
#                      expand = expansion(c(0, 0.2))) +
#   labs(x = "Monto en millones de pesos")
# 
# #prueba escalas
# corrupcion_3 <- corrupcion_2 |> 
#   filter(año >= 2020) |> 
#   filter(monto >= 100000000) |> 
#   mutate(monto_p = monto/sum(monto)) |> 
#   select(caso, monto, monto_p) |> 
#   mutate(monto_m = 1+(monto_p*10)*30,
#          monto_m = as.integer(monto_m)) |> 
#   mutate(caso = str_wrap(caso, 20),
#          caso = caso |> as.factor() |> forcats::fct_reorder(monto)) |> 
#   mutate(monto_m_max = max(monto_m),
#          monto_m_max_p = monto_m_max/250) |> #cathy barriga tiene que ser 300 pelotas para que el menor caso sea 1
#   mutate(piezas = monto_m / monto_m_max_p) |> 
#   print()
# 
# 
# corrupcion_3 |> 
#   ggplot(aes(piezas, caso)) + 
#   # geom_image(image = "iconos/billete2.svg", color = "green4", ) +
#   geom_point() +
#   theme_void() +
#   theme(axis.text.y = element_text(hjust = 1))
# 
# #casos en facetas
# corrupcion_2 |> 
#   filter(año >= 2019) |> 
#   ggplot(aes(x = monto_escalado, y = caso)) +
#   geom_col() +
#   geom_text(aes(label = monto_etiqueta), hjust = -0.2) +
#   scale_x_continuous(labels = ~format(.x, big.mark = ".") |> as.character(), 
#                      expand = expansion(c(0, 0.2))) +
#   labs(x = "Monto en millones de pesos") +
#   facet_wrap(~caso, ncol = 1, scales = "free_y")



#prueba dividiendo montos grandes en tres ----
corrupcion_dividido <- corrupcion |> 
  filter(año >= 2019) |> 
  #identificar si los montos de los mayores casos son enormes en comparación con el resto
  select(caso, monto) |> 
  mutate(magnitud = ifelse(monto > mean(monto)*2, "enorme", "normal")) |> 
  #dividir los montos grandes en tres partes iguales, los normales dejarlos como están
  group_by(caso) |> 
  mutate(divisor_montos = 3,
         monto1 = ifelse(magnitud == "enorme", monto/divisor_montos, monto),
         monto2 = ifelse(magnitud == "enorme", monto/divisor_montos, NA),
         monto3 = ifelse(magnitud == "enorme", monto/divisor_montos, NA),
  ) |> 
  select(-monto, -divisor_montos) |> 
  pivot_longer(cols = starts_with("monto"), names_to = "division_monto", values_to = "monto_dividido") |> 
  filter(!is.na(monto_dividido))


# #prueba con multiples columnas para cada caso
# #(el largo de las facetas es distinto entonces aprieta los valores)
# corrupcion_5 |> 
#   ungroup() |> 
#   # slice(1:9) |> 
#   ggplot(aes(x = value, y = name)) + 
#   # geom_image(image = "iconos/billete2.svg", color = "green4", ) +
#   geom_point(size =10) +
#   geom_segment(aes(xend = 0, yend = name), linewidth = 3) +
#   # theme_void() +
#   theme(axis.text.y = element_text(hjust = 1)) +
#   facet_grid(rows = vars(caso),
#              scales = "free_y", 
#              space = 'free', 
#              as.table = FALSE) +
#   theme(strip.text = element_text(hjust = 1),
#         #axis.text.y = element_blank()
#         ) +
#   coord_cartesian(clip = "off")


#escalar datos de 1 a 15, donde el monto maximo es 15
corrupcion_dividido_escalado <- corrupcion_dividido |> 
  ungroup() |> 
  mutate(x = monto_dividido,
         monto_escalado = (x - min(x)) / (max(x) - min(x)) * 15,
         monto_escalado = ceiling(monto_escalado)) |> 
  select(-x)

#el gráfico de pictogramas se basa en que una imagen se repita representando varias partes del valor total,
#por lo tanto hay que hacer una "escalera" de datos hasta llegar al valor escalado, 
#es decir, si el valor es 4, hay qe crear filas que sean 4, 3, 2, 1.
corrupcion_dividido_escalado_escalera <- corrupcion_dividido_escalado |> 
  #repetir las filas en base a el valor escalado; es decir, si el monto escalado es 4, crear 4 filas iguales
  tidyr::uncount(monto_escalado, .remove = FALSE) |> 
  #hacer que las filas iguales sean una sucesión de numeros decreciente; es decir, convertir 4 4 4 4 en 4 3 2 1
  group_by(caso, division_monto) |> 
  mutate(posicion = 1:n(), #hacer un 1 2 3 4 en cada grupo
         posicion = posicion - 1, #hacer un 0 1 2 3 en cada grupo
         monto_escalera = monto_escalado - posicion) |> 
  ungroup() |> 
  select(-posicion) |> 
  #volver a incluir el monto original de cada caso como una nueva columna
  left_join(corrupcion |> select(caso, monto, sector, año, partido), join_by(caso)) |> 
  #crear una variable que sirva como etiqueta (porque los montos son muy largos)
  mutate(caso_etiqueta = str_wrap(caso, 20),
         caso_etiqueta = fct_reorder(caso_etiqueta, monto_escalera)) |> 
  #como los valores grandes se van a dividir en 3 columnas, no queremos tener 3 etiquetas iguales, por lo que hacemos que solo sea visible la del medio, y la ordenamos para que quede en el medio
  mutate(division_monto_n = str_remove(division_monto, "monto") |> as.integer()) |> 
  mutate(caso_central = case_when(magnitud == "enorme" & division_monto == "monto2" ~ as.character(caso),
                                  magnitud == "normal" ~ as.character(caso), .default = "")) |> 
  mutate(division_monto_etiqueta = paste(caso_central, division_monto_n),
         division_monto_etiqueta = fct_reorder(division_monto_etiqueta, division_monto_n)) |> 
  #convertir monto total a una etiqueta
  mutate(monto_etiqueta = as.integer(monto/1000000),
         monto_etiqueta = format(monto_etiqueta, big.mark = ".", decimal.mark = ","),
         monto_etiqueta = paste(monto_etiqueta, "millones"))


# FUNCIONA ----

## prueba 1 ---- 
corrupcion_dividido_escalado_escalera |> 
  ggplot(aes(x = monto_escalera, y = division_monto_etiqueta)) + 
  # geom_image(image = "iconos/billete2.svg", color = "green4", ) +
  geom_point(size = 9) +
  geom_text(data = corrupcion_dividido_escalado_escalera |> 
              group_by(caso) |> 
              filter(monto_escalera == monto_escalado) |> 
              filter(nchar(as.character(division_monto_etiqueta)) > 3),
            aes(label = monto_etiqueta), 
            hjust = 0, nudge_x = 0.7) +
  theme(axis.text.y = element_text(hjust = 1)) +
  scale_y_discrete(labels = ~str_remove(.x, " \\d+") |> str_wrap(20)) +
  scale_x_continuous(expand = expansion(c(0.05, 0.3))) +
  coord_cartesian(clip = "off") +
  facet_grid(rows = vars(caso),
             scales = "free_y", 
             space = 'free', switch = "y",
             as.table = FALSE) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_blank()
        ) +
  theme(axis.text = element_text(size = 10)) +
  theme(plot.margin = unit(c(0, 0, 0, -3.5), "cm"))


pictograma = "iconos/billete4.svg"

corrupcion_dividido_escalado_escalera |> 
  mutate(tamaño = 0.3) |> 
  # slice(1:10) |> 
  ggplot(aes(x = monto_escalera, y = division_monto_etiqueta,
             color = sector)) + 
  # geom_point(size = 10) +
  geom_image(data = corrupcion_dividido_escalado_escalera |> filter(magnitud == "enorme"),
             image = pictograma, size = 0.22) +
  geom_image(data = corrupcion_dividido_escalado_escalera |> filter(magnitud == "normal"),
             image = pictograma, size = 0.6) +
  geom_text(data = corrupcion_dividido_escalado_escalera |> 
              group_by(caso) |> 
              filter(monto_escalera == monto_escalado) |> 
              filter(nchar(as.character(division_monto_etiqueta)) > 3),
            aes(label = monto_etiqueta), 
            hjust = 0, nudge_x = 0.7) +
  theme(axis.text.y = element_text(hjust = 1)) +
  scale_y_discrete(labels = ~str_remove(.x, " \\d+") |> str_wrap(20)) +
  scale_x_continuous(expand = expansion(c(0.05, 0.2))) +
  scale_color_manual(values = c("Izquierda" = "red", "Derecha" = "blue", "Desconocido" = "gray"), na.value = "gray40") +
  facet_grid(rows = vars(caso),
             scales = "free_y", 
             space = 'free', switch = "y",
             as.table = FALSE) +
  theme_minimal() +
  theme(strip.background = element_blank(), strip.text = element_blank(),
        panel.grid = element_blank(), 
        axis.title = element_blank(), axis.text.x = element_blank()) +
  coord_cartesian(clip = "off")


# por años ----
corrupcion |> 
  filter(!is.na(año)) |> 
  ggplot(aes(as.factor(año), monto/1000000, fill = sector)) +
  geom_col(position = position_dodge()) +
  scale_y_log10(labels = ~format(.x, big.mark = ".")) +
  scale_x_discrete()

corrupcion |> 
  filter(!is.na(año)) |> 
  group_by(año) |> 
  count(caso) |> 
  ggplot(aes(as.factor(año), n)) +
  geom_col() +
  scale_y_continuous(breaks = 1:20, expand = expansion(c(0, 0.1)))

#torta sector ----
corrupcion |> 
  count(sector) |>
  # mutate(sector = forcats::fct_inorder(sector)) |>
  arrange(desc(sector)) |> 
  mutate(prop = n / sum(n) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5 * prop) |> 
  ggplot(aes("", prop, fill = sector)) +
  geom_col(width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(x = 1.1, y = ypos, label = sector), color = "white", size = 4) +
  geom_point(aes(x = 1.7, y = ypos, col = sector), size = 12) +
  geom_text(aes(x = 1.7, y = ypos, label = scales::percent(prop/100, accuracy = 1)), 
            color = "black", size = 3) +
  scale_fill_brewer(palette="Set1") +
  theme(legend.position="none")

#partido ----
corrupcion |> 
  count(partido) |>
  separate(partido, into = c("partido1", "partido2", "partido3")) |> 
  tidyr::pivot_longer(cols = starts_with("partido"), values_to = "partido") |> 
  filter(!is.na(partido) & partido != "") |> 
  select(-name) |> 
  tidyr::uncount(weights = n) |> 
  count(partido) |> 
  arrange(desc(partido)) |> 
  mutate(prop = n / sum(n) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5 * prop) |> 
  ggplot(aes("", prop, fill = partido)) +
  geom_col(width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(x = 1.1, y = ypos, label = partido), color = "white", size = 4) +
  geom_point(aes(x = 1.7, y = ypos, col = partido), size = 12) +
  geom_text(aes(x = 1.7, y = ypos, label = scales::percent(prop/100, accuracy = 1)), 
            color = "white", size = 3) +
  scale_fill_brewer(palette="Set1") +
  theme(legend.position="none")



#cep ----
cep <- readr::read_rds("app/cep_corrupcion.rds")
unique(cep$variable)

cep |> 
  filter(variable %in% c("Corrupción")) |> 
  # mutate(fecha = as.Date(fecha)) |> 
  # group_by(ano) |> 
  # summarize(porcentaje = mean(porcentaje)) |> 
  ggplot(aes(as.Date(fecha), y = porcentaje)) +
  geom_errorbar(aes(ymin = porcentaje_ic_inferior, ymax = porcentaje_ic_superior),
                alpha = .2) +
  geom_line(linewidth = 1, alpha = .4) +
  geom_point(size = 4, alpha = .9) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = ~scales::percent(.x, accuracy = 1)) +
  theme(panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(r=6), face = "italic")) +
  labs(y = "porcentaje de encuestados", x = NULL)



#izquierda vs derecha ----
corrupcion

datos <- corrupcion |> 
  select(caso, monto, sector) |> 
  filter(sector %in% c("Izquierda", "Derecha")) |> 
  mutate(monto2 = case_when(sector == "Izquierda" ~ 0-monto, .default = monto)) |> 
  mutate(sector =  factor(sector, levels = c("Izquierda", "Derecha"))) |> 
  arrange(sector, desc(monto)) |> 
  mutate(n_total = n()) |> 
  group_by(sector) |> 
  mutate(id = n():1,
         max_id = max(id),
         id2 = id-max_id) |> 
  filter(id2 > -10) |> 
  ungroup() |> 
  mutate(monto_etiqueta = monto,
         monto_etiqueta = monto_etiqueta |> format(big.mark=".", decimal.mark = ",", trim = T),
         monto_etiqueta = paste0("$", monto_etiqueta)) |> 
  mutate(espacio = 1000000000)

datos |> 
  
  ungroup() |> 
  ggplot(aes(monto2, id2, fill = sector, color = sector)) +
  geom_col() +
  # geom_text(data = datos |> filter(sector == "Izquierda"),
  #           # filter(monto < mean(monto)*2),
  #           aes(label = caso), hjust = 1, size = 3) +
  # # geom_text(data = datos |> filter(sector == "Izquierda") |> 
  # #             filter(monto >= mean(monto)*2),
  # #           aes(label = paste(" ", caso)), hjust = 0, color = "white", size = 3) +
  #textos izquierda caso afuera
  geom_text(data = datos |> filter(sector == "Izquierda") |>
              filter(monto < mean(monto)*3),
            aes(label = caso, x = monto2-espacio), hjust = 1, vjust = -0.2, size = 4) +
  #textos izquierda monto afuera
  geom_text(data = datos |> filter(sector == "Izquierda") |>
              filter(monto < mean(monto)*2),
            aes(label = monto_etiqueta, x = monto2-espacio), hjust = 1, vjust = 1.2, size = 3) +
  #textos derecha caso afuera
  geom_text(data = datos |> filter(sector == "Derecha") |>
              filter(monto < mean(monto)*3),
            aes(label = caso, x = monto2+espacio), hjust = 0, vjust = -0.2, size = 4) +
  #textos derecha monto afuera
  geom_text(data = datos |> filter(sector == "Derecha") |>
              filter(monto < mean(monto)*2),
            aes(label = monto_etiqueta, x = monto2+espacio), hjust = 0, vjust = 1.2, size = 3) +
  #textos derecha caso adentro
  geom_text(data = datos |> filter(sector == "Derecha") |> 
              filter(monto >= mean(monto)*2),
            aes(label = caso, x = monto2-espacio), hjust = 1, vjust = -0.2, color = "white", size = 4) +
  #textos derecha monto adentro
  geom_text(data = datos |> filter(sector == "Derecha") |> 
              filter(monto >= mean(monto)*2),
            aes(label = monto_etiqueta, x = monto2-espacio), 
            hjust = 1, vjust = 1.4, color = "white", size = 3) +
  scale_x_continuous(limits = c(0-max(datos$monto), max(datos$monto)),
                     expand = expansion(c(0, 0.2)),
                     breaks = c(-30000000000, -10000000000, 0, 10000000000, 30000000000), minor_breaks = NULL,
                     labels = ~format(abs(.x), big.mark = ".", decimal.mark = ",", trim = T)) +
  scale_y_discrete() +
  theme(legend.position = "top",
        legend.title = element_blank())


#CPI ----

cpi <- readr::read_rds("app/corruption_perception_index_chile.rds")

cpi |> 
  ggplot(aes(año, cpi)) +
  geom_line(linewidth = 2, alpha = .4) +
  geom_point(size = 7, alpha = .9) +
  geom_point(aes(y = cpi + 0.3, shape = cambio, fill = cambio, color = cambio),
               size = 4, alpha = 0.8) +
  scale_shape_manual(values = c("baja" = 25, "sube" = 24, "igual" = NA)) +
  scale_fill_manual(values = c("baja" = "red", "sube" = "green", "igual" = NA), aesthetics = c("color", "fill")) +
  scale_x_continuous(breaks = cpi$año) +
  theme(legend.position = "none") +
  # theme(#text = element_text(family = "IBM Plex Mono"),
  #   panel.grid.minor.x = element_blank(),
  #   axis.text = element_text(size = opt_texto_axis),
  #   axis.title.y = element_text(size = opt_texto_plot, margin = margin(r=6), face = "italic")) +
  labs(y = "Índice de percepción de la corrupción", x = NULL)




# alcaldías ----

corrupcion <- readRDS("app/corrupcion_datos.rds")

library(gt)

corrupcion |> 
  filter(alcalde == "Alcaldías") |> 
  select(caso, comuna, año, partido, sector, monto) |> 
  arrange(desc(monto)) |> 
  gt() |> 
  cols_align(columns = where(is.numeric), align = "right") |> 
  cols_align(columns = caso, align = "left") |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
  data_color(
    columns = c(sector), method = "factor",
    levels = c("Izquierda", "Derecha"), palette = c("red", "blue")
  ) |> 
  fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
  cols_label(
    caso = "Caso de corrupción",
    comuna = "Comuna",
    año = "Año",
    partido = "Partido político",
    sector = "Sector político",
    monto = "Monto defraudado",
  ) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
  tab_options(table.font.color = "green") |> 
  tab_style(style = list(
      cell_fill(color = "grey80"),
      cell_text(style = "italic")
    ),
    locations = cells_body(
      columns = caso
    )
  )
