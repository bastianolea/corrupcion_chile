# este script lee el excel con los datos, los limpia y los guarda en otros formatos para usarlos en la app
# hay que ejecutarlo cada vez que se guardan datos nuevos en la planilla de datos, para que lleguen a la app

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(forcats)


options(scipen = 9999)

#cargar ----
#cargar base de datos de casos de corrupción en chile
corrupcion_0 <- readxl::read_excel("datos/casos_corrupcion_chile.xlsx")


#limpiar ----
corrupcion <- corrupcion_0 |> 
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
  #variables nuevas para visualizador
  mutate(partido = ifelse(partido == "" | is.na(partido), "Ninguno", partido),
         caso_fundaciones = ifelse(!is.na(fundacion), "Caso fundaciones", "Otros casos"),
         alcalde = ifelse(posicion == "Alcalde", "Alcaldías", "Otros casos"),
         alcalde = replace_na(alcalde, "Otros casos"),
         perjudicado = ifelse(perjudicado == "" | is.na(perjudicado), "Otros", perjudicado),
  ) |> 
  ungroup() |> 
  #otros
  mutate(sector = replace_na(sector, "Ninguno"),
         partido = replace_na(partido, "Ninguno"),
         delitos = str_to_sentence(delitos)
         )

## guardar ----
readr::write_rds(corrupcion, "app/corrupcion_datos.rds")



# dividir montos grandes ----
# browser()
# mean(corrupcion$monto)
# median(corrupcion$monto)

# cantidad_circulos = 15
# corte_enormes = 30000 #millones
# divisor_montos_enormes = 3 #cantidad de columnas en montos enormes
cantidad_circulos = 25 #15
corte_enormes = 10000 #millones
corte_ultra = 50000
divisor_montos_enormes = 3 #cantidad de columnas en montos enormes

corrupcion_dividido <- corrupcion |>
  arrange(desc(monto)) |> 
  #identificar si los montos de los mayores casos son enormes en comparación con el resto
  select(caso, monto) |> 
  mutate(magnitud = case_when(monto >= corte_ultra*1000000 ~ "ultra",
                              monto >= corte_enormes*1000000 ~ "enorme",
                              .default = "normal")) |> 
  #dividir los montos grandes en tres partes iguales, los normales dejarlos como están
  group_by(caso) |> 
  # mutate(monto1 = ifelse(magnitud == "enorme", monto/divisor_montos_enormes, monto),
  #        monto2 = ifelse(magnitud == "enorme", monto/divisor_montos_enormes, NA),
  #        monto3 = ifelse(magnitud == "enorme", monto/divisor_montos_enormes, NA),
  # ) |>
  mutate(monto1 = ifelse(magnitud == "enorme", (monto/10)*2, monto),
         monto2 = ifelse(magnitud == "enorme", (monto/10)*3, NA),
         monto3 = ifelse(magnitud == "enorme", (monto/10)*5, NA),
  ) |>
  mutate(monto1 = ifelse(magnitud == "ultra", (monto/10)*1, monto1),
         monto2 = ifelse(magnitud == "ultra", (monto/10)*1.5, monto2),
         monto3 = ifelse(magnitud == "ultra", (monto/10)*2, monto3),
         monto4 = ifelse(magnitud == "ultra", (monto/10)*2.5, NA),
         monto5 = ifelse(magnitud == "ultra", (monto/10)*3, NA),
  ) |>
  # mutate(monto1 = ifelse(magnitud == "ultra", (monto/10)*(0.357*1), monto1),
  #        monto2 = ifelse(magnitud == "ultra", (monto/10)*(0.357*2), monto2),
  #        monto3 = ifelse(magnitud == "ultra", (monto/10)*(0.357*3), monto3),
  #        monto4 = ifelse(magnitud == "ultra", (monto/10)*(0.357*4), NA),
  #        monto5 = ifelse(magnitud == "ultra", (monto/10)*(0.357*5), NA),
  #        monto6 = ifelse(magnitud == "ultra", (monto/10)*(0.357*6), NA),
  #        monto7 = ifelse(magnitud == "ultra", (monto/10)*(0.357*7), NA),
  # ) |>
  select(-monto) |> 
  pivot_longer(cols = starts_with("monto"), names_to = "division_monto", values_to = "monto_dividido") |> 
  filter(!is.na(monto_dividido))

#escalar datos de 1 a 15, donde el monto maximo es 15
corrupcion_dividido_escalado <- corrupcion_dividido |> 
  ungroup() |> 
  mutate(x = monto_dividido,
         monto_escalado = (x - min(x)) / (max(x) - min(x)) * cantidad_circulos,
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
  select(-posicion)

corrupcion_escalado_0 <- corrupcion_dividido_escalado_escalera |> 
  #volver a incluir el monto original de cada caso como una nueva columna
  left_join(corrupcion |> 
              select(caso, monto, sector, año, partido, perjudicado, alcalde, caso_fundaciones), 
            join_by(caso)) |> 
  #crear una variable que sirva como etiqueta (porque los montos son muy largos)
  mutate(#caso_etiqueta = str_wrap(caso, 40),
    caso_etiqueta = fct_reorder(caso, monto_escalera)) |> 
  #como los valores grandes se van a dividir en 3 columnas, no queremos tener 3 etiquetas iguales, por lo que hacemos que solo sea visible la del medio, y la ordenamos para que quede en el medio
  mutate(division_monto_n = str_remove(division_monto, "monto") |> as.integer()) |> 
  mutate(caso_central = case_when(magnitud == "ultra" & division_monto == "monto3" ~ as.character(caso_etiqueta),
                                  magnitud == "enorme" & division_monto == "monto2" ~ as.character(caso_etiqueta),
                                  magnitud == "normal" ~ as.character(caso_etiqueta), 
                                  .default = "")) |> 
  mutate(division_monto_etiqueta = paste(caso_central, division_monto_n),
         division_monto_etiqueta = fct_reorder(division_monto_etiqueta, division_monto_n)) |> 
  #convertir monto total a una etiqueta
  mutate(monto_etiqueta = as.integer(monto/1000000),
         monto_etiqueta = format(monto_etiqueta, big.mark = ".", decimal.mark = ","),
         monto_etiqueta = paste(monto_etiqueta, "millones")) |> 
  ungroup() |> 
  mutate(ninguno = "ninguno") #variable falsa para desagrupar todos los casos en el gráfico

# View(corrupcion_dividido_escalado_escalera)


## guardar ----
readr::write_rds(corrupcion_escalado_0, "app/corrupcion_datos_escalados.rds")



# datos cep ----
cep <- readxl::read_excel("datos/CEP_datos_percepcion_1_a_total_(1994-2023).xlsx") |> 
  janitor::clean_names() |> 
  mutate(variable = stringr::str_wrap(variable, 20)) |> 
  mutate(año = lubridate::year(fecha))

readr::write_rds(cep, "app/cep_corrupcion.rds")



# datos índice de percepciones de corrupción ----
# https://www.transparency.org/en/cpi/2023/index/chl

cpi <- readxl::read_excel("datos/corruption_perception_index_chile.xlsx")

cpi2 <- cpi |> 
  mutate(cambio = case_when(lag(cpi) > cpi ~ "baja", 
                            lag(cpi) == cpi ~ "igual",
                            lag(cpi) < cpi ~ "sube"),
         cambio = ifelse(is.na(cambio), "igual", cambio))

readr::write_rds(cpi2, "app/corruption_perception_index_chile.rds")