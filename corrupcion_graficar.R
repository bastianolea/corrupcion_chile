# actualiza todos los gráficos y los pone en el readme

library(furrr)
plan(multisession)

# source("graficos/graficos_torta.R")
# source("mapas/graficar_mapas.R")
# source("graficos/grafico_montos_pelotas.R")
# source("tablas/tabla_casos_corrupcion.R")
# source("tablas/tabla_municipios.R")

scripts <- c("graficos/graficos_torta.R",
"mapas/graficar_mapas.R",
"graficos/grafico_montos_pelotas.R",
"tablas/tabla_casos_corrupcion.R",
"tablas/tabla_municipios.R")

# ejecutar multicore
future_walk(scripts, source)