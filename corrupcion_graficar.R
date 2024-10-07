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

# copiar a la app
file.copy(c("graficos/grafico_torta_montos_sector.png",
            "graficos/grafico_torta_sector.png",
            "graficos/grafico_torta_partido.png",
            "mapas/mapa_corrupcion_municipios_rm.png",
            "graficos/grafico_corrupcion_montos_sector.png",
            "graficos/grafico_corrupcion_montos_caso_fundaciones.png",
            "tablas/tabla_corrupcion_partidos_chile.png",
            "tablas/tabla_corrupcion_municipalidades_chile.png",
            "tablas/tabla_corrupcion_municipalidades_rm.png"),
          "app/www/graficos", overwrite = TRUE)
