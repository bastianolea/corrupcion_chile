# crea una tabla con objetos y sus respectivos precios, que se usa para el gráfico de la app que compara montos con su equivalente en cosas tangibles

library(dplyr)

#objetos ----

#containers de pizza
#https://www.sjonescontainers.co.uk/container-dimensions/

container_mt2 = 13.93
container_cm2 = container_mt2*100
container_alto_mt = 2.59
container_alto_cm = container_alto_mt*100

pizza_cm2 = 36
pizza_alto_cm = 5

pizzas_suelo = container_cm2/pizza_cm2
pizzas_alto <- container_alto_cm/pizza_alto_cm
pizzas_container = as.integer(pizzas_suelo * pizzas_alto)

completo_cm2 = 15
completo_alto_cm = 8

completo_suelo = container_cm2/completo_cm2
completo_alto <- container_alto_cm/completo_alto_cm
completo_container = as.integer(completo_suelo * completo_alto)

precios <- tibble::tribble(~unidad, ~objeto, ~precio, ~imagen, ~ancho,
                           1, "radiopatrullas de Carabineros (Toyota Corolla Cross)", 34493970, "car.svg", 32,
                           1, "toneladas de palta", 5000*1000, "palta.png", 32,
                           10000, "helados Centella", 300*10000, "centella.png", 32,
                           1000, "completos italianos", 2500*1000, "completo.png", 32,
                           1, "container lleno de completos italianos", 2500*completo_container, "completo.png", 32,
                           1000, "pizzas familiares", 17000*1000, "pizza.svg", 32,
                           1, "container lleno de pizzas familiares", 17000*pizzas_container, "pizza.svg", 32,
                           200, "computadores gamer", 750000, "computer.svg", 32,
                           200, "computadores portátiles", 280000, "laptop.svg", 32,
                           300, "sueldos mínimos", 500000, "money.svg", 32,
                           1, "buses eléctricos", 160400000, "bus.svg", 32,
                           1, "buses eléctricos de dos pisos", 520800000, "bus.svg", 32,
                           1, "autos nuevos (chicos)", 10390000, "car.svg", 32,
                           1, "camionetas nuevas", 18076000, "truck.svg", 32,
                           1, "vehículos de Carabineros (Dodge Charger)", 39777000, "car.svg", 32,
                           1, "camionetas de Carabineros (Ford Ranger)", 50833227, "truck.svg", 32,
                           1, "viviendas sociales construidas", 14700344, "house.svg", 32,
                           1, "viviendas sociales en condominio construidas", 19110447, "house.svg", 32,
                           1, "cesfam", mean(c(7557000000, 6300000000, 7819818000, 4786766000)), "hospital.svg", 32, #https://www.goremaule.cl/goremauleVII/2022/02/22/gobierno-regional-financiara-la-construccion-de-los-cesfam-de-maule-linares-y-vichuquen/
                           1, "departamentos (económicos, 30mt2)", 36700000, "building.svg", 32,  #https://www.latercera.com/la-tercera-pm/noticia/cuanto-cuesta-una-vivienda-en-chile/TS3V3YFASZBD7JDKGFWMYCFD24/#:~:text=“Respecto%20de%20la%20oferta%20habitacional,de%20Estudios%20de%20TOCTOC.com.
                           1, "departamentos promedio en santiago", 115000000,  "building.svg", 32, #https://www.latercera.com/la-tercera-pm/noticia/cuanto-cuesta-una-vivienda-en-chile/TS3V3YFASZBD7JDKGFWMYCFD24/#:~:text=“Respecto%20de%20la%20oferta%20habitacional,de%20Estudios%20de%20TOCTOC.com.
                           1, "casas (económicas, 51mt2)", 47200000, "house.svg", 32,
                           1, "casas promedio en Santiago", 163600000, "house.svg", 32,
                           1, "viviendas con subsidio Clase Media DS1", 40425946, "house.svg", 32,
                           1, "viviendas con subsidio DS49", 34913000, "house.svg", 32)


readr::write_rds(precios, "app/precios_objetos.rds")