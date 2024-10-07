# Este script ejecuta los dos scripts principales del proyecto: el de procesamiento, y el de visualización de los datos

# Los datos de la plataforma se procesan ejecutando el script `corrupcion_procesar.R`, 
# que procesa los datos desde el archivo Excel para dejarlos en formato .rds (nativo de R) 
# en el archivo `app/corrupcion_datos.rds`. Este archivo es el que alimenta la aplicación 
# y todos los gráficos y tablas.
source("corrupcion_procesar.R")

# El script `corrupcion_graficar.R` ejecuta varios otros scripts que producen gráficos, 
# tablas y mapas a partir de los datos procesados, `app/corrupcion_datos.rds`. Los gráficos, 
# tablas y mapas se guardan en las carpetas `graficos`, `tablas` y `mapas`, 
# respectivamente, con nombres de archivo con o sin fecha. Los archivos sin fecha 
# son usados en este `readme.md` para mantener actualizadas las visualizaciones 
# estáticas, y los con fecha se guardan sólo por temas de archivación de versiones anteriores.
source("corrupcion_graficar.R")
