# Visualizador de datos de corrupción en Chile

[Aplicación web](https://bastianoleah.shinyapps.io/corrupcion_chile/) que permite visualizar datos sobre los casos más relevantes de corrupción en Chile.

Este repositorio compila datos abiertos sobre este tema país, y produce gráficos que permiten analizar cómo y desde dónde ha operado la corrupción en Chile.

 
## Metodología

Los casos de corrupción incluidos son aquellos donde se involucre perjuicio económico a recursos públicos, sector público, o al fisco en general. 

Los datos son recopilados manualmente, y se obtienen desde fuentes periodísticas. El criterio de inclusión para cada caso es que existan fuentes confiables de prensa que indiquen la existencia de una investigación, evidencia plausible de posible corrupción, o bien una sentencia o condena.

Los datos son [organizados en una planilla]((https://github.com/bastianolea/corrupcion_chile/raw/main/datos/casos_corrupcion_chile.xlsx)) con sus respectivas **fuentes** y la información disponible en casa caso. Accede a la carpeta `datos` de este repositorio para revisarlos.

Todo el sistema de análisis de datos está programado en el lenguaje de programación estadística R, y la app misma está desarrollada en Shiny, un paquete de R para el desarrollo de aplicaciones web interactivas.

### Fuente de los datos
Cada caso incluido en las tablas, gráficos y la [aplicación web interactiva](https://bastianoleah.shinyapps.io/corrupcion_chile/) están catalogados en una base de datos abierta y pública. En esta base, cada fila corresponde a un caso de corrupción, y cada columna agrega información para caracterizar los casos, sus participantes y desenlaces. Las últimas cinco columnas corresponden a fuentes. **Todos** los casos considerados tienen entre 1 y 5 fuentes pediodísticas desde las cuales se obtiene la información que alimenta al resto de las columnas.

**[Descarga a la base de datos de casos de corrupción en formato Excel en este enlace.](https://github.com/bastianolea/corrupcion_chile/raw/main/datos/casos_corrupcion_chile.xlsx)**

**[Contáctame](https://bastianolea.rbind.io/contact/)** para agregar, complementar o corregir casos de corrupción.

Los datos están siendo compilados manualmente en este repositorio, por lo que se trata de una aplicación en constante proceso. Si quieres complementar los datos existentes, ayudar con correcciones, agregar casos nuevos, o hacer cualquier comentario, puedes encontrar los datos en el [repositorio](https://github.com/bastianolea/corrupcion_chile), o bien, [contactarme](https://bastianolea.rbind.io/contact/) por alguno de los medios disponibles en mi sitio web.

_Otras fuentes de datos:_
- Encuesta CEP, cuyos datos son obtenidos a través del [Graficador CEP](https://www.cepchile.cl/opinion-publica/encuesta-cep/), visualizador de datos de la Encuesta CEP programado por Bastián Olea Herrera como parte del equipo DataUC.
- Índice de percepción de la corrupción (Corruption Perceptions Index), [Tranparency International](https://www.transparency.org/en/cpi/2023/index/chl)


## Visualizador interactivo de datos

[La aplicación web está disponible en shinyapps.io](https://bastianoleah.shinyapps.io/corrupcion_chile/), o bien, puedes clonar este repositorio en tu equipo para usarla por medio de RStudio.


## Estructura del código

Esta breve sección explica a grandes rasgos cómo funciona este proyecto, en términos programáticos.

Todo el contenido de este repositorio depende de un archivo de datos en formato Excel, `datos/casos_corrupcion_chile.xlsx`. Este archivo es producido a partir de `datos/casos_corrupcion_chile.numbers`, que es una planilla de Apple Numbers, pero también podría editarse directamente el Excel. En esta planilla se almacenan manualmente todos los datos de la plataforma.

Los datos de la plataforma se procesan ejecutando el script `corrupcion_procesar.R`, que procesa los datos desde el archivo Excel para dejarlos en formato .rds (nativo de R) en el archivo `app/corrupcion_datos.rds`. Este archivo es el que alimenta la aplicación y todos los gráficos y tablas.

El script `corrupcion_graficar.R` ejecuta varios otros scripts que producen gráficos, tablas y mapas a partir de los datos procesados, `app/corrupcion_datos.rds`. Los gráficos, tablas y mapas se guardan en las carpetas `graficos`, `tablas` y `mapas`, respectivamente, con nombres de archivo con o sin fecha. Los archivos sin fecha son usados en este `readme.md` para mantener actualizadas las visualizaciones estáticas, y los con fecha se guardan sólo por temas de archivación de versiones anteriores.

----


## Casos de corrupción por monto
![Gráfico de torta de montos sumados de casos corrupción por sector político](graficos/grafico_torta_montos_sector.png)
----
![Gráfico de torta de cantidad de casos corrupción por sector político](graficos/grafico_torta_sector.png)
----
![Gráfico de torta de cantidad de casos corrupción por partido político](graficos/grafico_torta_partido.png)
----
![Gráfico de casos de corrupción por montos defraudados, indicando casos relacionados a fundaciones](graficos/grafico_corrupcion_montos_caso_fundaciones.png)
----
![Gráfico de casos de corrupción por montos defraudados, indicando sector político implicado](graficos/grafico_corrupcion_montos_sector.png)
----
![Gráfico de casos de corrupción por montos defraudados, indicando casos de alcaldes y municipios con su sector político](graficos/grafico_corrupcion_montos_alcaldes_sector.png)


----

### Tabla de casos de corrupción donde implicados tengan afiliación a partidos políticos
![Tabla de casos de corrupción en municipios donde implicados tengan afiliación a partidos políticos](tablas/tabla_corrupcion_partidos_chile.png)

----

## Casos de corrupción en municipios

### Tabla de casos de corrupción donde implicados tengan afiliación a partidos políticos
![Municipios y/o alcaldes involucrados en casos de corrupción en Chile, con su partido y sector político](tablas/tabla_corrupcion_municipalidades_chile.png)
----

### Mapa de casos de corrupción en municipalidades de la Región Metropolitana
![Mapa de municipios y/o alcaldes involucrados en casos de corrupción en Chile, por sector político, en la Región Metopolitana](mapas/mapa_corrupcion_municipios_rm.png)
----
![Tabla de municipios y/o alcaldes involucrados en casos de corrupción en Chile, por sector político, en la Región Metopolitana](tablas/tabla_corrupcion_municipalidades_rm.png)

----

#### Pantallazos de la app
![Corrupción en Chile, visualizador interactivo de datos 1](otros/pantallazos/pantallazo_corrupcion_chile_a.jpg)
![Corrupción en Chile, visualizador interactivo de datos 2](otros/pantallazos/pantallazo_corrupcion_chile_b.jpg)
![Corrupción en Chile, visualizador interactivo de datos 3](otros/pantallazos/pantallazo_corrupcion_chile_c.jpg)
![Corrupción en Chile, visualizador interactivo de datos 4](otros/pantallazos/pantallazo_corrupcion_chile_d.jpg)


---- 

Diseñado y programado en R por Bastián Olea Herrera. Magíster en Sociología, data scientist.

https://bastianolea.rbind.io

bastianolea@gmail.com


----

_Agradecimientos:_
- Miguel Oyarzo, por sus comentarios, contribución de datos nuevos e ideas
- Rodrigo Rettig, por sus contribución de datos nuevos y difusión


----

_Citar:_

Olea H., B. (2024). Corrupción en Chile. Obtenido desde https://github.com/bastianolea/corrupcion_chile

https://osf.io/xmnbd/