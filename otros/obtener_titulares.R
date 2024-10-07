library(tidyverse)
library(rvest)

#aplica web scraping a las fuentes de los casos para obtener los titulares de las noticias o sitios web


corrupcion <- readRDS("app/corrupcion_datos.rds")

corrupcion2 <- corrupcion |> select(caso, sector, partido, fuente1, fuente2)

enlaces <- corrupcion2 |> pull(fuente1)


titulos <- map(enlaces, ~{
  # .x <- enlaces[2]
  message(.x)
  
  sesion <- session(.x) |> read_html() |> try()
  
  if (class(sesion)[1] == "try-error") return(NULL)
  
  titulo <- sesion |> 
    html_elements("h1")
  
  if (length(titulo) == 0) {
    titulo <- sesion |> 
      html_elements("h2")
  }
  if (length(titulo) == 0) {
    titulo <- sesion |> 
      html_elements(".title")
  }
  if (length(titulo) == 0) {
    titulo <- sesion |> 
      html_elements(".titulo")
  }
  return(list(.x, titulo))
})
