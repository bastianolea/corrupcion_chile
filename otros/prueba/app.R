library(shiny)
library(dplyr)
library(purrr)
options(scipen=9999)

precios <- tibble::tribble(~objeto, ~precio, ~imagen, ~ancho,
                           "radiopatrullas de Carabineros (Toyota Corolla Cross)", 3449397, "car.svg", 32,
                           "completos italianos", 2500*100, "completos100.png", 32,
                           "pizzas familiares", 17000*100, "pizzas100.png", 32,
                           "menús de comida china para 2 personas", 23000*100, "bowl.svg", 32,
                           "suledos mínimos", 500000, "money.svg", 32,
                           "autos nuevos (chicos)", 10390000, "car.svg", 32,
                           "camionetas nuevas", 18076000, "truck.svg", 32,
                           "vehículos de Carabineros (Dodge Charger)", 39777000, "car.svg", 32,
                           "camionetas de Carabineros (Ford Ranger)", 50833227, "truck.svg", 32,
                           "viviendas sociales construidas", 14700344, "house.svg", 32,
                           "viviendas sociales en condominio construidas", 19110447, "house.svg", 32,
                           "cesfam", mean(c(7557000000, 6300000000, 7819818000, 4786766000)), "hospital.svg", 32, #https://www.goremaule.cl/goremauleVII/2022/02/22/gobierno-regional-financiara-la-construccion-de-los-cesfam-de-maule-linares-y-vichuquen/
                           "departamentos (económicos, 30mt2)", 36700000, "building.svg", 32,  #https://www.latercera.com/la-tercera-pm/noticia/cuanto-cuesta-una-vivienda-en-chile/TS3V3YFASZBD7JDKGFWMYCFD24/#:~:text=“Respecto%20de%20la%20oferta%20habitacional,de%20Estudios%20de%20TOCTOC.com.
                           "departamentos promedio en santiago", 115000000,  "building.svg", 32, #https://www.latercera.com/la-tercera-pm/noticia/cuanto-cuesta-una-vivienda-en-chile/TS3V3YFASZBD7JDKGFWMYCFD24/#:~:text=“Respecto%20de%20la%20oferta%20habitacional,de%20Estudios%20de%20TOCTOC.com.
                           "casas (económicas, 51mt2)", 472000000, "house.svg", 32,
                           "casas promedio en Santiago", 163600000, "house.svg", 32,
                           "viviendas con subsidio Clase Media DS1", 40425946, "house.svg", 32,
                           "viviendas con subsidio DS49", 34913000, "house.svg", 32)

ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = "white", fg = "black", primary = "black"
  ),
  
  selectInput("comparar", label = "comparar:", choices = precios$objeto),
  p("un monto de", textOutput("comparar_monto", inline = T), "es aproximadamente equivalente a ", textOutput("comparar_n_objetos", inline = T), textOutput("comparar_objeto", inline = T)),
  p("precio unitario de referencia: ", textOutput("comparar_precio", inline = T), style = "font-size: 80%;"),
  uiOutput("prueba")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  comparar_monto <- reactive(6000000000)
  
  comparar <- reactive(precios |> filter(objeto == input$comparar))
  cantidad_objetos <- reactive(as.integer(comparar_monto()/comparar()$precio))
  
  output$comparar_monto <- renderText(format(comparar_monto(), big.mark = "."))
  output$comparar_precio <- renderText(format(comparar()$precio, big.mark = "."))
  output$comparar_objeto <- renderText(input$comparar)
  output$comparar_n_objetos <- renderText(cantidad_objetos())
  
  output$prueba <- renderUI({
    
    # filtro = "filter: invert(44%) sepia(14%) saturate(1820%) hue-rotate(97deg) brightness(88%) contrast(90%);"
    # filtro = "filter: invert(1);"
    
    output <- map(1:cantidad_objetos(), ~{
      div(
        img(
          src = comparar()$imagen, width = comparar()$ancho, 
          style = "margin: 3px;"),
        style = "display: inline-block;") 
    })
    return(output)
  })
}


shinyApp(ui = ui, server = server)
