library(shiny)
library(ggplot2)
library(dplyr)
options(scipen=9999)

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

precios <- tibble::tribble(~objeto, ~precio, ~imagen, ~ancho,
                           "radiopatrullas de Carabineros (Toyota Corolla Cross)", 34493970, "car.svg", 32,
                           # "1000xcompletos italianos", 2500*100, "completos100.png", 32,
                           # "cientos de pizzas familiares", 17000*100, "pizzas100.png", 32,
                           "containers de completos italianos", 2500*completo_container, "completo.png", 32,
                           "containers de pizzas familiares", 17000*pizzas_container, "pizza.svg", 32,
                           # "menús de comida china para 4 personas", 41000, "bowl.svg", 32,
                           # "suledos mínimos", 500000, "money.svg", 32,
                           "autos nuevos (chicos)", 10390000, "car.svg", 32,
                           "camionetas nuevas", 18076000, "truck.svg", 32,
                           "vehículos de Carabineros (Dodge Charger)", 39777000, "car.svg", 32,
                           "camionetas de Carabineros (Ford Ranger)", 50833227, "truck.svg", 32,
                           "viviendas sociales construidas", 14700344, "house.svg", 32,
                           "viviendas sociales en condominio construidas", 19110447, "house.svg", 32,
                           "cesfam", mean(c(7557000000, 6300000000, 7819818000, 4786766000)), "hospital.svg", 32, #https://www.goremaule.cl/goremauleVII/2022/02/22/gobierno-regional-financiara-la-construccion-de-los-cesfam-de-maule-linares-y-vichuquen/
                           "departamentos (económicos, 30mt2)", 36700000, "building.svg", 32,  #https://www.latercera.com/la-tercera-pm/noticia/cuanto-cuesta-una-vivienda-en-chile/TS3V3YFASZBD7JDKGFWMYCFD24/#:~:text=“Respecto%20de%20la%20oferta%20habitacional,de%20Estudios%20de%20TOCTOC.com.
                           "departamentos promedio en santiago", 115000000,  "building.svg", 32, #https://www.latercera.com/la-tercera-pm/noticia/cuanto-cuesta-una-vivienda-en-chile/TS3V3YFASZBD7JDKGFWMYCFD24/#:~:text=“Respecto%20de%20la%20oferta%20habitacional,de%20Estudios%20de%20TOCTOC.com.
                           "casas (económicas, 51mt2)", 47200000, "house.svg", 32,
                           # "casas promedio en Santiago", 163600000, "house.svg", 32,
                           "viviendas con subsidio Clase Media DS1", 40425946, "house.svg", 32,
                           "viviendas con subsidio DS49", 34913000, "house.svg", 32)




ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = "black", fg = "white", primary = "white"
  ),
  
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  
  ### selector ----
  selectInput("selector_comparar", label = "comparar:", choices = precios$objeto),
  actionButton("comparar", "comparar"),
  
  ### texto ----
  div(style = "margin-top:24px;",
  uiOutput("comparar_parrafo")
  ),
  
  verbatimTextOutput("dimension"),
  
  ### grafico puntos comparación ----
  div(style = "height: 100%; width: 100%;",
      # plotOutput("grafico_comparacion")
      
      uiOutput("ui", fill = "container")
  ),
  p("iuiuiu")
)





server <- function(input, output) {
  
  #ancho de ventana ----
  ancho <- reactive(input$dimension[1]) |> bindEvent(input$dimension)
  ancho_ventana <- ancho |> debounce(500)
  output$dimension <- renderText(ancho_ventana())
  
  #monto del caso seleccionado
  comparar_monto <- reactive(60000000000)
  
  ## objeto elegido ----
  #objeto por comparar seleccionado
  comparar <- reactive({
    req(input$comparar > 0)
    message("")
    message("objeto: ", input$selector_comparar)
    precios |> filter(objeto == input$selector_comparar)
  })
  
  ## conversión monto a objetos ----
  #cantidad de objetos a lo que equivale el monto del caso
  cantidad_objetos <- reactive({
    # browser()
    cantidad <- as.integer(comparar_monto()/comparar()$precio)
    cantidad <- ifelse(cantidad == 0, 1, cantidad)
    
    message("cantidad de objetos: ", cantidad)
    cantidad
  })
  
  ## textos ----
  output$comparar_monto <- renderText(format(comparar_monto(), big.mark = ".", decimal.mark = ","))
  output$comparar_precio <- renderText(format(comparar()$precio, big.mark = ".", decimal.mark = ","))
  output$comparar_objeto <- renderText(input$selector_comparar)
  output$comparar_n_objetos <- renderText(cantidad_objetos())
  
  ### párrafo comparacion ----
  output$comparar_parrafo <- renderUI({
    req(comparar())
    message("rendering texto...")
    fluidRow(
      column(12, style = "display: inline-block;",
             
             div(style = "width: 100px; margin-left: 18px; margin-top: 8px;",
                 img(
                   src = comparar()$imagen, width = "100%", style = "margin-left: auto; margin-right: 0; filter: invert(1);"),
             ),
             div(style = "margin-left: 138px; max-width: 500px;",
                 div(
                   p("un monto de", textOutput("comparar_monto", inline = T), "es aproximadamente equivalente a ", textOutput("comparar_n_objetos", inline = T), textOutput("comparar_objeto", inline = T)),
                   p("precio unitario de referencia: ", textOutput("comparar_precio", inline = T), style = "font-size: 80%;"),
                 )
             )
      )
    )
  })
  
  ## cantidad de objetos divida por 10 ----
  cantidad <- reactive({
    cantidad <- cantidad_objetos()/10
    cantidad <- ifelse(cantidad < 1, 1, cantidad)
    # cantidad = 500
    message("cantidad de objetos 2: ", cantidad)
    cantidad
  })
  
  ## medida horizontal ----
  puntos_ancho <- reactive(80)
  
  ## medida vertical ----
  medida_grafico <- reactive({
    # medida <- cantidad()*6
    # message("medida inicial del gráfico: ", medida)
    # medida <- ifelse(medida < 500, 500, medida)
    # medida <- ifelse(medida >= 50000, 49999, medida)
    # medida <- ifelse(cantidad_objetos() < 10, 100, medida)
    # message("medida final del gráfico: ", medida)
    # medida
    # browser()
    
    #si en ancho_ventana() pixeles muestro 150, entonces necesito mostrar 150 x cada ancho_ventana() hasta cumplit el lartgo
    medida = (cantidad()/puntos_ancho())*ancho_ventana()
    medida = as.integer(medida)
    medida
    
  }) 
  
  ### matriz ----
  datos_comparar <- reactive({
    cantidad <- cantidad()
    # cantidad = 200
    
    matriz <- matrix(1:cantidad,
                     nrow = cantidad, ncol = puntos_ancho()) |> 
      as_tibble() |> 
      tidyr::pivot_longer(cols = everything()) |> 
      mutate(name = stringr::str_remove(name, "V"),
             name = as.integer(name))
  })
  
  ### tamaño puntos ----
  tamaño_punto <- reactive({
    # cantidad_tamaño = (200/cantidad())
    # cantidad_tamaño = ifelse(cantidad_objetos() <= 10, 10/cantidad(), cantidad_tamaño)
    cantidad_tamaño = 0.7
    message("tamaño del punto: ", cantidad_tamaño)
    cantidad_tamaño
  })
  
  ## gráfico comparación ----
  output$grafico_comparacion <- renderPlot({
    req(datos_comparar())
    req(medida_grafico())
    req(ancho_ventana())
    message("rendering gráfico...")
    
    p <- datos_comparar() |> 
      ggplot(aes(name, value)) +
      geom_point(size = tamaño_punto(), color = "white", shape = 15)
    
    #cuadros rojos que indican cuánto es 100 o 1000
    if (cantidad() > 1000) {
      p1 <- p +
        annotate("rect", color = "red", fill = NA,
                 xmin = 1, xmax = 100, ymin = max(datos_comparar()$value)-100, ymax = max(datos_comparar()$value)) +
        annotate("text", label = "1.000", size = 5, color = "red", alpha = 0.5, angle = 90, vjust = 0,
                 x = -1, y = max(datos_comparar()$value)-50)
    } else if (cantidad() > 100) {
      p1 <- p +
        annotate("rect", color = "red", fill = NA,
                 xmin = 1, xmax = 10, ymin = max(datos_comparar()$value)-9, ymax = max(datos_comparar()$value)) +
        annotate("text", label = "100", size = 5, color = "red", alpha = 0.5, angle = 90, vjust = 0,
                 x = 0, y = max(datos_comparar()$value)-5)
    } else {
      p1 <- p
    }
    p1 <- p1 +
      coord_cartesian(expand = FALSE, clip = "off") +
      theme_void() +
      theme(plot.margin = unit(rep(1, 4), "mm"),
            plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black"))
    p1
    
  }, height = reactive(medida_grafico()), 
  #width = reactive(medida_grafico())
  # width = ancho_comparacion
  )
  
  
  
  ## ui comparacion ----
  #porque el gráfico tiene que poder ampliarse verticalmente
  output$ui <- renderUI({
    req(datos_comparar())
    req(medida_grafico())
    req(ancho_ventana())
    
    message("rendering ui gráfico...")
    plotOutput("grafico_comparacion",
               # width = ancho_comparacion,
               height = medida_grafico())
  })
  
}


shinyApp(ui = ui, server = server)
