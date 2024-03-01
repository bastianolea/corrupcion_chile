library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(glue)
library(gt)

#temas
library(thematic)
library(showtext)
library(bslib)
source("colores.R")
thematic_shiny(font = "auto", bg = color_fondo, fg = color_texto, accent = color_destacado)



#opciones
options(scipen = 99999)
opt_texto_geom = 6
opt_texto_plot = 16
opt_texto_axis = 13

#datos ----
corrupcion <- readRDS("corrupcion_datos.rds")
corrupcion_escalados <- readRDS("corrupcion_datos_escalados.rds")
cep_corrupcion <- readRDS("cep_corrupcion.rds")
cpi_corrupcion <- readRDS("corruption_perception_index_chile.rds")
precios <- readRDS("precios_objetos.rds")

#funciones ----
source("funciones.R")

css <- function(text) {
  tags$style(glue(text, .open = "{{", .close = "}}"))
}


#ui ----
ui <- fluidPage(
  title = "Corrupción en Chile", 
  lang = "es",
  
  theme = bslib::bs_theme(
    bg = color_fondo, fg = color_texto, primary = color_destacado,
    base_font = font_link(
      "IBM Plex Mono",
      href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,500;0,700;1,400&display=swap"
    )
  ),
  
  useShinyjs(),
  
  # función para obtener la posición vertical de desplazamiento del usuario usando javascript
  js_get_vertical_position(),
  
  # función para obtener el ancho de ventana usando javascript
  js_get_window_width(),

  
  css("
                       h1 { font-size: 180%; font-weight: bold; }
                       h2 { margin-top: 24px; font-size: 150%; font-weight: bold; }
                       "),
  
  css("a { 
      color: {{color_enlaces}}; 
      }"),
  
  css(".action-button {
                       background-color: {{color_destacado}};
                       color: {{color_texto}};
                       }"),
  
  
  fluidRow(
    ##título ----
    div(style = "padding-top: 12px; padding-bottom: 20px;",
        
        titlePanel(h1("Corrupción en Chile"), 
                   windowTitle = "Corrupción en Chile"),
        p("Visualizador interactivo de datos sobre casos de corrupción en Chile.", 
          style = "margin-bottom: 8px; font-size: 90%;"),
        em(tags$a("Bastián Olea Herrera", 
                  href = "http://bastian.olea.biz",
                  target = "_blank")),
        hr()
    )
  ),
  
  ##intro ----
  fluidRow(
    column(12, style = "margin-top: -26px;",
           
           verbatimTextOutput("posicion_vertical"),
           
           markdown("Se denomina **Corrupción** al aprovechamiento del poder político y/o económico en pos de beneficios personales o ilegítimos.
             Agentes del Estado, políticos o empresarios suelen ser sus perpetradores, debido a sus posiciones privilegiadas, capacidad de negociación y redes de contactos."),
           
           markdown("La corrupción ha copado los medios comunicacionales en los últimos meses. Sin embargo, la información que los medios deciden presentar u omitir, y el modo mismo en que la exponen, es generalmente tendencioso."),
           
           markdown("Este visualizador compila [datos abiertos](https://github.com/bastianolea/corrupcion_chile/tree/main/datos) sobre este tema país, y produce gráficos que permiten analizar cómo y desde dónde ha operado la corrupción en Chile, especificando montos, responsables y sus sectores políticos."),
           
           hr()
    )
  ),
  
  fluidRow(
    column(12, align = "center", #style = "max-height: 100px;",
           style = "padding: 20px; padding-top: 0px; padding-bottom: 0px;
           max-width: 450px; margin: auto;",
           
           
           em("Selecciona un rango de años:"),
           
           # sliderInput("años",
           #             "Seleccionar rango de años:",
           #             step = 1, sep = "",
           #             min = min(corrupcion$año, na.rm=T),
           #             max = max(corrupcion$año, na.rm=T), 
           #             dragRange = F,
           #             value = c(2010, 2024), width = "100%"
           # ),
           
           div(style = "margin-top: 12px;display: inline-block;",
             pickerInput("desde", label = "Desde",
                         choices = 2010:2024, selected = 2010, 
                         multiple = F, inline = T),
             pickerInput("hasta", label = "Hasta",
                         choices = 2010:2024, selected = 2024,
                         multiple = F, inline = T)
           )
    )
  ),
  
  ##body ----
  ###grafico años ----
  fluidRow(
    column(12, 
           hr(),
           h2("Cantidad anual de casos de corrupción"),
           em(p("Conteo de casos de corrupción descubiertos entre los años",
                em(textOutput("rango_años_barras", inline = T),
                   "y considerados en la", 
                   tags$a("compilación de datos",
                          href = "https://github.com/bastianolea/corrupcion_chile", target = "_blank"),
                   "de este visualizador.")
           )),
           plotOutput("grafico_años") |> withSpinner(color = color_destacado, type = 8)
    )
  ),
  
  ###grafico cep ----
  fluidRow(
    column(12,
           hr(),
           h2("Percepción de la corrupción"),
           
           # ¿Cuáles son los tres problemas a los que debería dedicar mayor esfuerzo en solucionar el gobierno?
           p("Datos de la encuesta CEP que indican el porcentaje de la población nacional encuestada 
             que considera la",  strong("corrupción como uno de los temas principales"), "que el Gobierno debiese solucionar."),
           shinyWidgets::pickerInput("cep", 
                                     label = em("Variables a graficar:"),
                                     choices = unique(cep_corrupcion$variable), 
                                     selected = "Corrupción", multiple = T,
                                     inline = T, width = "fit"
           ),
           plotOutput("grafico_cep") |> withSpinner(color = color_destacado, type = 8),
           
           div(style = "font-size: 80%; margin: 18px; opacity: 0.5;",
               p(em("Datos obtenidos usando el", 
                    tags$a("Graficador CEP,", 
                           href = "https://www.cepchile.cl/opinion-publica/encuesta-cep/", target = "_blank"),
                    "aplicación web para el Centro de Estudios Públicos diseñada y programada por Bastián Olea Herrera como parte del equipo DataUC.")
               )),
           # hr(),
    )
  ),
  
  ###grafico cpi  ----
  fluidRow(
    column(12,
           p("El", strong("índice de percepción de la corrupción"), "de", 
             tags$a("Transparency International", 
                    href = "https://www.transparency.org/en/cpi/2023/index/chl", target = "_blank"),
             "es un ranking de 180 países sobre los niveles percibidos de corrupción en el sector público, que usa una escala de 0 a 100, donde 0 es altamente corrupto."),
           p("En su última medición, correspondiente al año 2023, el índice indica que", 
             strong("la corrupción en Chile aumentó,"), "bajando de 67 a 66 puntos."),
           
           plotOutput("grafico_cpi") |> withSpinner(color = color_destacado, type = 8),
           
           hr(),
    )
  ),
  
  ####graficos torta ---- 
  fluidRow(
    column(6, style = "margin-bottom: -30px;",
           
           h2("Casos de corrupción por sector político"),
           em(p("Porcentaje de casos entre",
                em(textOutput("rango_años_torta1", inline = T),
                   "cuyo responsable o responsables principales pertenecen a un determinado sector político")
           )
           ),
           div(style = "width: 320px; margin: auto; margin-top: -20px;",
               plotOutput("torta_sector") |> withSpinner(color = color_destacado, type = 8)
           )
    ),
    column(6, style = "margin-bottom: -30px;",
           
           h2("Casos de corrupción por partido político"),
           em(p("Porcentaje de casos entre",
                em(textOutput("rango_años_torta2", inline = T),
                   "cuyo responsable o responsables principales tienen o tuvieron afiliación política")
           )
           ),
           div(style = "width: 320px; margin: auto; margin-top: -20px;",
               plotOutput("torta_partido") |> withSpinner(color = color_destacado, type = 8)
           )
    )
  ),
  
  
  ### tabla alcaldías ----
  fluidRow(
    column(12,
           hr(),
           h2("Alcaldes y municipios implicados en casos de corrupción"),
           p("Tabla con todos los casos de corrupción que conciernen a municipios o alcaldías de Chile."),
           
           shinyWidgets::pickerInput("sector_alcaldias", 
                                     label = em("Sector político:"),
                                     choices = c("Todos", "Derecha", "Izquierda"), 
                                     selected = "Todos", multiple = F,
                                     inline = T, width = "fit"
           ),
           
           # div(style = "max-height: 600px; overflow-y: scroll;",
           gt_output("tabla_alcaldías")
           # )
    )
  ),
  
  ### tabla fundaciones ----
  fluidRow(
    column(12,
           hr(),
           h2("Fundaciones involucradas o investigadas por corrupción"),
           p("Tabla con todos los casos de corrupción que involucran a fundaciones."),
           
           # div(style = "max-height: 600px; overflow-y: scroll;",
           gt_output("tabla_fundaciones")
           # )
    )
  ),
  
  
  ###grafico montos ----
  fluidRow(
    column(12,
           hr(),
           h2("Comparación de montos de los mayores casos de corrupción"),
           
           pickerInput("variable_color", label = em("Distinguir casos de corrupción por variable:"),
                       choices = c("No" = "ninguno", 
                                   "Sector político" = "sector",
                                   "Partido político" = "partido",
                                   "Entidad perjudicada" = "perjudicado",
                                   "Caso Fundaciones" = "caso_fundaciones",
                                   "Casos de alcaldías o municipios" = "alcalde"
                       ), inline = T, width = "fit", multiple = F
           ),
           p("Seleccione una opción para distinguir con un color distinto cada caso de corrupción según variables como partido político, sector político, fundaciones involucradas, y otras."),
           p("El gráfico indica los mayores casos de corrupción entre los años",
             em(textOutput("rango_años_montos", inline = T)),
             "y los puntos son unidades ilustrativas para comparar visualmente las magnitudes.")
    ),
    column(12, align = "center", 
           #style = "height: 900px;",
           div(
             style = "min-height: 900px; width: 800px;",
             # min-width: 830px; max-width: 1024px;",
             htmlOutput("ui_montos", fill = TRUE) |> 
               withSpinner(color = color_destacado, type = 8, proxy.height = 400),
             # plotOutput("grafico_montos", width = 900, fill = TRUE) |> withSpinner(color = color_destacado, type = 8)
             
             hr()
           )
    )
  ),
  
  
  ### comparación de objetos ----
  fluidRow(
    column(12,
           h2("¿A qué equivale el monto...?"),
           
           
           p("Las cifras de los casos de corrupción suelen ser muy elevadas, lo que hace difícil entender de cuánto dinero estamos hablando.
                 ¿Qué mejor que convertir los montos en su equivalente en cosas tangibles que podrían haberse financiado con la misma plata?"),
           p("Selecciona un caso de corrupción, y luego una medida para obtener su equivalencia en objetos de forma gráfica."),
           
           p("Por ejemplo, 
                 ¿cuántas radiopatrullas de Carabineros podrían haberse comprado con la plata del municipio de Maipú? 
                 ¿Cuántas viviendas sociales se podrían haber construido con la plata del Pacogate?"),
           
           div(style = "max-width: 500px;",
               pickerInput("caso_elegido_comparar", label = em("Seleccione un caso de corrupción:"),
                           choices = NULL, multiple = F, width = "100%"
               ),
               
               pickerInput("selector_comparar", 
                           label = em("Elija una medida para la equivalencia:"), 
                           choices = precios$objeto, width = "100%", multiple = F),
               
               # div(style = "margin-top: 24px;",
               #     p(em("Presiona el botón para obtener el gráfico:"), style = "margin-bottom: 10px;"),
               #     actionButton("comparar", "comparar monto", width = "100%")
               # )
           ),
           
           ### texto
           uiOutput("comparar_info", fill = TRUE),
    ),
    #### grafico puntos comparación ----
    column(12, align = "center",     
           div(style = "min-width: 420px; 
                        max-width: 1080px;",
               uiOutput("ui_comparacion", fill = "container") |> withSpinner(color = color_destacado, type = 8)
           ),
    )
  ),
  
  
  ### tabla todos los casos ----
  fluidRow(
    column(12, 
           hr(),
           h2("Recopilación de todos los casos de corrupción en Chile"),
           
           markdown("Finalmente, esta tabla transparenta todos los datos recopilados por esta plataforma, que alimentan el resto de visualizaciones. Puedes acceder a estos datos en formato Excel en el [repositorio de GitHub de este proyecto.](https://github.com/bastianolea/corrupcion_chile/tree/main/datos) Si encuentras errores o deseas hacer una corrección, [no dudes en contactarme.](https://twitter.com/bastimapache)"),
           
           div(style = "max-height: 720px; overflow-y: scroll;",
           gt_output("tabla_casos")
           )
    )
  ),
  
  
  
  
  ### firma ----
  fluidRow(
    column(12, style = "padding: 28px;",
           hr(),
           markdown("Desarrollado y programado por [Bastián Olea Herrera.](https://bastian.olea.biz)"),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/corrupcion_chile)"),
           
           div(style = "opacity: 0.4; font-size: 80%;",
               markdown("Los datos de este visualizador son compilados manualmente. Si quieres complementar los datos existentes, ayudar con correcciones, agregar casos nuevos, o hacer cualquier comentario,
             puedes encontrar los datos en el [repositorio](https://github.com/bastianolea/corrupcion_chile), o bien, [contactarme.](http://bastian.olea.biz)")
           ),
           
           div(style = "height: 40px")
           
    )
  )
  
)


#—----

# server ----
server <- function(input, output, session) {
  
  
  # año_min_0 <- reactive(input$años[1])
  # año_max_0 <- reactive(input$años[2])
  # 
  # año_min <- año_min_0 %>% debounce(2000, priority = 200)
  # año_max <- año_max_0 %>% debounce(2000, priority = 200)
  
  año_min <- reactive(as.numeric(input$desde))
  año_max <- reactive(as.numeric(input$hasta))
  
  # datos ----
 
  ## datos año ----
  corrupcion_años <- reactive({
    corrupcion |> 
      filter(!is.na(año)) |> 
      filter(año >= año_min(),
             año <= año_max())
  })
  
  rango_años <- reactive({
    paste0(año_min(), " y ",
           año_max(), ",")
  })
  
  output$rango_años_barras <- renderText(rango_años())
  output$rango_años_torta1 <- renderText(rango_años())
  output$rango_años_torta1 <- renderText(rango_años())
  output$rango_años_montos <- renderText(rango_años())
  
  ## datos montos ----
  corrupcion_escalado <- reactive({
    req(nrow(corrupcion_años()) > 0)
    # browser()
    
    corrupcion_escalados_años <- corrupcion_escalados |> 
      filter(!is.na(año)) |> 
      filter(año >= año_min(),
             año <= año_max())
    
    # browser()
    #filtrar top de casos
    casos <- unique(corrupcion_escalados_años$caso)
    # 
    corrupcion_escalado_filtrado <- corrupcion_escalados_años |>
      filter(caso %in% casos[1:20])
    
    # corrupcion_escalados_años
    corrupcion_escalado_filtrado
  })
  
  
  #ancho de ventana ----
  ancho <- reactive(input$dimension[1]) |> bindEvent(input$dimension)
  ancho_ventana <- ancho |> debounce(500)
  
  # output$dimension <- renderText({
  #   ancho_ventana()
  #   })
  
  #scroll ----
  # guardar posición de desplazamiento como input
    observeEvent(input$y_offset, {
      runjs("Shiny.setInputValue('y_offset', window.pageYOffset);")
    })
  
  # obtener input de desplazamiento y revisarlo para que sea válido
  vertical <- reactive({
    if (length(input$y_offset) > 0) {
    return(input$y_offset[1])
    } else {
      return(0)
    }
  })
  
  # hacer que el input solo se actualice 500 milisegundos después de terminar el scrolling
  vertical_position <- vertical |> debounce(200)
  
  observe({
    message("vertical scroll position debounced: ", vertical_position())
  })
  
  #si el scrolling supera este valor (pixeles de desplazamiento vertical), entonces se cargarán los gráficos grandes
  scroll <- reactiveValues(abajo = FALSE)
  observeEvent(vertical_position(), {
    if (vertical_position() > 1500) {
      scroll$abajo <- TRUE
    }
               })
  
  
  
  # gráfico barras años ----
  output$grafico_años <- renderPlot({
    req(nrow(corrupcion_años()) > 0)
    
    # browser()
    # dev.new()
    corrupcion_años() |> 
      count(año) |> 
      ggplot(aes(año, n)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = n, y = n-0.25), vjust = 1, color = color_enlaces,
                size = opt_texto_geom) +
      scale_y_continuous(breaks = 1:20, expand = expansion(c(0, 0.1))) +
      scale_x_continuous(breaks = año_min():año_max()) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()) +
      labs(y = "casos de corrupción anuales", x = NULL) +
      theme(axis.title.y = element_text(size = opt_texto_plot, margin = margin(r=6), face = "italic"),
            axis.text = element_text(size = opt_texto_axis))
  })
  
  #gráfico cep puntos percepcion ----
  output$grafico_cep <- renderPlot({
    # browser()
    
    datos_cep <- cep_corrupcion |> 
      filter(variable %in% input$cep) |> 
      filter(year(fecha) >= año_min(),
             year(fecha) <= año_max())
    
    if (length(input$cep) > 1) {
      p <- datos_cep |>  
        ggplot(aes(as.Date(fecha), y = porcentaje, color = variable))
    } else {
      p <- datos_cep |>  
        ggplot(aes(as.Date(fecha), y = porcentaje))
    }
    p +
      geom_errorbar(aes(ymin = porcentaje_ic_inferior, ymax = porcentaje_ic_superior),
                    alpha = .3) +
      geom_line(linewidth = 2, alpha = .4) +
      geom_point(size = 7, alpha = .9) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(labels = ~scales::percent(.x, accuracy = 1)) +
      theme(#text = element_text(family = "IBM Plex Mono"),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = opt_texto_axis),
        axis.title.y = element_text(size = opt_texto_plot, margin = margin(r=6), face = "italic")) +
      labs(y = "porcentaje de encuestados", x = NULL) +
      theme(legend.position = "top", legend.direction = "horizontal", 
            legend.title = element_blank(),
            legend.margin = margin(t = 0, b = -3))
  })
  
  #gráfico cpi puntos ----
  
  output$grafico_cpi <- renderPlot({
    # browser()
    
    cpi <- cpi_corrupcion |> 
      filter(año >= año_min(),
             año <= año_max()) 
    
    cpi |> 
      ggplot(aes(año, cpi)) +
      geom_line(linewidth = 2, alpha = .4) +
      geom_point(size = 7, alpha = .9) +
      # geom_text(aes(label = cpi), color = color_fondo, size = opt_texto_geom, face = "IBM Plex Mono") +
      geom_point(aes(y = cpi + 0.5, shape = cambio, fill = cambio, color = cambio),
                 size = 4, alpha = 1) +
      scale_shape_manual(values = c("baja" = 25, "sube" = 24, "igual" = NA)) +
      scale_fill_manual(values = c("baja" = color_negativo, "sube" = color_destacado, "igual" = NA), aesthetics = c("color", "fill")) +
      scale_x_continuous(breaks = cpi$año) +
      scale_y_continuous(breaks = 66:73) +
      theme(legend.position = "none") +
      theme(
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = opt_texto_axis),
        axis.title.y = element_text(size = opt_texto_plot, margin = margin(r=6), face = "italic")) +
      labs(y = "Índice de percepción de la corrupción", x = NULL)
  })
  
  
  #gráfico torta sector ----
  output$torta_sector <- renderPlot({
    req(nrow(corrupcion_años()) > 0)
    
    # browser()
    datos <- corrupcion_años() |> 
      count(sector) |>
      arrange(desc(sector)) |> 
      #calcular posición de etiquetas en torta
      mutate(suma = sum(n)) |> 
      mutate(prop = n / suma *100) %>%
      mutate(ypos = cumsum(prop)- 0.5 * prop)
    
    datos |> 
      ggplot(aes("", prop, fill = sector)) +
      geom_col(width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      geom_text(aes(x = 1.1, y = ypos, label = sector), color = "white", 
                size = opt_texto_geom) +
      geom_point(aes(x = 1.75, y = ypos, col = sector), size = 12) +
      geom_text(aes(x = 1.75, y = ypos, label = scales::percent(prop/100, accuracy = 1)), 
                color = "white", size = opt_texto_geom-1) +
      scale_fill_manual(values = degradado_verde(length(datos$sector)), aesthetics = c("fill", "color")) +
      theme(legend.position="none") +
      theme(plot.margin = unit(rep(-0.5, 4), "cm"))
  })
  
  #gráfico torta partido ----
  output$torta_partido <- renderPlot({
    
    datos_partidos <- corrupcion_años() |> 
      count(partido) |>
      tidyr::separate(partido, into = c("partido1", "partido2", "partido3"), sep = ", ") |> 
      tidyr::pivot_longer(cols = starts_with("partido"), values_to = "partido") |> 
      filter(!is.na(partido) & partido != "") |> 
      select(-name) |> 
      tidyr::uncount(weights = n)
    
    # si un partido solo sale una vez, mandarlo a "otros"
    datos_partidos_reduc <- datos_partidos |> 
      group_by(partido) |> 
      mutate(n = n()) |>
      rowwise() |> 
      mutate(partido_reduc = if_else(n == 1, "Otros", partido)) |> 
      select(-n) |> 
      ungroup()
    
    datos <- datos_partidos_reduc |> 
      # count(partido) |> 
      count(partido_reduc) |>
      rename(partido = partido_reduc) |> 
      arrange(desc(partido)) |> 
      mutate(prop = n / sum(n) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5 * prop)
    
    # browser()
    
    # dev.new()
    datos |> 
      ggplot(aes("", prop, fill = partido)) +
      geom_col(width=1) +
      coord_polar("y", start=0) +
      theme_void() +
      geom_text(aes(x = 1.1, y = ypos, label = partido), color = "white", 
                size = opt_texto_geom) +
      geom_point(aes(x = 1.75, y = ypos, col = partido), size = 12) +
      geom_text(aes(x = 1.75, y = ypos, label = scales::percent(prop/100, accuracy = 1)), 
                color = "white", size = opt_texto_geom-1) +
      # scale_fill_brewer(palette="Set1") +
      # scale_fill_gradient(low = "red", high = "blue") +
      scale_fill_manual(values = degradado_verde(length(datos$partido)), aesthetics = c("fill", "color")) +
      theme(legend.position="none") +
      theme(plot.margin = unit(rep(-0.5, 4), "cm"))
  })
  
  
  
  # gráfico montos divididos ----
  alto_grafico_montos <- reactive({
    casos = length(unique(corrupcion_escalado()$caso))
    # message("casos en gráfico montos: ", casos)
    # alto = 58 * casos
    alto = 60 * casos
    message("gráfico montos: alto gráfico montos: ", alto)
    return(alto)
  })
  
  #etiquetas de texto para los millones 
  monto_etiqueta <- reactive({
    corrupcion_escalado() |> 
      group_by(caso) |> 
      filter(monto_escalera == monto_escalado) |> 
      filter(nchar(as.character(division_monto_etiqueta)) > 3) |> 
      select(caso, monto, monto_etiqueta, monto_escalera, division_monto_etiqueta) |> 
      mutate(monto_etiqueta = str_trim(monto_etiqueta)
             # monto_etiqueta = ifelse(monto >= 10000000000,
             #                         str_wrap(monto_etiqueta, 6),
             #                         monto_etiqueta)
      )
  })
  
  ## gráfico ----
  output$grafico_montos <- renderPlot({
    req(nrow(corrupcion_escalado()) > 0)
    req(monto_etiqueta())
    req(alto_grafico_montos())
    req(scroll$abajo) # req(vertical_position() > 2000)
    
    message("rendering gráfico montos...")
    
    ### opciones
    expansion_y = 0.4 #espacio entre borde de cada faceta de cada caso y sus valores
    expansion_x = 0.8 #espacio entre valor máximo y borde derecho del gráfico
    espaciado_y = 4 #espacio entre facetas
    texto_eje_y = opt_texto_plot+1 #texto casos
    texto_montos = opt_texto_geom #texto montos
    tamaño_punto = 10 #tamaño de círculos
    corte_etiqueta_casos = 32 #caracteres antes del corte de línea de etiquetas y
    
    # browser()
    grafico <- corrupcion_escalado() |> 
      ggplot(aes(x = monto_escalera, y = division_monto_etiqueta,
                 color = .data[[input$variable_color]])) + 
      #puntos
      geom_point(size = tamaño_punto) +
      #signo peso en puntos
      geom_text(aes(label = "$"), color = color_detalle, size = tamaño_punto*0.8) +
      #etiquetas millones
      geom_text(data = monto_etiqueta(),
                aes(label = monto_etiqueta, x = monto_escalera+0.65), 
                color = color_texto, size = texto_montos, hjust = 0, fontface = "italic", lineheight = 0.85) +
      #escala vertical
      scale_y_discrete(labels = ~str_remove(.x, " \\d+") |> str_wrap(corte_etiqueta_casos), #cortar línea de etiquetas eje y
                       expand = expansion(expansion_y)) + #apretar columnas horizontales de puntos
      scale_x_continuous(expand = expansion(c(0.1, expansion_x))) +
      coord_cartesian(clip = "off") +
      facet_grid(rows = vars(caso),
                 scales = "free_y", space = 'free', switch = "y", as.table = FALSE) +
      theme_minimal() +
      theme(strip.background = element_blank(),
            strip.text = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(family = "IBM Plex Mono", face = "bold",
                                       size = texto_eje_y, color = color_texto,
                                       margin = margin(r = 7)),
            axis.text.x = element_blank(),
            panel.background = element_rect(fill = color_detalle, linewidth = 0),
            panel.grid = element_blank(),
            panel.spacing.y = unit(espaciado_y, "mm"),
            legend.text = element_text(color = color_texto, size = 18, hjust = 0, margin = margin(t = 0, b = 0, r = 10)),
            legend.title = element_blank(),
            plot.margin = unit(c(0, 0, 0,  1), "cm")) +
      theme(legend.position = "top", legend.direction = "horizontal", 
            legend.margin = margin(t = 0, b = -3),
            legend.text = element_text(family = "IBM Plex Mono", size = 10))
    
    #sin escala si no se divide el gráfico por colores
    if (input$variable_color == "ninguno") {
      grafico <- grafico +
        scale_color_manual(values = color_destacado) +
        theme(legend.position = "none")
    }
    
    #si color es por sector político
    if (input$variable_color == "sector") {
      grafico <- grafico +
        scale_color_manual(values = c("Derecha" = color_derecha, "Izquierda" = color_izquierda,
                                      "Ninguno" = color_destacado))
    }
    
    plot(grafico)
  }, height = reactive(alto_grafico_montos()), res = 72) #|> 
  # bindCache(input$variable_color, año_min(), año_max())
  
  
  
  # output$div_montos <- renderUI({
  #   # browser()
  #   alto <- 64 * length(unique(corrupcion_escalado()$caso))
  #   output <- plotOutput("grafico_montos", width = 800, height = alto)
  #   return(output)
  # })
  
  ## ui montos ----
  output$ui_montos <- renderUI({
    req(nrow(corrupcion_escalado()) > 0)
    req(monto_etiqueta())
    req(alto_grafico_montos())
    
    message("rendering ui gráfico montos...")
    
    # plotOutput("grafico_comparacion",
    plotOutput("grafico_montos", width = 900, 
               height = alto_grafico_montos()) |> withSpinner(color = color_destacado, type = 8)
  })
  
  
  
  
  # gráfico comparación de objetos ----
  
  
  observeEvent(año_min() | año_max(), {
                 updatePickerInput(session, "caso_elegido_comparar",
                                   choices = unique(as.character(corrupcion_años()$caso))
                 )
               })
  
  ## objeto elegido ----
  #objeto por comparar seleccionado
  comparar <- reactive({
    # req(input$comparar > 0)
    message("")
    message("objeto: ", input$selector_comparar)
    precios |> filter(objeto == input$selector_comparar)
  })
  
  #monto del caso seleccionado
  comparar_monto <- reactive({
    req(input$caso_elegido_comparar)
    # req(input$comparar > 0)
    # browser()
    
    monto <- corrupcion_años() |> 
      filter(caso == input$caso_elegido_comparar) |> 
      pull(monto)
    
    # message("monto del caso elegido: ", monto)
    monto
  })
  
  ## conversión monto a objetos ----
  #cantidad de objetos a lo que equivale el monto del caso
  cantidad_objetos <- reactive({
    # browser()
    cantidad <- as.integer(comparar_monto()/comparar()$precio)
    cantidad <- ifelse(cantidad == 0, 1, cantidad)
    
    message("gráfico comparación: cantidad de objetos: ", cantidad)
    cantidad
  })
  
  ## textos e imagen ----
  output$comparar_monto <- renderText({
    monto <- format(comparar_monto(), big.mark = ".", decimal.mark = ",")
    paste0("$", monto)
  })
  output$comparar_precio <- renderText({
    precio <- format(comparar()$precio, big.mark = ".", decimal.mark = ",")
    paste0("$", precio)
  })
  output$comparar_objeto <- renderText(input$selector_comparar)
  output$comparar_objeto_2 <- renderText(input$selector_comparar)
  output$comparar_n_objetos <- renderText(format(cantidad_objetos(), big.mark = ".", decimal.mark = ","))
  output$comparar_unidad <- renderText({
    unidad <- comparar()$unidad
    
    #singular o plural
    texto <- ifelse(unidad > 1, paste(format(unidad, big.mark = ".", decimal.mark = ","), "unidades"), paste(unidad, "unidad"))
    #por si es solo 1 objeto
    texto <- ifelse(cantidad_objetos() == 1, "menos de 1 unidad", texto)
    
    return(texto)
  })
  
  output$comparar_caso_seleccionado <- renderText(input$caso_elegido_comparar)
  
  ### párrafo e imagen comparacion ----
  
  output$comparar_info <- renderUI({
    req(comparar())
    
    div(style = "margin-top:34px; padding: 8px;",
        column(12, 
               
               #imagen a la izquierda
               div(style = "width: 100px; margin-left: 24px;",
                   # uiOutput("comparar_objeto_imagen"),
                   img(
                     src = comparar()$imagen, 
                     width = "100%", 
                     style = "margin-left: auto; margin-right: 0; 
                 filter: invert(84%) sepia(8%) saturate(1115%) hue-rotate(97deg) brightness(85%) contrast(88%);")
                 
               ),
               
               #texto a la derecha
               div(style = "min-height: 120px; margin-left: 162px; margin-top: -100px; margin-bottom: 24px; max-width: 500px;",
                   # uiOutput("comparar_objeto_parrafo")
                   div(
                     p("El monto de", textOutput("comparar_monto", inline = T), "del caso de corrupción",
                       em(input$caso_elegido_comparar),
                       "es aproximadamente equivalente a ", strong(textOutput("comparar_n_objetos", inline = T), textOutput("comparar_objeto", inline = T))),
                     
                     p("precio unitario de referencia: ", textOutput("comparar_precio", inline = T), style = "font-size: 75%; opacity: 0.6;"),
                   )
               ),
               
               #explicación abajo
               div(
                 p("En esta visualización,", strong("cada punto"), 
                   "equivale a", 
                   strong(textOutput("comparar_unidad", inline = T), "de",
                          textOutput("comparar_objeto_2", inline = T)),
                   "que podrían haberse comprado con la suma de dinero del caso",
                   textOutput("comparar_caso_seleccionado", inline = T),
                   style = "margin-top: 12px;"
                 )
               )
        )
    )
  })
  
  ## cantidad de objetos divida por 10 ----
  cantidad <- reactive({
    # cantidad <- cantidad_objetos()/10
    cantidad <- cantidad_objetos()
    cantidad <- ifelse(cantidad < 1, 1, cantidad)
    # cantidad = 500
    message("gráfico comparación: cantidad de objetos 2: ", cantidad)
    cantidad
  })
  
  ## medidas ----
  #horizontal
  puntos_ancho <- reactive(40) #cantidad de puntos por cada fila
  
  ## medida vertical 
  medida_grafico <- reactive({
    message("ancho ventana: ", ancho_ventana())
    # medida <- cantidad()*6
    # message("medida inicial del gráfico: ", medida)
    # medida <- ifelse(medida < 500, 500, medida)
    # medida <- ifelse(medida >= 50000, 49999, medida)
    # medida <- ifelse(cantidad_objetos() < 10, 100, medida)
    # message("medida final del gráfico: ", medida)
    # medida
    # browser()
    
    #si en ancho_ventana() pixeles muestro 150, entonces necesito mostrar 150 x cada ancho_ventana() hasta cumplir el largo
    # medida = (cantidad()/puntos_ancho()) * 12 #ancho_ventana()
    
    #filas necesarias
    cantidad_filas <- cantidad()/puntos_ancho()
    message("gráfico comparación: cantidad de filas: ", cantidad_filas)
    
    #pixeles de ancho que ocupa cada punto horizontal
    pixeles_por_punto <- ancho_ventana()/puntos_ancho()
    #pixeles que se necesitarían hacia abajo con la cantidad de filas que van a haber
    medida = as.integer(pixeles_por_punto * cantidad_filas)
    if (cantidad_filas > puntos_ancho()) {
      medida = ifelse(medida < 420, 420, medida) #ancho mínimo
    }
    if (cantidad_filas < 1) {
      medida = pixeles_por_punto
    }
    message("gráfico comparación: largo del gráfico: ", medida)
    medida
  }) 
  
  ### matriz ----
  datos_comparar <- reactive({
    cantidad <- as.integer(cantidad())
    # cantidad = 200
    # browser()
    
    # matriz <- matrix(1:cantidad,
    #                  nrow = cantidad/puntos_ancho(), ncol = puntos_ancho()) |> 
    #   as_tibble() |> 
    #   tidyr::pivot_longer(cols = everything()) |> 
    #   mutate(name = stringr::str_remove(name, "V"),
    #          name = as.integer(name))
    
    # puntos_ancho = 80 #cantidad de columnas de ancho de la visualización
    
    puntos_largo = as.integer(cantidad/puntos_ancho()) #cantidad de filas hacia abajo de puntos
    
    #crear una matriz con la cantidad de puntos posible multiplicando la cantidad de puntos de ancho
    matriz <- matrix(1:puntos_largo,
                     nrow = puntos_largo, ncol = puntos_ancho()) 
    
    matriz <- as.data.frame(matriz)
    names(matriz) <- 1:puntos_ancho()
    
    #convertir matriz a formato largo para graficar
    matriz_2 <- matriz |> 
      tidyr::pivot_longer(cols = everything()) |> 
      mutate(name = as.integer(name))
    
    #calcular si el multiplo del ancho da exacto la cantidad de puntos o no
    cantidad_graficada = puntos_ancho() * puntos_largo
    
    if (cantidad_graficada != cantidad) {
      cantidad_faltante = cantidad - cantidad_graficada
      message("gráfico comparación: matriz comparación: faltaron ", cantidad_faltante, " puntos. agregando...")
      
      #si faltan puntos para alcanzar la cifra, se agrega una sola fila con la cantidad de puntos (que es menor al ancho, siempre)
      puntos_faltantes <- tibble(value = 0, name = 1:cantidad_faltante)
      
      matriz_3 <- matriz_2 |> 
        bind_rows(puntos_faltantes)
    } else {
      matriz_3 <- matriz_2
    }
    matriz_3
  })
  
  ### tamaño puntos ----
  tamaño_punto <- reactive({
    # cantidad_tamaño = (200/cantidad())
    # cantidad_tamaño = ifelse(cantidad_objetos() <= 10, 10/cantidad(), cantidad_tamaño)
    if (ancho_ventana() < 600) {
      tamaño_punto = 2
    } else if (ancho_ventana() < 900) {
      tamaño_punto = 3
    } else if (ancho_ventana() >= 900) {
      tamaño_punto = 4
    }
    message("gráfico comparación: tamaño del punto: ", tamaño_punto)
    tamaño_punto
  })
  
  ## gráfico ----
  output$grafico_comparacion <- renderPlot({
    req(datos_comparar())
    req(medida_grafico())
    req(ancho_ventana())
    req(scroll$abajo) # req(vertical_position() > 2000)
    
    message("rendering gráfico comparación...")
    
    p <- datos_comparar() |> 
      ggplot(aes(name, value)) +
      geom_point(size = tamaño_punto()+1, color = color_texto, shape = 15) +
      geom_point(size = tamaño_punto(), color = color_destacado, shape = 15)
    
    # dev.new()
    # browser()
    # #cuadros rojos que indican cuánto es 100 o 1000
    
    # margen = 0.5
    # p +
    #   annotate("rect", color = color_complementario, fill = NA,
    #                         xmin = 1-margen, xmax = 10+margen,
    #                         ymin = max(datos_comparar()$value)-9-margen, ymax = max(datos_comparar()$value)+margen)
    # 
    # p +
    #   annotate("rect", color = color_complementario, fill = NA,
    #            xmin = 1-margen, xmax = 10+margen,
    #            ymin = max(datos_comparar()$value)-100-margen, ymax = max(datos_comparar()$value)+margen)
    # 
    # if (cantidad() > 1000) {
    #   margen = 0.5
    #   p1 <- p +
    #     annotate("rect", color = color_complementario, fill = NA,
    #              xmin = 1-margen, xmax = 100+margen, 
    #              ymin = max(datos_comparar()$value)-100-margen, ymax = max(datos_comparar()$value)+margen)
    #   # annotate("text", label = "1.000", size = 5, color = "red", alpha = 0.5, angle = 90, vjust = 0,
    #   #          x = -1, y = max(datos_comparar()$value)-50)
    # } else if (cantidad() > 100) {
    #   margen = 0.5
    #   p1 <- p +
    #     annotate("rect", color = color_complementario, fill = NA,
    #              xmin = 1-margen, xmax = 10+margen, 
    #              ymin = max(datos_comparar()$value)-9-margen, ymax = max(datos_comparar()$value)+margen)
    #   # annotate("label", label = "100", size = 4, color = color_complementario, 
    #   #          label.size = 0, fill = color_fondo, label.padding = unit(0.3, "lines"), label.r = unit(0, "lines"),
    #   #          alpha = 1, angle = 90, hjust = 0,
    #   #          x = 10+margen, y = max(datos_comparar()$value)-5)
    # } else {
    #   p1 <- p
    # }
    p1 <- p +
      coord_cartesian(expand = FALSE, clip = "off") +
      theme_void() +
      theme(plot.margin = unit(rep(5, 4), "mm"),
            plot.background = element_rect(fill = color_fondo),
            panel.background = element_rect(fill = color_fondo))
    p1
    
  }, height = reactive(medida_grafico()), 
  #width = reactive(medida_grafico())
  # width = ancho_comparacion
  )
  
  
  
  ## ui comparacion ----
  #porque el gráfico tiene que poder ampliarse verticalmente
  output$ui_comparacion <- renderUI({
    req(datos_comparar())
    req(medida_grafico())
    
    # message("rendering ui gráfico comparacion...")
    
    plotOutput("grafico_comparacion",
               # width = ancho_comparacion,
               height = medida_grafico()) |> withSpinner(color = color_destacado, type = 8)
  })
  
  
  # tablas ----
  
  ## tabla alcaldías ----
  output$tabla_alcaldías <- render_gt({
    
    if (input$sector_alcaldias == "Todos") {
      filtro_sector <- c("Derecha", "Izquierda", "Ninguno")
      } else {
        filtro_sector <- input$sector_alcaldias
        # filtro_sector <- "Derecha"
        # filtro_sector <- "Izquierda"
      }
    
    # browser()
    
    corrupcion_años() |> 
      filter(alcalde == "Alcaldías") |> 
      filter(sector %in% filtro_sector) |> 
      select(comuna,  partido, responsable, sector, monto, año,
             fuente1, fuente2, fuente3) |> 
      # count(sector)
      mutate(sector = factor(sector, c("Izquierda", "Derecha", "Ninguno"))) |> 
      mutate(
        fuente = case_when(!is.na(fuente3) ~ glue::glue("[1]({fuente1}), [2]({fuente2}), [3]({fuente3})"),
                           !is.na(fuente2) ~ glue::glue("[1]({fuente1}), [2]({fuente2})"),
                           !is.na(fuente1) ~ glue::glue("[1]({fuente1})"), 
                           .default = "Sin fuentes"),
        fuente = purrr::map(fuente, gt::md)) |> 
      select(-fuente1, -fuente2, -fuente3) |> 
      arrange(desc(monto)) |> 
      gt() |> 
      cols_align(columns = where(is.numeric), 
                 align = "right") |> 
      cols_align(columns = comuna, 
                 align = "left") |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_style(locations = cells_body(columns = comuna),
                style = list(
                  cell_fill(color = color_detalle),
                  cell_text(style = "italic"))) |> 
      # tab_style(locations = cells_body(columns = fuente),
      #           style = list(
      #             cell_text(color = color_detalle2))) |> 
      data_color(columns = c(sector), 
                 method = "factor", 
                 domain = c("Derecha", "Izquierda", "Ninguno"), ordered = T, 
                 levels = c("Derecha", "Izquierda", "Ninguno"),
                 palette = c("Derecha" = color_derecha, "Izquierda" = color_izquierda, "Ninguno" = color_fondo)
                 ) |> 
      # data_color(columns = c(sector),
      #            method = "factor", apply_to = "text",
      #            domain = c("Derecha", "Izquierda", "Ninguno"), ordered = T,
      #            levels = c("Izquierda", "Derecha", "Ninguno"),
      #            palette = c("white", "white", color_texto)) |>
      #color partido
      data_color(columns = c(partido, sector), 
                 method = "factor", apply_to = "text",
                 levels = c("Ninguno"),
                 palette = color_detalle2, na_color = color_texto) |> 
      fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
      cols_label(
        comuna = "Municipio",
        año = "Año",
        partido = "Partido político",
        sector = "Sector político",
        responsable = "Alcalde",
        monto = "Monto defraudado",
        fuente = "Fuentes"
      ) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto, 
                  table_body.hlines.color = color_detalle2,
                  table_body.vlines.color = color_detalle2,
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle2, 
                  table_body.border.bottom.color = color_detalle2,
                  table.background.color = color_fondo, 
                  table.font.names = "IBM Plex Mono")
  })
  
  ## tabla fundaciones ----
  output$tabla_fundaciones <- render_gt({
    # browser()
    corrupcion_años() |> 
      filter(!is.na(fundacion)) |> 
      select(fundacion, comuna,  partido, sector, monto, año, fuente1, fuente2, fuente3) |> 
      mutate(
        fuente = case_when(!is.na(fuente3) ~ glue::glue("[1]({fuente1}), [2]({fuente2}), [3]({fuente3})"),
                           !is.na(fuente2) ~ glue::glue("[1]({fuente1}), [2]({fuente2})"),
                           !is.na(fuente1) ~ glue::glue("[1]({fuente1})"), 
                           .default = "Sin fuentes"),
        fuente = purrr::map(fuente, gt::md)) |> 
      select(-fuente1, -fuente2, -fuente3) |> 
      arrange(desc(monto)) |> 
      gt() |> 
      cols_align(columns = where(is.numeric), 
                 align = "right") |> 
      cols_align(columns = fundacion, 
                 align = "left") |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_style(locations = cells_body(columns = comuna),
                style = list(
                  cell_fill(color = color_detalle),
                  cell_text(style = "italic"))) |> 
      data_color(columns = c(sector), 
                 method = "factor",
                 levels = c("Izquierda", "Derecha", "Ninguno"), 
                 palette = c(color_izquierda, color_derecha, color_fondo)) |> 
      data_color(columns = c(sector), 
                 method = "factor", apply_to = "text",
                 levels = c("Izquierda", "Derecha", "Ninguno"), 
                 palette = c("white", "white", color_texto)) |> 
      #color partido
      data_color(columns = c(partido, sector), 
                 method = "factor", apply_to = "text",
                 levels = c("Ninguno"),
                 palette = color_detalle2, na_color = color_texto) |> 
      fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
      cols_label(
        fundacion = "Fundación",
        comuna = "Ubicación",
        año = "Año",
        partido = "Partido político",
        sector = "Sector político",
        monto = "Monto defraudado",
        fuente = "Fuentes"
      ) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto,
                  table_body.hlines.color = color_detalle2,
                  table_body.vlines.color = color_detalle2, 
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle2, 
                  table_body.border.bottom.color = color_detalle2,
                  table.background.color = color_fondo, 
                  table.font.names = "IBM Plex Mono")
  })
  
  
  ## tabla todo ----
  output$tabla_casos <- render_gt({
    req(scroll$abajo)
    
    message("rendering tabla de todo...")
    # browser()
    corrupcion_años() |> 
      select(caso, monto, 
             responsable, 
             partido, sector, fundacion, 
             posicion, perjudicado, 
             año, comuna,
             delitos,
             fuente1, fuente2, fuente3, fuente4) |> 
      mutate(
        fuente = case_when(!is.na(fuente4) ~ glue::glue("[1]({fuente1}), [2]({fuente2}), [3]({fuente3}), [4]({fuente4})"),
                           !is.na(fuente3) ~ glue::glue("[1]({fuente1}), [2]({fuente2}), [3]({fuente3})"),
                           !is.na(fuente2) ~ glue::glue("[1]({fuente1}), [2]({fuente2})"),
                           !is.na(fuente1) ~ glue::glue("[1]({fuente1})"), 
                           .default = "Sin fuentes"),
        fuente = purrr::map(fuente, gt::md)) |> 
      mutate(across(where(is.character), ~tidyr::replace_na(.x, " "))) |> 
      select(-fuente1, -fuente2, -fuente3, -fuente4) |> 
      arrange(desc(monto)) |> 
      gt() |> 
      cols_align(columns = where(is.numeric), 
                 align = "right") |> 
      cols_align(columns = caso, 
                 align = "left") |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_style(locations = cells_body(columns = caso),
                style = list(
                  cell_fill(color = color_detalle),
                  cell_text(style = "italic"))) |> 
      #color sector político
      data_color(columns = c(sector), 
                 method = "factor", apply_to = "fill",
                 levels = c("Izquierda", "Derecha", "Ninguno"), 
                 palette = c(color_izquierda, color_derecha, color_fondo)) |> 
      data_color(columns = c(sector), 
                 method = "factor", apply_to = "text",
                 levels = c("Izquierda", "Derecha", "Ninguno"), 
                 palette = c("white", "white", color_texto)) |> 
      #color partido
      data_color(columns = c(partido, sector), 
                 method = "factor", apply_to = "text",
                 levels = c("Ninguno"),
                 palette = color_detalle2, na_color = color_texto) |> 
      #color otros
      data_color(columns = c(perjudicado), 
                 method = "factor", apply_to = "text",
                 levels = c("Otros"),
                 palette = color_detalle2, na_color = color_texto) |> 
      fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
      cols_label(
        caso = "Caso",
        monto = "Monto defraudado",
        responsable = "Responsable individual",
        fundacion = "Fundación",
        comuna = "Ubicación",
        año = "Año",
        partido = "Partido político",
        sector = "Sector político",
        posicion = "Entidad perjudicante",
        perjudicado = "Entidad perjudicada",
        delitos = "Delitos cometidos",
        fuente = "Fuentes"
      ) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto,
                  table_body.hlines.color = color_detalle2,
                  table_body.vlines.color = color_detalle2, 
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle2, 
                  table_body.border.bottom.color = color_detalle2,
                  table.background.color = color_fondo, 
                  table.font.names = "IBM Plex Mono")
  })
  
  
}


shinyApp(ui = ui, server = server)
