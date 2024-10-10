js_get_window_width <- function() {
  #ancho de ventana
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
                            '))
}


js_get_vertical_position <- function() {
  # get vertical scroll position using javascript
  tags$head(
    tags$script(
      "$(document).ready(function() {
          $(document).on('scroll', function() {
      Shiny.onInputChange('y_offset', $('body').position());
          });
        });")
  ) 
}
# 
# js_set_input_vertical_position <- function() {
#   # write scroll position into shiny input
#   observeEvent(input$y_offset, {
#     runjs("Shiny.setInputValue('y_offset', window.pageYOffset);")
#   })
# }

boton_descarga_imagen <- function(texto = "Descargar gráfico", 
                                  enlace = "graficos/grafico_torta_montos_sector.png") {
  tags$a(
    div(style = paste0("background-color:", "#272E2B", ";
                              color:", color_texto, "; 
                              font-size: 80%;
                              min-width: 200px; max-width: 240px;
                              min-height: 32px;
                              padding: 10px; margin: auto; 
                              border-radius: 4px;
                              margin-bottom: 20px; margin-top: 20px;"),
        div(style = "text-align: center;",
            div(texto)
        )
    ),
    href = enlace, target = "_blank")
}


girafear <- function(objeto, alto = 8, ancho = 7) {
  girafe(ggobj = objeto,
         bg = color_fondo,
         width_svg = ancho,
         height_svg = alto,
         options = list(opts_hover(css = paste0("fill: ", color_destacado, "; stroke: ", color_destacado, ";")),
                        opts_tooltip(opacity = 0.8, css = paste0("background-color: ", color_fondo, "; color: ", color_texto, "; padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")),
                        opts_sizing(rescale = TRUE), opts_toolbar(hidden = "selection", saveaspng = FALSE)))
}