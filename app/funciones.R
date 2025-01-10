js_get_window_width <- function() {
  #ancho de ventana
  tags$head(tags$script('
                                var dimension_ventana = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension_ventana[0] = window.innerWidth;
                                    dimension_ventana[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension_ventana", dimension_ventana);
                                });
                                $(window).resize(function(e) {
                                    dimension_ventana[0] = window.innerWidth;
                                    dimension_ventana[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension_ventana", dimension_ventana);
                                });
                            '))
}


# js_get_vertical_position <- function() {
#   # get vertical scroll position using javascript
#   tags$head(
#     tags$script(
#       "$(document).ready(function() {
#           $(document).on('scroll', function() {
#             Shiny.onInputChange('y_offset', $('body').position());
#           });
#         });")
#   ) 
# }
js_get_vertical_position <- function() {
  # get vertical scroll position using javascript
  tags$head(
    tags$script("
    var posicion_y = [0]; 
      $(document).on('scroll', function() {
        posicion_y[0] = window.pageYOffset;
        Shiny.onInputChange('posicion_y', posicion_y);
      });
      ")
  ) 
}
# posicion_y[0] = $('body').position();

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
         # width_svg = ancho,
         # height_svg = alto,
         options = list(opts_hover(css = paste0("fill: ", color_destacado, "; stroke: ", color_destacado, ";")),
                        opts_tooltip(opacity = 0.8, css = paste0("background-color: ", color_fondo, "; color: ", color_texto, "; padding: 4px; max-width: 200px; border-radius: 4px; font-size: 80%;")),
                        opts_sizing(rescale = TRUE), opts_toolbar(hidden = "selection", saveaspng = FALSE)))
}


mutate_link_fuentes <- function(data) {
  data |> 
    mutate(
      link_fuente = case_when(!is.na(fuente4) ~ paste0("[", fuente1_sitio, "](", fuente1, "), [", fuente2_sitio, "](", fuente2, "), [", fuente3_sitio, "](", fuente3, "), [", fuente4_sitio, "](", fuente4, ")"),
                              !is.na(fuente3) ~ paste0("[", fuente1_sitio, "](", fuente1, "), [", fuente2_sitio, "](", fuente2, "), [", fuente3_sitio, "](", fuente3, ")"),
                              !is.na(fuente2) ~ paste0("[", fuente1_sitio, "](", fuente1, "), [", fuente2_sitio, "](", fuente2, ")"),
                              !is.na(fuente1) ~ paste0("[", fuente1_sitio, "](", fuente1, ")"),
                              .default = "Sin fuentes"))
}