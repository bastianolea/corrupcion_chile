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