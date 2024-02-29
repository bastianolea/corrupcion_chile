library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  # get vertical scroll position using javascript
  tags$head(
    tags$script(
      "$(document).ready(function() {
          $(document).on('scroll', function() {
      Shiny.onInputChange('y_offset', $('body').position());
          });
        });")
  ),
  
  div(h2("Example app")
  ),
  div(
    textOutput("filler_text")
  )
)


server <- function(input, output) {
  
  # fill the page with a lot of text
  output$filler_text <- renderText({
    lorem_ipsum <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
do eiusmod tempor incididunt ut labore et dolore magna aliqua. Neque convallis a cras semper
auctor neque vitae tempus. Morbi tristique senectus et netus et malesuada fames ac. Gravida neque
convallis a cras. Semper quis lectus nulla at volutpat diam ut venenatis. Amet consectetur
adipiscing elit pellentesque. In nulla posuere sollicitudin aliquam ultrices sagittis orci a
scelerisque. Nisi vitae suscipit tellus mauris. Ut aliquam purus sit amet luctus venenatis lectus
magna. Diam quam nulla porttitor massa id neque aliquam."
    
    paste(rep(lorem_ipsum, 60), 
          collapse = "\n")
  })
  
  # write scroll position into shiny input
  observeEvent(input$y_offset, {
    runjs("Shiny.setInputValue('y_offset', window.pageYOffset);")
  })
  
  # get input and check if its valid before using it
  vertical <- reactive({
    req(length(input$y_offset) > 0,
        is.numeric(input$y_offset)
    )
    input$y_offset[1]
  })
  
  # debounce input, so that you dont get it updated dozens of time per second. 
  # instead, settle in the value it gets 500 milliseconds after the scrolling stops
  vertical_d <- vertical |> debounce(500)
  
  # print a message in console with vertical screen position
  observe({
    message("vertical scroll position debounced: ", vertical_d())
  })
  
  # display notifications in the browser after some milestones are reached
  observeEvent(vertical_d(), {
    if (vertical_d() < 100) showNotification("You are back at the top of the screen", type = "message")
    
    if (vertical_d() > 100 & vertical_d() < 1000) showNotification("You just scrolled a bit")
    
    if (vertical_d() > 1000 & vertical_d() < 3000) showNotification("You are far down", type = "error")
    
    if (vertical_d() > 3000) showNotification("You have scrolled a lot", type = "error")
  })
}

shinyApp(ui = ui, server = server)