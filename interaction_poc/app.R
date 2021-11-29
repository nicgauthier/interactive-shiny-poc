#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(data.table)
library(plotly)
library(purrr)
library(readr)

iris_dt <- as.data.table(iris)[sample(1:.N, 25)]
iris_dt[ , weight := 1]

draw_dt <- data.table(x = runif(10))
draw_dt[ , y := x + runif(10)/10]
draw_dt[ , weight := 1]

# Define UI for application that draws a histogram
ui <- fluidPage(
  DTOutput("input_dt"),
  plotlyOutput("model_plot"),
  plotlyOutput("draw_plot"),
  DTOutput("draw_table"),
  #DTOutput("post_dt"),
  textOutput("model_summary")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$input_dt <- DT::renderDT(datatable(iris_dt, editable = list(target  = 'cell', disable = list(columns = 1:(ncol(iris_dt)-1))), selection = list(target = 'row')))
  
  train_dt <- reactive({
    if (length(input$input_dt_rows_selected) == 0) {
      iris_dt[0]
    } else {
      iris_dt[input$input_dt_rows_selected]
    }
  })
  
  #output$post_dt <- renderDT(data.table(train_dt()))
  
  model_obj <- reactive({
    lm(Sepal.Length ~ Petal.Length, data = train_dt(), weights = train_dt()$weight)
  })
  
  output$model_plot <- renderPlotly({
    if (length(input$input_dt_rows_selected) == 0) {
      plot_ly(iris_dt) %>%
        add_trace(x = ~Petal.Length, y = ~Sepal.Length, type = 'scatter', mode = "markers")
    }
    else {
      plot_ly(iris_dt) %>%
        add_trace(x = ~Petal.Length, y = ~Sepal.Length, type = 'scatter', mode = "markers") %>%
        add_trace(x = seq(1,7,0.5), y = predict(model_obj(), newdata = data.table(Petal.Length = seq(1,7,0.5))), type = 'scatter', mode = 'lines')
    }
   
  })
  


  
  output$model_summary <- renderPrint({
    event_data("plotly_relayout")
  })
  
  rv <- reactiveValues(
    x = draw_dt$x,
    y = draw_dt$y,
    w = draw_dt$weight
  )
  
  draw_dt <- reactive(data.table(x = rv$x, rv$y))
  
  grid <- reactive({
    data.frame(x = seq(min(rv$x), max(rv$x), length = 10))
  })
  model <- reactive({
    d <- data.frame(x = rv$x, y = rv$y)
    lm(y ~ x, d, weights = rv$w)
  })
  
  output$draw_plot <- renderPlotly({
    
    # creates a list of circle shapes from x/y data
    circles <- map2(rv$x, rv$y, 
                    ~list(
                      type = "circle",
                      # anchor circles at (mpg, wt)
                      xanchor = .x,
                      yanchor = .y,
                      # give each circle a 2 pixel diameter
                      x0 = -4, x1 = 4,
                      y0 = -4, y1 = 4,
                      xsizemode = "pixel", 
                      ysizemode = "pixel",
                      # other visual properties
                      fillcolor = "blue",
                      line = list(color = "transparent")
                    )
    )
    
    for (i in seq_along(circles)) {
      circles[[i]]$x0 <- circles[[i]]$x0 * rv$w[i] 
      circles[[i]]$x1 <- circles[[i]]$x1 * rv$w[i]
      circles[[i]]$y0 <- circles[[i]]$y0 * rv$w[i]
      circles[[i]]$y1 <- circles[[i]]$y1 * rv$w[i]
    }

    if (length(input$draw_table_rows_selected) > 0) {
      circles[[input$draw_table_rows_selected]]$fillcolor <- "orange"
    }

    
    # plot the shapes and fitted line
    plot_ly() %>%
      add_lines(x = grid()$x, y = predict(model(), grid()), color = I("red")) %>%
      layout(shapes = circles) %>%
      config(edits = list(shapePosition = TRUE))
  })
  
  output$summary <- renderPrint({a
    summary(model())
  })
  
  # update x/y reactive values in response to changes in shape anchors
  observeEvent(event_data("plotly_relayout"),{
    ed <- event_data("plotly_relayout")
    print(ed)
    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
    if (length(shape_anchors) != 2) return()
    row_index <- unique(parse_number(names(shape_anchors)) + 1)

    pts <- as.numeric(shape_anchors)
    rv$x[row_index] <- pts[1]
    rv$y[row_index] <- pts[2]
  })
  
  output$draw_table <- renderDT({
    datatable(data.table(x = rv$x, y = rv$y, weight = rv$w), editable = T, selection = list(mode = 'single', target = 'row'))
  })
  
  proxy <- dataTableProxy('draw_table')
  
  observeEvent(input$draw_table_cell_edit, {
    info = input$draw_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    if (j == 1) {
      rv$x[i] <- DT::coerceValue(v, rv$x[i])
    }
    if (j == 2) {
      rv$y[i] <- DT::coerceValue(v, rv$y[i])
    }
    if (j == 3) {
      rv$w[i] <- DT::coerceValue(v, rv$w[i])
    }
    
    temp_selected <- input$draw_table_rows_selected
    
    replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')  # important

  })
  
   

}

# Run the application 
shinyApp(ui = ui, server = server)
