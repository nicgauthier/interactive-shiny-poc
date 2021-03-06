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

valueBox <- function(value, subtitle, icon, color) {
  div(class = "row col-md-12",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}


validate_user_csv <- function(user_csv) {
  names_csv <- names(user_csv)
  
  if(user_csv[ , .N] == 0) {
    return(list(valid = FALSE,
                message = paste0("csv contains no data points")))
  }
  
  if(!all(names_csv %in% c("x", "y", "weight"))) {
    return(list(valid = FALSE,
                message = paste0("csv does not contain all necessary column \n", "missing column(s) : ", names_csv[!(names_csv %in% c("x", "y", "weight"))])))
  }
  
  typeof_csv <- user_csv[ , lapply(.SD, function(x) typeof(x) %in% c("integer", "double"))]
  if (!all(typeof_csv)) {
    return(list(valid = FALSE,
                message = paste0("type of following columns :", names_csv[!typeof_csv], "\n is not either an integer or a double")))
    
  }
  
  which_neg_weight <- which(user_csv$weight < 0)
  if (length(which_neg_weight) > 0) {
    if (length(which_neg_weight) == 1) {
      return(list(valid = FALSE,
                  message = paste0("weight value for line number ", which_neg_weight, " is negative")))
    } else {
      return(list(valid = FALSE,
                  message = paste0("weight value for lines number ", which_neg_weight, " are negative")))
    }
  }
  
  return(list(valid = TRUE,
              message = "csv passed all checks"))
}

draw_dt <- data.table(x = runif(10))
draw_dt[ , y := x + runif(10)/10]
draw_dt[ , weight := 1]

marker_radius <- 9

help_dialog <- modalDialog(
  h1("Two-way interaction between plots and tables"),
  p("Hi, welcome to this demonstration of two-way interactions between plots and tables in Shiny!"),
  p("The goal of this app is to show some of the ways you can interact with your data through tables and plots and how they can share information with each others"),
  p("We will demonstrate how you can interact with your data in the context of analysing the impact of data points on the result of a linear model"),
  h3("Selecting data points"),
  h4("table interaction"),
  p("you can select a data point by clicking anywhere on the corresponding row of the table"),
  p("Selected lines will be highlighted in blue."),
  p("You can select multiple lines and you can deselect any lines by clicking on it again,"),
  h5(strong("TRY IT!")),
  p("Notice how when you select a line in the table it affects the color of the corresponding data points in the plot"),
  p("This allow you to quickly identify which lines correspond to which data points"),
  h4("plot interaction") ,
  p("If you click and drag anywhere on the plot , you'll see a dotted box appear."),
  p("You can click and drag to create a box around any data points to select them. Any points that are within the box area will be selected."),
  h5(strong("TRY IT!")),
  p("Once again, the selected data points will change color to give visual feedback about their current state"),
  p("You can deselect data points by selecting an empty area of the plot"),
  p("If look at the table, you'll see that the corresponding rows of the points you have selected are now highlighted in blue"),
  p("That's what two-way interaction looks like , you can select data points on both the table and the plot and both will always be updated"),
  h3("Adding and deleting data points"),
  p("Once you have selected data points you can delete them if you want."),
  p("Select some data points either using the table or the plot and then press the button labeled 'remove data point(s)'"),
  p("Notice how the point have been deleted on both the table and the plot."),
  p("You can also add new data point by pressing the button labeled 'add data point'"),
  p("Notice how newly added data point are automatically selected to allow you to identify them quickly"),
  h3("Change the coordinates of a data point"),
  h4("Table interaction"),
  p("You can change the coordinates of any data point by editing the values of any cells in the column 'x' and 'y which represent the coordinate of each data points."),
  p("To edit a cell double click on an existing number, an editing box should appear allowing to change the value, click anywhere outside the box to confirmed the newly entered value."),
  p("Notice how if affects the position of the data point on the plot in real time!"),
  p("You can also the impact of the new coordinates on the linear model in real time."),
  h4("plot interaction"),
  p("The most intuitive way to change the coordinate of a data point when interacting with a plot is to simply grab the data point and move it around"),
  h5(strong("TRY IT!")),
  p("grab any point by moving your cursor to it and clicking and holding your mouse left-click button, move the data point around and let go your left-click button when your happy with the new coordinates"),
  p("Notice how the table is updated with the new coordinates automatically"),
  p("This is once again an example of two-way interaction where you can update the coordinates values either on the table or on the plot and both are always in sync with each others"),
  h3("Editing weight"),
  h4("Table interaction"),
  p("You can edit the weight of any point the same way you did for the coordinates in the table by double-clicking on any weight values in the table and input the new value"),
  h5(strong("TRY IT!")),
  p("Notice how the size of the corresponding marker has changed according to the newly inputed weight value!"),
  h4("Plot interaction"),
  p("If you place your cursor, directly on the perimeter of any markers in the plot, you'll notice that your cursor will transform into a two-sided arrow."),
  p("If you click and hold the left-click button of your mouse and move in any direction, you will be able to resize the marker."),
  p("Once you let go the left-click button of your mouse, the weight value for this data point will be updated."),
  p("Notice how the new weight value is updated in the table and how it affected the result of the linear model"),
  size = "l",
  easyClose = TRUE
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
  actionButton("add_point", "add new data point"),
  actionButton("remove_point", "remove data point(s)"),
  downloadButton("save_session", "save session", icon = icon("arrow-down")),
  actionButton("load_session", "load session", icon = icon("arrow-up")),
  actionButton("show_help", "instructions", icon = icon("question")),
  fileInput("input_csv", label = "", buttonLabel = "Import points from...", placeholder = "csv file", accept = '.csv'),
  plotlyOutput("draw_plot", height = "600px"),
  br(),
  br(),
  DTOutput("draw_table"),
  br(),
  valueBox(value = 'rmse_box', subtitle = "RMSE", icon =  'tachometer-alt', color = "#fcba03")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  observeEvent(input$input_csv, {
    user_csv <- fread(input$input_csv$datapath)
    csv_check <- validate_user_csv(user_csv)
    if (csv_check$valid) {
      rv$x <- user_csv$x
      rv$y <- user_csv$y
      rv$w <- user_csv$weight
      replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')  # important
    } else {
      showModal(modalDialog(
        title = "Error when importing csv",
        paste0(csv_check$message, "\n the csv was not imported"),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  
  })
  
  rv <- reactiveValues(
    x = draw_dt$x,
    y = draw_dt$y,
    w = draw_dt$weight,
    selected_lines = NULL,
    ci_visible = FALSE
  )
  
  grid <- reactive({
    data.table(x = seq(min(rv$x), max(rv$x), length = 10))
  })
  
  model <- reactive({
    d <- data.table(x = rv$x, y = rv$y)
    lm(y ~ x, d, weights = rv$w)
  })
  
  output$draw_plot <- renderPlotly({

    validate(
      need(length(rv$x) > 0, "use the button to add new data points")
    )
    # creates a list of circle shapes from x/y data
    circles <- map2(rv$x, rv$y, 
                    ~list(
                      type = "circle",
                      # anchor circles at (mpg, wt)
                      xanchor = .x,
                      yanchor = .y,
                      # give each circle a 2 pixel diameter
                      x0 = -marker_radius, x1 = marker_radius,
                      y0 = -marker_radius, y1 = marker_radius,
                      xsizemode = "pixel", 
                      ysizemode = "pixel",
                      # other visual properties
                      fillcolor = '#398ded',
                      line = list(color = "transparent")
                    )
    )
    
    for (i in seq_along(circles)) {
      circles[[i]]$x0 <- circles[[i]]$x0 * rv$w[i] 
      circles[[i]]$x1 <- circles[[i]]$x1 * rv$w[i]
      circles[[i]]$y0 <- circles[[i]]$y0 * rv$w[i]
      circles[[i]]$y1 <- circles[[i]]$y1 * rv$w[i]
    }

    if (!is.null(rv$selected_lines[1])) {
      for (i in rv$selected_lines) {
        circles[[i]]$fillcolor <- "#fcba03"
      }
    
    }

    
    # plot the shapes and fitted line
    p <- plot_ly(data = as.data.frame(cbind(x = grid()$x, predict(model(), grid(), interval = "confidence")))) %>%
        add_lines(x = ~x, y = ~fit, color = I("#b75de8"), name = "LM Fit") %>%
        layout(shapes = circles, dragmode = "select") %>%
        config(edits = list(shapePosition = TRUE)) %>%
        event_register("plotly_brushed") %>%
        event_register('plotly_legendclick') 
    
    if (rv$ci_visible) {
      p %>% add_ribbons(x = ~x, ymin = ~lwr, ymax = ~upr, name = "LM 95% CI", line = list(width=0), fillcolor='rgba(183,93,232,0.2)', visible = TRUE)
    } else {
      p %>% add_ribbons(x = ~x, ymin = ~lwr, ymax = ~upr, name = "LM 95% CI", line = list(width=0), fillcolor='rgba(183,93,232,0.2)', visible = "legendonly") 
    }
    
    
  })
  
  # update x/y reactive values in response to changes in shape anchors
  observeEvent(event_data("plotly_relayout"),{
    ed <- event_data("plotly_relayout")
    shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
    shapes <- ed[grepl("^shapes.*[0-1]$", names(ed))]
 
    if (length(shape_anchors) == 2) {
      row_index <- unique(parse_number(names(shape_anchors)) + 1)
      pts <- as.numeric(shape_anchors)
      rv$x[row_index] <- pts[1]
      rv$y[row_index] <- pts[2]
      
      replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')  # important
      
    }
    
    if (length(shapes) == 4) {
      row_index <- unique(parse_number(names(shapes)) + 1)
      pts <-  as.numeric(shapes)
      delta_x <- pts[4] - pts[3]
      delta_y <- pts[1] - pts[2]
      max_delta <- max(delta_x, delta_y)
      rv$w[row_index] <- (max_delta/2)/marker_radius
      replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')  # important
      
    }
   
  })
  
  observeEvent(event_data("plotly_legendclick"), {
    ed_leg_click <- event_data("plotly_legendclick")
    if (ed_leg_click$name == "LM 95% CI") {
      if (ed_leg_click$visible == "legendonly") {
        rv$ci_visible <- TRUE
      } else {
        rv$ci_visible <- FALSE
      }
    }
  })
  
  observeEvent(event_data("plotly_brushed", priority = 'event'),{
    ed_brushed <- event_data("plotly_brushed")
    rv$selected_lines <- which(rv$x >= ed_brushed$x[1] & rv$x <= ed_brushed$x[2] & rv$y >= ed_brushed$y[1] & rv$y <= ed_brushed$y[2])
    selectRows(proxy, rv$selected_lines)
  })

  
  output$draw_table <- renderDT({
    datatable(draw_dt, editable = T, selection = list(mode = 'multiple', target = 'row'), options = list(pageLength = -1, dom = 't',  initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#398ded', 'color': 'white'});",
      "}")))
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
    
    replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')  # important

  })
  

  
  observeEvent(input$draw_table_rows_selected,{
    rv$selected_lines <- input$draw_table_rows_selected
  })
  
  observeEvent(input$add_point, {
    rv$x <- c(rv$x, 0.5)
    rv$y <- c(rv$y, 0.5)
    rv$w <- c(rv$w, 1)
    rv$selected_lines <- length(rv$x)
    selectRows(proxy, rv$selected_lines)
    replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')  # important
  
  })
  
  observeEvent(input$remove_point, {
    rv$x <- rv$x[-rv$selected_lines]
    rv$y <- rv$y[-rv$selected_lines]
    rv$w <- rv$w[-rv$selected_lines]
    replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'all')  # important
    rv$selected_lines <- NULL
  })
  
  observeEvent(input$show_help, {
    showModal(help_dialog)
  })
  
  observeEvent(input$load_session, {
    showModal(
      modalDialog(
        title = "Load previous session data",
        fileInput(inputId = "session_upload", label =  "Session data (json)",
                  accept = c(".json"), buttonLabel = "Browse...", placeholder = "session file"),
        easyClose = TRUE
      )
    )
  })

  observeEvent(input$session_upload, {
    loaded_sesh <- jsonlite::fromJSON(input$session_upload$datapath, simplifyVector = TRUE)
    rv$x <- loaded_sesh$x
    rv$y <- loaded_sesh$y
    rv$w <- loaded_sesh$w
    rv$selected_lines <- loaded_sesh$selected_lines
    rv$ci_visible <- loaded_sesh$ci_visible
    replaceData(proxy, data.table(x = rv$x , y = rv$y, weight = rv$w), resetPaging = FALSE, clearSelection = 'none')
    removeModal()
  })

  output$save_session <- downloadHandler(
    filename = "session.json",
    content = function(file) {
      jsonlite::write_json(list(x = rv$x, y = rv$y, w = rv$w, selected_lines = rv$selected_lines, ci_visible = rv$ci_visible), file, auto_unbox = TRUE)
    }
  )
  
  rmse <- reactive({
    sqrt(sum(predict(model(), data.table(x = rv$x)) - rv$y)^2)
  })
  
  output$rmse_box <- renderText({
    if (length(rv$x) == 0) {
      return("No model provided")
    } else {
      rmse()
      
    }
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
