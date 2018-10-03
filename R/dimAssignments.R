dispFields = dplyr::tibble(
  var = c('x', 'y', 'facetRowsBy', 'facetColsBy', 'colorBy')
          , label = c('X', 'Y', 'Facet rows by', 'Facet cols by', 'Color')
          , required = c(T, T, F, F, T)
          , include = c(T, T, F, F, T)
)
getDefaultsTibble <- function(...) { tidyr::gather(dplyr::tibble(...))}
dimAssignmentInput <- function(id, label="Assign data dims to display dims") {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dataDimSelect"))
}

dimAssignmentServer <- function(input, output, session, df, dimParams) {
  dims <- dispFields
  choices <- names(df)

  selectVal <- reactive({
    # selectVal is a reactive that contains a named vector or list of functions
    # (named for each dimParam) 
    # each of those functions retrieves the value for that param from input
    # if it hasn't been set in input, get it from default dimParams instead
    mapply(function(dn) 
            function() ifelse(length(input[[dn]]), input[[dn]], dimParams[[dn]]),
            dispFields$var)
  })
  getDimAssignments <- function() {
    dims$default <- vapply(dims$var, function(dn) dimParams[[dn]], 'a')
    dims$val <- vapply(dims$var, function(dn) selectVal()[[dn]](), 'a')
    dims$include <- vapply(
        dims$var, 
        function(dn) {
          id <- glue::glue('include_{dn}')
          ifelse(length(input[[id]]), input[[id]], dims[dims$var==dn,'include'][[1]])
        }, F)
    return(dims)
  }
  dimAssignments <- reactive({
    getDimAssignments()
  })

  ns <- session$ns
  addSelectField <- function(dimRow, els) {
    selectField <- shiny::selectInput(
                          inputId = ns(dimRow$var),
                          label = dimRow$label,
                          choices = choices,
                          selected = dimRow$val
                        )
    if(!dimRow['required']) {
      cbId <- glue::glue('include_{dimRow$var}')
      els <- c(els,
        list(shiny::checkboxInput(ns(cbId), label = dimRow$label, value = dimRow$include)))
      # shinyjs::disable(cbId)   not working
      # wanted to add for required but disable
    }
    els <- c(els, list(selectField))
  }
  selectBoxes <- reactive({
    dims <- dimAssignments()
    formElements <- list()
    for(i in 1:nrow(dims)) {
      formElements <- addSelectField(dims[i,], formElements)
    }
    print(formElements)
    formElements
  })
  output$dataDimSelect <- shiny::renderUI({
    shiny::tagList(
      selectBoxes()
    )
  })
  return(dimAssignments)
}

linkedScatterUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    #shiny::uiOutput(ns("stuff")),
    #shiny::selectInput(ns("col2"), "Columns again", choices=c('a','b','c'), multiple = TRUE),
    shiny::plotOutput(ns("plot1"), brush = ns("brush")),
    shiny::plotOutput(ns("plot2"), brush = ns("brush"))
  )
}
linkedScatter <- function(input, output, session, data, left, right) {
  cat('\nin linkedScatter\n')
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  dataWithSelection <- shiny::reactive({
    shiny::brushedPoints(data(), input$brush, allRows = TRUE)
  })

  output$plot1 <- shiny::renderPlot({
    scatterPlot(dataWithSelection(), left())
  })

  output$plot2 <- shiny::renderPlot({
    scatterPlot(dataWithSelection(), right())
  })
#  output$stuff <- shiny::renderUI({
#    ns <- session$ns
#    selectInput(ns("col"), "Columns", choices=names(dataWithSelection()), multiple = TRUE)
#  })
#  output$choices <- names(data)
  # from renderUI example (https://shiny.rstudio.com/articles/modules.html)
  # i don't understand what it's doing
  # also, selectInput is just a text box right now, don't know why
#  return(reactive({
#    validate(need(input$col, FALSE))
#    data[,input$col]
#  }))

  return(dataWithSelection)
}

scatterPlot <- function(data, cols) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = cols[1], y = cols[2])) +
    ggplot2::geom_point(ggplot2::aes(color = selected_)) +
    ggplot2::scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}







kmeansClusInput <- function(id, label) {
  ns <- shiny::NS(id)

  col <- colorPickerInput(ns("col"), "Color scheme")
  tags <- shiny::tagList(
    shiny::selectInput(ns('xcol'), 'X Variable', names(iris)),
    shiny::selectInput(ns('ycol'), 'Y Variable', names(iris), selected=names(iris)[[2]]),
    shiny::numericInput(ns('clusters'), 'Cluster count', 3, min = 1, max = 9))

  shiny::fluidRow(
    shiny::column(4, tags, col),
    shiny::column(8, shiny::plotOutput(ns("plot1")))
  )
}

kmeansClus <- function(input, output, session) {
  innerResult <- callModule(colorPicker, "col", bar="from kmeansClus")

  # Combine the selected variables into a new data frame
  selectedData <- shiny::reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    cols <- RColorBrewer::brewer.pal(input$clusters, innerResult$scheme)[clusters()$cluster]
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = cols,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

}


# Module UI function
colorPickerInput <- function(id, label = "Coloring", params ) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("scheme"), label = label, choices = c("Dark2" = "Dark2", "Set1" = "Set1", "Set2" = "Set2"), selected = "Dark2"),
    shiny::checkboxInput(ns("reverse"), label = "Reverse scheme"),
    shiny::sliderInput(ns("transparency"), label = "Transparency", min = 0, max = 1, value = 1)
  )
}

# Module server function
colorPicker <- function(input, output, session, bar) {
  shiny::observe({
    msg <- glue::glue("{bar} Color scheme was selected {input$scheme}")
    cat(msg, "\n")
  })
  return(input)
}




