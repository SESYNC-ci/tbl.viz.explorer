#' @include selectLevels.R
ui <- shiny::fluidPage(
  #tbl.viz.explorer::csvInput("someId"),
  #shiny::titlePanel(label),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      dimAssignmentInput("dimAssignment"),
      shiny::checkboxInput("logTransform", "Log Transform", FALSE)
    ),
    shiny::mainPanel(
      #shiny::plotOutput("slPlot"),
      shiny::plotOutput("mainPlot"),
      #varValueSelectionInput("varValSel"),
      shiny::tableOutput("dftable")

# use this for brushing!!!
#      shiny::h4("linkedScatterInput:"),
#      linkedScatterUI("scatters"),
#      shiny::textOutput("summary"),
#      shiny::p("after summary")
      #shiny::plotOutput("view")
    )
  )
)

#' @importFrom ggplot2 ggplot aes_string geom_line coord_trans facet_grid ggtitle vars
#' @importFrom glue glue
getPlot <- function(df, dims, input) {
  print(dims)
  d <- dims$val
  names(d) <- dims$var
  x <- d[['x']]
  y <- d[['y']]

  color <- d[['colorBy']]
  rows <- d[['facetRowsBy']]
  cols <- d[['facetColsBy']]
  print(c(x,y,color, rows,cols))
  print(nrow(df))

  plot <- ggplot(df,
            aes_string(x=x, y=y, color=color)) + # FIXME color does not respond to check box
            geom_line(stat = "summary", fun.y = "sum", alpha=1)

  if (input$logTransform == TRUE) {
    plot <- plot + coord_trans(y="log10")
  }

  dorows <- dims[dims$var=='facetRowsBy','include'][[1]]
  docols <- dims[dims$var=='facetColsBy','include'][[1]]
  if (dorows && docols) {
    plot <- plot +
      facet_grid(rows=vars(.data[[rows]]),
                 cols=vars(.data[[cols]])) +
      ggtitle(glue('rows {rows}, cols {cols}, color {color}'))
  } else if (dorows) {
    plot <- plot +
      facet_grid(rows=vars(.data[[rows]])) +
      ggtitle(glue('rows {rows}, color {color}'))
  } else if (docols) {
    plot <- plot +
      facet_grid(cols=vars(.data[[cols]])) +
      ggtitle(glue('cols {cols}, color {color}'))
  } else {
    plot <- plot + ggtitle(glue('cols {cols}, color {color}'))
  }
  return(plot)
}

#' @importFrom tibble as_tibble
shinyAppWrapper <- function(df, dimParams) {
  df <- as_tibble(df)   # maybe makes things quicker?

  appServer <- function(input, output, session) {

#    shiny::callModule(
#        module=tbl.viz.explorer::varValueSelectionServer,
#        id="varValSel",
#        session=session,
#        df=df)

    dimsr <- shiny::callModule(
        module=dimAssignmentServer,
        id="dimAssignment",
        session=session,
        df=df, dimParams=dimParams)

    print(dimsr)
    output$mainPlot <- renderPlot({
      getPlot(df, dimsr(), input)
    })
    output$dftable <- shiny::renderTable(head(df))

    #output$slPlot <- renderPlot({p})
    #output$dimtable <- shiny::renderTable(da$dims())
#      tbl.viz.explorer::dimAssignmentInput(ns("dimAssignment"), "Pick dim assignments",
#                                              params=c(df,dimParams)),
# bring this back to figure out brushing:
#    dfl <- callModule(tbl.viz.explorer::linkedScatter, "scatters", reactive(ggplot2::mpg),
#      left = reactive(c("cty", "hwy")),
#      right = reactive(c("drv", "hwy"))
#    )

    #output$data <- renderTable(dfl()[1:5,])
#    output$summary <- renderText({
#      sprintf("%d observation(s) selected", nrow(dplyr::filter(dfl(), selected_)))
#    })
  }
  return(shiny::shinyApp(ui, appServer))
}

# test with:
#   hit shift-ctrl-B to rebuild
#   shiny::runApp(testApp(loadTestData()))
loadTestData <- function() {
  gapminder
}

testApp <- function(vdata) {
  shinyAppWrapper(df=vdata,dimParams=c(x="year", y="pop", facetRowsBy='continent', facetColsBy='continent', colorBy='continent'))
}

#' @export
appDemo <- function() {
  print("don't forget to hit shift-ctrl-B to rebuild package")
  print("running: shiny::runApp(testApp(loadTestData()))")
  shiny::runApp(testApp(loadTestData()))
}

#' @export
csvDemo <- function(){
  loc <- system.file("examples","moduleExample", package = "tbl.viz.explorer")
  shiny::shinyAppDir(loc)
}
