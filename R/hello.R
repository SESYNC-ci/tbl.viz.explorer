library(tidyr)
library(magrittr)
library(dplyr)
library(gapminder)

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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

  plot <- ggplot2::ggplot(df,
            ggplot2::aes_string(x=x, y=y, color=color)) +
            ggplot2::geom_line(stat = "summary", fun.y = "sum", alpha=1)

  if (input$logTransform == TRUE) {
    plot <- plot + ggplot2::coord_trans(y="log10")
  }

  dorows <- dims[dims$var=='facetRowsBy','include'][[1]]
  docols <- dims[dims$var=='facetColsBy','include'][[1]]
  if (dorows && docols) {
    plot <- plot +
      ggplot2::facet_grid(rows=ggplot2::vars(.data[[rows]]),
                          cols=ggplot2::vars(.data[[cols]])) +
      ggplot2::ggtitle(glue::glue('rows {rows}, cols {cols}, color {color}'))
  } else if (dorows) {
    plot <- plot +
      ggplot2::facet_grid(rows=ggplot2::vars(.data[[rows]])) +
      ggplot2::ggtitle(glue::glue('rows {rows}, color {color}'))
  } else if (docols) {
    plot <- plot +
      ggplot2::facet_grid(cols=ggplot2::vars(.data[[cols]])) +
      ggplot2::ggtitle(glue::glue('cols {cols}, color {color}'))
  } else {
    plot <- plot + ggplot2::ggtitle(glue::glue('cols {cols}, color {color}'))
  }
  return(plot)
}
shinyAppWrapper <- function(df, dimParams) {
  df <- tibble::as_tibble(df)   # maybe makes things quicker?

  appServer <- function(input, output, session) {

#    shiny::callModule(
#        module=tbl.viz.explorer::varValueSelectionServer,
#        id="varValSel",
#        session=session,
#        df=df)

    dimsr <- shiny::callModule(
        module=tbl.viz.explorer::dimAssignmentServer,
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
  tbl.viz.explorer::shinyAppWrapper(df=vdata,dimParams=c(x="year", y="pop", facetRowsBy='continent', facetColsBy='continent', colorBy='continent'))
}

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

