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

# ' @include dimAssignments.R
#' @include df-browse.R

ui <- shiny::fluidPage(
        browseUI("browse"),
        shiny::checkboxInput("logTransform", "Log Transform", FALSE),
        shiny::tableOutput("dims"),
        shiny::plotOutput("mainPlot")
        #dimAssignmentInput("dimAssignment")
)
  #tbl.viz.explorer::csvInput("someId"),
  #shiny::titlePanel(label),
#  shiny::sidebarLayout(
#    shiny::sidebarPanel(
#      shiny::tags$div(id = 'browse-module-container'),
#      shiny::checkboxInput("logTransform", "Log Transform", FALSE)
#    ),
#    shiny::mainPanel(
#      browseUI("browse"),
#      shiny::hr(),
#      #shiny::plotOutput("slPlot"),
#      shiny::plotOutput("mainPlot"),
#      shiny::tableOutput("dftable")

# use this for brushing!!!
#      shiny::h4("linkedScatterInput:"),
#      linkedScatterUI("scatters"),
#      shiny::textOutput("summary"),
#      shiny::p("after summary")
      #shiny::plotOutput("view")
#    )
#  )
shinyAppWrapper <- function(df, colConf_, mapConf_, logTransform=F ) {
  df <- tibble::as_tibble(df)   # maybe makes things quicker?

  # above trying to take place of GlobalData = callModule(GlobalModule, "globals")
  # from example code
  # from: https://community.rstudio.com/t/best-practices-for-global-external-variables-used-in-a-module/5820/2

  appServer <- function(input, output, session) {

    ConfData <- shiny::callModule(
      module=ConfigManager,
      id='confMgr',
      session=session,
      colConf_=colConf_,
      mapConf_=mapConf_)

    nuthin <- shiny::callModule(
        module=tbl.viz.explorer::browse, 
        id="browse", 
        session=session,
        ConfData=ConfData,
        df=df)

#    dimsr <- shiny::callModule(
#        module=tbl.viz.explorer::dimAssignmentServer,
#        id="dimAssignment",
#        session=session,
#        df=df )
#
#    #print(dimsr)
#    output$mainPlot <- renderPlot({
#      makePlot(df=df, dims=dimsr(), input=input, plotFunc=linePlot)
#    })
    output$dftable <- shiny::renderTable(head(df))
#    output$dims <- shiny::renderTable(dimsr())

    #shinyBS::addPopover(session, "mainPlot", "Data", content = "should be in a popover", trigger = 'click')

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

  cd <- data.frame(
    year      =c( label='Year',           type='ordinal'), 
    continent =c( label='Continent',      type='factor'),
    country   =c( label='Country',        type='factor'),
    lifeExp   =c( label='Life Expectancy',type='numeric'),
    pop       =c( label='Population',     type='numeric'), 
    gdpPercap =c( label='GDP per capita', type='numeric')
  )

  dfcd <- data.frame(
    year.x          = c( colId='year',      dispId='x'), 
    continent.rows  = c( colId='continent', dispId='rows'),
    continent.color = c( colId='continent', dispId='color') 
  )

  tbl.viz.explorer::shinyAppWrapper(df=vdata, colConf_=cd, mapConf_=dfcd)
}

appDemo <- function() {
  print("don't forget to hit shift-ctrl-B to rebuild package")
  print("and shift-ctrl-L to reload stuff")
  print("running: shiny::runApp(testApp(loadTestData()))")
  #shiny::runApp(testApp(loadTestData()), display.mode="showcase")
  shiny::runApp(testApp(loadTestData()))
}

#' @export
csvDemo <- function(){
  loc <- system.file("examples","moduleExample", package = "tbl.viz.explorer")
  shiny::shinyAppDir(loc)
}

