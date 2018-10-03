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
)

#' @importFrom tibble as_tibble
#' @keywords internal
shinyAppWrapper <- function(df, colConf_, mapConf_, logTransform=F ) {
  df <- as_tibble(df)   # maybe makes things quicker?

  # above trying to take place of GlobalData = callModule(GlobalModule, "globals")
  # from example code
  # from: https://community.rstudio.com/t/best-practices-for-global-external-variables-used-in-a-module/5820/2

  appServer <- function(input, output, session) {

    confData <- shiny::callModule(
      module=ConfigManager,
      id='confMgr',
      session=session,
      colConf_=colConf_,
      mapConf_=mapConf_)

    nuthin <- shiny::callModule(
        module=browse,
        id="browse",
        session=session,
        confData=confData,
        df=df)
    output$dftable <- shiny::renderTable(head(df))
  }
  return(shiny::shinyApp(ui, appServer))
}

# test with:
#   hit shift-ctrl-B to rebuild
#   shiny::runApp(testApp(loadTestData()))
#' @import gapminder
#' @keywords internal
loadTestData <- function() {
  gapminder
}


testApp <- function(vdata) {

  cd <- data.frame(
    year      =c( col.label='Year',           type='ordinal'),
    continent =c( col.label='Continent',      type='factor'),
    country   =c( col.label='Country',        type='factor'),
    lifeExp   =c( col.label='Life Expectancy',type='numeric'),
    pop       =c( col.label='Population',     type='numeric'),
    gdpPercap =c( col.label='GDP per capita', type='numeric')
  )

  dfcd <- data.frame(
    year.x          = c( colId='year',      dispId='x'),
    continent.rows  = c( colId='continent', dispId='rows'),
    continent.color = c( colId='continent', dispId='color')
  )

  shinyAppWrapper(df=vdata, colConf_=cd, mapConf_=dfcd)
}

#' Demonstration of tbl.viz.explorer
#' @export
appDemo <- function() {
  print("don't forget to hit shift-ctrl-B to rebuild package")
  print("running: shiny::runApp(testApp(loadTestData()))")
  #shiny::runApp(testApp(loadTestData()), display.mode="showcase")
  shiny::runApp(testApp(loadTestData()))
}

#' @export
csvDemo <- function(){
  loc <- system.file("examples","moduleExample", package = "tbl.viz.explorer")
  shiny::shinyAppDir(loc)
}

