library(tidyr)
library(magrittr)
library(dplyr)

hello <-function() {
  return("Hello, World!")
}

varValueSelectionInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("varValueSelection"))
}

varValueSelectionServer <- function(input, output, session, df) {
  ns <- session$ns

  varInputs <- reactive({
    dfVars <- names(df)
    cat(glue::glue("\n\n(wtf?) doing varValueSelectionServer for {ns('foo ')}\n\n"))
    mapply(function(varName) {
        shiny::callModule(
          module=tbl.viz.explorer::selectLevelsServer,
          id=ns(glue::glue('input_for_{varName}')),
          session=session,
          df=df,
          varName=varName)
      },
      dfVars
    )
    vapply(dfVars, selectLevelsInput, selectLevelsInput(dfVars[[1]]))
  })

#  cnts <- df %>% dplyr::group_by(!!as.name(x)) %>% summarise(n=n()) %>% top_n(10)
#  ctbl <- shiny::renderTable(cnts)
#  output$main <- ctbl


  output$varValueSelection <- shiny::renderUI({
    shiny::tagList(varInputs())
  })
}

#' @export
selectLevelsInput <- function(id) {
  ns <- shiny::NS(id)
    #shiny::tableOutput(ns("main")),
  shiny::textOutput(ns('selectLevelsContent'))
}

selectLevelsServer <- function(input, output, session, df, varName) {
  ns <- session$ns
  cat(glue::glue("\n\n(rrr) doing selectLevelsServer for {ns(varName)}\n\n"))
  output$selectLevelsContent <- shiny::renderText(
    glue::glue('{length(unique(df[,varName])) vals for {varName}'))
#  cnts <- df %>% dplyr::group_by(!!as.name(x)) %>% summarise(n=n()) %>% top_n(10)
#  ctbl <- shiny::renderTable(cnts)
#  output$main <- ctbl
}
