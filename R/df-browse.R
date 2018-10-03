#' @importFrom data.table transpose
#' @keywords internal
confSetup <- function(conf, idName) {
  out <- transpose(conf)
  names(out) <- rownames(conf)
  rownames(out) <- names(conf)
  out[[idName]] <- names(conf)
  out
  #htmlTable(displayConfig)
}

dc <- data.frame(
  x     = c( disp.label='X',          required=T, enabled=T, numericOk=T, maxCard=Inf),
  y     = c( disp.label='Y',          required=T, enabled=T, numericOk=T, maxCard=Inf),
  rows =  c( disp.label='Row Facets', required=F, enabled=F, numericOk=F, maxCard=15),
  cols =  c( disp.label='Col Facets', required=F, enabled=F, numericOk=F, maxCard=8),
  color = c( disp.label='Color',      required=T, enabled=T, numericOk=F, maxCard=10)
)

# in globaldata.R

# from: https://community.rstudio.com/t/best-practices-for-global-external-variables-used-in-a-module/5820/2
ConfigManager <- function(input, output, session, colConf_, mapConf_) {
  colConf <- confSetup(colConf_, 'colId')
  mapConf <- confSetup(mapConf_, 'mapId')
  dispConf <- confSetup(dc, 'dispId')

  stash = reactiveValues()
  stash$colConf = colConf
  stash$mapConf = mapConf
  stash$dispConf = dispConf

  return(list(
              getColConf = reactive(stash$colConf),
              getMapConf = reactive(stash$mapConf),
              setMapConf = function(mc) stash$mapConf = mc,
              getDispConf = reactive(stash$dispConf)
#              getMergedConf = reactive({
#                merge(merge(stash$mapConf, stash$colConf, by='colId'), stash$dispConf, by='dispId')
#              })
          ))
  observeEvent(stash$mapConf, {
      print('mapConf', stash$mapConf)
  })
}

getMergedConf <- function(confData, allDisps=T, allData=T) {
  return(
    merge(merge(confData$getMapConf(), confData$getColConf(), 
                by='colId', all=allData), 
          confData$getDispConf(), by='dispId', all=allDisps)
  )
}
browseUI <- function(id, label="Pick from your data") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("overview")),
    shiny::uiOutput(ns('widgetContainer'))
  )
}

#' @importFrom glue glue
#' @keywords internal
browse <- function(input, output, session, df, confData) {
  mapWidgets <- reactive({
    cds <- apply(
      getMergedConf(confData, allDisps=F, allData=T), 1,
      function(colDesc) {
        cd <- callColWidgetMod(df=df, colDesc=colDesc, 
                               modId=session$ns(colDesc[['colId']]),
                               confData=confData)
        #cd(newColDesc)
        return(cd())
      }
    )
    return(cds)
  })
  output$widgetContainer <- shiny::renderUI(
    shiny::tagList(
      shiny::tags$div(id = 'browse-module-container'),
      shiny::h4('mapConf:'),
      shiny::renderTable(confData$getMapConf()),
      shiny::h4('mergedConf:'),
      shiny::renderTable(getMergedConf(confData, F, F)),
      HTML( "mapConf: <code>", dput(confData$getMapConf()), "</code><br/>"),
      HTML( "mapWidgets: <code>", dput(mapWidgets()), "</code><br/>")
    )
  )

  output$overview <- shiny::renderUI(
    shiny::pre(
      glue('{nrow(df)} rows in df, names: {paste(names(df), collapse=", ")}')
  ))
  output$stuff <- shiny::renderPrint({
    #print("here comes clientData")
    session$clientData
  })
  #output$output <- shiny::renderTable(summary(df))
  return(df)
}

# found module insert examples:
    # http://shiny.rstudio-staging.com/articles/dynamic-ui.html
    # https://gallery.shinyapps.io/insertUI-modules/
callColWidgetMod <- function(df, colDesc, modId, confData) {
  insertUI(
    selector = "#browse-module-container",
    where = "beforeEnd",
    colWidgetUI(modId, colDesc)
  )
  colWidgetReturn <- shiny::callModule(
                  module=colWidget,
                  id=colDesc[['colId']],
                  colDesc=colDesc,
                  modId=modId,
                  confData=confData,
                  df=df)
  #msg <- glue::glue('      modId({modId}) got back from server({foo})\n')
  #cat(msg, '\n')
  #cat(newColDesc())
  return(colWidgetReturn)
}

#' @importFrom glue glue
#' @keywords internal
colWidgetUI <- function(id, colDesc) {
  ns <- shiny::NS(id)
  msg <- glue('   running colWidgetUI id({id})\n')
  shiny::tagList(
    shiny::h4(colDesc[['colId']]),
    shiny::verbatimTextOutput(ns("dispChoice")),
    shiny::uiOutput(ns('valSelect')),
    shiny::uiOutput(ns('dispSelect')),
    shiny::uiOutput(ns('plots'))
  )
}
colWidget <- function(input, output, session, modId, df, colDesc, confData) {
  ns <- session$ns
  maxVals <- 4
  colName <- colDesc[['colId']]
  col <- df[[colName]]

  options <- c(NA, confData$getDispConf()$dispId)
  names(options) <- c('select disp dim', confData$getDispConf()$disp.label)
  output$dispSelect <- shiny::renderUI(
    shiny::selectInput(ns('dispSelect'),
                              choices=options,
                              label="Display as",
                              selected=colDesc[['dispId']]
                              #multiple=T,
                              #options=list(labels=dispFields$label, placeholder = 'select a display dim')
                          ))

#  observeEvent(input$dispSelect, {
#    #req(input$dispSelect)
#    #browser()
#    mc <- confData$getMapConf()
#    mc[mc$coldId==colDesc[['colId']],'dispId']] <- input$dispSelect # gets more than one, doesn't work
#    confData$setMapConf(mc)
#  })

  output$dispChoice <- reactive(input$dispSelect)
  dispChoice <- reactive(input$dispSelect)
  #return(reactive(colDesc))
  cd <- reactive({
    if (length(input$dispSelect)) {
      ds <- input$dispSelect
      if(ds != colDesc[['dispId']]) {
        #print("got some value for dispSelect!!!!", dispChoice(),'\n')
        colDesc[['dispId']] <- ds
      }
    }
  })
  output$dispChoice <- reactive(input$dispSelect)
  return(cd)
}

makePlot <- function(df, dims, input, plotFunc) {
  #print(dims)
  d <- dims$val
  names(d) <- dims$var
  x <- d[['x']]
  y <- d[['y']]

  color <- d[['colorBy']]
  rows <- d[['rows']]
  cols <- d[['cols']]
  #print(c(x,y,color, rows,cols))
  #print(nrow(df))

  plot <- plotFunc(df=df, input=input, x=x, y=y, color=color)
  plot <- facetPlot(df=df, plot=plot, dims=dims, rows=rows, cols=cols, color=color)

  return(plot)
}

#' @importFrom ggplot2 ggplot aes_string geom_line coord_trans
#' @export
linePlot <- function(df, input, x, y, color) {
  plot <- ggplot(df,
            aes_string(x=x, y=y, color=color)) +
            geom_line(stat = "summary", fun.y = "sum", alpha=1)

  if (input$logTransform == TRUE) {
    plot <- plot + coord_trans(y="log10")
  }
  return(plot)
}
#smallBarPlot <- function(df, input, x, y, color) {
#  plot <- ggplot2::ggplot(ggplot2::aes_string(x=colName, fill=colName)) +
#        ggplot2::geom_bar( stat = "count")
#
#
#    ggplot2::ggplot(df,
#            ggplot2::aes_string(x=x, y=y, color=color)) +
#            ggplot2::geom_line(stat = "summary", fun.y = "sum", alpha=1)
#
#  if (input$logTransform == TRUE) {
#    plot <- plot + ggplot2::coord_trans(y="log10")
#  }
#  return(plot)
#}
#' @importFrom ggplot2 facet_grid ggtitle vars
#' @importFrom glue glue
#' @export
facetPlot <- function(df, plot, dims, rows, cols, color) {
  dorows <- dims[dims$var=='rows','enabled'][[1]]
  docols <- dims[dims$var=='cols','enabled'][[1]]
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
    plot <- plot + ggtitle(glue::glue('cols {cols}, color {color}'))
  }
  return(plot)
}
