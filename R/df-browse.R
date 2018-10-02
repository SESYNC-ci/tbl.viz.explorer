confSetup <- function(conf, idName) {
  out <- data.table::transpose(conf)
  names(out) <- rownames(conf)
  rownames(out) <- names(conf)
  out[[idName]] <- names(conf)
  out
  #htmlTable(displayConfig)
}
dc <- data.frame(
  x     = c( label='X',          required=T, enabled=T, numericOk=T, maxCard=Inf),
  y     = c( label='Y',          required=T, enabled=T, numericOk=T, maxCard=Inf),
  rows =  c( label='Row Facets', required=F, enabled=F, numericOk=F, maxCard=15),
  cols =  c( label='Col Facets', required=F, enabled=F, numericOk=F, maxCard=8),
  color = c( label='Color',      required=T, enabled=T, numericOk=F, maxCard=10)
)

# in globaldata.R

# from: https://community.rstudio.com/t/best-practices-for-global-external-variables-used-in-a-module/5820/2
ConfigManager <- function(input, output, session, colConf_, mapConf_) {
  colConf <- confSetup(colConf_, 'colId')
  mapConf <- confSetup(mapConf_, 'mapId')
  dispConf <- confSetup(dc, 'dispId')
  mergedConf <- merge(merge(mapConf, colConf, by='colId'), dispConf, by='dispId')

  stash = reactiveValues()
  stash$colConf = colConf
  stash$mapConf = mapConf
  stash$dispConf = dispConf
  stash$mergedConf = mergedConf

  return(list(
              GetColConf = reactive(stash$colConf),
              GetMapConf = reactive(stash$mapConf),
              GetDispConf = reactive(stash$dispConf),
              GetMergedConf = reactive(stash$mergedConf)))
}

topByMaxPop <- function(df, colId, maxVals) {
  # top 5 by Y
  tbl <- df %>% group_by_(colName) %>% summarise(max=max(pop)) %>% top_n(maxVals, max)
  return(as.matrix(tbl)[,1])
}
whichVals <- function(df, colName, selectFunc, maxVals) {
  return(selectFunc(df, colName, maxVals))
}

browseUI <- function(id, label="Pick from your data") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("overview")),
    shiny::tags$div(id = 'browse-module-container'),
    shiny::tableOutput(ns('dispFields')),
    shiny::h4('mergedConf:'),
    #shiny::uiOutput(ns('mergedConf')),
    shiny::uiOutput(ns('mapWidgets'))

#    shiny::pre({
#      shiny::textOutput(ns("stuff"))
#    })
    #shiny::uiOutput(ns("output"))
  )
}

browse <- function(input, output, session, df, ConfData) {

  mergedConf <- reactive({
    mc <- ConfData$GetMergedConf()
    #cat('does this log mergedConf?\n')
    #dput(mc)
    return(mc)
  })
  output$mergedConf <- shiny::renderUI(
    shiny::tagList(
      HTML( "colConf: <code>", mergedConf(), "</code><br/>"),
      shiny::renderTable(mergedConf())
    )
  )
  mapWidgets <- reactive({
    apply(
      mergedConf(), 1,
      function(colDesc) {
        cd <- callColWidgetMod(df=df, colDesc=colDesc, modId=session$ns(colDesc[['colId']]))
        #cd(newColDesc)
        return(cd())
      }
    )
  })
  
  output$mapWidgets <- reactive({
    print(dput(mapWidgets()))
    shiny::renderUI(
      shiny::tagList(
        HTML( "colConf: <code>", dput(mapWidgets()), "</code><br/>")
        #shiny::renderTable(mapWidgets())
      )
    )
    shiny::renderUI(shiny::H3("wtf?"))
  })
  return()

  dispFields$col <- NA
  #print(dispFields)
  #print(colDescs)

# won't work if dispFields isn't reactive
#  dispFields[dispFields$var %in% colDescs[colDescs$disp != '','disp'],'col'] <- 
#    colDescs[!is.na(colDescs$disp),'colId']

  output$dispFields <- shiny::renderTable(dispFields)
  #browser()
  #cat(glue::glue('varMods got msgs back:\n   {paste(varMods, collapse="\n   ")}\n\n'))
  output$overview <- shiny::renderUI(
    shiny::pre(
      glue::glue('{nrow(df)} rows in df, names: {paste(names(df), collapse=", ")}')
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
callColWidgetMod <- function(df, colDesc, modId) {
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
                  df=df)
  #msg <- glue::glue('      modId({modId}) got back from server({foo})\n')
  #cat(msg, '\n')
  #cat(newColDesc())
  return(colWidgetReturn)
}

colWidgetUI <- function(id, colDesc) {
  ns <- shiny::NS(id)
  msg <- glue::glue('   running colWidgetUI id({id})\n')
  shiny::tagList(
    shiny::h4(colDesc[['colId']]),
    shiny::verbatimTextOutput(ns("subtitle")),
    shiny::verbatimTextOutput(ns("msg")),
##    shiny::h4('dispChoice:'),
##    shiny::verbatimTextOutput(ns("dispChoice")),
    shiny::uiOutput(ns('valSelect')),
    shiny::uiOutput(ns('dispSelect'))
#    shiny::uiOutput(ns('plots'))
    #shiny::pre(msg),
  )
}
colWidget <- function(input, output, session, modId, df, colDesc) {
  ns <- session$ns
  maxVals <- 4
  colName <- colDesc[['colId']]
  col <- df[[colName]]

  output$dispSelect <- shiny::renderUI(
    shiny::selectInput(ns('dispSelect'),
                              choices=options,
                              label="Display as",
                              selected=colDesc[['disp']]
                              #options=list(labels=dispFields$label, placeholder = 'select a display dim')
                          ))
  output$dispChoice <- reactive(input$dispSelect)
  dispChoice <- reactive(input$dispSelect)
  #return(reactive(colDesc))
  cd <- reactive({
    if (length(input$dispSelect)) {
      browser()
      print("got some value for dispSelect!!!!", dispChoice(),'\n')
      #browser()
      colDesc[['dispId']] <- input$dispSelect
    }
  })
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
linePlot <- function(df, input, x, y, color) {
  plot <- ggplot2::ggplot(df,
            ggplot2::aes_string(x=x, y=y, color=color)) +
            ggplot2::geom_line(stat = "summary", fun.y = "sum", alpha=1)

  if (input$logTransform == TRUE) {
    plot <- plot + ggplot2::coord_trans(y="log10")
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
facetPlot <- function(df, plot, dims, rows, cols, color) {
  dorows <- dims[dims$var=='rows','enabled'][[1]]
  docols <- dims[dims$var=='cols','enabled'][[1]]
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
