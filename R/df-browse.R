
topByMaxPop <- function(df, colName, maxVals) {
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
    shiny::tableOutput(ns('colDescs'))
#    shiny::pre({
#      shiny::textOutput(ns("stuff"))
#    })
    #shiny::uiOutput(ns("output"))
  )
}

browse <- function(input, output, session, df, colDescs) {

  dispFields$col <- NA
  print(dispFields)
  print(colDescs)

  dispFields[dispFields$var %in% colDescs[colDescs$disp != '','disp'],'col'] <- 
    colDescs[!is.na(colDescs$disp),'name']

  output$dispFields <- shiny::renderTable(dispFields)
  output$colDescs <- shiny::renderTable(colDescs)

  varMods <- apply(
    colDescs, 1,
    function(colDesc) {
      colName <- colDesc[['name']]
      disp <- colDesc['disp']
      type <- colDesc[['type']]
      if ( ! is.na(disp)) {
        dispField <- dispFields[dispFields$var == disp,]
      } else {
        dispField <- NA
      }
      callColWidgetMod(df=df, colName=colName, modId=session$ns(colName), disp=disp, type=type, dispField=dispField)
    }
    #names(df)  #[2:3]
  )
  #cat(glue::glue('varMods got msgs back:\n   {paste(varMods, collapse="\n   ")}\n\n'))
  output$overview <- shiny::renderUI(
    shiny::pre(
      glue::glue('{nrow(df)} rows in df, names: {paste(names(df), collapse=", ")}')
  ))
  output$stuff <- shiny::renderPrint({
    print("here comes clientData")
    session$clientData
  })
  #output$output <- shiny::renderTable(summary(df))
  return(df)
}

# found module insert examples:
    # http://shiny.rstudio-staging.com/articles/dynamic-ui.html
    # https://gallery.shinyapps.io/insertUI-modules/
callColWidgetMod <- function(df, colName, modId, type, disp, dispField) {
  foo <- shiny::callModule(
                  module=colWidget,
                  id=colName,
                  modId=modId,
                  df=df,
                  colName=colName,
                  type=type,
                  disp=disp,
                  dispField=dispField)
  #msg <- glue::glue('      modId({modId}) got back from server({foo})\n')
  #cat(msg, '\n')
  insertUI(
    selector = "#browse-module-container",
    where = "beforeEnd",
    colWidgetUI(modId, colName, dispField)
  )
  
  #return(msg)
}

colWidgetUI <- function(id, colName, dispField) {
  ns <- shiny::NS(id)
  msg <- glue::glue('   running colWidgetUI id({id})\n')
  if (is.na(dispField[1])) {
    title <- colName
  } else {
    title <- glue::glue('{colName} displayed as {dispField[["label"]]}')
  }
  shiny::tagList(
    shiny::h3(title),
    shiny::verbatimTextOutput(ns("msg")),
    shiny::verbatimTextOutput(ns("msg2")),
    shiny::uiOutput(ns('plots'))
    #shiny::pre(msg),
    #shiny::checkboxInput(paste0("ignore-", colName), label = paste("Ignore", colName)),
  )
}
colWidget <- function(input, output, session, modId, df, colName, type, disp, dispField) {
  # sum top y across all x
  ns <- session$ns
  maxVals <- 4
  #print('names(df):')
  #print(names(df))
  #print('   colName:')
  #print(colName)
  col <- df[[colName]]

  output$msg2 <- shiny::renderText({
    glue::glue('\n{colName}, {type}\n')
  })
#  if (type == 'factor') {
#    factorVarWidget(input, output, session, modId, df, colName, type, disp, dispField)
#  }
  if (is.factor(col)) {
    lvls <- col %>% table() %>% sort(decreasing = T) %>% names()
#    vals <- reactive({
#      df[[colName]] <- factor(col,levels = lvls())
#      vals <- unique(as.matrix(col)[,1])
#      if (length(vals) > 10) {
#        tbl <- df %>% group_by_(colName) %>% summarise(max=max(pop)) %>% top_n(maxVals, max)
#        #vals <- whichVals(df, colName, topByMaxPop, 5)
#        vals <- as.matrix(tbl)[,1]
#      }
#      if(!(length(vals) <= 10)) {
#        stop('problem with vals')
#      }
#      return(vals)
#    })

    output$msg <- shiny::renderText({
      #glue::glue('\n{length(lvls())} vals in {cls()} df[{colName}]: {paste(vals, collapse=", ")}\n')
      cardinality <- length(unique(col))
      glue::glue('\n{colName}, factor w/ {cardinality} levels, top {maxVals}: {paste(lvls[1:maxVals], collapse=", ")}\n')
    })

    output$recordsBar <- shiny::renderPlot({
      df[[colName]] <- factor(df[[colName]],levels = lvls)
      # reorder factors in decreasing frequency... probably bad idea
      
      #dims <- dispFields
      df %>%  
        #filter(.data[[colName]] %in% vals) %>%
        #    makePlot(dims=dims, input=input, plotFunc=smallBarPlot)
        ggplot2::ggplot(ggplot2::aes_string(x=colName, fill=colName, 
                                              #forcats::fct_infreq(colName)
                                              )) +
          #ggplot2::scale_x_discrete(limits = lvls()) +
          ggplot2::geom_bar( stat = "count", show.legend=F) +
          ggplot2::ggtitle(glue::glue('record count by {colName}')) +
          ggplot2::theme(
                axis.text.x=ggplot2::element_blank(),
                axis.title.x=ggplot2::element_blank(),
                axis.ticks.x=ggplot2::element_blank(),
                axis.text.y=ggplot2::element_blank(),
                axis.title.y=ggplot2::element_blank(),
                axis.ticks.y=ggplot2::element_blank()
              )
    })
    output$distPlot <- shiny::renderPlot({
      tbl <- df %>% group_by_(colName) %>% summarise(max=max(pop)) %>% arrange(desc(max)) 
      tbl %>%  
        ggplot2::ggplot(ggplot2::aes_string(x=colName, fill=colName, y='max')) +
          ggplot2::scale_x_discrete(limits = tbl[[colName]]) +
          ggplot2::geom_bar( stat = "sum", show.legend=F) +
          ggplot2::ggtitle(glue::glue('sum(pop) by {colName}')) +
          ggplot2::theme(
                axis.text.x=ggplot2::element_blank(),
                axis.title.x=ggplot2::element_blank(),
                axis.ticks.x=ggplot2::element_blank(),
                axis.text.y=ggplot2::element_blank(),
                axis.title.y=ggplot2::element_blank(),
                axis.ticks.y=ggplot2::element_blank()
              )
    })
    output$plots <- shiny::renderUI(
      shiny::tagList(
        shiny::plotOutput(ns("recordsBar"), hover=ns("plotHover"), width=250, height=70),
        shiny::plotOutput(ns("distPlot"), hover=ns("plotHover"), width=250, height=70),
        shiny::htmlOutput(ns("hoverMsg"))
      )
    )
  } else {
    output$msg <- shiny::renderText({
      #glue::glue('\n{length(lvls())} vals in {cls()} df[{colName}]: {paste(vals, collapse=", ")}\n')
      cardinality <- length(unique(col))
      glue::glue('\n{colName}, {class(col)} w/ {cardinality} vals, range: {min(col)} - {max(col)}\n')
    })
    output$distPlot <- shiny::renderPlot({
      df %>%  
        #filter(.data[[colName]] %in% vals) %>%
        #    makePlot(dims=dims, input=input, plotFunc=smallBarPlot)
        ggplot2::ggplot(ggplot2::aes_string(x=colName)) +
          ggplot2::geom_histogram() +
          #ggplot2::scale_x_continuous(trans="log1p", expand=c(0,0)) +
          #ggplot2::coord_trans(y="log10") +
          ggplot2::ggtitle(glue::glue('histogram by {colName}')) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 315, hjust = 0)
          )
            #plot.margin = ggplot2::margin(10, 40, 10, 10))
#                axis.text.x=ggplot2::element_blank(),
#                axis.title.x=ggplot2::element_blank(),
#                axis.ticks.x=ggplot2::element_blank(),
#                axis.text.y=ggplot2::element_blank(),
#                axis.title.y=ggplot2::element_blank(),
#                axis.ticks.y=ggplot2::element_blank()
#              )
    })
    output$plots <- shiny::renderUI(
      shiny::plotOutput(ns("distPlot"), width=550, height=170)
    )
  }
  #vals <- reactive({
  #return(vals)
  #})
  output$hoverMsg <- shiny::renderText({
    plotHover <- input$plotHover
    #print(dput(plotHover))
    col <- df[,colName]
    #print(str(col))
    if (is.null(plotHover$x)) return("")
    lvls <- levels(df[[colName]])
    name <- lvls[round(plotHover$x)]
    HTML(
         #"head(col) <code>", dput(head(col)), "</code><br/>",
         "hover info: <code>", dput(plotHover), "</code><br/>",
         "lvls <code>", dput(lvls), "</code><br/>",
         "name <code>", name, "</code><br/>"
         )
  })
  #msg <- glue::glue('   running server modId({modId}) in session({ns("")}\n')
  #return(msg)
  #  output$output <- shiny::renderTable(summary(df))
  #  return(df)
}

makePlot <- function(df, dims, input, plotFunc) {
  #print(dims)
  d <- dims$val
  names(d) <- dims$var
  x <- d[['x']]
  y <- d[['y']]

  color <- d[['colorBy']]
  rows <- d[['facetRowsBy']]
  cols <- d[['facetColsBy']]
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
  dorows <- dims[dims$var=='facetRowsBy','enabled'][[1]]
  docols <- dims[dims$var=='facetColsBy','enabled'][[1]]
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
