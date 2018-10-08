csvInput <- function(id, label = "Data to Display Dimension Assignments") {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fileInput(ns("file"), label),
    shiny::checkboxInput(ns("heading"), "Has heading"),
    shiny::selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

csvInputServer <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- shiny::reactive({
    # If no file is selected, don't do anything
    shiny::validate(shiny::need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  dataframe <- shiny::reactive({
    shiny::read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })

  # We can run observers in here if we want to
  shiny::observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}
