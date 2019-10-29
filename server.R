list_to_string <- function(obj, listname) {
  if (is.null(names(obj))) {
    paste(listname, "[[", seq_along(obj), "]] = ", obj,
          sep = "", collapse = "\n")
  } else {
    paste(listname, "$", names(obj), " = ", obj,
          sep = "", collapse = "\n")
  }
}

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# This server has been modified to be used specifically on
# Sage Bionetworks Synapse pages
# to log into Synapse as the currently logged in user from the
# web portal using the session token.
#
# https://www.synapse.org

library(shiny)
library(synapser)

shinyServer(function(input, output, session) {

  session$sendCustomMessage(type = "readCookie", message = list())

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.") # nolint
      )
    )
  })

  foo <- observeEvent(input$cookie, {

    synLogin(sessionToken = input$cookie)

    output$title <- renderUI({
      titlePanel(sprintf("Welcome, %s", synGetUserProfile()$userName))
    })


    observeEvent(input$save, {
      cdata <- session$clientData

      # Values from cdata returned as text
      output$clientdataText <- renderText({
        cnames <- names(cdata)

        allvalues <- lapply(cnames, function(name) {
          paste(name, cdata[[name]], sep = " = ")
        })
        paste(allvalues, collapse = "\n")
      })

      tryCatch({
        file_to_upload <- synapser::File(
          input$file$datapath,
          parent = "syn21068819",
          name = input$file$name
        )
        stored <- synapser::synStore(file_to_upload)
        output$stored <- renderText({
          list_to_string(
            list(
              stored$properties$createdBy,
              stored$properties$dataFileHandleId
            ),
            c("createdBy", "dataFileHandleId")
          )
        })
      },
      error = function(err) {
        output$error <- renderText({err$message})
      })
    })
  })
})
