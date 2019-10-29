
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

    folder <- try({
      new_folder <- synapser::Folder(
        name = "ServerTest",
        parent = "syn20400157"
      )
      created_folder <- synapser::synStore(new_folder)
      created_folder
    })

    observeEvent(input$save, {
      tryCatch({
        cat(session$clientData, file = stderr())
        file_to_upload <- synapser::File(
          input$file$datapath,
          parent = folder,
          name = input$file$name
        )
        synapser::synStore(file_to_upload)
      },
      error = function(err) {
        output$error <- renderText({err$message})
      })
    })
  })
})
