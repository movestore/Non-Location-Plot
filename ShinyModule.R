library("shiny")
library("shinyWidgets")
library("move2")
library("dplyr")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal() -> logger.trace()

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Plot of selected Non-Location Measure"),
      fluidRow(
      column(3,uiOutput(ns("uiind"))),
      column(3,uiOutput(ns("uiattr")))
      ),
    plotOutput(ns("plot"))
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns ## all IDs of UI functions need to be wrapped in ns()
  current <- reactiveVal(data)

  output$uiattr <- renderUI({
    dataNNA <- data[,!sapply(data, function(x) all(is.na(x)))]
    #dataNUM <- dataNNA[,!sapply(as.list(dataNNA), function(x) is.numeric(x))]
  selectInput(ns("attr"),"Select attribute", choices=names(dataNNA))
  })
  
  output$uiind <- renderUI({
    selectInput(ns("ind"),"Select track", choices=unique(mt_track_id(data)),selected=mt_track_id(data)[1])
  })
  
  output$plot <- renderPlot({
    #if (is.null(input$attr))
    #{
    #  plot(0,0,type="n",xlab="timestamp",ylab="",axes=FALSE)
    #} else
    #{
      plot(mt_time(data[mt_track_id(data)==input$ind,]),as.data.frame(data[mt_track_id(data)==input$ind,input$attr])[,1],type="l",col=4,xlab="timestamp",ylab=input$attr)
    #}
  })
  
  return(reactive({ current() })) ## if data are not modified, the unmodified input data must be returned
}
