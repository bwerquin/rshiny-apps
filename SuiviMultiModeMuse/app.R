#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Suivi de la collecte multimode EEC"),
   
   # radioButtons to selected data collection mode for filter
   sidebarLayout(
      sidebarPanel(
        radioButtons("mode", label = h3("Choix du mode"), 
                     choices = list("CAPI" = "CAPI", "CAWI" = "CAWI", "All" = "All"),
                     selected = "CAWI")
      ),
      
      # Show the table
      mainPanel(
        tableOutput('suivi')
      )
   )
)

# Define server logic required to call the data from a web service
server <- function(input, output) {

     output$suivi <- renderTable({
       modeSelected <- input$mode
       if(modeSelected=="All"){
         fromJSON("http://localhost:4000/rest/suivi")
       }
       else{
         fromJSON("http://localhost:4000/rest/suivi") %>%
           filter(mode==modeSelected)
       }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

