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
library(httr)
library(devtools)
library(curl)


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
         config = httr::config(ssl_verifypeer = 0L)
         set_config(config)
         t <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
         fromJSON(content(t, "text"))
     })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

