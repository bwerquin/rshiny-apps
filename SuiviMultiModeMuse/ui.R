# library(shiny)
library(shinythemes)

ui <- navbarPage(theme=shinytheme("cerulean"),"Suivi de la collecte multimode EEC",
                 tabPanel("DONNEES",icon=icon("table"),
                          # sidebarLayout(
                          #   sidebarPanel(
                          #     radioButtons("mode", label = h3("Choix du mode"), 
                          #                  choices = list("CAPI" = "CAPI", "CAWI" = "CAWI", "All" = "All"),
                          #                  selected = "CAWI")
                          #   ),
                            
                            mainPanel(
                             h1("Tableau de données"),
                               DT::dataTableOutput('Donnees')
                            )
                          # )
                 ),
                 tabPanel("SUIVI DEM",icon=icon("user"),
                          sidebarLayout(
                            sidebarPanel(width=12,
                              h1("Panneau de sélection"),
                              uiOutput("Afficher_ChxReg")
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('SuiviDem')

                            )
                          )
                 ),
                 tabPanel("SUIVI CONCEPTEUR",icon=icon("user-o"),
                          sidebarLayout(
                            sidebarPanel(
                              h1("Panneau de sélection")
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('SuiviConcepteur')
                            )
                          )
                 )
)
