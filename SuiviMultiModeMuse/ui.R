CheminPkg <- "Packages/"

library(shinythemes,lib.loc = CheminPkg)

ui <- navbarPage(theme=shinytheme("cerulean"),"Suivi de la collecte multimode EEC 6 Dernière mise à jour : 05012017 - 1700",
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
                                         uiOutput("Afficher_ChxReg"),
                                         uiOutput("Afficher_Chx_SemaineRef2"),
                                         p("(Affichage, par défaut, des 3 dernières semaines de collecte)"),
                                         radioButtons("TypeValeurs_DEM",
                                                      h3("Format des données"),
                                                      c("Valeurs brutes"=1,"Pourcentages"=2),
                                                      inline = T)
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('SuiviDem')
                              
                            )
                          )
                 ),
                 tabPanel("SUIVI CONCEPTEUR",icon=icon("user-o"),
                          sidebarLayout(
                            sidebarPanel(width=12,
                                         h1("Panneau de sélection"),
                                         uiOutput("Afficher_ChxReg2"),
                                         uiOutput("Afficher_Chx_SemaineRef"),
                                         p("(Affichage, par défaut, des 4 dernières semaines de collecte)"),
                                         radioButtons("TypeValeurs_CPS",
                                                      h3("Format des données"),
                                                      c("Valeurs brutes"=1,"Pourcentages"=2),
                                                      inline = T)
                                         
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('SuiviConcepteur')
                            )
                          )
                 )
)
