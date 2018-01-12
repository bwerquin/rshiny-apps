CheminPkg <- "Packages/"

library(shinythemes,lib.loc = CheminPkg)

ui <- navbarPage(theme=shinytheme("cerulean"),"Suivi de la collecte multimode EEC - Dernière mise à jour : 11012018 - 17h30",
                 tabPanel("DONNEES",icon=icon("table"),
                          sidebarLayout(
                            sidebarPanel(width=12,
                              uiOutput("Afficher_ChxReg0"),
                              uiOutput("Afficher_Chx_SemaineRef0"),
                              checkboxGroupInput("ModCollect",
                                            h3("Mode de collecte"),
                                            c("Enquêteur"="Enquêteur","Web"="Web","Pôle EEC"="Pôle EEC"),
                                            inline = T,selected = c("Enquêteur","Web","Pôle EEC"))
                             # ,p("(Affichage, par défaut, des 3 dernières semaines de collecte (présentes en base))")
                              
                            ),
                          
                          mainPanel(
                            h1("Tableau de données"),
                            h3("Test Test ... Test Test"),
                            DT::dataTableOutput('ExportDonnees')
                          )
                          )
                 ),
                 tabPanel("SUIVI DEM",icon=icon("user"),
                          sidebarLayout(
                            sidebarPanel(width=12,
                                         h1("Panneau de sélection"),
                                         uiOutput("Afficher_ChxReg"),
                                         uiOutput("Afficher_Chx_SemaineRef2"),
                                         p("(Affichage, par défaut, des 3 dernières semaines de collecte (présentes en base))"),
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
                                         p("(Affichage, par défaut, des 4 dernières semaines de collecte (présentes en base))"),
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
