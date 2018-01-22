CheminPkg <- "Packages/"

library(shinythemes,lib.loc = CheminPkg)

ui <- navbarPage(theme=shinytheme("cerulean"),"Suivi de la collecte multimode EEC - Dernière mise à jour : 22012018 - 12h00",
                 tabPanel("DONNEES",icon=icon("table"),
                          sidebarLayout(
                            sidebarPanel(width=12,
                                         uiOutput("Afficher_ChxReg0"),
                                         uiOutput("Afficher_Chx_SemaineRef0"),
                                         p("(Affichage, par défaut, des 3 dernières semaines de collecte (présentes en base))")
                                         ,uiOutput("Afficher_Chx_Grappes")
                                         ,checkboxGroupInput("ModCollect",
                                                             h3("Mode de collecte"),
                                                             c("Enquêteur"="Enquêteur","Web"="Web","Pôle EEC"="Pôle EEC"),
                                                             inline = T,selected = c("Enquêteur","Web","Pôle EEC"))
                                         
                            ),
                            
                            mainPanel(
                              h1("Tableau de données"),
                              h4("Statut des questionnaires démarrés : par FA"),
                              h5("(une FA avec plusieurs statuts ('en cours enquêteur' et 'en cours web', par ex.) simultanément est comptabilisée autant de fois)"),
                              DT::dataTableOutput('ExportDonnees')
                            )
                          )
                 ),
                 tabPanel("SUIVI DEM",icon=icon("user"),
                          sidebarLayout(
                            sidebarPanel(width=12,
                                         h1("Panneau de sélection"),
                                         uiOutput("Afficher_ChxReg"),
                                         uiOutput("Afficher_ChxEnqueteur"),
                                         uiOutput("Afficher_Chx_SemaineRef2"),
                                         p("(Affichage, par défaut, des 3 dernières semaines de collecte (présentes en base))"),
                                         radioButtons("TypeValeurs_DEM",
                                                      h3("Format des données"),
                                                      c("en nombre de FA"=1,"Ventilation des FA"=2),
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
                                                      c("en nombre de FA"=1,"Ventilation des FA"=2),
                                                      inline = T)
                                         
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('SuiviConcepteur')
                            )
                          )
                 ),
                 tabPanel("INFORMATIONS DIVERSES",
                          mainPanel(
                            h1("Dessin de fichier")
                            ,tags$hr()
                            
                            ,h3("1/ DONNEES")
                            ,p("_____________")
                            
                            ,tags$hr(),tags$hr()
                            
                            ,h3("2/ SUIVI DEM")
                            ,h5("- Données brutes")
                            ,p("Colonne 3 : Nombre initial de FA : nombre d'enregistrements")
                            ,p("Colonne 4 : Total FA : nombre de fiches adresse en cours, en prenant en compte les doublons i.e. des FA en cours démarrée par un enquêteur et celles démarrées par le web.")
                            ,tags$hr()
                            
                            ,h5("- Ventilation %")
                            ,p("_____________")
                            
                            ,tags$hr(),tags$hr()
                            
                            ,h3("3/ SUIVI CONCEPTEUR") 
                            ,h5("- Données brutes")
                            ,p("_____________")
                            
                            ,h5("- Ventilation %")
                            ,p("_____________")
                            
                            
                            # --------------------------------------------------------------------------------------------------------------
                            
                          )
                 )
)