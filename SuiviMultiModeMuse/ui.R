chemin <- "/mnt/applishare/capi3g/recette/shiny/sources/"  #chemin vers mon appli
bibliotheque <- paste0(chemin,"Packages") #chemin complet vers mon dossier de packages
.libPaths(bibliotheque) #on fixe une variable d'environnement : en gros, il va chercher par défaut les packages dans le chemin bibliotheque
cheminProd <- "/mnt/applishare/capi3g/3.2.5/shiny/sources/"  #chemin vers mon appli
bibliothequeProd <- paste0(cheminProd,"Packages") #chemin complet vers mon dossier de packages
.libPaths(bibliothequeProd) #on fixe une variable d'environnement : en gros, il va chercher par défaut les packages dans le chemin bibliotheque


library(shinythemes, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

ui <- navbarPage(theme=shinytheme("cerulean"),"Suivi de la collecte multimode EEC - Dernière mise à jour : 24/01/2018 - 17h00",
                 
                 
                 
                 tabPanel("INFORMATIONS DIVERSES",
                          mainPanel(
                            h1("Application de suivi de l'enquête Muse")
                            ,tags$hr()
                            ,h4("1/ DONNEES")
                            ,p("Ce tableau affiche les FA démarrées.")
                            ,h5("- DESSIN DE FICHIER")
                            ,p("-- Mode de collecte (Colonne 7) : enquêteur, web, pôle EEC dans le cas des FA abandonnées")
                            ,p("-- Etat de la FA (Colonne 8) : statut de la FA en cours de collecte.")
                            ,h5("- OPTIONS :")
                            ,p("--- Filtre selon pôles de gestion, semaines de référence,grappes, mode de collecte.")
                            ,p("--- Afficher/Désafficher des colonnes,")
                            ,p("--- Déplacement des colonnes par Cliquez/Glissez,")
                            
                            
                            ,tags$hr(),tags$hr()
                            
                            ,h3("2/ SUIVI DEM")
                            ,p("Regroupement des FA, par enquêteur")
                            ,h4("- EN NOMBRE DE FA")
                            ,h5("- DESSIN DE FICHIER")
                            ,p("-- Total FA (Colonne 5) : nombre de FA à enquêter i.e. au nombre d'enregistrements")
                            ,p("--- Certaines FA peuvent être simultanément en cours de collecte 'enquêteur' et 'web' donc 'Total FA' n'est pas égal à la somme des colonnes 6 à 14")
                            ,p("-- sert de dénominateur dans la ventilation des FA par statut ('VENTILATION DES FA') ")
                            ,p("--- Colonne - Collecteur Pôle EEC : FA abandonnées")
                            # ,p("-- ______________________ (Colonne 6) : ________________________")
                            # ,p("-- ______________________ (Colonne 15): ________________________")
                            # ,p("-- ______________________ (Colonne 16): ________________________")
                            # ,p("-- ______________________ (Colonne 17): ________________________")
                            # ,p("-- ______________________ (Colonne 18): ________________________")
                            
                            
                            
                            ,h5("- OPTIONS :")
                            ,p("--- Filtre selon pôle de gestion, les enquêteurs, semaines de référence.")
                            ,p("--- Format des données,")
                            ,p("--- Copie du tableau (bouton Copy),")
                            ,p("--- Impression au format PDF (bouton Print),")
                            ,p("--- Chargement du tableau de suivi selon différents formats (bouton Download),")
                            ,p("--- Afficher/Désafficher des colonnes (bouton Masquer colonnes)")
                            ,tags$hr()
                            
                            ,h4("- VENTILATION DES FA")
                            ,p("ventilation des FA selon son statut actuel")
                            ,p("Dénominateur = Total FA")
                            ,tags$hr(),tags$hr()
                            
                            ,h3("3/ SUIVI CONCEPTEUR") 
                            ,p("Regroupement des FA, par pôle de gestion")
                            
                          )
                 ),
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
                                                      inline = T),
                                         checkboxInput("AfficherGraph",
                                                       "Afficher le graphique de répartition, par pôle de gestion",
                                                       TRUE,
                                                       width = "100%")
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('SuiviConcepteur'),
                              h3("Représentation graphique"),
                              plotOutput("PlotCPS")
                              
                              
                            )
                          )
                 )     
)

