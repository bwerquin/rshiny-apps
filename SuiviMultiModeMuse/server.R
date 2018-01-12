CheminPkg <- "Packages/"

library(shiny,lib.loc = CheminPkg)
library(jsonlite,lib.loc = CheminPkg)
library(dplyr,lib.loc = CheminPkg)
library(tidyr,lib.loc = CheminPkg)
library(httr,lib.loc = CheminPkg)
library(devtools,lib.loc = CheminPkg)
library(curl,lib.loc = CheminPkg)
library(DT,lib.loc = CheminPkg)

server <- function(input, output) {
  
  # 0/ DONNEES  ___________________________________________________________________________________________________________________________________ ####
  # 0A/ RECUPERATION DES DONNEES --------------------------------------------------------------------------------------------------------------- 
  Donnees <- reactive({
  #   # 0A1/ ACCES AU WEBSERVICE ----------------------------------------------------------------------------------------
  #   config = httr::config(ssl_verifypeer = 0L)
  #   set_config(config)
  #   Donnees <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
  #   Donnees <- fromJSON(content(Donnees, "text"))
  #   Donnees$NumSemaine <- paste0("S.",format(as.Date(Donnees$semainereference), "%U"))
  #   Donnees$IdentifEnqueteur <- paste0(Donnees$enqueteurnom," ",Donnees$enqueteurprenom)
  
  #   # EN ATTENDANT D'AVOIR LES VERITABLES VARIABLES RELATIVES AU POLE EEC
  #   Donnees$EEC_refus <- "VarTemp"
  #   Donnees$EEC_horschamp <- "VarTemp"
    
    # ----------------------------------------------------------------------------------------
    # !!! POUR TESTER AVEC UN JEU DE DONNEES PLUS CONSEQUENT ... OU LORSQUE PBM DE CONNEXION AU WEBSERVICE
    Donnees <- readRDS("Temp/Donnees5.rds")
    # ----------------------------------------------------------------------------------------
    return(Donnees)
  })
  
  # 0B/ FILTRE REGIONS (i.e. ETABLISSEMENTS) -----------------------------------------------------------------------------------------------------------------------
  output$Afficher_ChxReg0 <- renderUI({
    # 0B0 - ACCES AU WEBSERVICE : RECUPERATION DES NOMS DE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    # config = httr::config(ssl_verifypeer = 0L)
    # set_config(config)
    # Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
    # ListReg <<- fromJSON(content(Reg, "text"))

    # ----------------------------------------------------------------------------------------
    # !!! POUR TESTER AVEC UN JEU DE DONNEES PLUS CONSEQUENT ... OU LORSQUE PBM DE CONNEXION AU WEBSERVICE
    ListReg <<- readRDS("Temp/ListReg.rds")
    # ----------------------------------------------------------------------------------------
    
    # 0B1 - SELECTINPUT : FILTRE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    selectInput("ChxReg0",
                h3("Sélectionnez une ou plusieurs régions"),
                choices = ListReg,
                multiple = T,
                width = "50%",
                selected = ListReg)
  })
  
  # 0B2 - SELECTINPUT : FILTRE SEMAINES DE REFERENCE ----------------------------------------------------------------------------------------
  output$Afficher_Chx_SemaineRef0 <- renderUI({
    # ListSemaineRef <- Donnees()$NumSemaine
    # ListSemaineRef <<- sort(unique(ListSemaineRef))
    ListSemaineRef<<-c("S.42","S.43","S.44","S.45","S.46","S.41","S.47","S.48","S.49","S.50","S.51","S.52")
    selectInput("ChxNumSemN0",
                h3("Sélectionnez la ou les semaines de référence"),
                choices = ListSemaineRef,
                multiple = T,
                width = "50%",
                selected = tail(ListSemaineRef,3))
    
  })
  
  # 0C/ DATATABLEOUTPUT : TABLEAU ENSEMBLE DES DONNEES -------------------------------------------------------------------------------------------------------------------
  output$ExportDonnees <- renderDataTable({
    # Export_Donnees <- Donnees()
    # Export_Donnees <- Export_Donnees %>% 
    #   filter(polegestioncode %in% input$ChxReg0,NumSemaine %in% input$ChxNumSemN0) %>%
    #   select(c(10,19,4:6,11:18,21,22))
    # names(Export_Donnees) <-c("Pôle Gestion","Numéro de semaine","semaine de référence"
    #                           ,"Début collecte","Fin collecte","Au moins 1 contact (enq)","Questionnaire démarré (enq)","Finalisé (enq)","Refus (enq)"
    #                           ,"Hors champ (enq)","Validé (enq)","Encours (Web)","Validé (Web)","VarTemp : Refus (Pôle EEC)","VarTemp : Hors champ (Pôle EEC)")
    
    # ---------------------------------------------------------------------------------------------------------------
    
    
    Export_Donnees <- Donnees()
    Export_Donnees <- Export_Donnees %>% 
      filter(polegestioncode %in% input$ChxReg0,NumSemaine %in% input$ChxNumSemN0) %>%
      select(c(10,19,4:6,11:18,21,22))
    names(Export_Donnees) <-c("Pôle Gestion","Numéro de semaine","semaine de référence"
                              ,"Début collecte","Fin collecte","Enquêteur_Au moins 1 contact","Enquêteur_Questionnaire démarré","Enquêteur_Finalisé","Enquêteur_Refus"
                              ,"Enquêteur_Hors champ","Enquêteur_Validé","Web_En cours","Web_Validé"," Pôle EEC_VarTemp : Refus","Pôle EEC_VarTemp : Hors champ")

    Export_Donnees <- Export_Donnees %>% 
      gather("Etat","RESULTAT",-c(1:5))%>% 
      filter(RESULTAT==TRUE)%>%
      separate(Etat, c("mode","Res"),sep="_")%>% 
      select(-RESULTAT) %>%  
      arrange(`Pôle Gestion`,`Numéro de semaine`)
    names(Export_Donnees)[c(6:7)] <- c("Mode de collecte","Etat de la FA")
    
    Export_Donnees <- Export_Donnees %>% 
      filter(`Mode de collecte` %in% input$ModCollect)
    
    # ---------------------------------------------------------------------------------------------------------------
    
    
    # Création de fonctions spécifiques au tableau : boutons affichage et 'désaffichage' de colonnes + Réorganisation des colonnes + Sélection de cellules
    datatable(
      Export_Donnees,
      rownames = T,
      extensions = c('ColReorder','KeyTable','FixedHeader','Buttons'),
      options = list(dom = 'Bfrtip',
                     # extensions : ColReorder (bouger les colonnes)
                     colReorder = TRUE,
                     # extensions : KeyTable (cliquer sur une cellule)
                     keys = TRUE,

                     # extensions : FixedHeader
                     pageLength = 50,
                     fixedHeader = F,

                     # Formattage de la ligne d'en-tête (couleurs)
                     initComplete = JS("function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': 'LightBlue', 'color': 'Black'});","}"),

                     # extensions : Buttons (colonnes à afficher ou 'désafficher')
                     buttons = list(list(extend = 'colvis', columns = c(3:5))))) %>%
      formatStyle(c(3:5), backgroundColor = 'LightBlue')
  })
  
  # I/ SUIVI DEM ___________________________________________________________________________________________________________________________________ ####
  # IA1/ SELECTINPUT : FILTRE REGIONS (i.e. ETABLISSEMENTS) --------------------------------------------------------------------------------------------------------------- 
  output$Afficher_ChxReg <- renderUI({
    selectInput("ChxReg",
                h3("Sélectionnez une ou plusieurs régions"),
                choices = ListReg,
                multiple = F,
                width = "50%",
                selected = "ETB de LILLE")
  })
  
  # IA2 - SELECTINPUT : FILTRE SEMAINES DE REFERENCE ----------------------------------------------------------------------------------------
  output$Afficher_Chx_SemaineRef2 <- renderUI({
    selectInput("ChxNumSemN2",
                h3("Sélectionnez la ou les semaines de référence"),
                choices = sort(ListSemaineRef),
                multiple = T,
                width = "50%",
                selected = tail(ListSemaineRef,3))
  })
  
  # IB/ DATATABLEOUTPUT : TABLEAU DE SUIVI DEM -------------------------------------------------------------------------------------------------------------------
    output$SuiviDem <- renderDataTable({
    
    if (input$TypeValeurs_DEM == 1){
      Stats_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2 ) %>%
        group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats_DEM <- Stats_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA",
                                "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      
      
      Stats_Enqu_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,IdentifEnqueteur) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats_Enqu_DEM$nograp <- c("ENSEMBLE")
      Stats_Enqu_DEM$NumSemaine <- c(" ")
      Stats_Enqu_DEM <- Stats_Enqu_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA",
                                          "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                          "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      colnames(Stats_Enqu_DEM)
      
      
      
      Stats0_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats0_DEM$nograp <- c(" ")
      Stats0_DEM$IdentifEnqueteur <- c("TOTAUX")
      Stats0_DEM <- Stats0_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                  "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      
      
      StatsTot_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      # colnames(StatsTot_DEM)
      StatsTot_DEM$polegestioncode <- c("TOTAL")
      StatsTot_DEM$IdentifEnqueteur <- c(" ")
      StatsTot_DEM$nograp <- c(" ")
      StatsTot_DEM$NumSemaine <- c(" ")
      
      StatsTot_DEM <- StatsTot_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA",
                                      "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                      "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      
      
      
      Stats1_DEM <- bind_rows(Stats_DEM,Stats_Enqu_DEM)
      Stats1_DEM <- Stats1_DEM[order(Stats1_DEM$polegestioncode,Stats1_DEM$IdentifEnqueteur),]
      Stats1_DEM <- bind_rows(Stats1_DEM,Stats0_DEM)
      Stats1_DEM <- bind_rows(Stats1_DEM,StatsTot_DEM)
      # ------------------------------------------------------------------------------------------------------
      sketch1 = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 5, ' '),
            th(colspan = 5, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX') 
          ),
          tr(
            lapply(c("REG","Enquêteur","Grappe","Semaine de référence","Total FA","Au moins un essai de contact"
                     ,"Questionnaire démarré","Finalisation","Refus / Hors champ","Validé","en cours","Validé"
                     ,"Refus/Hors champ","Total validé","Total Refus/Hors champ", "Reste à traiter"), th)
          )
        )
      ))
      # ------------------------------------------------------------------------------------------------------
      
      # Création de fonctions spécifiques au tableau : boutons Copier, Imprimer et télécharger + Réorganisation des colonnes + Sélection de cellules
      datatable(Stats1_DEM,
                # Ajout de la ligne d'en tête'
                container = sketch1, rownames = F,
                extensions = c('Buttons','KeyTable','ColReorder','FixedHeader'),
                options =list(dom = 'Bfrtip',
                  keys = TRUE, 
                  colReorder = F,
                  # extensions : FixedHeader
                  pageLength = 100,
                  
                  initComplete = JS("function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': 'PowderBlue', 'color': 'Black'});","}"),
                  buttons = list('copy', 'print',list(extend = 'collection',
                                                      buttons = c('csv', 'excel', 'pdf'),
                                                      text = 'Download')))) %>% 
        formatStyle(c(1:5,10,12),backgroundColor = 'PowderBlue') %>% 
        formatStyle(c(14:16),backgroundColor = 'SkyBlue') 
       
    }else if(input$TypeValeurs_DEM == 2){
    
      Stats_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      Stats_DEM <- Stats_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA"
                                ,"Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide"
                                ,"EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste")]
      
      
      Stats_Enqu_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,IdentifEnqueteur) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      Stats_Enqu_DEM$nograp <- c("ENSEMBLE")
      Stats_Enqu_DEM$NumSemaine <- c(" ")
      Stats_Enqu_DEM <- Stats_Enqu_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA",
                                          "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                          "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      colnames(Stats_Enqu_DEM)
      
      
      
      Stats0_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      Stats0_DEM$IdentifEnqueteur <- c("TOTAUX")
      Stats0_DEM$nograp <- c(" ")
      Stats0_DEM <- Stats0_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA"
                                  ,"Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide"
                                  ,"EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste")]
      
      
      
      
      
      StatsTot_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      StatsTot_DEM$polegestioncode <- c("TOTAL")
      StatsTot_DEM$IdentifEnqueteur <- c(" ")
      StatsTot_DEM$nograp <- c(" ")
      StatsTot_DEM$NumSemaine <- c(" ")
      
      StatsTot_DEM <- StatsTot_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA",
                                      "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                      "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      
      
      
      Stats1_DEM <- bind_rows(Stats_DEM,Stats_Enqu_DEM)
      Stats1_DEM <- Stats1_DEM[order(Stats1_DEM$polegestioncode,Stats1_DEM$IdentifEnqueteur),]
      Stats1_DEM <- bind_rows(Stats1_DEM,Stats0_DEM)
      Stats1_DEM <- bind_rows(Stats1_DEM,StatsTot_DEM)
      
      
      # ------------------------------------------------------------------------------------------------------
      sketch2 = htmltools::withTags(table(
        # tableHeader("%"),
        class = 'display',
        thead(
          tr(
            th(colspan = 5, '(%) '),
            th(colspan = 5, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX')
          ),
          tr(
            lapply(c("REG","Enquêteur","Grappe","Semaine de référence","Total FA","Au moins un essai de contact"
                     ,"Questionnaire démarré","Finalisation","Refus / Hors champ","Validé","Web : en cours","web : validé"
                     ,"Pôle : Refus/Hors champ","Total validé","Total Refus/Hors champ", "Reste à traiter"), th)
          )
        )
      ))
      # ------------------------------------------------------------------------------------------------------
      
      # Création de fonctions spécifiques au tableau : boutons Copier, Imprimer et télécharger + Réorganisation des colonnes + Sélection de cellules
      datatable(Stats1_DEM,
                # Ajout de la ligne d'en tête'
                container = sketch2, rownames = F,
                
                extensions = c('Buttons','ColReorder','KeyTable','FixedHeader'),
                options =list(dom = 'Bfrtip',
                              keys = TRUE,
                              colReorder = F,
                              # extensions : FixedHeader
                              pageLength = 100,
                              
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'PowderBlue', 'color': 'Black'});","}"),
                              buttons = list('copy', 'print',list(extend = 'collection',
                                                                  buttons = c('csv', 'excel', 'pdf'),
                                                                  text = 'Download')))) %>% 
        formatStyle(c(1:5,10,12),backgroundColor = 'PowderBlue') %>% 
        formatStyle(c(14:16),backgroundColor = 'SkyBlue') %>% 
        
        formatStyle(c(5:16),
                    color = styleInterval(c(0.50,0.90),c("DarkRed","Black","DarkBlue")))  %>%
        
        formatPercentage(c(6:16),2)
    }
    })
  
  # II/ SUIVI CONCEPTEUR ___________________________________________________________________________________________________________________________________ ####
  # IIA1/ SELECTINPUT : FILTRE REGIONS (i.e. ETABLISSEMENTS) -----------------------------------------------------------------------------------------------------------------------
  output$Afficher_ChxReg2 <- renderUI({
    selectInput("ChxReg2",
                h3("Sélectionnez une ou plusieurs régions"),
                choices = ListReg,
                multiple = T,
                width = "50%",
                selected = ListReg)
  })
  
  # IIA2/ SELECTINPUT : FILTRE SEMAINES DE REFERENCE ----------------------------------------------------------------------------------------
  output$Afficher_Chx_SemaineRef <- renderUI({
    selectInput("ChxNumSemN",
                h3("Sélectionnez la ou les semaines de référence"),
                choices = sort(ListSemaineRef),
                multiple = T,
                width = "50%",
                selected = tail(ListSemaineRef,4))
    
  })
  
  # IIB/ DATATABLEOUTPUT : TABLEAU SUIVI CONCEPTEUR ---------------------------------------------------------------------------------------------------------------
  output$SuiviConcepteur <- renderDataTable({
    
    if (input$TypeValeurs_CPS == 1){
      Stats_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN ) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats_CPS <- Stats_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                ,"Total_Valide","Total_Refus_HC","Reste")]
      
      
      Stats_REG_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats_REG_CPS$NumSemaine <- c("ENSEMBLE")
      Stats_REG_CPS <- Stats_REG_CPS[,c("polegestioncode","NumSemaine","Total_FA",
                                        "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                        "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      colnames(Stats_REG_CPS)
      
      
      
      Stats0_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats0_CPS$polegestioncode <- c("Total Hebdo")
      Stats0_CPS <- Stats0_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                  ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                  ,"Total_Valide","Total_Refus_HC","Reste")]
      
      
      StatsTot_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      # colnames(StatsTot_CPS)
      StatsTot_CPS$polegestioncode <- c("TOTAL")
      StatsTot_CPS$NumSemaine <- c("ENSEMBLE")
      
      StatsTot_CPS <- StatsTot_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                      ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                      ,"Total_Valide","Total_Refus_HC","Reste")]
      
      
      
      Stats1_CPS <- bind_rows(Stats_CPS,Stats_REG_CPS)
      Stats1_CPS <- arrange(Stats1_CPS,polegestioncode,desc(NumSemaine))
      Stats1_CPS <- bind_rows(Stats1_CPS,Stats0_CPS)
      Stats1_CPS <- bind_rows(Stats1_CPS,StatsTot_CPS)
      
      
      
      # ------------------------------------------------------------------------------------------------------
      sketch1 = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 3, ' '),
            th(colspan = 5, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX') 
          ),
          tr(
            lapply(c("REG","Semaine de référence","Total FA","Au moins un essai de contact"
                     ,"Questionnaire démarré","Finalisation","Refus / Hors champ","Validé","en cours","Validé"
                     ,"Refus/Hors champ","Total validé","Total Refus/Hors champ", "Reste à traiter"), th)
          )
        )
      ))
      # ------------------------------------------------------------------------------------------------------
      
      # Création de fonctions spécifiques au tableau : boutons Copier, Imprimer et télécharger + Réorganisation des colonnes + Sélection de cellules
      datatable(Stats1_CPS,
                # Ajout de la ligne d'en tête'
                container = sketch1, rownames = F,
                
                extensions = c('Buttons','ColReorder','KeyTable',"FixedHeader"),
                options =list(dom = 'Bfrtip',
                              keys = TRUE,
                              # extensions : FixedHeader
                              pageLength = 100,
                              fixedHeader = F,
                              
                              colReorder = F,
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'Cornsilk', 'color': 'Black'});","}"),
                              buttons = list('copy', 'print',list(extend = 'collection',
                                                                  buttons = c('csv', 'excel', 'pdf'),
                                                                  text = 'Download')))) %>%         
        formatStyle(c(1:3,8,10),backgroundColor = 'Cornsilk') %>% 
        formatStyle(c(12:14),backgroundColor = 'Bisque') 
      
    }else if(input$TypeValeurs_CPS == 2){

      
      Stats_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      Stats_CPS <- Stats_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                ,"Total_Valide","Total_Refus_HC","Reste")]
      
      
      Stats_REG_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      Stats_REG_CPS$NumSemaine <- c("ENSEMBLE")
      Stats_REG_CPS <- Stats_REG_CPS[,c("polegestioncode","NumSemaine","Total_FA",
                                        "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide",
                                        "Web_EnCours","Web_Valide","EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste" )]
      colnames(Stats_REG_CPS)
      
      Stats0_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(NumSemaine) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC/TOTFA,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      Stats0_CPS$polegestioncode <- c("Total Hebdo")
      Stats0_CPS <- Stats0_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                  ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                  ,"Total_Valide","Total_Refus_HC","Reste")]
      
      StatsTot_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        summarise(TOTFA=sum((aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                            +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(EEC_refus==TRUE)+(EEC_horschamp==TRUE)),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum((EEC_refus==TRUE)+(EEC_horschamp==TRUE))/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC/TOTFA,
                  Total_FA=TOTFA,
                  Reste=1-Total_Valide-Total_Refus_HC)
      # colnames(StatsTot_CPS)
      StatsTot_CPS$polegestioncode <- c("TOTAL")
      StatsTot_CPS$NumSemaine <- c(" ")
      
      StatsTot_CPS <- StatsTot_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                      ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                      ,"Total_Valide","Total_Refus_HC","Reste")]
      
      
      Stats1_CPS <- bind_rows(Stats_CPS,Stats_REG_CPS)
      Stats1_CPS <- arrange(Stats1_CPS,polegestioncode,desc(NumSemaine))
      Stats1_CPS <- bind_rows(Stats1_CPS,Stats0_CPS)
      Stats1_CPS <- bind_rows(Stats1_CPS,StatsTot_CPS)
      
      # ------------------------------------------------------------------------------------------------------
      sketch2 = htmltools::withTags(table(
        # tableHeader("%"),
        class = 'display',
        thead(
          tr(
            th(colspan = 3, '(%) '),
            th(colspan = 5, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX')
          ),
          tr(
            lapply(c("REG","Semaine de référence","Total FA","Au moins un essai de contact"
                     ,"Questionnaire démarré","Finalisation","Refus / Hors champ","Validé","en cours","Validé"
                     ,"Refus/Hors champ","Total validé","Total Refus/Hors champ", "Reste à traiter"), th)
          )
        )
      ))
      # ------------------------------------------------------------------------------------------------------
      
      # Création de fonctions spécifiques au tableau : boutons Copier, Imprimer et télécharger + Réorganisation des colonnes + Sélection de cellules
      datatable(Stats1_CPS,
                # Ajout de la ligne d'en tête'
                container = sketch2, rownames = F,
                
                extensions = c('Buttons','ColReorder','KeyTable','FixedHeader'),
                options =list(dom = 'Bfrtip',
                              keys = TRUE,
                              colReorder = F,
                              # extensions : FixedHeader
                              pageLength = 100,
                              
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'Cornsilk', 'color': 'Black'});","}"),
                              buttons = list('copy', 'print',list(extend = 'collection',
                                                                  buttons = c('csv', 'excel', 'pdf'),
                                                                  text = 'Download')))) %>% 
        formatStyle(c(1:3,8,10),backgroundColor = 'Cornsilk') %>% 
        formatStyle(c(12:14),backgroundColor = 'Bisque') %>% 
        formatStyle(c(3:14),color = styleInterval(c(0.50,0.90),c("DarkRed","Black","DarkBlue"))) %>%
        
        formatPercentage(c(4:14),2)
      # %>%
        # formatPercentage(c(4:8,10:13),1)
    }
  })
}
