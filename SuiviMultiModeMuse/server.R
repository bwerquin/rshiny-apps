CheminPkg <- "Packages/"

library(shiny,lib.loc = CheminPkg)
library(jsonlite,lib.loc = CheminPkg)
library(dplyr,lib.loc = CheminPkg)
library(httr,lib.loc = CheminPkg)
library(devtools,lib.loc = CheminPkg)
library(curl,lib.loc = CheminPkg)
library(DT,lib.loc = CheminPkg)
server <- function(input, output) {
  
  # 0/ DONNEES  ___________________________________________________________________________________________________________________________________ ####
  # 0A/ RECUPERATION DES DONNEES --------------------------------------------------------------------------------------------------------------- 
  Donnees <- reactive({
    # 0A1/ ACCES AU WEBSERVICE ----------------------------------------------------------------------------------------
    
    # config = httr::config(ssl_verifypeer = 0L)
    # set_config(config)
    # Donnees <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
    # Donnees <- fromJSON(content(Donnees, "text"))
    # Donnees$NumSemaine <- paste0("S.",format(as.Date(Donnees$semainereference), "%U"))
    # Donnees$IdentifEnqueteur <- paste0(Donnees$enqueteurnom," ",Donnees$enqueteurprenom)
    # 
    # # EN ATTENDANT D'AVOIR LES VERITABLES VARIABLES RELATIVES AU POLE EEC
    # Donnees$EEC_refus <- Donnees$refus
    # Donnees$EEC_horschamp <- Donnees$horschamp
    
    
    # ----------------------------------------------------------------------------------------
    # !!! LORSQUE PBM DE CONNEXION AU WEBSERVICE
    Donnees <- readRDS("Temp/Donnees5.rds")
    # Ou Donnees2 pour avoir plus de données (2 établissements)
    # Donnees <- readRDS("Temp/Donnees2.rds")
    
    # ----------------------------------------------------------------------------------------
    
    return(Donnees)
  })
  
  # 0B/ DATATABLEOUTPUT : TABLEAU ENSEMBLE DES DONNEES -------------------------------------------------------------------------------------------------------------------
  output$Donnees <- renderDataTable({
    Export_Donnees <- Donnees()
    names(Export_Donnees) <- c("Numéro Grappe","nolog","nole","semaine de référence","Début collecte","Fin collecte","Nom enquêteur"
                               ,"Prénom","IDEP","Pôle Gestion","Au moins 1 contact (enq)","Questionnaire démarré (enq)","Finalisé (enq)","Refus (enq)"
                               ,"Hors champ (enq)","Validé (enq)","Encours (Web)","Validé (Web)","Numéro de semaine","Identif enquêteur","VarTemp : Refus (Pôle EEC)","VarTemp : Hors champ (Pôle EEC)")
    Export_Donnees <- Export_Donnees[,c(10,20,7:9,1,19,4:6,2,3,11:18,21,22)]
    
    # Création de fonctions spécifiques au tableau : boutons affichage et 'désaffichage' de colonnes + Réorganisation des colonnes + Sélection de cellules
    datatable(
      Export_Donnees, rownames = T,
      extensions = c('ColReorder','KeyTable','FixedHeader','Buttons'),
      options = list(dom = 'Bfrtip',
                     # extensions : ColReorder (bouger les colonnes)
                     colReorder = TRUE,
                     # extensions : KeyTable (cliquer sur une cellule)
                     keys = TRUE,
                     
                     # extensions : FixedHeader
                     pageLength = 20,
                     fixedHeader = F,
                     
                     # Formattage de la ligne d'en-tête (couleurs)
                     initComplete = JS("function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': 'LightBlue', 'color': 'Black'});","}"),
                     
                     # extensions : Buttons (colonnes à afficher ou 'désafficher')
                     buttons = list(list(extend = 'colvis', columns = c(2:6,8:22))))) %>% 
      
      # Formattage des colonnes (couleurs)
      formatStyle(c(1,7), backgroundColor = 'LightBlue') 
  })
  
  # I/ SUIVI DEM ___________________________________________________________________________________________________________________________________ ####
  # IA/ FILTRE REGIONS (i.e. ETABLISSEMENTS) --------------------------------------------------------------------------------------------------------------- 
  output$Afficher_ChxReg <- renderUI({
    # IA1 - ACCES AU WEBSERVICE : RECUPERATION DES NOMS DE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    
    # config = httr::config(ssl_verifypeer = 0L)
    # set_config(config)
    # Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
    # Reg <- fromJSON(content(Reg, "text"))
    # 
    # # POUR LES TESTS, EN ATTENDANT D'AVOIR PLUS DE DONNEES
    # ListReg <- as.data.frame(c("Ensemble des régions","ETB de LILLE","ETB de TOULOUSE","ETB de TOULOUSE",Reg))
    # # ListReg <- as.data.frame(c("Ensemble des régions",Reg))
    # 
    # names(ListReg) <- "NomsReg"
    # ListReg <- as.character(unique(ListReg$NomsReg))
    
    
    # ----------------------------------------------------------------------------------------
    # !!! LORSQUE PBM DE CONNEXION AU WEBSERVICE
    ListReg <- readRDS("Temp/ListReg.rds")
    
    # IA2 - SELECTINPUT : CONSTRUCTION DU FILTRE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    selectInput("ChxReg",
                h3("Sélectionnez une ou plusieurs régions"),
                choices = ListReg,
                multiple = F,
                width = "50%",
                selected = "ETB de LILLE")
  })
  # IIA3 - SELECTINPUT : FILTRE SEMAINES DE REFERENCE ----------------------------------------------------------------------------------------
  output$Afficher_Chx_SemaineRef2 <- renderUI({
    # ListSemaineRef <- Donnees()$NumSemaine
    # ListSemaineRef <- unique(ListSemaineRef)
    ListSemaineRef=c("S.42","S.43","S.44","S.45","S.41","S.47","S.48","S.51","S.52")
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
      # TOTAUX PAR ETAB ET SEMAINE
      Stats0_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum(refus==TRUE) + sum(horschamp==TRUE),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(EEC_refus==TRUE) + sum(EEC_horschamp==TRUE),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats0_DEM$nograp <- c(" ")
      Stats0_DEM$IdentifEnqueteur <- c("TOTAUX")
      Stats0_DEM <- Stats0_DEM[,c(1,16,15,2,13,3:12,14)]
      
      # NOMBRE DE FA PAR ETAB, ENQUETEUR, GRAPPE ET SEMAINE
      Stats_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2 ) %>%
        group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum(refus==TRUE) + sum(horschamp==TRUE),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(EEC_refus==TRUE) + sum(EEC_horschamp==TRUE),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats_DEM <- Stats_DEM[,c(1:4,15,5:14,16)]
      
      Stats1_DEM <- bind_rows(Stats_DEM,Stats0_DEM)
      
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
                
                extensions = c('KeyTable','ColReorder'),
                options =list(
                  keys = TRUE,
                  colReorder = F,
                  initComplete = JS("function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': 'PowderBlue', 'color': 'Black'});","}"))) %>% 
        formatStyle(c(1:5,10,12),backgroundColor = 'PowderBlue') %>% 
        formatStyle(c(14:16),backgroundColor = 'SkyBlue')
      
      
    }else if(input$TypeValeurs_DEM == 2){
      # filter(polegestioncode %in% input$ChxReg || input$ChxReg =="Ensemble des régions") %>%
      
      Stats0_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(TOTFA=sum(aaumoinsuncontact==TRUE)+sum(questdemarreenqueteur==TRUE)+sum(finaliseenqueteur==TRUE)+sum(refus,horschamp==TRUE)
                  +sum(valideenqueteur==TRUE)+sum(encoursinternet==TRUE)+sum(valideinternet==TRUE)+sum(EEC_refus==TRUE,EEC_horschamp==TRUE),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum(refus,horschamp==TRUE)/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum(EEC_refus==TRUE,EEC_horschamp==TRUE)/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide/TOTFA,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC/TOTFA,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC/TOTFA,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC/Total_FA)
      Stats0_DEM$IdentifEnqueteur <- c("TOTAUX")
      Stats0_DEM$nograp <- c(" ")
      Stats0_DEM <- Stats0_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA"
                                  ,"Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide"
                                  ,"EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste")]
      
      
      Stats_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2) %>%
        group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
        summarise(TOTFA=sum(aaumoinsuncontact==TRUE)+sum(questdemarreenqueteur==TRUE)+sum(finaliseenqueteur==TRUE)+sum(refus,horschamp==TRUE)
                  +sum(valideenqueteur==TRUE)+sum(encoursinternet==TRUE)+sum(valideinternet==TRUE)+sum(EEC_refus==TRUE,EEC_horschamp==TRUE),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum(refus,horschamp==TRUE)/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum(EEC_refus==TRUE,EEC_horschamp==TRUE)/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide/TOTFA,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC/TOTFA,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC/TOTFA,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC/Total_FA)
      Stats_DEM <- Stats_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Total_FA"
                                ,"Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide"
                                ,"EEC_Refus_HC","Total_Valide","Total_Refus_HC","Reste")]
      
      Stats1_DEM <- bind_rows(Stats_DEM,Stats0_DEM)
      
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
                
                extensions = c('ColReorder','KeyTable'),
                options =list(keys = TRUE,
                              colReorder = F,
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'PowderBlue', 'color': 'Black'});","}"))) %>% 
        formatStyle(c(1:5,10,12),backgroundColor = 'PowderBlue') %>% 
        formatStyle(c(14:16),backgroundColor = 'SkyBlue') %>% 
      
        formatStyle(c(5:16),
                    color = styleInterval(c(0.50,0.90),c("DarkRed","Black","DarkBlue")))  %>%
        
        formatPercentage(c(5,11,16),0) %>%
        formatPercentage(c(6:10,12:15),1)
    }
    
  })
  
  # II/ SUIVI CONCEPTEUR ___________________________________________________________________________________________________________________________________ ####
  # IIA/ FILTRE REGIONS (i.e. ETABLISSEMENTS) -----------------------------------------------------------------------------------------------------------------------
  output$Afficher_ChxReg2 <- renderUI({
    # IIA1 - ACCES AU WEBSERVICE : RECUPERATION DES NOMS DE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    # config = httr::config(ssl_verifypeer = 0L)
    # set_config(config)
    # Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
    # Reg <- fromJSON(content(Reg, "text"))
    # 
    # # POUR LES TESTS, EN ATTENDANT D'AVOIR PLUS DE DONNEES
    # ListReg <- as.data.frame(c("Ensemble des régions","ETB de LILLE","ETB de TOULOUSE","ETB de TOULOUSE",Reg))
    # # ListReg <- as.data.frame(c("Ensemble des régions",Reg))
    # 
    # names(ListReg) <- "NomsReg"
    # ListReg <- as.character(unique(ListReg$NomsReg))
    
    
    # ----------------------------------------------------------------------------------------
    # !!! LORSQUE PBM DE CONNEXION AU WEBSERVICE (PENDANT LE DEVELOPPEMENT)
    ListReg <- readRDS("Temp/ListReg.rds")
    
    # IIA2 - SELECTINPUT : FILTRE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    selectInput("ChxReg2",
                h3("Sélectionnez une ou plusieurs régions"),
                choices = ListReg,
                multiple = T,
                width = "50%",
                selected = ListReg)
  })
  
  # IIA3 - SELECTINPUT : FILTRE SEMAINES DE REFERENCE ----------------------------------------------------------------------------------------
  output$Afficher_Chx_SemaineRef <- renderUI({
    # ListSemaineRef <- Donnees()$NumSemaine
    # ListSemaineRef <- unique(ListSemaineRef)
    ListSemaineRef=c("S.42","S.43","S.44","S.45","S.46","S.41","S.47","S.48","S.49","S.50","S.51","S.52")
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
      
      Stats0_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum(refus,horschamp==TRUE),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(EEC_refus==TRUE,EEC_horschamp==TRUE),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats0_CPS$polegestioncode <- c("Total")
      Stats0_CPS <- Stats0_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                  ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                  ,"Total_Valide","Total_Refus_HC","Reste")]
      
      Stats_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum(refus,horschamp==TRUE),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(EEC_refus==TRUE,EEC_horschamp==TRUE),
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC)
      Stats_CPS <- Stats_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                ,"Total_Valide","Total_Refus_HC","Reste")]
      
      Stats1_CPS <- bind_rows(Stats_CPS,Stats0_CPS)
      
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
                
                extensions = c('KeyTable','ColReorder'),
                options =list(keys = TRUE,
                              colReorder = F,
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'Cornsilk', 'color': 'Black'});","}"))) %>% 
        
      formatStyle(c(1:3,8,10),backgroundColor = 'Cornsilk') %>% 
        formatStyle(c(12:14),backgroundColor = 'Bisque') 
        
      
    }else if(input$TypeValeurs_CPS == 2){
      # filter(polegestioncode %in% input$ChxReg || input$ChxReg =="Ensemble des régions") %>%
      
      Stats0_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(NumSemaine) %>%
        summarise(TOTFA=sum(aaumoinsuncontact==TRUE)+sum(questdemarreenqueteur==TRUE)+sum(finaliseenqueteur==TRUE)+sum(refus,horschamp==TRUE)
                  +sum(valideenqueteur==TRUE)+sum(encoursinternet==TRUE)+sum(valideinternet==TRUE)+sum(EEC_refus==TRUE,EEC_horschamp==TRUE),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum(refus,horschamp==TRUE)/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum(EEC_refus==TRUE,EEC_horschamp==TRUE)/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide/TOTFA,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC/TOTFA,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC/TOTFA,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC/Total_FA)
      Stats0_CPS$polegestioncode <- c("Total")
      Stats0_CPS <- Stats0_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                  ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                  ,"Total_Valide","Total_Refus_HC","Reste")]
      
      Stats_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(TOTFA=sum(aaumoinsuncontact==TRUE)+sum(questdemarreenqueteur==TRUE)+sum(finaliseenqueteur==TRUE)+sum(refus,horschamp==TRUE)
                  +sum(valideenqueteur==TRUE)+sum(encoursinternet==TRUE)+sum(valideinternet==TRUE)+sum(EEC_refus==TRUE,EEC_horschamp==TRUE),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/TOTFA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/TOTFA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/TOTFA,
                  Enq_Refus_HC=sum(refus,horschamp==TRUE)/TOTFA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/TOTFA,
                  Web_EnCours=sum(encoursinternet==TRUE)/TOTFA,
                  Web_Valide=sum(valideinternet==TRUE)/TOTFA,
                  EEC_Refus_HC=sum(EEC_refus==TRUE,EEC_horschamp==TRUE)/TOTFA,
                  Total_Valide=Enq_Valide+Web_Valide/TOTFA,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC/TOTFA,
                  Total_FA=Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC/TOTFA,
                  Reste=Total_FA-Total_Valide-Total_Refus_HC/Total_FA)
      Stats_CPS <- Stats_CPS[,c("polegestioncode","NumSemaine","Total_FA","Enq_AuMoinsUn","Enq_Demarre"
                                ,"Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide","EEC_Refus_HC"
                                ,"Total_Valide","Total_Refus_HC","Reste")]
      
      Stats1_CPS <- bind_rows(Stats_CPS,Stats0_CPS)
      
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
                
                extensions = c('ColReorder','KeyTable'),
                options =list(keys = TRUE,
                              colReorder = F,
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'Cornsilk', 'color': 'Black'});","}"))) %>% 
        formatStyle(c(1:3,8,10),backgroundColor = 'Cornsilk') %>% 
        formatStyle(c(12:14),backgroundColor = 'Bisque') %>% 
        formatStyle(c(3:14),color = styleInterval(c(0.50,0.90),c("DarkRed","Black","DarkBlue"))) %>%
        
        formatPercentage(c(3,9,14),0) %>%
        formatPercentage(c(4:8,10:13),1)
    }
  })
}
