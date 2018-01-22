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
    ## 0A1/ ACCES AU WEBSERVICE ----------------------------------------------------------------------------------------
    config = httr::config(ssl_verifypeer = 0L)
    set_config(config)
    Donnees <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
    Donnees <- fromJSON(content(Donnees, "text"))
    
    
    ## 0A2/ CREATION VARIABLES  ----------------------------------------------------------------------------------------
    ## 0A20/ Variables divers : NumSemaine & IdentifEnqueteur  ----------------------------------------------------------------------------------------
    Donnees$NumSemaine <- paste0(".S.",format(as.Date(Donnees$semainereference), "%U"))
    # Donnees$IdentifEnqueteur <- paste0(Donnees$enqueteurnom," ",Donnees$enqueteurprenom)
    
    ## 0A21/ Indicatrice FA en cours de collecte (commencée et non terminée)  ----------------------------------------------------------------------------------------
    # Oui = FA en cours soit : internet, au moins un contact, démarré enquêteur ou finalisation enquêteur
    # Non = FA non démarrées ou validée soit : Aucun essai, Refus et Hors champ (enquêteur ou Pôle) ou FA validée
    Donnees$Indic_FaEnCours <- ifelse(Donnees$encoursinternet==TRUE,"Oui",
                                      ifelse(Donnees$aaumoinsuncontact==TRUE,"Oui",
                                             ifelse(Donnees$questdemarreenqueteur==TRUE,"Oui",
                                                    ifelse(Donnees$finaliseenqueteur==TRUE,"Oui","Non"))))
    
    ## 0A22/ Indicatrice FA démarrée ou non  ----------------------------------------------------------------------------------------
    # Concerne le mode de collecte enquêteur
    # Non = aucun essai de contact
    # Oui = au moins un contact ou démarré ou finalisation ou refus ou hors champ ou FA validée par l'enquêteur 
    Donnees$Indic_ContactEtabli <- ifelse(Donnees$aaumoinsuncontact==TRUE,"Oui",
                                          ifelse(Donnees$questdemarreenqueteur==TRUE,"Oui",
                                                 ifelse(Donnees$finaliseenqueteur==TRUE,"Oui",
                                                        ifelse(Donnees$finaliseenqueteur==TRUE,"Oui",
                                                               ifelse(Donnees$refus==TRUE,"Oui",
                                                                      ifelse(Donnees$horschamp==TRUE,"Oui",
                                                                             ifelse(Donnees$valideenqueteur==TRUE,"Oui","Non")))))))
    
    # TEMPORAIRE - A SUPPRIMER DES QUE JEU DE DONNEES COHERENTES : modif numéro GRAPPE ERIC & MICKAEL car déjà pris
    Donnees$nograp[Donnees$enqueteurnom == "Thuaud"] <- "80630BVL"
    Donnees$nograp[Donnees$enqueteurnom == "Moineau"] <- "80000AMS"
    
    # CREATION D'UNE TABLE DE PASSAGE POUR IdentifEnqueteur ####
    # LES FA WEB sont affetées à un numéro de grappe et non à l'enquêteur
    # Une grappe = un enquêteur
    # Il faut rattacher les FA WEB à l'enquêteur responsable de la grappe
    TabPassage_EnqGrap <- Donnees %>% 
      select(nograp,enqueteurnom,enqueteurprenom) %>% 
      filter(enqueteurnom != "") %>%  
      distinct(nograp,enqueteurnom,enqueteurprenom) %>%
      mutate(IdentifEnqueteur = paste0(toupper(enqueteurnom)," ",enqueteurprenom)) %>% 
      select(nograp,IdentifEnqueteur) %>% 
      arrange(nograp)
    
    Donnees <- left_join(Donnees,TabPassage_EnqGrap,"nograp")
    
    # ----------------------------------------------------------------------------------------
    # !!! POUR TESTER AVEC UN JEU DE DONNEES PLUS CONSEQUENT ... 
    # Donnees <- readRDS("Temp/BigDonnees.rds")
    # ----------------------------------------------------------------------------------------
    
    return(Donnees)
  })
  
  # 0B/ FILTRES  -----------------------------------------------------------------------------------------------------------------------
  output$Afficher_ChxReg0 <- renderUI({
    # 0B0 - ACCES AU WEBSERVICE : RECUPERATION DES NOMS DE REGIONS (i.e. ETABLISSEMENTS) ----------------------------------------------------------------------------------------
    # config = httr::config(ssl_verifypeer = 0L)
    # set_config(config)
    # Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
    # ListReg <<- fromJSON(content(Reg, "text"))
    # ----------------------------------------------------------------------------------------
    ListReg <<- sort(unique(Donnees()$polegestioncode))
    # ----------------------------------------------------------------------------------------
    selectInput("ChxReg0",
                h3("Sélectionnez une ou plusieurs régions"),
                choices = ListReg,
                multiple = T,
                width = "50%",
                selected = ListReg)
  })
  
  # 0B1 - SELECTINPUT : FILTRE SEMAINES DE REFERENCE ----------------------------------------------------------------------------------------
  output$Afficher_Chx_SemaineRef0 <- renderUI({
    ListSemaineRef <<- sort(unique(Donnees()$NumSemaine))
    
    selectInput("ChxNumSemN0",
                h3("Sélectionnez la ou les semaines de référence"),
                choices = ListSemaineRef,
                multiple = T,
                width = "50%",
                selected = tail(ListSemaineRef,3))
    
  })
  
  # 0B2 - SELECTINPUT : FILTRE GRAPPES ----------------------------------------------------------------------------------------
  output$Afficher_Chx_Grappes <- renderUI({
    ListGrappes <- sort(unique(Donnees()$nograp),decreasing=TRUE)
    selectInput("ChxGrappes",
                h3("Sélectionnez une ou plusieurs grappes"),
                choices = ListGrappes,
                multiple = T,
                width = "50%",
                selected = head(ListGrappes,4))
    
  })
  
  # 0C/ DATATABLEOUTPUT : TABLEAU ENSEMBLE DES DONNEES -------------------------------------------------------------------------------------------------------------------
  output$ExportDonnees <- renderDataTable({
    Export_Donnees <- Donnees() %>% 
      filter(polegestioncode %in% input$ChxReg0,NumSemaine %in% input$ChxNumSemN0) %>%
      select(polegestioncode,nograp,NumSemaine,semainereference,datedebutcollecte,datefincollecte
             ,aaumoinsuncontact,questdemarreenqueteur,finaliseenqueteur,refus,horschamp,valideenqueteur
             ,encoursinternet,valideinternet,abandonPole)
    # select(c(10,20,19,4:6,11:18,21,22))
    names(Export_Donnees) <-c("Pôle Gestion","Grappes","Numéro de semaine","semaine de référence"
                              ,"Début collecte","Fin collecte","Enquêteur_Au moins 1 contact","Enquêteur_Questionnaire démarré","Enquêteur_Finalisé","Enquêteur_Refus"
                              ,"Enquêteur_Hors champ","Enquêteur_Validé","Web_En cours","Web_Validé","Pôle EEC_Abandon")
    
    Export_Donnees <- Export_Donnees %>% 
      gather("Etat","RESULTAT",-c(1:6))%>% 
      filter(RESULTAT==TRUE)%>%
      separate(Etat, c("mode","Res"),sep="_")%>% 
      select(-RESULTAT) %>%  
      arrange(`Pôle Gestion`,`Numéro de semaine`)
    names(Export_Donnees)[c(7:8)] <- c("Mode de collecte","Etat de la FA")
    
    Export_Donnees <- Export_Donnees %>% 
      filter(`Mode de collecte` %in% input$ModCollect,Grappes %in% input$ChxGrappes)
    
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
                                       "$(this.api().table().header()).css({'background-color': 'Lavender', 'color': 'Black'});","}"),
                     
                     # extensions : Buttons (colonnes à afficher ou 'désafficher')
                     buttons = list(list(extend = 'colvis', columns = c(2,4:6),text='Masquer colonnes')))) %>%
      formatStyle(c(2,4:6), backgroundColor = 'Lavender')
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
  
  output$Afficher_ChxEnqueteur <- renderUI({
    # IA3 - SELECTINPUT : FILTRE ENQUETEURS ----------------------------------------------------------------------------------------
    ListEnqueteur <- sort(unique(Donnees()[Donnees()$polegestioncode %in% input$ChxReg,"IdentifEnqueteur"]))
    
    selectInput("ChxEnq",
                h3("Sélectionnez un ou plusieurs enquêteurs"),
                choices = ListEnqueteur,
                multiple = T,
                width = "50%",
                selected = ListEnqueteur)
  })
  
  
  # IB/ DATATABLEOUTPUT : TABLEAU DE SUIVI DEM -------------------------------------------------------------------------------------------------------------------
  output$SuiviDem <- renderDataTable({
    
    if (input$TypeValeurs_DEM == 1){
      
      # Etape 1
      Stats_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      
      Stats_DEM <- Stats_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 2
      Stats_Enqu_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        group_by(polegestioncode,IdentifEnqueteur) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      # Stats_Enqu_DEM <- as.data.frame(Stats_Enqu_DEM)
      # class(Stats_Enqu_DEM)
      Stats_Enqu_DEM$nograp <- "Total Enquêteur"
      Stats_Enqu_DEM$NumSemaine <- ""
      
      Stats_Enqu_DEM <- Stats_Enqu_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                          "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                          "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 3
      
      Stats0_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats0_DEM$nograp <- "Ts Enquêteurs"
      Stats0_DEM$IdentifEnqueteur <- "Total hebdo"
      
      Stats0_DEM <- Stats0_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 4
      StatsTot_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      StatsTot_DEM$polegestioncode <- "TOTAL"
      StatsTot_DEM$IdentifEnqueteur <- ".........."
      StatsTot_DEM$nograp <- ".........."
      StatsTot_DEM$NumSemaine <- ".........."
      
      StatsTot_DEM <- StatsTot_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                      "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                      "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      # Etape 5
      Stats1_DEM <- bind_rows(Stats_DEM,Stats_Enqu_DEM) %>% 
        arrange(polegestioncode,IdentifEnqueteur,desc(NumSemaine))
      Stats1_DEM <- bind_rows(Stats1_DEM,Stats0_DEM)
      Stats1_DEM <- bind_rows(Stats1_DEM,StatsTot_DEM)

      Stats1_DEM <- Stats1_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      names(Stats1_DEM)
      
      # Etape 6
      # ------------------------------------------------------------------------------------------------------
      # Nombre initial de FA = Nb d'enregistrement
      # Total FA = Nb de questionnaires en cours. > au nb initial car certaines FA en cours de collecte enquêteur + Web
      sketch1 = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 5, ' '),
            th(colspan = 6, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX'),
            th(colspan = 1, ' ') 
            
          ),
          tr(
            lapply(c("REG","Enquêteur","Grappe","Semaine de référence","Total FA","dont aucun essai de contact","dont au moins un essai de contact"
                     ,"dont questionnaires démarrés","dont  finalisation","dont refus / hors champ","dont validés","dont en cours","dont validés"
                     ,"dont refus/hors champ","Total validé","Total Refus/Hors champ","FA non terminées","FA non entamées"), th)
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
                              pageLength = 25,
                              
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'SkyBlue', 'color': 'Black'});","}"),
                              
                              buttons = list('copy', 'print',
                                             list(extend = 'collection',
                                                  buttons = c('csv','excel', 'pdf'),
                                                  text = 'Download'),
                                             list(extend = 'colvis',
                                                  columns = c(4:5,15:17),text='Masquer colonnes')))) %>%
        
        
        
        formatStyle(c(1:5,11,13),backgroundColor = 'PowderBlue') %>% 
        formatStyle(c(15:17),backgroundColor = 'SkyBlue') %>%     
        formatStyle(18,backgroundColor = 'PowderBlue') 
      
      
    }else if(input$TypeValeurs_DEM == 2){
      # TOTFA : Nb de FA yc celles non démarrées
      # FA_EnCours : Nb de FA démarrées. Dénominateur de la ventilation %
      # Etape 1
      Stats_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      
      Stats_DEM <- Stats_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 2
      Stats_Enqu_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        group_by(polegestioncode,IdentifEnqueteur) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats_Enqu_DEM$nograp <- "Total Enquêteur"
      Stats_Enqu_DEM$NumSemaine <- ""
      
      Stats_Enqu_DEM <- Stats_Enqu_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                          "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                          "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 3
      Stats0_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats0_DEM$nograp <- "Ts Enquêteurs"
      Stats0_DEM$IdentifEnqueteur <- "Total hebdo"
      
      Stats0_DEM <- Stats0_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 4
      StatsTot_DEM <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg,NumSemaine %in% input$ChxNumSemN2,IdentifEnqueteur %in% input$ChxEnq) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      StatsTot_DEM$polegestioncode <- "TOTAL"
      StatsTot_DEM$IdentifEnqueteur <- ".........."
      StatsTot_DEM$nograp <- ".........."
      StatsTot_DEM$NumSemaine <- ".........."
      
      StatsTot_DEM <- StatsTot_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                      "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                      "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 5
      Stats1_DEM <- bind_rows(Stats_DEM,Stats_Enqu_DEM) %>% 
        arrange(polegestioncode,IdentifEnqueteur,desc(NumSemaine))
      Stats1_DEM <- bind_rows(Stats1_DEM,Stats0_DEM)
      Stats1_DEM <- bind_rows(Stats1_DEM,StatsTot_DEM)
      
      Stats1_DEM <- Stats1_DEM[,c("polegestioncode","IdentifEnqueteur","nograp","NumSemaine","Nb_FA","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      # Etape 6
      # ------------------------------------------------------------------------------------------------------
      sketch2 = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(colspan = 5, ' '),
            th(colspan = 6, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX'),
            th(colspan = 1, ' ') 
            
          ),
          tr(
            lapply(c("REG","Enquêteur","Grappe","Semaine de référence","Total FA","dont aucun essai de contact","dont au moins un essai de contact"
                     ,"dont questionnaires démarrés","dont  finalisation","dont refus / hors champ","dont validés","dont en cours","dont validés"
                     ,"dont refus/hors champ","Total validé","Total Refus/Hors champ","FA non terminées","FA non entamées"), th)
          )
        )
      ))  # ------------------------------------------------------------------------------------------------------
      
      # Création de fonctions spécifiques au tableau : boutons Copier, Imprimer et télécharger + Réorganisation des colonnes + Sélection de cellules
      datatable(Stats1_DEM,
                # Ajout de la ligne d'en tête'
                container = sketch2, rownames = F,
                
                extensions = c('Buttons','ColReorder','KeyTable','FixedHeader'),
                options =list(dom = 'Bfrtip',
                              keys = TRUE,
                              colReorder = F,
                              # extensions : FixedHeader
                              pageLength = 25,
                              
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'SkyBlue', 'color': 'Black'});","}"),
                              
                              buttons = list('copy', 'print',
                                             list(extend = 'collection',
                                                  buttons = c('csv','excel', 'pdf'),
                                                  text = 'Download'),
                                             list(extend = 'colvis',
                                                  columns = c(4:5,15:17),text='Masquer colonnes')))) %>%        
        formatStyle(c(1:5,11,13),backgroundColor = 'PowderBlue') %>% 
        formatStyle(c(15:17),backgroundColor = 'SkyBlue') %>%     
        formatStyle(18,backgroundColor = 'PowderBlue') %>% 
        
        formatStyle(c(5:18),
                    color = styleInterval(c(0.50,0.90),c("DarkRed","Black","DarkBlue")))  %>%
        
        # formatPercentage(c(6),0) %>% 
        formatPercentage(c(6:18),2)
      
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
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      
      Stats_CPS <- Stats_CPS[,c("polegestioncode","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      Stats_REG_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats_REG_CPS$NumSemaine <- c("ENSEMBLE")
      Stats_REG_CPS <- Stats_REG_CPS[,c("polegestioncode","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                        "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                        "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      colnames(Stats_REG_CPS)
      
      Stats0_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(NumSemaine) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats0_CPS$polegestioncode <- c("Total Hebdo")
      Stats0_CPS <- Stats0_CPS[,c("polegestioncode","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      StatsTot_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        summarise(Nb_FA=n(),
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non"),
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE),
                  Enq_Finalise=sum(finaliseenqueteur==TRUE),
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE)),
                  Enq_Valide=sum(valideenqueteur==TRUE),
                  Web_EnCours=sum(encoursinternet==TRUE),
                  Web_Valide=sum(valideinternet==TRUE),
                  EEC_Refus_HC=sum(abandonPole==TRUE),
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui"),
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      StatsTot_CPS$polegestioncode <- c("TOTAL")
      StatsTot_CPS$NumSemaine <- c("ENSEMBLE")
      
      StatsTot_CPS <- StatsTot_CPS[,c("polegestioncode","NumSemaine","Nb_FA","Total_FA","Enq_AucunEssai",
                                      "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                      "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      Stats1_CPS <- bind_rows(Stats_CPS,Stats_REG_CPS) %>% 
        arrange(polegestioncode,NumSemaine)
      Stats1_CPS <- bind_rows(Stats1_CPS,Stats0_CPS)
      Stats1_CPS <- bind_rows(Stats1_CPS,StatsTot_CPS)
      
      Stats1_CPS <- Stats1_CPS[,c("polegestioncode","NumSemaine","Nb_FA","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      
      # ------------------------------------------------------------------------------------------------------
      sketch1 = htmltools::withTags(table(
        class = 'display',
        thead(
          
          tr(
            th(colspan = 3, ' '),
            th(colspan = 6, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX'),
            th(colspan = 1, ' ') 
          ),
          tr(
            lapply(c("REG","Semaine de référence","Total FA","dont aucun essai de contact","dont au moins un essai de contact"
                     ,"dont questionnaires démarrés","dont  finalisation","dont refus / hors champ","dont validés","dont en cours","dont validés"
                     ,"dont refus/hors champ","Total validé","Total Refus/Hors champ","FA non terminées","FA non entamées"), th)
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
                              pageLength = 25,
                              fixedHeader = F,
                              
                              colReorder = F,
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'Bisque', 'color': 'Black'});","}"),
                              buttons = list('copy', 'print',
                                             list(extend = 'collection',
                                                  buttons = c('csv','excel', 'pdf'),
                                                  text = 'Download'),
                                             list(extend = 'colvis',
                                                  columns = c(2,12:15),text='Masquer colonnes')))) %>%        
        formatStyle(c(1:3,13:15),backgroundColor = 'Bisque') %>% 
        formatStyle(c(9,11,16),backgroundColor = 'Cornsilk') 
      
    }else if(input$TypeValeurs_CPS == 2){
      
      Stats_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode,NumSemaine) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      
      Stats_CPS <- Stats_CPS[,c("polegestioncode","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      Stats_REG_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(polegestioncode) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats_REG_CPS$NumSemaine <- "ENSEMBLE"
      Stats_REG_CPS <- Stats_REG_CPS[,c("polegestioncode","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                        "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                        "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      Stats0_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        group_by(NumSemaine) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      Stats0_CPS$polegestioncode <- "Total Hebdo"
      Stats0_CPS <- Stats0_CPS[,c("polegestioncode","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      StatsTot_CPS <- Donnees() %>%
        filter(polegestioncode %in% input$ChxReg2,NumSemaine %in% input$ChxNumSemN) %>%
        summarise(FA_EnCours=sum((Indic_ContactEtabli=="Non")+(aaumoinsuncontact==TRUE)+(questdemarreenqueteur==TRUE)+(finaliseenqueteur==TRUE)+(refus==TRUE)+(horschamp==TRUE)
                                 +(valideenqueteur==TRUE)+(encoursinternet==TRUE)+(valideinternet==TRUE)+(abandonPole==TRUE)),
                  Nb_FA=n(),
                  FA_EnCours_Percent=FA_EnCours/Nb_FA,
                  Enq_AucunEssai=sum(Indic_ContactEtabli=="Non")/Nb_FA,
                  Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE)/Nb_FA,
                  Enq_Demarre=sum(questdemarreenqueteur==TRUE)/Nb_FA,
                  Enq_Finalise=sum(finaliseenqueteur==TRUE)/Nb_FA,
                  Enq_Refus_HC=sum((refus==TRUE)+(horschamp==TRUE))/Nb_FA,
                  Enq_Valide=sum(valideenqueteur==TRUE)/Nb_FA,
                  Web_EnCours=sum(encoursinternet==TRUE)/Nb_FA,
                  Web_Valide=sum(valideinternet==TRUE)/Nb_FA,
                  EEC_Refus_HC=sum(abandonPole==TRUE)/Nb_FA,
                  Total_FA=Enq_AucunEssai+Enq_AuMoinsUn+Enq_Demarre+Enq_Finalise+Enq_Refus_HC+Enq_Valide+Web_EnCours+Web_Valide+EEC_Refus_HC,
                  Total_Valide=Enq_Valide+Web_Valide,
                  Total_Refus_HC=Enq_Refus_HC+EEC_Refus_HC,
                  FA_EnCours_ASupp=sum(Indic_FaEnCours=="Oui")/Nb_FA,
                  Total_FaNonTerminees=Total_FA-Total_Valide-Total_Refus_HC,
                  Total_NonEntamees=Total_FaNonTerminees-FA_EnCours_ASupp)
      StatsTot_CPS$polegestioncode <- "TOTAL"
      StatsTot_CPS$NumSemaine <- ""
      StatsTot_CPS <- StatsTot_CPS[,c("polegestioncode","NumSemaine","Nb_FA","FA_EnCours","Enq_AucunEssai",
                                      "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                      "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      Stats1_CPS <- bind_rows(Stats_CPS,Stats_REG_CPS) %>% 
        arrange(polegestioncode,NumSemaine)
      Stats1_CPS <- bind_rows(Stats1_CPS,Stats0_CPS)
      Stats1_CPS <- bind_rows(Stats1_CPS,StatsTot_CPS)
      
      Stats1_CPS <- Stats1_CPS[,c("polegestioncode","NumSemaine","Nb_FA","Enq_AucunEssai",
                                  "Enq_AuMoinsUn","Enq_Demarre","Enq_Finalise","Enq_Refus_HC","Enq_Valide","Web_EnCours","Web_Valide",
                                  "EEC_Refus_HC","Total_Valide","Total_Refus_HC","Total_FaNonTerminees","Total_NonEntamees")]
      
      
      # ------------------------------------------------------------------------------------------------------
      sketch2 = htmltools::withTags(table(
        # tableHeader("%"),
        class = 'display',
        thead(
          tr(
            th(colspan = 3, ' '),
            th(colspan = 6, 'COLLECTEUR : ENQUETEUR'),
            th(colspan = 2, 'COLLECTEUR : WEB'),
            th(colspan = 1, 'COLLECTEUR : POLE EEC'),
            th(colspan = 3, 'TOTAUX'),
            th(colspan = 1, ' ') 
            
          ),
          tr(
            lapply(c("REG","Semaine de référence","Total FA","dont aucun essai de contact","dont au moins un essai de contact"
                     ,"dont questionnaires démarrés","dont  finalisation","dont refus / hors champ","dont validés","dont en cours","dont validés"
                     ,"dont refus/hors champ","Total validé","Total Refus/Hors champ","FA non terminées","FA non entamées"), th)
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
                              pageLength = 25,
                              
                              initComplete = JS("function(settings, json) {",
                                                "$(this.api().table().header()).css({'background-color': 'Bisque', 'color': 'Black'});","}"),
                              buttons = list('copy', 'print',
                                             list(extend = 'collection',
                                                  buttons = c('csv','excel', 'pdf'),
                                                  text = 'Download'),
                                             list(extend = 'colvis',
                                                  columns = c(2,12:15),text='Masquer colonnes')))) %>%        
        formatStyle(c(1:3,13:15),backgroundColor = 'Bisque') %>% 
        formatStyle(c(9,11,16),backgroundColor = 'Cornsilk') %>% 
        
        formatStyle(c(3:14),color = styleInterval(c(0.50,0.90),c("DarkRed","Black","DarkBlue"))) %>%
        
        formatPercentage(c(4:16),2)
      
    }
  })
}

