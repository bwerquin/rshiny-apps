
library(shiny)
library(jsonlite)
library(dplyr)
library(httr)
library(devtools)
library(curl)
library(DT)
server <- function(input, output) {
  
  Donnees <- reactive({
    config = httr::config(ssl_verifypeer = 0L)
    set_config(config)
    Donnees <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
    Donnees <- fromJSON(content(Donnees, "text"))
    Donnees$NumSemaine <- paste0("N°",format(as.Date(Donnees$semainereference), "%U"))
    Donnees$IdentifEnqueteur <- paste0(Donnees$enqueteurnom," ",Donnees$enqueteurprenom," ",Donnees$enqueteuridep)
    
    # EN ATTENDANT D'AVOIR LES VERITABLES VARIABLES RALTIVES AU POLE EEC
    Donnees$EEC_refus <- Donnees$refus
    Donnees$EEC_horschamp <- Donnees$horschamp
    
    # LORSQUE PBM DE CONNEXION AU WEBSERVICE
    # Donnees <- readRDS("Temp/Donnees.rds")
    
    # A compléter. Attention : utilisation des noms dans Stats1_DEM ... A adpater !!!
    # names(Donnees) <- c("Numéro Grappe","nolog","nole","semaine de référence","Début collecte","Fin collecte","Nom enquêteur")
    return(Donnees)
  })
  
  output$Donnees <- renderDataTable({
    datatable(
      Donnees(), rownames = F,
      extensions = c('Buttons','ColReorder','KeyTable'), options = list(dom = 'Bfrtip',
                                                                        colReorder = TRUE,
                                                                        keys = TRUE,
                                                                        buttons = list(list(extend = 'colvis', columns = c(1:6,8:10))))
    )
    
    # LORSQUE PBM DE CONNEXION AU WEBSERVICE
    # Donnees()
  })
  
  
  output$Afficher_ChxReg <- renderUI({
    config = httr::config(ssl_verifypeer = 0L)
    set_config(config)
    Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
    Reg <- fromJSON(content(Reg, "text"))
    
    # POUR LES TESTS, EN ATTENDANT D'AVOIR PLUS DE DONNEES
    ListReg <- as.data.frame(c("Ensemble des régions","ETB de LILLE","ETB de TOULOUSE","ETB de TOULOUSE",Reg))
    # ListReg <- as.data.frame(c("Ensemble des régions",Reg))
    
    names(ListReg) <- "NomsReg"
    ListReg <- as.character(unique(ListReg$NomsReg))
    
    # LORSQUE PBM DE CONNEXION AU WEBSERVICE
    # ListReg <- readRDS("Temp/ListReg.rds")
    # ListReg
    selectInput("ChxReg",
                "Sélectionnez une ou plusieurs régions",
                choices = ListReg,
                multiple = F,
                width = "50%",
                selected = "Ensemble des régions")
  })
  
  output$SuiviDem <- renderDataTable({
    Stats1_DEM <- Donnees() %>%
      filter(polegestioncode %in% input$ChxReg || input$ChxReg =="Ensemble des régions") %>%
      group_by(IdentifEnqueteur,nograp,NumSemaine) %>%
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
    names(Stats1_DEM) <- c("Enquêteur","Grappe","Semaine de référence","Au moins un essai de contact","Questionnaire démarré","Finalisation","Refus / Hors champ","Validé","Web : en cours","web : validé","Pôle : Refus/Hors champ","Total validé","Total Refus/Hors champ","Total des FA", "Reste à traiter")
    Stats1_DEM <- Stats1_DEM[,c(1:3,14,4:13,15)]
    
    datatable(Stats1_DEM,
              extensions = 'Buttons', options =list(
                dom = 'Bfrtip',
                buttons = list('copy', 'print', list(
                  extend = 'collection',
                  buttons = c('csv', 'excel', 'pdf'),
                  text = 'Download'))
              )
    ) %>% 
      formatStyle(c(1:3,10,15), backgroundColor = 'LightBlue') 
  })
}
