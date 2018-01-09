# Récupération et sauvegarde des données
config = httr::config(ssl_verifypeer = 0L)
set_config(config)
Donnees <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
Donnees <- fromJSON(content(Donnees, "text"))

# création de la variable NumSemaine en convertissant la date de référence en numéro de semaine
Donnees$NumSemaine <- paste0("S.",format(as.Date(Donnees$semainereference), "%U"))
# Transformer une date en numéro de semaine
# format(as.Date("2017-01-07"), "%U") #S01
# format(as.Date("2017-01-08"), "%U") #S02
# Peut-on considérer que la S1 débute le 1er jour de l'année ? vraisemblablement Oui


Donnees$enqueteurnom <- "DELIL"
Donnees$enqueteurprenom <- "Lydéric"
# Quelles données inscrire dans la case enquêteur ? Id, Nom, Prénom ?
Donnees$IdentifEnqueteur <- paste0(Donnees$enqueteurnom," ",Donnees$enqueteurprenom)

# Pas de données pour le pôle EEC. 
Donnees$EEC_refus <- FALSE
Donnees$EEC_horschamp <- FALSE

# saveRDS(Donnees,"U:/UIA/CAPI3G_Suivi/rshiny-apps/SuiviMultiModeMuse/Temp/Donnees.rds")
colnames(Donnees)

# _____________________________________________________________________________________________________________________________

# Pour avoir un jeu de données un peu plus conséquent
# Donnees <- readRDS("Temp/Donnees.rds")

DonneesTlse <- Donnees
DonneesTlse$enqueteurnom <- "STOCHA"
DonneesTlse$enqueteurprenom <- "Harry"
DonneesTlse$enqueteuridep <- "P204VUL"
DonneesTlse$polegestioncode <- "ETB de TOULOUSE"

DonneesTlse$IdentifEnqueteur <- paste0(DonneesTlse$enqueteurnom," ",DonneesTlse$enqueteurprenom)
DonneesTlse$valideinternet <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE)

Donnees2 <- bind_rows(Donnees,DonneesTlse)
# saveRDS(Donnees2,"Temp/Donnees2.rds")

# Donnees2 <- readRDS("Temp/Donnees2.rds")
DonneesAmiens <- Donnees2
DonneesAmiens$enqueteurnom <- "LAFLEUR"
DonneesAmiens$enqueteurprenom <- "Damien"
DonneesAmiens$enqueteuridep <- "D80R22"
DonneesAmiens$polegestioncode <- "ETB de AMIENS"

DonneesAmiens$IdentifEnqueteur <- paste0(DonneesAmiens$enqueteurnom," ",DonneesAmiens$enqueteurprenom)
DonneesAmiens$questdemarreenqueteur <- c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE)
DonneesAmiens$valideenqueteur <- c(TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE)

Donnees3 <- bind_rows(Donnees2,DonneesAmiens)
saveRDS(Donnees3,"Temp/Donnees4.rds")
# _____________________________________________________________________________________________________________________________

# Récupération et sauvegarde des noms de région
config = httr::config(ssl_verifypeer = 0L)
set_config(config)
Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
Reg <- fromJSON(content(Reg, "text"))

# ajout d'enregistrements multiples, pour l'exercice
# ListReg <- as.data.frame(c("ETB de LILLE","ETB de TOULOUSE","ETB de TOULOUSE","ETB de AMIENS",Reg))
ListReg <- c("ETB de LILLE","ETB de TOULOUSE","ETB de TOULOUSE","ETB de AMIENS",Reg)
names(ListReg) <- "NomsReg"
# ListReg <- as.character(unique(ListReg$NomsReg))
ListReg <- as.character(unique(ListReg))

saveRDS(ListReg,"U:/UIA/CAPI3G_Suivi/rshiny-apps/SuiviMultiModeMuse/Temp/ListReg.rds")
colnames(ListReg)

# _____________________________________________________________________________________________________________________________
# Suivi DEM
Stats0_DEM <- Donnees %>%
  filter(polegestioncode %in% "ETB de LILLE" ) %>%
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
Stats0_DEM$nograp <- c(" ")
Stats0_DEM$IdentifEnqueteur <- c("TOTAUX")
Stats0_DEM <- Stats0_DEM[,c(1,16,15,2,13,3:12,14)]

Stats_DEM <- Donnees %>%
  filter(polegestioncode %in% "ETB de LILLE" ) %>%
  group_by(polegestioncode,IdentifEnqueteur,nograp,NumSemaine) %>%
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
      th(colspan = 2, 'COLLECTEUR : POLE EEC'),
      th(colspan = 2, 'TOTAUX') 
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
          
          extensions = c('Buttons','KeyTable','ColReorder'),
          options =list(dom = 'Bfrtip',
                        keys = TRUE,
                        colReorder = F,
                        initComplete = JS("function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': 'PowderBlue', 'color': 'Black'});","}"),
                        buttons = list('copy', 'print',list(extend = 'collection',
                                                            buttons = c('csv', 'excel', 'pdf'),
                                                            text = 'Download')))) %>% 
  
  formatStyle(c(1:5,10,12,14,16),backgroundColor = 'PowderBlue')
