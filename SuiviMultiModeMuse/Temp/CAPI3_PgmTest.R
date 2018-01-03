# Récupération et sauvegarde des données
Donnees <- fromJSON(content(t, "text"))
# création de la variable NumSemaine en convertissant la date de référence en numéro de semaine
Donnees$NumSemaine <- paste0("N°",format(as.Date(Donnees$semainereference), "%U"))
# Transformer une date en numéro de semaine
# format(as.Date("2017-01-07"), "%U") #S01
# format(as.Date("2017-01-08"), "%U") #S02
# Peut-on considérer que la S1 débute le 1er jour de l'année ? vraisemblablement Oui

# Quelles données inscrire dans la case enquêteur ? Id, Nom, Prénom ?
Donnees$IdentifEnqueteur <- paste0(Donnees$enqueteurnom," ",Donnees$enqueteurprenom," ",Donnees$enqueteuridep)

# Pas de données pour le pôle EEC. 
Donnees$EEC_refus <- Donnees$refus
Donnees$EEC_horschamp <- Donnees$horschamp

saveRDS(Donnees,"U:/UIA/CAPI3G_Suivi/rshiny-apps/SuiviMultiModeMuse/Temp/Donnees.rds")
colnames(Donnees)

# _____________________________________________________________________________________________________________________________

# Récupération et sauvegarde des noms de région
config = httr::config(ssl_verifypeer = 0L)
set_config(config)
Reg <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/etab", use_proxy(url = ""), verbose() )
Reg <- fromJSON(content(Reg, "text"))

# ajout d'enregistrements multiples, pour l'exercice
ListReg <- as.data.frame(c("Ensemble des régions","ETB de LILLE","ETB de TOULOUSE","ETB de TOULOUSE",Reg))
names(ListReg) <- "NomsReg"
ListReg <- as.character(unique(ListReg$NomsReg))

saveRDS(ListReg,"U:/UIA/CAPI3G_Suivi/rshiny-apps/SuiviMultiModeMuse/Temp/ListReg.rds")
colnames(ListReg)

# _____________________________________________________________________________________________________________________________

# Suivi DEM


Stats1_DEM <- Donnees %>%
  filter(polegestioncode == "ETB de LILLE" || polegestioncode =="Ensemble") %>%
  group_by(IdentifEnqueteur,nograp,NumSemaine) %>%
  summarise(
            Enq_AuMoinsUn=sum(aaumoinsuncontact==TRUE),
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

# Pour mise en forme du tableau : code à rectifier
# sketch2 = htmltools::withTags(table(
#   tableHeader("Enquête xxx - Tableau 1 - suivi de l'avancement de la collecte selon l'état des FA"),
#   tableFooter("CAPI3G-Suivi DEM"),
#   class = 'display',
#   thead(
#     tr(
#       th(colspan = 3, 'Identification'),
#       th(colspan = 4, 'ENQUETEUR'),
#       th(colspan = 2, 'WEB'),
#       th(colspan = 2, 'Pôle EEC'),
#       th(colspan = 3, 'Totaux')
#     ),
#     tr(
#       lapply(c("Enquêteur","Grappe","Semaine de référence","Total des FA","Au moins un essai de contact","Questionnaire démarré","Finalisation","Refus / Hors champ","Validé","Web : en cours","web : validé","Pôle : Refus/Hors champ","Total validé","Total Refus/Hors champ", "Reste à traiter"), th)
#     )
#   )
# ))
# 
# datatable(Stats1_DEM,container = sketch2, rownames = FALSE)
