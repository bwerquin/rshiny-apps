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

Donnees$EEC_refus <- FALSE
Donnees$EEC_horschamp <- FALSE

Donnees$aaumoinsuncontact <- FALSE
Donnees$questdemarreenqueteur <- FALSE
Donnees$finaliseenqueteur <- FALSE
Donnees$refus <- FALSE
Donnees$horschamp <- FALSE
Donnees$valideenqueteur <- FALSE
Donnees$encoursinternet <- FALSE
Donnees$valideinternet <- FALSE

Donnees <- bind_rows(Donnees,Donnees)
Donnees <- bind_rows(Donnees,Donnees)
Donnees <- bind_rows(Donnees,Donnees)

Donnees[1:5,"aaumoinsuncontact"] <- TRUE
Donnees[6:14,"questdemarreenqueteur"] <- TRUE
Donnees[15:17,"finaliseenqueteur"] <- TRUE
Donnees[18:20,"refus"] <- TRUE
Donnees[21:24,"horschamp"] <- TRUE
Donnees[25:30,"valideenqueteur"] <- TRUE
Donnees[31:34,"encoursinternet"] <- TRUE
Donnees[35:40,"valideinternet"] <- TRUE
Donnees[41:43,"EEC_refus"] <- TRUE
Donnees[44:48,"EEC_horschamp"] <- TRUE

# saveRDS(Donnees,"U:/UIA/CAPI3G_Suivi/rshiny-apps/SuiviMultiModeMuse/Temp/Donnees.rds")
colnames(Donnees)

# _____________________________________________________________________________________________________________________________

# Pour avoir un jeu de données un peu plus conséquent
# Donnees <- readRDS("Temp/Donnees.rds")

DonneesTlse <- Donnees[c(1:3,6:9,16,19,22,23,26:28,32:34,36:39,42,45:48),]
DonneesTlse$enqueteurnom <- "STOCHA"
DonneesTlse$enqueteurprenom <- "Harry"
DonneesTlse$enqueteuridep <- "P204VUL"
DonneesTlse$polegestioncode <- "ETB de TOULOUSE"
DonneesTlse$IdentifEnqueteur <- paste0(DonneesTlse$enqueteurnom," ",DonneesTlse$enqueteurprenom)
Donnees <- bind_rows(Donnees,DonneesTlse)
# saveRDS(Donnees2,"Temp/Donnees2.rds")

# Donnees2 <- readRDS("Temp/Donnees2.rds")
DonneesAmiens <- Donnees[c(1:2,8:9,16,19,22,23,26:27,32:33,36:38,42,45:47,50:53,57:60,64,68,69,70,74),]
DonneesAmiens$enqueteurnom <- "LAFLEUR"
DonneesAmiens$enqueteurprenom <- "Damien"
DonneesAmiens$enqueteuridep <- "D80R22"
DonneesAmiens$polegestioncode <- "ETB de AMIENS"
DonneesAmiens$IdentifEnqueteur <- paste0(DonneesAmiens$enqueteurnom," ",DonneesAmiens$enqueteurprenom)

Donnees <- bind_rows(Donnees,DonneesAmiens)
saveRDS(Donnees,"Temp/Donnees5.rds")
