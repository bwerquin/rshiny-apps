library(httr)
library(devtools)
library(jsonlite)
library(curl)

# Exemple par défaut : DNS ok, HTTPS ok
data <- fromJSON("https://api.github.com/users/hadley/repos", flatten = TRUE)

# En interne, ça se complique : 

# Étape 1 : se passer du proxy de sortie vers Internet RIE
proxy <- ""
verbose <-  TRUE
h <- new_handle(verbose = verbose, proxy = proxy)
con <- curl("http://dvrmspogbolht01.ad.insee.intra/ddi-access-services/api/search/series", handle = h)
data <- fromJSON(con)


# Étape 2 : Pour pouvoir passer la contrainte des certificats non reconnus par défaut
config = httr::config(ssl_verifypeer = 0L)
set_config(config)
t <- httr::GET( "https://qfloccapi3lht01.ad.insee.intra/loccapi3g/rest/multimode/suivi", use_proxy(url = ""), verbose() )
data <- fromJSON(content(t, "text"))
