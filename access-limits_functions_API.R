### =========== ACCESS AND LIMITS ===========


## FUNCION PARA OBTENER EL OAUTH SIGN
# @params: auth="indica autenticación por user o app"
getSignOAuth <- function(auth = TRUE){
        
        print("Acceso a endpoint (firma)")
        
        #Guardamos los datos de autenticación para aplicacion
        myapp <- oauth_app("twitter", key = api_key, secret = api_secret)
        
        #Creamos una firma con los datos de aplicacion y el token de acceso de usuario user application
        #Si no incluimos los token de acceso de usuario, nos autenticamos application-only
        if(auth == TRUE){
            sig <<- sign_oauth1.0(myapp, token=access_token, token_secret=access_token_secret)
        }else{
            sig <<- sign_oauth1.0(myapp)
        }

}


## FUNCION PARA OBETENER LOS LIMITES DE USER-APPLICATION
# @params: resoureces= "help,users,search,statuses"
callsInfoUserAPI <- function(resources="help,users,search,statuses"){
    
    getSignOAuth()
    urlAPI <- "https://api.twitter.com/1.1/application/rate_limit_status.json?"
    url <- paste(urlAPI, "resources=", resources, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    callsInfo <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}


## FUNCION PARA OBETENER LOS LIMITES DE APPLICATION-ONLY
# @params: resoureces= "help,users,search,statuses"
callsInfoAppAPI <- function(resources="help,users,search,statuses"){
    
    #Consumer Key and Consumer Secret
    consumer_key = "A33TVpy7zcUi2idhCMbeX4oNj";
    consumer_secret = "HD4YOkm9ZGMMNKe3CADK868Ebby6NplpiPDK6FEUDmW8CFOuRa";
    
    #Use basic only-app auth
    secret <- jsonlite::base64_enc(paste(consumer_key, consumer_secret, sep = ":"))
    req <- httr::POST("https://api.twitter.com/oauth2/token",
                      httr::add_headers(
                          "Authorization" = paste("Basic", gsub("\n", "", secret)),
                          "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"
                      ),
                      body = "grant_type=client_credentials"
    );
    
    #Extract the access token
    httr::stop_for_status(req, "authenticate with twitter")
    token <- paste("Bearer", httr::content(req)$access_token)
    
    #help,users,search,statuses
    urlSearch <- "https://api.twitter.com/1.1/application/rate_limit_status.json?resources="
    url <- paste(urlSearch, resources, sep="")
    req <- httr::GET(url, httr::add_headers(Authorization = token))
    json <- httr::content(req, as = "text")
    callsInfoApp <<- fromJSON(json) 
    
}

