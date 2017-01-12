### =========== ENDPOINTS ===========


    ### ----------- FUNCIONES DE ACCESO A ENDPOINTS -----------


## FUNCION PARA OBETENER UN LISTADO DE TWEETS
# @params: query, lang, result_type, count, since_id, max_id,auth
searchTweetsAPI <- function(query, lang="es", result_type = "recent", 
                            count = 25, since_id = "FALSE", max_id = "FALSE",
                            auth=TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/search/tweets.json?"
    url <- paste(urlAPI, "q=", query, "&lang=", lang, "&result_type=", result_type, 
                 "&count=", count, "&since_id=", since_id, "&max_id=", max_id, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    searchTweets <<- jsonlite::fromJSON(toJSON(jsonContent))

}


## FUNCION PARA BUSCAR USUARIOS
# @params:
usersSearchAPI <- function(query, page = 1, count = 20, auth = TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/users/search.json?"
    url <- paste(urlAPI, "q=", query, "&page=", page, "&count=", count, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    usersSearch <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}


## FUNCION PARA BUSCAR INFORMACION ACERCA DE VARIOS USUARIOS
# @params:
usersLookupAPI <- function(screen_name, auth = TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/users/lookup.json?"
    url <- paste(urlAPI, "screen_name=", screen_name, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    usersLookup <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}


## FUNCION PARA BUSCAR INFORMACION ACERCA DE LOS TWEETS DE VARIOS USUARIOS
# @params:
statusesUserTimelineAPI <- function(screen_name, count = 200, trim_user = "TRUE", 
                                    exclude_replies = "TRUE", contributor_details = "FALSE", 
                                    include_rts = "FALSE", auth=TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/statuses/user_timeline.json?"
    url <- paste(urlAPI, "screen_name=", screen_name, "&count=", count, 
                 "&trim_user=", trim_user, "&exclude_replies=", exclude_replies, 
                 "&contributor_details=", contributor_details, 
                 "&include_rts=", include_rts, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    statusesUserTimeline <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}


## FUNCION PARA OBTENER LA EDAD Y EL GENERO DE UNO O VARIOS USUARIOS (APPLIED)
# @params:
demographicsUsersAPI <- function(users, texts, language_iso = "spa", 
                                 apiKey = token_applied[1]){
    
    users <- cleanText(users)
    texts <- cleanText(texts)
    users <- gsub(" ", "%20", users)
    texts <- gsub(" ", "%20", texts)
    datas <- ""
    coma <- ""
    for( i in 1:length(users)){
        if(i == 2){coma <- ","}
        id <- i
        part <- paste("{%20%22text%22:%22", texts[i], "%22,%20%22language_iso%22:%22", language_iso, "%22,%20%22user%22:%22", users[i], "%22,%20%22id%22:", as.character(id),"%20}", collapse = "", sep = "")
        datas <- paste(part, datas, sep = coma, collapse = "")
    }
    urlreq <- paste("http://api.ai-applied.nl/api/demographics_api/?request={%20%22data%22:{%20%22api_key%22:%22", apiKey, "%22,%20%22call%22:{%20%22return_original%22:true,%20%22data%22:[%20", datas, "%20]%20}%20}%20}", collapse = "", sep = "")
    req <- httr::GET(urlreq)
    json <- httr::content(req, as = "text", encoding = "UTF-8")
    demographicsUsers <<- fromJSON(json)
    
}


## FUNCION PARA OBTENER LA POSITIVIDAD DE UNO O VARIOS TWEETS (APPLIED)
# @params:
sentimentTweetsAPI <- function(texts, classifier = "default", language_iso = "spa", 
                               apiKey = token_applied[1]){
    
    texts <- cleanText(texts)
    texts <- gsub(" ", "%20", texts)
    datas <- ""
    coma <- ""
    for( i in 1:length(texts)){
        if(i == 2){coma <- ","}
        id <- i
        part <- paste("{%20%22text%22:%22", texts[i], "%22,%20%22language_iso%22:%22", language_iso, "%22,%20%22id%22:", as.character(id), "%20}", collapse = "", sep = "")
        datas <- paste(part, datas, sep = coma, collapse = "")
    }
    urlreq <- paste("http://api.ai-applied.nl/api/sentiment_api/?request={%20%22data%22:{%20%22api_key%22:%22", apiKey, "%22,%20%22call%22:{%20%22return_original%22:true,%20%22classifier%22:%22", classifier, "%22,%20%22data%22:[%20", datas, "%20]%20}%20}%20}", collapse = "", sep = "")
    req <- httr::GET(urlreq)
    json <- httr::content(req, as = "text", encoding = "UTF-8")
    sentimentTweets <<- fromJSON(json)
    
}


## FUNCION PARA OBTENER LOS AMIGOS DE UN USUARIO
# @params:
friendsListAPI <- function(screen_name, cursor = -1, count = 200, 
                           skip_status = 1, include_user_entities = FALSE, 
                           auth = TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/friends/list.json?"
    url <- paste(urlAPI, "screen_name=", screen_name, "&cursor=", cursor, 
                 "&count=", count, "&skip_status=", skip_status, 
                 "&include_user_entities=", include_user_entities, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    friendsList <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}


## FUNCION PARA OBTENER LOS SEGUIDORES DE UN USUARIO
# @params:
followersListAPI <- function(screen_name, cursor = -1, count = 200, 
                           skip_status = 1, include_user_entities = FALSE, 
                           auth = TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/followers/list.json?"
    url <- paste(urlAPI, "screen_name=", screen_name, "&cursor=", cursor, 
                 "&count=", count, "&skip_status=", skip_status, 
                 "&include_user_entities=", include_user_entities, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    followersList <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}


    ### ----------- FUNCIONES EN DESHUSO -----------


## FUNCION PARA BUSCAR INFORMACION ACERCA DE UN USUARIO
# @params:
usersShowAPI <- function(screen_name, auth = TRUE){
    
    getSignOAuth(auth)
    urlAPI <- "https://api.twitter.com/1.1/users/show.json?"
    url <- paste(urlAPI, "screen_name=", screen_name, sep="")
    getUrl <- GET(url, sig)
    jsonContent <- content(getUrl)
    usersShow <<- jsonlite::fromJSON(toJSON(jsonContent))
    
}

## FUNCION PARA OBTENER LA EDAD Y EL GENERO DE UN USUARIO (APPLIED)
# @params:
demographicsUserAPI <- function(user, text, language_iso = "spa", apiKey = token_applied[1]){
    
    user <- cleanText(user)
    text <- cleanText(text)
    user <- gsub(" ", "%20", user)
    text <- gsub(" ", "%20", text)
    urlreq <- paste("http://api.ai-applied.nl/api/demographics_api/?request={%20%22data%22:{%20%22api_key%22:%22", apiKey, "%22,%20%22call%22:{%20%22return_original%22:true,%20%22data%22:[%20{%20%22text%22:%22", text, "%22,%20%22language_iso%22:%22", language_iso, "%22,%20%22user%22:%22", user, "%22,%20%22id%22:1%20}%20]%20}%20}%20}", collapse = "", sep = "")
    req <- httr::GET(urlreq)
    json <- httr::content(req, as = "text", encoding = "UTF-8")
    demographicsUser <<- fromJSON(json) 
    demographicsUser$response$data
    #sentimentTweet$response$quota$remaining_credits
    
}

## FUNCION PARA OBTENER LA POSITIVIDAD DE UN TWEET (APPLIED)
# @params:
sentimentTweetAPI <- function(text, classifier = "subjective", language_iso = "spa", apiKey = token_applied[1]){
    
    text <- cleanText(text)
    text <- gsub(" ", "%20", text)
    urlreq <- paste("http://api.ai-applied.nl/api/sentiment_api/?request={%20%22data%22:{%20%22api_key%22:%22", apiKey, "%22,%20%22call%22:{%20%22return_original%22:true,%20%22classifier%22:%22", classifier, "%22,%20%22data%22:[%20{%20%22text%22:%22", text, "%22,%20%22language_iso%22:%22", language_iso, "%22,%20%22id%22:1%20}%20]%20}%20}%20}", collapse = "", sep = "")
    req <- httr::GET(urlreq)
    json <- httr::content(req, as = "text", encoding = "UTF-8")
    sentimentTweet <<- fromJSON(json) 
    sentimentTweet$response$data
    #sentimentTweet$response$quota$remaining_credits
    
}

