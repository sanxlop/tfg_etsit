### =========== GENERATE DATASET ===========


    ### ----------- FUNCIONES PARA OBTENER TWEETS Y USUARIOS -----------


## FUNCION PARA CREAR UN DATASET DE TWEETS
# @params: query, lang, result_type, count, since_id, max_id, auth
storeTweets <- function(query, lang="es", result_type = "recent", 
                        count = 100, since_id = "FALSE", max_id = "FALSE",
                        auth = TRUE){
    
    #Obtenemos los datos en JSON mediante la función y lo guardamos en la variable
    dataAll <- searchTweetsAPI(query = query, lang = lang, result_type = result_type, 
                                count = count, since_id = since_id, max_id = max_id, auth = auth)
    
    #Obtenemos todos los datos de interes
    idT <- c(unlist(dataAll$statuses$id))
    created_atT <- c(unlist(dataAll$statuses$created_at))
    rt_from_screen_name <- oneStringPerUser(dataAll$statuses$retweeted_status$user$screen_name, type = "RT")
    retweet_countT <- c(unlist(dataAll$statuses$retweet_count))
    favorite_countT <- c(unlist(dataAll$statuses$favorite_count))
    textT <- c(unlist(dataAll$statuses$text))
    langT <- c(unlist(dataAll$statuses$lang))
    hashtagsT <- oneStringPerUser(dataAll$statuses$entities$hashtags, type = "#") #%23
    nHashtagsT <- numberOfThings(dataAll$statuses$entities$hashtags, type = "#")
    user_mentionsT <- oneStringPerUser(dataAll$statuses$entities$user_mentions, type = "@") #%40
    nUser_mentionsT <- numberOfThings(dataAll$statuses$entities$user_mentions, type = "@")
    #symbolsT <- oneStringPerUser(dataAll$statuses$entities$symbols, type = "$") #%24
    urlsT <- oneStringPerUser(dataAll$statuses$entities$urls, type = "url")
    mediaUrlT <- oneStringPerUser(dataAll$statuses$entities$media, type = "media_url")
    #in_reply_to_screen_nameT <- oneStringPerUser(dataAll$statuses$in_reply_to_screen_name, type = "resp")
    sourceT <- sourceDetect(c(unlist(dataAll$statuses$source)))
    result_typeT <- c(unlist(dataAll$statuses$metadata$result_type))
    user_nameT <- c(unlist(dataAll$statuses$user$name))
    user_screen_nameT <- c(unlist(dataAll$statuses$user$screen_name))
    
    #Los almacenamos en forma de dataframe    
    store_tweets <- data.frame(idT, created_atT, rt_from_screen_name, retweet_countT,
                                 favorite_countT, textT, langT, hashtagsT, nHashtagsT,
                                 user_mentionsT, nUser_mentionsT, urlsT, mediaUrlT,
                                 sourceT, result_typeT, user_nameT, user_screen_nameT)
    
}


## FUNCION PARA CREAR UN DATASET DE UN USUARIO DESDE TWEETS
# @params: screen_name, auth
storeUsersFromTweets <- function(screen_name, auth = TRUE){
    
    #Obtenemos los datos en JSON mediante la función y lo guardamos en la variable
    dataAll2 <- usersLookupAPI(paste(screen_name, collapse = ","), auth)
    
    #Obtenemos todos los datos de interes de cada usuario
    idU <- c(unlist(dataAll2$id))
    created_atU <- c(unlist(dataAll2$created_at))
    user_nameU <- c(unlist(dataAll2$name))
    user_screen_nameU <- c(unlist(dataAll2$screen_name))
    followers_countU <- c(unlist(dataAll2$followers_count))
    friends_countU <- c(unlist(dataAll2$friends_count))
    listed_countU <- c(unlist(dataAll2$listed_count))
    locationU <- c(unlist(dataAll2$location)) #time_zone, es mas preciso el timezone
    descriptionU <- c(unlist(dataAll2$description))
    favourites_countU <- c(unlist(dataAll2$favourites_count))
    verifiedU <- c(unlist(dataAll2$verified))
    statuses_countU <- c(unlist(dataAll2$statuses_count))
    langU <- c(unlist(dataAll2$lang))
    obtainedU <- vector(mode = "character", length = length(dataAll2$id))
        for(i in 1:length(obtainedU)){
            obtainedU[i] <- "from tweets"
        }
    
    #Los almacenamos en forma de dataframe    
    store_users_tweets <- data.frame(idU, created_atU, user_nameU, user_screen_nameU, 
                              followers_countU, friends_countU, listed_countU,
                              locationU, descriptionU, favourites_countU, verifiedU,
                              statuses_countU, langU, obtainedU)
}


## FUNCION PARA CREAR UN DATASET DE UN USUARIO DESDE DUEÑOS DE RT DE TWEETS
# @params: screen_name, auth
storeUsersFromOwnerRt <- function(screen_name, auth = TRUE){
    #Quitamos los valores de usuario que se llamen "None"
    screen_name <- screen_name[screen_name != "None"]
    #Obtenemos los datos en JSON mediante la función y lo guardamos en la variable
    dataAll2 <- usersLookupAPI(paste(screen_name, collapse = ","), auth)
    
    #Obtenemos todos los datos de interes de cada usuario
    idU <- c(unlist(dataAll2$id))
    created_atU <- c(unlist(dataAll2$created_at))
    user_nameU <- c(unlist(dataAll2$name))
    user_screen_nameU <- c(unlist(dataAll2$screen_name))
    followers_countU <- c(unlist(dataAll2$followers_count))
    friends_countU <- c(unlist(dataAll2$friends_count))
    listed_countU <- c(unlist(dataAll2$listed_count))
    locationU <- c(unlist(dataAll2$location)) #time_zone, es mas preciso el timezone
    descriptionU <- c(unlist(dataAll2$description))
    favourites_countU <- c(unlist(dataAll2$favourites_count))
    verifiedU <- c(unlist(dataAll2$verified))
    statuses_countU <- c(unlist(dataAll2$statuses_count))
    langU <- c(unlist(dataAll2$lang))
    obtainedU <- vector(mode = "character", length = length(dataAll2$id))
    for(i in 1:length(obtainedU)){
        obtainedU[i] <- "from owner rt"
    }
    
    #Los almacenamos en forma de dataframe    
    store_users_words <- data.frame(idU, created_atU, user_nameU, user_screen_nameU,
                                    followers_countU, friends_countU, listed_countU,
                                    locationU, descriptionU, favourites_countU, verifiedU,
                                    statuses_countU, langU, obtainedU)
}


## FUNCION PARA CREAR UN DATASET DE UN USUARIO DESDE PALABRAS
# @params: word, auth
storeUsersFromWords <- function(word, auth = TRUE){
    
    #Obtenemos los datos en JSON mediante la función y lo guardamos en la variable
    dataAll2 <- usersSearchAPI(query = word, auth)
    
    #Obtenemos todos los datos de interes de cada usuario
    idU <- c(unlist(dataAll2$id))
    created_atU <- c(unlist(dataAll2$created_at))
    user_nameU <- c(unlist(dataAll2$name))
    user_screen_nameU <- c(unlist(dataAll2$screen_name))
    followers_countU <- c(unlist(dataAll2$followers_count))
    friends_countU <- c(unlist(dataAll2$friends_count))
    listed_countU <- c(unlist(dataAll2$listed_count))
    locationU <- c(unlist(dataAll2$location)) #time_zone, es mas preciso el timezone
    descriptionU <- c(unlist(dataAll2$description))
    favourites_countU <- c(unlist(dataAll2$favourites_count))
    verifiedU <- c(unlist(dataAll2$verified))
    statuses_countU <- c(unlist(dataAll2$statuses_count))
    langU <- c(unlist(dataAll2$lang))
    obtainedU <- vector(mode = "character", length = length(dataAll2$id))
    for(i in 1:length(obtainedU)){
        obtainedU[i] <- "from key words"
    }
    
    #Los almacenamos en forma de dataframe    
    store_users_words <- data.frame(idU, created_atU, user_nameU, user_screen_nameU,
                                    followers_countU, friends_countU, listed_countU,
                                    locationU, descriptionU, favourites_countU, verifiedU,
                                    statuses_countU, langU, obtainedU)
}


## FUNCION QUE GENERA UN LISTADO DE TWEETS Y USUARIOS
# @params: query, lang, result_type, count, since_id, repeatTimes, first, error, new
repeatSearch <- function(query, lang="es", result_type = "recent", count = 100, 
                         since_id = FALSE, repeatTimes = 3, first, error = FALSE, new = TRUE){
    
    if(first){
        if(new){
            stored_tweets <<- data.frame()
            stored_users <<- data.frame()
        }
        max_id <- FALSE
        lastAdd <- 0
        
        #Obtención de usuarios por palabras clave
        vectorQuery <<- strsplit(query, split="%20OR%20")[[1]]
        print("Obtención de usuarios a partir de palabras clave")
        for(i in 1:length(vectorQuery)){
            message <- sprintf("%d/%d", i, length(vectorQuery))
            print(message)
            #Obtencion de usuarios a partir de palabras clave
            called_users_words <- storeUsersFromWords(vectorQuery[i])
            stored_users <<- rbind(stored_users, called_users_words)
        }
    }
    else if (error){
            #max_id <- stored_tweets$idT[[length(stored_tweets$idT)-1]]
        max_id <- min(stored_tweets$idT)-100000000000 #00000000000
    } 
    else{
            #max_id <- stored_tweets$idT[[length(stored_tweets$idT)]]
        max_id <- min(stored_tweets$idT)
        lastAdd <- length(stored_users[[1]])
    }
    
    print("Obtención de usuarios a partir de tweets y dueños de rt")
    for(i in 1:repeatTimes){
        message <- sprintf("%d/%d --> %e", i, repeatTimes, max_id)
        print(message)
        
        #Recogemos los Tweets
        called_tweets <- storeTweets(query, lang, result_type, count, since_id = since_id, max_id = max_id, auth = TRUE)
            #max_id <- called_tweets$idT[[length(called_tweets$idT)]]
        max_id <- min(called_tweets$idT)
        stored_tweets <<- rbind(stored_tweets, called_tweets)
        
        #Obtención de usuarios a partir de los tweets
        called_users_tweet <- storeUsersFromTweets(screen_name = called_tweets$user_screen_nameT, auth = TRUE)
        #Obtención de usuarios a partir de los usuarios de los rt de los tweets
        called_users_rt <- storeUsersFromOwnerRt(screen_name = called_tweets$rt_from_screen_name, auth = FALSE)
        
        #Almacenamos las dos formas de obetener usuarios
        stored_users <<- rbind(stored_users, called_users_tweet, called_users_rt)
        
    }
    
}


## FUNCION QUE UNE LAS PUNTUACIONES DE LOS 3 EXPERIMENTOS
# @params: generalData, data1, data2, data3
puntuationTableAll <- function(generalData = followers_users, data1 = experiment1, data2 = experiment2, data3 = experiment3){
    
    #Guardamos el experimento 3 en data
    data <- data3
    data_result <- data.frame()
    
    #Cogemos los datos de cada experimento y los unimos en una tabla
    for(i in 1:length(data$user_screen_nameP)){ #
        
        tableGeneral <- generalData[generalData$user_screen_nameU==data$user_screen_nameP[i], ]
        table1 <- data1[data1$user_screen_nameP==as.character(data$user_screen_nameP[i]), ]
        table2 <- data2[data2$user_screen_nameP==as.character(data$user_screen_nameP[i]), ]
        table3 <- data3[data3$user_screen_nameP==as.character(data$user_screen_nameP[i]), ]
        
        confidence1 <- table1$fact_confianza
        confidence2 <- table2$fact_confianza
        confidence3 <- table3$fact_confianza
        
        influence1 <- table1$fact_capacidad_influencia
        influence2 <- table2$fact_capacidad_influencia
        influence3 <- table3$fact_capacidad_influencia
        
        relevance1 <- table1$fact_relevancia_campana
        relevance2 <- table2$fact_relevancia_campana
        relevance3 <- table3$fact_relevancia_campana
        
        user_nameP <- tableGeneral$user_nameU
        user_screen_nameP <- table3$user_screen_nameP
        total_confianza <- (0.4*confidence1 + 0.4*confidence2 + 0.2*confidence3)
        total_capacidad_influencia <- (0.45*influence1 + 0.45*influence2 + 0.1*influence3)
        total_relevancia_campana <- (0.4*relevance1 + 0.4*relevance2 + 0.2*relevance3)
        total_puntuacion <- (total_confianza + total_capacidad_influencia + total_relevancia_campana)/3
        
        print(i)
        
        data_result_temp <- data.frame(user_screen_nameP, user_nameP, total_confianza, total_capacidad_influencia, 
                                       total_relevancia_campana, total_puntuacion)
        data_result <- rbind(data_result, data_result_temp)
        
    }
    #Ordenacion
    data_result <- data_result[order(data_result$total_puntuacion, decreasing = TRUE), ]
    #Representacion
    toRepresent <- 1
    representUserPuntuation(confidence = data_result$total_confianza[toRepresent], 
                            influence = data_result$total_capacidad_influencia[toRepresent], 
                            relevance = data_result$total_relevancia_campana[toRepresent], 
                            user = data_result$user_screen_nameP[toRepresent])
    data_result
    
}


    ### ----------- FUNCIONES PARA OBTENER INFORMACION DE LOS TWEETS DE CADA USUARIO -----------


## FUNCION PARA AÑADIR A UN DATASET DE UN USUARIO COLUMNAS VACIAS DE ESTADISTICOS DE TWEETS DE USUARIOS
# @params: data_users, firstAdd, lastAdd
statisticUsers <- function(data_users, firstAdd = 1, 
                           lastAdd = length(data_users$id)){
    
    #Datos obtenidos de los statuses de cada usuario:
    length_vector_normal <- vector(length = length(data_users[firstAdd:lastAdd, 1]))
    length_vector_numeric <- vector(mode = "numeric", length(data_users[firstAdd:lastAdd, 1]))
    tweets_analyzed <- length_vector_normal
    #Medias, Desviacion tipica, Cuantiles, Varianza, Moda
    meanRt <- length_vector_numeric
    sdRt <- length_vector_numeric
    quantileRt <- length_vector_normal
    varRt <- length_vector_numeric
    modaRt <- length_vector_numeric
    
    meanFav <- length_vector_numeric
    sdFav <- length_vector_numeric
    quantileFav <- length_vector_normal
    varFav <- length_vector_numeric
    modaFav <- length_vector_numeric
    
    meanHashtags <- length_vector_numeric
    sdHashtags <- length_vector_numeric
    quantileHashtags <- length_vector_normal
    varHashtags <- length_vector_numeric
    modaHashtags <- length_vector_numeric
    
    meanMentions <- length_vector_numeric
    sdMentions <- length_vector_numeric
    quantileMentions <- length_vector_normal
    varMentions <- length_vector_numeric
    modaMentions <- length_vector_numeric
    #Otros
    commonSource <- length_vector_normal
    meanKeyWords <- length_vector_numeric
    meanKeyWordsEarly <- length_vector_numeric
    meanKeyWordsFinances <- length_vector_numeric

    
    #Los almacenamos en forma de dataframe    
    statistic_table_users <- cbind(data_users[firstAdd:lastAdd,], tweets_analyzed, meanRt, sdRt, 
                                   quantileRt, varRt, modaRt, meanFav, sdFav, quantileFav, 
                                   varFav, modaFav, meanHashtags, sdHashtags, quantileHashtags, 
                                   varHashtags, modaHashtags, meanMentions, sdMentions,
                                   quantileMentions, varMentions, modaMentions, meanKeyWords, meanKeyWordsEarly,
                                   meanKeyWordsFinances, commonSource)
    
}


## FUNCION PARA CALCULAR LAS ESTADÍSTICAS
# @params: data, firstAdd, lastAdd, auth
addStatistic <- function(data, firstAdd = 1, lastAdd = length(data_users$id), auth = TRUE) {
    
    #Máximo de 900 llamadas cada 15 min
    dataAll4 <- data
    
    for( i in firstAdd:lastAdd ){
        message <- sprintf("%d/%d", i, lastAdd)
        print(message)
        #Obtenemos los tweets publicados de un usuario y la demografia
        dataAll3 <- statusesUserTimelineAPI(data$user_screen_nameU[i], auth = auth)
        if (length(dataAll3) != 0){
            #Agregamos los datos al dataset de cada usuario
            dataAll4$tweets_analyzed[i] <- length(c(unlist(dataAll3$retweet_count)))
            
            dataAll4$meanRt[i] <- mean(c(unlist(dataAll3$retweet_count)))
            dataAll4$sdRt[i] <- sd(c(unlist(dataAll3$retweet_count)))
            dataAll4$quantileRt[i] <- paste(quantile(c(unlist(dataAll3$retweet_count))), collapse = ", ")
            dataAll4$varRt[i] <- var(c(unlist(dataAll3$retweet_count)))
            dataAll4$modaRt[i] <- mfv(c(unlist(dataAll3$retweet_count)))[1]
            
            dataAll4$meanFav[i] <- mean(c(unlist(dataAll3$favorite_count)))
            dataAll4$sdFav[i] <- sd(c(unlist(dataAll3$favorite_count)))
            dataAll4$quantileFav[i] <- paste(quantile(c(unlist(dataAll3$favorite_count))), collapse = ", ")
            dataAll4$varFav[i] <- var(c(unlist(dataAll3$favorite_count)))
            dataAll4$modaFav[i] <- mfv(c(unlist(dataAll3$favorite_count)))[1]
            
            dataAll4$meanHashtags[i] <- mean(numberOfThings(List = dataAll3$entities$hashtags, type = "#"))
            dataAll4$sdHashtags[i] <- sd(numberOfThings(List = dataAll3$entities$hashtags, type = "#"))
            dataAll4$quantileHashtags[i] <- paste(quantile(numberOfThings(List = dataAll3$entities$hashtags, type = "#")), collapse = ", ")
            dataAll4$varHashtags[i] <- var(numberOfThings(List = dataAll3$entities$hashtags, type = "#"))
            dataAll4$modaHashtags[i] <- mfv(numberOfThings(List = dataAll3$entities$hashtags, type = "#"))[1]
            
            dataAll4$meanMentions[i] <- mean(numberOfThings(List = dataAll3$entities$user_mentions, type = "@"))
            dataAll4$sdMentions[i] <- sd(numberOfThings(List = dataAll3$entities$user_mentions, type = "@"))
            dataAll4$quantileMentions[i] <- paste(quantile(numberOfThings(List = dataAll3$entities$user_mentions, type = "@")), collapse = ", ")
            dataAll4$varMentions[i] <- var(numberOfThings(List = dataAll3$entities$user_mentions, type = "@"))
            dataAll4$modaMentions[i] <- mfv(numberOfThings(List = dataAll3$entities$user_mentions, type = "@"))[1]
            
            dataAll4$meanKeyWords[i] <- meanWords(text = dataAll3$text, words = keyWordsVec)
            dataAll4$meanKeyWordsEarly[i] <- meanWords(text = dataAll3$text, words = keyWordsEarly)
            dataAll4$meanKeyWordsFinances[i] <- meanWords(text = dataAll3$text, words = keyWordsFinances)
            dataAll4$commonSource[i] <- names(sort(table(data.frame(sourceDetect(c(unlist(dataAll3$source))))), decreasing = TRUE))[1]
        }
        
    }
    
    dataAll4
    
}


    ### ----------- FUNCIONES PARA OBTENER INFORMACION DE LOS SEGUIDORES -----------


## FUNCION PARA AÑADIR A UN DATASET DE UN USUARIO COLUMNAS INFORMACION DE FOLLOWERS
# @params: data_users, auth, firstAdd, lastAdd
followersInfoUsers <- function(data_users, auth = TRUE, firstAdd = 1, 
                           lastAdd = length(data_users$id)){
    
    #Datos obtenidos de los statuses de cada usuario:
    length_vector_normal <- vector(length = length(data_users$id[firstAdd:lastAdd]))
    length_vector_numeric <- vector(mode = "numeric", length = length(data_users$id[firstAdd:lastAdd]))
    followers_analyzed <- length_vector_normal
    #Medias, Desviacion tipica, Cuantiles, Varianza, Moda
    meanTweetsF <- length_vector_numeric
    sdTweetsF <- length_vector_numeric
    quantileTweetsF <- length_vector_normal
    varTweetsF <- length_vector_numeric
    modaTweetsF <- length_vector_numeric
    
    meanFollowersF <- length_vector_numeric
    sdFollowersF <- length_vector_numeric
    quantileFollowersF <- length_vector_normal
    varFollowersF <- length_vector_numeric
    modaFollowersF <- length_vector_numeric
    
    meanFriendsF <- length_vector_numeric
    sdFriendsF <- length_vector_numeric
    quantileFriendsF <- length_vector_normal
    varFriendsF <- length_vector_numeric
    modaFriendsF <- length_vector_numeric
    #Otros
    meanLangF <- length_vector_normal
    meanDescriptionKeysF <- length_vector_numeric
    #Genero y Edad
    gender <- length_vector_normal
    genderConfidence <- length_vector_numeric
    age <- length_vector_normal
    ageConfidence <- length_vector_numeric

    
    #Los almacenamos en forma de dataframe    
    followers_table_users <- cbind(data_users, followers_analyzed,
                                   meanTweetsF, sdTweetsF, quantileTweetsF, varTweetsF,
                                   modaTweetsF, meanFollowersF, sdFollowersF, quantileFollowersF,
                                   varFollowersF, modaFollowersF, meanFriendsF, sdFriendsF,
                                   quantileFriendsF, varFriendsF, modaFriendsF, meanLangF, meanDescriptionKeysF, 
                                   gender, genderConfidence, age, ageConfidence)
    
    followers_table_users
    
}


## FUNCION PARA CALCULAR LAS ESTADÍSTICAS DE LOS FOLLOWERS DE CADA USUARIO SOLO 15 PETICIONES
# @params: data, firstAdd, lastAdd, auth
addFollowersInfo <- function(data, firstAdd = 1, lastAdd = length(data_users$id), auth = TRUE) {
    
    library(modeest)
    #Máximo de 900 llamadas cada 15 min
    dataAll8 <- data
    
    for( i in firstAdd:lastAdd ){
        message <- sprintf("%d/%d", i, lastAdd)
        print(message)
        #Obtenemos los seguidores de cada usuario
        dataAll7 <- followersListAPI(data$user_screen_nameU[i], auth = auth)
        dataAll5 <- demographicsUsersAPI(users = data$user_nameU[i], texts = data$descriptionU[i])
        if (length(dataAll7) != 0){
            #Agregamos los datos al dataset de cada usuario
            dataAll8$followers_analyzed[i] <- length(c(unlist(dataAll7$users$id)))
            
            dataAll8$meanTweetsF[i] <- mean(c(unlist(dataAll7$users$statuses_count)))
            dataAll8$sdTweetsF[i] <- sd(c(unlist(dataAll7$users$statuses_count)))
            dataAll8$quantileTweetsF[i] <- paste(quantile(c(unlist(dataAll7$users$statuses_count))), collapse = ", ")
            dataAll8$varTweetsF[i] <- var(c(unlist(dataAll7$users$statuses_count)))
            dataAll8$modaTweetsF[i] <- mfv(c(unlist(dataAll7$users$statuses_count)))[1]
            
            dataAll8$meanFollowersF[i] <- mean(c(unlist(dataAll7$users$followers_count)))
            dataAll8$sdFollowersF[i] <- sd(c(unlist(dataAll7$users$followers_count)))
            dataAll8$quantileFollowersF[i] <- paste(quantile(c(unlist(dataAll7$users$followers_count))), collapse = ", ")
            dataAll8$varFollowersF[i] <- var(c(unlist(dataAll7$users$followers_count)))
            dataAll8$modaFollowersF[i] <- mfv(c(unlist(dataAll7$users$followers_count)))[1]
            
            dataAll8$meanFriendsF[i] <- mean(c(unlist(dataAll7$users$friends_count)))
            dataAll8$sdFriendsF[i] <- sd(c(unlist(dataAll7$users$friends_count)))
            dataAll8$quantileFriendsF[i] <- paste(quantile(c(unlist(dataAll7$users$friends_count))), collapse = ", ")
            dataAll8$varFriendsF[i] <- var(c(unlist(dataAll7$users$friends_count)))
            dataAll8$modaFriendsF[i] <- mfv(c(unlist(dataAll7$users$friends_count)))[1]
            
            dataAll8$meanLangF[i] <- names(sort(table(data.frame(c(unlist(dataAll7$users$lang)))), decreasing = TRUE))[1]
            dataAll8$meanDescriptionKeysF[i] <- meanWords(text = dataAll7$users$description, words = keyWordsFollowers)
            
            dataAll8$gender[i] <- dataAll5$response$data$gender
            dataAll8$genderConfidence[i] <- dataAll5$response$data$confidence_gender
            dataAll8$age[i] <- dataAll5$response$data$age
            dataAll8$ageConfidence[i] <- dataAll5$response$data$confidence_age
            
        }
        
    }
    
    dataAll8
    
}




