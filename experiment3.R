### =========== EXPERIMENTO 3 ===========


    ### ----------- EJECUCION -----------


## EJECUCION DEL EXPERIMENTO 3
# @params:
automaticExperiment3 <- function(){
    
    #Se crean la tablas y se calculan las metricas, subfactores y factores
    followers_users_points_3 <- createPuntuationTable3()
    followers_users_points_3 <- addMetrics3(data = followers_users_points_3)
    followers_users_points_3 <- addSubFactors3(data = followers_users_points_3)
    followers_users_points_3 <- addFactors3(data = followers_users_points_3)
    #Se ordenan los datos en funcion de la puntuacion total
    followers_users_points_3 <- followers_users_points_3[order(followers_users_points_3$fact_total, decreasing = TRUE), ]
    experiment3 <<- followers_users_points_3
    #Representacion
    toRepresent <- 1
    representUserPuntuation(confidence = followers_users_points_3$fact_confianza[toRepresent], 
                            influence = followers_users_points_3$fact_capacidad_influencia[toRepresent], 
                            relevance = followers_users_points_3$fact_relevancia_campana[toRepresent], 
                            user = followers_users_points_3$user_screen_nameP[toRepresent])
    
}


    ### ----------- CREACION DATASET Y CALCULOS -----------


## CREA LA TABLA DE PUNTUACIONES PARA EL EXPERIMENTO
# @params:
createPuntuationTable3 <- function(){
    
    length_vector_numeric <- vector(mode = "numeric", length = length(followers_users$idU))
    
    #Primary Key
    user_screen_nameP <- followers_users$user_screen_nameU
    
    #METRICAS
    met_genderP <- length_vector_numeric #Confianza | Genero 
    met_mean_followers_followersP <- length_vector_numeric #Confianza e Influencia | Veracidad y Alcance de los seguidores
    met_mean_friends_followersP <- length_vector_numeric #Confianza e Influencia | Veracidad y Actividad de los seguidores
    met_mean_statuses_followersP <- length_vector_numeric #Confianza e Influencia | Veracidad y Actividad de los seguidores
    met_mean_interest_description_followersP <- length_vector_numeric #Relevancia | Relacion de la red
    met_moda_lang_followersP <- length_vector_numeric #Relevancia | Idioma de su red
    met_ageP <- length_vector_numeric #Relevancia | Edad
    met_gender2P <- length_vector_numeric #Relevancia | Genero

    ##SUBFACTORES
    #Confianza
    sub_veracity_followers <- length_vector_numeric 
    sub_gender <- length_vector_numeric 
    #Capacidad de influencia
    sub_activity_followers <- length_vector_numeric
    sub_quantitative_range_followers <- length_vector_numeric
    #Relevancia para la campaña
    sub_relation_followers <- length_vector_numeric
    sub_lang_followers <- length_vector_numeric
    sub_age <- length_vector_numeric
    sub_gender2 <- length_vector_numeric
    
    #FACTORES
    fact_confianza <- length_vector_numeric
    fact_capacidad_influencia <- length_vector_numeric
    fact_relevancia_campana <- length_vector_numeric
    fact_total <- length_vector_numeric
    
    #Creamos la tabla
    followers_users_points_3 <- data.frame(
        user_screen_nameP,
        
        met_genderP,

        met_mean_followers_followersP,
        met_mean_friends_followersP,
        met_mean_statuses_followersP,
        met_mean_interest_description_followersP,
        met_moda_lang_followersP,
        met_ageP,
        met_gender2P,
        
        sub_veracity_followers,
        sub_gender,
        sub_activity_followers,
        sub_quantitative_range_followers,
        sub_relation_followers,
        sub_lang_followers,
        sub_age,
        sub_gender2,
        
        fact_confianza,
        fact_capacidad_influencia,
        fact_relevancia_campana,
        fact_total
    )
    
}


## AÑADE LAS METRICAS A LA TABLA
# @params: data
addMetrics3 <- function(data){
    
    dataPoints <- data
    
    dataPoints$met_mean_followers_followersP <- calculateFromNumberAdaptPuntuation(value = followers_users$meanFollowersF)
    dataPoints$met_mean_friends_followersP <- calculateFromNumberAdaptPuntuation(value = followers_users$meanFriendsF)
    dataPoints$met_mean_statuses_followersP <- calculateFromNumberAdaptPuntuation(value = followers_users$meanTweetsF)
    dataPoints$met_mean_interest_description_followersP <- calculateFromNumberAdaptPuntuation(value = followers_users$meanDescriptionKeysF)
    dataPoints$met_moda_lang_followersP <- calculateFromWordsPuntuation(value = followers_users$meanLangF, words = lang_words)
    dataPoints$met_genderP <- calculateFromGenderPuntuation(value = followers_users$gender, confidence = followers_users$genderConfidence)
    dataPoints$met_ageP <- calculateFromNeutralPuntuation(value = followers_users$age)
    dataPoints$met_gender2P <- calculateFromGenderPuntuation(value = followers_users$gender, confidence = followers_users$genderConfidence)
    
    dataPoints
    
}


## AÑADE LOS SUBFACTORES A LA TABLA
# @params: data
addSubFactors3 <- function(data){
    
    dataPoints <- data
    
    #Confianza
    dataPoints$sub_veracity_followers <- ((dataPoints$met_mean_followers_followersP +
                                                  dataPoints$met_mean_friends_followersP +
                                                  dataPoints$met_mean_statuses_followersP)/3)
    dataPoints$sub_gender <- dataPoints$met_genderP
    
    #Capacidad de influencia
    dataPoints$sub_activity_followers <- ((dataPoints$met_mean_friends_followersP +
                                                   dataPoints$met_mean_statuses_followersP)/2)
    dataPoints$sub_quantitative_range_followers <- dataPoints$met_mean_followers_followersP
    
    #Relevancia para la campaña
    dataPoints$sub_relation_followers <- dataPoints$met_mean_interest_description_followersP
    dataPoints$sub_lang_followers <- dataPoints$met_moda_lang_followersP
    dataPoints$sub_age <- dataPoints$met_ageP
    dataPoints$sub_gender2 <- dataPoints$met_gender2P
    
    dataPoints
    
}


## AÑADE LOS FACTORES A LA TABLA
# @params: data
addFactors3 <- function(data){
    
    dataPoints <- data
    
    dataPoints$fact_confianza <- 0.7 * dataPoints$sub_veracity_followers + 0.3 * dataPoints$sub_gender
    dataPoints$fact_capacidad_influencia <- 0.3 * dataPoints$sub_activity_followers + 
        0.7 * dataPoints$sub_quantitative_range_followers
    dataPoints$fact_relevancia_campana <- 0.3 * dataPoints$sub_relation_followers + 
        0.3 * dataPoints$sub_lang_followers + 0.1 * dataPoints$sub_age + 0.3 * dataPoints$sub_gender2
    dataPoints$fact_total <- (dataPoints$fact_confianza + dataPoints$fact_capacidad_influencia + dataPoints$fact_relevancia_campana)/3
    
    dataPoints
    
}


