### =========== EXPERIMENTO 1 ===========


    ### ----------- EJECUCION -----------


## EJECUCION DEL EXPERIMENTO 1
# @params:
automaticExperiment1 <- function(){
    
    #Se crean la tablas y se calculan las metricas, subfactores y factores
    stored_users_points_1 <- createPuntuationTable1()
    stored_users_points_1 <- addMetrics1(data = stored_users_points_1)
    stored_users_points_1 <- addSubFactors1(data = stored_users_points_1)
    stored_users_points_1 <- addFactors1(data = stored_users_points_1)
    #Se ordenan los datos en funcion de la puntuacion total
    stored_users_points_1 <- stored_users_points_1[order(stored_users_points_1$fact_total, decreasing = TRUE), ]
    experiment1 <<- stored_users_points_1
    #Representacion
    toRepresent <- 1
    representUserPuntuation(confidence = stored_users_points_1$fact_confianza[toRepresent], 
                            influence = stored_users_points_1$fact_capacidad_influencia[toRepresent], 
                            relevance = stored_users_points_1$fact_relevancia_campana[toRepresent], 
                            user = stored_users_points_1$user_screen_nameP[toRepresent])
    
}


    ### ----------- CREACION DATASET Y CALCULOS -----------


## CREA LA TABLA DE PUNTUACIONES PARA EL EXPERIMENTO
# @params:
createPuntuationTable1 <- function(){
    
    length_vector_numeric <- vector(mode = "numeric", length = length(stored_users$idU))
    
    #Primary Key
    user_screen_nameP <- stored_users$user_screen_nameU
        
    #METRICAS
    met_followers_countP <- length_vector_numeric #Confianza y Influencia | Seguido y Alcance
    met_friends_countP <- length_vector_numeric #Confianza y Influencia | Usuario activo y Actividad en la red
    met_listed_countP <- length_vector_numeric #Confianza y Influencia | Usuario activo y Actividad en la red
    met_favourites_countP <- length_vector_numeric #Confianza y Influencia | Usuario activo y Actividad en la red
    met_statuses_countP <- length_vector_numeric #Confianza y Influencia | Usuario activo y Actividad en la red
    met_verifiedP <- length_vector_numeric #Confianza | Identidad verificada
    met_locationP <- length_vector_numeric #Relevancia | Localización geográfica
    met_langP <- length_vector_numeric #Relevancia | Idioma
    met_descriptionP <- length_vector_numeric #Relevancia | Relación campaña
        
    ##SUBFACTORES
    #Confianza
    sub_seguido <- length_vector_numeric 
    sub_usuario_activo <- length_vector_numeric
    sub_identidad_verificada <- length_vector_numeric
    #Capacidad de influencia
    sub_alcance <- length_vector_numeric
    sub_actividad_red <- length_vector_numeric
    #Relevancia para la campaña
    sub_localizacion <- length_vector_numeric
    sub_idioma <- length_vector_numeric
    sub_relacion_campana <- length_vector_numeric
    
    #FACTORES
    fact_confianza <- length_vector_numeric
    fact_capacidad_influencia <- length_vector_numeric
    fact_relevancia_campana <- length_vector_numeric
    fact_total <- length_vector_numeric
    
        #Creamos la tabla
        stored_users_points_1 <- data.frame(
            user_screen_nameP,
            
            met_followers_countP,
            met_friends_countP,
            met_listed_countP,
            met_favourites_countP,
            met_statuses_countP,
            met_verifiedP,
            met_locationP, 
            met_langP,
            met_descriptionP,
            
            sub_seguido,
            sub_usuario_activo,
            sub_identidad_verificada,
            sub_alcance,
            sub_actividad_red,
            sub_localizacion,
            sub_idioma,
            sub_relacion_campana,
            
            fact_confianza,
            fact_capacidad_influencia,
            fact_relevancia_campana,
            fact_total
        )
    
}


## AÑADE LAS METRICAS A LA TABLA
# @params: data
addMetrics1 <- function(data){
    
    dataPoints <- data
    
    dataPoints$met_followers_countP <- calculateFromNumberAdaptPuntuation(value = stored_users$followers_countU)
    dataPoints$met_friends_countP <- calculateFromNumberAdaptPuntuation(value = stored_users$friends_countU)
    dataPoints$met_listed_countP <- calculateFromNumberAdaptPuntuation(value = stored_users$listed_countU)
    dataPoints$met_favourites_countP <- calculateFromNumberAdaptPuntuation(value = stored_users$favourites_countU)
    dataPoints$met_statuses_countP <- calculateFromNumberAdaptPuntuation(value = stored_users$statuses_countU)
    dataPoints$met_verifiedP <- as.numeric(calculateFromBooleanPuntuation(value = stored_users$verifiedU))
    dataPoints$met_locationP <- calculateFromWordsPuntuation(value = stored_users$locationU, words = location_key_words)
    dataPoints$met_langP <- calculateFromWordsPuntuation(value = stored_users$langU, words = lang_words)
    dataPoints$met_descriptionP <- calculateFromWordsPuntuation(value = stored_users$descriptionU, words = keyWordsVec)
    
    dataPoints
}


## AÑADE LOS SUBFACTORES A LA TABLA
# @params: data
addSubFactors1 <- function(data){
    
    dataPoints <- data
    
    #Confianza
    dataPoints$sub_seguido <- dataPoints$met_followers_countP
    dataPoints$sub_usuario_activo <- ((dataPoints$met_friends_countP +
                                 dataPoints$met_listed_countP +
                                 dataPoints$met_favourites_countP + 
                                 dataPoints$met_statuses_countP)/4)
    dataPoints$sub_identidad_verificada <- dataPoints$met_verifiedP
    #Capacidad de influencia
    dataPoints$sub_alcance <- dataPoints$met_followers_countP
    dataPoints$sub_actividad_red <- ((dataPoints$met_friends_countP +
                                 dataPoints$met_listed_countP +
                                 dataPoints$met_favourites_countP + 
                                 dataPoints$met_statuses_countP)/4)
    #Relevancia para la campaña
    dataPoints$sub_localizacion <- dataPoints$met_locationP
    dataPoints$sub_idioma <- dataPoints$met_langP
    dataPoints$sub_relacion_campana <- dataPoints$met_descriptionP
    
    dataPoints
    
}


## AÑADE LOS FACTORES A LA TABLA
# @params: data
addFactors1 <- function(data){
    
    dataPoints <- data
    
    dataPoints$fact_confianza <- 0.5 * dataPoints$sub_seguido + 0.2 * dataPoints$sub_usuario_activo + 0.3 * dataPoints$sub_identidad_verificada
    dataPoints$fact_capacidad_influencia <- 0.7 * dataPoints$sub_alcance + 0.3 * dataPoints$sub_actividad_red
    dataPoints$fact_relevancia_campana <- 0.4 * dataPoints$sub_localizacion + 0.3 * dataPoints$sub_idioma + 0.3 * dataPoints$sub_relacion_campana
    dataPoints$fact_total <- (dataPoints$fact_confianza + dataPoints$fact_capacidad_influencia + dataPoints$fact_relevancia_campana)/3
    
    dataPoints
    
}


