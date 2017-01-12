### =========== EXPERIMENTO 2 ===========


    ### ----------- EJECUCION -----------


## EJECUCION DEL EXPERIMENTO 2
# @params:
automaticExperiment2 <- function(){
    
    #Se crean la tablas y se calculan las metricas, subfactores y factores
    statistic_users_points_2 <- createPuntuationTable2()
    statistic_users_points_2 <- addMetrics2(data = statistic_users_points_2)
    statistic_users_points_2 <- addSubFactors2(data = statistic_users_points_2)
    statistic_users_points_2 <- addFactors2(data = statistic_users_points_2)
    #Se ordenan los datos en funcion de la puntuacion total
    statistic_users_points_2 <- statistic_users_points_2[order(statistic_users_points_2$fact_total, decreasing = TRUE), ]
    experiment2 <<- statistic_users_points_2
    #Representacion
    toRepresent <- 1
    representUserPuntuation(confidence = statistic_users_points_2$fact_confianza[toRepresent], 
                            influence = statistic_users_points_2$fact_capacidad_influencia[toRepresent], 
                            relevance = statistic_users_points_2$fact_relevancia_campana[toRepresent], 
                            user = statistic_users_points_2$user_screen_nameP[toRepresent])
    
}


    ### ----------- CREACION DATASET Y CALCULOS -----------


## CREA LA TABLA DE PUNTUACIONES PARA EL EXPERIMENTO
# @params:
createPuntuationTable2 <- function(){
    
    length_vector_numeric <- vector(mode = "numeric", length = length(statistic_users$idU))
    
    #Primary Key
    user_screen_nameP <- statistic_users$user_screen_nameU
    
    #METRICAS
    met_mean_rtP <- length_vector_numeric #Confianza e Influencia | Calidad Tweets y Alcance recursivo
    met_mean_favP <- length_vector_numeric #Confianza e Influencia | Calidad Tweets y Alcance cualitativo
    met_mean_mentionsP <- length_vector_numeric #Confianza | Interactuacion
    met_mean_hashtagsP <- length_vector_numeric #Influencia | Intencion de ser encontrado
    met_mean_interest_generalP <- length_vector_numeric #Relevancia | Interes general
    met_mean_interest_earlyadopterP <- length_vector_numeric #Relevancia | Interes nuevas tecnologias
    met_mean_interest_financesP <- length_vector_numeric #Relevancia | Interes finanzas
    met_moda_sourceP <- length_vector_numeric #Relevancia | Dispositivos usados
    
    ##SUBFACTORES
    #Confianza
    sub_tweets_quality <- length_vector_numeric
    sub_interaction <- length_vector_numeric
    #Capacidad de influencia
    sub_recursive_range <- length_vector_numeric
    sub_qualitative_range <- length_vector_numeric
    sub_find_intention <- length_vector_numeric
    #Relevancia para la campaña

    sub_interest_general <- length_vector_numeric
    sub_interest_earlyadopter <- length_vector_numeric
    sub_interest_finances <- length_vector_numeric
    sub_source <- length_vector_numeric
    
    #FACTORES
    fact_confianza <- length_vector_numeric
    fact_capacidad_influencia <- length_vector_numeric
    fact_relevancia_campana <- length_vector_numeric
    fact_total <- length_vector_numeric
    
    #Creamos la tabla
    statistic_users_points_2 <- data.frame(
        user_screen_nameP,
        
        met_mean_rtP,
        met_mean_favP,
        met_mean_mentionsP,
        met_mean_hashtagsP,
        met_mean_interest_generalP,
        met_mean_interest_earlyadopterP,
        met_mean_interest_financesP,
        met_moda_sourceP,
        
        sub_tweets_quality,
        sub_interaction,
        sub_recursive_range,
        sub_qualitative_range,
        sub_find_intention,
        sub_interest_general,
        sub_interest_earlyadopter,
        sub_interest_finances,
        sub_source,
        
        fact_confianza,
        fact_capacidad_influencia,
        fact_relevancia_campana,
        fact_total
    )
    
}


## AÑADE LAS METRICAS A LA TABLA
# @params: data
addMetrics2 <- function(data){
    
    dataPoints <- data
    
    dataPoints$met_mean_rtP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanRt)
    dataPoints$met_mean_favP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanFav)
    dataPoints$met_mean_mentionsP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanMentions)
    dataPoints$met_mean_hashtagsP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanHashtags)
    dataPoints$met_mean_interest_generalP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanKeyWords)
    dataPoints$met_mean_interest_earlyadopterP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanKeyWordsEarly)
    dataPoints$met_mean_interest_financesP <- calculateFromNumberAdaptPuntuation(value = statistic_users$meanKeyWordsFinances)
    dataPoints$met_moda_sourceP <- calculateFromNeutralPuntuation(value = statistic_users$commonSource)
    
    dataPoints
}


## AÑADE LOS SUBFACTORES A LA TABLA
# @params: data
addSubFactors2 <- function(data){
    
    dataPoints <- data
    
    #Confianza
    dataPoints$sub_tweets_quality <- ((dataPoints$met_mean_rtP + 
                                                 dataPoints$met_mean_favP)/2)
    dataPoints$sub_interaction <- dataPoints$met_mean_mentionsP
    #Capacidad de influencia
    dataPoints$sub_recursive_range <- dataPoints$met_mean_rtP
    dataPoints$sub_qualitative_range <- dataPoints$met_mean_favP
    dataPoints$sub_find_intention <- dataPoints$met_mean_hashtagsP
    
    #Relevancia para la campaña
    dataPoints$sub_interest_general <- dataPoints$met_mean_interest_generalP
    dataPoints$sub_interest_earlyadopter <- dataPoints$met_mean_interest_earlyadopterP
    dataPoints$sub_interest_finances <- dataPoints$met_mean_interest_financesP
    dataPoints$sub_source <- dataPoints$met_moda_sourceP
    
    dataPoints
    
}


## AÑADE LOS FACTORES A LA TABLA
# @params: data
addFactors2 <- function(data){
    
    dataPoints <- data
    
    dataPoints$fact_confianza <- 0.7 * dataPoints$sub_tweets_quality + 0.3 * dataPoints$sub_interaction
    dataPoints$fact_capacidad_influencia <- 0.5 * dataPoints$sub_recursive_range + 
        0.3 * dataPoints$sub_qualitative_range + 0.2 * dataPoints$sub_find_intention
    dataPoints$fact_relevancia_campana <- 0.3 * dataPoints$sub_interest_general + 
        0.3 * dataPoints$sub_interest_earlyadopter + 0.3 * dataPoints$sub_interest_finances + 0.1 * dataPoints$sub_source
    dataPoints$fact_total <- (dataPoints$fact_confianza + dataPoints$fact_capacidad_influencia + dataPoints$fact_relevancia_campana)/3
    
    dataPoints
    
}


