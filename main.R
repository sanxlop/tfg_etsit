### =========== MAIN ===========


library(httr)
library(jsonlite)
library(modeest)
library(fmsb)
library(ggplot2)


## DESARROLLO AUTOMATICO
# @params: flag, first, repeatTimes, error, new, statUsers, followUsersSize, firstAdd, lastAdd, auth, ntable
main <- function(flag, first = NA, repeatTimes = NA, error = NA, new = NA, statUsers = NA, 
                             followUsersSize = NA, firstAdd = NA, lastAdd = NA, auth = NA, ntable = NA){
    
    ##Comprobamos si la flag introducidad es correcta
    if(!is.numeric(flag) | flag > 10){
        stop("Introduce el parametro correctamente: flag(1:10)")
    }
    
    ##Obtenemos los tweets y los usuarios a partir de los tweets, keywords y dueños de rt
    ##Nos devuelve stored_tweets y stored_users
    if(flag == 1){
            message("FASE 1: Obteniendo tweets y usuarios")
            #Parametros que se deben introducir: first, repeatTimes, error y new
            if(!is.logical(first) || !is.numeric(repeatTimes) || !is.logical(error) || !is.logical(new)){
                stop("Introduce los parametros correctamente: first(TRUE, FALSE), repeatTimes(>1), error(TRUE, FALSE), new(TRUE, FALSE)")
            }
        repeatSearch(query = keyWords, count = 100, repeatTimes = repeatTimes, first = first, error = error, new = new)
            print("Completado")
    }
    ##Limpiamos los usuarios duplicados
    ##Ejecutamos el experimento 1 que nos devuelve un dataset experiment1
    if(flag == 2){
            message("FASE 2: Limpiando tweets y ususarios")
        stored_tweets <<- cleanTweetsDuplicates(data = stored_tweets)
            print("Tabla de tweets limpia")
        stored_users <<- cleanUserDuplicates(data = stored_users)
            print("Tabla de usuarios limpia")
            message("FASE 2: Ejecutando experimento 1")
        automaticExperiment1()
            print("Completado")
    }
    ##Almacenamos temporalmente los stored_users con mayor puntuacion en stored_users_temp
    ##Creamos la tabla vacia de las estadisticas de los tweets de cada uno de esos usuarios
    if(flag == 3){
            message("FASE 3: Creando tabla estadistica de tweets de usuarios vacia")
            #Parametros que se deben introducir: statUsers
            if(!is.numeric(statUsers) || statUsers>100000 ){
                stop("Introduce el parametro correctamente: statUsers(1:100000)")
            }
        names_user_temp <- as.vector(experiment1$user_screen_nameP[1:statUsers])
        stored_users_temp <- subset(stored_users, user_screen_nameU == names_user_temp[1])
        for(i in 2:statUsers){
            stored_users_temp <- rbind(stored_users_temp, subset(stored_users, user_screen_nameU == names_user_temp[i]))
        }
        
        statistic_users <<- statisticUsers(data_users = stored_users_temp, firstAdd = 1, lastAdd = statUsers)
            print("Completado")
    }
    ##Añadimos a la tabla la info de los followers en los huecos (limitado a 15 cada 15 min)
    ##Si alguno da error lo eliminamos del dataset y volvemos a intentarlo (ejecucionSegura)
    if(flag == 4){
            message("FASE 4: Calculando y añadiendo estadisticos en la tabla de tweets de usuarios")
            #Parametros que se deben introducir: firstAdd, lastAdd
            if(!is.numeric(firstAdd) || !is.numeric(lastAdd) || (lastAdd-firstAdd)>10000 || lastAdd<firstAdd){
                stop("Introduce los parametros correctamente: firstAdd(>1), lastAdd(>1)") 
            }
        statistic_users <<- addStatistic(statistic_users, firstAdd = firstAdd, lastAdd = lastAdd, auth = TRUE)
            print("Completado")
    }
    ##Ejecutamos el experimento 2 que nos devuelve un dataset experiment2
    if(flag == 5){
            message("FASE 5: Ejecutando experimento 2")
        automaticExperiment2()
            print("Completado")
    }
    ##Creamos la tabla con los huecos de la info de los followers ordenados del experimento 2 en followers_users
    if(flag == 6){
            message("FASE 6: Creando tabla estadistica de seguidores y genero de usuarios vacia")
            #Parametros que se deben introducir: followUsersSize
            if(!is.numeric(followUsersSize) || followUsersSize>5000){
                stop("Introduce el parametro correctamente: followUsersSize(1:5000)")
            }
        names_user_temp <- as.vector(experiment2$user_screen_nameP[1:followUsersSize])
        statistic_users_temp <- subset(statistic_users, user_screen_nameU == names_user_temp[1])
        for(i in 2:followUsersSize){
            statistic_users_temp <- rbind(statistic_users_temp, subset(statistic_users, 
                                                                       user_screen_nameU == names_user_temp[i]))
        }
        followers_users <<- followersInfoUsers(data_users = statistic_users_temp)
            print("Completado")
    }
    ##Añadimos a la tabla la info de los followers en los huecos (limitado a 15 cada 15 min)
    if(flag == 7){
            message("FASE 7: Calculando y añadiendo estadisticos en la tabla de seguidores y genero de usuarios")
            #Parametros que se deben introducir: firstAdd, lastAdd, auth
            if(!is.numeric(firstAdd) || !is.numeric(lastAdd)  || !is.logical(auth) 
               || (lastAdd-firstAdd)>14){
                stop("Introduce los parametros correctamente: firstAdd(1, 16, 31,...), lastAdd(15, 30, 45,...) y auth(TRUE, FALSE)") 
            }
        followers_users <<- addFollowersInfo(data = followers_users, firstAdd = firstAdd, lastAdd = lastAdd, auth = auth)
        authmess <- as.character(auth)
            print(paste("auth: ", authmess, sep = ""))
            print("Completado")
    }
    ##Recortamos la tabla a analizar solo con las casillas rellenadas
    ##Ejecutamos el experimento 3 que nos devuelve un dataset experiment3
    if(flag == 8){
            message("FASE 8: Ejecutando experimento 3")
        automaticExperiment3()
            print("Completado")
    }
    ##Realizamos los calculos totales de los experimentos
    if(flag == 9){
            message("FASE 9: Uniendo resultados finales de los 3 experimentos")
        allExperiments <<- puntuationTableAll(data1 = experiment1, data2 = experiment2, data3 = experiment3)
        print("Completado")
    }
    ##Exportamos todos los datos a excel
    if(flag == 10){
            message("FASE 10: Exportando resultados obtenidos a excel")
            #Parametros que se deben introducir: ntable
            if(!is.numeric(ntable) || ntable>8){
                stop("Introduce el parametro correctamente: ntable(1:8)")
            }
        exportWorkbookToExcel(ntable = ntable)
            print("Completado")
    }
    
}