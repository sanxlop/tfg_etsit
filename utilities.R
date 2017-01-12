### =========== UTILIDADES ===========


    ### ----------- FUNCIONES GENERALES -----------


## FUNCION QUE OBTIENE UN STRING CONCATENADO POR USUARIO DE UNA LISTA
# @params: List="Lista a analizar", TList="Donde se almacenan res", type="Tipo de dato"
oneStringPerUser <- function(List, TList = list(), type){
    
    if(type == "#"){
        for( i in 1:length(List) ){
            if (is.null(List[[i]]$text)){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste("#", List[[i]]$text[ 1:length(List[[i]]$text) ], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            }
        }
    }
    if(type == "@"){
        for( i in 1:length(List) ){
            if (is.null(List[[i]]$screen_name)){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste("@", List[[i]]$screen_name[ 1:length(List[[i]]$screen_name) ], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            }
        }
    }
    if(type == "$"){
        for( i in 1:length(List) ){
            if (is.null(List[[i]]$text)){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste("$", List[[i]]$text[ 1:length(List[[i]]$text) ], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            }
        }
    }
    if(type == "url"){
        for( i in 1:length(List) ){
            if (is.null(List[[i]]$url)){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste(List[[i]]$url[ 1:length(List[[i]]$url) ], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            }
        }
    }
    if(type == "media_url"){
        for( i in 1:length(List) ){
            if (is.null(List[[i]]$media_url)){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste(List[[i]]$media_url[ 1:length(List[[i]]$media_url) ], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            }
        }
    }
    if(type == "RT"){
        for( i in 1:length(List) ){
            if (is.null(List[[i]])){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste(List[[i]], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            }
        }
    }
    if(type == "resp"){
        for( i in 1:length(List) ){

            if (class(List) == "data.frame"){
                #print("Fallo1")
                TList <- c(TList, "NonReachable")
            }
            else if ( grepl( "list()", as.character(List[i]) ) ){
                TList <- c(TList, "None")
            } else {
                oneChar <- paste(List[i][ 1:length(List[i]) ], collapse=", ", sep = "")
                TList <- c(TList, oneChar)
            } 
        }
    }
    
    c(unlist(TList))
}


## FUNCION QUE DEVUELVE EL NUMERO DE UNIDADES
# @params: List="Lista a analizar", TNList="Donde se almacenan res", type="Tipo de dato"
numberOfThings <- function(List, TNList = list(), type){
  
    if(type == "#"){
      for( i in 1:length(List) ){
        if (is.null(List[[i]]$text)){
          TNList <- c(TNList, 0)
        } else {
          TNList <- c( TNList, length(List[[i]]$text) )
        }
      }
    }
    if(type == "@"){
      for( i in 1:length(List) ){
        if (is.null(List[[i]]$screen_name)){
          TNList <- c(TNList, 0)
        } else {
          TNList <- c( TNList, length(List[[i]]$screen_name) )
        }
      }
    }

    c(unlist(TNList))
    
}


## FUNCION QUE ELIMINA ETIQUETAS XML, QUEDANDO EL CONTENIDO
# @params: Charact="Cadena", CharactT="Cadena resultado"
sourceDetect <- function(Charact, CharactT = character()) {
    
    for (i in 1:length(Charact)) {
        CharactT[i] <- substring(text = Charact[i], first = regexpr(">", Charact[i]) + 1, 
                                 last = regexpr("</", Charact[i]) - 1)
    }
    CharactT
    
}


## FUNCION QUE LIMPIA CARACTERES ESPECIALES DE TEXTOS
# @params: text="Cadena de texto"
cleanText <- function(text){
    
    #Borra RT
    #txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    txtclean = gsub("(RT|via)", "", text)
    #Borra espacios al principio
    txtclean = gsub("^ ", "", txtclean)
    #Borra espacios al final
    txtclean = gsub(" $", "", txtclean)
    #Borra @usuarios
    #txtclean = gsub("@\\w+", "", txtclean)
    #Borra simbolos de puntuacion
    txtclean = gsub("[[:punct:]]", "", txtclean)
    #Borra numeros
    txtclean = gsub("[[:digit:]]", "", txtclean)
    #Borra links
    txtclean = gsub("http\\w+", "", txtclean)
    #Borra Tabs
    txtclean <- gsub("[ |\t]{2,}", "", txtclean)
    #Borrarmos todos los caracteres excepto
    txtclean <- gsub("[^A-Za-z0-9áéíóúÁÉÍÓÚñÑ ]", "", txtclean)
    #Transformamos a minusculas
    txtclean = tolower(txtclean)
    #Quita tildes
    txtclean <- chartr('áéíóú','aeiou',txtclean)
    
    txtclean
    
}


## FUNCION QUE DEVUELVE MEDIA DE PALABRAS POR TEXTO
# @params: text="texto", words="palabras"
meanWords <- function(text, words){
    
    cleaned_text <- cleanText(text)
    separed_text <- unlist(strsplit(cleaned_text, split=" "))
    counter_words <- 0
    
    for(i in 1:length(words)){
        counter_words <- counter_words + length(grep(words[i], cleaned_text, value = FALSE))
    }
    
    counter_words/length(cleaned_text)
    
}


## FUNCION QUE LIMPIA DUPLICADOS DE USUARIOS
# @params: text="dataframe a limpiar"
cleanUserDuplicates <- function(data){
    #Borramos filas iguales
    users_cleaned <- unique(data) 
    #Puede haber usuarios que hayan variado en la tabla con el tiempo
    #Luego nos quedamos con el ultimo valor conocido de cada usuario
    users_cleaned <- users_cleaned[!duplicated(users_cleaned[1], fromLast = TRUE), ]
}


## FUNCION QUE LIMPIA DUPLICADOS DE TWEETS
# @params: text="dataframe a limpiar"
cleanTweetsDuplicates <- function(data){
    #Borramos filas iguales
    tweets_cleaned <- unique(data)
    #Borramos Tweets que son Retweets luego no son el dueño
        #tweets_cleaned <- tweets_cleaned[!grep(pattern = "RT", tweets_cleaned[6]), ]
    #Puede haber usuarios que hayan variado en la tabla con el tiempo
    #Luego nos quedamos con el ultimo valor conocido de cada usuario
    tweets_cleaned <- tweets_cleaned[!duplicated(tweets_cleaned[6], fromLast = TRUE), ]
}


    ### ----------- FUNCIONES DE EXPERIMENTOS -----------


## FUNCION QUE CALCULA PUNTUACIONES RELATIVAS A PARTIR DE UN VECTOR DE NUMEROS
# @params: value="valores numericos"
calculateFromNumberPuntuation <- function(value){
    
    maxValue <- max(value)
    result <- (10*value)/maxValue
    result
    
}


## FUNCION QUE CALCULA PUNTUACIONES RELATIVAS ADAPTADAS A PARTIR DE UN VECTOR DE NUMEROS
# @params: value="valores numericos"
calculateFromNumberAdaptPuntuation <- function(value){
    
    maxValue <- max(value)
    lengthData <- as.numeric( substring( as.character(length(value)), 1, nchar(as.character(length(value)))-1 ) )
    meanMagic <- mean(sort(value, decreasing = TRUE)[1:lengthData])
    result <- vector(length = length(value))
    
    for(i in 1:length(value)){
        if(value[i] > meanMagic){
            result[i] <- 5+(value[i]*5)/maxValue
        }
        else{
            result[i] <- (value[i]*5)/meanMagic
        }
    }

    result
    
}


## FUNCION QUE CALCULA PUNTUACIONES A PARTIR DE UN VECTOR BOOLEAN
# @params: value="valores booleanos"
calculateFromBooleanPuntuation <- function(value){
    
    result <- gsub(pattern = TRUE, replacement = 10, x = value)
    result <- gsub(pattern = FALSE, replacement = 0, x = result)
    result
    
}


## FUNCION QUE CALCULA PUNTUACIONES A PARTIR DE UNA LISTA DE PALABRAS COINCIDENTES SOBRE UN TEXTO
# @params: value="texto", words="palabras"
calculateFromWordsPuntuation <- function(value, words){
    
    result <- vector(mode = "numeric", length = length(value))
    for(i in 1:length(value)){
        if (meanWords(text = value[i], tolower(words)) != 0){
            result[i] <- 10
        } else {
            result[i] <- 0
        }
    }
    result
    
}


## FUNCION QUE CALCULA PUNTUACIONES DISCRIMINANDO EL GENERO UNKNOWN
# @params: value="vector de male, female o unknown"
calculateFromGenderPuntuation <- function(value, confidence){
    
    result <- vector(mode = "numeric", length = length(value))
    for(i in 1:length(value)){
        if (value[i] == "unknown" || confidence[i]<0.65){
            result[i] <- 0
        } else {
            result[i] <- 10 #* confidence[i]
        }
    }
    result
    
}


## FUNCION QUE CALCULA PUNTUACIONES NEUTRALES INDEPENDIENTEMENTE DEL VALOR
# @params: value="valores"
calculateFromNeutralPuntuation <- function(value){
    
    result <- vector(mode = "numeric", length = length(value))
    for(i in 1:length(value)){
        result[i] <- 10
    }
    result
    
}


    ### ----------- FUNCIONES DE EXPORTACION -----------


## FUNCION QUE OPTIMIZA EL addDataFrame DE xlsx
jgc <- function(){
    gc()
    .jcall("java/lang/System", method = "gc")
}    


## FUNCION QUE EXPORTA LOS DATAFRAMES DEL PROYECTO A FORMATO EXCEL EN VARIAS SHEETS
# @params: ntable="numero de la tabla a exportar"
exportWorkbookToExcel <- function(ntable){
    
    options(java.parameters = "-Xmx6000m") #Memoria RAM
    Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111') # for 64-bit version
    library(xlsx)
    
    wb <- createWorkbook(type="xlsx")
    
    cs1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()  # header
    
    for(i in ntable:ntable){
        jgc()
        if(i==1){
            fileName <- "stored_tweets.xlsx"
            sheet_1 <- createSheet(wb = wb, sheetName="Lista_Tweets")
            addDataFrame(x = stored_tweets, sheet = sheet_1, colnamesStyle = cs1, row.names = FALSE)
            print("Lista_Tweets")
        }
        if(i==2){
            fileName <- "stored_users.xlsx"
            sheet_2 <- createSheet(wb = wb, sheetName="Info_Usuarios")
            addDataFrame(x = stored_users, sheet = sheet_2, colnamesStyle = cs1, row.names = FALSE)
            print("Info_Usuarios")
        }
        if(i==3){
            fileName <- "statistic_users.xlsx"
            sheet_3 <- createSheet(wb = wb, sheetName="Info_Tweets_Usuarios")
            addDataFrame(x = statistic_users, sheet = sheet_3, colnamesStyle = cs1, row.names = FALSE)
            print("Info_Tweets_Usuarios")
        }
        if(i==4){
            fileName <- "followers_users.xlsx"
            sheet_4 <- createSheet(wb = wb, sheetName="Info_Seguidores_Usuarios")
            addDataFrame(x = followers_users, sheet = sheet_4, colnamesStyle = cs1, row.names = FALSE)
            print("Info_Seguidores_Usuarios")
        }
        if(i==5){
            fileName <- "experiment1.xlsx"
            sheet_5 <- createSheet(wb = wb, sheetName="Experimento_1")
            addDataFrame(x = experiment1, sheet = sheet_5, colnamesStyle = cs1, row.names = FALSE)
            print("Experimento_1")
        }
        if(i==6){
            fileName <- "experiment2.xlsx"
            sheet_6 <- createSheet(wb = wb, sheetName="Experimento_2")
            addDataFrame(x = experiment2, sheet = sheet_6, colnamesStyle = cs1, row.names = FALSE)
            print("Experimento_2")
        }
        if(i==7){
            fileName <- "experiment3.xlsx"
            sheet_7 <- createSheet(wb = wb, sheetName="Experimento_3")
            addDataFrame(x = experiment3, sheet = sheet_7, colnamesStyle = cs1, row.names = FALSE)
            print("Experimento_3")
        }
        if(i==8){
            fileName <- "allExperiments.xlsx"
            sheet_8 <- createSheet(wb = wb, sheetName="Resultados_Experimentos")
            addDataFrame(x = allExperiments, sheet = sheet_8, colnamesStyle = cs1, row.names = FALSE)
            print("Resultados_Experimentos")
        }
    }
    
    saveWorkbook(wb = wb, file = fileName)
    
}


    ### ----------- FUNCIONES EN DESHUSO -----------


## FUNCION QUE EXPORTA UN DATAFRAME A FORMATO EXCEL
# @params: dataframe
exportToExcel <- function(dataframe, file = "exported.xlsx", sheet = "1"){
    
    options(java.parameters = "-Xmx6000m") #Memoria RAM
    Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111') # for 64-bit version
    library(xlsx)
    jgc()
    #cs1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()  # header
    message("INICIO")
    write.xlsx2(x = dataframe, file = file, sheetName = sheet, row.names = FALSE)
    message("FIN")
    
}


## FUNCION QUE DEVUELVE FRECUENCIAS DE PALABRAS DADAS CADENAS DE CARACTERES
# @params: text="Cadena de texto", wordsN="n palabras a mostrar"
frecWords <- function(text, wordsN = 10){
    
    #Borra RT
    #txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
    txtclean = gsub("(RT|via)", "", text)
    #Borra espacios al principio
    txtclean = gsub("^ ", "", txtclean)
    #Borra espacios al final
    txtclean = gsub(" $", "", txtclean)
    #Borra @usuarios
    #txtclean = gsub("@\\w+", "", txtclean)
    #Borra simbolos de puntuacion
    txtclean = gsub("[[:punct:]]", "", txtclean)
    #Borra numeros
    txtclean = gsub("[[:digit:]]", "", txtclean)
    #Borra links
    txtclean = gsub("http\\w+", "", txtclean)
    #Borra Tabs
    txtclean <- gsub("[ |\t]{2,}", "", txtclean)
    #Borrarmos todos los caracteres excepto
    txtclean <- gsub("[^A-Za-z0-9áéíóúÁÉÍÓÚñÑ ]", "", txtclean)
    #Transformamos a minusculas
    txtclean = tolower(txtclean)
    #Quita tildes
    txtclean <- chartr('áéíóú','aeiou',txtclean)
    
    words <- c("de", "que", "no", "el", "a", "me", "y", "la", "en", "con", 
               "se", "por", "los", "lo", "es", "para", "como", "ya", "un", 
               "mi", "yo", "mas", "si", "del", "le", "porque", "una", "les", 
               "las", "asi", "te", "ni")
    
    result <- strsplit(txtclean, split=" ")
    
    #Listado de palabras recogidas
    texto_col <<- as.character(unlist(result))
    #Listado de palabras segun su frecuencia
    texto_frame <<- table(data.frame(texto_col))
    #Listado de palabras indicadas ordenadas segun su frecuencia
    result <- sort(texto_frame, decreasing = TRUE)[1:wordsN]
    result
    
}



