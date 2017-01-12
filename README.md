# DISEÑO DE UNA HERRAMIENTA PARA LA CARACTERIZACIÓN FINANCIERA DE USUARIOS DE REDES SOCIALES 

### Trabajo de Fin de Grado
### Grado en Ingeniería de Tecnologías y Servicios de Telecomunicación

### Autores:
	- Alberto Sánchez López          

### Contenido:
	- Memoria del TFG
	- Herramienta desarrollada
	- Datasets generados para el caso de estudio

### Resumen:      

Los datos están presentes cada vez más en nuestras vidas, y sin embargo apenas nos damos cuenta de sus aplicaciones. Ya existen ejemplos exitosos de monetización, compañías puramente digitales que generan valor a partir de los datos, siendo estos el centro de su actividad. La oferta de herramientas a terceros para la explotación de datos constituye una de las principales propuestas de valor.
La evolución que está sufriendo nuestra cultura y sociedad en la última década ha cambiado la forma de comunicarse e informarse abriendo una nueva era en la que las redes sociales constituyen una de las principales fuentes de información. En estos medios destaca la existencia de determinados personajes o entidades, conocidos como influencers, capaces de influir con trascendencia en la forma de actuar y pensar de los usuarios que se encuentran en su red.
Las nuevas tecnologías están cambiando la propuesta de valor de los productos y servicios financieros existentes. Esta tendencia denominada Fintech, describe cómo los negocios de siempre asimilan las ideas innovadoras mientras las startups irrumpen en el sector. El marketing de este movimiento es un proceso estratégico, en el que el lanzamiento de nuevos productos a un mercado inestable y competitivo, es primordial para alcanzar el éxito.
El objetivo de este proyecto es lograr una ventaja competitiva para el marketing de aplicaciones Fintech exprimiendo las oportunidades del marco contextual descrito mediante la elaboración de un caso real de estudio. Para ello, se desarrolla una herramienta con capacidad para extraer datos de Twitter, procesar la información obtenida e inferir datos estadísticos almacenándolos en forma de dataset y, analizar las colecciones de datos siguiendo una metodología basada en experimentos. 
El resultado que proporciona el caso de estudio, es un listado que clasifica a los usuarios más influyentes en Twitter, relacionados con la temática, en función de una serie de métricas basadas en la confianza, influencia y relevancia.

### Manual:

##4.5.1. ESTRUCTURA Y PREPARACIÓN

El estudio se desarrolla siguiendo el modelo de obtención de datos junto a los experimentos asociados. Este proceso está dividido en 10 fases de ejecución de la herramienta implementada mediante la introducción de una serie de parámetros, los cuales son necesarios para realizar cada uno de los experimentos. 
La herramienta se prepara para la ejecución cargando todos los scripts “.R” disponibles que conforman el programa en el entorno global. Será necesario instalar los paquetes de las librerías utilizadas mediante el comando:

	install.packages("httr","jsonlite","modeest","fmsb","ggplot2","rJava","xlsx")

También será necesario introducir los tokens de acceso a la API de Twitter en el archivo “parameters.R”.
Una vez instaladas las librerías se procede comenzar el desarrollo del estudio mediante la ejecución del programa. 

##4.5.2. EJECUCIÓN Y OBTENCIÓN DE DATOS

El programa se ejecuta desde el archivo main.R que contiene la función main(). Se incluye la documentación de esta función para comprender su funcionamiento según se desarrolla el estudio. 

	DESARROLLO AUTOMATICO
	@params: flag, first, repeatTimes, error, new, statUsers, followUsersSize, firstAdd, lastAdd, auth, ntable
	main <- function(flag, first = NA, repeatTimes = NA, error = NA, new = NA, statUsers = NA, followUsersSize = NA, firstAdd = NA, lastAdd = NA, auth = NA, ntable = NA)

#4.5.2.1. Obtención y experimentación de tweets y usuarios

En la ejecución de la primera fase se obtiene una muestra en forma de dataset de 270.844 tweets, hallados a partir de un listado de palabras clave; y otra de 311.728 usuarios, conseguidos por búsqueda de palabras clave, usuarios que han publicado los tweets y autores originales de los RT’s. 
El dataset de tweets, “stored_tweets”, incluye la siguiente información: Id, Fecha de creación, @Usuario dueño RT, Número de RT, Número de Favoritos, Texto, Lenguaje, Hashtags, Nº de hashtags, Nombres de usuario de menciones, Nº de menciones, Símbolos, URLs, Media, Dispositivo, Tipo de tweet, Nombre de usuario y @Usuario
Mientras que el dataset de usuarios, “stored_users”, incluye: Id, Fecha de creación, Nombre de usuario, @Usuario, Número de seguidores, Número de seguidos, Número de listas, Localización, Descripción, Número de favoritos, Cuenta verificada, Número de tweets, Lenguaje y la forma de obtención.
	
	Fase 1: 
	main(flag = 1, first = TRUE, repeatTimes = 100, error = FALSE, new = TRUE)
		flag = numeric #Nº de fase
		first = logical #Indica continuar busqueda sobre el MAX_ID
		repeatTimes = numeric #Nº de veces que se ejecuta la obtención de datos
		error = logical #Soluciona errores de la API
		new = logical #Indica crear nuevas tablas

En la segunda fase realizamos tareas de limpieza de duplicados quedando una muestra de 193.541 tweets, y un listado de 177.467 usuarios a analizar. En este caso, los tweets corresponden a las fechas del 2 al 5 de enero de 2017 y podrían añadirse nuevos resultados en caso de requerirlo. 
A partir de la información adquirida de los usuarios, se ejecuta el primer experimento con la configuración descrita que evalúa los factores, subfactores y métricas relacionados. Los resultados de este experimento se almacenan en el dataset “experiment1”.
	
	Fase 2: 
	main(flag = 2)
		flag = numeric #Nº de fase

#4.5.2.2. Obtención y experimentación de datos estadísticos inferidos de sus tweets

Se ejecuta la tercera fase obteniendo un nuevo dataset “statistic_users” que incluye los datos de “stored_users” más las nuevas columnas vacías relacionadas con los datos estadísticos inferidos de los tweets de cada usuario. Las limitaciones de la API obligan a restringir esta parte del estudio por lo que tomaremos 4993 usuarios, debido a que la obtención de esta información es costosa en cuanto a número de usuarios y tiempo. 
	
	Fase 3: 
	main(flag = 3, statUsers = 1000)
		flag = numeric #Nº de fase
		statUsers = numeric #Tamaño de la tabla

En la cuarta fase de ejecución, se obtienen y calculan los valores estadísticos tales como la media, desviación típica, cuantiles, varianza y moda de los RT’s, Fav’s, hashtags, palabras clave y dispositivos habituales, de los últimos 100 tweets de cada usuario y se almacenan en “statistitc_users”.

	Fase 4: 
	main(flag = 4, firstAdd = 1, lastAdd = 100)
		flag = numeric #Nº de fase
		firstAdd = numeric #Fila inicial
		lastAdd = numeric #Fila final

A partir de la información estadística inferida de los tweets de los usuarios, se ejecuta en la quinta fase el segundo experimento con la configuración descrita que evalúa los factores, subfactores y métricas relacionados. Los resultados de este experimento se almacenan en el dataset “experiment2”.
	
	Fase 5: 
	main(flag = 5)
		flag = numeric #Nº de fase

#4.5.2.3. Obtención y experimentación de datos estadísticos inferidos de sus seguidores y demografía

Se procede a ejecutar la sexta fase del proceso, que al igual que en la tercera fase se obtiene un nuevo dataset “followers_users” que incluye los datos de “stored_users” y “statistic_users” más las nuevas columnas vacías relacionadas con los datos estadísticos inferidos de los seguidores de cada usuario y la demografía. Las limitaciones de la API vuelven a restringir esta parte del estudio, por lo que tomaremos 615 usuarios. 
	
	Fase 6: 
	main(flag = 6, followUsersSize = 1000)
		flag = numeric #Nº de fase
		followUsersSize = numeric #Tamaño de la tabla

En la séptima fase de ejecución, se obtienen y calculan los valores estadísticos tales como la media, desviación típica, cuantiles, varianza y moda de los tweets, seguidores, seguidos, lenguaje y palabras clave, de los últimos 200 seguidores de cada usuario junto con el género y edad. Estos datos se almacenan en “followers_users”.
	
	Fase 7: 
	main(flag = 7, firstAdd = 1, lastAdd = 15, auth = TRUE)
		flag = numeric #Nº de fase
		firstAdd = numeric #Fila inicial
		lastAdd = numeric #Fila final
		auth = logical #Indica el tipo de autorización

A partir de la información estadística inferida de los seguidores de los candidatos y la demografía, se ejecuta en la octava fase el tercer experimento con la configuración descrita que evalúa los factores, subfactores y métricas relacionados. Los resultados de este experimento se almacenan en el dataset “experiment3”.

	Fase 8: 
	main(flag = 8)
		flag = numeric #Nº de fase

#4.5.2.4. Obtención de puntuaciones finales

Una vez obtenidos los resultados de los tres experimentos, calculamos las puntaciones finales introduciendo el coeficiente de importancia de cada factor/experimento como parámetro, en la ejecución de la novena fase. El dataset con las puntaciones finales se almacenan en “allExperiments”.
	
	Fase 9: 
	main(flag = 9)
		flag = numeric #Nº de fase

#4.5.2.5. Exportación de datasets

La exportación de todos los dataset obtenidos se realiza en la décima y última fase de ejecución del programa. Para ello se introducen una serie de parámetros que indican el dataset a exportar y lo transforma a un archivo “xlsx” (formato Excel).
	Fase 10: 
	main(flag = 10, ntable = 1)
		flag = numeric #Nº de fase
		ntable = numeric #Nº de tabla a exportar (1:8)




