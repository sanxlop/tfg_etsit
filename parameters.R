### =========== PARAMETROS ===========


    ### ----------- TOKENS -----------

#Listado de claves y tokens para autenticacion para Twitter
api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""

#Tokens para AI Applied
token_applied <- c("381e69c67a63f500a4ac1ea77ebf1b5c00016a21278de62e4abe1171a423ef86")


    ### ----------- LISTADOS DE PALABRAS CLAVE -----------


#Lista de palabras clave generales
keyWordsVec <- c("fintech", "earlyadopter", "startup", "digital", "app", "bitcoin", 
                 "business", "fintonic", "twyp", "bizum", "imaginebank", "mooverang",
                 "wallo", "santander", "bbva", "caixa", "bankia", "ing", "finances", 
                 "finanzas", "payment", "pago", "mobile", "movil", "bank", "banca", 
                 "technology", "tecnologia", "cash", "dinero", "wallet", "cartera")

#Lista de palabras clave concatenadas para la busqueda de tweets
keyWords <- paste(as.character(keyWordsVec), collapse = "%20OR%20")

#Lista de palabras clave localizacion geografica
location_key_words <- c("spain", "españa", "madrid", "barcelona", "valencia", "vasco", 
                        "galicia", "asturias", "cantabria", "navarra", "cataluña", 
                        "castilla", "leon", "mancha", "rioja", "aragon", "extremadura", 
                        "baleares", "canarias", "murcia", "andalucia", "salamanca", "jaen", 
                        "cordoba", "sevilla", "huelva", "cadiz", "malaga", "granada", "almeria",
                        "valladolid", "huesca", "teruel", "zaragoza", "oviedo", "mallorca", 
                        "tenerife", "santander", "cuenca", "albacete", "guadalajara", "toledo",
                        "avila", "burgos", "palencia", "segovia", "zamora", "gerona", "lerida",
                        "tarragona", "alicante", "castellon", "lugo", "pontevedra", "orense",
                        "pamplona", "vitoria", "bilbao", "ceuta", "melilla")

#Lista de palabras clave idioma
lang_words <- c("es")

#Lista de palabras clave early adopter
keyWordsEarly <- c("early", "adopter", "alpha", "beta", "android", "ios", "windows", 
                   "os", "app", "web", "iot", "machine", "learning", "big", "data", 
                   "scientist", "science", "test", "prueba", "aprender", "datos", 
                   "aplicacion", "cientifico", "aprendizaje", "automatico", "auto", 
                   "ciencia", "innovate", "new", "nuevo", "salida", "innovar", "probar", 
                   "tech", "internet", "thing")

#Lista de palabras clave finances
keyWordsFinances <- c("finance", "bank", "fintech", "cash", "bitcoin", "bolsa", "invertir",
                      "inversion", "banco", "credit", "moneda", "coin", "invest", "stock",
                      "money", "amortiza", "accion", "current", "asset", "passive", "activo",
                      "corriente", "pasivo", "acreedor", "caida", "subida", "fall", "drop",
                      "rise", "interes", "cajero", "transferencia", "transfer", "virtual", "online",
                      "finanza", "conta", "account", "direct", "bet", "apostar", "open", "abierto",
                      "inflacion", "cae", "sube", "empresa", "cooperativa")

#Lista de palabras clave followers
keyWordsFollowers <- unique(c(keyWordsVec, keyWordsEarly, keyWordsFinances)) #keyWordsVec, keyWordsEarly, keyWordsFinances

#Lista de palabras clave concatenadas para la busqueda de tweets
keyWordsPrueba <- paste(as.character(keyWordsFollowers), collapse = "%20OR%20")

