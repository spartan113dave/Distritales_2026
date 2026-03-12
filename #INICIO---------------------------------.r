#_edit20260311
#INICIO-------------------------------------------------------------------------
gc(rm(list=ls()))

#Instrucción para correr el script con output mínimo en la consola para minimizar el tiempo de ejecución
#source("DISTRITOS_FEDERALES_TAMPS-8.R", echo = FALSE, verbose = FALSE)

#Iniciador del cronómetro
starttime <- Sys.time()

#DIRECTORIO---------------------------------------------------------------------
setwd("C:/Users/LapOne MX/OneDrive/Documents/GobMex/Distritales/TAMPS08_20260303")

#ARCHIVO SAV--------------------------------------------------------------------
archivo_sav <- list.files(path = "C:/Users/LapOne MX/OneDrive/Documents/GobMex/Distritales/TAMPS08_20260303", pattern = "\\.sav$", full.names = TRUE)[1]

#PERIODO RONDA ACTUAL-----------------------------------------------------------
estado <- "Tamaulipas" #<⚠️CAMBIAR NOMBRE-----------------
estado_mayus <- "TAMAULIPAS" #<⚠️CAMBIAR NOMBRE------------
distrito <- "TAMAULIPAS-8"#<⚠️CAMBIAR NOMBRE-----------

#BIBLIOTECAS--------------------------------------------------------------------
library(pacman)
pacman::p_load(readxl,openxlsx,ggplot2,maditr,ggrepel,haven,scales,extrafont,forcats,showtext,magick,tidyr,flextable,
               haven,stringr,dplyr,foreign,purrr,plotrix,stringi,formattable,gt,webshot2,magrittr,officer,showtext,fuzzyjoin)

#fuente Geomanist
font_add(family = "Geomanist",regular = "C:/Users/LapOne MX/OneDrive/Documents/GobMex/Distritales/Paquete_distritales/Geomanist-Regular.OTF") #fuente de pc oficina
showtext_auto()
showtext_opts(dpi = 300)

#fuente Pop pins
# font_add(family = "Pop pins",regular = "C:/Users/PC/Documents/ISRAEL/PONDERACIÓN/Pop pins-Regular.TTF") #fuente de pc oficina
# showtext_auto()
# showtext_opts(dpi = 300)

#CARGAMOS BASE ACUMULADOS-----------------------------------------------------------------

#LAPTOP
# setwd("D:/CASA/PONDERACIÓN") #LAP
# font_add(family = "Geomanist",regular = "D:/CASA/PONDERACIÓN/Geomanist-Regular.OTF") #lap
# archivo_sav <- list.files(path = "D:/CASA/PONDERACIÓN", pattern = "\\.sav$", full.names = TRUE)[1]#LAP

#++++++++++++++++++++++++++++++-------------------------------------------------
#++++++++++++++++++++++++++++++-------------------------------------------------
#DICCIONARIO------------------------------------------------------
archivo_sav
datos2 <- read_sav(archivo_sav, encoding = "UTF8")

names(datos2) <- names(datos2)

names(datos2) <- names(datos2) |>
    gsub("\\{|\\}|\\[|\\]", "", x = _) |>
    gsub("^X_", "", x = _) |>
    gsub("\\.", "_", x = _) |>
    gsub("_V[0-9A-Za-z]+_", "_", x = _) |>
    gsub("__+", "_", x = _) |>
    gsub("^_|_$", "", x = _)


#ETIQUETAS DE VARIABLES---------------------------------------------------------
etiquetas_variables <- sapply(datos2, function(x) attr(x, "label"))
etiquetas_variables
etiquetas_variables <- iconv(etiquetas_variables, from = "", to = "UTF-8") #"ISO-8859-1"
# Convertir a data frame
tabla_etiquetas_variables <- data.frame(
    variable = names(etiquetas_variables),
    etiqueta = etiquetas_variables,
    row.names = NULL
)

tabla_etiquetas_variables <- tabla_etiquetas_variables %>%
    mutate_all(~ str_replace_all(., 
                                 c("ÃƒÂ¡" = "á", "ÃƒÂ©" = "é", "ÃƒÂ­" = "í", "ÃƒÂ³" = "ó", "ÃƒÃº" = "ú",
                                   "ÃƒÃ‰" = "É", "ÃƒÃ“" = "Ó", "ÃƒÃš" = "Ú","ã�"="ñ", "Ã³"="ó",
                                   "ÃƒÃ‘" = "Ñ","ÃƒÃ±" ="ñ","Ã‘"="Ñ", "ã¿" = "¿","Ã�" = "Ñ","ã¿" = "¿",
                                   "Â¿" ="¿"," Ã¿" = "¿","Ã‚¿"="¿","ãï¿½"="ñ"))) #, "" = "Í","" = "Á",

tabla_etiquetas_variables <- dplyr::rename(tabla_etiquetas_variables, "Variable"="variable")

tabla_etiquetas_variables

#quitamos : espacios y los cambiamos por _
tabla_etiquetas_variables <- tabla_etiquetas_variables %>%
    mutate(etiqueta_limpia = str_replace_all(etiqueta, ":\\s*", "_") %>%  
               str_replace_all("\\s+", "_"))   

write.xlsx(tabla_etiquetas_variables,"RESULTADOS/Etiquetas de variables.xlsx")
#ETIQUETAS VALORES--------------------------------------------------------------
etiquetas_valores <- sapply(datos2, function(var) attr(var, "labels"))

#FILTRAMOS VARIABLES CON ETIQUETAS
etiquetas_valores <- etiquetas_valores[!sapply(etiquetas_valores, is.null)]

#CONVERTIMOS LA LISTA EN DATA FRAME---------------------------------------------
df_etiquetas <- do.call(rbind, lapply(names(etiquetas_valores), function(var) {
    data.frame(
        Variable = var, 
        Valor = names(etiquetas_valores[[var]]), 
        Etiqueta = etiquetas_valores[[var]],
        stringsAsFactors = FALSE
    )
}))

df_etiquetas2 <- as.data.frame(df_etiquetas)

df_etiquetas2 <- df_etiquetas2 %>%
    mutate_all(~ str_replace_all(., 
                                 c("ÃƒÂ¡" = "á", "ÃƒÂ©" = "é", "ÃƒÂ­" = "í", "ÃƒÂ³" = "ó", "ÃƒÃº" = "ú",
                                   "ÃƒÃ‰" = "É", "ÃƒÃ“" = "Ó", "ÃƒÃš" = "Ú","Ã³" ="ó", "ã�"="ñ",
                                   "ÃƒÃ‘" = "Ñ","ÃƒÃ±" ="ñ","Ã‘"="Ñ", "ã¿" = "¿","Ã�" = "Ñ","ã¿" = "¿",
                                   "Â¿" ="¿"," Ã¿" = "¿","ãï¿½"="ñ",
                                   "Ð"="Ñ"))) #, "" = "Í","" = "Á"

#Quitamos (no leer) de toda la tabla
patrones_no_leer <- c(
    "\\(No leer\\)",
    "\\(No leer9\\)",
    "\\(NO LEER\\)",
    ",?\\s*\\(Especificar\\)"
)

df_etiquetas2[] <- lapply(df_etiquetas2, function(x) {
    if (is.character(x)) str_remove_all(x, regex(paste(patrones_no_leer, collapse = "|"), ignore_case = TRUE)) else x
})

#Quitamos (xxxx) de toda la tabla (para las preguntas abiertas)
df_etiquetas2[] <- lapply(df_etiquetas2, function(col) {
    if (is.character(col)) {
        gsub("\\(\\d{4}\\)\\s*", "", col)
    } else {
        col
    }
})

#QUITAR LOS NUMEROS A PARTIDOS
partidos_num <- c("PRI", "PAN", "PT", "PARTIDO VERDE", "MOVIMIENTO CIUDADANO", "MORENA")
patron <- paste0(".*?(", paste(partidos_num, collapse = "|"), ")$")

df_etiquetas2[] <- lapply(df_etiquetas2, function(col) {
    if (is.character(col)) {
        tiene <- grepl(paste(partidos_num, collapse="|"), col)
        col[tiene] <- trimws(gsub(patron, "\\1", col[tiene]))
        col
    } else {
        col
    }
})

#SUSTITUIR POR "NS / NC"
reemplazos_clas <- c(
    "Otra fecha, ¿cuál\\?"                   = "Otra fecha",
    "Otra, ¿cuál\\?"                         = "Otra fecha",
    "15 MOVIMIENO CIUDADANO"                 = "MOVIMIENTO CIUDADANO",
    "15 MOVIMIENO CIUDADANO"                 = "MOVIMIENTO CIUDADANO",
    "15 MOVIMIENTO CIUDADANO"                = "MOVIMIENTO CIUDADANO",
    "14 PARTIDO VERDE"                       = "PARTIDO VERDE",
    "contesto"                               = "contestó",
    "Otra, especificar"                      = "Otra",
    "No sabe / No recuerda"                  = "No sabe",
    "(?i)no sabe\\s*/\\s*no contest[óo]"     = "NS / NC",
    "(?i)no_sabe_no_contesto"                = "NS / NC",
    #"(?i)NS / NC \\(No leer\\)"              = "NS / NC",
    "(?i)Ns / nc"                            = "NS / NC",
    "(?i)No contesto"                        = "NS / NC",
    "(?i)NS / Nc"                            = "NS / NC",
    "(?i)^\\s*NS\\s*/\\s*Nc\\s*$"            = "NS / NC",
    "(?i)Su cercanía con CSP y el gobernador"= "Su cercanía con la presidenta y el gobernador",
    #"(?i)Nada/\\s*ninguno"                   = "Nada",
    #"(?i)NADA"                               = "Nada",
    "(?i)TODO"                               = "Todo",
    "(?i)OTRO"                               = "Otro",
    "(?i)REGULAR"                            = "Regular",
    # "(?i)Ninguno \\(No leer\\)"              = "Ninguno",
    # "(?i)Otro ?\\(No leer\\)"                = "Otro",
    # "(?i)Es indistinto \\(no leer\\)"        = "Es indistinto",
    # "(?i)REGULAR \\(NO LEER\\)"              = "Regular",
    "(?i)Continuar con la 4t"                = "Continuar con la 4T",
    "(?i)Su actitud y caracter"              = "Su actitud y carácter",
    "(?i)Programa (niños niñas )?madres trabajadoras" = "Programa madres trabajadoras",
    "(?i)Programa (para el Bienestar de)? Niñas y Niños Hijos de Madres Trabajadoras" = "Programa madres trabajadoras",
    "Alberto Gular Solózano" = "Alberto Gular Solórzano"
)

df_etiquetas2 <- df_etiquetas2 %>%
    mutate(across(everything(), ~ str_replace_all(.x, reemplazos_clas)))

#CREAMOS LIBRO DE EXCEL
wb <- createWorkbook()
addWorksheet(wb, "Diccionario")
writeData(wb, sheet = "Diccionario", df_etiquetas2)
saveWorkbook(wb, "RESULTADOS/DICCIONARIO.xlsx", overwrite = TRUE)

#**************************************-----------------------------------------

print("**********COPIA LAS ETIQUETAS CON (0 DEL DICCIONARIO**********")
#SUSTITUIR ETIQUETAS------------------------------------------
patrones <- c("Tengo ánimo", "Tengo confianza", "Tengo enojo", "Tengo esperanza", "Tengo preocupación", "Tengo temor", "la corrupcion","caracter",
              "educacion","economia","pais","economica",
              "Falta de seguridad -  VIGILANCIA","Narcotrafico -  NARCOMENUDEO","Empleado de la ip ",
              "60 o mas","publico","Corrupcion","Contaminacion","Desaparicion","Capaz","Vinculos","Egoista","Votaria","Nunca votaria","Debil",
              "alvarez","juarez","Rodriguez","jose", "baez","Ruiz","Martinez","sanchez","jesus","cortes",
              "davila", "diaz","perez","solis","ramirez", "agaton","lopez","leon","mexico","aprobacion","oposicion",
              "Tecnica","maestria","publicas","en Educación","Menos Corrupción","Mas seguridad","domestico",
              "izaguirre","alfaro","No incentiva Economía","Desapariciónes","problematicas","rita cetina","Jovenes construyendo",
              "Podria cambiar de opcion","para gober","2 o mas", "4 o mas",
              "gamez","dominguez","menendez","Ruben","Felix","Gonzalez","Sanchez","Garcia","Ns / Nc","pudiera cambiar de opcion",
              "Joséfina", "Avalos","aguilar","lilia","rivera","rivas","silva","Moron","ávila","avila","Alcazar","alcazar",
              "alanis","manzo","herrera","molina","torres","piña",
              "dominguez","gamez","berdegue","rocha","politica","cometio",
              "Berdegue","castro","palacios","zamora","de dios","Luevano",
              "hernandez","gutierrez","kantun", "de la torre","ravelo","gomez","damian","julian","cespedes",
              "veronica","mejia","saul","revolucion","del rio","Del Río","maria ","astiazaran",
              "fernandez","andres","cantu","adrian","de la garza","De la Garza",
              "Baez","hector","alvidrez","chavez","jimenez","santillan","No tener habilidad para gobernarnar",
              "â«feliferâ»","lucia","agundez","bugarin","ángel","Angel","Gonzales","Adriána","Anabelle",
              "Otro (No leer)","Ninguno (No leer)","Astiazaran","vazquez","ávalos","LEVANTATE","BUENA","MUY BUENA","MALA","MUY MALA",
              "Garcia", "TresPalacios", "Solózano"
)

reemplazos <- c("Ánimo", "Confianza", "Enojo", "Esperanza", "Preocupación", "Temor","la corrupción","carácter",
                "Educación","Economía","país", "económica",
                "Falta de seguridad / Vigilancia","Narcotráfico / Narcomenudeo","Empleado de la IP ",
                "60 o más","público","Corrupción","Contaminación","Desaparición","Capaz","Vínculos","Egoísta","Votaría","Nunca votaría","Débil",
                "Álvarez","Juárez","Rodríguez", "José","Báez","Ruíz", "Martínez","Sánchez","Jesús","Cortés", 
                "Dávila",  "Díaz",  "Pérez","Solís",  "Ramírez", "Agatón", "López","León","México","aprobación","oposición",
                "Técnica","Maestría","públicas","en educación","Menos corrupción","Más seguridad","doméstico",
                "Izaguirre","Alfaro","No incentiva economía","desapariciones","problemáticas","Rita Cetina","Jóvenes construyendo",
                "Podría cambiar de opción","para gobernar","2 o más", "4 o más",
                "Gámez","Domínguez","Menéndez","Rubén","Félix","González","Sánchez","García","NS / NC","Pudiera cambiar de opción",
                "Josefina","Ávalos","Aguilar","Lilia","Rivera","Rivas","Silva","Morón","Ávila","Ávila","Alcázar","Alcázar",
                "Alanís","Manzo","Herrera","Molina","Torres","Piña",
                "Domínguez","Gámez","Berdegué","Rocha","política","cometió",
                "Berdegué","Castro","Palacios","Zamora","de Dios","Luévano",
                "Hernández","Gutiérrez","Kantún","de la Torre","Rabelo","Gómez","Damián","Julián","Céspedes",
                "Verónica","Mejía","Saúl","REVOLUCIÓN","del Río","del Río","María ","Astiazarán",
                "Fernández","Andrés","Cantú","Adrián","de la Garza","de la Garza",
                "Báez","Héctor","Alvídrez","Chávez","Jiménez","Santillán","No tener habilidad para gobernar",
                "Felifer","Lucía","Agúndez","Bugarín","Ángel","Ángel","González","Adriana","Anabell",
                "Otro","Ninguno","Astiazarán","Vázquez","Ávalos","LEVÁNTATE","Buena","Muy buena","Mala","Muy mala",
                "García", "Trespalacios", "Solórzano"
)


#Función para aplicar múltiples reemplazos
reemplazar_texto <- function(texto) {
    for (i in seq_along(patrones)) {
        texto <- str_replace_all(texto, regex(patrones[i], ignore_case = TRUE), reemplazos[i])
    }
    return(texto)
}

# Aplicar la función a todas las columnas
df_etiquetas2 <- df_etiquetas2 %>%
    # mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), reemplazar_texto))

df_etiquetas2$Variable <- toupper(df_etiquetas2$Variable)

diccionario <- left_join(df_etiquetas2,tabla_etiquetas_variables, by="Variable")
diccionario <- dplyr::rename(diccionario,"Descripción" = "etiqueta", "Etiqueta" = "Valor", "Valor" = "Etiqueta")
diccionario <- dplyr::select(diccionario,Variable,Descripción,Valor,`Etiqueta`)
diccionario$Descripción[diccionario$Variable == "FORMULA_1_I"] <- "AMAI"

#DICCIONARIO FINAL--------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Diccionario")
writeData(wb, sheet = "Diccionario", diccionario)

#saveWorkbook(wb, "RESULTADOS/1.DICCIONARIO.xlsx", overwrite = TRUE)
#*******************************************----------------------------------------------------------------------------------------------------------
#*******************************************---------------------------------------------------------------------------------------------------------------------------------------------
#CARGAMOS BASE SAV------------------------------------------------------------------------------------------------------------------------------------
datos <- read.spss(archivo_sav,
                   to.data.frame = TRUE,reencode = "UTF-8")

#DATOS A FACTOR-----------------------------------
#CONVERTIMOS LOS DATOS A ETIQUETAS
datos_factor <- as_factor(datos)

#MUESTRA PRIMEROS DATOS PARA CORROBORAR EL CAMBIO
head(datos_factor)

datos_factor <- datos_factor %>%
    mutate_all(~ stringr::str_replace_all(., 
                                          c("ÃƒÂ¡" = "á", "ÃƒÂ©" = "é", "ÃƒÂ­" = "í", "ÃƒÂ³" = "ó", "ÃƒÃº" = "ú",
                                            "ÃƒÃ‰" = "É", "ÃƒÃ“" = "Ó", "ÃƒÃš" = "Ú","Ã³" ="ó", "ã�"="ñ",
                                            "ÃƒÃ‘" = "Ñ","ÃƒÃ±" ="ñ","Ã‘"="Ñ", "ã¿" = "¿","Ã�" = "Ñ","ã¿" = "¿",
                                            "Â¿" ="¿"," Ã¿" = "¿","ãï¿½"="ñ",
                                            "Ð"="Ñ"))) #, "" = "Í","" = "Á"

head(datos_factor)
#write.xlsx(datos_factor,"BASE_ROTATOR_ETIQUETAS_EN_VALORES.xlsx")

#Quitamos (xxxx) de toda la tabla (para las preguntas abiertas)
datos_factor[] <- lapply(datos_factor, function(col) {
    if (is.character(col)) {
        gsub("\\(\\d{4}\\)\\s*", "", col)
    } else {
        col
    }
})

#QUITAR LOS NUMEROS A PARTIDOS
datos_factor[] <- lapply(datos_factor, function(col) {
    if (is.character(col)) {
        tiene <- grepl(paste(partidos_num, collapse="|"), col)
        col[tiene] <- trimws(gsub(patron, "\\1", col[tiene]))
        col
    } else {
        col
    }
})

#para la palabra Nada
datos_factor[] <- lapply(datos_factor, function(col) {
    
    if (!is.character(col) && !is.factor(col)) return(col)
    
    x <- as.character(col)
    
    #1️⃣ Detectar SOLO "Nada"
    es_solo_nada <- grepl("^\\s*nada\\s*$", x, ignore.case = TRUE)
    
    #2️⃣ Detectar "Nada" al INICIO del texto
    es_inicio_nada <- grepl("^\\s*nada\\b", x, ignore.case = TRUE)
    
    #Forzar "Nada" cuando va sola o al inicio
    x[es_solo_nada | es_inicio_nada] <- sub(
        "^\\s*nada\\b",
        "Nada",
        x[es_solo_nada | es_inicio_nada],
        ignore.case = TRUE
    )
    
    #Convertir a "nada" cuando está en medio o al final
    x[!(es_solo_nada | es_inicio_nada)] <- gsub(
        "\\bNada\\b",
        "nada",
        x[!(es_solo_nada | es_inicio_nada)],
        ignore.case = TRUE
    )
    
    x
})

#Quitamos (no leer) de toda la tabla
datos_factor[] <- lapply(datos_factor, function(x) {
    if (is.character(x)) 
        str_remove_all(x, regex(paste(patrones_no_leer, collapse = "|"), ignore_case = TRUE)) 
    else x
})

#Aplicar la función a todas las columnas
datos_factor <- datos_factor %>%
    # mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), reemplazar_texto))

#SUSTITUIR REMPLAZOS
datos_factor <- datos_factor %>%
    mutate(across(everything(), ~ str_replace_all(.x, reemplazos_clas)))

#QUITAR ESPACIOS AL INICIO Y FINAL
datos_factor <- datos_factor %>%
    mutate(across(where(is.character), trimws))

#datos_factor <- rename(datos_factor,"Formula_1_I"="_AMAI_(Indicador)")
datos_factor$AMAI <- datos_factor$Formula_1_I

#++++++++++++++++++++++++++++++-------------------------------------------------
#++++++++++++++++++++++++++++++-------------------------------------------------
#sobremuestas es 1
# datos_factor$SOBREMUESTRA <- datos_factor$V1562
# datos_factor$SOBREMUESTRA[is.na(datos_factor$SOBREMUESTRA) |
#                             datos_factor$SOBREMUESTRA == "NANA"] <- 0
# datos_factor$SOBREMUESTRA[datos_factor$SOBREMUESTRA == "CONTINUAR"] <- 1
# table(datos_factor$SOBREMUESTRA)

##⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️⚠️--------------------
#SOBREMUESTRAS------------------------------------------------------------------
#+++ COMENTAR EL QUE NO SE UTILIZARÁ (SIEMPRE Y CUANDO HAYA SOMBREMUESTRA)
#datos_factor <- datos_factor[datos_factor$SOBREMUESTRA != 0, ] #para sobre muestra
#datos_factor <- datos_factor[datos_factor$SOBREMUESTRA == 0, ] #normal

# table(datos_factor$SOBREMUESTRA)
datos_factor$SO7_original <- datos_factor$SO7

RESPALDO <- datos_factor
#datos_factor <- RESPALDO
#++++++++++++++++++++++++++++++-------------------------------------------------
#++++++++++++++++++++++++++++++-------------------------------------------------

#PONDERACION AMAI------------------------------------------------------------------------------------------------------------------
pond_nal <- read.xlsx("32_POND_DTTF.xlsx") 
table(pond_nal$clave)
pond_nal <-  pond_nal[pond_nal$CVE_ENT_DTTF == distrito, ]
pond_nal <- dplyr::select(pond_nal, clave, porcen)

frec_gen_edad <- dplyr::select(datos_factor,SO8,SO7,Formula_1_I)
frec_gen_edad$SO7 <- gsub(" ", "", frec_gen_edad$SO7)
frec_gen_edad$SO7[frec_gen_edad$SO7%in%c("60omás")] <- "60 o más"
frec_gen_edad$Formula_1_I <- as.character(frec_gen_edad$Formula_1_I)

frec_gen_edad$Formula_1_I[!frec_gen_edad$Formula_1_I%in%c("A/B","C+")] <- "Otras"
frec_gen_edad$Formula_1_I[frec_gen_edad$Formula_1_I%in%c("A/B","C+")] <- "A/B+C+"

frec_gen_edad$SO7 <- car::recode(frec_gen_edad$SO7, "'18-19' = '18-29';
                                                     '20-29'='18-29';
                                                     '40-49'='40-59';
                                                     '50-59'='40-59'")

frec_gen_edad <-  as.data.frame(table(frec_gen_edad$SO7, frec_gen_edad$SO8, frec_gen_edad$Formula_1_I))

frec_tot <- sum(frec_gen_edad$Freq)
print(frec_tot)

frec_gen_edad <- frec_gen_edad %>%
    mutate(
        porcentaje = (Freq / frec_tot)
    )

frec_gen_edad$clave <- paste(frec_gen_edad$Var1, frec_gen_edad$Var2,frec_gen_edad$Var3, sep=" ")
frec_gen_edad <- dplyr::select(frec_gen_edad,clave,Freq,porcentaje)

#unimos datos para ponderador final
frec_gen_edad <- merge(frec_gen_edad,pond_nal, by="clave")

frec_gen_edad$pond <- ((frec_gen_edad$porcen)/100)/(frec_gen_edad$porcentaje)
frec_pond <- dplyr::select(frec_gen_edad,clave, pond)
frec_pond$clave <- gsub(" ", "", frec_pond$clave)

datos_factor$SO7 <- car::recode(datos_factor$SO7, "'18-19' = '18-29';
                                                     '20-29'='18-29';
                                                     '40-49'='40-59';
                                                     '50-59'='40-59'")

datos_factor$SO7 <- gsub(" ", "", datos_factor$SO7)
datos_factor$Formula_1_I <- as.character(datos_factor$Formula_1_I)
datos_factor$Formula_1_I[!datos_factor$Formula_1_I%in%c("A/B","C+")] <- "Otras"
datos_factor$Formula_1_I[datos_factor$Formula_1_I%in%c("A/B","C+")] <- "A/B+C+"
datos_factor$clave <- paste(datos_factor$SO7,datos_factor$SO8,datos_factor$Formula_1_I, sep = "")

datos_factor <- dplyr::select(datos_factor, clave,everything())

#▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄----------------------------
print("+++++++++++++++++VERIFICA QUE EL TOTAL DE TRUE+++++++++++++++++++++++++++")
table(datos_factor$clave %in% frec_pond$clave)#!!!!!!VERIFICAR EL TOTAL----------

datos_factor <- left_join(distinct(datos_factor),frec_pond, by = "clave")
datos_factor[is.na(datos_factor)] <- "NANA"
#GUARDAMOS LA BASE EN EXCEL-----------------------------------------------------
#!!sustitucion de nombres en columnas!!---------------------------------------------
nombres_finales <- c("clave",tabla_etiquetas_variables$etiqueta_limpia, "AMAI", "SO7_original", "pond")
d_factor2 <- datos_factor
colnames(d_factor2) <- nombres_finales

names(d_factor2) <- names(d_factor2)

names(d_factor2) <- names(d_factor2) |>
    gsub("\\{|\\}|\\[|\\]", "", x = _) |>
    gsub("^X_", "", x = _) |>
    gsub("\\.", "_", x = _) |>
    gsub("_V[0-9A-Za-z]+_", "_", x = _) |>
    gsub("__+", "_", x = _) |>
    gsub("^_|_$", "", x = _)

d_factor2$P4_PEOR_GOB_CSP[d_factor2$P4_PEOR_GOB_CSP == "MORENA"] <- "Cercana a AMLO / Ser de la 4T / MORENA"
d_factor2$`E_AÃO_ESTUDIO`
#write.xlsx(d_factor2,"RESULTADOS/2.BASE_PONDERADA.xlsx")
write.csv(d_factor2,"RESULTADOS/2.BASE_PONDERADA.csv",fileEncoding = "Latin1")

#CARGAMOS EXCEL DE MUESTRAS SISTEMATICA Y MARCO ESTADISTICO---------------------
sistematica <- read.xlsx("32_POND_DTTF.xlsx",sheet = "SISTEMATICA")

marco_est <- read.xlsx("AGEEML_2025326911543.xlsx",sheet = 2)

#MUN2 NOS SIRVE SOLO PARA PONER BIEN LOS NOMBRES
mun2 <- dplyr::select(marco_est,CLAVE_MUNICIPIO,NOM_MUN,NOM_ENT,NOM_LOC)
table(mun2$NOM_ENT)

mun2 <- mun2[mun2$NOM_ENT==estado, ] 
mun2
mun2_municipio <- mun2[!duplicated(mun2$CLAVE_MUNICIPIO), ]

mun1 <-dplyr::select(sistematica, CLAVE_MUNICIPIO,NOMBRE.ENTIDAD,DISTRITO_F,NOMBRE.MUNICIPIO,SECCION,TIPO_NOMBRE)
table(sistematica$NOMBRE.ENTIDAD)
mun1 <- mun1[mun1$NOMBRE.ENTIDAD== estado_mayus, ] 
mun1
mun1$Tipo_seccion <- car::recode(mun1$TIPO_NOMBRE, "'URBANO(A)' = 'Urbana';
                                            'RURAL'='Rural';
                                            'MIXTO(A)'='Mixta'")
#▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄-------------------------------------------------------------------------------------
#////////////////////////////////////////////////////////////////////////////////////////////----------------------------------------------------------------------------
#FUNCIONES**************************************************--------------------------------------------------------------------------------------------------------------
#VECTORES--------------------------------------------------------------------------------------------------------------------------------------
partidos <- c("PAN","PRI","PT","PARTIDO VERDE","MOVIMIENTO CIUDADANO","MORENA")

#VECTOR OTRO NS / NC NINGUNO--------------------
al_final <- c("Otro","Otra","Es secreto","Cualquiera","Ninguno","Ninguna","NINGUNO","Nada","Todo","Todos","No iría a votar","No los conoce","Todos / Cualquiera","No sabe","No contestó","NS / NC","NANA")
patron <- paste(al_final, collapse = "|")  #se convierte en "TOTAL|OTRO|NS/NR"

#SEPARAMOS VALORES ESPECIALES-------------------------------------------------------------------------------------------------------------------------------------
especiales <- c("Otro", "Otra","Es secreto","Cualquiera","Ninguno","Ninguna", "Nada","Todo","Todos",
                "No iría a votar", "No los conoce","Todos / Cualquiera","No sabe / No contestó", "No sabe", "No contestó")

especiales_oscuro <- c("AB","A/B","C+","C","C-","D+","D","D-","E",
                       "No completo primaria","No completó primaria","Primaria o secundaria","Primaria o Secundaria","Preparatoria o carrera técnica","Licenciatura","Posgrado")

#PARTIDOS FEDERALES-----------------------------------------------------------------------------------------------------------------------------------------------
funcion_partidos <- function(data,var,nombre_columna,frec_tot,orden_base,al_final) 
{
    
    tabla <- data %>%
        group_by({{ var }}) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    #Forzar que aparezcan todos los valores del orden_base aunque no existan
    tabla <- tabla %>%
        rename(tmp = 1) %>% 
        right_join(
            data.frame(tmp = orden_base, stringsAsFactors = FALSE),
            by = "tmp"
        ) %>%
        mutate(Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia))
    
    tabla <- tabla %>%
        mutate(Porcentaje = Frecuencia / frec_tot)
    
    tabla <- tabla[order(tabla$Frecuencia, decreasing = TRUE), ]
    
    names(tabla)[1] <- nombre_columna
    
    niveles_completos <- unique(c(orden_base, unique(tabla[[nombre_columna]])))
    
    tabla <- tabla %>%
        mutate(!!nombre_columna := factor(.data[[nombre_columna]], levels = niveles_completos)) %>%
        mutate(
            orden_final = match(toupper(as.character(.data[[nombre_columna]])), toupper(al_final)),
            orden_final = ifelse(is.na(orden_final), 0, orden_final)
        ) %>%
        arrange(orden_final, .data[[nombre_columna]]) %>%
        select(-orden_final)
    
    tabla <- tabla %>%
        mutate(
            Porcentaje_válido = Frecuencia / sum(Frecuencia)
        )
    
    total <- data.frame(
        tmp = "Total",
        Frecuencia = sum(tabla$Frecuencia),
        Porcentaje = sum(tabla$Porcentaje),
        Porcentaje_válido = sum(tabla$Porcentaje_válido)
    )
    
    names(total)[1] <- nombre_columna
    
    tabla_final <- rbind(tabla, total)
    
    return(tabla_final)
}

#APROBACION DESAPROBACION----------------------------------------------------------------------------------------------------------------------------------------------------
orden_aprob <- c("Aprueba","Ni aprueba, ni desaprueba","Desaprueba","No sabe","No contestó")

tabla_aprobacion <- function(data, var, nombre_columna, frec_tot, orden_aprob, excluir_NANA = FALSE) {
    
    library(dplyr)
    
    # Tabulado base
    tabla <- data %>%
        group_by({{ var }}) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    # Normalizar texto
    tabla[[1]] <- trimws(as.character(tabla[[1]]))
    
    # Renombrar columna
    names(tabla)[1] <- nombre_columna
    
    tabla <- tabla %>%
        rename(tmp = 1) %>%
        right_join(
            data.frame(tmp = orden_aprob, stringsAsFactors = FALSE),
            by = "tmp"
        ) %>%
        mutate(Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia))
    
    names(tabla)[1] <- nombre_columna
    
    tabla <- tabla %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot
        )
    
    tabla <- tabla %>%
        mutate(
            orden_tmp = match(.data[[nombre_columna]], orden_aprob)
        ) %>%
        arrange(orden_tmp) %>%
        select(-orden_tmp)
    
    if (excluir_NANA) {
        
        total_valido <- sum(tabla$Frecuencia[tabla[[nombre_columna]] != "NANA"])
        
        tabla <- tabla %>%
            mutate(
                Porcentaje_válido = ifelse(.data[[nombre_columna]] == "NANA", 0, Frecuencia / total_valido)
            )
        
    } else {
        
        tabla <- tabla %>%
            mutate(
                Porcentaje_válido = Frecuencia / sum(Frecuencia)
            )
    }
    
    total <- data.frame(
        tmp = "Total",
        Frecuencia = sum(tabla$Frecuencia),
        Porcentaje = sum(tabla$Porcentaje),
        Porcentaje_válido = sum(tabla$Porcentaje_válido)
    )
    
    names(total)[1] <- nombre_columna
    
    tabla_final <- rbind(tabla, total)
    
    return(tabla_final)
}


#CLASIFICACION MEJOR PEOR -----------------------------------------------------------------------------------------------------------------------------------------
procesar_clasificacion_gobernador <- function( data, variable, nombre_columna_final, al_final, nombre_objeto_final, frec_tot,ordenar_desc = TRUE) 
    
{
    
    clasif <- data %>%
        group_by(.data[[variable]]) %>% 
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    
    clasif[[1]][clasif[[1]] == "Ninguno nada"] <- "Nada"
    clasif[[1]][clasif[[1]] == "Nada/ Ninguno"] <- "Nada"
    clasif[[1]][clasif[[1]] == "Nada/ ninguno"] <- "Nada"
    clasif[[1]][clasif[[1]] == "Otra"] <- "Otro" 
    
    #Corrección adicional tipo P8
    clasif[[1]] <- sub("(?i)nada$", "nada", clasif[[1]])
    clasif[[1]][clasif[[1]] == "nada"] <- "Nada"
    
    clasif <- clasif %>%
        mutate(
            Porcentaje = (Frecuencia / frec_tot)
        )
    
    if (ordenar_desc) {
        clasif <- clasif[order(clasif$Frecuencia, decreasing = TRUE), ]
    }
    
    clasif <- clasif %>% mutate(across(1, trimws))
    clasif <- clasif %>% rename("{nombre_columna_final}" := 1)
    
    clasif_normales <- clasif %>%
        filter(!.data[[nombre_columna_final]] %in% al_final)
    
    clasif_finales <- clasif %>%
        filter(.data[[nombre_columna_final]] %in% al_final) %>%
        mutate(
            orden_final = match(.data[[nombre_columna_final]], al_final)
        ) %>%
        arrange(orden_final) %>%
        select(-orden_final)
    
    clasif <- bind_rows(clasif_normales, clasif_finales)
    
    clasif <- clasif %>%
        mutate(
            Porcentaje_válido = (Frecuencia / sum(Frecuencia))
        )
    
    tot <- data.frame(
        a = "Total",
        b = sum(clasif$Frecuencia),
        c = sum(clasif$Porcentaje),
        d = sum(clasif$Porcentaje_válido)
    )
    names(tot) <- names(clasif)
    
    salida <- rbind(clasif, tot)
    
    #salida[[nombre_columna_final]][salida[[nombre_columna_final]] %in% c("Ninguna","Ninguno")] <- "Nada"
    
    assign(nombre_objeto_final, salida, envir = .GlobalEnv)
    
    return(salida)
}

#TABLA POCOS------------------------------------------------------------------------------------------------------------------------------------------------
funcion_opciones <- function(data, var, nombre_columna, frec_tot, orden_respuestas, al_final,ordenar_desc = TRUE) 
{
    library(dplyr)
    
    tabla <- data %>%
        group_by({{ var }}) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    # Normalizar texto
    tabla[[1]] <- trimws(as.character(tabla[[1]]))
    
    # Renombrar columna
    names(tabla)[1] <- nombre_columna
    
    tabla <- tabla %>%
        rename(tmp = 1) %>%
        right_join(
            data.frame(tmp = orden_respuestas, stringsAsFactors = FALSE),
            by = "tmp"
        ) %>%
        mutate(Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia))
    
    names(tabla)[1] <- nombre_columna
    
    tabla <- tabla %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot
        )
    
    if (ordenar_desc) {
        tabla <- tabla %>%
            arrange(desc(Frecuencia))
    } else {
        tabla <- tabla %>%
            mutate(
                orden_tmp = match(.data[[nombre_columna]], orden_respuestas)
            ) %>%
            arrange(orden_tmp) %>%
            select(-orden_tmp)
    }
    
    total_valido <- sum(tabla$Frecuencia)
    
    tabla <- tabla %>%
        mutate(
            Porcentaje_válido = if (total_valido == 0) 0 else Frecuencia / total_valido
        )
    
    total <- data.frame(
        tmp = "Total",
        Frecuencia = sum(tabla$Frecuencia),
        Porcentaje = sum(tabla$Porcentaje),
        Porcentaje_válido = sum(tabla$Porcentaje_válido)
    )
    
    names(total)[1] <- nombre_columna
    
    tabla_final <- rbind(tabla, total)
    
    return(tabla_final)
}

#FUNCION REDES frec--------------------------------------------------------------------------------------------------------------------------------------------------
# tabla_redes_general <- function(data, vars, nombres_redes, niveles, peso = pond) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(rlang)
#   
#   peso_sym <- ensym(peso)
#   
#   resultados <- purrr::map2_dfr(vars, nombres_redes, function(v, nombre_red) {
#     
#     v_sym <- ensym(v)
#     
#     data %>%
#       mutate(tmp = as.character(!!v_sym)) %>%
#       group_by(tmp) %>%
#       summarise(Frecuencia = sum(!!peso_sym), .groups = "drop") %>%
#       complete(tmp = niveles, fill = list(Frecuencia = 0)) %>%
#       mutate(
#         RESPUESTA = tmp,
#         red = nombre_red
#       ) %>%
#       select(red, RESPUESTA, Frecuencia)
#   })
#   
#   resultados %>%
#     pivot_wider(
#       names_from = RESPUESTA,
#       values_from = Frecuencia,
#       values_fill = 0
#     )
# }

#FUNCION REDES PORCENTAJE--------------------------------------------------------------------------------------------------------------------------------------------------
tabla_redes_general <- function(data, vars, nombres_redes, niveles, peso = pond, frec_tot) {
    
    library(dplyr)
    library(tidyr)
    library(rlang)
    library(purrr)
    
    peso_sym <- ensym(peso)
    
    resultados <- purrr::map2_dfr(vars, nombres_redes, function(v, nombre_red) {
        
        v_sym <- ensym(v)
        
        data %>%
            mutate(tmp = as.character(!!v_sym)) %>%
            
            filter(!is.na(tmp), tmp != "NANA") %>%
            
            group_by(tmp) %>%
            summarise(Frecuencia = sum(!!peso_sym), .groups = "drop") %>%
            complete(tmp = niveles, fill = list(Frecuencia = 0)) %>%
            mutate(
                Porcentaje = Frecuencia / frec_tot,
                RESPUESTA = tmp,
                red = nombre_red
            ) %>%
            select(red, RESPUESTA, Porcentaje)
    })
    
    resultados %>%
        pivot_wider(
            names_from = RESPUESTA,
            values_from = Porcentaje,
            values_fill = 0
        )
}

#FUNCION RED USA--------------------------------------------------------------------------------------------------------------------------------
tabla_redes_usa <- function(data, vars, nombres_redes, niveles, peso = pond) {
    
    library(dplyr)
    library(tidyr)
    library(rlang)
    library(purrr)
    
    peso_sym <- ensym(peso)
    
    resultados <- purrr::map2_dfr(vars, nombres_redes, function(v, nombre_red) {
        
        v_sym <- ensym(v)
        
        data %>%
            mutate(tmp = as.character(!!v_sym)) %>%
            
            # 🔥 Quitar NANA
            filter(!is.na(tmp), tmp != "NANA") %>%
            
            group_by(tmp) %>%
            summarise(Frecuencia = sum(!!peso_sym), .groups = "drop") %>%
            
            complete(tmp = niveles, fill = list(Frecuencia = 0)) %>%
            
            mutate(
                Total_red = sum(Frecuencia),
                Porcentaje = if_else(Total_red > 0, Frecuencia / Total_red, 0),
                RESPUESTA = tmp,
                red = nombre_red
            ) %>%
            select(red, RESPUESTA, Porcentaje)
    })
    
    resultados %>%
        pivot_wider(
            names_from = RESPUESTA,
            values_from = Porcentaje,
            values_fill = 0
        )
}

#QUITAR NANA Y TOTAL-------------------------------------------------------------------------------------------------------------------------------
limpiar_tabla <- function(df) {
    col1 <- names(df)[1]
    
    df %>%
        dplyr::filter(!.data[[col1]] %in% c("Total", "NANA"))
}
#////////////////////////////////////////////////////////////////////////////////////////////----------------------------------------------------------------------------

#********************************************--------------------------------------------------------------------------------------------
#TABULADOS-------------------------------------------------------------------------------------------------------------------------------
#1.Tabulado municipio------------------------------------------------------------------------------------------------------------------
municipio_secc <- datos_factor %>%
    group_by(V1454) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop")

municipio_secc <- municipio_secc %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot) )

#generamos variable municipio desde variable V1454
municipio_secc$Municipio <- sub("^(.*?)\\s*-.*$", "\\1", municipio_secc$V1454) #quitar guión

municipio <- municipio_secc %>%
    group_by(Municipio) %>%
    summarise(Frecuencia = sum(Frecuencia), .groups = "drop")

print(sum(municipio$Frecuencia))

municipio <- municipio %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

municipio$Porcentaje_válido <- municipio$Porcentaje

#ponemos nombre con acentos
#Función para normalizar texto (quitar acentos y pasar a minúsculas)
normalizar <- function(x) {
    x %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        tolower() %>%
        trimws()
}
#Normalizar solo la columna Municipio (para la comparación)
municipio$clave_normalizada <- normalizar(municipio$Municipio)
#para NOM_MUN
mun2_municipio$clave_normalizada <- normalizar(mun2_municipio$NOM_MUN)
#match municipio (son los municipios de la muestra) vs mun2 (municipios marco estadistico AGEEML)
municipio <- municipio %>%
    left_join(mun2_municipio %>%
                  dplyr::select(NOM_MUN, clave_normalizada), 
              by = "clave_normalizada") %>%
    mutate(Municipio = ifelse(!is.na(NOM_MUN), NOM_MUN, Municipio)) %>%
    dplyr::select(,-NOM_MUN)  

tot_mun <- data.frame(
    Municipio = "Total",
    Frecuencia = sum(municipio$Frecuencia),
    Porcentaje = sum(municipio$Porcentaje),
    Porcentaje_válido = sum(municipio$Porcentaje_válido)
)

municipio_sin_norm <- dplyr::select(municipio,-clave_normalizada)
T1_municipio <- rbind(municipio_sin_norm, tot_mun)
print(T1_municipio)

#2.Tabulado tipo sección---------------------------------------------------------------------------------------------------------------------
municipio_secciones <-  municipio_secc %>%
    mutate(SECCION = as.numeric(str_trim(str_extract(V1454, "(?<=-)\\s*.+"))))

tipo_seccion <- left_join(municipio_secciones, mun1, by ="SECCION") #solo cuando las claves de municipios sean iguales en mabas bases

tipo_seccion <- tipo_seccion %>%
    group_by(Tipo_seccion) %>%
    summarise(Frecuencia = sum(Frecuencia), .groups = "drop")

tipo_seccion <- tipo_seccion %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

tipo_seccion$Porcentaje_válido <- (tipo_seccion$Porcentaje)
#tipo_seccion$`Porcentaje_válido` <- paste0(sprintf("%.1f", tipo_seccion$Porcentaje * 100), "%")

tipo_seccion <- tipo_seccion %>%
    mutate(Tipo_seccion = factor(Tipo_seccion, levels = c("Urbana", "Rural", "Mixta"))) %>%
    arrange(Tipo_seccion)

print(tipo_seccion)

tot_secc <- data.frame(
    Tipo_seccion = "Total",
    Frecuencia = sum(tipo_seccion$Frecuencia),
    Porcentaje = sum(tipo_seccion$Porcentaje),
    Porcentaje_válido = sum(tipo_seccion$Porcentaje_válido)
)

T2_tipo_seccion <- rbind(tipo_seccion, tot_secc)

T2_tipo_seccion

#▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄----------------------------------------------------------------------------------------------
#5.tabulado género----------------------------------------------------------------------------------------------------------------------------
edad_gen <- datos_factor %>%
    group_by(SO7_original, SO8) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop") #hacemos la suma de ponderadores 

edad_gen <- edad_gen[order(edad_gen$SO8), ]

edad_gen <- edad_gen %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

print(edad_gen)

gen <- dcast(edad_gen,SO8 ~ SO7_original)
gen$Porcentaje <- rowSums(gen[ , 2:ncol(gen)])
gen <- dplyr::select(gen,SO8,Porcentaje)
gen$Porcentaje_válido <- gen$Porcentaje
#gen$`Porcentaje_válido` <- paste0(sprintf("%.1f", gen$Porcentaje * 100), "%")

gen2 <-dcast(edad_gen, SO7_original~ SO8,value.var = "Frecuencia")
m <- (sum(gen2$Mujer))
h <- (sum(gen2$Hombre))

T5_genero <- gen 
T5_genero$Frecuencia <- c(h,m)
T5_genero <- dplyr::select(T5_genero,SO8,Frecuencia,Porcentaje,`Porcentaje_válido`)
T5_genero <- T5_genero[c(2, 1), ]
T5_genero <- rename(T5_genero,"Género"="SO8")

tot_gen <- data.frame(
    Género = "Total",
    Frecuencia = sum(T5_genero$Frecuencia),
    Porcentaje = sum(T5_genero$Porcentaje),
    Porcentaje_válido = sum(T5_genero$`Porcentaje_válido`)
)

T5_genero <- rbind(T5_genero, tot_gen)
T5_genero #!!!!!!!REVISAR EL PORCENTAJE DE CADA EDO--------------

#3.Tabulado edad------------------------------------------------------------------------------------------------------------------------
e1 <- dcast(edad_gen,SO7_original ~ SO8)
e1$Porcentaje <- e1$Hombre+ e1$Mujer #**
e1 <- dplyr::select(e1,-Hombre,-Mujer)

e2 <- dcast(edad_gen ,SO7_original ~ SO8, value.var = "Frecuencia")
e2$Frecuencia <- e2$Hombre+e2$Mujer
e2 <- dplyr::select(e2,-Hombre,-Mujer)

edad <- merge(e2,e1)
edad$Porcentaje_válido <- edad$Porcentaje
edad <- rename(edad,"Edad"="SO7_original")
T3_edad <- edad

tot_ed <- data.frame(
    Edad = "Total",
    Frecuencia = sum(T3_edad$Frecuencia),
    Porcentaje = sum(T3_edad$Porcentaje),
    Porcentaje_válido =sum(T3_edad$`Porcentaje_válido`)
)
T3_edad <- rbind(T3_edad, tot_ed,fill=T)
T3_edad

#4.Tabulado ocupacion-------------------------------------------------------------------------------------------------------------------
ocupacion <- datos_factor %>%
    group_by(SO9) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop")

ocupacion <- ocupacion %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

ocupacion <- ocupacion[order(ocupacion$Frecuencia,decreasing=TRUE), ]
ocupacion$SO9 <- trimws(ocupacion$SO9)
ocupacion <- rename(ocupacion, "Ocupación"="SO9")

# Separar filas: las que NO contienen esas palabras y las que SÍ
ocupacion <- ocupacion %>%
    mutate(
        orden_final = match(toupper(Ocupación), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final) %>%
    dplyr::select(-orden_final)

ocupacion$Porcentaje_válido <- ocupacion$Porcentaje

tot_ocup <- data.frame(
    Ocupación = "Total",
    Frecuencia = sum(ocupacion$Frecuencia),
    Porcentaje = sum(ocupacion$Porcentaje),
    Porcentaje_válido = sum(ocupacion$Porcentaje_válido)
)

T4_ocupacion <- rbind(ocupacion, tot_ocup)
print(T4_ocupacion)

#P1 Identifica partido -----------------------------------------------------------------------------------------------------------------------------
#orden_iden <- c(partidos,"Otro","Es secreto","Cualquiera","Ninguno","No sabe","No contestó")

T5_P1_ident_part <- procesar_clasificacion_gobernador(datos_factor,"V2924",
                                                      "P1.Identifica_partido",
                                                      al_final,"T5_P1_ident_part", frec_tot
)

T5_P1_ident_part 

#P2 Aprobación presidenta -------------------------------------------------------------------------------------------------------
T6_P2_aprob_pdta <- tabla_aprobacion(datos_factor, V1751, 
                                     "P2.Aprobación_presidenta", 
                                     frec_tot,orden_aprob, excluir_NANA = TRUE
)

T6_P2_aprob_pdta

#P3 Clas mejor gob presidenta-----------------------------------------------------------------------------------------
T7_P3_clas_gob_pdta_mejor <- procesar_clasificacion_gobernador(datos_factor, "V1752",
                                                               "P3.Clasificacion_gob_presidenta_mejor",
                                                               al_final, "T7_P3_clas_gob_pdta_mejor",frec_tot
)

T7_P3_clas_gob_pdta_mejor

#P4 Clas peor gob presidenta ---------------------------------------------------------------------------------------------
T8_P4_clas_gob_pdta_peor <- procesar_clasificacion_gobernador(datos_factor, "V1754",
                                                              "P4.Clasificacion_gob_presidenta_peor",
                                                              al_final, "T8_P4_clas_gob_pdta_peor", frec_tot
)
T8_P4_clas_gob_pdta_peor

#P5 Aprobación gobernador(a) --------------------------------------------------------------------------------------------------
T9_P5_Aprobación_gobernador <- tabla_aprobacion(datos_factor, V5128,
                                                "P5.Aprobación_gobernador",
                                                frec_tot, orden_aprob,excluir_NANA = FALSE
)

T9_P5_Aprobación_gobernador

#P6 Clasificación mejor gobernador(a) -----------------------------------------------------------------------------------------
T10_P6_clasificación_de_gobierno <- procesar_clasificacion_gobernador(datos_factor, "V1767",
                                                                      "P6.Clasificación_mejor_gobernador",
                                                                      al_final, "T10_P6_clasificación_de_gobierno",frec_tot
)

T10_P6_clasificación_de_gobierno

#P7 Clasificación peor gobernador(a) ---------------------------------------------------------------------------------------------
T11_P7_Clasificación_peor_de_gobierno <- procesar_clasificacion_gobernador(datos_factor, "V1768",
                                                                           "P7.Clasificación_peor_de_gobierno",
                                                                           al_final, "T11_P7_Clasificación_peor_de_gobierno", frec_tot
)

T11_P7_Clasificación_peor_de_gobierno

#P8 Aprobación P municipal 1 ------------------------------------------------------------------------------------------------------------------------------------
T12_P8_aprob_P_municipal <- tabla_aprobacion(datos_factor, V1769,
                                             "P8.Aprobacion_P_municipal",
                                             frec_tot, orden_aprob,excluir_NANA = FALSE
)

T12_P8_aprob_P_municipal

#P9 Clasificación mejor municipal -----------------------------------------------------------------------------------------
T13_P9_mejor_clasificación_mun <- procesar_clasificacion_gobernador(datos_factor, "V1770",
                                                                    "P9.Clasificación_mejor_mun",
                                                                    al_final, "T13_P9_mejor_clasificación_mun",frec_tot
)

T13_P9_mejor_clasificación_mun

#P10 Clasificación peor municipal ---------------------------------------------------------------------------------------------
T14_P10_peor_Clasificación_mun <- procesar_clasificacion_gobernador(datos_factor, "V1771",
                                                                    "P10.Clasificación_peor_mun",
                                                                    al_final, "T14_P10_peor_Clasificación_mun", frec_tot
)

T14_P10_peor_Clasificación_mun

#P11 Partido interes en familia -----------------------------------------------------------------------------------------------------------------------------
#orden_p18 <- c(partidos,"Otro","Es secreto","Cualquiera","Ninguno","No sabe")

T15_P11_partido_interes_familia <- procesar_clasificacion_gobernador(datos_factor,"V1773",
                                                                     "P11.Partido_interes_familia",
                                                                     al_final,"T15_P11_partido_interes_familia", frec_tot
)

T15_P11_partido_interes_familia

#P12 Principal problema---------------------------------------------------------------------------------------------------------
datos_factor$V3519[datos_factor$V3519=="Narcotráfico y crimen organizado" ] <- "Inseguridad / violencia" 
datos_factor$V3519[datos_factor$V3519=="Narcotráfico / Crimen Organizado" ] <- "Inseguridad / violencia" 
datos_factor$V3519[datos_factor$V3519=="Narcotráfico / crimen organizado" ] <- "Inseguridad / violencia" 
datos_factor$V3519[datos_factor$V3519=="Otra" ] <- "Otro" 

princ_problema <- datos_factor %>%
    group_by(V3519) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop")

princ_problema <- princ_problema %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

princ_problema <- princ_problema[order(princ_problema$Frecuencia,decreasing=TRUE), ]

princ_problema <- princ_problema %>% mutate(across(1, trimws))
princ_problema <- princ_problema %>% rename("P12.Principal_problema_edo" = 1)

# Separar filas: las que NO contienen esas palabras y las que SÍ
princ_problema <- princ_problema %>%
    mutate(
        orden_final = match(toupper(P12.Principal_problema_edo), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final) %>%
    dplyr::select(-orden_final)

princ_problema$Porcentaje_válido <- (princ_problema$Porcentaje)
#princ_problema$Porcentaje_válido <- paste0(sprintf("%.1f", princ_problema$Porcentaje * 100), "%")

tot_prin_prob <- data.frame(
    P12.Principal_problema_edo = "Total",
    Frecuencia = sum(princ_problema$Frecuencia),
    Porcentaje = sum(princ_problema$Porcentaje),
    Porcentaje_válido = sum(princ_problema$Porcentaje_válido)
)

T16_P12_Principal_problema_edo <- rbind(princ_problema, tot_prin_prob)
T16_P12_Principal_problema_edo

#++++++++++++++++++++++++++++++++-----------------------------------------------------------------------------------------------------------------------------------------
#++++++++++++++++++++++++++++++++-----------------------------------------------------------------------------------------------------------------------------------------
#🦰CAREOS PERSONAJES🦰-------------------------------------------------------------------------------------------------------------------------------------
#🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️🧍‍♂️--------
#<-----VECTOR PERSONAJES------>----------------------------------

personajes <- c("Luis Lauro Reyes Rodríguez","Mónica Villarreal Anaya","Adrián Oseguera Kernion","Jesús Antonio Nader Nasrallah","José Abdo Schekaiban ",
                "Rosa María González Azcárraga")

#✅ CONOCE++++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
orden_conoce <- c("Sí","No")

procesar_conoce_actor <- function(data, var, nombre, frec_tot, al_final, orden_conoce) {
    
    library(dplyr)
    library(rlang)
    
    var_sym <- sym(var)   # convierte "V5136" -> símbolo
    
    # 1) Recodificar a Sí / No
    data <- data %>%
        mutate(
            !!var_sym := case_when(
                grepl("sí", .data[[var]], ignore.case = TRUE) ~ "Sí",
                grepl("^no", .data[[var]], ignore.case = TRUE) ~ "No",
                TRUE ~ "NANA"
            )
        )
    
    # 2) Agrupar y resumir
    df <- data %>%
        group_by(!!var_sym) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop") %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot
        )
    
    # 3) Renombrar columna de respuestas
    names(df)[1] <- nombre
    
    # 4) Ordenar
    df <- df %>%
        mutate(
            orden_final = match(toupper(.data[[nombre]]), toupper(al_final)),
            orden_final = ifelse(is.na(orden_final), -1, orden_final)
        ) %>%
        arrange(match(.data[[nombre]], orden_conoce)) %>%
        select(-orden_final) %>%
        mutate(
            Porcentaje_válido = Frecuencia / sum(Frecuencia)
        )
    
    # 5) Fila total
    tot <- data.frame(
        tmp = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    names(tot)[1] <- nombre
    
    # 6) Unir
    bind_rows(df, tot)
}


T67.11_conoce_actor_1   <- procesar_conoce_actor(datos_factor, "V5136", "conoce_actor_1", frec_tot, al_final, orden_conoce)
T67.12_conoce_actor_2   <- procesar_conoce_actor(datos_factor, "V5137", "conoce_actor_2", frec_tot, al_final, orden_conoce)
T67.13_conoce_actor_3   <- procesar_conoce_actor(datos_factor, "V5138", "conoce_actor_3", frec_tot, al_final, orden_conoce)
T67.14_conoce_actor_4   <- procesar_conoce_actor(datos_factor, "V5139", "conoce_actor_4", frec_tot, al_final, orden_conoce)
T67.15_conoce_actor_5   <- procesar_conoce_actor(datos_factor, "V5140", "conoce_actor_5", frec_tot, al_final, orden_conoce)
T67.16_conoce_actor_6   <- procesar_conoce_actor(datos_factor, "V5141", "conoce_actor_6", frec_tot, al_final, orden_conoce)
# T67.17_conoce_actor_7   <- procesar_conoce_actor(datos_factor, "V5142", "conoce_actor_7", frec_tot, al_final, orden_conoce)
# T67.18_conoce_actor_8   <- procesar_conoce_actor(datos_factor, "V5143", "conoce_actor_8", frec_tot, al_final, orden_conoce)
# T67.19_conoce_actor_9   <- procesar_conoce_actor(datos_factor, "V5144", "conoce_actor_9", frec_tot, al_final, orden_conoce)
# T67.110_conoce_actor_10 <- procesar_conoce_actor(datos_factor, "V4978", "conoce_actor_10", frec_tot, al_final, orden_conoce)
# T67.111_conoce_actor_11 <- procesar_conoce_actor(datos_factor, "V4181", "conoce_actor_11", frec_tot, al_final, orden_conoce)
# T67.112_conoce_actor_12 <- procesar_conoce_actor(datos_factor, "V4182", "conoce_actor_12", frec_tot, al_final, orden_conoce)
# T67.113_conoce_actor_13 <- procesar_conoce_actor(datos_factor, "V4325", "conoce_actor_13", frec_tot, al_final, orden_conoce)
# T67.114_conoce_actor_14 <- procesar_conoce_actor(datos_factor, "V4335", "conoce_actor_14", frec_tot, al_final, orden_conoce)

#✅ OPINION+++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
procesar_opinion <- function(data, var, frec_tot, nombre_salida) {
    orden_opinion <- c("Buena", "Regular", "Mala", "NS / NC", "NANA")
    
    # Agrupar y sumar
    df <- data %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    columna <- names(df)[1]
    
    # Agregar filas faltantes
    faltan <- setdiff(orden_opinion, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(
            df,
            data.frame(setNames(list(faltan), columna), Frecuencia = 0)
        )
    }
    
    # Ordenar según vector
    df <- df[match(orden_opinion, df[[columna]]), ]
    
    # Calcular porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot,
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si la última fila es NANA, poner 0 en porcentaje válido
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0
    }
    
    # Renombrar la columna de la variable
    names(df)[1] <- nombre_salida
    
    # Agregar fila total
    total <- data.frame(
        setNames(list("Total"), nombre_salida),
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    # Unir con la tabla
    rbind(df, total)
}

T68.11_opinion_actor_1  <- procesar_opinion(datos_factor, "V5170", frec_tot, "Opinion_actor_1")
T68.12_opinion_actor_2  <- procesar_opinion(datos_factor, "V5171", frec_tot, "Opinion_actor_2")
T68.13_opinion_actor_3  <- procesar_opinion(datos_factor, "V5172", frec_tot, "Opinion_actor_3")
T68.14_opinion_actor_4  <- procesar_opinion(datos_factor, "V5173", frec_tot, "Opinion_actor_4")
T68.15_opinion_actor_5  <- procesar_opinion(datos_factor, "V5174", frec_tot, "Opinion_actor_5")
T68.16_opinion_actor_6  <- procesar_opinion(datos_factor, "V5175", frec_tot, "Opinion_actor_6")
# T68.17_opinion_actor_7  <- procesar_opinion(datos_factor, "V5176", frec_tot, "Opinion_actor_7")
# T68.18_opinion_actor_8  <- procesar_opinion(datos_factor, "V5177", frec_tot, "Opinion_actor_8")
# T68.19_opinion_actor_9  <- procesar_opinion(datos_factor, "V5178", frec_tot, "Opinion_actor_9")
# T68.110_opinion_actor_10 <- procesar_opinion(datos_factor, "V4981", frec_tot, "Opinion_actor_10")
# T68.111_opinion_actor_11 <- procesar_opinion(datos_factor, "V4193", frec_tot, "Opinion_actor_11")
# T68.112_opinion_actor_12 <- procesar_opinion(datos_factor, "V4194", frec_tot, "Opinion_actor_12")
# T68.113_opinion_actor_13 <- procesar_opinion(datos_factor, "V4326", frec_tot, "Opinion_actor_13")
# T68.114_opinion_actor_14 <- procesar_opinion(datos_factor, "V4336", frec_tot, "Opinion_actor_14")

#T67. Conocimiento y opinión de actores-----------------------------------------------------------------------------------------------------------------------------------
#conocimiento actores-------------------------------------------------------------------------------------------------------------------------------------------------------
conocimiento_actores <- data.frame(
    names <- c("Sí","No","Total"),
    lista_p4<- list( (as.numeric(T67.11_conoce_actor_1$Porcentaje_válido)),
                     (as.numeric(T67.12_conoce_actor_2$Porcentaje_válido)),
                     (as.numeric(T67.13_conoce_actor_3$Porcentaje_válido)),
                     (as.numeric(T67.14_conoce_actor_4$Porcentaje_válido)),
                     (as.numeric( T67.15_conoce_actor_5$Porcentaje_válido)),
                     (as.numeric(T67.16_conoce_actor_6$Porcentaje_válido))
                     # (as.numeric(T67.17_conoce_actor_7$Porcentaje_válido)),
                     # (as.numeric( T67.18_conoce_actor_8$Porcentaje_válido)),
                     # (as.numeric( T67.19_conoce_actor_9$Porcentaje_válido))
                     # (as.numeric( T67.110_conoce_actor_10$Porcentaje_válido))
                     # (as.numeric( T67.111_conoce_actor_11$Porcentaje_válido)),
                     # (as.numeric( T67.112_conoce_actor_12$Porcentaje_válido))
                     # (as.numeric( T67.113_conoce_actor_13$Porcentaje_válido)),
                     # (as.numeric( T67.114_conoce_actor_14$Porcentaje_válido))
    )
)

# conocimiento_actores <- ls(pattern = "^T67.\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------
# conocimiento_actores
# conocimiento_actores <- do.call(rbind, mget(conocimiento_actores))

conocimiento_actores <- as.data.frame(t(conocimiento_actores))
conocimiento_actores <- cbind(Personaje = rownames(conocimiento_actores), conocimiento_actores)

conocimiento_actores$Personaje <- c("Personaje",personajes)
conocimiento_actores <- dplyr::select(conocimiento_actores, Personaje, everything(),-ncol(conocimiento_actores))
rownames(conocimiento_actores) <- NULL
colnames(conocimiento_actores) <- as.character(conocimiento_actores[1, ])
conocimiento_actores <- conocimiento_actores[-1, ]
conocimiento_actores <- conocimiento_actores[ ,c(1:2)]

conocimiento_actores <- rename(conocimiento_actores,`Sí lo conoce`="Sí")

#opinion actores-------------------------------------------------------------------------------------------------------------------

opinion_actores <- data.frame(
    names <- c("Buena","Regular","Mala","NS / NC","NANA","Total"),
    lista_p4<- list( (as.numeric(T68.11_opinion_actor_1$Porcentaje)),
                     (as.numeric(T68.12_opinion_actor_2$Porcentaje)),
                     (as.numeric(T68.13_opinion_actor_3$Porcentaje)),
                     (as.numeric(T68.14_opinion_actor_4$Porcentaje)),
                     (as.numeric( T68.15_opinion_actor_5$Porcentaje)),
                     (as.numeric(T68.16_opinion_actor_6$Porcentaje))
                     # (as.numeric(T68.17_opinion_actor_7$Porcentaje)),
                     # (as.numeric( T68.18_opinion_actor_8$Porcentaje)), #REVISARRRRR CUANTOS SON!!!!!!
                     # (as.numeric( T68.19_opinion_actor_9$Porcentaje))
                     # (as.numeric( T68.110_opinion_actor_10$Porcentaje))
                     # (as.numeric( T68.111_opinion_actor_11$Porcentaje)),
                     # (as.numeric( T68.112_opinion_actor_12$Porcentaje))
                     # (as.numeric( T68.113_opinion_actor_13$Porcentaje)),
                     # (as.numeric( T68.114_opinion_actor_14$Porcentaje))
    )
)

opinion_actores <- as.data.frame(t(opinion_actores))
opinion_actores <- cbind(Personaje = rownames(opinion_actores), opinion_actores)

opinion_actores$Personaje <- c("Personaje",personajes)
opinion_actores <- dplyr::select(opinion_actores, Personaje, everything(),-ncol(opinion_actores))
rownames(opinion_actores) <- NULL
colnames(opinion_actores) <- as.character(opinion_actores[1, ])
opinion_actores <- opinion_actores[-1, ]
opinion_actores <- opinion_actores[ ,c(1:5)]

T67_actores <- left_join(conocimiento_actores,opinion_actores)

#reemplazos para acentos y demás
T67_actores <- T67_actores %>%
    mutate(across(1, reemplazar_texto))

T67_actores[ , 2:ncol(T67_actores)] <- lapply(T67_actores[ , 2:ncol(T67_actores)], function(x) as.numeric(as.character(x)))

T67_actores

T67_actores_nueva <- T67_actores

#🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA SALDO-🛑-🛑-🛑------------------------------------
T67_actores_nueva$saldo <- T67_actores_nueva$Buena - T67_actores_nueva$Mala

T67_actores_nueva

T67_actores[[1]][T67_actores[[1]] == "Adriána DÁvila"] <- "Adriana Dávila"
T67_actores[[1]][T67_actores[[1]] == "Adriana DÁvila"] <- "Adriana Dávila"
T67_actores[[1]][T67_actores[[1]] == "Adriána DÁvila Fernández"] <- "Adriana Dávila Fernández"
T67_actores[[1]][T67_actores[[1]] == "Adriana DÁvila Fernández"] <- "Adriana Dávila Fernández"


#✅ RELACIONA++++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
relaciona_personaje <- function(datos, var, personaje, al_final, frec_tot) {
    
    tabla <- datos %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop") %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot
        ) %>%
        arrange(desc(Frecuencia))
    
    # Renombrar columna principal con el nombre del personaje
    names(tabla)[1] <- personaje
    
    # Reordenar filas con base en al_final
    tabla <- tabla %>%
        mutate(
            orden_final = across(1, ~ match(toupper(.), toupper(al_final))) |> unlist(),
            orden_final = ifelse(is.na(orden_final), -1, orden_final)
        ) %>%
        arrange(orden_final) %>%
        dplyr::select(-orden_final)
    
    # Calcular Porcentaje válido (sin contar NANA)
    tabla <- tabla %>%
        mutate(
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si la última fila es NANA, poner Porcentaje_válido = 0
    if (tabla[nrow(tabla), 1] == "NANA") {
        tabla$Porcentaje_válido[nrow(tabla)] <- 0.0
    }
    
    # Agregar totales
    tot <- data.frame(
        Personaje = "Total",
        Frecuencia = sum(tabla$Frecuencia),
        Porcentaje = sum(tabla$Porcentaje),
        Porcentaje_válido = sum(tabla$Porcentaje_válido)
    )
    
    names(tot)[1] <- personaje
    
    tabla_final <- rbind(tabla, tot) %>%
        mutate(Personaje = personaje) %>%
        dplyr::select(Personaje, everything())
    
    return(tabla_final)
}

T69.11_partido_relaciona_personaje_1 <- relaciona_personaje(datos_factor, "V5180", personajes[1], al_final, frec_tot)
T69.12_partido_relaciona_personaje_2 <- relaciona_personaje(datos_factor, "V5181", personajes[2], al_final, frec_tot)
T69.13_partido_relaciona_personaje_3 <- relaciona_personaje(datos_factor, "V5182", personajes[3], al_final, frec_tot)
# T69.14_partido_relaciona_personaje_4 <- relaciona_personaje(datos_factor, "V5183", personajes[4], al_final, frec_tot)
# T69.15_partido_relaciona_personaje_5 <- relaciona_personaje(datos_factor, "V5184", personajes[5], al_final, frec_tot)
# T69.16_partido_relaciona_personaje_6 <- relaciona_personaje(datos_factor, "V5185", personajes[6], al_final, frec_tot)
# T69.17_partido_relaciona_personaje_7 <- relaciona_personaje(datos_factor, "V4201", personajes[7], al_final, frec_tot)
# T69.18_partido_relaciona_personaje_8 <- relaciona_personaje(datos_factor, "V4202", personajes[8], al_final, frec_tot)
# T69.19_partido_relaciona_personaje_9 <- relaciona_personaje(datos_factor, "V4203", personajes[9], al_final, frec_tot)

#T70. partido con que se relaciona el personaje---------------------------------------------------------------------------------------------------------
max_personaje_1 <- T69.11_partido_relaciona_personaje_1[1, ]
max_personaje_2 <- T69.12_partido_relaciona_personaje_2[1, ]
max_personaje_3 <- T69.13_partido_relaciona_personaje_3[1, ]
# max_personaje_4 <- T69.14_partido_relaciona_personaje_4[1, ]
# max_personaje_5 <- T69.15_partido_relaciona_personaje_5[1, ]
# max_personaje_6 <- T69.16_partido_relaciona_personaje_6[1, ]
# max_personaje_7 <- T69.17_partido_relaciona_personaje_7[1, ] #AGRAGAR O QUITAR PERSONAJES!!!!!1
# max_personaje_8 <- T69.18_partido_relaciona_personaje_8[1, ] #AGRAGAR O QUITAR PERSONAJES!!!!!1
#max_personaje_9 <- T69.19_partido_relaciona_personaje_9[1, ]

#base personajes relaciona
b_p_r <- length(ls(pattern = "T69.*partido_relaciona_personaje_\\d+$")) #✅ CONTEO DE VARIABLES EN TODO EL CODIGO-----------

rel_per_t <- ls(pattern = "T69.*partido_relaciona_personaje_\\d+$") 
rel_per_t

T70_BASE_T_partido_relaciona <- mget(rel_per_t)

#Cambiar el nombre de la segunda columna a "partido" en todos
lista_frames <- lapply(T70_BASE_T_partido_relaciona, function(df) {
    names(df)[2] <- "partido"
    return(df)
})

T70_BASE_T_partido_relaciona <- bind_rows(lista_frames)

T70_BASE_T_partido_relaciona <- T70_BASE_T_partido_relaciona[!T70_BASE_T_partido_relaciona$partido %in% c("NANA", "NS / NC", "Ninguno"), ]

T70_BASE_T_partido_relaciona$partido <- toupper(T70_BASE_T_partido_relaciona$partido)

T70_BASE_T_partido_relaciona <- T70_BASE_T_partido_relaciona %>%
    group_by(Personaje = .[[1]]) %>%  # Agrupar por la primera columna (personaje)
    mutate(
        `Partido con que lo relacionan más¹` = paste0(sprintf("%.1f", Porcentaje * 100), "% ", partido),
        `Porcentaje que lo relaciona con MORENA¹` = ifelse(partido == "MORENA", Porcentaje, NA)
    ) %>%
    fill(`Porcentaje que lo relaciona con MORENA¹`, .direction = "downup") %>%
    ungroup()

T70_BASE_T_partido_relaciona

#GENERA COLUMNAS CONCATENADA Y DE RELACION CON MORENA
T70_BASE_T_partido_relaciona <- T70_BASE_T_partido_relaciona %>%
    group_by(Personaje = .[[1]]) %>%  #Agrupar por la primera columna (personaje)
    mutate(
        `Porcentaje que lo relaciona con MORENA¹` = ifelse(partido == "MORENA", Porcentaje, NA),
        `Partido con que lo relacionan más¹` = paste0(sprintf("%.1f", Porcentaje * 100), "% ", partido)
    ) %>%
    fill(`Porcentaje que lo relaciona con MORENA¹`, .direction = "downup") %>%
    ungroup()

#REGRESA EL MAXIMO BASADO EN LA COLUMNA PORCENTAJE
T70_BASE_T_partido_relaciona_max <- T70_BASE_T_partido_relaciona %>%
    filter(partido != "TOTAL") %>%  # eliminar filas con partido == "TOTAL"
    group_by(Personaje = .[[1]]) %>%
    filter(Porcentaje == max(Porcentaje, na.rm = TRUE)) %>%
    ungroup()

T70_partido_relaciona <- dplyr::select(T70_BASE_T_partido_relaciona_max,Personaje, `Partido con que lo relacionan más¹`,`Porcentaje que lo relaciona con MORENA¹`)

#reemplazos para acentos y demás
T70_partido_relaciona <- T70_partido_relaciona %>%
    mutate(across(1, reemplazar_texto))

T70_partido_relaciona

#✅ HONESTIDAD+++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------

procesar_honestidad <- function(data, var, frec_tot, personaje) {
    orden_honestidad <- c("Mucho", "Algo", "Poco", "Nada", "NS / NC", "NANA")
    
    df <- data %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    columna <- names(df)[1]
    
    # Agregar filas faltantes
    faltan <- setdiff(orden_honestidad, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(
            df,
            data.frame(setNames(list(faltan), columna), Frecuencia = 0)
        )
    }
    
    # Ordenar según vector
    df <- df[match(orden_honestidad, df[[columna]]), ]
    
    # Calcular porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot,
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si la última fila es NANA, poner 0 en porcentaje válido
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0
    }
    
    # Renombrar la columna de la variable
    names(df)[1] <- personaje
    
    # Crear fila total
    total <- data.frame(
        Personaje = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    names(total)[1] <- personaje
    
    # Unir con el total
    df_final <- rbind(df, total)
    df_final$Personaje <- personaje
    df_final <- dplyr::select(df_final, Personaje, everything())
    
    df_final
}

T71.11_honestidad_personaje_1 <- procesar_honestidad(datos_factor, "V5190", frec_tot, personajes[1])
T71.12_honestidad_personaje_2 <- procesar_honestidad(datos_factor, "V5191", frec_tot, personajes[2])
T71.13_honestidad_personaje_3 <- procesar_honestidad(datos_factor, "V5192", frec_tot, personajes[3])
# T71.14_honestidad_personaje_4 <- procesar_honestidad(datos_factor, "V5193", frec_tot, personajes[4])
# T71.15_honestidad_personaje_5 <- procesar_honestidad(datos_factor, "V5194", frec_tot, personajes[5])
# T71.16_honestidad_personaje_6 <- procesar_honestidad(datos_factor, "V5195", frec_tot, personajes[6])
# T71.17_honestidad_personaje_7 <- procesar_honestidad(datos_factor, "V4213", frec_tot, personajes[7])
# T71.18_honestidad_personaje_8 <- procesar_honestidad(datos_factor, "V4214", frec_tot, personajes[8])
#T71.19_honestidad_personaje_9 <- procesar_honestidad(datos_factor, "V4215", frec_tot, personajes[9])

#71. honestidad de los personajes--------------------------------------------------------------------------------------------------------------------
#personaje 1
honestidad_1 <- T71.11_honestidad_personaje_1[ , c(2,4)]
honestidad_1 <- as.data.frame(t(honestidad_1))
honestidad_1

honestidad_1 <- tibble::rownames_to_column(as.data.frame(honestidad_1), var = "Personaje")
colnames(honestidad_1) <- as.character(unlist(honestidad_1[1, ]))
honestidad_1[2,1 ] <- honestidad_1[1,1 ]
honestidad_1 <- honestidad_1[-1, c(-7,-8)]
names(honestidad_1)[1] <- "Personaje"
honestidad_1

#personaje 2
honestidad_2 <- T71.12_honestidad_personaje_2[ , c(2,4)]
honestidad_2 <- as.data.frame(t(honestidad_2))
honestidad_2

honestidad_2 <- tibble::rownames_to_column(as.data.frame(honestidad_2), var = "Personaje")
colnames(honestidad_2) <- as.character(unlist(honestidad_2[1, ]))
honestidad_2[2,1 ] <- honestidad_2[1,1 ]
honestidad_2 <- honestidad_2[-1, c(-7,-8)]
names(honestidad_2)[1] <- "Personaje"
honestidad_2

#personaje 3
honestidad_3 <- T71.13_honestidad_personaje_3[ , c(2,4)]
honestidad_3 <- as.data.frame(t(honestidad_3))
honestidad_3

honestidad_3 <- tibble::rownames_to_column(as.data.frame(honestidad_3), var = "Personaje")
colnames(honestidad_3) <- as.character(unlist(honestidad_3[1, ]))
honestidad_3[2,1 ] <- honestidad_3[1,1 ]
honestidad_3 <- honestidad_3[-1, c(-7,-8)]
names(honestidad_3)[1] <- "Personaje"
honestidad_3

#personaje 4
# honestidad_4 <- T71.14_honestidad_personaje_4[ , c(2,4)]
# honestidad_4 <- as.data.frame(t(honestidad_4))
# honestidad_4
# 
# honestidad_4 <- tibble::rownames_to_column(as.data.frame(honestidad_4), var = "Personaje")
# colnames(honestidad_4) <- as.character(unlist(honestidad_4[1, ]))
# honestidad_4[2,1 ] <- honestidad_4[1,1 ]
# honestidad_4 <- honestidad_4[-1, c(-7,-8)]
# names(honestidad_4)[1] <- "Personaje"
# honestidad_4
# 
# #personaje 5
# honestidad_5 <- T71.15_honestidad_personaje_5[ , c(2,4)]
# honestidad_5 <- as.data.frame(t(honestidad_5))
# honestidad_5
# 
# honestidad_5 <- tibble::rownames_to_column(as.data.frame(honestidad_5), var = "Personaje")
# colnames(honestidad_5) <- as.character(unlist(honestidad_5[1, ]))
# honestidad_5[2,1 ] <- honestidad_5[1,1 ]
# honestidad_5 <- honestidad_5[-1, c(-7,-8)]
# names(honestidad_5)[1] <- "Personaje"
# honestidad_5
# 
# #personaje 6
# honestidad_6 <- T71.16_honestidad_personaje_6[ , c(2,4)]
# honestidad_6 <- as.data.frame(t(honestidad_6))
# honestidad_6
# 
# honestidad_6 <- tibble::rownames_to_column(as.data.frame(honestidad_6), var = "Personaje")
# colnames(honestidad_6) <- as.character(unlist(honestidad_6[1, ]))
# honestidad_6[2,1 ] <- honestidad_6[1,1 ]
# honestidad_6 <- honestidad_6[-1, c(-7,-8)]
# names(honestidad_6)[1] <- "Personaje"
# honestidad_6

# #personaje 7
# honestidad_7 <- T71.17_honestidad_personaje_7[ , c(2,4)]
# honestidad_7 <- as.data.frame(t(honestidad_7))
# honestidad_7
# 
# honestidad_7 <- tibble::rownames_to_column(as.data.frame(honestidad_7), var = "Personaje")
# colnames(honestidad_7) <- as.character(unlist(honestidad_7[1, ]))
# honestidad_7[2,1 ] <- honestidad_7[1,1 ]
# honestidad_7 <- honestidad_7[-1, c(-7,-8)]
# names(honestidad_7)[1] <- "Personaje"
# honestidad_7
# 
# # #personaje 8
# honestidad_8 <- T71.18_honestidad_personaje_8[ , c(2,4)]
# honestidad_8 <- as.data.frame(t(honestidad_8))
# honestidad_8
# 
# honestidad_8 <- tibble::rownames_to_column(as.data.frame(honestidad_8), var = "Personaje")
# colnames(honestidad_8) <- as.character(unlist(honestidad_8[1, ]))
# honestidad_8[2,1 ] <- honestidad_8[1,1 ]
# honestidad_8 <- honestidad_8[-1, c(-7,-8)]
# names(honestidad_8)[1] <- "Personaje"
# honestidad_8

# #personaje 9
# honestidad_9 <- T71.19_honestidad_personaje_9[ , c(2,4)]
# honestidad_9 <- as.data.frame(t(honestidad_9))
# honestidad_9
# 
# honestidad_9 <- tibble::rownames_to_column(as.data.frame(honestidad_9), var = "Personaje")
# colnames(honestidad_9) <- as.character(unlist(honestidad_9[1, ]))
# honestidad_9[2,1 ] <- honestidad_9[1,1 ]
# honestidad_9 <- honestidad_9[-1, c(-7,-8)]
# names(honestidad_9)[1] <- "Personaje"
# honestidad_9

hon_per <- ls(pattern = "^honestidad_\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------

T71_honestidad <- do.call(rbind, mget(hon_per))

#reemplazos para acentos y demás
T71_honestidad <- T71_honestidad %>%
    mutate(across(1, reemplazar_texto))

T71_honestidad

#✅ CERCANO+++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
procesar_cercano_personaje <- function(data, var, frec_tot, personaje) {
    orden_cercania <- c("Mucho", "Algo", "Poco", "Nada", "NS / NC", "NANA")
    
    df <- data %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    columna <- names(df)[1]
    
    # Agregar filas faltantes
    faltan <- setdiff(orden_cercania, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(
            df,
            data.frame(setNames(list(faltan), columna), Frecuencia = 0)
        )
    }
    
    # Ordenar según vector
    df <- df[match(orden_cercania, df[[columna]]), ]
    
    # Calcular porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot,
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si hay NANA, poner 0 en Porcentaje_válido
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0
    }
    
    # Renombrar y reordenar columnas
    names(df)[1] <- "Respuesta"
    df$Personaje <- personaje
    df <- dplyr::select(df, Personaje, Respuesta, Frecuencia, Porcentaje, Porcentaje_válido)
    
    # Agregar fila total
    total <- data.frame(
        Personaje = personaje,
        Respuesta = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    rbind(df, total)
}

T72.11_cercano_personaje_1 <- procesar_cercano_personaje(datos_factor, "V5200", frec_tot, personajes[1])
T72.12_cercano_personaje_2 <- procesar_cercano_personaje(datos_factor, "V5201", frec_tot, personajes[2])
T72.13_cercano_personaje_3 <- procesar_cercano_personaje(datos_factor, "V5202", frec_tot, personajes[3])
# T72.14_cercano_personaje_4 <- procesar_cercano_personaje(datos_factor, "V5203", frec_tot, personajes[4])
# T72.15_cercano_personaje_5 <- procesar_cercano_personaje(datos_factor, "V5204", frec_tot, personajes[5])
# T72.16_cercano_personaje_6 <- procesar_cercano_personaje(datos_factor, "V5205", frec_tot, personajes[6])
# T72.17_cercano_personaje_7 <- procesar_cercano_personaje(datos_factor, "V4221", frec_tot, personajes[7])
# T72.18_cercano_personaje_8 <- procesar_cercano_personaje(datos_factor, "V4222", frec_tot, personajes[8])
#T72.19_cercano_personaje_9 <- procesar_cercano_personaje(datos_factor, "V4332", frec_tot, personajes[9])

#72. cercania de los personajes-----------------------------------------------------------------------------------------------------------------------------
#personaje 1
cercano_1 <- T72.11_cercano_personaje_1[ , c(2,4)]
cercano_1 <- as.data.frame(t(cercano_1))
cercano_1

cercano_1 <- tibble::rownames_to_column(as.data.frame(cercano_1), var = "Personaje")
colnames(cercano_1) <- as.character(unlist(cercano_1[1, ]))
cercano_1[2,1 ] <- cercano_1[1,1 ]
cercano_1 <- cercano_1[-1, c(-7,-8)]
names(cercano_1)[1] <- "Personaje"
cercano_1

#personaje 2
cercano_2 <- T72.12_cercano_personaje_2[ , c(2,4)]
cercano_2 <- as.data.frame(t(cercano_2))
cercano_2

cercano_2 <- tibble::rownames_to_column(as.data.frame(cercano_2), var = "Personaje")
colnames(cercano_2) <- as.character(unlist(cercano_2[1, ]))
cercano_2[2,1 ] <- cercano_2[1,1 ]
cercano_2 <- cercano_2[-1, c(-7,-8)]
names(cercano_2)[1] <- "Personaje"
cercano_2

#personaje 3
cercano_3 <- T72.13_cercano_personaje_3[ , c(2,4)]
cercano_3 <- as.data.frame(t(cercano_3))
cercano_3

cercano_3 <- tibble::rownames_to_column(as.data.frame(cercano_3), var = "Personaje")
colnames(cercano_3) <- as.character(unlist(cercano_3[1, ]))
cercano_3[2,1 ] <- cercano_3[1,1 ]
cercano_3 <- cercano_3[-1, c(-7,-8)]
names(cercano_3)[1] <- "Personaje"
cercano_3

#personaje 4
# cercano_4 <- T72.14_cercano_personaje_4[ , c(2,4)]
# cercano_4 <- as.data.frame(t(cercano_4))
# cercano_4
# 
# cercano_4 <- tibble::rownames_to_column(as.data.frame(cercano_4), var = "Personaje")
# colnames(cercano_4) <- as.character(unlist(cercano_4[1, ]))
# cercano_4[2,1 ] <- cercano_4[1,1 ]
# cercano_4 <- cercano_4[-1, c(-7,-8)]
# names(cercano_4)[1] <- "Personaje"
# cercano_4
# 
# #personaje 5
# cercano_5 <- T72.15_cercano_personaje_5[ , c(2,4)]
# cercano_5 <- as.data.frame(t(cercano_5))
# cercano_5
# 
# cercano_5 <- tibble::rownames_to_column(as.data.frame(cercano_5), var = "Personaje")
# colnames(cercano_5) <- as.character(unlist(cercano_5[1, ]))
# cercano_5[2,1 ] <- cercano_5[1,1 ]
# cercano_5 <- cercano_5[-1, c(-7,-8)]
# names(cercano_5)[1] <- "Personaje"
# cercano_5
# 
# #personaje 6
# cercano_6 <- T72.16_cercano_personaje_6[ , c(2,4)]
# cercano_6 <- as.data.frame(t(cercano_6))
# cercano_6
# 
# cercano_6 <- tibble::rownames_to_column(as.data.frame(cercano_6), var = "Personaje")
# colnames(cercano_6) <- as.character(unlist(cercano_6[1, ]))
# cercano_6[2,1 ] <- cercano_6[1,1 ]
# cercano_6 <- cercano_6[-1, c(-7,-8)]
# names(cercano_6)[1] <- "Personaje"
# cercano_6

# #personaje 7
# cercano_7 <- T72.17_cercano_personaje_7[ , c(2,4)]
# cercano_7 <- as.data.frame(t(cercano_7))
# cercano_7
# 
# cercano_7 <- tibble::rownames_to_column(as.data.frame(cercano_7), var = "Personaje")
# colnames(cercano_7) <- as.character(unlist(cercano_7[1, ]))
# cercano_7[2,1 ] <- cercano_7[1,1 ]
# cercano_7 <- cercano_7[-1, c(-7,-8)]
# names(cercano_7)[1] <- "Personaje"
# cercano_7
# 
# # #personaje 8
# cercano_8 <- T72.18_cercano_personaje_8[ , c(2,4)]
# cercano_8 <- as.data.frame(t(cercano_8))
# cercano_8
# 
# cercano_8 <- tibble::rownames_to_column(as.data.frame(cercano_8), var = "Personaje")
# colnames(cercano_8) <- as.character(unlist(cercano_8[1, ]))
# cercano_8[2,1 ] <- cercano_8[1,1 ]
# cercano_8 <- cercano_8[-1, c(-7,-8)]
# names(cercano_8)[1] <- "Personaje"
# cercano_8
# 
#personaje 9
# cercano_9 <- T72.19_cercano_personaje_9[ , c(2,4)]
# cercano_9 <- as.data.frame(t(cercano_9))
# cercano_9
# 
# cercano_9 <- tibble::rownames_to_column(as.data.frame(cercano_9), var = "Personaje")
# colnames(cercano_9) <- as.character(unlist(cercano_9[1, ]))
# cercano_9[2,1 ] <- cercano_9[1,1 ]
# cercano_9 <- cercano_9[-1, c(-7,-8)]
# names(cercano_9)[1] <- "Personaje"
# cercano_9

cer_per <- ls(pattern = "^cercano_\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------
cer_per
T72_cercano <- do.call(rbind, mget(cer_per))

#reemplazos para acentos y demás
T72_cercano <- T72_cercano %>%
    mutate(across(1, reemplazar_texto))

T72_cercano

#T73 Característica de actores 1-----------------------------------------------------------------------------------------------------------------------------------
T73_caracteristicas_actores <- cbind(T71_honestidad,T72_cercano)
T73_caracteristicas_actores <- T73_caracteristicas_actores[ ,-7]
T73_caracteristicas_actores[ , 2:ncol(T73_caracteristicas_actores)] <- lapply(T73_caracteristicas_actores[ , 2:ncol(T73_caracteristicas_actores)], function(x) as.numeric(as.character(x)))

#reemplazos para acentos y demás
T73_caracteristicas_actores <- T73_caracteristicas_actores %>%
    mutate(across(1, reemplazar_texto))

rownames(T73_caracteristicas_actores) <- NULL

T73_caracteristicas_actores

T73_caracteristicas_actores_acum <- T73_caracteristicas_actores

T73_caracteristicas_actores_acum$honest_acum <- T73_caracteristicas_actores_acum$Mucho + (0.5 * T73_caracteristicas_actores_acum$Algo)
T73_caracteristicas_actores_acum$cerca_acum <- T73_caracteristicas_actores_acum$Mucho.1 + (0.5 * T73_caracteristicas_actores_acum$Algo.1)
T73_caracteristicas_actores_acum

T73_caracteristicas_actores_acum[[1]][T73_caracteristicas_actores_acum[[1]] == "Aldo Emmanuel Ruiz Sánchez"] <- "Aldo Emmanuel Ruíz Sánchez"

#✅ CONOCE+++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
procesar_conoce_personaje <- function(data, var, frec_tot, personaje) {
    orden_conoce <- c("Mucho","Algo","Poco","Nada","NS / NC","NANA")
    
    df <- data %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    columna <- names(df)[1]
    
    # Agregar filas faltantes
    faltan <- setdiff(orden_conoce, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(
            df,
            data.frame(setNames(list(faltan), columna), Frecuencia = 0)
        )
    }
    
    # Ordenar según vector
    df <- df[match(orden_conoce, df[[columna]]), ]
    
    # Calcular porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot,
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si la última fila es NANA, poner 0 en porcentaje válido
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0
    }
    
    # Renombrar la columna de la variable
    names(df)[1] <- "Respuesta"
    
    # Agregar columna de personaje
    df$Personaje <- personaje
    
    # Reordenar columnas
    df <- df %>%
        dplyr::select(Personaje, Respuesta, Frecuencia, Porcentaje, Porcentaje_válido)
    
    # Agregar fila total
    total <- data.frame(
        Personaje = personaje,
        Respuesta = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    rbind(df, total)
}

T74.11_conoce_personaje_1 <- procesar_conoce_personaje(datos_factor, "V5210", frec_tot, personajes[1])
T74.12_conoce_personaje_2 <- procesar_conoce_personaje(datos_factor, "V5211", frec_tot, personajes[2])
T74.13_conoce_personaje_3 <- procesar_conoce_personaje(datos_factor, "V5212", frec_tot, personajes[3])
# T74.14_conoce_personaje_4 <- procesar_conoce_personaje(datos_factor, "V5213", frec_tot, personajes[4])
# T74.15_conoce_personaje_5 <- procesar_conoce_personaje(datos_factor, "V5214", frec_tot, personajes[5])
# T74.16_conoce_personaje_6 <- procesar_conoce_personaje(datos_factor, "V5215", frec_tot, personajes[6])
# T74.17_conoce_personaje_7 <- procesar_conoce_personaje(datos_factor, "V4229", frec_tot, personajes[7])
# T74.18_conoce_personaje_8 <- procesar_conoce_personaje(datos_factor, "V4230", frec_tot, personajes[8])
#T74.19_conoce_personaje_9 <- procesar_conoce_personaje(datos_factor, "V4334", frec_tot, personajes[8])

#74. conoce estado personajes-----------------------------------------------------------------------------------------------------------------------------
#personaje 1
conoce_1 <- T74.11_conoce_personaje_1[ , c(2,4)]
conoce_1 <- as.data.frame(t(conoce_1))
conoce_1

conoce_1 <- tibble::rownames_to_column(as.data.frame(conoce_1), var = "Personaje")
colnames(conoce_1) <- as.character(unlist(conoce_1[1, ]))
conoce_1[2,1 ] <- conoce_1[1,1 ]
conoce_1 <- conoce_1[-1, c(-7,-8)]
names(conoce_1)[1] <- "Personaje"
conoce_1

#personaje 2
conoce_2 <- T74.12_conoce_personaje_2[ , c(2,4)]
conoce_2 <- as.data.frame(t(conoce_2))
conoce_2

conoce_2 <- tibble::rownames_to_column(as.data.frame(conoce_2), var = "Personaje")
colnames(conoce_2) <- as.character(unlist(conoce_2[1, ]))
conoce_2[2,1 ] <- conoce_2[1,1 ]
conoce_2 <- conoce_2[-1, c(-7,-8)]
names(conoce_2)[1] <- "Personaje"
conoce_2

#personaje 3
conoce_3 <- T74.13_conoce_personaje_3[ , c(2,4)]
conoce_3 <- as.data.frame(t(conoce_3))
conoce_3

conoce_3 <- tibble::rownames_to_column(as.data.frame(conoce_3), var = "Personaje")
colnames(conoce_3) <- as.character(unlist(conoce_3[1, ]))
conoce_3[2,1 ] <- conoce_3[1,1 ]
conoce_3 <- conoce_3[-1, c(-7,-8)]
names(conoce_3)[1] <- "Personaje"
conoce_3

#personaje 4
# conoce_4 <- T74.14_conoce_personaje_4[ , c(2,4)]
# conoce_4 <- as.data.frame(t(conoce_4))
# conoce_4
# 
# conoce_4 <- tibble::rownames_to_column(as.data.frame(conoce_4), var = "Personaje")
# colnames(conoce_4) <- as.character(unlist(conoce_4[1, ]))
# conoce_4[2,1 ] <- conoce_4[1,1 ]
# conoce_4 <- conoce_4[-1, c(-7,-8)]
# names(conoce_4)[1] <- "Personaje"
# conoce_4

#personaje 5
# conoce_5 <- T74.15_conoce_personaje_5[ , c(2,4)]
# conoce_5 <- as.data.frame(t(conoce_5))
# conoce_5
# 
# conoce_5 <- tibble::rownames_to_column(as.data.frame(conoce_5), var = "Personaje")
# colnames(conoce_5) <- as.character(unlist(conoce_5[1, ]))
# conoce_5[2,1 ] <- conoce_5[1,1 ]
# conoce_5 <- conoce_5[-1, c(-7,-8)]
# names(conoce_5)[1] <- "Personaje"
# conoce_5
# 
# #personaje 6
# conoce_6 <- T74.16_conoce_personaje_6[ , c(2,4)]
# conoce_6 <- as.data.frame(t(conoce_6))
# conoce_6
# 
# conoce_6 <- tibble::rownames_to_column(as.data.frame(conoce_6), var = "Personaje")
# colnames(conoce_6) <- as.character(unlist(conoce_6[1, ]))
# conoce_6[2,1 ] <- conoce_6[1,1 ]
# conoce_6 <- conoce_6[-1, c(-7,-8)]
# names(conoce_6)[1] <- "Personaje"
# conoce_6

# #personaje 7
# conoce_7 <- T74.17_conoce_personaje_7[ , c(2,4)]
# conoce_7 <- as.data.frame(t(conoce_7))
# conoce_7
# 
# conoce_7 <- tibble::rownames_to_column(as.data.frame(conoce_7), var = "Personaje")
# colnames(conoce_7) <- as.character(unlist(conoce_7[1, ]))
# conoce_7[2,1 ] <- conoce_7[1,1 ]
# conoce_7 <- conoce_7[-1, c(-7,-8)]
# names(conoce_7)[1] <- "Personaje"
# conoce_7
# 
# # #personaje 8
# conoce_8 <- T74.18_conoce_personaje_8[ , c(2,4)]
# conoce_8 <- as.data.frame(t(conoce_8))
# conoce_8
# 
# conoce_8 <- tibble::rownames_to_column(as.data.frame(conoce_8), var = "Personaje")
# colnames(conoce_8) <- as.character(unlist(conoce_8[1, ]))
# conoce_8[2,1 ] <- conoce_8[1,1 ]
# conoce_8 <- conoce_8[-1, c(-7,-8)]
# names(conoce_8)[1] <- "Personaje"
# conoce_8

#personaje 9
# conoce_9 <- T74.19_conoce_personaje_9[ , c(2,4)]
# conoce_9 <- as.data.frame(t(conoce_9))
# conoce_9
# 
# conoce_9 <- tibble::rownames_to_column(as.data.frame(conoce_9), var = "Personaje")
# colnames(conoce_9) <- as.character(unlist(conoce_9[1, ]))
# conoce_9[2,1 ] <- conoce_9[1,1 ]
# conoce_9 <- conoce_9[-1, c(-7,-8)]
# names(conoce_9)[1] <- "Personaje"
# conoce_9

con_per <- ls(pattern = "^conoce_\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------
con_per
T75_conoce <- do.call(rbind, mget(con_per))

#reemplazos para acentos y demás
T75_conoce <- T75_conoce %>%
    mutate(across(1, reemplazar_texto))

rownames(T75_conoce) <- NULL
T75_conoce

#✅ CUMPLE+++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
procesar_cumple_personaje <- function(data, var, frec_tot, personaje) {
    orden_cumple <- c("Mucho", "Algo", "Poco", "Nada", "NS / NC", "NANA")
    
    df <- data %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    columna <- names(df)[1]
    
    # Agregar filas faltantes si no aparecen en los datos
    faltan <- setdiff(orden_cumple, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(
            df,
            data.frame(setNames(list(faltan), columna), Frecuencia = 0)
        )
    }
    
    # Ordenar respuestas en el orden correcto
    df <- df[match(orden_cumple, df[[columna]]), ]
    
    # Calcular porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot,
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si existe “NANA”, poner 0 en porcentaje válido
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0
    }
    
    # Renombrar columna principal
    names(df)[1] <- "Respuesta"
    
    # Agregar columna de personaje
    df$Personaje <- personaje
    
    # Reordenar columnas
    df <- dplyr::select(df, Personaje, Respuesta, Frecuencia, Porcentaje, Porcentaje_válido)
    
    # Agregar fila total
    total <- data.frame(
        Personaje = personaje,
        Respuesta = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    rbind(df, total)
}

T76.11_cumple_personaje_1 <- procesar_cumple_personaje(datos_factor, "V5220", frec_tot, personajes[1])
T76.12_cumple_personaje_2 <- procesar_cumple_personaje(datos_factor, "V5221", frec_tot, personajes[2])
T76.13_cumple_personaje_3 <- procesar_cumple_personaje(datos_factor, "V5222", frec_tot, personajes[3])
# T76.14_cumple_personaje_4 <- procesar_cumple_personaje(datos_factor, "V5223", frec_tot, personajes[4])
# T76.15_cumple_personaje_5 <- procesar_cumple_personaje(datos_factor, "V5224", frec_tot, personajes[5])
# T76.16_cumple_personaje_6 <- procesar_cumple_personaje(datos_factor, "V5225", frec_tot, personajes[6])
# T76.17_cumple_personaje_7 <- procesar_cumple_personaje(datos_factor, "V4237", frec_tot, personajes[7])
# T76.18_cumple_personaje_8 <- procesar_cumple_personaje(datos_factor, "V4238", frec_tot, personajes[8])
#T76.19_cumple_personaje_9 <- procesar_cumple_personaje(datos_factor, "V4338", frec_tot, personajes[9])

#76. cumplen de los personajes---------------------------------------------------------------------------------------------------------------------------------------
#personaje 1
cumple_1 <- T76.11_cumple_personaje_1[ , c(2,4)]
cumple_1 <- as.data.frame(t(cumple_1))
cumple_1

cumple_1 <- tibble::rownames_to_column(as.data.frame(cumple_1), var = "Personaje")
colnames(cumple_1) <- as.character(unlist(cumple_1[1, ]))
cumple_1[2,1 ] <- cumple_1[1,1 ]
cumple_1 <- cumple_1[-1, c(-7,-8)]

names(cumple_1)[1] <- "Personaje"
cumple_1

#personaje 2
cumple_2 <- T76.12_cumple_personaje_2[ , c(2,4)]
cumple_2 <- as.data.frame(t(cumple_2))
cumple_2

cumple_2 <- tibble::rownames_to_column(as.data.frame(cumple_2), var = "Personaje")
colnames(cumple_2) <- as.character(unlist(cumple_2[1, ]))
cumple_2[2,1 ] <- cumple_2[1,1 ]
cumple_2 <- cumple_2[-1, c(-7,-8)]
names(cumple_2)[1] <- "Personaje"
cumple_2

#personaje 3
cumple_3 <- T76.13_cumple_personaje_3[ , c(2,4)]
cumple_3 <- as.data.frame(t(cumple_3))
cumple_3

cumple_3 <- tibble::rownames_to_column(as.data.frame(cumple_3), var = "Personaje")
colnames(cumple_3) <- as.character(unlist(cumple_3[1, ]))
cumple_3[2,1 ] <- cumple_3[1,1 ]
cumple_3 <- cumple_3[-1, c(-7,-8)]
names(cumple_3)[1] <- "Personaje"
cumple_3

#personaje 4
# cumple_4 <- T76.14_cumple_personaje_4[ , c(2,4)]
# cumple_4 <- as.data.frame(t(cumple_4))
# cumple_4
# 
# cumple_4 <- tibble::rownames_to_column(as.data.frame(cumple_4), var = "Personaje")
# colnames(cumple_4) <- as.character(unlist(cumple_4[1, ]))
# cumple_4[2,1 ] <- cumple_4[1,1 ]
# cumple_4 <- cumple_4[-1, c(-7,-8)]
# names(cumple_4)[1] <- "Personaje"
# cumple_4
# 
# #personaje 5
# cumple_5 <- T76.15_cumple_personaje_5[ , c(2,4)]
# cumple_5 <- as.data.frame(t(cumple_5))
# cumple_5
# 
# cumple_5 <- tibble::rownames_to_column(as.data.frame(cumple_5), var = "Personaje")
# colnames(cumple_5) <- as.character(unlist(cumple_5[1, ]))
# cumple_5[2,1 ] <- cumple_5[1,1 ]
# cumple_5 <- cumple_5[-1, c(-7,-8)]
# names(cumple_5)[1] <- "Personaje"
# cumple_5
# 
# #personaje 6
# cumple_6 <- T76.16_cumple_personaje_6[ , c(2,4)]
# cumple_6 <- as.data.frame(t(cumple_6))
# cumple_6
# 
# cumple_6 <- tibble::rownames_to_column(as.data.frame(cumple_6), var = "Personaje")
# colnames(cumple_6) <- as.character(unlist(cumple_6[1, ]))
# cumple_6[2,1 ] <- cumple_6[1,1 ]
# cumple_6 <- cumple_6[-1, c(-7,-8)]
# names(cumple_6)[1] <- "Personaje"
# cumple_6

# #personaje 7
# cumple_7 <- T76.17_cumple_personaje_7[ , c(2,4)]
# cumple_7 <- as.data.frame(t(cumple_7))
# cumple_7
# 
# cumple_7 <- tibble::rownames_to_column(as.data.frame(cumple_7), var = "Personaje")
# colnames(cumple_7) <- as.character(unlist(cumple_7[1, ]))
# cumple_7[2,1 ] <- cumple_7[1,1 ]
# cumple_7 <- cumple_7[-1, c(-7,-8)]
# names(cumple_7)[1] <- "Personaje"
# cumple_7
# 
# # #personaje 8
# cumple_8 <- T76.18_cumple_personaje_8[ , c(2,4)]
# cumple_8 <- as.data.frame(t(cumple_8))
# cumple_8
# 
# cumple_8 <- tibble::rownames_to_column(as.data.frame(cumple_8), var = "Personaje")
# colnames(cumple_8) <- as.character(unlist(cumple_8[1, ]))
# cumple_8[2,1 ] <- cumple_8[1,1 ]
# cumple_8 <- cumple_8[-1, c(-7,-8)]
# names(cumple_8)[1] <- "Personaje"
# cumple_8

#personaje 9
# cumple_9 <- T76.19_cumple_personaje_9[ , c(2,4)]
# cumple_9 <- as.data.frame(t(cumple_9))
# cumple_9
# 
# cumple_9 <- tibble::rownames_to_column(as.data.frame(cumple_9), var = "Personaje")
# colnames(cumple_9) <- as.character(unlist(cumple_9[1, ]))
# cumple_9[2,1 ] <- cumple_9[1,1 ]
# cumple_9 <- cumple_9[-1, c(-7,-8)]
# names(cumple_9)[1] <- "Personaje"
# cumple_9

cum_per <- ls(pattern = "^cumple_\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------
cum_per
T76_cumple <- do.call(rbind, mget(cum_per))

rownames(T76_cumple) <- NULL
T76_cumple

#T77 Característica de actores 2---------------------------------------------------------------------------------------------------------------------------------------------
T77_caracteristicas_actores_2 <- cbind(T75_conoce,T76_cumple)
T77_caracteristicas_actores_2 <- T77_caracteristicas_actores_2[ ,-7]
T77_caracteristicas_actores_2

T77_caracteristicas_actores_2_acum <- T77_caracteristicas_actores_2
T77_caracteristicas_actores_2_acum[ , 2:ncol(T77_caracteristicas_actores_2_acum)] <- lapply(T77_caracteristicas_actores_2_acum[ , 2:ncol(T77_caracteristicas_actores_2_acum)], function(x) as.numeric(as.character(x)))
T77_caracteristicas_actores_2_acum$conoce_acum <- T77_caracteristicas_actores_2_acum$Mucho + (0.5 * T77_caracteristicas_actores_2_acum$Algo)
T77_caracteristicas_actores_2_acum$cumple_acum <- T77_caracteristicas_actores_2_acum$Mucho.1 + (0.5 * T77_caracteristicas_actores_2_acum$Algo.1)

T77_caracteristicas_actores_2_acum$Personaje <- personajes[1:nrow(T77_caracteristicas_actores_2_acum)]
T77_caracteristicas_actores_2_acum

# #reemplazos para acentos y demás
# T77_caracteristicas_actores_2 <- T77_caracteristicas_actores_2 %>%
#   mutate(across(1, reemplazar_texto))

#pasara  numerico
T77_caracteristicas_actores_2[ , 2:ncol(T77_caracteristicas_actores_2)] <- lapply(T77_caracteristicas_actores_2[ , 2:ncol(T77_caracteristicas_actores_2)], function(x) as.numeric(as.character(x)))

T77_caracteristicas_actores_2

T77_caracteristicas_actores_2$Personaje <- personajes[1:nrow(T77_caracteristicas_actores_2)]
T77_caracteristicas_actores_2
T77_caracteristicas_actores_2[[1]][T77_caracteristicas_actores_2[[1]] == "Andrea Chavez Treviño"] <- "Andrea Chávez Treviño"

#✅ BUEN  CANDIDATO++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
procesar_buen_personaje <- function(data, var, frec_tot, personaje) {
    orden_buen <- c("Sí", "No", "NS / NC", "NANA")
    
    df <- data %>%
        group_by(.data[[var]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    # corregir "Si" → "Sí"
    df[[1]] <- ifelse(df[[1]] == "Si", "Sí", df[[1]])
    
    columna <- names(df)[1]
    
    # agregar filas faltantes
    faltan <- setdiff(orden_buen, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(
            df,
            data.frame(setNames(list(faltan), columna), Frecuencia = 0)
        )
    }
    
    # ordenar según vector
    df <- df[match(orden_buen, df[[columna]]), ]
    
    # porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot,
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # si hay NANA → porcentaje válido = 0
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0
    }
    
    # renombrar columna principal
    names(df)[1] <- "Respuesta"
    df$Personaje <- personaje
    df <- dplyr:::select(df, Personaje, Respuesta, Frecuencia, Porcentaje, Porcentaje_válido)
    
    # agregar fila total
    total <- data.frame(
        Personaje = personaje,
        Respuesta = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    rbind(df, total)
}

T78.11_buen_personaje_1 <- procesar_buen_personaje(datos_factor, "V5230", frec_tot, personajes[1])
T78.12_buen_personaje_2 <- procesar_buen_personaje(datos_factor, "V5231", frec_tot, personajes[2])
T78.13_buen_personaje_3 <- procesar_buen_personaje(datos_factor, "V5232", frec_tot, personajes[3])
# T78.14_buen_personaje_4 <- procesar_buen_personaje(datos_factor, "V5233", frec_tot, personajes[4])
# T78.15_buen_personaje_5 <- procesar_buen_personaje(datos_factor, "V5234", frec_tot, personajes[5])
# T78.16_buen_personaje_6 <- procesar_buen_personaje(datos_factor, "V5235", frec_tot, personajes[6])
# T78.17_buen_personaje_7 <- procesar_buen_personaje(datos_factor, "V4245", frec_tot, personajes[7])
# T78.18_buen_personaje_8 <- procesar_buen_personaje(datos_factor, "V4246", frec_tot, personajes[8])
#T78.19_buen_personaje_9 <- procesar_buen_personaje(datos_factor, "V4247", frec_tot, personajes[9])

#79. buen candidato personajes---------------------------------------------------------------------------------------------------------------------------------------
#personaje 1
buen_1 <- T78.11_buen_personaje_1[ , c(2,4)]
buen_1 <- as.data.frame(t(buen_1))
buen_1

buen_1 <- tibble::rownames_to_column(as.data.frame(buen_1), var = "Personaje")
colnames(buen_1) <- as.character(unlist(buen_1[1, ]))
buen_1[2,1 ] <- buen_1[1,1 ]
buen_1 <- buen_1[-1, c(-7,-8)]
names(buen_1)[1] <- "Personaje"
buen_1

#personaje 2
buen_2 <- T78.12_buen_personaje_2[ , c(2,4)]
buen_2 <- as.data.frame(t(buen_2))
buen_2

buen_2 <- tibble::rownames_to_column(as.data.frame(buen_2), var = "Personaje")
colnames(buen_2) <- as.character(unlist(buen_2[1, ]))
buen_2[2,1 ] <- buen_2[1,1 ]
buen_2 <- buen_2[-1, c(-7,-8)]
names(buen_2)[1] <- "Personaje"
buen_2

#personaje 3
buen_3 <- T78.13_buen_personaje_3[ , c(2,4)]
buen_3 <- as.data.frame(t(buen_3))
buen_3

buen_3 <- tibble::rownames_to_column(as.data.frame(buen_3), var = "Personaje")
colnames(buen_3) <- as.character(unlist(buen_3[1, ]))
buen_3[2,1 ] <- buen_3[1,1 ]
buen_3 <- buen_3[-1, c(-7,-8)]
names(buen_3)[1] <- "Personaje"
buen_3

#personaje 4
# buen_4 <- T78.14_buen_personaje_4[ , c(2,4)]
# buen_4 <- as.data.frame(t(buen_4))
# buen_4
# 
# buen_4 <- tibble::rownames_to_column(as.data.frame(buen_4), var = "Personaje")
# colnames(buen_4) <- as.character(unlist(buen_4[1, ]))
# buen_4[2,1 ] <- buen_4[1,1 ]
# buen_4 <- buen_4[-1, c(-7,-8)]
# names(buen_4)[1] <- "Personaje"
# buen_4
# 
# #personaje 5
# buen_5 <- T78.15_buen_personaje_5[ , c(2,4)]
# buen_5 <- as.data.frame(t(buen_5))
# buen_5
# 
# buen_5 <- tibble::rownames_to_column(as.data.frame(buen_5), var = "Personaje")
# colnames(buen_5) <- as.character(unlist(buen_5[1, ]))
# buen_5[2,1 ] <- buen_5[1,1 ]
# buen_5 <- buen_5[-1, c(-7,-8)]
# names(buen_5)[1] <- "Personaje"
# buen_5

# #personaje 6
# buen_6 <- T78.16_buen_personaje_6[ , c(2,4)]
# buen_6 <- as.data.frame(t(buen_6))
# buen_6
# 
# buen_6 <- tibble::rownames_to_column(as.data.frame(buen_6), var = "Personaje")
# colnames(buen_6) <- as.character(unlist(buen_6[1, ]))
# buen_6[2,1 ] <- buen_6[1,1 ]
# buen_6 <- buen_6[-1, c(-7,-8)]
# names(buen_6)[1] <- "Personaje"
# buen_6

# #personaje 7
# buen_7 <- T78.17_buen_personaje_7[ , c(2,4)]
# buen_7 <- as.data.frame(t(buen_7))
# buen_7
# 
# buen_7 <- tibble::rownames_to_column(as.data.frame(buen_7), var = "Personaje")
# colnames(buen_7) <- as.character(unlist(buen_7[1, ]))
# buen_7[2,1 ] <- buen_7[1,1 ]
# buen_7 <- buen_7[-1, c(-7,-8)]
# names(buen_7)[1] <- "Personaje"
# buen_7
# 
# #personaje 8
# buen_8 <- T78.18_buen_personaje_8[ , c(2,4)]
# buen_8 <- as.data.frame(t(buen_8))
# buen_8
# 
# buen_8 <- tibble::rownames_to_column(as.data.frame(buen_8), var = "Personaje")
# colnames(buen_8) <- as.character(unlist(buen_8[1, ]))
# buen_8[2,1 ] <- buen_8[1,1 ]
# buen_8 <- buen_8[-1, c(-7,-8)]
# names(buen_8)[1] <- "Personaje"
# buen_8

#personaje 9
# buen_9 <- T78.19_buen_personaje_9[ , c(2,4)]
# buen_9 <- as.data.frame(t(buen_9))
# buen_9
# 
# buen_9 <- tibble::rownames_to_column(as.data.frame(buen_9), var = "Personaje")
# colnames(buen_9) <- as.character(unlist(buen_9[1, ]))
# buen_9[2,1 ] <- buen_9[1,1 ]
# buen_9 <- buen_9[-1, c(-7,-8)]
# names(buen_9)[1] <- "Personaje"
# buen_9

buen_can <- ls(pattern = "^buen_\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------
buen_can
T79_buen_candidato <- do.call(rbind, mget(buen_can))

rownames(T79_buen_candidato) <- NULL
T79_buen_candidato

#reemplazos para acentos y demás
T79_buen_candidato <- T79_buen_candidato %>%
    mutate(across(1, reemplazar_texto))

T79_buen_candidato

#✅ VOTARIA++++++++++++++++++++++++++++++++++++++-++++++++++++++++++++++++++------------------------------------------------------------------------------------------------------------
procesar_votaria_personaje <- function(datos, variable, nombre_personaje, frec_tot, orden_votaria) {
    
    df <- datos %>%
        group_by(.data[[variable]]) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop")
    
    # Uniformar texto “Nunca votaría”
    df[[1]] <- ifelse(df[[1]] == "Nunca Votaría", "Nunca votaría", df[[1]])
    
    # Asegurar todas las categorías
    columna <- names(df)[1]
    faltan <- setdiff(orden_votaria, df[[columna]])
    if (length(faltan) > 0) {
        df <- rbind(df,
                    data.frame(
                        setNames(list(faltan), columna),
                        Frecuencia = 0
                    )
        )
    }
    
    # Ordenar categorías
    df <- df[match(orden_votaria, df[[columna]]), ]
    
    # Calcular porcentajes
    df <- df %>%
        mutate(
            Porcentaje = Frecuencia / frec_tot
        )
    
    names(df)[1] <- nombre_personaje
    
    # Porcentaje válido (sin NANA)
    df <- df %>%
        mutate(
            Porcentaje_válido = Frecuencia / sum(Frecuencia[.[[1]] != "NANA"])
        )
    
    # Si aparece NANA → 0 en porcentaje válido
    if (df[nrow(df), 1] == "NANA") {
        df$Porcentaje_válido[nrow(df)] <- 0.0
    }
    
    # Totales
    tot <- data.frame(
        Personaje = "Total",
        Frecuencia = sum(df$Frecuencia),
        Porcentaje = sum(df$Porcentaje),
        Porcentaje_válido = sum(df$Porcentaje_válido)
    )
    
    names(tot)[1] <- nombre_personaje
    
    # Unir todo
    df_final <- rbind(df, tot)
    df_final$Personaje <- nombre_personaje
    df_final <- dplyr::select(df_final, Personaje, everything())
    
    return(df_final)
}

orden_votaria <- c("Votaría", "Nunca votaría", "NS / NC", "NANA")

T80.11_votaria_personaje_1 <- procesar_votaria_personaje(datos_factor, "V5240", personajes[1], frec_tot, orden_votaria)
T80.12_votaria_personaje_2 <- procesar_votaria_personaje(datos_factor, "V5241", personajes[2], frec_tot, orden_votaria)
T80.13_votaria_personaje_3 <- procesar_votaria_personaje(datos_factor, "V5242", personajes[3], frec_tot, orden_votaria)
# T80.14_votaria_personaje_4 <- procesar_votaria_personaje(datos_factor, "V5243", personajes[4], frec_tot, orden_votaria)
# T80.15_votaria_personaje_5 <- procesar_votaria_personaje(datos_factor, "V5244", personajes[5], frec_tot, orden_votaria)
# T80.16_votaria_personaje_6 <- procesar_votaria_personaje(datos_factor, "V5245", personajes[6], frec_tot, orden_votaria)
# T80.17_votaria_personaje_7 <- procesar_votaria_personaje(datos_factor, "V4253", personajes[7], frec_tot, orden_votaria)
# T80.18_votaria_personaje_8 <- procesar_votaria_personaje(datos_factor, "V4254", personajes[8], frec_tot, orden_votaria)
#T80.19_votaria_personaje_9 <- procesar_votaria_personaje(datos_factor, "V4342", personajes[9], frec_tot, orden_votaria)

#81. votaria candidato personajes---------------------------------------------------------------------------------------------------------------------------------------
#personaje 1
votaria_1 <- T80.11_votaria_personaje_1[ , c(2,4)]
votaria_1 <- as.data.frame(t(votaria_1))
votaria_1

votaria_1 <- tibble::rownames_to_column(as.data.frame(votaria_1), var = "Personaje")
colnames(votaria_1) <- as.character(unlist(votaria_1[1, ]))
votaria_1[2,1 ] <- votaria_1[1,1 ]
votaria_1 <- votaria_1[-1, c(-7,-8)]
names(votaria_1)[1] <- "Personaje"
votaria_1

#personaje 2
votaria_2 <- T80.12_votaria_personaje_2[ , c(2,4)]
votaria_2 <- as.data.frame(t(votaria_2))
votaria_2

votaria_2 <- tibble::rownames_to_column(as.data.frame(votaria_2), var = "Personaje")
colnames(votaria_2) <- as.character(unlist(votaria_2[1, ]))
votaria_2[2,1 ] <- votaria_2[1,1 ]
votaria_2 <- votaria_2[-1, c(-7,-8)]
names(votaria_2)[1] <- "Personaje"
votaria_2

#personaje 3
votaria_3 <- T80.13_votaria_personaje_3[ , c(2,4)]
votaria_3 <- as.data.frame(t(votaria_3))
votaria_3

votaria_3 <- tibble::rownames_to_column(as.data.frame(votaria_3), var = "Personaje")
colnames(votaria_3) <- as.character(unlist(votaria_3[1, ]))
votaria_3[2,1 ] <- votaria_3[1,1 ]
votaria_3 <- votaria_3[-1, c(-7,-8)]
names(votaria_3)[1] <- "Personaje"
votaria_3

#personaje 4
# votaria_4 <- T80.14_votaria_personaje_4[ , c(2,4)]
# votaria_4 <- as.data.frame(t(votaria_4))
# votaria_4
# 
# votaria_4 <- tibble::rownames_to_column(as.data.frame(votaria_4), var = "Personaje")
# colnames(votaria_4) <- as.character(unlist(votaria_4[1, ]))
# votaria_4[2,1 ] <- votaria_4[1,1 ]
# votaria_4 <- votaria_4[-1, c(-7,-8)]
# names(votaria_4)[1] <- "Personaje"
# votaria_4
# 
# #personaje 5
# votaria_5 <- T80.15_votaria_personaje_5[ , c(2,4)]
# votaria_5 <- as.data.frame(t(votaria_5))
# votaria_5
# 
# votaria_5 <- tibble::rownames_to_column(as.data.frame(votaria_5), var = "Personaje")
# colnames(votaria_5) <- as.character(unlist(votaria_5[1, ]))
# votaria_5[2,1 ] <- votaria_5[1,1 ]
# votaria_5 <- votaria_5[-1, c(-7,-8)]
# names(votaria_5)[1] <- "Personaje"
# votaria_5
# 
# #personaje 6
# votaria_6 <- T80.16_votaria_personaje_6[ , c(2,4)]
# votaria_6 <- as.data.frame(t(votaria_6))
# votaria_6
# 
# votaria_6 <- tibble::rownames_to_column(as.data.frame(votaria_6), var = "Personaje")
# colnames(votaria_6) <- as.character(unlist(votaria_6[1, ]))
# votaria_6[2,1 ] <- votaria_6[1,1 ]
# votaria_6 <- votaria_6[-1, c(-7,-8)]
# names(votaria_6)[1] <- "Personaje"
# votaria_6

# #personaje 7
# votaria_7 <- T80.17_votaria_personaje_7[ , c(2,4)]
# votaria_7 <- as.data.frame(t(votaria_7))
# votaria_7
# 
# votaria_7 <- tibble::rownames_to_column(as.data.frame(votaria_7), var = "Personaje")
# colnames(votaria_7) <- as.character(unlist(votaria_7[1, ]))
# votaria_7[2,1 ] <- votaria_7[1,1 ]
# votaria_7 <- votaria_7[-1, c(-7,-8)]
# names(votaria_7)[1] <- "Personaje"
# votaria_7
# 
# #personaje 8
# votaria_8 <- T80.18_votaria_personaje_8[ , c(2,4)]
# votaria_8 <- as.data.frame(t(votaria_8))
# votaria_8
# 
# votaria_8 <- tibble::rownames_to_column(as.data.frame(votaria_8), var = "Personaje")
# colnames(votaria_8) <- as.character(unlist(votaria_8[1, ]))
# votaria_8[2,1 ] <- votaria_8[1,1 ]
# votaria_8 <- votaria_8[-1, c(-7,-8)]
# names(votaria_8)[1] <- "Personaje"
# votaria_8

#personaje 9
# votaria_9 <- T80.19_votaria_personaje_9[ , c(2,4)]
# votaria_9 <- as.data.frame(t(votaria_9))
# votaria_9
# 
# votaria_9 <- tibble::rownames_to_column(as.data.frame(votaria_9), var = "Personaje")
# colnames(votaria_9) <- as.character(unlist(votaria_9[1, ]))
# votaria_9[2,1 ] <- votaria_9[1,1 ]
# votaria_9 <- votaria_9[-1, c(-7,-8)]
# names(votaria_9)[1] <- "Personaje"
# votaria_9

vot_can <- ls(pattern = "^votaria_\\d+$")  #✅ LISTA DE VARIABLES EN TODO EL CODIGO-----------
vot_can
T81_votaria_candidato <- do.call(rbind, mget(vot_can))

#reemplazos para acentos y demás
T81_votaria_candidato <- T81_votaria_candidato %>%
    mutate(across(1, reemplazar_texto))

T81_votaria_candidato
rownames(T81_votaria_candidato) <- NULL 

#T82 Característica de actores 3 🛑---------------------------------------------------------------------------------------------------------------------------------------------
T82_caracteristicas_actores_3 <- cbind(T79_buen_candidato,T81_votaria_candidato)
T82_caracteristicas_actores_3 <- T82_caracteristicas_actores_3[ ,c(-1,-5,-6,-11,-12)]
T82_caracteristicas_actores_3 <- dplyr::select(T82_caracteristicas_actores_3, c(4,1:3,5:7))

#pasara  numerico
T82_caracteristicas_actores_3[ , 2:ncol(T82_caracteristicas_actores_3)] <- lapply(T82_caracteristicas_actores_3[ , 2:ncol(T82_caracteristicas_actores_3)], function(x) as.numeric(as.character(x)))

T82_caracteristicas_actores_3

T82_caracteristicas_actores_3[[1]] <- ifelse(T82_caracteristicas_actores_3[[1]] == "Antonio Menéndez De Llano",
                                             "Antonio Menéndez de Llano",
                                             T82_caracteristicas_actores_3[[1]])

#T82_caracteristicas_actores_3$Personaje <- personajes[1:nrow(T82_caracteristicas_actores_3)] #poner nombre personajes

T82_caracteristicas_actores_3_nueva <- T82_caracteristicas_actores_3

#🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------

cono <- dplyr::select(T67_actores_nueva, 1,2)

T82_caracteristicas_actores_3_nueva <- 
    right_join(
        cono,
        T82_caracteristicas_actores_3_nueva,
        by = "Personaje"
    ) 

columnas_revisar <- names(T82_caracteristicas_actores_3_nueva)
for(col in columnas_revisar) {
    T82_caracteristicas_actores_3_nueva[[col]][T82_caracteristicas_actores_3_nueva[[col]] == 0] <- 0.001
}

T82_caracteristicas_actores_3_nueva$saldo2 <- (T82_caracteristicas_actores_3_nueva$Votaría / T82_caracteristicas_actores_3_nueva$`Nunca votaría`)* T82_caracteristicas_actores_3_nueva$`Sí lo conoce`

T82_caracteristicas_actores_3_nueva <- dplyr::select(T82_caracteristicas_actores_3_nueva, -2)
#🛑-🛑-🛑--🛑-🛑-🛑------------------------------------------------------------

#P22 Candidato preferible para diputado MORENA-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
personaje_preferible <- datos_factor %>%
    group_by(V5260) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

# personaje_preferible[[1]] <- ifelse(personaje_preferible[[1]] == "Antonio Menéndez De Llano",
#                                     "Antonio Menéndez de Llano",
#                                     personaje_preferible[[1]])
# 
# personaje_preferible[[1]] <- ifelse(personaje_preferible[[1]] == "Froylán Gámez",
#                                     "Froylan Gámez",
#                                     personaje_preferible[[1]])

personaje_preferible <- personaje_preferible %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

personaje_preferible <- personaje_preferible %>% rename("Personaje_preferible" = 1)

personaje_preferible <- personaje_preferible[order(personaje_preferible$Frecuencia,decreasing=TRUE), ]

#Separar filas: las que NO contienen esas palabras y las que SÍ
personaje_preferible <- personaje_preferible %>%
    mutate(
        orden_final = match(toupper(`Personaje_preferible`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final)%>%
    dplyr::select(-orden_final)

#no toma en cuanta la fila NANA para la suma
personaje_preferible <- personaje_preferible %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

#si aparece NANA coloca 0 en porcentaje valido
if (personaje_preferible[nrow(personaje_preferible), 1] == "NANA") {
    personaje_preferible$Porcentaje_válido[nrow(personaje_preferible)] <- 0.0
}

tot_personaje_preferible <- data.frame(
    Personaje_preferible = "Total",
    Frecuencia = sum(personaje_preferible$Frecuencia),
    Porcentaje = sum(personaje_preferible$Porcentaje),
    Porcentaje_válido = sum(personaje_preferible$Porcentaje_válido)
)

T16_P22_personaje_preferible <- rbind(personaje_preferible, tot_personaje_preferible)

#reemplazos para acentos y demás
T16_P22_personaje_preferible <- T16_P22_personaje_preferible %>%
    mutate(across(1, reemplazar_texto))

T16_P22_personaje_preferible <- dplyr::rename(T16_P22_personaje_preferible,"Personaje"="Personaje_preferible")
T16_P22_personaje_preferible


#🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮🧮---------------------------------------------------------------------------------------------------------------------------
#T84. puntaje actores------------------------------------------------------------------------------------------------------------------------------------------------------
actores_0 <- dplyr::select(T67_actores,Personaje,Buena)
actores_0

#🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
actores_0_1 <- dplyr::select(T67_actores_nueva,Personaje,saldo)
actores_0_1
#🛑-🛑-🛑--🛑-🛑-🛑------------------------------------------------------------


actores_1_1 <- T73_caracteristicas_actores
actores_1_1$honesto <- T73_caracteristicas_actores$Mucho + (0.5*T73_caracteristicas_actores$Algo) 
actores_1_1$cercano <- T73_caracteristicas_actores$Mucho.1 + (0.5*T73_caracteristicas_actores$Algo.1)
actores_1_1 <- dplyr::select(actores_1_1,Personaje,honesto,cercano)
actores_1_1

actores_1_2 <- T77_caracteristicas_actores_2
actores_1_2$conoce <- T77_caracteristicas_actores_2$Mucho + (0.5*T77_caracteristicas_actores_2$Algo) 
actores_1_2$cumple <- T77_caracteristicas_actores_2$Mucho.1 + (0.5*T77_caracteristicas_actores_2$Algo.1)
actores_1_2 <- dplyr::select(actores_1_2,Personaje,conoce,cumple)
actores_1_2

actores_1_3 <- dplyr::select(T82_caracteristicas_actores_3,Personaje,Sí,Votaría)
actores_1_3

#🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
actores_1_3_1 <- dplyr::select(T82_caracteristicas_actores_3_nueva,Personaje,Sí,saldo2)
actores_1_3_1
#🛑-🛑-🛑--🛑-🛑-🛑------------------------------------------------------------

actores_1_4 <- dplyr::select(T16_P22_personaje_preferible,Personaje,Porcentaje)
actores_1_4 <- actores_1_4 %>% filter(!Personaje %in% c("Ninguno", "NS / NC", "Total"))
actores_1_4

per <- actores_1_3$Personaje

actores_1_4 <- actores_1_4 %>%arrange(match(Personaje, per))

lista_actores <- list(actores_0,actores_1_1,actores_1_2,actores_1_4,actores_1_3)

tabulado_actores <- purrr::reduce(lista_actores, left_join, by = "Personaje")
tabulado_actores
tabulado_actores <- na.omit(tabulado_actores)
tabulado_actores
tabulado_actores <- dplyr:::select(tabulado_actores,-Porcentaje, Porcentaje)

#nuevas tablas------------------------------------------------------------------
lista_actores2 <- list(actores_0_1,actores_1_1,actores_1_2,actores_1_4,actores_1_3_1)

tabulado_actores2 <- purrr::reduce(lista_actores2, left_join, by = "Personaje")
tabulado_actores2
tabulado_actores2 <- na.omit(tabulado_actores2)
tabulado_actores2
tabulado_actores2 <- dplyr:::select(tabulado_actores2,-Porcentaje, Porcentaje)
#-------------------------------------------------------------------------------
#vector de valores de puntaje
valores_max <- c(2, 1.25, 0.25, 0.25, 0.25, 1, 2, 3)

#seleccionamos cols númericas
columnas_numericas <- names(tabulado_actores)[2:9]

#buscamos max y damos puntaje
tabulado_puntajes <- tabulado_actores %>%
    mutate(across(all_of(columnas_numericas),
                  ~ case_when(
                      . == max(., na.rm = TRUE) ~ paste0(formatC(. * 100, format = "f", digits = 1), "%", "\n", valores_max[which(columnas_numericas == cur_column())]),  #Máx con puntaje debajo
                      TRUE ~ paste0(formatC(. * 100, format = "f", digits = 1), "%")  #Mantener los valores sin cambio
                  ),
                  .names = "{.col}"))


#Sumamos puntajes
tabulado_puntajes <- tabulado_puntajes %>%
    rowwise() %>%
    mutate(`Puntaje final` = sum(as.numeric(gsub(".*\n", "", c_across(all_of(columnas_numericas)))), na.rm = TRUE)) %>%
    ungroup()

tabulado_puntajes$`Puntaje final` <- car::recode(tabulado_puntajes$`Puntaje final`, "'0' = '-'")

tabulado_puntajes

base1 <- dplyr::select(T67_actores,Personaje,`Sí lo conoce`)
base1$`Sí lo conoce` <- scales::percent(base1$`Sí lo conoce`, accuracy = 0.1) 

base3 <- left_join(tabulado_puntajes,base1)
base3 <- dplyr::select(base3,Personaje,`Sí lo conoce`,everything())
base3

base3 <- base3 %>%
    mutate(
        `Puntaje final` = as.character(`Puntaje final`)
    )


T84_puntaje_actores <- base3

#T84.puntaje actores nuevo-------------------------------------------------------------------------------
#vector de valores de puntaje

#seleccionamos cols númericas********************************************
# columnas_numericas2 <- names(tabulado_actores2)[2:9]
# 
# #buscamos max y damos puntaje
# tabulado_puntajes2 <- tabulado_actores2 %>%
#   mutate(across(all_of(columnas_numericas2), 
#                 ~ case_when(
#                   . == max(., na.rm = TRUE) ~ paste0(formatC(. * 100, format = "f", digits = 1), "%", "\n", valores_max[which(columnas_numericas2 == cur_column())]),  #Máx con puntaje debajo
#                   TRUE ~ paste0(formatC(. * 100, format = "f", digits = 1), "%")  #Mantener los valores sin cambio
#                 ), 
#                 .names = "{.col}"))
# 
# #Sumamos puntajes
# tabulado_puntajes2 <- tabulado_puntajes2 %>%
#   rowwise() %>%
#   mutate(`Puntaje final` = sum(as.numeric(gsub(".*\n", "", c_across(all_of(columnas_numericas2)))), na.rm = TRUE)) %>%
#   ungroup()
# 
# tabulado_puntajes2$`Puntaje final` <- car::recode(tabulado_puntajes2$`Puntaje final`, "'0' = '-'")
# 
# tabulado_puntajes2
# 
# base1 <- dplyr::select(T67_actores,Personaje,`Sí lo conoce`)
# base1$`Sí lo conoce` <- scales::percent(base1$`Sí lo conoce`, accuracy = 0.1) 
# 
# base4 <- left_join(tabulado_puntajes2,base1)
# base4 <- dplyr::select(base4,Personaje,`Sí lo conoce`,everything())
# base4
# 
# T84_puntaje_actores_nueva <- base4

#**********************************************************
# seleccionamos columnas numéricas
columnas_numericas2 <- names(tabulado_actores2)[2:9]

# columnas que deben quedar como número
cols_saldo <- c("saldo", "saldo2")

#  Formateo y asignación de puntaje

tabulado_puntajes2 <- tabulado_actores2 %>%
    mutate(across(
        all_of(columnas_numericas2),
        ~ case_when(
            
            # 🟢 saldo y saldo2 (2 decimales)
            cur_column() %in% cols_saldo &
                . == max(., na.rm = TRUE) ~
                paste0(
                    formatC(., format = "f", digits = 2),
                    "\n",
                    valores_max[which(columnas_numericas2 == cur_column())]
                ),
            
            cur_column() %in% cols_saldo ~
                formatC(., format = "f", digits = 2),
            
            # 🔵 columnas porcentaje
            . == max(., na.rm = TRUE) ~
                paste0(
                    formatC(. * 100, format = "f", digits = 1),
                    "%",
                    "\n",
                    valores_max[which(columnas_numericas2 == cur_column())]
                ),
            
            TRUE ~
                paste0(formatC(. * 100, format = "f", digits = 1), "%")
        ),
        .names = "{.col}"
    ))

# Sumar SOLO puntajes asignados (si hay \n)

tabulado_puntajes2 <- tabulado_puntajes2 %>%
    rowwise() %>%
    mutate(
        Puntaje_num = sum(
            ifelse(
                grepl("\n", c_across(all_of(columnas_numericas2))),
                as.numeric(sub(".*\n", "", c_across(all_of(columnas_numericas2)))),
                0
            ),
            na.rm = TRUE
        )
    ) %>%
    ungroup()

#  Dejar SOLO el máximo global

tabulado_puntajes2 <- tabulado_puntajes2 %>%
    mutate(
        `Puntaje final` = ifelse(
            Puntaje_num == 0,
            "-",
            ifelse(
                Puntaje_num %% 1 == 0,
                as.character(Puntaje_num),                     # entero
                formatC(Puntaje_num, format = "f", digits = 2) # con decimales
            )
        )
    ) %>%
    select(-Puntaje_num)

#  Agregar conocimiento

base1 <- dplyr::select(T67_actores, Personaje, `Sí lo conoce`)
base1$`Sí lo conoce` <-
    scales::percent(base1$`Sí lo conoce`, accuracy = 0.1)

base4 <- left_join(tabulado_puntajes2, base1, by = "Personaje")
base4 <- dplyr::select(base4, Personaje, `Sí lo conoce`, everything())

T84_puntaje_actores_nueva <- base4
T84_puntaje_actores_nueva


#++++++++++++++++++++++++++++++++-----------------------------------------------------------------------------------------------------------------------------------------
#++++++++++++++++++++++++++++++++-----------------------------------------------------------------------------------------------------------------------------------------

#P23 Mejor diputado federal-------------------------------------------------------------------------------------------------------------------------
T17_P23_mejor_diputado <- procesar_clasificacion_gobernador(datos_factor, "V5262",
                                                            "P23.Mejor_diputado_federal",
                                                            al_final, "T_P23_mejor_diputado",frec_tot
)

T17_P23_mejor_diputado

#P24 peor diputado federal-------------------------------------------------------------------------------------------------------------------------
T18_P24_peor_diputado <- procesar_clasificacion_gobernador(datos_factor, "V5264",
                                                           "P24.Peor_diputado_federal",
                                                           al_final, "T_P24_peor_diputado",frec_tot,
)

T18_P24_peor_diputado

#P25 Personaje más cercano gobernador(a)-------------------------------------------------------------------------------------------------------------------
T19_P25_cercano_gob <- procesar_clasificacion_gobernador(datos_factor, "V5265",
                                                         "P25.Cercano_gobernador",
                                                         al_final, "T_P25_cercano_gob",frec_tot
)
T19_P25_cercano_gob

#P26 Personaje más cercano presidenta-------------------------------------------------------------------------------------------------------------------
T20_P26_cercano_pdta <- procesar_clasificacion_gobernador(datos_factor, "V5266",
                                                          "P26.Cercano_presidenta",
                                                          al_final, "T_P26_cercano_pdta",frec_tot
)


T20_P26_cercano_pdta

#P27 Mayor posibilidades de ganar--------------------------------------------------------------------------------------------------------------------------------
T21_P27_P27.Mayor_posibilidad_ganar <- procesar_clasificacion_gobernador(datos_factor, "V5267",
                                                                         "P27.Mayor_posibilidad_ganar",
                                                                         al_final, "T_P27_P27.Mayor_posibilidad_ganar",frec_tot
)

T21_P27_P27.Mayor_posibilidad_ganar

#P28 Problema a resolver primero--------------------------------------------------------------------------------------------------------------------------------
procesar_problema <- function(data, var, etiqueta) {
    
    library(dplyr)
    library(tidyr)
    library(rlang)
    
    var_sym <- ensym(var)
    
    niveles <- c("1", "2", "3", "NANA")
    
    tabla <- data %>%
        mutate(
            tmp = as.character(!!var_sym)
        ) %>%
        group_by(tmp) %>%
        summarise(Frecuencia = sum(pond), .groups = "drop") %>%
        complete(tmp = niveles, fill = list(Frecuencia = 0)) %>%
        arrange(match(tmp, niveles))
    
    names(tabla)[1] <- etiqueta
    
    return(tabla)
}

problema_1  <- procesar_problema(datos_factor, V5330, "La falta de empleo")
problema_2  <- procesar_problema(datos_factor, V5331, "La inseguridad")
problema_3  <- procesar_problema(datos_factor, V5332, "Los problemas económicos")
problema_4  <- procesar_problema(datos_factor, V5333, "La falta de agua")
problema_5  <- procesar_problema(datos_factor, V5334, "El problema del transporte público")
problema_6  <- procesar_problema(datos_factor, V5335, "La mala calidad de la educación pública")
problema_7  <- procesar_problema(datos_factor, V5336, "La falta de apoyos sociales")
problema_8  <- procesar_problema(datos_factor, V5337, "Mala calidad de los servicios de salud")
problema_9  <- procesar_problema(datos_factor, V5338, "Los baches y las calles en mal estado")
problema_10 <- procesar_problema(datos_factor, V5339, "Los problemas de inundaciones y drenaje")
problema_11 <- procesar_problema(datos_factor, V5340, "Todos")
problema_12 <- procesar_problema(datos_factor, V5341, "Ninguno")
problema_13 <- procesar_problema(datos_factor, V5342, "No sabe")
problema_14 <- procesar_problema(datos_factor, V5343, "No contestó")

agregar_nombre <- function(df, nombre) {
    df %>%
        rename(RESPUESTA = 1) %>%
        mutate(PROBLEMA = nombre)
}

lista_problemas <- list(
    agregar_nombre(problema_1,  "La falta de empleo"),
    agregar_nombre(problema_2,  "La inseguridad"),
    agregar_nombre(problema_3,  "Los problemas económicos"),
    agregar_nombre(problema_4,  "La falta de agua"),
    agregar_nombre(problema_5,  "El problema del transporte público"),
    agregar_nombre(problema_6,  "La mala calidad de la educación pública"),
    agregar_nombre(problema_7,  "La falta de apoyos sociales"),
    agregar_nombre(problema_8,  "Mala calidad de los servicios de salud"),
    agregar_nombre(problema_9,  "Los baches y las calles en mal estado"),
    agregar_nombre(problema_10, "Los problemas de inundaciones y drenaje"),
    agregar_nombre(problema_11, "Todos"),
    agregar_nombre(problema_12, "Ninguno"),
    agregar_nombre(problema_13, "No sabe"),
    agregar_nombre(problema_14, "No contestó")
)

tabla_problemas_final <- bind_rows(lista_problemas) %>%
    mutate(
        RESPUESTA = as.character(RESPUESTA),
        Frecuencia = as.numeric(Frecuencia)
    ) %>%
    pivot_wider(
        names_from = RESPUESTA,
        values_from = Frecuencia,
        values_fill = 0
    ) %>%
    select(PROBLEMA, `1`, `2`, `3`, NANA)

tabla_problemas_final

T22_P28_problemas <- dplyr::select(tabla_problemas_final,PROBLEMA, `1` )

T22_P28_problemas <- dplyr::rename(T22_P28_problemas, Frecuencia = `1`)

T22_P28_problemas <- T22_P28_problemas %>%
    mutate(
        Porcentaje_válido = (Frecuencia / frec_tot)
    )

T22_P28_problemas <- T22_P28_problemas[order(T22_P28_problemas$Porcentaje_válido,decreasing=TRUE), ]

T22_P28_problemas <- T22_P28_problemas %>%
    mutate(
        orden_final = match(PROBLEMA, al_final),
        orden_final = ifelse(is.na(orden_final), 0, orden_final)
    ) %>%
    arrange(orden_final) %>%
    select(-orden_final)

#P29 Partido voto gobernador 📩📩📩📩📩📩️ ----------------------------------------------------------------------------------------------------------------------------------------
orden_p14.1 <- c("PAN", "PRI","PT","PARTIDO VERDE","MOVIMIENTO CIUDADANO","MORENA", #cambiar partidos
                 "Otro","No iría a votar","Anularía su voto","Es secreto","Cualquiera","Ninguno","No sabe","No contestó")

orden_p19 <-  orden_p14.1

part_voto <- datos_factor %>%
    group_by(V1883) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop") %>%
    rename("Partido_voto" = 1)


base_partidos <- data.frame(
    Partido_voto = orden_p19,
    stringsAsFactors = FALSE
)

part_voto <- base_partidos %>%
    left_join(part_voto, by = "Partido_voto") %>%
    mutate(
        Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia)
    )


part_voto <- part_voto %>%
    mutate(
        Porcentaje = Frecuencia / frec_tot,
        Partido_voto = as.character(Partido_voto),
        orden_final = match(Partido_voto, orden_p19),
        orden_final = ifelse(is.na(orden_final), 0, orden_final)
    ) %>%
    arrange(orden_final, Partido_voto) %>%
    select(-orden_final) %>%
    mutate(
        Porcentaje_válido = Frecuencia / sum(Frecuencia)
    )

tot_part_voto <- data.frame(
    `Partido_voto` = "Total",
    Frecuencia = sum(part_voto$Frecuencia),
    Porcentaje = sum(part_voto$Porcentaje),
    Porcentaje_válido = sum(part_voto$Porcentaje_válido)
)

part_voto
T42_P19_part_voto <- part_voto

#P30 Voto otro partido 2da opcion---------------------------------------------------------------------------------------------------------------------
otro_partido <- datos_factor %>%
    group_by(V5269) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop") %>%
    rename("Otro_partido_voto" = 1)


base_partidos <- data.frame(
    Otro_partido_voto = orden_p19,
    stringsAsFactors = FALSE
)

otro_partido <- base_partidos %>%
    left_join(otro_partido, by = "Otro_partido_voto") %>%
    mutate(
        Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia)
    )


otro_partido <- otro_partido %>%
    mutate(
        Porcentaje = Frecuencia / frec_tot,
        Otro_partido_voto = as.character(Otro_partido_voto),
        orden_final = match(Otro_partido_voto, orden_p19),
        orden_final = ifelse(is.na(orden_final), 0, orden_final)
    ) %>%
    arrange(orden_final, Otro_partido_voto) %>%
    select(-orden_final) %>%
    mutate(
        Porcentaje_válido = Frecuencia / sum(Frecuencia)
    )

tot_otro_partido <- data.frame(
    `Otro_partido_voto` = "Total",
    Frecuencia = sum(otro_partido$Frecuencia),
    Porcentaje = sum(otro_partido$Porcentaje),
    Porcentaje_válido = sum(otro_partido$Porcentaje_válido)
)

otro_partido
T43_P20_otro_partido <- otro_partido

#P31 Partido voto p municipal 📩📩📩📩📩📩️ ----------------------------------------------------------------------------------------------------------------------------------------
part_voto_mun <- datos_factor %>%
    group_by(V5271) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop") %>%
    rename("Partido_voto_mun" = 1)


base_partidos <- data.frame(
    Partido_voto_mun = orden_p19,
    stringsAsFactors = FALSE
)

part_voto_mun <- base_partidos %>%
    left_join(part_voto_mun, by = "Partido_voto_mun") %>%
    mutate(
        Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia)
    )


part_voto_mun <- part_voto_mun %>%
    mutate(
        Porcentaje = Frecuencia / frec_tot,
        Partido_voto_mun = as.character(Partido_voto_mun),
        orden_final = match(Partido_voto_mun, orden_p19),
        orden_final = ifelse(is.na(orden_final), 0, orden_final)
    ) %>%
    arrange(orden_final, Partido_voto_mun) %>%
    select(-orden_final) %>%
    mutate(
        Porcentaje_válido = Frecuencia / sum(Frecuencia)
    )

tot_part_voto_mun <- data.frame(
    `Partido_voto_mun` = "Total",
    Frecuencia = sum(part_voto_mun$Frecuencia),
    Porcentaje = sum(part_voto_mun$Porcentaje),
    Porcentaje_válido = sum(part_voto_mun$Porcentaje_válido)
)

part_voto_mun
T42_P19_part_voto_mun <- part_voto_mun

#P32 Voto Diputado federal---------------------------------------------------------------------------------------------------------------------
part_voto_df <- datos_factor %>%
    group_by(V5273) %>%
    summarise(Frecuencia = sum(pond), .groups = "drop") %>%
    rename("Partido_voto_df" = 1)


base_partidos <- data.frame(
    Partido_voto_df = orden_p19,
    stringsAsFactors = FALSE
)

part_voto_df <- base_partidos %>%
    left_join(part_voto_df, by = "Partido_voto_df") %>%
    mutate(
        Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia)
    )


part_voto_df <- part_voto_df %>%
    mutate(
        Porcentaje = Frecuencia / frec_tot,
        Partido_voto_df = as.character(Partido_voto_df),
        orden_final = match(Partido_voto_df, orden_p19),
        orden_final = ifelse(is.na(orden_final), 0, orden_final)
    ) %>%
    arrange(orden_final, Partido_voto_df) %>%
    select(-orden_final) %>%
    mutate(
        Porcentaje_válido = Frecuencia / sum(Frecuencia)
    )

tot_part_voto_df <- data.frame(
    `Partido_voto_df` = "Total",
    Frecuencia = sum(part_voto_df$Frecuencia),
    Porcentaje = sum(part_voto_df$Porcentaje),
    Porcentaje_válido = sum(part_voto_df$Porcentaje_válido)
)

part_voto_df
T42_P19_part_voto_df <- part_voto_df

# #45.P22 Preferencia intención -----------------------------------------------------------------------------------------------------------------------------
#filas_a_quitar <- c("NANA", "Ninguno","NINGUNO", "NS / NC", "Otro","Total")
filas_a_quitar <- c("NANA","Total")

# T41_P18_clean <- T5_P1_ident_part_ajustado[
#   !((T5_P1_ident_part_ajustado$Identifica_partido) %in% filas_a_quitar),
#   !names(T5_P1_ident_part_ajustado) %in% c("Frecuencia", "Porcentaje")]
# T41_P18_clean <- dplyr::rename(T41_P18_clean, "Partido"="Identifica_partido","Partido se identifica más"="Porcentaje_válido")

T42_P19_clean <- T42_P19_part_voto[
    !((T42_P19_part_voto$Partido_voto) %in% filas_a_quitar),
    !names(T42_P19_part_voto) %in% c("Frecuencia", "Porcentaje")]
T42_P19_clean <- dplyr::rename(T42_P19_clean, "Partido"="Partido_voto","1° Opción de voto"="Porcentaje_válido")

T43_P20_clean <- T43_P20_otro_partido[
    !((T43_P20_otro_partido$Otro_partido_voto) %in% filas_a_quitar),
    !names(T43_P20_otro_partido) %in% c("Frecuencia", "Porcentaje")]
T43_P20_clean <- dplyr::rename(T43_P20_clean, "Partido"="Otro_partido_voto","2° Opción de voto"="Porcentaje_válido")

T44_P21_clean <- T42_P19_part_voto_mun[
    !((T42_P19_part_voto_mun$Partido_voto_mun) %in% filas_a_quitar),
    !names(T42_P19_part_voto_mun) %in% c("Frecuencia", "Porcentaje")]
T44_P21_clean <- dplyr::rename(T44_P21_clean, "Partido"="Partido_voto_mun","Voto p mun"="Porcentaje_válido")

T44_P22_clean <- T42_P19_part_voto_df[
    !((T42_P19_part_voto_df$Partido_voto_df) %in% filas_a_quitar),
    !names(T42_P19_part_voto_df) %in% c("Frecuencia", "Porcentaje")]
T44_P22_clean <- dplyr::rename(T44_P22_clean, "Partido"="Partido_voto_df","Voto df"="Porcentaje_válido")


union_votos <- list(#T41_P18_clean,
    T42_P19_clean, 
    T43_P20_clean, 
    T44_P21_clean,
    T44_P22_clean)

T45_P22_Preferencia_intencion <- reduce(union_votos, function(x, y) {
    full_join(x, y, by = "Partido")
})

T45_P22_Preferencia_intencion[is.na(T45_P22_Preferencia_intencion)] <- 0

T45_P22_Preferencia_intencion

T45_P22_Preferencia_intencion

orden_prefe_inten <- c("PAN","PRI","PT","PARTIDO VERDE","MOVIMIENTO CIUDADANO","MORENA","Otro","No iría a votar","Anularía su voto","Es secreto",
                       "Cualquiera","Ninguno","No sabe","No contestó")

T45_P22_Preferencia_intencion <- T45_P22_Preferencia_intencion %>%
    
    # Forzar todas las categorías del orden en la primera columna
    right_join(
        data.frame(Partido = orden_prefe_inten, stringsAsFactors = FALSE),
        by = "Partido"
    ) %>%
    
    # Todo lo que no exista -> 0 en todas las columnas numéricas
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
    
    # Convertir en factor ordenado
    mutate(
        Partido = factor(Partido, levels = orden_prefe_inten)
    ) %>%
    
    # Ordenar según ese factor
    arrange(Partido)

#********************************-----------------------------------------------------------------------------------------------------------------------------------------
#P33 Conocimiento Revocacion ---------------------------------------------------------------------------------------------------------------------------------------
orden_si_no <- c("Sí","No","No sabe","No contestó")

T23_P33_revocacion <- funcion_opciones(datos_factor,V2877,
                                       "P33.Conoce_revocacion",
                                       frec_tot,orden_si_no,al_final,
                                       ordenar_desc = F
)

T23_P33_revocacion

#P34 revocacion_voto_mismo_dia ---------------------------------------------------------------------------------------------------------------------------------------
T24_P34_revocacion_voto_mismo_dia <- funcion_opciones(datos_factor,V2368,
                                                      "P34.Revocacion_voto_mismo_dia",
                                                      frec_tot,orden_si_no,al_final,
                                                      ordenar_desc = F
)
T24_P34_revocacion_voto_mismo_dia

#P35 Participar en revocacion -------------------------------------------------------------------------------------------------------
orden_par_rev <- c("Muy dispuesto","Algo dispuesto","Poco dispuesto","Nada dispuesto","No sabe","No contestó")

T25_P35_participar_revocacion <- funcion_opciones(datos_factor,V3140,
                                                  "P35.Dispuesto_participar_revocacion",
                                                  frec_tot,orden_par_rev,al_final
)

T25_P35_participar_revocacion

#P36 Revocacion presidenta -------------------------------------------------------------------------------------------------------
orden_rev_pdta <- c("Que siga en la presidencia de la república",
                    "Que se le revoque el mandato por pérdida de confianza",
                    "No iría a votar","No sabe","No contestó")

T26_P36_revoacion_pdta <- funcion_opciones(datos_factor,V3141,
                                           "P36.Revocacion_pdta",
                                           frec_tot,orden_rev_pdta,al_final,
                                           ordenar_desc = F
)

T26_P36_revoacion_pdta

#P37 reeleccion_p_mun ---------------------------------------------------------------------------------------------------------------------------------------
T27_P37_reeleccion_p_mun <- funcion_opciones(datos_factor,V5275,
                                             "P37.Reeleccion_p_mun",
                                             frec_tot,orden_si_no,al_final,
                                             ordenar_desc = F
)

T27_P37_reeleccion_p_mun

#P38 de_acuerdo_con_reeleccion_p_mun ---------------------------------------------------------------------------------------------------------------------------------------
orden_ree_p_mun <- c("De acuerdo","En desacuerdo","Ni de acuerdo, ni desacuerdo","No sabe","No contestó")

T28_P38_de_acuerdo_con_reeleccion_p_mun <- funcion_opciones(datos_factor,V5276,
                                                            "P38.De_acuerdo_con_reeleccion_p_mun",
                                                            frec_tot,orden_ree_p_mun,al_final,
                                                            ordenar_desc = F
)

T28_P38_de_acuerdo_con_reeleccion_p_mun

#P39 Partido con el que le iria mejor------------------------------------------------------------------------------------------------------------
orden_p_mejor <- c("PAN", "PRI","PT","PARTIDO VERDE","MOVIMIENTO CIUDADANO","MORENA", 
                   "Otro","Es secreto","Cualquiera","Ninguno","No sabe","No contestó")

T29_P39_iria_mejor_partido <- funcion_opciones(datos_factor,V5277,
                                               "P39.Iria_mejor_partido",
                                               frec_tot,orden_p_mejor,al_final,
                                               ordenar_desc = F
)

#P40 Partido con el que le iria peor------------------------------------------------------------------------------------------------------------
T30_P40_iria_peor_partido <- funcion_opciones(datos_factor,V5279,
                                              "P40.Iria_peor_partido",
                                              frec_tot,orden_p_mejor,al_final,
                                              ordenar_desc = F
)


#P41 Partido más posibilidades de ganar------------------------------------------------------------------------------------------------------------
T31_P41_part_pos_ganar <- funcion_opciones(datos_factor,V5281,
                                           "P41.Partido_mas_posibilidades_de_ganar",
                                           frec_tot,orden_p_mejor,al_final,
                                           ordenar_desc = F
)

#UNIMOS TABLAS
unir_tabulados <- function(...) {
    
    listas <- list(...)
    
    tablas_limpias <- lapply(listas, function(tab) {
        
        nombre_col <- names(tab)[1]
        
        tab %>%
            filter(.data[[nombre_col]] != "Total") %>%
            select(
                Partido = all_of(nombre_col),
                Porcentaje_válido
            ) %>%
            rename(!!nombre_col := Porcentaje_válido)
    })
    
    # Unir todas por Partido
    resultado <- Reduce(function(x, y) full_join(x, y, by = "Partido"), tablas_limpias)
    
    return(resultado)
}

tabla_consolidada <- unir_tabulados(
    T29_P39_iria_mejor_partido,
    T30_P40_iria_peor_partido,
    T31_P41_part_pos_ganar
)

#P42 medio_comunicacion ---------------------------------------------------------------------------------------------------------------------------------------
orden_redes <- c("Internet y Redes sociales","Televisión","Periódico","Radio","Otro","Todos","Ninguno","No sabe","No contestó")

T32_P42_medio_comunicacion <- funcion_opciones(datos_factor,V3149,
                                               "P42.Medio_comunicacion",
                                               frec_tot,orden_redes,al_final
)

#P43 Red social pertenece-----------------------------------------------------------------------------------------------------------------------------------
niveles_P43 <- c("Sí pertenece", "No pertenece", "No sabe / No recuerda", "No contestó")

vars_P43 <- c("V5283","V5284","V5285","V5286")
nombres_P43 <- c("Facebook","Twitter o X","Instagram","TikTok")

T33_P43_redes <- tabla_redes_general(datos_factor,vars_P43,nombres_P43,niveles_P43,pond,frec_tot)

T33_P43_redes

#P43B Que tanto usa red socialL------------------------------------------------------------------------------------------------------------------------------
niveles_P43B <- c("No la usa a diario","1 o 2 veces al día","3 o 4 veces al día","Más de 4 veces al día",
                  "No sabe / No recuerda","No contestó")

vars_P43B <- c("V5287","V5288","V5289","V5290")

nombres_P43B <- c("Facebook","Twitter o X","Instagram","TikTok")

T34_P43B_redes <- tabla_redes_usa(datos_factor,vars_P43B,nombres_P43B,niveles_P43B,pond)

T34_P43B_redes

#55.P30 Estudios ----------------------------------------------------------------------------------------------------------------------------------------------------
orden_p30 <- c("No estudio","Primaria incompleta","Primaria completa","Secundaria incompleta","Secundaria completa",
               "Preparatoria incompleta / Carrera comercial/ Técnica","Preparatoria completa","Licenciatura incompleta","Licenciatura completa","Diplomado o Maestría / Doctorado")
estudios <- datos_factor %>%
    group_by(V184) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

estudios <- estudios %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

estudios <- estudios[order(estudios$Frecuencia,decreasing=TRUE), ]
estudios$V184 <- trimws(estudios$V184)
estudios <- rename(estudios, "Estudios"="V184")

#Separar filas: las que NO contienen esas palabras y las que SÍ
estudios <- estudios %>%
    mutate(
        orden_final = match(toupper(`Estudios`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(match(`Estudios`,orden_p30))%>%
    dplyr::select(-orden_final)

estudios <- estudios %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_estudios <- data.frame(
    `Estudios` = "Total",
    Frecuencia = sum(estudios$Frecuencia),
    Porcentaje = sum(estudios$Porcentaje),
    Porcentaje_válido = sum(estudios$Porcentaje_válido)
)

T55_P30_estudios <- rbind(estudios, tot_estudios)
print(T55_P30_estudios)

#56.P30.1 Estudios agrup----------------------------------------------------------------------------------------------------------------------------------------------------
T56_P30.1_estudios_agrup <- T55_P30_estudios

orden_p30.1 <- c("No completo primaria","Primaria o Secundaria", "Preparatoria o carrera técnica","Licenciatura","Posgrado")
# Crear los grupos
estudios_agrup <- c(
    rep("No completo primaria", 2),
    rep("Primaria o Secundaria", 3),
    rep("Preparatoria o carrera técnica", 2),
    rep("Licenciatura", 2),
    "Posgrado","Total"
)

# Agregar el grupo como columna
T56_P30.1_estudios_agrup$Estudios_agrupados <- estudios_agrup

T56_P30.1_estudios_agrup <- dplyr::select(T56_P30.1_estudios_agrup,Estudios_agrupados,Frecuencia)
print(T56_P30.1_estudios_agrup)

T56_P30.1_estudios_agrup <- aggregate(Frecuencia ~ Estudios_agrupados, data = T56_P30.1_estudios_agrup, sum)

T56_P30.1_estudios_agrup <- T56_P30.1_estudios_agrup %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

T56_P30.1_estudios_agrup$Porcentaje_válido <- T56_P30.1_estudios_agrup$Porcentaje

T56_P30.1_estudios_agrup <- T56_P30.1_estudios_agrup %>% arrange(match(Estudios_agrupados,orden_p30.1))
print(T56_P30.1_estudios_agrup)

#57.P31 Hogar autos ----------------------------------------------------------------------------------------------------------------------------------------------------
orden_p31 <- c("0","1","2 o más","NS / NC")
hog_aut <- datos_factor %>%
    group_by(V185) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

hog_aut <- hog_aut %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

hog_aut <- hog_aut[order(hog_aut$Frecuencia,decreasing=TRUE), ]
hog_aut$V185 <- trimws(hog_aut$V185)
hog_aut <- rename(hog_aut, "Hogar_autos"="V185")

#Separar filas: las que NO contienen esas palabras y las que SÍ
hog_aut <- hog_aut %>%
    mutate(
        orden_final = match(toupper(`Hogar_autos`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(match(Hogar_autos,orden_p31))%>%
    dplyr::select(-orden_final)

hog_aut <- hog_aut %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_hog_aut <- data.frame(
    `Hogar_autos` = "Total",
    Frecuencia = sum(hog_aut$Frecuencia),
    Porcentaje = sum(hog_aut$Porcentaje),
    Porcentaje_válido = sum(hog_aut$Porcentaje_válido)
)

T57_P31_hog_aut <- rbind(hog_aut, tot_hog_aut)
print(T57_P31_hog_aut)

#58.P32 Hogar baños ----------------------------------------------------------------------------------------------------------------------------------------------------
hog_ba <- datos_factor %>%
    group_by(V186) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

hog_ba <- hog_ba %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

#hog_ba <- hog_ba[order(hog_ba$Frecuencia,decreasing=TRUE), ]
hog_ba$V186 <- trimws(hog_ba$V186)
hog_ba <- rename(hog_ba, "Hogar_baños"="V186")

#Separar filas: las que NO contienen esas palabras y las que SÍ
hog_ba <- hog_ba %>%
    mutate(
        orden_final = match(toupper(`Hogar_baños`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final)%>%
    dplyr::select(-orden_final)

hog_ba <- hog_ba %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_hog_ba <- data.frame(
    `Hogar_baños` = "Total",
    Frecuencia = sum(hog_ba$Frecuencia),
    Porcentaje = sum(hog_ba$Porcentaje),
    Porcentaje_válido = sum(hog_ba$Porcentaje_válido)
)

T58_P32_hog_ba <- rbind(hog_ba, tot_hog_ba)
print(T58_P32_hog_ba)

#59.P33 Hogar internet---------------------------------------------------------------------------------------------------------------------------------------
hog_inter <- datos_factor %>%
    group_by(V187) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

hog_inter <- hog_inter %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

hog_inter <- hog_inter[order(hog_inter$Frecuencia,decreasing=TRUE), ]
hog_inter$V187 <- trimws(hog_inter$V187)
hog_inter <- rename(hog_inter, "Hogar_internet"="V187")

#Separar filas: las que NO contienen esas palabras y las que SÍ
hog_inter <- hog_inter %>%
    mutate(
        orden_final = match(toupper(`Hogar_internet`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final)%>%
    dplyr::select(-orden_final)

hog_inter <- hog_inter %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_hog_inter <- data.frame(
    `Hogar_internet` = "Total",
    Frecuencia = sum(hog_inter$Frecuencia),
    Porcentaje = sum(hog_inter$Porcentaje),
    Porcentaje_válido = sum(hog_inter$Porcentaje_válido)
)

T59_P33_hog_inter <- rbind(hog_inter, tot_hog_inter)
print(T59_P33_hog_inter)

#60.P34 Hogar personas 14 ----------------------------------------------------------------------------------------------------------------------------------------------------
hog_p_14_a <- datos_factor %>%
    group_by(V188) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

hog_p_14_a <- hog_p_14_a %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

#hog_p_14_a <- hog_p_14_a[order(hog_p_14_a$Frecuencia,decreasing=TRUE), ]
hog_p_14_a$V188 <- trimws(hog_p_14_a$V188)
hog_p_14_a <- rename(hog_p_14_a, "Hogar_personas_14_años"="V188")

#Separar filas: las que NO contienen esas palabras y las que SÍ
hog_p_14_a <- hog_p_14_a %>%
    mutate(
        orden_final = match(toupper(`Hogar_personas_14_años`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final)%>%
    dplyr::select(-orden_final)

hog_p_14_a <- hog_p_14_a %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_hog_p_14_a <- data.frame(
    `Hogar_personas_14_años` = "Total",
    Frecuencia = sum(hog_p_14_a$Frecuencia),
    Porcentaje = sum(hog_p_14_a$Porcentaje),
    Porcentaje_válido = sum(hog_p_14_a$Porcentaje_válido)
)

T60_P34_hog_p_14_a <- rbind(hog_p_14_a, tot_hog_p_14_a)
print(T60_P34_hog_p_14_a)

#61.P35 Vivienda cuartos ----------------------------------------------------------------------------------------------------------------------------------------------------
viv_cuartos <- datos_factor %>%
    group_by(V189) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

viv_cuartos <- viv_cuartos %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

#viv_cuartos <- viv_cuartos[order(viv_cuartos$Frecuencia,decreasing=TRUE), ]
viv_cuartos$V189 <- trimws(viv_cuartos$V189)
viv_cuartos <- rename(viv_cuartos, "Vivienda_cuartos"="V189")

#Separar filas: las que NO contienen esas palabras y las que SÍ
viv_cuartos <- viv_cuartos %>%
    mutate(
        orden_final = match(toupper(`Vivienda_cuartos`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(orden_final)%>%
    dplyr::select(-orden_final)

viv_cuartos <- viv_cuartos %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_viv_cuartos <- data.frame(
    `Vivienda_cuartos` = "Total",
    Frecuencia = sum(viv_cuartos$Frecuencia),
    Porcentaje = sum(viv_cuartos$Porcentaje),
    Porcentaje_válido = sum(viv_cuartos$Porcentaje_válido)
)

T61_P35_viv_cuartos <- rbind(viv_cuartos, tot_viv_cuartos)
print(T61_P35_viv_cuartos)

#62.P36 AMAI ----------------------------------------------------------------------------------------------------------------------------------------------------
orden_p36 <- c("A/B","C+","C","C-","D+","D","E")
amai <- datos_factor %>%
    group_by(AMAI) %>% 
    summarise(Frecuencia = sum(pond), .groups = "drop")

amai <- amai %>%
    mutate(
        Porcentaje = (Frecuencia / frec_tot)
    )

amai <- amai[order(amai$Frecuencia,decreasing=TRUE), ]
amai$AMAI <- trimws(amai$AMAI)

#Separar filas: las que NO contienen esas palabras y las que SÍ
amai <- amai %>%
    mutate(
        orden_final = match(toupper(`AMAI`), toupper(al_final)),   #NA si no está
        orden_final = ifelse(is.na(orden_final), -1, orden_final)  #los "no finales" van primero
    ) %>%
    arrange(match(`AMAI`,orden_p36))%>%
    dplyr::select(-orden_final)

amai <- amai %>%
    mutate(
        Porcentaje_válido = (Frecuencia / sum(Frecuencia)) 
    )

tot_amai <- data.frame(
    `AMAI` = "Total",
    Frecuencia = sum(amai$Frecuencia),
    Porcentaje = sum(amai$Porcentaje),
    Porcentaje_válido = sum(amai$Porcentaje_válido)
)

T62_P36_amai <- rbind(amai, tot_amai)
print(T62_P36_amai)

#++++++++++++++++++++++++++++++++-------------------------------------------------------------------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++-------------------------------------------------------------------------------------------------------------------------------
#******************************************---------------------------------------------------------------------------------------------------------------------------------------
#📝LISTA DE DFS📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝---------------------------------------------------------------------------------------------------------------------------------------------------
#📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝📝------------------
df_tabulado_1 <-  T1_municipio
df_tabulado_2 <-  T2_tipo_seccion
df_tabulado_3 <-  T3_edad
df_tabulado_4 <-  T4_ocupacion
df_tabulado_5 <-  T5_genero
df_tabulado_6 <-  T5_P1_ident_part
df_tabulado_7 <-  T6_P2_aprob_pdta
df_tabulado_8 <-  T7_P3_clas_gob_pdta_mejor
df_tabulado_9 <-  T8_P4_clas_gob_pdta_peor
df_tabulado_10 <- T9_P5_Aprobación_gobernador
df_tabulado_11 <-  T10_P6_clasificación_de_gobierno
df_tabulado_12 <-  T11_P7_Clasificación_peor_de_gobierno
df_tabulado_13 <-  T12_P8_aprob_P_municipal
df_tabulado_14 <-  T13_P9_mejor_clasificación_mun
df_tabulado_15 <-  T14_P10_peor_Clasificación_mun
df_tabulado_16 <-  T15_P11_partido_interes_familia
df_tabulado_17 <-  T16_P12_Principal_problema_edo
df_tabulado_18 <-  T67_actores
df_tabulado_19 <-  T67_actores_nueva
df_tabulado_20 <-  T70_partido_relaciona
df_tabulado_21 <-  T73_caracteristicas_actores_acum
df_tabulado_22 <-  T77_caracteristicas_actores_2
df_tabulado_23 <-  T82_caracteristicas_actores_3
df_tabulado_24 <-  T82_caracteristicas_actores_3_nueva
df_tabulado_25 <-  T84_puntaje_actores
df_tabulado_26 <-  T84_puntaje_actores_nueva
df_tabulado_27 <-  T17_P23_mejor_diputado
df_tabulado_28 <-  T18_P24_peor_diputado
df_tabulado_29 <-  T19_P25_cercano_gob
df_tabulado_30 <-  T20_P26_cercano_pdta
df_tabulado_31 <-  T21_P27_P27.Mayor_posibilidad_ganar
df_tabulado_32 <-  T22_P28_problemas
df_tabulado_33 <-  T45_P22_Preferencia_intencion
df_tabulado_34 <-  T23_P33_revocacion
df_tabulado_35 <-  T24_P34_revocacion_voto_mismo_dia
df_tabulado_36 <-  T25_P35_participar_revocacion
df_tabulado_37 <-  T26_P36_revoacion_pdta
df_tabulado_38 <-  T27_P37_reeleccion_p_mun
df_tabulado_39 <-  T28_P38_de_acuerdo_con_reeleccion_p_mun
df_tabulado_40 <-  T29_P39_iria_mejor_partido
df_tabulado_41 <-  T30_P40_iria_peor_partido
df_tabulado_42 <-  T31_P41_part_pos_ganar
df_tabulado_43 <-  tabla_consolidada
df_tabulado_44 <-  T32_P42_medio_comunicacion
df_tabulado_45 <-  T33_P43_redes
df_tabulado_46 <-  T34_P43B_redes
df_tabulado_47 <-  T56_P30.1_estudios_agrup
df_tabulado_48 <-  T57_P31_hog_aut
df_tabulado_49 <-  T58_P32_hog_ba
df_tabulado_50 <-  T59_P33_hog_inter
df_tabulado_51 <-  T60_P34_hog_p_14_a
df_tabulado_52 <-  T61_P35_viv_cuartos
df_tabulado_53 <-  T62_P36_amai

#EXPORTAR EXCEL FRECUENCIAS-----------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Frecuencias")

#encabezados
estilo_header <- createStyle(
    textDecoration = "bold",
    fontSize = 12
)
#color primera columna (sin encabezado)
estilo_columna1 <- createStyle(
    fgFill = "#E0E0E0"
)
#bordes
estilo_bordes <- createStyle(
    border = "TopBottomLeftRight",
    borderColour = "#808080"
)
#tabulados

#Obtener todos los nombres que coinciden con el patrón
nombres <- ls(pattern = "^df_tabulado_[0-9]+$", envir = .GlobalEnv)

#Ordenar por el número que aparece después de df_tabulado_
nombres_ordenados <- nombres[order(as.numeric(sub("df_tabulado_", "", nombres)))]

#LISTA FINAL TABS
lista_dfs <- mget(nombres_ordenados, envir = .GlobalEnv)

#fila 
fila_inicial <- 1
for (df in lista_dfs) {
    if (is.data.frame(df) && nrow(df) > 0 && ncol(df) > 0) {
        
        #tab con encabezado y estilo
        writeData(
            wb,
            sheet = c("Frecuencias"),
            x = df,
            startRow = fila_inicial,
            headerStyle = estilo_header
        )
        #Rango de filas y columnas del data frame
        filas <- (fila_inicial + 1):(fila_inicial + nrow(df))
        columnas <- 1:ncol(df)
        #color primera columna (sin tocar encabezado)
        addStyle(
            wb,
            sheet = "Frecuencias",
            style = estilo_columna1,
            rows = filas,
            cols = 1,
            gridExpand = TRUE,
            stack = TRUE
        )
        #bordes
        addStyle(
            wb,
            sheet = "Frecuencias",
            style = estilo_bordes,
            rows = fila_inicial:(fila_inicial + nrow(df)),
            cols = columnas,
            gridExpand = TRUE,
            stack = TRUE
        )
        #fila para sigueinte tab
        fila_inicial <- fila_inicial + nrow(df) + 3
    }
}


#GURADAR BASE-----------------------------------------------------------------------------------------------------------------------------------------------------
saveWorkbook(wb, "RESULTADOS/FRECUENCIAS_DF_.xlsx", overwrite = TRUE)

#******************************************-----------------------------------------------------------------------------------------------------------------------
#PALETA-----------------------------------------------------------------------------------------------------------------------------------------------------------
define_colors <- function(labels) {
    base_color <- "#52786D"#52786D"  # Color base 
    gray_color <- "#D3D3D3"  # Color gris para los valores especiales
    dgray_color <- "#6B6B6B" # Color para graficos AMAI y ocupacion
    colors <- ifelse(labels %in% especiales, gray_color,ifelse(labels %in% especiales_oscuro, dgray_color ,base_color))
    return(colors)
}

#*******************************************----------------------------------------------------------------------------------------------------------------------
#📊GRAFICOS📊---------------------------------------------------------------------------------------------------------------------------------------------------------

#BARRAS HORIZONTALES---------------------------------------------------------------------------------------------------------------------------------------------------
graficar_clasificacion_gobierno <- function(data,variable, nombre_objeto, archivo_salida, width = 10, height = 5.5, wrap_width = 30 ) 
{
    #QUITAMOS FILA DE TOTALES
    df <- data %>%
        filter(!.data[[variable]] %in% c("Total"))
    
    if (!is.null(wrap_width)) {
        df[[variable]] <- str_wrap(df[[variable]], width = wrap_width)
    }
    
    #SEPARAMOS VALORES ESPECIALES Y NORMALES
    normales <- setdiff(unique(df[[variable]]), especiales)
    
    orden_normales <- df$Frecuencia[match(normales, df[[variable]])]
    #normales <- normales[order(orden_normales, decreasing = TRUE)]
    
    graf <- c(normales, especiales)
    
    df[[variable]] <- factor(df[[variable]], levels = graf)
    
    #CUANTAS CATEGORIAS HAY
    n_categorias <- nlevels(factor(df[[variable]]))
    
    #TAMAÑO DE LETRA DINÁMICO
    size_eje_y <- ifelse(n_categorias > 12, 12, 17)
    
    size_label <- ifelse(n_categorias > 12, 5, 7)
    
    p <- ggplot(df, aes(
        y = fct_rev(.data[[variable]]),
        x = Frecuencia,
        fill = .data[[variable]]
    )) +
        geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
        coord_cartesian(clip = "off") +
        geom_text(aes(label = percent(`Porcentaje_válido`, 0)),
                  colour = "#404040",
                  size = size_label,
                  family = "Geomanist",
                  hjust = -0.15,
                  nudge_x = max(df$Frecuencia) * 0.01) +
        scale_fill_manual(values = define_colors(graf)) +
        scale_x_continuous(expand = expansion(mult = c(0, 1.8))) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(title = "", x = "", y = "") +
        theme(
            plot.margin = margin(0, 50, 10, 0),
            text = element_text(family = "Geomanist"),
            axis.text.y = element_text(
                angle = 0,
                hjust = 1,
                size = size_eje_y,
                colour = "#7F7F7F"),
            axis.text.x = element_text(colour = "transparent", size = 0.1),
            plot.background = element_rect(fill = "transparent", color ="transparent"),
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_line(color = "transparent"),
            axis.line.x.bottom = element_line(color = "transparent", linewidth = .2),
            axis.line.y.left = element_line(colour = "#d9d9d9", linewidth = .2),
            axis.ticks.y.left = element_line(colour = "transparent", linewidth = .5),
            axis.ticks.x = element_line(colour = "transparent", linewidth = .5),
            axis.line = element_line(lineend = "butt")
        )
    
    assign(nombre_objeto, p, envir = .GlobalEnv)
    
    ggsave(filename = archivo_salida, plot = p, width = width, height = height, dpi = 300)
    
    return(p)
}

#APROBACION DESAPROBACION-----------------------------------------------------------------------------------------------------------------------
grafico_aprob <- function(data, var, nombre_objeto, archivo_salida) {
    
    data <- data %>% filter(!.data[[var]] %in% c("Total", "NANA"))
    
    data[[var]] <- str_wrap(data[[var]], width = 15)
    
    graf <- c(setdiff(unique(data[[var]]), especiales), especiales)
    data[[var]] <- factor(data[[var]], levels = graf)
    
    graf_final <- ggplot(data, aes(x = .data[[var]], y = Frecuencia, fill = .data[[var]])) +
        geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = percent(`Porcentaje_válido`, 0)),
                  colour = "#404040", size = 7, family = "Geomanist",
                  vjust = -0.15,
                  nudge_y = max(data$Frecuencia) * 0.02) +
        scale_fill_manual(values = define_colors(graf)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.7))) +
        scale_x_discrete(expand =c(0, 0.4)) +
        labs(title = "", x = "", y = "") +
        theme(
            plot.margin = margin(0, 0, 2, 0),
            text = element_text(family = "Geomanist"),
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, colour = "#7F7F7F"),
            axis.text.y = element_text(colour = "transparent", size = 2),
            plot.background = element_rect(fill = "transparent", color = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            panel.grid = element_line(color = "transparent"),
            axis.line.x.bottom = element_line(color = "#d9d9d9", linewidth = .5),
            axis.line.y.left = element_line(colour = "transparent", linewidth = .2),
            axis.ticks.y.left = element_line(colour = "transparent", linewidth = .2),
            axis.ticks.x = element_line(colour = "transparent", linewidth = .2)
        )
    
    assign(nombre_objeto, graf_final, envir = .GlobalEnv)
    
    ggsave(filename = archivo_salida, plot = graf_final, width = 10, height = 5, dpi = 300)
}

#G1_T5_P1_ident_part--------------------------------------------------------------------------------------------------------------------------------------------
T5_P1_ident_part <- limpiar_tabla(T5_P1_ident_part)

graficar_clasificacion_gobierno(T5_P1_ident_part,"P1.Identifica_partido","G1_T5_P1_ident_part",
                                "RESULTADOS/GRAFICOS/G1_T5_P1_ident_part.png",
                                width = 10, height = 5,wrap_width = NULL)

#G2_T6_P2_aprob_pdta--------------------------------------------------------------------------------------------------------------------------------------------
grafico_aprob(T6_P2_aprob_pdta,"P2.Aprobación_presidenta", "G2_T6_P2_aprob_pdta",
              "RESULTADOS/GRAFICOS/G2_T6_P2_aprob_pdta.png")

#G3_T7_P3_clas_gob_pdta_mejor--------------------------------------------------------------------------------------------------------------------------------------------
T7_P3_clas_gob_pdta_mejor <- limpiar_tabla(T7_P3_clas_gob_pdta_mejor)

graficar_clasificacion_gobierno(T7_P3_clas_gob_pdta_mejor,"P3.Clasificacion_gob_presidenta_mejor",
                                "G3_T7_P3_clas_gob_pdta_mejor","RESULTADOS/GRAFICOS/G3_T7_P3_clas_gob_pdta_mejor.png",
                                width = 10,height = 5,wrap_width = NULL)

#G4_T8_P4_clas_gob_pdta_peor--------------------------------------------------------------------------------------------------------------------------------------------
T8_P4_clas_gob_pdta_peor <- limpiar_tabla(T8_P4_clas_gob_pdta_peor)

graficar_clasificacion_gobierno(T8_P4_clas_gob_pdta_peor,"P4.Clasificacion_gob_presidenta_peor",
                                "G4_T8_P4_clas_gob_pdta_peor","RESULTADOS/GRAFICOS/G4_T8_P4_clas_gob_pdta_peor.png",
                                width = 10, height = 5,wrap_width = NULL)

#G5_T9_P5_Aprobación_gobernador--------------------------------------------------------------------------------------------------------------------------------------------
grafico_aprob(T9_P5_Aprobación_gobernador,"P5.Aprobación_gobernador", 
              "G5_T9_P5_Aprobación_gobernador","RESULTADOS/GRAFICOS/G5_T9_P5_Aprobación_gobernador.png")

#G6_T10_P6_MEJOR_clasificación_de_gobierno--------------------------------------------------------------------------------------------------------------------------------------------
T10_P6_clasificación_de_gobierno <- limpiar_tabla(T10_P6_clasificación_de_gobierno)

graficar_clasificacion_gobierno(T10_P6_clasificación_de_gobierno,"P6.Clasificación_mejor_gobernador",
                                "G6_T10_P6_clasificación_de_gobierno","RESULTADOS/GRAFICOS/G6_T10_P6_clasificación_de_gobierno.png",
                                width = 10, height = 5,wrap_width = NULL)

#G7_T11_P7_Clasificación_peor_de_gobierno--------------------------------------------------------------------------------------------------------------------------------------------
T11_P7_Clasificación_peor_de_gobierno <- limpiar_tabla(T11_P7_Clasificación_peor_de_gobierno)

graficar_clasificacion_gobierno(T11_P7_Clasificación_peor_de_gobierno,"P7.Clasificación_peor_de_gobierno",
                                "G7_T11_P7_Clasificación_peor_de_gobierno","RESULTADOS/GRAFICOS/G7_T11_P7_Clasificación_peor_de_gobierno.png",
                                width = 10, height = 5,wrap_width = NULL)

#G8_T12_P8_aprob_P_municipal--------------------------------------------------------------------------------------------------------------------------------------------
grafico_aprob(T12_P8_aprob_P_municipal,"P8.Aprobacion_P_municipal", 
              "G8_T12_P8_aprob_P_municipal","RESULTADOS/GRAFICOS/G8_T12_P8_aprob_P_municipal.png")

#G9_T13_P9_mejor_clasificación_mun--------------------------------------------------------------------------------------------------------------------------------------------
T13_P9_mejor_clasificación_mun <- limpiar_tabla(T13_P9_mejor_clasificación_mun)

graficar_clasificacion_gobierno(T13_P9_mejor_clasificación_mun,"P9.Clasificación_mejor_mun",
                                "G9_T13_P9_mejor_clasificación_mun","RESULTADOS/GRAFICOS/G9_T13_P9_mejor_clasificación_mun.png",
                                width = 10, height = 5,wrap_width = NULL)

#G10_T14_P10_peor_Clasificación_mun--------------------------------------------------------------------------------------------------------------------------------------------
T14_P10_peor_Clasificación_mun <- limpiar_tabla(T14_P10_peor_Clasificación_mun)

graficar_clasificacion_gobierno(T14_P10_peor_Clasificación_mun,"P10.Clasificación_peor_mun",
                                "G10_T14_P10_peor_Clasificación_mun","RESULTADOS/GRAFICOS/G10_T14_P10_peor_Clasificación_mun.png",
                                width = 10, height = 5,wrap_width = NULL)

#G11_T15_P11_partido_interes_familia--------------------------------------------------------------------------------------------------------------------------------------------
T15_P11_partido_interes_familia <- limpiar_tabla(T15_P11_partido_interes_familia)

graficar_clasificacion_gobierno(T15_P11_partido_interes_familia,"P11.Partido_interes_familia",
                                "G11_T15_P11_partido_interes_familia","RESULTADOS/GRAFICOS/G11_T15_P11_partido_interes_familia.png",
                                width = 10, height = 5,wrap_width = NULL)

#G12_T16_P12_Principal_problema_edo--------------------------------------------------------------------------------------------------------------------------------------------
T16_P12_Principal_problema_edo <- limpiar_tabla(T16_P12_Principal_problema_edo)

graficar_clasificacion_gobierno(T16_P12_Principal_problema_edo,"P12.Principal_problema_edo",
                                "G12_T16_P12_Principal_problema_edo","RESULTADOS/GRAFICOS/G12_T16_P12_Principal_problema_edo.png",
                                width = 10, height = 5.5,wrap_width = NULL)

#G13_T17_P23_mejor_diputado--------------------------------------------------------------------------------------------------------------------------------------------
T17_P23_mejor_diputado <- limpiar_tabla(T17_P23_mejor_diputado)

graficar_clasificacion_gobierno(T17_P23_mejor_diputado,"P23.Mejor_diputado_federal",
                                "G13_T17_P23_mejor_diputado","RESULTADOS/GRAFICOS/G13_T17_P23_mejor_diputado.png",
                                width = 10, height = 5,wrap_width = NULL)

#G14_T18_P24_peor_diputado--------------------------------------------------------------------------------------------------------------------------------------------
T18_P24_peor_diputado <- limpiar_tabla(T18_P24_peor_diputado)

graficar_clasificacion_gobierno(T18_P24_peor_diputado,"P24.Peor_diputado_federal",
                                "G14_T18_P24_peor_diputado","RESULTADOS/GRAFICOS/G14_T18_P24_peor_diputado.png",
                                width = 10, height = 5,wrap_width = NULL)

#G15_T19_P25_cercano_gob--------------------------------------------------------------------------------------------------------------------------------------------
T19_P25_cercano_gob <- limpiar_tabla(T19_P25_cercano_gob)

graficar_clasificacion_gobierno(T19_P25_cercano_gob,"P25.Cercano_gobernador",
                                "G15_T19_P25_cercano_gob","RESULTADOS/GRAFICOS/G15_T19_P25_cercano_gob.png",
                                width = 10, height = 5,wrap_width = NULL)

#G16_T20_P26_cercano_pdta--------------------------------------------------------------------------------------------------------------------------------------------
T20_P26_cercano_pdta <- limpiar_tabla(T20_P26_cercano_pdta)

graficar_clasificacion_gobierno(T20_P26_cercano_pdta,"P26.Cercano_presidenta",
                                "G16_T20_P26_cercano_pdta","RESULTADOS/GRAFICOS/G16_T20_P26_cercano_pdta.png",
                                width = 10, height = 5,wrap_width = NULL)

#G17_T21_P27.Mayor_posibilidad_ganar--------------------------------------------------------------------------------------------------------------------------------------------
T21_P27_P27.Mayor_posibilidad_ganar <- limpiar_tabla(T21_P27_P27.Mayor_posibilidad_ganar)

graficar_clasificacion_gobierno(T21_P27_P27.Mayor_posibilidad_ganar,"P27.Mayor_posibilidad_ganar",
                                "G17_T21_P27.Mayor_posibilidad_ganar","RESULTADOS/GRAFICOS/G17_T21_P27.Mayor_posibilidad_ganar.png",
                                width = 10, height = 5,wrap_width = NULL)

#G18_T22_P28_problemas--------------------------------------------------------------------------------------------------------------------------------------------
T22_P28_problemas <- limpiar_tabla(T22_P28_problemas)

graficar_clasificacion_gobierno(T22_P28_problemas,"PROBLEMA",
                                "G18_T22_P28_problemas","RESULTADOS/GRAFICOS/G18_T22_P28_problemas.png",
                                width = 10, height = 5,wrap_width = NULL)

#G19_T45_P22_Preferencia_intencion------------------------------------------------------------------------------------------------------------------------------------------
G19_T45_P22_Preferencia_intencion <- T45_P22_Preferencia_intencion %>% gt()%>%
    cols_width(everything() ~ px(100))%>%
    # cols_width( Partido ~ px(95))%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_title()
        )
    )%>%
    
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(columns = 1)
    ) %>%
    fmt_percent(
        columns = 2:ncol(T45_P22_Preferencia_intencion),  
        decimals = 1
    )%>%
    #formato encabezado
    cols_label(
        `1° Opción de voto` = md("1° Opción de voto gobernador(a)^1^"),
        `2° Opción de voto` = md("2° Opción de voto gobernador(a)^2^"),
        `Voto p mun` = md("Voto presidente(a) municipales^3^"),
        `Voto df` = md("Voto diputado(a) federales^4^"),
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T45_P22_Preferencia_intencion), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T45_P22_Preferencia_intencion), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T45_P22_Preferencia_intencion)))
    )%>%
    tab_options(
        .,
        table.font.size = 7, 
        column_labels.font.size = 9,
        column_labels.border.top.color = "#ffffff",
        column_labels.border.bottom.color = "#ffffff",
        table_body.hlines.color = "#ffffff",
        data_row.padding = px(2),
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(3),
        column_labels.padding.horizontal = px(6)
        
    )

G19_T45_P22_Preferencia_intencion
gtsave(G19_T45_P22_Preferencia_intencion,filename = "G19_T45_P22_Preferencia_intencion.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)


#G20_T23_P33_revocacion--------------------------------------------------------------------------------------------------------------------------------------------
T23_P33_revocacion <- limpiar_tabla(T23_P33_revocacion)

graficar_clasificacion_gobierno(T23_P33_revocacion,"P33.Conoce_revocacion",
                                "G20_T23_P33_revocacion","RESULTADOS/GRAFICOS/G20_T23_P33_revocacion.png",
                                width = 10, height = 4,wrap_width = NULL)


#G21_T24_P34_revocacion_voto_mismo_dia--------------------------------------------------------------------------------------------------------------------------------------------
T24_P34_revocacion_voto_mismo_dia <- limpiar_tabla(T24_P34_revocacion_voto_mismo_dia)

graficar_clasificacion_gobierno(T24_P34_revocacion_voto_mismo_dia,"P34.Revocacion_voto_mismo_dia",
                                "G21_T24_P34_revocacion_voto_mismo_dia","RESULTADOS/GRAFICOS/G21_T24_P34_revocacion_voto_mismo_dia.png",
                                width = 10, height = 4,wrap_width = NULL)

#G22_T25_P35_participar_revocacion--------------------------------------------------------------------------------------------------------------------------------------------
T25_P35_participar_revocacion <- limpiar_tabla(T25_P35_participar_revocacion)

graficar_clasificacion_gobierno(T25_P35_participar_revocacion,"P35.Dispuesto_participar_revocacion",
                                "G22_T25_P35_participar_revocacion","RESULTADOS/GRAFICOS/G22_T25_P35_participar_revocacion.png",
                                width = 10, height = 4,wrap_width = NULL)


#G23_T26_P36_revoacion_pdta--------------------------------------------------------------------------------------------------------------------------------------------
T26_P36_revoacion_pdta <- limpiar_tabla(T26_P36_revoacion_pdta)

graficar_clasificacion_gobierno(T26_P36_revoacion_pdta,"P36.Revocacion_pdta",
                                "G23_T26_P36_revoacion_pdta","RESULTADOS/GRAFICOS/G23_T26_P36_revoacion_pdta.png",
                                width = 10, height = 4,wrap_width = 30)

#G24_T27_P37_reeleccion_p_mun--------------------------------------------------------------------------------------------------------------------------------------------
T27_P37_reeleccion_p_mun <- limpiar_tabla(T27_P37_reeleccion_p_mun)

graficar_clasificacion_gobierno(T27_P37_reeleccion_p_mun,"P37.Reeleccion_p_mun",
                                "G24_T27_P37_reeleccion_p_mun","RESULTADOS/GRAFICOS/G24_T27_P37_reeleccion_p_mun.png",
                                width = 10, height = 4,wrap_width = NULL)

#G25_T28_P38_de_acuerdo_con_reeleccion_p_mun--------------------------------------------------------------------------------------------------------------------------------------------
T28_P38_de_acuerdo_con_reeleccion_p_mun <- limpiar_tabla(T28_P38_de_acuerdo_con_reeleccion_p_mun)

graficar_clasificacion_gobierno(T28_P38_de_acuerdo_con_reeleccion_p_mun,"P38.De_acuerdo_con_reeleccion_p_mun",
                                "G25_T28_P38_de_acuerdo_con_reeleccion_p_mun","RESULTADOS/GRAFICOS/G25_T28_P38_de_acuerdo_con_reeleccion_p_mun.png",
                                width = 10, height = 4,wrap_width = NULL)

#G26_tabla_consolidada------------------------------------------------------------------------------------------------------------------------------------------
G26_tabla_consolidada <- tabla_consolidada %>% gt()%>%
    cols_width(everything() ~ px(120))%>%
    # cols_width( Partido ~ px(95))%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_title()
        )
    )%>%
    
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(columns = 1)
    ) %>%
    fmt_percent(
        columns = 2:ncol(tabla_consolidada),  
        decimals = 1
    )%>%
    #formato encabezado
    cols_label(
        `P39.Iria_mejor_partido` = md("Partido con el que le iría mejor^1^"),
        `P40.Iria_peor_partido` = md("Partido con el que le iría peor^2^"),
        `P41.Partido_mas_posibilidades_de_ganar` = md("Expectativa partido ganador^3^"),
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(tabla_consolidada), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(tabla_consolidada), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(tabla_consolidada)))
    )%>%
    tab_options(
        .,
        table.font.size = 7, 
        column_labels.font.size = 9,
        column_labels.border.top.color = "#ffffff",
        column_labels.border.bottom.color = "#ffffff",
        table_body.hlines.color = "#ffffff",
        data_row.padding = px(2),
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(2),
        column_labels.padding.horizontal = px(6)
        
    )

G26_tabla_consolidada
gtsave(G26_tabla_consolidada,filename = "G26_tabla_consolidada.PNG", path = "RESULTADOS/GRAFICOS",zoom = 4)

#G27_T32_P42_medio_comunicacion--------------------------------------------------------------------------------------------------------------------------------------------
T32_P42_medio_comunicacion <- limpiar_tabla(T32_P42_medio_comunicacion)

graficar_clasificacion_gobierno(T32_P42_medio_comunicacion,"P42.Medio_comunicacion",
                                "G27_T32_P42_medio_comunicacion","RESULTADOS/GRAFICOS/G27_T32_P42_medio_comunicacion.png",
                                width = 10, height = 4,wrap_width = NULL)

#G28_P43_Red social a la que pertenece--------------------------------------------------------------------------------------------------------------------------------------------
orden_respuestas <- c("Sí pertenece","No pertenece","No sabe / No recuerda","No contestó")

G1_long <- T33_P43_redes %>%
    pivot_longer(
        cols = 2:5,
        names_to = "respuesta",
        values_to = "Valor"
    )

G1_long$Valor <- as.numeric(G1_long$Valor)

G1_long$red <- factor(
    G1_long$red,
    levels = unique(T33_P43_redes$red)
)

#orden de las respuestas 
G1_long$respuesta <- factor(
    G1_long$respuesta,
    levels = orden_respuestas
)

colores <- c(
    "Sí pertenece" = "#154B3B",
    "No pertenece" = "#2B6C55",
    "No sabe / No recuerda" = "#5F5F5F",
    "No contestó" = "#a1a1a1"
)

#BARRAS HORIZONTALES
G28_P43_redes <- ggplot(G1_long, aes(x = red, y = Valor, fill = respuesta )) +
    geom_bar(stat = "identity", width = 0.6,position = position_stack(reverse = TRUE)) +
    
    geom_text(
        aes(label = ifelse(Valor < 0.004, "", paste0(round(Valor*100), "%"))),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        colour = "white", size = 4, family = "Geomanist")+
    
    
    
    scale_fill_manual(values = colores, name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = "", y = "") +
    theme_minimal(base_family = "Geomanist") +
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 14, colour = "transparent"),
        axis.text.y = element_text(size = 14, colour = "#7F7F7F"),
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = "#7F7F7F"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
    ) +
    coord_flip()

G28_P43_redes
ggsave(filename = paste0("RESULTADOS/GRAFICOS/G28_P43_redes.png"), plot = G28_P43_redes, width = 10, height = 6, dpi = 300)

#G28B_P43B_Red social a la que usa--------------------------------------------------------------------------------------------------------------------------------------------
orden_respuestas2 <- c("No la usa a diario","1 o 2 veces al día","3 o 4 veces al día","Más de 4 veces al día","No sabe / No recuerda","No contestó","NANA")

orden_redes <- T33_P43_redes %>%
    arrange(`Sí pertenece`) %>%   # ⬅️ ahora es ascendente
    pull(red)

labels_pertenece <- dplyr::select(T33_P43_redes,1,5)

G2_long <- T34_P43B_redes %>%
    pivot_longer(
        cols = 2:7,
        names_to = "respuesta",
        values_to = "Valor"
    )

totales_barra <- G2_long %>%
    group_by(red) %>%
    summarise(Total = sum(Valor, na.rm = TRUE), .groups = "drop")


G2_long$Valor <- as.numeric(G2_long$Valor)

G2_long$red <- factor(
    G2_long$red,
    levels = unique(T34_P43B_redes$red)
)

#orden de las respuestas 
G2_long$respuesta <- factor(
    G2_long$respuesta,
    levels = orden_respuestas2
)

G2_long$red <- factor(
    G2_long$red,
    levels = orden_redes
)

colores <- c(
    "No la usa a diario" = "#154B3B",
    "1 o 2 veces al día" = "#2B6C55",
    "3 o 4 veces al día" = "#308265",
    "Más de 4 veces al día"="#3e8b70",
    "No sabe / No recuerda" ="#5F5F5F",
    "No contestó" = "#a1a1a1"
)

#BARRAS HORIZONTALES
G28B_P43B_uso_redes <- ggplot(G2_long, aes(x = red, y = Valor, fill = respuesta )) +
    geom_bar(stat = "identity", width = 0.6,position = position_stack(reverse = TRUE)) +
    
    geom_text(
        aes(label = ifelse(Valor < 0.0039, "", paste0(round(Valor*100), "%"))),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        colour = "white", size = 4, family = "Geomanist")+
    
    geom_text(
        data = labels_pertenece,
        aes(
            x = red,
            y = 1+0.02,  #final de la barra
            label = paste0(round(`Sí pertenece` * 100), "% Sí pertenece")
        ),
        inherit.aes = FALSE,
        colour = "#154B3B",
        size = 5,
        family = "Geomanist",
        hjust = 0
    )+
    
    
    scale_fill_manual(
        values = colores,
        breaks = setdiff(names(colores), "NANA"),  # ⬅️ quita NANA de la leyenda
        name = NULL
    ) +
    
    scale_y_continuous(
        labels = percent_format(accuracy = 1),
        expand = expansion(mult = c(0, 0.30)) 
    )+
    
    labs(x = "", y = "") +
    theme_minimal(base_family = "Geomanist") +
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 14, colour = "transparent"),
        axis.text.y = element_text(size = 14, colour = "#7F7F7F"),
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = "#7F7F7F"),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
    ) +
    coord_flip(clip = "off")

G28B_P43B_uso_redes
ggsave(filename = paste0("RESULTADOS/GRAFICOS/G28B_P43B_uso_redes.png"), plot = G28B_P43B_uso_redes, width = 10, height = 6, dpi = 300)

#G29_T3_edad------------------------------------------------------------------------------------------------------------------------------------------------------
#QUITAMOS FILA DE TOTALES
T3_edad
T3_edad$Edad[T3_edad$Edad == "60omás"] <- "60 o más"
G29_T3_edad <- T3_edad %>%
    filter(!Edad %in% c("Total", "NANA"))
#Parrafo en texto x para RENGLONES
G29_T3_edad$Edad <- str_wrap(G29_T3_edad$Edad, width = 15)
#SEPARAMOS VALORES ESPECIALES Y NORMALES
graf <- setdiff(unique(G29_T3_edad$Edad), especiales)
graf <- c((graf), especiales) #sin órden alfabético (si queremos orden meter sort) y luego los especiales
G29_T3_edad$Edad <- factor(G29_T3_edad$Edad, levels = graf)

#gráfico
G29_T3_edad <- ggplot(G29_T3_edad, aes(x = Edad, y = Frecuencia, fill = Edad)) +
    geom_bar(stat = "identity", width = 0.6,show.legend = FALSE) +
    geom_text(aes(label = paste0(round(`Porcentaje_válido`*100, 0))),
              colour = "#404040", size = 7, family = "Geomanist", fontface = "plain" ,
              vjust = -0.15, nudge_y = max(G29_T3_edad$Frecuencia) * 0.02) +
    scale_fill_manual(values = define_colors(graf)) +  # Aplica los colores correctamente
    scale_y_continuous(expand = expansion(mult = c(0, 0.99))) + #estirar o contraer la barra
    scale_x_discrete(expand = c(0, 0)) + # aplastar la grafica y cortar el eje justo al termino de las barras
    labs(title = "", x = "", y = "") +
    theme(
        plot.margin = margin(0, 3, 2, 0),
        text = element_text(family = "Geomanist"),#face = "italic"),
        axis.text.x = element_text(
            angle = 0, 
            hjust = 0.5, 
            size =  10,
            colour = "#7F7F7F"),
        axis.text.y = element_text(colour = "transparent", size = 2),
        plot.background = element_rect(fill = "transparent", color ="transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "transparent"),
        axis.line.x.bottom = element_line(color = "#d9d9d9", linewidth = .5),
        axis.line.y.left = element_line(colour = "transparent", linewidth = .2),
        axis.ticks.y.left = element_line(colour = "transparent", linewidth = .2),
        axis.ticks.x = element_line(colour = "transparent", linewidth = .2),
        #plot.title = element_text(colour = "#5E5E5E", size = 20, hjust = 0.5, vjust = .5)
    )

G29_T3_edad
ggsave(filename = paste0("RESULTADOS/GRAFICOS/G29_T3_edad.png"), plot = G29_T3_edad, width = 5, height = 2, dpi = 300)

#G30_T62_amai------------------------------------------------------------------------------------------------------------------------------------------------------
#QUITAMOS FILA DE TOTALES
G30_T62_amai <- T62_P36_amai %>%
    filter(!AMAI %in% c("Total", "NANA"))

G30_T62_amai$AMAI[1] <- "AB" 
#Parrafo en texto x para RENGLONES
G30_T62_amai$AMAI <- str_wrap(G30_T62_amai$AMAI, width = 15)
#SEPARAMOS VALORES ESPECIALES Y NORMALES
graf <- setdiff(unique(G30_T62_amai$AMAI), especiales)
graf <- c((graf), especiales) #sin órden alfabético (si queremos orden meter sort) y luego los especiales
G30_T62_amai$AMAI <- factor(G30_T62_amai$AMAI, levels = graf)

#gráfico
G30_T62_amai <- ggplot(G30_T62_amai, aes(x = AMAI, y = Frecuencia, fill = AMAI)) +
    geom_bar(stat = "identity", width = 0.6,show.legend = FALSE) +
    geom_text(aes(label = paste0(round(`Porcentaje_válido`*100, 0))),
              colour = "#404040", size = 7, family = "Geomanist", fontface = "plain" ,
              vjust = -0.15, nudge_y = max(G30_T62_amai$Frecuencia) * 0.02) +
    scale_fill_manual(values = define_colors(graf)) +  # Aplica los colores correctamente
    scale_y_continuous(expand = expansion(mult = c(0, 0.99))) + #estirar o contraer la barra
    scale_x_discrete(expand = c(0, 0)) + # aplastar la grafica y cortar el eje justo al termino de las barras
    labs(title = "", x = "", y = "") +
    theme(
        plot.margin = margin(0, 2, 2, 0),
        text = element_text(family = "Geomanist"),#face = "italic"),
        axis.text.x = element_text(
            angle = 0, 
            hjust = 0.5, 
            size =  12,
            colour = "#7F7F7F"),
        axis.text.y = element_text(colour = "transparent", size = 2),
        plot.background = element_rect(fill = "transparent", color ="transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "transparent"),
        axis.line.x.bottom = element_line(color = "#d9d9d9", linewidth = .5),
        axis.line.y.left = element_line(colour = "transparent", linewidth = .2),
        axis.ticks.y.left = element_line(colour = "transparent", linewidth = .2),
        axis.ticks.x = element_line(colour = "transparent", linewidth = .2),
        #plot.title = element_text(colour = "#5E5E5E", size = 20, hjust = 0.5, vjust = .5)
    )

G30_T62_amai
ggsave(filename = paste0("RESULTADOS/GRAFICOS/G30_T62_amai.png"), plot = G30_T62_amai, width = 4.5, height = 2, dpi = 300)

#G31_T56_estudios_agrup------------------------------------------------------------------------------------------------------------------------------------------------------
#QUITAMOS FILA DE TOTALES
G31_T56_estudios_agrup <- T56_P30.1_estudios_agrup %>%
    filter(!Estudios_agrupados %in% c("Total", "NANA"))

#G31_T56_estudios_agrup <- G31_T56_estudios_agrup[order(G31_T56_estudios_agrup$Frecuencia,decreasing = T), ]
#Parrafo en texto x para RENGLONES
#G31_T56_estudios_agrup$Estudios_agrupados <- str_wrap(G31_T56_estudios_agrup$Estudios_agrupados, width = 15)
#SEPARAMOS VALORES ESPECIALES Y NORMALES
graf <- setdiff(unique(G31_T56_estudios_agrup$Estudios_agrupados), especiales)
graf <- c((graf), especiales) #sin órden alfabético (si queremos orden meter sort) y luego los especiales

G31_T56_estudios_agrup$Estudios_agrupados <- factor(G31_T56_estudios_agrup$Estudios_agrupados, levels = graf)

G31_T56_estudios_agrup <- ggplot(G31_T56_estudios_agrup, aes(y = fct_rev(Estudios_agrupados), x = Frecuencia, fill = Estudios_agrupados)) +
    geom_bar(stat = "identity", width = 0.7,show.legend = FALSE,) +
    coord_cartesian(clip = "off") +
    geom_text(aes(label = paste0(round(`Porcentaje_válido`*100, 0))),
              colour = "#404040", 
              size = 7,
              family = "Geomanist", fontface = "plain" ,
              hjust = -0.15, nudge_x = max(G31_T56_estudios_agrup$Frecuencia) * 0.01) +
    scale_fill_manual(values = define_colors(graf)) +  # Aplica los colores correctamente
    scale_x_continuous(expand = expansion(mult = c(0, .05))) + #estirar o contraer la barra
    #scale_y_discrete(expand = expansion(mult = 0.22)) + #aplastar la gráfica
    scale_y_discrete(expand = c(0, 0)) + # aplastar la grafica y cortar el eje justo al termino de las barras
    labs(title = "", x = "", y = "") +
    theme(
        plot.margin = margin(0, 50, 10, 0),
        text = element_text(family = "Geomanist"),#face = "italic"),
        axis.text.y = element_text(
            angle = 0, 
            hjust = 1, 
            size =  15, 
            colour = "#7F7F7F"),
        axis.text.x = element_text(colour = "transparent", size = 0.1),
        plot.background = element_rect(fill = "transparent", color ="transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "transparent"),
        axis.line.x.bottom = element_line(color = "transparent", linewidth = .2),
        axis.line.y.left = element_line(colour = "#d9d9d9", linewidth = .2),
        axis.ticks.y.left = element_line(colour = "transparent", linewidth = .5),
        axis.ticks.x = element_line(colour = "transparent", linewidth = .5),
        axis.line = element_line(lineend = "butt",)
        #aspect.ratio = 0.5
        #plot.title = element_text(colour = "#5E5E5E", size = 20, hjust = 0.5, vjust = .5)
    )

G31_T56_estudios_agrup
ggsave(filename = paste0("RESULTADOS/GRAFICOS/G31_T56_estudios_agrup.png"), plot = G31_T56_estudios_agrup, width = 5, height = 3, dpi = 300)

#G32_T4_ocupacion------------------------------------------------------------------------------------------------------------------------------------------------------
#QUITAMOS FILA DE TOTALES
G32_T4_ocupacion <- T4_ocupacion %>%
    filter(!Ocupación %in% c("Total", "NANA"))

#G32_T4_ocupacion <- G32_T4_ocupacion[order(G32_T4_ocupacion$Frecuencia,decreasing = T), ]
#Parrafo en texto x para RENGLONES
#G32_T4_ocupacion$Ocupación <- str_wrap(G32_T4_ocupacion$Ocupación, width = 15)
#SEPARAMOS VALORES ESPECIALES Y NORMALES
graf <- setdiff(unique(G32_T4_ocupacion$Ocupación), especiales)
graf <- c((graf), especiales) #sin órden alfabético (si queremos orden meter sort) y luego los especiales

G32_T4_ocupacion$Ocupación <- factor(G32_T4_ocupacion$Ocupación, levels = graf)

paleta_ocup <- function(labels) {
    base_color <- "#52786D"  # Color base 
    gray_color <- "#6B6B6B"  # Color gris para los valores especiales
    dgray_color <- "#6B6B6B" # Color para graficos AMAI y ocupacion
    colors <- ifelse(labels %in% especiales, gray_color,ifelse(labels %in% especiales_oscuro, dgray_color ,base_color))
    return(colors)
}

G32_T4_ocupacion <- ggplot(G32_T4_ocupacion, aes(y = fct_rev(Ocupación), x = Frecuencia, fill = Ocupación)) +
    geom_bar(stat = "identity", width = 0.7,show.legend = FALSE,) +
    coord_cartesian(clip = "off") +
    geom_text(aes(label = paste0(round(`Porcentaje_válido`*100, 0))),
              colour = "#404040", 
              size = 7,
              family = "Geomanist", fontface = "plain" ,
              hjust = -0.15, nudge_x = max(G32_T4_ocupacion$Frecuencia) * 0.01) +
    scale_fill_manual(values = paleta_ocup(graf)) +  # Aplica los colores correctamente
    scale_x_continuous(expand = expansion(mult = c(0, .1))) + #estirar o contraer la barra
    #scale_y_discrete(expand = expansion(mult = 0.22)) + #aplastar la gráfica
    scale_y_discrete(expand = c(0, 0)) + # aplastar la grafica y cortar el eje justo al termino de las barras
    labs(title = "", x = "", y = "") +
    theme(
        plot.margin = margin(0, 50, 10, 0),
        text = element_text(family = "Geomanist"),#face = "italic"),
        axis.text.y = element_text(
            angle = 0, 
            hjust = 1, 
            size =  12, 
            colour = "#7F7F7F"),
        axis.text.x = element_text(colour = "transparent", size = 0.1),
        plot.background = element_rect(fill = "transparent", color ="transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "transparent"),
        axis.line.x.bottom = element_line(color = "transparent", linewidth = .2),
        axis.line.y.left = element_line(colour = "#d9d9d9", linewidth = .2),
        axis.ticks.y.left = element_line(colour = "transparent", linewidth = .5),
        axis.ticks.x = element_line(colour = "transparent", linewidth = .5),
        axis.line = element_line(lineend = "butt",)
        #aspect.ratio = 0.5
        #plot.title = element_text(colour = "#5E5E5E", size = 20, hjust = 0.5, vjust = .5)
    )

G32_T4_ocupacion
ggsave(filename = paste0("RESULTADOS/GRAFICOS/G32_T4_ocupacion.png"), plot = G32_T4_ocupacion, width = 7, height = 5, dpi = 300)


#++++++++++++++++++++++++++++-----------------------------------------------------------------------------------------------------------------------------------------------
#++++++++++++++++++++++++++++-----------------------------------------------------------------------------------------------------------------------------------------------
#GRAFICOS CAREOS-------------------------------------------------------------------------------------------------------------------------------------------------------------
#G89.1_T67 Conocimiento y opinion actores------------------------------------------------------------------------------------------------------------------------------------------
G1_T67_conocimiento_y_opinion_actores <- T67_actores %>% gt()%>%
    cols_width(everything() ~ px(82))%>%
    cols_width( Personaje ~ px(175))%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_title()
        )
    )%>%
    fmt_percent(
        columns = 2:ncol(T67_actores),  
        decimals = 1
    )%>%
    #formato encabezado
    cols_label(
        `Sí lo conoce` = md("Sí lo conoce^1^"),
        `Buena` = md("Opinión Buena^2^<br> <span style='font-size:10px;'> <b> ***[Escala de 3]*** </b> </span>"),
        `Regular` = md("Opinión Regular^2^<br> <span style='font-size:10px;'> <b> ***[Escala de 3]*** </b> </span>"),
        `Mala` = md("Opinión Mala^2^<br> <span style='font-size:10px;'> <b> ***[Escala de 3]*** </b> </span>"),
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T67_actores), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T67_actores), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T67_actores)))
    )%>%
    tab_options(
        .,
        table.font.size = 11, 
        column_labels.font.size = 12,
        column_labels.border.top.color = "#ffffff",
        column_labels.border.bottom.color = "#ffffff",
        table_body.hlines.color = "#ffffff",
        data_row.padding = px(2),
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(3),
        column_labels.padding.horizontal = px(6)
        
    )

G1_T67_conocimiento_y_opinion_actores
gtsave(G1_T67_conocimiento_y_opinion_actores,filename = "G1_T67_conocimiento_y_opinion_actores.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.1_T67.1_conocimiento_y_opinion_actores_nueva------------------------------------------------------------------------------------------------------------------------------------------
G1_T67_conocimiento_y_opinion_actores_nueva <- T67_actores_nueva %>% gt()%>%
    cols_width(everything() ~ px(82))%>%
    cols_width( Personaje ~ px(175))%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_title()
        )
    )%>%
    fmt_percent(
        columns = 2:ncol(T67_actores_nueva)-1,  
        decimals = 1
    )%>%
    fmt_number(
        columns = saldo,
        decimals = 2
    )%>%
    #formato encabezado
    cols_label(
        `Sí lo conoce` = md("Sí lo conoce^1^"),
        `Buena` = md("Opinión Buena^2^<br> <span style='font-size:10px;'> <b> ***[Escala de 3]*** </b> </span>"),
        `Regular` = md("Opinión Regular^2^<br> <span style='font-size:10px;'> <b> ***[Escala de 3]*** </b> </span>"),
        `Mala` = md("Opinión Mala^2^<br> <span style='font-size:10px;'> <b> ***[Escala de 3]*** </b> </span>"),
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        saldo = md("**Factor de opinión** <br> <span style='font-size:10px;'>***(Buena / Mala) * Conocimiento***</span>")
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T67_actores_nueva), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T67_actores_nueva), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T67_actores_nueva)))
    )%>%
    tab_options(
        .,
        table.font.size = 11, 
        column_labels.font.size = 12,
        column_labels.border.top.color = "#ffffff",
        column_labels.border.bottom.color = "#ffffff",
        table_body.hlines.color = "#ffffff",
        data_row.padding = px(2),
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(3),
        column_labels.padding.horizontal = px(6)
        
    )

G1_T67_conocimiento_y_opinion_actores_nueva
gtsave(G1_T67_conocimiento_y_opinion_actores_nueva,filename = "G1_T67_conocimiento_y_opinion_actores_nueva.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.4_T70 partido relaciona personaje-------------------------------------------------------------------------------------------------------------------------------
G4_T70_partido_relaciona <- T70_partido_relaciona %>% gt()%>%
    cols_width(everything() ~ px(220))%>%
    #cols_width( Tema ~ px(210))%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("left","top","bottom","right"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_title()
        )
    )%>%
    fmt_percent(
        columns = 2:ncol(T70_partido_relaciona),  
        decimals = 1
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T70_partido_relaciona), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T70_partido_relaciona), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T70_partido_relaciona)))
    )%>%
    tab_options(
        .,
        table.font.size = 12, 
        column_labels.font.size = 10,
        # table.font.weight = "normal",
        # table.font.style = "normal",
        column_labels.border.top.color = "#ffffff",
        column_labels.border.bottom.color = "#ffffff",
        table_body.hlines.color = "#ffffff",
        data_row.padding = px(9),
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(15),
        column_labels.padding.horizontal = px(2)
    )

G4_T70_partido_relaciona
gtsave(G4_T70_partido_relaciona,filename = "G4_T70_partido_relaciona.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.5_T73 caracteristicas_actores_1-------------------------------------------------------------------------------------------------------------------------------
G5_T73_caracteristicas_actores_1 <- T73_caracteristicas_actores %>% gt(id = "gt")%>%
    cols_width(everything() ~ px(65))%>%
    cols_width( Personaje ~ px(210))%>%
    #añadimos spanner
    tab_spanner(
        label = md("**Honesto^1^** <br> <br>***Escala de 4***"),
        columns = names(T73_caracteristicas_actores)[2:6],# Aplica a columnas 2 a 6
    )%>%
    tab_spanner(
        label = md("**Cercano a la gente^2^** <br> <br>***Escala de 4***"),
        columns = names(T73_caracteristicas_actores)[7:11]  # Aplica a columnas 7 a 12
    )%>%
    #formato de spanners
    tab_style(
        style = list(
            cell_fill(color = "#52786D"), 
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold"),
            cell_borders(sides = "all", 
                         color = "#ffffff",
                         weight = px(1.5),)
        ),
        locations = cells_column_spanners()
    )%>%
    fmt_percent(
        columns = 2:ncol(T73_caracteristicas_actores),  
        decimals = 1
    )%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_column_spanners(),
            cells_title()
        )
    )%>%
    #poner superindices a las labels de cols
    cols_label(
        `Mucho.1` =md("Mucho"),
        `Algo.1` = md("Algo"),
        `Poco.1` = md("Poco"),
        `Nada.1` = md("Nada"),
        `NS / NC.1` = md("NS / NC"),
    )%>% 
    #formato encabezado
    # cols_label(
    #   !!!setNames(
    #     lapply(col_names[-1], function(col) md(paste0("***", col, "***"))),
    #     names(T73_caracteristicas_actores)[-1] # el-1 para quitar la primera col
    #   )
    # )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T73_caracteristicas_actores), by = 2))
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T73_caracteristicas_actores), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T73_caracteristicas_actores)))
    )%>%
    tab_options(
        .,
        table.font.size = 15, 
        column_labels.font.size = 13,
        # table.font.weight = "normal",
        # table.font.style = "normal",
        column_labels.border.top.color = "#ffffff",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "#ffffff",
        data_row.padding = px(10), #alto filas
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(5), #alto encabezados
        column_labels.padding.horizontal = px(4) # para ancho encabezados
    ) %>%
    opt_css(
        css = "
    #gt .gt_column_spanner_outer {
      padding-left: 0px !important;
      padding-right: 0px !important;
      border-bottom-width: 1px !important;
    }
    "
    )

G5_T73_caracteristicas_actores_1
gtsave(G5_T73_caracteristicas_actores_1,filename = "G5_T73_caracteristicas_actores_1.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.6_T77 caracteristicas_actores_2-------------------------------------------------------------------------------------------------------------------------------
G89.6_T77_caracteristicas_actores_2 <- T77_caracteristicas_actores_2 %>% gt(id = "gt")%>%
    cols_width(everything() ~ px(65))%>%
    cols_width( Personaje ~ px(210))%>%
    #añadimos spanner
    tab_spanner(
        label = md("**Conoce al Estado^1^** <br> <br> ***Escala de 4***"),
        columns = names(T77_caracteristicas_actores_2)[2:6],# Aplica a columnas 2 a 6
    )%>%
    tab_spanner(
        label = md("**Cumple lo que dice^2^** <br> <br> ***Escala de 4***"),
        columns = names(T77_caracteristicas_actores_2)[7:11]  # Aplica a columnas 7 a 12
    )%>%
    fmt_percent(
        columns = 2:ncol(T77_caracteristicas_actores_2),  
        decimals = 1
    )%>%
    #poner superindices a las labels de cols
    cols_label(
        `Mucho.1` =md("Mucho"),
        `Algo.1` = md("Algo"),
        `Poco.1` = md("Poco"),
        `Nada.1` = md("Nada"),
        `NS / NC.1` = md("NS / NC"),
    )%>% 
    #formato de spanners
    tab_style(
        style = list(
            cell_fill(color = "#52786D"), 
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold"),
            cell_borders(sides = "all", 
                         color = "#ffffff",
                         weight = px(1.5),)
        ),
        locations = cells_column_spanners()
    )%>%
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_column_spanners(),
            cells_title()
        )
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T77_caracteristicas_actores_2), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T77_caracteristicas_actores_2), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T77_caracteristicas_actores_2)))
    )%>%
    tab_options(
        .,
        table.font.size = 15, 
        column_labels.font.size = 13,
        # table.font.weight = "normal",
        # table.font.style = "normal",
        column_labels.border.top.color = "#ffffff",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "#ffffff",
        data_row.padding = px(10), #alto filas
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(5), #alto encabezados
        column_labels.padding.horizontal = px(4) # para ancho encabezados
    ) %>%
    opt_css(
        css = "
    #gt .gt_column_spanner_outer {
      padding-left: 0px !important;
      padding-right: 0px !important;
      border-bottom-width: 1px !important;
    }
    "
    )

G89.6_T77_caracteristicas_actores_2
gtsave(G89.6_T77_caracteristicas_actores_2,filename = "G6_T77_caracteristicas_actores_2.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.7_T82_caracteristicas_actores_3-------------------------------------------------------------------------------------------------------------------------------
G7_T82_caracteristicas_actores_3 <- T82_caracteristicas_actores_3 %>% gt(id = "gt")%>%
    cols_width(everything() ~ px(100))%>%
    cols_width( Personaje ~ px(210))%>%
    #añadimos spanner
    tab_spanner(
        label = md("**Buen(a) candidato(a) para diputado(a) federal^1^** <br> ***Escala de 2***"),
        columns = names(T82_caracteristicas_actores_3)[2:4],# Aplica a columnas 2 a 6
    )%>%
    tab_spanner(
        label = md("**Votaría para diputado(a) federal^2^** <br> ***Escala de 2***"),
        columns = names(T82_caracteristicas_actores_3)[5:7]  # Aplica a columnas 7 a 12
    )%>% 
    
    #formato de spanners
    tab_style(
        style = list(
            cell_fill(color = "#52786D"), 
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold"),
            cell_borders(sides = "all", 
                         color = "#ffffff",
                         weight = px(1.5),)
        ),
        locations = cells_column_spanners()
    )%>%
    cols_label(
        `NS / NC.1` =md("NS / NC"),
    )%>% 
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_column_spanners(),
            cells_title()
        )
    )%>%
    fmt_percent(
        columns = 2:ncol(T82_caracteristicas_actores_3),  
        decimals = 1
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T82_caracteristicas_actores_3), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T82_caracteristicas_actores_3), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T82_caracteristicas_actores_3)))
    )%>%
    tab_options(
        .,
        table.font.size = 15, 
        column_labels.font.size = 13,
        # table.font.weight = "normal",
        # table.font.style = "normal",
        column_labels.border.top.color = "#ffffff",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "#ffffff",
        data_row.padding = px(10), #alto filas
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(5), #alto encabezados
        column_labels.padding.horizontal = px(3) # para ancho encabezados
    ) %>%
    opt_css(
        css = "
    #gt .gt_column_spanner_outer {
      padding-left: 0px !important;
      padding-right: 0px !important;
      border-bottom-width: 1px !important;
    }
    "
    )

G7_T82_caracteristicas_actores_3
gtsave(G7_T82_caracteristicas_actores_3,filename = "G7_T82_caracteristicas_actores_3.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.7.1_T82_caracteristicas_actores_3_nueva-------------------------------------------------------------------------------------------------------------------------------
G7_T82_caracteristicas_actores_3_nueva <- T82_caracteristicas_actores_3_nueva %>% gt(id = "gt")%>%
    cols_width(everything() ~ px(100))%>%
    cols_width( Personaje ~ px(210))%>%
    #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
cols_width( saldo2 ~ px(120))%>%
    #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------  
#añadimos spanner
tab_spanner(
    label = md("**Buen(a) candidato(a) para diputado(a) federal^1^** <br> ***Escala de 2***"),
    columns = names(T82_caracteristicas_actores_3_nueva)[2:4],# Aplica a columnas 2 a 6
)%>%
    tab_spanner(
        label = md("**Votaría para diputado(a) federal^2^** <br> ***Escala de 2***"),
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        columns = names(T82_caracteristicas_actores_3_nueva)[5:8]  # Aplica a columnas 7 a 12
    )%>%
    cols_label(
        `saldo2` = md ("**Factor de votación** <br> <span style='font-size:10px;'>***(Votaría / Nunca votaría) * Conocimiento***</span>")
    )%>% 
    #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
#formato de spanners
tab_style(
    style = list(
        cell_fill(color = "#52786D"), 
        cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold"),
        cell_borders(sides = "all", 
                     color = "#ffffff",
                     weight = px(1.5),)
    ),
    locations = cells_column_spanners()
)%>%
    cols_label(
        `NS / NC.1` =md("NS / NC"),
    )%>% 
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("all"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_column_spanners(),
            cells_title()
        )
    )%>%
    fmt_percent(
        columns = 2:ncol(T82_caracteristicas_actores_3_nueva)-1,  
        decimals = 1
    )%>%
    fmt_number(
        columns = saldo2,
        decimals = 2
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T82_caracteristicas_actores_3_nueva), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T82_caracteristicas_actores_3_nueva), by = 2))
    )%>%
    #alinear porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T82_caracteristicas_actores_3_nueva)))
    )%>%
    tab_options(
        .,
        table.font.size = 15, 
        column_labels.font.size = 13,
        # table.font.weight = "normal",
        # table.font.style = "normal",
        column_labels.border.top.color = "#ffffff",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "#ffffff",
        data_row.padding = px(10), #alto filas
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(5), #alto encabezados
        column_labels.padding.horizontal = px(3) # para ancho encabezados
    ) %>%
    opt_css(
        css = "
    #gt .gt_column_spanner_outer {
      padding-left: 0px !important;
      padding-right: 0px !important;
      border-bottom-width: 1px !important;
    }
    "
    )

G7_T82_caracteristicas_actores_3_nueva
gtsave(G7_T82_caracteristicas_actores_3_nueva,filename = "G7_T82_caracteristicas_actores_3_nueva.PNG", path = "RESULTADOS/GRAFICOS/CAREOS",zoom = 4)

#G89.8_T16_P22_personaje_preferible------------------------------------------------------------------------------------------------------------------------------------------------------
#QUITAMOS FILA DE TOTALES
T16_P22_personaje_preferible <- T16_P22_personaje_preferible %>%
    filter(!Personaje %in% c("Total", "NANA"))

graf <- setdiff(unique(T16_P22_personaje_preferible$Personaje), especiales)
graf <- c((graf), especiales) #sin órden alfabético (si queremos orden meter sort) y luego los especiales

T16_P22_personaje_preferible$Personaje <- factor(T16_P22_personaje_preferible$Personaje, levels = graf)

G8_T16_P22_personaje_preferible <- ggplot(T16_P22_personaje_preferible, aes(y = fct_rev(Personaje), x = Frecuencia, fill = Personaje)) +
    geom_bar(stat = "identity", width = 0.8,show.legend = FALSE,) +
    coord_cartesian(clip = "off") +
    geom_text(aes(label = paste0(round(`Porcentaje`*100, 0), "%")),
              colour = "#404040", 
              size = 7,
              family = "Geomanist", fontface = "plain" ,
              hjust = -0.15, nudge_x = max(T16_P22_personaje_preferible$Frecuencia) * 0.01) +
    scale_fill_manual(values = define_colors(graf)) +  # Aplica los colores correctamente
    scale_x_continuous(expand = expansion(mult = c(0, .01))) + #estirar o contraer la barra
    #scale_y_discrete(expand = expansion(mult = 0.22)) + #aplastar la gráfica
    scale_y_discrete(expand = c(0, 0)) + # aplastar la grafica y cortar el eje justo al termino de las barras
    labs(title = "", x = "", y = "") +
    theme(
        plot.margin = margin(0, 50, 10, 0),
        text = element_text(family = "Geomanist"),#face = "italic"),
        axis.text.y = element_text(
            angle = 0, 
            hjust = 1, 
            size =  17, 
            colour = "#7F7F7F"),
        axis.text.x = element_text(colour = "transparent", size = 0.1),
        plot.background = element_rect(fill = "transparent", color ="transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "transparent"),
        axis.line.x.bottom = element_line(color = "transparent", linewidth = .2),
        axis.line.y.left = element_line(colour = "#d9d9d9", linewidth = .2),
        axis.ticks.y.left = element_line(colour = "transparent", linewidth = .5),
        axis.ticks.x = element_line(colour = "transparent", linewidth = .5),
        axis.line = element_line(lineend = "butt",)
        #aspect.ratio = 0.5
        #plot.title = element_text(colour = "#5E5E5E", size = 20, hjust = 0.5, vjust = .5)
    )

G8_T16_P22_personaje_preferible
ggsave(filename = paste0("RESULTADOS/GRAFICOS/CAREOS/G8_T16_P22_personaje_preferible_morena.png"), plot = G8_T16_P22_personaje_preferible, width = 6, height = 5, dpi = 300)

#G89.9_T84_puntaje_actores------------------------------------------------------------------------------------------------------------------------------------------------------

# PASO 1: Calcular Saldo en T82 
T82_caracteristicas_actores_Saldo <- T82_caracteristicas_actores_3 %>%
    mutate(Saldo = Votaría - `Nunca votaría`)
T67_actores_Saldo <- T67_actores %>%
    mutate(Saldo =  Buena - Mala) %>%
    inner_join(.,T82_caracteristicas_actores_Saldo, by = "Personaje", keep = FALSE) %>%
    select(Personaje,Saldo.x)

# PASO 2: Construir T84_puntaje_actores_anterior 
# Toma T84_puntaje_actores, sustituye la columna Votaría por Saldo formateado,
# conservando el \n2 (puntaje) si el original Votaría lo tenía
T84_puntaje_actores_anterior <- T84_puntaje_actores %>%
    mutate(
        # Valor numérico de Saldo desde T82 (en el mismo orden de filas)
        Saldo_num = T82_caracteristicas_actores_Saldo$Saldo,
        # Formatear como porcentaje con 1 decimal
        Saldo_pct = paste0(formatC(Saldo_num * 100, format = "f", digits = 1), "%"),
        # Si Votaría tenía \n (puntaje), conservarlo en Saldo
        Saldo = ifelse(
            grepl("\n", Votaría),
            paste0(Saldo_pct, "\n", sub(".*\n", "", Votaría)),
            Saldo_pct
        )
    ) %>%
    # Eliminar columna Votaría y auxiliares, dejar Saldo en su lugar
    select(-Votaría, -Saldo_num, -Saldo_pct) %>%
    relocate(Saldo, .before = Porcentaje) 


T84_puntaje_actores_anterior <- T84_puntaje_actores_anterior %>%
    mutate(
        # Valor numérico de Saldo desde T82 (en el mismo orden de filas)
        Saldo_num = T67_actores_Saldo$Saldo,
        # Formatear como porcentaje con 1 decimal
        Saldo_pct = paste0(formatC(Saldo_num * 100, format = "f", digits = 1), "%"),
        # Si Votaría tenía \n (puntaje), conservarlo en Saldo
        Saldo_op = ifelse(
            grepl("\n", Buena),
            paste0(Saldo_pct, "\n", sub(".*\n", "", Buena)),
            Saldo_pct
        )
    ) %>%
    # Eliminar columna Votaría y auxiliares, dejar Saldo en su lugar
    select(-Buena, -Saldo_num, -Saldo_pct) %>%
    relocate(Saldo_op, .before = honesto) 


valores_max <- c(2, 1.25, 0.25, 0.25, 0.25, 1, 2, 3)

columnas_numericas <- c(
    "Saldo_op",
    "honesto",
    "cercano",
    "conoce",
    "cumple",
    "Sí",
    "Saldo",
    "Porcentaje"
)

T84_recalculado <- T84_puntaje_actores_anterior %>%
    mutate(across(all_of(columnas_numericas),
                  ~ as.numeric(gsub("%", "",
                                    sub("\n.*", "", .))) / 100))


T84_recalculado <- T84_recalculado %>%
    mutate(across(all_of(columnas_numericas),
                  ~ case_when(
                      . == max(., na.rm = TRUE) ~ 
                          paste0(formatC(. * 100, format = "f", digits = 1),
                                 "%\n",
                                 valores_max[which(columnas_numericas == cur_column())]),
                      TRUE ~ 
                          paste0(formatC(. * 100, format = "f", digits = 1), "%")
                  )))


T84_recalculado <- T84_recalculado %>%
    rowwise() %>%
    mutate(`Puntaje final` =
               sum(as.numeric(sub(".*\n", "",
                                  c_across(all_of(columnas_numericas)))),
                   na.rm = TRUE)) %>%
    ungroup()

T84_recalculado$`Puntaje final` <-
    ifelse(T84_recalculado$`Puntaje final` == 0,
           "-",
           T84_recalculado$`Puntaje final`)

T84_puntaje_actores_anterior <- T84_recalculado

G89.9_T84_puntaje_actores <- T84_puntaje_actores_anterior %>% gt() %>%
    cols_width(everything() ~ px(75)) %>%
    cols_width(Sí ~ px(95)) %>%
    cols_width(Personaje ~ px(170)) %>%
    # Etiquetas de columnas
    cols_label(
        `Sí lo conoce` = md("Conocimiento"),
        `Saldo_op`     = md("Opinión positiva^1^ <br> Saldo de opinión <br> *%Buena - %Mala <br> [Escala de 3] <br> Valor=2*"),
        honesto        = md("Honesto^2^ <br> *Mucho + <br> ½ Algo <br> [Escala de 4] <br> Valor=1.25*"),
        cercano        = md("Cercano a la <br> gente^3^ <br> *Mucho + ½ Algo <br> [Escala de 4]<br>  Valor=0.25*"),
        `conoce`       = md("Conoce el <br>estado^4^ <br>*Mucho + ½ Algo <br>[Escala de 4] <br>Valor=0.25*"),
        cumple         = md("Cumple lo que <br>dice^5^ <br>*Mucho +<br> ½ Algo <br>[Escala de 4]<br> Valor=0.25*"),
        Sí             = md("Buen(a) candidato(a)<br> para diputado(a) federal^6^<br> *Sí <br>[Escala de 2] <br>Valor=1*"),
        Saldo          = md("Votaría^7^<br>Saldo de opinión<br>*Votaría - Nunca votaría<br>Valor=2*"),
        Porcentaje     = md("Preferencia <br> candidato de <br> MORENA^8^<br>*Valor=3*")
    ) %>%
    # Estilo de fuente general
    tab_style(
        style = list(
            cell_text(font = "Geomanist"),
            cell_borders(
                sides = c("left", "top", "bottom", "right"),
                color = "#ffffff",
                weight = px(1.5)
            )
        ),
        locations = list(
            cells_body(),
            cells_column_labels(),
            cells_title()
        )
    ) %>%
    # Formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff", align = "center", v_align = "middle", weight = "bold")
        ),
        locations = cells_column_labels()
    ) %>%
    # Fondo filas pares
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6"),
            cell_text(color = "#5F5F5F", weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T84_puntaje_actores_anterior), by = 2))
    ) %>%
    # Fondo filas impares
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F", weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T84_puntaje_actores_anterior), by = 2))
    ) %>%
    # Alineación columnas numéricas
    tab_style(
        style = list(cell_text(align = "center")),
        locations = cells_body(columns = (2:ncol(T84_puntaje_actores_anterior)))
    ) %>%
    # Negrita en columna de puntaje final
    tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(columns = (ncol(T84_puntaje_actores_anterior)))
    ) %>%
    # Negrita en texto después de \n
    text_transform(
        locations = cells_body(columns = everything()),
        fn = function(x) {
            x <- gsub("\n", "<br>", x)
            gsub("(<br>)(.*)", "<br><b>\\2</b>", x)
        }
    ) %>%
    tab_options(
        .,
        table.font.size                    = 13,
        column_labels.font.size            = 9,
        column_labels.border.top.color     = "#ffffff",
        column_labels.border.bottom.color  = "#ffffff",
        table_body.hlines.color            = "#ffffff",
        data_row.padding                   = px(4),
        table.border.bottom.color          = "#ffffff",
        table.border.bottom.width          = px(3),
        column_labels.padding              = px(9),
        column_labels.padding.horizontal   = px(1)
    )

G89.9_T84_puntaje_actores
gtsave(G89.9_T84_puntaje_actores, filename = "G9_T84_puntaje_actores.png",
       path = "RESULTADOS/GRAFICOS/CAREOS", zoom = 4)

#G89.9.1_T84_puntaje_actores_nueva------------------------------------------------------------------------------------------------------------------------------------------------------

G89.9_T84_puntaje_actores_nueva <- T84_puntaje_actores_nueva %>% gt()%>%
    cols_width(everything() ~ px(75))%>%
    cols_width(Sí ~ px(95))%>%
    cols_width( Personaje ~ px(170))%>%
    #poner superindices a las labels de cols
    cols_label(
        `Sí lo conoce` = md ("Conocimiento"),
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        saldo = md("**Factor de opinión^1^** <br> *(%Buena / %Mala) * Conocimiento <br> [Escala de 3] <br> Valor=2*"),
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        honesto = md("Honesto^2^ <br> *Mucho + <br> ½ Algo <br> [Escala de 4] <br> Valor=1.25*"),
        cercano = md("Cercano a la <br> gente^3^ <br> *Mucho + ½ Algo <br> [Escala de 4]<br>  Valor=0.25*"),
        `conoce` = md("Conoce el <br>estado^4^ <br>*Mucho + ½ Algo <br>[Escala de 4] <br>Valor=0.25*"),
        cumple = md("Cumple lo que <br>dice^5^ <br>*Mucho +<br> ½ Algo <br>[Escala de 4]<br> Valor=0.25*"),
        Sí = md("Buen(a) candidato(a)<br> para diputado(a) federal^6^<br> *Sí <br>[Escala de 2] <br>Valor=1*"),
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        saldo2 = md("**Factor de votación^7^** <br> *(Votaría / Nunca votaría) * Conocimiento <br> Valor=2*"),
        #🛑-🛑-🛑-CAMBIO PARA TERCERA RONDA-🛑-🛑-🛑------------------------------------------------------------
        Porcentaje =md("Preferencia <br> candidato de <br> MORENA^8^<br>*Valor=3*")
    )%>% 
    #estilo de fuente gral
    tab_style(
        style = list(cell_text(font = "Geomanist"),
                     cell_borders(
                         sides = c("left","top","bottom","right"),     
                         color = "#ffffff", 
                         weight = px(1.5))     
        ),    
        locations = list(
            cells_body(),          
            cells_column_labels(),
            cells_title()
        )
    )%>%
    #formato encabezado
    tab_style(
        style = list(
            cell_fill(color = "#52786D"),
            cell_text(color = "#ffffff",align = "center",v_align = "middle",weight = "bold")),
        locations = cells_column_labels()
    )%>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#F5F5F6") ,
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(2, nrow(T84_puntaje_actores_nueva), by = 2))
    ) %>%
    #fondo de filas
    tab_style(
        style = list(
            cell_fill(color = "#EAEBED"),
            cell_text(color = "#5F5F5F",weight = "normal")
        ),
        locations = cells_body(rows = seq(1, nrow(T84_puntaje_actores_nueva), by = 2))
    )%>%
    #alineamos porcentajes
    tab_style(
        style = list(
            cell_text(align = "center")
        ),
        locations = cells_body(columns = (2:ncol(T84_puntaje_actores_nueva)))
    )%>%
    #formato columna puntajes
    tab_style(
        style = list(
            cell_text(weight = "bold")),
        locations = cells_body(columns = (ncol(T84_puntaje_actores_nueva)))
    )%>%
    fmt_number(
        columns = c(saldo,saldo2),
        decimals = 2
    )%>%
    #cambiamos a negrito los puntajes
    text_transform(
        locations = cells_body(columns = everything()),
        fn = function(x) {
            x <- gsub("\n", "<br>", x) 
            gsub("(<br>)(.*)", "<br><b>\\2</b>", x) 
        }
    )%>%
    tab_options(
        .,
        table.font.size = 13, 
        column_labels.font.size = 9,
        # table.font.weight = "normal",
        # table.font.style = "normal",
        column_labels.border.top.color = "#ffffff",
        column_labels.border.bottom.color = "#ffffff",
        table_body.hlines.color = "#ffffff",
        data_row.padding = px(4),
        table.border.bottom.color = "#ffffff",
        table.border.bottom.width = px(3),
        column_labels.padding = px(9),
        column_labels.padding.horizontal = px(1)
    )

G89.9_T84_puntaje_actores_nueva
gtsave(G89.9_T84_puntaje_actores_nueva,filename = "G9_T84_puntaje_actores_nueva.png", path = "RESULTADOS/GRAFICOS/CAREOS",zoom=4)

#Finalizador del cronómetro
endtime <- Sys.time()
runtime <- endtime-starttime
runtime