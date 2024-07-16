# about: from a data frame of news links to a data base ready for shiny production
library(tidyverse)
library(rvest)
library(furrr)
library(janitor)
library(htmltools)

# activar  procesamiento parelelo
plan(multisession, workers = 12)

# base de noticias
df <- rio::import("input/links_noticias.csv") %>% 
  janitor::remove_empty()

# limpieza
df %>% 
  fill(anexo_original) %>% 
  mutate(
    tema = case_when(
      nchar(tema) == 0 ~ NA_character_,
      T ~ tema
    ),
    num = 1:nrow(.)
  ) %>% 
  fill(tema) %>% 
  select(num, everything()) -> df


# limpieza de temas
df %>% 
  mutate(
    tema = case_when(
      tema %in% c("economía y política económica", "economía, caída de ingReSoS fiScaleS y déficit de empReSaS eStataleS") ~ "Economía",
      tema %in% c("dichoS del gabinete de campaña", "campaña Evo: Fraude-golpe") ~ "Campaña Evo | Fraude-Golpe",
      tema %in% c("ADEPCOCA", "Coca, Cocaína") ~ "Coca-Cocaína",
      tema %in% c("tieRRa y teRRitoRio", "luchaS poR tieRRa y teRRitoRioS", "Conflictos por tierra y áreas protegidas", "Conflictos por tierra, territorios y socioambientales") ~ "Tierra-Territorio",
      tema == "Censo de población y vivienda" ~ "Censo",
      tema %in% c("Reforma judicial", "delitoS tcp", "fiScalía") ~ "Justicia",
      tema %in% c("Luchas internas del MAS", "Interna del MAS") ~ "MAS-IPSP",
      tema %in% c("ff. aa.", "Policía") ~ "Fuerzas Armadas & Policía",
      tema == "maRchaS y contRamaRchaS" ~ "Marchas y contramarchas",
      tema == "coRpoRativiSmo" ~ "Corporativismo",
      tema %in% c("cambio climático-medioambiente", "Incendios") ~ "Medio Ambiente",
      tema == "nuevaS autoRidadeS" ~ "Autoridades electas",
      T ~ tema
    )
  ) -> df

# limpieza fuente
df %>% 
  mutate(
    enlace = str_replace_all(enlace, "\\s+", ""),
    periodico = case_when(
      str_detect(enlace, "paginasiete") ~ "Página 7",
      str_detect(enlace, "eldeber") ~ "El Deber",
      str_detect(enlace, "elpais") ~ "El País (España)",
      str_detect(enlace, "lostiempos") ~ "Los Tiempos",
      str_detect(enlace, "la-razon") ~ "La Razón",
      str_detect(enlace, "ftierra") ~ "Fundación Tierra",
      str_detect(enlace, "corteidh") ~ "CIDH",
      str_detect(enlace, "cancilleria") ~ "Cancillería boliviana",
      enlace == "https://www.youtube.com/watch?v=c8Q95YS_AlM" ~ "Cabildeo Digital",
      enlace == "https://brujuladigital.net/opinion/economia-los-temas-que-no-debatimos-pero-que-deberiamosQuicoVelazco" ~ "Brujula Digital",
      enlace == "https://brujuladigital.net/opinion/economia-los-temas-que-no-debatimos-pero-que-deberiamosQuicoVelazco" ~ "Brujula Digital",
      enlace == "https://hparlante.wixsite.com/digital-media/single-post/2020/02/28/el-regreso-de-walter-ch%C3%A1vez" ~ "H Parlante",
      enlace == "https://abi.bo/index.php/polititca2/34651-molina-declaraciones-de-la-jefa-del-comando-sur-de-eeuu-muestran-poco-respeto-a-los-paises-del-triangulo-del-litio" ~ "ABI",
      enlace == "https://www.clarin.com/sociedad/nuevo-superalimento-gobierno-busca-impulsar-cannabis-cocina_0_dAL6Hji72y.html" ~ "Clarin",
      enlace == "HTTPS://WWW.LOSTIEMPOS.COM/ACTUALIDAD/ECONOMIA/20230713/MILENIO-40-DEL-DEFICIT-FISCAL-2022-SE-FINANCIO-CREDITOS-DEL-BANCO" ~ "Los Tiempos",
      enlace == "https://eldebr.com.bo/pais/diputado-del-mas-hizo-37-giros-a-cuatro-paises-por-el-valor-de-us-51-millones_317119" ~ "El Deber",
    ),
    enlace = case_when(
      enlace == "https://eldebr.com.bo/pais/diputado-del-mas-hizo-37-giros-a-cuatro-paises-por-el-valor-de-us-51-millones_317119" ~ "https://eldeber.com.bo/pais/diputado-del-mas-hizo-37-giros-a-cuatro-paises-por-el-valor-de-us-51-millones_317119",
      enlace == "https://eldeber.com.bo/pais/el-censo-mostrara-un-pais-mas-urbano-y-preven-que-revelara-fallas-del-padron_301337ttps://eldeber.com.bo/santa-cruz/afines-al-MAS-advierten-con-cercar-santa-cruz-si-no-se-levanta-el-paro-en-48-horas_301363" ~ "https://eldeber.com.bo/pais/el-censo-mostrara-un-pais-mas-urbano-y-preven-que-revelara-fallas-del-padron_301337", 
      T ~ enlace
    )
  ) -> df


# funcion para verificar enlaces vivos
check_url <- function(url) {
  error_report <- tryCatch(
    expr = read_html(url),
    error = function(e) e
  )
  
  if (inherits(error_report, "error")) {
    cat(paste0("Error: ", error_report$message, "\n"))
    return(paste0("Error: ", error_report$message))
  } else {
    cat("Success: Sin errores\n")
    return("Success: Sin errores")
  }
}

# ejecución de función check_url
df %>% 
  mutate(
    check_url = future_map_chr(.$enlace, check_url, .progress = T) 
  ) -> df

# filtrar links vivos
df %>% 
  filter(check_url == "Success: Sin errores") -> df

# funcion para extraer la fecha de publicación de los artículos de prensa
published_date <- function(url, idOrClass) {
  page <- read_html(url)
  date_extracted <- html_text(html_node(page, idOrClass))
  
  return(date_extracted)
}

# funcion para extraer el titulo de los articulos
title <- function(url) {
  page <- read_html(url)
  title_extracted <- html_text(html_node(page, "title"))
  
  return(title_extracted)
} 

# execución de functiones
df %>% 
  mutate(
    fecha = case_when(
      periodico == "El Deber" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".dateNote"), .progress = T),
      periodico == "La Razón" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".p-lg-0"), .progress = T),
      periodico == "Página 7" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".date"), .progress = T),
      periodico == "Los Tiempos" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".date-publish"), .progress = T),
      periodico == "El País (España)" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = "#article_date_p"), .progress = T),
      periodico == "CIDH" ~ "2021-06-07"
    )
  ) -> df

df %>% 
  mutate(
    titulo = case_when(
      periodico == "CIDH" ~ "Opinión Consultiva OC-28/21:  La figura de la reelección presidencial indefinida en sistemas presidenciales en el contexto del sistema interamericano de derechos humanos",
      periodico == "Fundación Tierra" ~ "Despojo de tierras de comunidades por el agronegocio boliviano",
      T ~ future_map_chr(.$enlace, title, .progress = T)
    )
  ) -> df

# limpieza de titulos
df %>% 
  mutate(
    titulo = str_replace(titulo, "\\|", ""),
    titulo = str_replace(titulo, "El Deber", ""),
    titulo = str_replace(titulo, "- La Razón", ""),
    titulo = str_replace(titulo, "Los Tiempos", ""),
    titulo = str_replace(titulo, "EL PAÍS México", ""),
    titulo = str_replace(titulo, "EL PAÍS América", ""),
    titulo = str_replace(titulo, "\\|", ""),
    titulo = str_trim(titulo, side = "both")
  ) -> df


# arreglo de fechas
df %>% 
  filter(periodico == "El Deber") -> eldb

eldb %>% 
  separate(fecha , into = c("fecha", "null"), sep = ",") %>% 
  select(-null) %>% 
  mutate(fecha = gsub(" de ", "-", .$fecha)) %>% 
  separate(fecha , into = c("dia", "mes", "año"), sep = "-") %>% 
  mutate(
    mes = case_when(
      mes == "enero" ~ "01",
      mes == "febrero" ~ "02",
      mes == "marzo" ~ "03",
      mes == "abril" ~ "04",
      mes == "mayo" ~ "05",
      mes == "junio" ~ "06",
      mes == "julio" ~ "07",
      mes == "agosto" ~ "08",
      mes == "septiembre" ~ "09",
      mes == "octubre" ~ "10",
      mes == "noviembre" ~ "11",
      mes == "diciembre" ~ "12",
      T ~ mes
    ),
    dia = trimws(dia),
    nchar = nchar(dia),
    dia = case_when(
      nchar == 1 ~ paste0("0", dia),
      T ~ dia
    ),
    fecha = paste0(año, "-", mes, "-", dia) %>% as.Date()
  ) -> eldb

# resolver casos específicos de el deber
eldb %>% 
  mutate(
    fecha = case_when(
      enlace == "https://eldeber.com.bo/pais/garcia-linera-desmiente-declaraciones-del-general-terceros-y-reuniones-con-el-alto-mando_238112" ~ "2021-07-07" %>% as.Date(),
      enlace == "https://eldeber.com.bo/pais/relatora-especial-sobre-la-situacion-de-los-defensores-de-ddhh-observa-presunta-criminalizacion-cont_312872juiciomontadocontraWaldoA." ~ "2023-01-30" %>% as.Date(),
      T ~ fecha
    ),
    enlace = case_when(
      enlace == "https://eldeber.com.bo/pais/relatora-especial-sobre-la-situacion-de-los-defensores-de-ddhh-observa-presunta-criminalizacion-cont_312872juiciomontadocontraWaldoA." ~ "https://eldeber.com.bo/pais/relatora-especial-sobre-la-situacion-de-los-defensores-de-ddhh-observa-presunta-criminalizacion-cont_312872juiciomontadocontraWaldoA",
      T ~ enlace
    )
  ) -> eldb

# lso tiempos
df %>% 
  filter(periodico == "Los Tiempos") -> lt

lt %>% 
  mutate(fecha = gsub("Publicado el ", "", .$fecha) %>% trimws) %>% 
  mutate(fecha = gsub(" a ", ",", .$fecha) %>% trimws) %>% 
  # pull(fecha)
  separate(fecha, into = c("fecha", "null"), sep = ",") %>% 
  select(-null) %>% 
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) -> lt

# casos específicos los tiempos
lt %>% 
  mutate(
    fecha = case_when(
      enlace == "https://www.lostiempos.com/actualidad/opinion/20230721/columna/verdaderos-peligros-deuda-publica-bolivianaQuicoVelazco" ~ "2023-07-21" %>% as.Date(),
      enlace == "https://www.lostiempos.com/especial-multimedia/20230605/explotacion-oro-petroleo-afecta-seriamente-6-areas-protegidas" ~ "2023-06-05" %>% as.Date(),
      enlace == "https://www.lostiempos.com/especial-multimedia/20230507/DIARIO-DEL-PADRE-PICA-ACUSADO-ABUSAR-SEXUALMENTE-85-NINOS-SACUDE" ~ "2023-05-07" %>% as.Date(),
      T ~ fecha
    )
  ) -> lt

# el pais españa
df %>% 
  filter(periodico == "El País (España)") -> elp

elp %>% 
  separate(fecha, into = c("fecha", "null"), sep = " - ") %>% 
  select(-null) %>% 
  separate(fecha, into = c("dia", "mes", "año"), sep = " ") %>% 
  mutate(
    mes = case_when(
      mes == "jun" ~ "06",
      mes == "feb" ~ "02",
      mes == "jul" ~ "07",
      mes %in% c("may", "may.") ~ "05",
      mes == "abr" ~ "04"
    ),
    fecha = paste0(año, "-", mes, "-", dia) %>% as.Date()
  ) -> elp

# armado de df
df %>% 
  filter(!periodico %in% c("El País (España)", "Los Tiempos", "El Deber")) -> df

df %>% 
  mutate(
    fecha = as.Date(fecha),
    fecha = case_when(
      enlace == "https://www.youtube.com/watch?v=c8Q95YS_AlM" ~ "2022-05-03" %>% as.Date(),
      enlace == "https://brujuladigital.net/opinion/economia-los-temas-que-no-debatimos-pero-que-deberiamosQuicoVelazco" ~ "2022-11-19" %>% as.Date(),
      enlace == "https://hparlante.wixsite.com/digital-media/single-post/2020/02/28/el-regreso-de-walter-ch%C3%A1vez" ~ "2022-02-28" %>% as.Date(),
      enlace == "https://abi.bo/index.php/polititca2/34651-molina-declaraciones-de-la-jefa-del-comando-sur-de-eeuu-muestran-poco-respeto-a-los-paises-del-triangulo-del-litio" ~ "2023-03-12" %>% as.Date(),
      T ~ fecha
    ),
    titulo = case_when(
      fecha == "2022-05-03" ~ "Día de la libertad de prensa: ¡Amalia responde al hijo de Arce!",
      fecha == "2022-11-19" ~ "Los temas que no debatimos pero deberíamos",
      fecha == "2022-02-28" ~ "El regreso de Walter Chavez",
      fecha == "2023-03-12" ~ "Molina: Declaraciones de la jefa del Comando Sur de EEUU muestran poco respeto a los países del triángulo del litio",
      T ~ titulo
    ),
    enlace = case_when(
      fecha == "2022-11-19" ~ "https://brujuladigital.net/opinion/economia-los-temas-que-no-debatimos-pero-que-deberiamos",
      T ~ enlace
    )
  ) %>% 
  filter(!is.na(fecha)) %>% 
  bind_rows(., lt, elp, eldb) -> df

# arreglos finales
df %>% 
  mutate(
    enlace = case_when(
      enlace == "https://eldeber.com.bo/pais/relatora-especial-sobre-la-situacion-de-los-defensores-de-ddhh-observa-presunta-criminalizacion-cont_312872juiciomontadocontraWaldoA" ~ "https://eldeber.com.bo/pais/relatora-especial-sobre-la-situacion-de-los-defensores-de-ddhh-observa-presunta-criminalizacion-cont_312872",
      T ~ enlace
    ), 
    titulo = case_when(
      enlace == "https://eldeber.com.bo/pais/relatora-especial-sobre-la-situacion-de-los-defensores-de-ddhh-observa-presunta-criminalizacion-cont_312872" ~ "Relatora Especial sobre la situación de los defensores de DDHH observa presunta criminalización contra Waldo Albarracín",
      enlace == "https://www.lostiempos.com/especial-multimedia/20230605/explotacion-oro-petroleo-afecta-seriamente-6-areas-protegidas" ~ "La explotación de oro y petróleo afecta seriamente 6 áreas protegidas",
      enlace == "https://www.lostiempos.com/especial-multimedia/20230507/DIARIO-DEL-PADRE-PICA-ACUSADO-ABUSAR-SEXUALMENTE-85-NINOS-SACUDE" ~ "El diario del padre ‘Pica’, acusado de abusar sexualmente a 85 niños, sacude a la Iglesia Católica",
      T ~ titulo
    )
  ) -> df

#  crear clases para tarjetas bootstrap
df %>% 
  mutate(
    clase = case_when(
      periodico == "El Deber" ~ "card h-100 bg-success",
      periodico == "El País (España)" ~ "card h-100 bg-primary",
      periodico == "CIDH" ~ "card h-100 bg-dark",
      periodico == "La Razón"  ~ "card h-100 bg-danger",
      periodico == "Los Tiempos" ~ "card h-100 bg-warning",
      periodico == "Fundación Tierra" ~ "card h-100 bg-info",
      T ~ "card h-100 bg-light"
    )
  ) -> df


# cambiar locale a español para extraer fechas literales
# nota: ajustar de acuerdo a sistema operativo. El mío macOS Sonoma 14.5
Sys.setlocale(locale = "es_ES.UTF-8")

# ad column with literal date
df %>% 
  mutate(
    fecha_literal = format(fecha, "%d de %B de %Y")
  ) -> df

# funcion para crear tarjetas
card <- function(.title, .paper, .date, .class, .link) {
  tags$div(
    class = "col",
    tags$div(
      class = .class,
      tags$div(
        class = "card-body",
        tags$h5(class = "card-title", .title),
        tags$br(),
        tags$p(class = "card-text", .paper),
        tags$p(class = "card-text", .date),
        # hacer toda la tarjta clickeable
        tags$a(class = "stretched-link", href=.link)
      )
    )
  )
}

cards <- list()

# crear tarjetas
for (i in seq_len(nrow(df))) {
  cards[[i]] <- card(
    .title = df[i, "titulo"], 
    .paper = df[i, "periodico"], 
    .date = df[i, "fecha_literal"],
    .class = df[i, "clase"],
    .link = df[i, "enlace"]
  )
}

# añadir tarjetas a data frame
df %>% 
  mutate(
    card = cards
  ) -> df

# exportar data frame para shiny en producción
df %>% 
  select(-dia, -mes, -año, -nchar, -num, -anexo_original, -check_url) %>% 
  write_rds("output/df_shiny.rds")








