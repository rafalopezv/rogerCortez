# about: from a data frame of news links to a data base ready for shiny production
library(tidyverse)
library(rvest)
library(furrr)

# activate parallel computing
plan(multisession, workers = 6)

# this data frame was mainly build with chatgpt v. 3.5: 
df <- rio::import("input/links_noticias.csv")

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


# format column tema (topic)
df %>% 
  mutate(
    tema = str_to_sentence(tema),
    tema = case_when(
      tema == "Campaña evo: fraude-golpe" ~ "Campaña Evo: fraude-golpe",
      tema == "Cidh y giei" ~ "CIDH y GEI",
      tema == "Murillo miami" ~ "Murillo Miami",
      tema == "Delitos tcp" ~ "Delitos TCP",
      tema == "Ff. aa." ~ "FF.AA",
      # cambio porque es la misma idea
      tema == "Luchas por tierra y territorios" ~ "Tierra y territorio",
      T ~ tema
    )
  ) -> df

# format column periodico (newspaper)
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
      str_detect(enlace, "cancilleria") ~ "Cancillería boliviana"
    )
  ) -> df

# function to verify links are valid
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

# function execution
df %>% 
  mutate(
    check_url = future_map_chr(.$enlace, check_url, .progress = T) 
  ) -> df

# filter out links with 404 or any other client error message 
df %>% 
  filter(check_url == "Success: Sin errores") -> df

# function to extract date article was published
published_date <- function(url, idOrClass) {
  page <- read_html(url)
  date_extracted <- html_text(html_node(page, idOrClass))
  
  return(date_extracted)
}

# function to extract article title
title <- function(url) {
  page <- read_html(url)
  title_extracted <- html_text(html_node(page, "title"))
  
  return(title_extracted)
} 

# execution of functions: title and published_date
# note: html ids and classes for extraction were examined inspecting newspaper web sites
df %>% 
  mutate(
    fecha = case_when(
      periodico == "El Deber" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".dateNote"), .progress = T),
      periodico == "La Razón" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".p-lg-0"), .progress = T),
      periodico == "Página 7" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".date"), .progress = T),
      periodico == "Los Tiempos" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = ".date-publish"), .progress = T),
      periodico == "El País (España)" ~ future_map_chr(.$enlace, ~published_date(., idOrClass = "#article_date_p"), .progress = T),
      periodico == "CIDH" ~ "2021-06-07", 
      num %in% c("69", "70") ~ "2021-05-01"
    )
  ) -> df

df %>% 
  mutate(
    titulo = case_when(
      periodico == "CIDH" ~ "Opinión Consultiva OC-28/21:  La figura de la reelección presidencial indefinida en sistemas presidenciales en el contexto del sistema interamericano de derechos humanos",
      num %in% c("69", "70") ~ "Despojo de tierras de comunidades por el agronegocio boliviano",
      T ~ future_map_chr(.$enlace, title, .progress = T)
    )
  ) -> df

# cleaning column titulo 
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


# note: before doing this see the variability of column fecha
# instead of cleaning this variability through code, I passed a the orginal string of dates to chatgpt and got the fromatted ones
fechas_formato <- c("2021-08-07",
  "2021-08-25",
  "2021-07-20",
  "2021-07-19",
  "2021-06-18",
  "2021-08-15",
  "2021-06-23",
  "2021-06-22",
  "2021-06-24",
  "2021-05-10",
  "2021-08-25",
  "2021-08-25",
  "2021-06-27",
  "2021-06-07",
  "2021-08-13",
  "2021-08-13",
  "2021-08-10",
  "2021-08-14",
  "2021-08-19",
  "2021-08-25",
  "2021-08-19",
  "2021-08-27",
  "2021-08-18",
  "2021-08-18",
  "2021-07-24",
  "2021-08-23",
  "2021-08-23",
  "2021-08-24",
  "2021-08-19",
  "2021-08-26",
  "2021-06-28",
  "2021-07-22",
  "2021-08-29",
  "2021-05-04",
  "2021-08-25",
  "2021-06-13",
  "2021-08-13",
  "2021-08-13",
  "2021-08-10",
  "2021-08-14",
  "2021-08-19",
  "2021-08-26",
  "2021-08-18",
  "2021-08-24",
  "2021-08-23",
  "2021-08-23",
  "2021-08-24",
  "2021-08-19",
  "2021-05-08",
  "2021-07-05",
  "2021-06-11",
  "2021-05-27",
  "2021-07-05",
  "2021-07-15",
  "2021-08-27",
  "2021-07-28",
  "2021-08-24",
  "2021-08-01",
  "2021-03-18",
  "2021-05-01",
  "2021-05-01",
  "2021-07-25",
  "2021-07-02",
  "2021-07-20",
  "2021-05-03",
  "2021-07-26",
  "2021-07-05",
  "2021-05-06",
  "2021-06-28",
  "2021-07-19",
  "2021-07-15",
  "2021-06-25",
  "2021-06-02",
  "2021-08-21",
  "2021-08-25",
  "2021-08-25",
  "2021-08-24",
  "2021-08-09",
  "2021-08-10",
  "2021-08-22",
  "2021-08-23",
  "2021-08-21",
  "2021-05-13",
  "2021-05-23",
  "2021-06-28",
  "2021-07-19",
  "2021-08-09",
  "2021-07-04",
  "2021-06-04",
  "2021-06-21",
  "2021-05-01",
  "2021-05-09",
  "2021-05-22",
  "2021-05-24",
  "2021-06-13",
  "2021-06-18",
  "2021-06-14",
  "2021-09-03",
  "2021-08-14",
  "2021-07-01",
  "2021-05-27",
  "2021-06-20",
  "2021-06-07",
  "2021-05-05",
  "2021-05-04",
  "2021-06-11",
  "2021-07-27",
  "2021-06-01",
  "2021-05-27",
  "2021-05-19",
  "2021-05-18",
  "2021-05-14",
  "2021-07-15",
  "2021-08-23",
  "2021-07-04",
  "2021-07-07",
  "2021-07-08",
  "2021-07-06",
  "2021-07-06",
  "2021-07-06",
  "2021-08-23",
  "2021-08-27",
  "2021-06-24",
  "2021-06-26",
  "2021-07-14",
  "2021-08-27",
  "2021-08-19",
  "2021-09-17",
  "2021-09-18",
  "2021-09-27",
  "2021-09-29",
  "2021-10-07",
  "2021-10-07",
  "2021-10-08",
  "2021-10-10",
  "2021-10-11",
  "2021-10-11",
  "2021-10-11",
  "2021-10-12",
  "2021-10-12",
  "2021-10-11",
  "2021-10-14",
  "2021-10-18",
  "2021-10-18",
  "2021-10-20",
  "2021-10-22",
  "2021-10-21",
  "2021-10-22",
  "2021-11-01",
  "2021-11-02",
  "2021-11-02",
  "2021-11-05",
  "2021-11-05",
  "2021-11-06",
  "2021-11-08",
  "2021-11-08",
  "2021-11-08",
  "2021-11-12",
  "2021-11-12",
  "2021-11-12",
  "2021-11-12",
  "2021-11-12",
  "2021-11-12",
  "2021-11-14",
  "2021-11-14",
  "2021-11-14",
  "2021-11-14",
  "2021-11-15",
  "2021-11-15",
  "2021-11-16",
  "2021-11-17",
  "2021-08-04",
  "2021-03-18",
  "2021-08-24",
  "2021-08-02",
  "2021-11-19",
  "2021-11-06",
  "2021-11-05",
  "2021-11-03",
  "2021-11-01",
  "2021-11-02",
  "2021-11-01",
  "2021-11-02",
  "2021-11-02",
  "2021-11-02",
  "2021-11-01",
  "2021-10-31",
  "2021-11-01",
  "2021-10-31",
  "2021-10-16",
  "2021-10-16",
  "2021-10-08",
  "2021-10-06",
  "2021-09-24",
  "2021-09-24",
  "2021-09-20",
  "2021-09-20",
  "2021-09-20",
  "2021-09-19",
  "2021-09-19",
  "2021-09-17",
  "2021-11-25",
  "2021-11-25",
  "2021-11-18",
  "2021-11-22",
  "2021-11-01",
  "2021-11-04",
  "2021-11-05",
  "2021-11-19",
  "2021-11-07",
  "2021-11-02",
  "2021-11-02",
  "2021-11-25",
  "2021-11-25",
  "2021-08-10",
  "2021-08-19",
  "2021-08-25",
  "2021-08-31",
  "2021-09-01",
  "2021-09-03",
  "2021-09-05",
  "2021-09-06",
  "2021-09-06",
  "2021-09-19",
  "2021-09-17",
  "2021-09-03",
  "2021-08-29",
  "2021-09-21",
  "2021-09-23",
  "2021-09-25",
  "2021-10-08",
  "2021-10-20",
  "2021-10-22",
  "2021-10-21",
  "2021-10-22",
  "2021-11-02",
  "2021-11-03",
  "2021-11-01",
  "2021-11-10",
  "2021-11-15",
  "2021-11-10",
  "2021-11-21",
  "2021-11-17",
  "2021-11-16",
  "2021-11-25",
  "2021-11-25", 
  "2021-11-22") 

#  introduce right date format and add booststrap class to get right colors on cards 
df %>% 
  mutate(
    fecha = as.Date(fechas_formato),
    clase = case_when(
      periodico == "El Deber" ~ "card h-100 bg-success",
      periodico == "Página 7" ~ "card h-100 bg-warning",
      periodico == "CIDH" ~ "card h-100 bg-dark",
      periodico == "La Razón"  ~ "card h-100 bg-danger",
      periodico == "Los Tiempos" ~ "card h-100 bg-light",
      periodico == "Fundación Tierra" ~ "card h-100 bg-info",
      T ~ "card h-100 bg-light"
    )
  ) -> df

# Change locale to get literal dates in spanish
# note: this code my not work in your computer. I'm using macOS Monterrey 12.5.1
Sys.setlocale(locale = "es_ES.UTF-8")

# ad column with literal date
df %>% 
  mutate(
    fecha_literal = format(fecha, "%d de %B de %Y")
  ) -> df

# function to create bootstrap cards based on data frame
# tip: give pure html code from bootstrap site to chatgpt and ask for translation into shiny 
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
        # super tip: this makes the whole card linkable
        tags$a(class = "stretched-link", href=.link)
      )
    )
  )
}

cards <- list()

# iterate over the rows of the data frame and create the cards
for (i in seq_len(nrow(df))) {
  cards[[i]] <- card(
    .title = df[i, "titulo"], 
    .paper = df[i, "periodico"], 
    .date = df[i, "fecha_literal"],
    .class = df[i, "clase"],
    .link = df[i, "enlace"]
  )
}

# create column with shiny cards
df %>% 
  mutate(
    card = cards
  ) -> df

# export data frame ready for shiny production
df %>% 
  write_rds("data/noticias/output/df_shiny.rds")






