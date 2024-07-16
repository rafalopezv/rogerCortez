library(shiny)
library(tidyverse)
library(purrr)
library(here)
library(shinyWidgets) # Load the shinyWidgets library

# Importando datos
df <- read_rds(here::here("output/df_shiny.rds"))
Sys.setlocale(locale = "es_ES.UTF-8")

# Helper function to format dates in Spanish
format_date_spanish <- function(date) {
  months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
  weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
  
  formatted_date <- format(date, "%A %d de %B de %Y") %>% as.character()
  formatted_date <- gsub(paste(weekdays, collapse = "|"), function(x) weekdays[which(weekdays == x)], formatted_date)
  formatted_date <- gsub(paste(months, collapse = "|"), function(x) months[which(months == x)], formatted_date)
  
  return(formatted_date)
}

# UI
ui <- fluidPage(
  includeCSS("www/styles.css"),
  tags$head(
    tags$script(HTML("
      
      window.onscroll = function() {scrollFunction()};

      function scrollFunction() {
        if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
          document.getElementById('toTopBtn').style.display = 'block';
        } else {
          document.getElementById('toTopBtn').style.display = 'none';
        }
      }

      
      function topFunction() {
        document.body.scrollTop = 0;
        document.documentElement.scrollTop = 0;
      }
    "))
  ),
  tags$br(),
  theme = bslib::bs_theme(
    version = 5,
    base_font = "Roboto Mono Light"
  ),
  fluidRow(
    id = "cabecera",
    tags$br(),
    column(
      width = 6,
      class = "selector-container",
      dateRangeInput(
        separator = "a",
        inputId = "year_filter",
        label = "Elija el rango de fechas",
        start = as.Date(min(df$fecha)),
        end = as.Date(max(df$fecha)),
        min = as.Date(min(df$fecha)),
        max = as.Date(max(df$fecha)),
        format = 'dd  MM  yyyy',
        language = "es",
        width = "100%"
      )
    ),
    column(
      width = 6,
      class = "selector-container",
      selectInput(
        inputId = "tema_filter",
        choices = c("Todos", unique(df$tema)),
        label = "Elija el tema",
        selected = "Todos"
      )
    ),
    column(
      width = 6,
      class = "selector-container",
      selectInput(
        inputId = "news_filter",
        choices = c("Todos", unique(df$periodico)),
        label = "Elija la fuente",
        selected = "Todos"
      )
    ),
    column(
      width = 6,
      class = "selector-container",
      searchInput(
        inputId = "search_box",
        label = "Búsqueda específica",
        placeholder = "Escriba su búsqueda...",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "100%"
      )
    )
  ),
  fluidRow(
    column(
      class = "resultados",
      width = 12,
      align = "center",
      textOutput("counter"),
    ),
    tags$br(),
    tags$br(),
    tags$br(),
    
  ),
  fluidRow(
    column(
      width = 12,
      uiOutput("cards")
    )
  ),
  tags$button(
    id = "toTopBtn",
    class = "btn",
    onclick = "topFunction()",
    icon("arrow-up")
  )
)

# Server
server <- function(input, output, session) {
  filtered_df <- reactive({
    search_term <- input$search_box
    df_filtered <- df %>%
      filter(
        fecha >= input$year_filter[1] & fecha <= input$year_filter[2],
        (tema == input$tema_filter | input$tema_filter == "Todos"),
        (periodico == input$news_filter | input$news_filter == "Todos")
      )
    
    if (search_term != "") {
      search_filtered <- df_filtered %>%
        filter(
          str_detect(tolower(titulo), tolower(search_term))
        )
      if (nrow(search_filtered) == 0) {
        return(df_filtered) # Return all cards if search yields no results
      } else {
        return(search_filtered)
      }
    }
    
    df_filtered
  })
  
  output$counter <- renderText({
    n <- nrow(filtered_df())
    if (n == 0) {
      "No hay ninguna fuente con los filtros seleccionados"
    } else {
      paste(n, "fuentes encontradas")
    }
  })
  
  output$cards <- renderUI({
    req(filtered_df()) # Asegúrate de que los datos filtrados estén disponibles antes de renderizar
    
    tags$div(
      class = "row row-cols-1 row-cols-sm-2 row-cols-md-4 g-4",
      map(filtered_df()$card, ~ .x)
    )
  })
}

shinyApp(ui = ui, server = server)