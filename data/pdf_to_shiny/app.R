# about: running app 
library(shiny)
library(tidyverse)
library(purrr)

# importing data. Tip: if app is inside a larger project use here library for the relative path
df <- read_rds(here::here("output/df_shiny.rds")) 

# ui
ui <- fluidPage(
  tags$br(),
  tags$br(),
  tags$head(
    tags$style(
      # inline css: hard to translate to R!
      HTML("
      .card {
         border-width: 0px;
        transition: transform 0.5s cubic-bezier(0.165, 0.84, 0.44, 1) 0s;
      }

      .card:hover {
        transform: scale(1.1);
      }
        ")
    )
  ),
  theme = bslib::bs_theme(
    version = 5, 
    base_font = "Roboto Mono Light", 
  ),
  fluidRow(
    tagList(
      tags$style(
        # all this css code modify the dates' slider appearance.
        type = 'text/css', 
        '#big_slider  {
        padding-left: 25%;
        }',
        '#big_slider .irs-from {
        font-size: 19px;
        top:-13px;
        background-color: #FFFFFF;
        color: #15202c;
        z-index:5;
        }',
        '#big_slider .irs-to {
        font-size: 19px; 
        top:-13px; 
        background-color: #FFFFFF; 
        color: #15202c; 
        z-index:5;
        }',
        '#big_slider .irs-min {
        font-size: 19px;
        top:-13px;
        }',
        '#big_slider .irs-max {
        font-size: 19px;
        top:-13px;
        }',
        '#big_slider .irs-bar {
        background: #1c9af1; 
        border: 0px;
        }'
      ),
      tags$br(),
      tags$br(),
      div(
        id = 'big_slider',
        sliderInput(
          inputId = "year_filter",
          label = NULL,
          min = min(df$fecha),
          max = max(df$fecha),
          value = c(min(df$fecha), max(df$fecha)),
          dragRange = T,
          step = 1, 
          timeFormat = "%B %d, %Y", 
          ticks = F,
          width = "60%"
        )
      )
    ),
    selectInput(
      inputId = "tema_filter", 
      choices = df$tema %>% unique, 
      label = "Topic", 
      selected = df$tema %>% unique
    ),
    selectInput(
      inputId = "news_filter", 
      choices = df$periodico %>% unique, 
      label = "Where it was published", 
      selected = df$periodico %>% unique, 
      multiple = T
    ),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    uiOutput("cards")
  )
)

server <- function(input, output, session) {
  filtered_df <- reactive({
    df %>%
      filter(
        fecha >= input$year_filter[1] & fecha <= input$year_filter[2],
        tema %in% input$tema_filter,
        periodico %in% input$news_filter
      )
  })
  
  output$cards <- renderUI({
    tags$div(
      # this class was taken directly from bootsrap page
      class = "row row-cols-1 row-cols-md-6 g-4",
      map(filtered_df()$card, ~.x)
    )
    
  })
  
}

shinyApp(ui = ui, server = server)
