# sobre: graficos/tablas 2019_3
library(tidyverse)
library(highcharter)
library(reactable)
library(reactablefmtr)
library(ggchicklet)

df <- rio::import("data/ano_2019_3/viaciencia_conteo.csv") 

# interactivo claro computo
cols <- c("#0047AB", "#F26524", "#E10915", "#4CBB17", "#00A18A", "#FFC0CB", "#FF0000", "#0080FF", "#D6D6D6")

df %>% 
  arrange(desc(valor)) %>% 
  hchart("bar", hcaes(candidato, valor, color = cols), name = "Conteo rápido", color = "#15202c") %>% 
  hc_title(text = "Gráfico 1. Conteo rápido al 100%") %>% 
  hc_tooltip(
    valueSuffix = "%",
    borderWidth = 1/20,
    style = list(
      fontSize = "16"
    )
  ) %>% 
  hc_yAxis(
    title = list(text = "valor del conteo rápido")
  ) %>% 
  hc_xAxis(title = list(text = "candidatura")) %>% 
  hc_chart(style = list(fontFamily = "Raleway")) %>% 
  hc_plotOptions(
    bar = list(
      borderRadius = 7, 
      borderWidth = 0
    )
  ) -> hc_2019_3

# interactivo oscuro computo
cols <- c("#0047AB", "#F26524", "#E10915", "#4CBB17", "#00A18A", "#FFC0CB", "#FF0000", "#0080FF", "#D6D6D6")

df %>% 
  arrange(desc(valor)) %>% 
  hchart("bar", hcaes(candidato, valor, color = cols), name = "Conteo rápido", color = "#15202c") %>% 
  hc_title(
    text = "Gráfico 1. Conteo rápido al 100%",
    style = list(
      color = "#f7f9f9"
    )
  ) %>% 
  hc_tooltip(
    valueSuffix = "%",
    borderWidth = 1/20, 
    backgroundColor = highcharter::hex_to_rgba(x = "#15202c", alpha = .9),
    style = list(
      color = "#F5F5F5",
      fontSize = 16
    )
  ) %>% 
  hc_yAxis(
    gridLineColor = '#81868e',
    gridLineWidth = .1,
    title = list(
      text = "valor del conteo rápido",
      style = list(color = "#f7f9f9")
    ),
    labels = list(
      style = list(
        color = "#f7f9f9"
      )
    )
  ) %>% 
  hc_xAxis(
    gridLineColor = '#d0d2d4',
    title = list(
      text = "candidatura", 
      style = list(color = "#f7f9f9")
    ),
    labels = list(
      style = list(
        color = "#f7f9f9"
      )
    )
  ) %>% 
  hc_chart(
    style = list(
      fontFamily = "Raleway",
      backgroundColor = "#15202c"
    )
  ) %>% 
  hc_plotOptions(
    bar = list(
      borderWidth = 0,
      borderRadius = 5
    )
  ) -> hc_2019_3_dark

# estatico computo
scale_fill_estatico <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(
      c("#0047AB", "#F26524", "#E10915", "#4CBB17", "#00A18A", "#FFC0CB", "#FF0000", "#0080FF", "#D6D6D6"),
      df$candidato
    ), 
    ...
  )
}

# estatico conteo rapido
df %>% 
  arrange(desc(valor)) %>% 
  mutate(
    candidato = as.factor(candidato),
    num = 1:nrow(.)
  ) %>% 
  ggplot(aes(fct_reorder(candidato, num, .desc = F), valor, fill = candidato)) +
  geom_chicklet(color = NA) +
  hrbrthemes::theme_ipsum_rc(
    base_family = "Raleway", 
    grid = "Y", 
    base_size = 10
  ) +
  geom_text(
    aes(
      label = paste0(valor, "%"), 
      family = "Raleway"
    ), 
    vjust = -.8,
    size = 2.5
  ) +
  labs(
    title = "Gráfico 1. Conteo rápido al 100%",
    x = "valor del conteo rápido",
    y = "candidatura",
    caption = "Del auge a la descomposición de un proceso de cambio\nBolivia 2013-2021"
  ) +
  theme(
    plot.caption = element_text(family = "Raleway", size = 5),
    plot.title = element_text(size = 11),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x =  element_text(size = 6),
    axis.text.y =  element_text(size = 6)
  ) +
  theme(
    legend.position = "none"
  ) +
  scale_fill_estatico() -> plot_ano_2019_3

# interactivo computo claro
df <- rio::import("data/ano_2019_3/computo_2019.csv") 

cols <- c("#0047AB", "#F26524", "#E10915", "#4CBB17", "#00A18A", "#FFC0CB", "#FF0000", "#0080FF", "#D6D6D6")

df %>% 
  mutate(prop = (prop.table(computo)*100) %>% round(., 2)) %>% 
  arrange(desc(prop)) %>% 
  hchart("bar", hcaes(partido, computo, color = cols)) %>% 
  hc_title(text = "Gráfico 2. Resultados oficiales parciales del OEP") %>% 
  hc_tooltip(
    table = T,
    borderWidth = 1/20,
    style = list(
      fontSize = "16"
    ),
    pointFormat = paste0(
      "<b>{point.partido}</b><br><br>
       <b>Votos:</b> {point.computo}<br>
       <b>Porcentaje [%]:</b> {point.prop}%"
    ), 
    headerFormat = ""
  ) %>% 
  hc_yAxis(
    title = list(text = "cantidad y porcentaje de votos")
  ) %>% 
  hc_xAxis(title = list(text = "partido")) %>% 
  hc_chart(style = list(fontFamily = "Raleway")) %>% 
  hc_plotOptions(
    bar = list(
      borderRadius = 7, 
      borderWidth = 0
    )
  ) -> hc_2019_3_1

# computo dark
df %>% 
  mutate(prop = (prop.table(computo)*100) %>% round(., 2)) %>% 
  arrange(desc(prop)) %>% 
  hchart("bar", hcaes(partido, computo, color = cols)) %>% 
  hc_title(
    text = "Gráfico 2. Resultados oficiales parciales del OEP", 
    style = list(
      color = "#f7f9f9"
    )
  ) %>% 
  hc_tooltip(
    backgroundColor = highcharter::hex_to_rgba(x = "#15202c", alpha = .9),
    style = list(
      color = "#F5F5F5",
      fontSize = 16
    ),
    table = T,
    borderWidth = 1/20,
    style = list(
      fontSize = "16"
    ),
    pointFormat = paste0(
      "<b>{point.partido}</b><br><br>
       <b>Votos:</b> {point.computo}<br>
       <b>Porcentaje [%]:</b> {point.prop}%"
    ), 
    headerFormat = ""
  ) %>% 
  hc_yAxis(
    gridLineColor = '#81868e',
    gridLineWidth = .1,
    title = list(
      text = "cantidad y porcentaje [%] de votos",
      style = list(color = "#f7f9f9")
    ),
    labels = list(
      style = list(
        color = "#f7f9f9"
      )
    )
  ) %>% 
  hc_xAxis(
    gridLineColor = '#d0d2d4',
    title = list(
      text = "partido o agrupación ciudadana", 
      style = list(color = "#f7f9f9")
    ),
    labels = list(
      style = list(
        color = "#f7f9f9"
      )
    )
  ) %>% 
  hc_chart(
    style = list(
      fontFamily = "Raleway",
      backgroundColor = "#15202c"
    )
  ) %>% 
  hc_plotOptions(
    bar = list(
      borderWidth = 0,
      borderRadius = 5
    )
  ) -> hc_2019_3_1_dark
  
# estatico computo
df %>% 
  arrange(desc(computo)) -> df

scale_fill_estatico <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(
      c("#0047AB", "#F26524", "#E10915", "#4CBB17", "#00A18A", "#FFC0CB", "#FF0000", "#0080FF", "#D6D6D6"),
      df$partido
    ), 
    ...
  )
}

options(scipen = 999)

df %>% 
  arrange(desc(computo)) %>% 
  mutate(
    partido = as.factor(partido),
    prop = (prop.table(computo)*100) %>% round(., 2),
    num = 1:nrow(.), 
    etiqueta = paste0(computo, "\n", prop, "%")
  ) %>%  
  ggplot(aes(fct_reorder(partido, num, .desc = T), computo, fill = partido)) +
  geom_chicklet(color = NA) +
  hrbrthemes::theme_ipsum_rc(
    base_family = "Raleway", 
    grid = "X", 
    base_size = 10
  ) +
  geom_text(
    aes(
      label = etiqueta, 
      family = "Raleway"
    ), 
    hjust = 1,
    size = 2.5
  ) +
  labs(
    title = "Gráfico 2. Resultados oficiales parciales del OEP",
    x = "partido o agrupación ciudadana",
    y = "cantidad y porcentaje de votos",
    caption = "Del auge a la descomposición de un proceso de cambio\nBolivia 2013-2021"
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6)) +
  coord_flip() +
  theme(
    plot.caption = element_text(family = "Raleway", size = 5),
    plot.title = element_text(size = 11),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x =  element_text(size = 6),
    axis.text.y =  element_text(size = 6)
  ) +
  theme(
    legend.position = "none"
  ) +
  scale_fill_estatico() -> plot_ano_2019_3_1


# tablas de cómputo departamental
df <- rio::import("data/ano_2019_3/computo_deptal.csv")
  

# Tabla clara
df %>% 
  mutate_if(is.numeric, ~./100) %>% 
  reactable(
    columns = list(
      `MAS-IPSP` =  colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      CC =  colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      OTROS = colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      MV = colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      N = colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Raleway"
      )
    )
  ) -> tabla_2019_3_1


# dark mode
df %>% 
  mutate_if(is.numeric, ~./100) %>% 
  reactable(
    columns = list(
      `MAS-IPSP` =  colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      CC =  colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      OTROS = colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      MV = colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      N = colDef(
        format = colFormat(percent = TRUE, digits = 1),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Raleway",
        backgroundColor = "#15202C",
        color = "white"
      )
    )
  ) -> tabla_2019_3_1_dark




  
  




