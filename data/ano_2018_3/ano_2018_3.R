library(tidyverse)
library(highcharter)
library(ggchicklet)
library(reactable)

df <- rio::import("data/ano_2018_3/ano_2018_3.csv", skip = 1) %>% 
  rename(Departamento = depto)
colnames(df) <- colnames(df) %>% toupper()

# tabla light
df %>% 
  reactable(
    columns = list(
      `PAN-BOL` =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      MAS =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      CC = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      PDC = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      UCS = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      MNR = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `MTS(*)` = colDef(
        format = colFormat(separators = TRUE),
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
  ) -> tabla_2018_3


# dark mode
df %>% 
  reactable(
    columns = list(
      `PAN-BOL` =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      MAS =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      CC = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      PDC = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      UCS = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      MNR = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `MTS(*)` = colDef(
        format = colFormat(separators = TRUE),
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
  ) -> tabla_2018_3_dark

# graficos
df %>% 
  gather(partido, valor, -DEPARTAMENTO) %>% 
  group_by(partido) %>% 
  summarise(valor = sum(valor)) %>% 
  mutate(
    valor = case_when(
      partido == "CC" ~ 88122,
      partido == "MAS" ~ 991092,
      partido == "MNR" ~ 58377,
      partido == "MTS(*)" ~ 95391,
      partido == "PAN-BOL" ~ 92210,
      partido == "PDC" ~ 28717,
      partido == "UCS" ~ 38421
    )
  ) %>% 
  ungroup() %>% 
  arrange(valor) %>% 
  mutate(num = 1:nrow(.)) -> temp

# interactivo claro
cols <- c("#0047AB", "#00A18A", "#FF0000", "#F26524", "#FFC0CB", "#0080FF", "#E10915")

temp %>% 
  arrange(desc(num)) %>% 
  hchart("column", hcaes(partido, valor, color = cols), name = "Militantes", color = "#15202c") %>% 
  hc_title(text = "Gráfico 1. Militantes acreditados oficialmente por partido o coalición") %>% 
  hc_tooltip(
    borderWidth = 1/20,
    style = list(
      fontSize = "16"
    )
  ) %>% 
  hc_yAxis(
    title = list(text = "número de militantes"),
    max = 1000000
  ) %>% 
  hc_xAxis(title = list(text = "partido o agrupación ciudadana")) %>% 
  hc_chart(style = list(fontFamily = "Raleway")) %>% 
  hc_plotOptions(
    column = list(
      borderRadius = 7, 
      borderWidth = 0
    )
  ) -> hc_2018_3

# interactivo oscuro
cols <- c("#0047AB", "#00A18A", "#FF0000", "#F26524", "#FFC0CB", "#0080FF", "#E10915")

temp %>% 
  arrange(desc(valor)) %>% 
  hchart("column", hcaes(partido, valor, color = cols), name = "Militantes") %>% 
  hc_title(
    text = "Gráfico 1. Militantes acreditados oficialmente por partido o coalición",
    style = list(
      color = "#f7f9f9"
    )
  ) %>% 
  hc_tooltip(
    borderWidth = 1/20, 
    backgroundColor = highcharter::hex_to_rgba(x = "#15202c", alpha = .9),
    style = list(
      color = "#F5F5F5",
      fontSize = 16
    )
  ) %>% 
  hc_yAxis(
    max = 1000000,
    gridLineColor = '#81868e',
    gridLineWidth = .1,
    title = list(
      text = "número de militantes",
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
    column = list(
      borderWidth = 0,
      borderRadius = 5
    )
  ) -> hc_2018_3_dark

  
# estático
scale_fill_estatico <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(
      c("#0047AB", "#00A18A", "#FF0000", "#F26524", "#FFC0CB", "#0080FF", "#E10915"),
      c("MAS", "MTS(*)", "PAN-BOL", "CC", "MNR", "UCS", "PDC")
    ), 
    ...
  )
}



temp %>% 
  arrange(desc(num)) %>% 
  mutate(partido = as.factor(partido)) %>% 
  ggplot(aes(fct_reorder(partido, num, .desc = T), valor, fill = partido)) +
  geom_chicklet(color = NA) +
  hrbrthemes::theme_ipsum_rc(
    base_family = "Raleway", 
    grid = "Y", 
    base_size = 10
  ) +
  geom_text(
    aes(
      label = valor, 
      family = "Raleway"
    ), 
    vjust = -.8,
    size = 2.5
  ) +
  labs(
    title = "Gráfico 1. Militantes acreditados oficialmente por partido o coalición",
    x = "partido o agrupación ciudadana",
    y = "número de militantes",
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
  scale_fill_estatico() -> plot_ano_2018_3



