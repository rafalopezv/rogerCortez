# sobre: graficos resultados primarias
library(tidyverse)
library(highcharter)
library(ggchicklet)


df <- read_csv("data/ano_2019_1/primarias_limpio.csv")

df %>% 
  group_by(sigla) %>% 
  summarise(
    Militantes = sum(inscritos),
    `Votos válidos` = sum(validos)
  ) %>% 
  mutate(
    `Votos nulos, blancos, ausentes` = Militantes - `Votos válidos`,
    percent_validos = (`Votos válidos`/Militantes *100) %>% round(., 2),
    sigla = case_when(
      sigla == "MAS_IPSP" ~ "MAS-IPSP",
      T ~ sigla
    ),
    llave = paste0(sigla, " ", percent_validos, "%")
  ) %>% 
  arrange(desc(Militantes)) -> aa
  
# gráfico claro
highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = aa$llave) %>%
  hc_add_series(name = "Militantes", data = aa$Militantes, color = "#264653") %>%
  hc_add_series(name="`Votos nulos, blancos, ausentes",data = aa$`Votos nulos, blancos, ausentes`, color = "#2a9d8f") %>% 
  hc_add_series(name="Votos válidos",data = aa$`Votos válidos`, color = "#e76f51") %>% 
  hc_tooltip(
    shared = T, 
    table = T,  
    borderWidth = 1/20,
    style = list(
      fontSize = 16
    )
  ) %>% 
  hc_chart(style = list(fontFamily = "Raleway")) %>% 
  hc_title(text = "Gráfico 1. Aprobación de candidaturas") %>%
  hc_yAxis(
    title = list(text = "resultados primarias"),         
    max =  1000000
  ) %>% 
  hc_xAxis(title = list(text = "partido o agrupación ciudadana")) %>% 
  hc_credits(
    enabled = TRUE,
    text = "TSE",
    href = "https://atlaselectoral.oep.org.bo/#/subproceso/103/1/2"
  ) %>% 
  hc_plotOptions(
    column = list(
      borderRadius = 5, 
      borderWidth = 0
    )
  ) -> hc_2019_1

# oscuro
highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = aa$llave) %>%
  hc_add_series(name = "Militantes", data = aa$Militantes, color = "#8b1e3f") %>%
  hc_add_series(name="`Votos nulos, blancos, ausentes",data = aa$`Votos nulos, blancos, ausentes`, color = "#89bd9e") %>% 
  hc_add_series(name="Votos válidos",data = aa$`Votos válidos`, color = "#8F8389") %>% 
  hc_tooltip(
    shared = T, 
    table = T,  
    borderWidth = 1/20,
    backgroundColor = highcharter::hex_to_rgba(x = "#15202c", alpha = .9),
    style = list(
      color = "#F5F5F5",
      fontSize = 16
    )
  ) %>% 
  hc_chart(
    style = list(
      fontFamily = "Raleway",
      backgroundColor = "#15202c"
  )
) %>% 
  hc_title(
    text = "Gráfico 1. Aprobación de candidaturas", 
    style = list(
      color = "#DCE5E5"
    )
  ) %>%
  hc_yAxis(
    gridLineColor = '#81868e',
    gridLineWidth = .1,
    style = list(color = "#f7f9f9"),
    title = list(
      text = "resultados primarias", 
      style = list(color = "#f7f9f9")
    ),         
    max =  1000000,
    labels = list(
      style = list(
        color = "#DCE5E5"
      )
    )
  ) %>% 
  hc_xAxis(
    title = list(
      text = "partido o agrupación ciudadana", 
      style = list(color = "#f7f9f9")
    ),
    labels = list(
      style = list(
        color = "#DCE5E5"
      )
    )
  ) %>% 
  hc_credits(
    enabled = TRUE,
    text = "TSE",
    href = "https://atlaselectoral.oep.org.bo/#/subproceso/103/1/2"
  ) %>% 
  hc_plotOptions(
    column = list(
      borderRadius = 5, 
      borderWidth = 0
    )
  ) %>% 
  hc_legend(
    itemStyle = list(
      color = "#DCE5E5"
    )
  ) -> hc_2019_1_dark


# estatico
aa %>% 
  mutate(
    orden = 1:nrow(.),
    llave = paste0(sigla, "\n", percent_validos, "%")
  ) %>% 
  select(6:7, 2:4) %>% 
  gather(cat, valor, -llave, -orden) %>% 
  ggplot(aes(fct_reorder(llave, orden), valor, fill = cat)) +
  geom_chicklet(
    position = "dodge2", 
    color =- NA
  ) +
  hrbrthemes::theme_ipsum_rc(
    base_family = "Raleway", 
    grid = "Y", 
    base_size = 10
  ) +
  labs(
    title = "Gráfico 1. Aprobación de candidaturas",
    x = "partido o agrupación ciudadana",
    y = "resultados primarias",
    caption = "Del auge a la descomposición de un proceso de cambio\nBolivia 2013-2021"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(family = "Raleway", size = 5),
    plot.title = element_text(size = 11),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x =  element_text(size = 6),
    axis.text.y =  element_text(size = 6)
  ) +
  scale_fill_manual(values = c("#264653", "#2a9d8f", "#e76f51")) -> plot_ano_2019_1
  





  