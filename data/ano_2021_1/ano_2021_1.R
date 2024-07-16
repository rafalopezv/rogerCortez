# sobre: cálculo y graficacion de curvas epedimiologicas
library(tidyverse)
library(magrittr)

#-------------------------
# limpieza datos
#-------------------------

# serie de tiempo
url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

# descarga y limpieza
df <- read_csv(url) %>% 
  mutate(base = "confirmados")
rm(url)

df %>% 
  rename(pais_region = `Country/Region`) %>% 
  dplyr::select(-matches("Lat|Long")) %>% 
  filter(pais_region %in% c("Israel", "Chile", "Germany", "US")) %>% 
  mutate(
    pais_region = case_when(
      pais_region == "Germany" ~ "Alemania",
      pais_region == "US" ~ "Estados Unidos",
      T ~ pais_region
    ),
    orden = case_when(
      pais_region == "Israel" ~ 1,
      pais_region == "Estados Unidos" ~ 2,
      pais_region == "Chile" ~ 3,
      pais_region == "Alemania" ~ 4
    )
  ) %>% 
  dplyr::select(-`Province/State`) %>% 
  gather(fecha, casos_acumulados, -pais_region, -base, -orden) -> df

# ajuste de fecha
df$fecha %<>% as.Date(., format = "%m/%d/%y")


# estandarización desde pacientes 0 e individualización de países en formato lista
df %>% 
  filter(
    fecha <= "2021-04-30",
    fecha > "2021-02-28"
  ) %>% 
  group_split(pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., dias = 1:nrow(.),
                 total_semanas = nrow(.)/7)) %>% 
  map(., ~mutate_if(., is.numeric, round, 0)) %>% 
  bind_rows() %>% 
  arrange(base, pais_region, fecha) -> df

# añadir variable de número de semanas: extensión de un año 
tibble(
  semana = rep((1:200), 7)
) %>% 
  arrange(semana) %>% 
  mutate(dias = 1:nrow(.)) -> temp

df %<>% merge(., temp, all.x = T)
rm(temp)

# dividir entre países/semanas
df %>% 
  group_split(base, pais_region) %>% 
  map(., ~arrange(., fecha)) %>% 
  map(., ~mutate(., incidencia = lag(casos_acumulados),
                 incidencia = casos_acumulados - incidencia,
                 incidencia = abs(incidencia))) %>%
  bind_rows() %>% 
  filter(!is.na(incidencia)) -> df_graf 

df_graf %>% 
  select(fecha) %>% 
  unique %>% 
  arrange(fecha) -> juntar

sem <- rep(1:122, 7) %>% sort

sem[1: nrow(juntar)]
juntar %>% 
  mutate(semana_pandemia = sem[1: nrow(juntar)]) %>% 
  left_join(df_graf, .) -> df_graf


# grafica clara
df_graf %>% 
  ungroup() %>% 
  group_by(pais_region, semana, orden) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  ggplot(aes(semana, incidencia)) +
  geom_area(fill = "#e01f52", alpha = .9) +
  geom_line(size = .1, color = "darkgrey") +
  theme_void(base_family = "Raleway") +
  facet_wrap(~fct_reorder(pais_region, orden), ncol = 4, scales = "free") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "#F8F8F8", colour = NA),
    strip.background = element_rect(fill = "#F8F8F8", colour = NA),
    strip.text = element_text(family = "Raleway Bold")
  ) -> plot_ano_2021_1


# grafica clara
df_graf %>% 
  ungroup() %>% 
  group_by(pais_region, semana, orden) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  ggplot(aes(semana, incidencia)) +
  geom_area(fill = "#e01f52", alpha = .9) +
  geom_line(size = .1, color = "darkgrey") +
  theme_void(base_family = "Raleway") +
  facet_wrap(~fct_reorder(pais_region, orden), ncol = 4, scales = "free") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background = element_rect(fill = "#FFFFFF", colour = NA),
    strip.background = element_rect(fill = "#FFFFFF", colour = NA),
    strip.text = element_text(family = "Raleway Bold")
  ) -> plot_ano_2021_1


# grafica oscura
df_graf %>% 
  ungroup() %>% 
  group_by(pais_region, semana, orden) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  ggplot(aes(semana, incidencia)) +
  geom_area(fill = "#e01f52", alpha = .9) +
  geom_line(size = .1, color = "darkgrey") +
  theme_void(base_family = "Raleway") +
  facet_wrap(~fct_reorder(pais_region, orden), ncol = 4, scales = "free") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "#15202c", colour = NA),
    panel.background = element_rect(fill = "#15202c", colour = NA),
    strip.background = element_rect(fill = "#15202c", colour = "#1f2733"),
    strip.text = element_text(family = "Raleway Bold", color = "#FFFFFF")
  ) -> plot_ano_2021_1_dark









