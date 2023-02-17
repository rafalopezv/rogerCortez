library(tidyverse)
library(reactable)

a <- rio::import("data/ano_2017_3/ano_2017_3.csv")

a %>% 
  mutate(
    porcentaje_2001 = porcentaje_2001/100,
    porcentaje_2017 = porcentaje_2017/100
  ) %>% 
  reactable(
    columns = list(
      porcentaje_2001 =  colDef(
        format = colFormat(percent = TRUE, digits = 2),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Porcentaje [%]"
      ),
      porcentaje_2017 =  colDef(
        format = colFormat(percent = TRUE, digits = 2),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Porcentaje [%]"
      ),
      total_2001 = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Total"
      ),
      total_2017 = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Total"
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Raleway"
      )
    ),
    columnGroups = list(
      colGroup(name = "2011", columns = c("total_2001", "porcentaje_2001")),
      colGroup(name = "2017", columns = c("total_2017", "porcentaje_2017"))
    )
  ) -> tabla_2017_3


# dark mode
a %>% 
  mutate(
    porcentaje_2001 = porcentaje_2001/100,
    porcentaje_2017 = porcentaje_2017/100
  ) %>% 
  reactable(
    columns = list(
      porcentaje_2001 =  colDef(
        format = colFormat(percent = TRUE, digits = 2),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Porcentaje [%]"
      ),
      porcentaje_2017 =  colDef(
        format = colFormat(percent = TRUE, digits = 2),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Porcentaje [%]"
      ),
      total_2001 = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Total"
      ),
      total_2017 = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        ),
        name = "Total"
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Raleway",
        backgroundColor = "#15202C",
        color = "white"
      )
    ),
    columnGroups = list(
      colGroup(name = "2011", columns = c("total_2001", "porcentaje_2001")),
      colGroup(name = "2017", columns = c("total_2017", "porcentaje_2017"))
    )
  ) -> tabla_2017_3_dark


