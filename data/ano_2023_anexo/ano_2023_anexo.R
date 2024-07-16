library(tidyverse)
library(reactable)

t1 <- read_csv("data/ano_2023_anexo/ano_2023_anexo_1.csv")
t2 <- read_csv("data/ano_2023_anexo/ano_2023_anexo_2.csv")

# tabla light t1
t1 %>% 
  reactable(
    columns = list(
      Regiones =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Precio promedio de los mercados autorizados | (USD/Kg)` =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Producción estimada de coca (tm) | Límite inferior` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Valor total de la hoja de coca (MM USD) | Límite inferior` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Valor total de la hoja de coca (MM USD) | Límite superior` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Roboto"
      )
    )
  ) -> tabla_2023_anexo_1


# dark mode
t1 %>% 
  reactable(
    columns = list(
      Regiones =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Precio promedio de los mercados autorizados | (USD/Kg)` =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Producción estimada de coca (tm) | Límite inferior` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Valor total de la hoja de coca (MM USD) | Límite inferior` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `Valor total de la hoja de coca (MM USD) | Límite superior` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Roboto",
        backgroundColor = "#15202C",
        color = "white"
      )
    )
  ) -> tabla_2023_anexo_1_dark


# tabla 2
t2 %>% 
  reactable(
    columns = list(
      Pais =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1994` =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1995` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1996` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1997` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1998` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1999` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2000` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2001` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2002` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2003` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `% Var 2002-2003` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Roboto"
      )
    )
  ) -> tabla_2023_anexo_2
  

t2 %>% 
  reactable(
    columns = list(
      Pais =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1994` =  colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1995` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1996` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1997` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1998` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `1999` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2000` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2001` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2002` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `2003` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      ),
      `% Var 2002-2003` = colDef(
        format = colFormat(separators = TRUE),
        style = list(
          fontFamily = "Roboto Mono Light"
        )
      )
    ),
    theme = reactableTheme(
      style = list(
        fontFamily = "Roboto",
        backgroundColor = "#15202C",
        color = "white"
      )
    )
  ) -> tabla_2023_anexo_2_dark
  
  
