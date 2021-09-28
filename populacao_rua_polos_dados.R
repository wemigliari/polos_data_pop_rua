library(readxl)
library(plotly)
library(sf) ## Read shapefiles
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(tmap)
library(dplyr)
library(RColorBrewer)


### Regi??es Totais de Moradores

prpolos <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                      sheet="Etnia")

names(prpolos)[1] <- "nome"

regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                  options = "ENCODING=windows-1252")

data_polos1 <- merge(prpolos, regioes, by = "nome")


###

shp_joined <- inner_join(regioes, data_polos1, by = "nome")
class(shp_joined) # Dataframe. It does not work yet!

shp_joined <- st_as_sf(shp_joined) 
class(shp_joined) # It is now converted to "sf" and "data.frame". It works!

data_polos1 <- shp_joined %>%
  arrange(Total) %>%
  mutate(name=factor(nome, unique(nome)))


####

mybins1 <- c(5414, 11203, 20334, 22900, 100259)
mypalette1 <- colorBin(palette="YlOrBr", na.color="transparent", domain=data_polos1, bins=mybins1)

mytext1 <- paste(
  "Regi??o: ", data_polos1$nome, "<br/>",
  "Total: ", data_polos1$Total, "<br/>",
  "Preta: ", data_polos1$Preta, "<br/>",
  "Parda: ", data_polos1$Parda, "<br/>",
  "Ind??gena: ", data_polos1$Ind??gena, "<br/>",
  "Branca: ", data_polos1$Branca, "<br/>",
  "Sem Resposta: ", data_polos1$`Sem Resposta`, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#####

leaflet(data_polos1) %>% 
  addPolygons( 
    fillColor = ~mypalette1(Total), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext1,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend(pal=mypalette1, values=~data_polos1$Total, opacity=1, title = "Regi??o, Totais de Moradores em Situa????o de Rua", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.3)


### Regi??es Percentuais de Moradores

prpolos_p <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx")

names(prpolos_p)[1] <- "nome"

regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")

data_polos_p <- merge(prpolos_p, regioes, by = "nome")


###

shp_joined <- inner_join(regioes, data_polos_p, by = "nome")
class(shp_joined) # Dataframe. It does not work yet!

shp_joined <- st_as_sf(shp_joined) 
class(shp_joined) # It is now converted to "sf" and "data.frame". It works!

data_polos_p <- shp_joined %>%
  arrange(`Total (%)`)%>%
  mutate(name=factor(nome, unique(nome)))


#### Regioes Percentuais Moradores Pardos

mybins_p <- c(0,35, 50, 75, 85)
mypalette_p <- colorBin(palette="YlOrBr", na.color="transparent", domain=data_polos_p$parda, bins=mybins_p)

mytext1 <- paste(
  "Regi??o: ", data_polos_p$nome, "<br/>",
  "Total: ", data_polos_p$`Total (%)`, "<br/>",
  "Preta: ", data_polos_p$preta, "<br/>",
  "Parda: ", data_polos_p$parda, "<br/>",
  "Ind??gena: ", data_polos_p$ind??gena, "<br/>",
  "Branca: ", data_polos_p$branca, "<br/>",
  "Sem Resposta: ", data_polos_p$`sem resposta`, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#####

leaflet(data_polos_p) %>% 
  addPolygons( 
    fillColor = ~mypalette_p(parda), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext1,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend(pal=mypalette_p, values=~data_polos_p$`Total (%)`, opacity=1, title = "Regi??o, Percentual de Moradores Pardos em Situa????o de Rua", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.3)

#### Regioes Percentuais Moradores Negros

mybins_pre <- c(0, 10, 15, 20)
mypalette_p <- colorBin(palette="YlOrBr", na.color="transparent", domain=data_polos_p$parda, bins=mybins_pre)

mytext1 <- paste(
  "Regi??o: ", data_polos_p$nome, "<br/>",
  "Total: ", data_polos_p$`Total (%)`, "<br/>",
  "Preta: ", data_polos_p$preta, "<br/>",
  "Parda: ", data_polos_p$parda, "<br/>",
  "Ind??gena: ", data_polos_p$ind??gena, "<br/>",
  "Branca: ", data_polos_p$branca, "<br/>",
  "Sem Resposta: ", data_polos_p$`sem resposta`, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#####

leaflet(data_polos_p) %>% 
  addPolygons( 
    fillColor = ~mypalette_p(preta), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext1,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend(pal=mypalette_p, values=~data_polos_p$`Total (%)`, opacity=1, title = "Regi??o, Percentual de Moradores Negros em Situa????o de Rua", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.3)

#### Regioes Percentual Moradores Negros & Pardos

prpolos_p_n <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx")

names(prpolos_p_n)[1] <- "nome"

regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")

data_polos_p_n <- merge(prpolos_p_n, regioes, by = "nome")


###

shp_joined <- inner_join(regioes, data_polos_p_n, by = "nome")
class(shp_joined) # Dataframe. It does not work yet!

shp_joined <- st_as_sf(shp_joined) 
class(shp_joined) # It is now converted to "sf" and "data.frame". It works!

data_polos_p_n <- shp_joined %>%
  arrange(`Pretos & Pardos (%)`)%>%
  mutate(name=factor(nome, unique(nome)))

mybins_pre <- c(42, 43, 67, 88, 89, 90)
mypalette_p <- colorBin(palette="YlOrBr", na.color="transparent", domain=data_polos_p$parda, bins=mybins_pre)

mytext1 <- paste(
  "Regi??o: ", data_polos_p_n$nome, "<br/>",
  "Total: ", data_polos_p_n$`Total (%)`, "<br/>",
  "Preta: ", data_polos_p_n$preta, "<br/>",
  "Parda: ", data_polos_p_n$parda, "<br/>",
  "Ind??gena: ", data_polos_p_n$ind??gena, "<br/>",
  "Branca: ", data_polos_p_n$branca, "<br/>",
  "Pretos & Pardos: ", data_polos_p_n$`Pretos & Pardos (%)`, "<br/>",
  "Sem Resposta: ", data_polos_p_n$`sem resposta`, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#####

leaflet(data_polos_p_n) %>% 
  addPolygons( 
    fillColor = ~mypalette_p(`Pretos & Pardos (%)`), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext1,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend(pal=mypalette_p, values=~data_polos_p$`Total (%)`, opacity=1, title = "Regi??o, Percentual de Moradores Negros & Pardos em Situa????o de Rua", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.3)


### Capitais Totais Shape 

prpolos2 <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx")

names(prpolos2)[1] <- "nome"

capitais <- read_sf("/Users/wemigliari/Documents/R/data/BRA_adm2.shp", 
                  options = "ENCODING=windows-1252")

populacao <- as.numeric(capitais$populacao)

capitais <- cbind(capitais, populacao)


capitais <- capitais%>%
  filter(populacao.1>320000)

data_polos2 <- merge(prpolos2, capitais, by = "nome")

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BRA_adm1.shp", 
                  options = "ENCODING=windows-1252")
brasil <- st_as_sf(brasil)



###

shp_joined2 <- inner_join(capitais, data_polos2, by = "nome")
class(shp_joined) # Dataframe. It does not work yet!

shp_joined2 <- st_as_sf(shp_joined2) 
class(shp_joined2) # It is now converted to "sf" and "data.frame". It works!

data_polos2 <- shp_joined2 %>%
  arrange(Total) %>%
  mutate(name=factor(nome, unique(nome)))


####

mybins2 <- c(0,10, 20, 30, 40, 50, 100, 200, 500, 1000, 2000, 5000, 8700, 10000, 25000, 50000, 70000)
mypalette2 <- colorBin(palette="YlOrBr", na.color="transparent", domain=test$Soma, bins=mybins2)

mytext2 <- paste(
  "Capital: ", data_polos2$nome, "<br/>",
  "Total: ", data_polos2$Total, "<br/>",
  "Preta: ", data_polos2$Preta, "<br/>",
  "Parda: ", data_polos2$Parda, "<br/>",
  "Ind??gena: ", data_polos2$Ind??gena, "<br/>",
  "Branca: ", data_polos2$Branca, "<br/>",
  "Indefinido: ", data_polos2$`Sem Resposta`, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#####

leaflet(data_polos2) %>% 
  addPolygons(
    fillColor = ~mypalette2(Total), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext2,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$Stamen.TonerBackground) %>%
  addLegend(pal=mypalette2, values=~data_polos2$Total, opacity=1, title = "Moradores em Situa????o de Rua, Capitais", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.7)

##### Capitais Totais Faixa Et??ria

library(leaflet.minicharts)

escolaridade1 <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                       sheet = "Faixa Et??ria")

escolaridade1 <- escolaridade1[-c(1:6), ]
seq <- seq(1,53, 2)
escolaridade1 <- escolaridade1[-c(seq), ]

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BRA_adm1.shp", 
                   options = "ENCODING=windows-1252")
brasil <- st_as_sf(brasil)

colors = c("gray", "yellow", "gold", "coral", "black", "orange") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Capitais, Faixa Et??ria, Total dos Moradores em Situa????o de Rua")
)  


leaflet(brasil) %>%
  addPolygons( 
    fillColor = "red", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = escolaridade1$Longitude, 
                lat = escolaridade1$Latitude, 
                type = "pie", 
                chartdata = escolaridade1[ , c("0-6 (pr??-escolar)",	"7-17 (fundamental e m??dio)",	"18-59", "A partir de 60", "Sem Resposta", "Total")], 
                colorPalette = colors, 
                width = 60, height=80,
                transitionTime = 0,
                showLabels = FALSE,
                layerId = escolaridade1$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title, position = "topleft", className="map-title")

##### Capital Totais

genero <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                            sheet = "Etnia")
genero <- genero[-c(1:6),]

n <- seq(1,54, 2)

genero <- genero[-c(n),]


brasil <- read_sf("/Users/wemigliari/Documents/R/data/BRA_adm1.shp", 
                  options = "ENCODING=windows-1252")
brasil <- st_as_sf(brasil)

colors = c("white", "black", "yellow", "orange", "coral", "lightgray", "red") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title2 <- tags$div(
  tag.map.title, HTML("Capitais, Total de Pessoas em Situa????o de Rua")
)  

leaflet(brasil) %>%
  addPolygons( 
    fillColor = "red", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = genero$Longitude, 
                lat = genero$Latitude, 
                type = "pie", 
                chartdata = genero[ ,c("Branca", "Preta", "Amarela", "Parda",	"Ind??gena", "Sem Resposta", "Total")], 
                colorPalette = colors, 
                width = 60, height=80,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = genero$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title2, position = "topleft", className="map-title")


### Capital Totais Feminino

genero_f <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                     sheet = "Etnia-2")
genero_f <- genero_f[-c(1:6),]

n <- seq(1,54, 2)

genero_f <- genero_f[-c(n),]


brasil <- read_sf("/Users/wemigliari/Documents/R/data/BRA_adm1.shp", 
                  options = "ENCODING=windows-1252")
brasil <- st_as_sf(brasil)

colors_f = c("white", "black", "yellow", "orange", "coral", "lightgray", "purple") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title3 <- tags$div(
  tag.map.title, HTML("Capitais, Total de Moradoras em Situa????o de Rua")
)  

leaflet(brasil) %>%
  addPolygons( 
    fillColor = "red", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = genero_f$Longitude, 
                lat = genero_f$Latitude, 
                type = "pie", 
                chartdata = genero_f[ ,c("Branca", "Preta", "Amarela", "Parda",	"Ind??gena", "Sem Resposta", "Soma")], 
                colorPalette = colors_f, 
                width = 100 * sqrt(genero_f$Soma) / sqrt(max(genero_f$Soma)),
                transitionTime = 0,
                showLabels = FALSE,
                layerId = genero_f$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title3, position = "topleft", className="map-title")


### Capital Totais Masculino

genero_m <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                       sheet = "Etnia-3")
genero_m <- genero_m[-c(1:6),]

n <- seq(1,54, 2)

genero_m <- genero_m[-c(n),]


brasil <- read_sf("/Users/wemigliari/Documents/R/data/BRA_adm1.shp", 
                  options = "ENCODING=windows-1252")
brasil <- st_as_sf(brasil)

colors_m = c("white", "black", "yellow", "orange", "coral", "lightgray", "green") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title4 <- tags$div(
  tag.map.title, HTML("Capitais, Total de Moradores em Situa????o de Rua")
)  

leaflet(brasil) %>%
  addPolygons( 
    fillColor = "red", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = genero_m$Longitude, 
                lat = genero_m$Latitude, 
                type = "pie", 
                chartdata = genero_m[ ,c("Branca", "Preta", "Amarela", "Parda",	"Ind??gena", "Sem Resposta", "Soma")], 
                colorPalette = colors_m, 
                width = 100 * sqrt(genero_m$Soma) / sqrt(max(genero_m$Soma)),
                transitionTime = 0,
                showLabels = FALSE,
                layerId = genero_m$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title4, position = "topleft", className="map-title")


### Percentual Capital Feminino

percentuais_c_f <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                            sheet = "Etnia-2")
percentuais_c_f <- percentuais_c_f[-c(1:6),]

n <- seq(1,54, 2)

percentuais_c_f <- percentuais_c_f[-c(n),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_p = c("white", "black", "yellow", "orange", "coral", "gray", "purple") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Capitais, Percentual de Moradoras em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_c_f$Longitude, 
                lat = percentuais_c_f$Latitude, 
                type = "pie", 
                chartdata = percentuais_c_f[ ,c("branca", "preta", "amarela", "parda", "ind??gena", "sem resposta", "Total (%)")], 
                colorPalette = colors_p, 
                width = 60, height=80,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = percentuais_c_f$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Percentual Capital Masculino

percentuais_c_m <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                              sheet = "Etnia-3")
percentuais_c_m <- percentuais_c_m[-c(1:6),]

n <- seq(1,54, 2)

percentuais_c_m <- percentuais_c_m[-c(n),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_p = c("white", "black", "yellow", "orange", "coral", "gray", "green") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Capitais, Percentual de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_c_m$Longitude, 
                lat = percentuais_c_m$Latitude, 
                type = "pie", 
                chartdata = percentuais_c_m[ ,c("branca", "preta", "amarela", "parda", "ind??gena", "sem resposta", "Total (%)")], 
                colorPalette = colors_p, 
                width = 60, height=80,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = percentuais_c_m$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")

### Percentual Capital Faixa Et??ria

percentuais_c_fe <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                              sheet = "Faixa Et??ria")
percentuais_c_fe <- percentuais_c_fe[-c(1:6),]

n <- seq(1,54, 2)

percentuais_c_fe <- percentuais_c_fe[-c(n),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_fe = c("gray", "yellow", "gold", "coral", "orange") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Capitais, Faixa Et??ria, Percentual de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_c_fe$Longitude, 
                lat = percentuais_c_fe$Latitude, 
                type = "pie", 
                chartdata = percentuais_c_fe[ ,c("0-6 (Pr??-Escolar)",	"7-17 (Fundamental e M??dio)",	"De 18 a 59", "A Partir de 60", "Total (%)")], 
                colorPalette = colors_fe, 
                width = 60, height=80,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = percentuais_c_fe$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")



### Percentual Capital Cor

percentuais_c_cor <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                               sheet = "Etnia")
percentuais_c_cor <- percentuais_c_cor[-c(1:6),]

n <- seq(1,54, 2)

percentuais_c_cor <- percentuais_c_cor[-c(n),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_c_cor = c("white", "black", "yellow", "orange", "coral", "gray", "red") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Capitais, Percentual de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_c_cor$Longitude, 
                lat = percentuais_c_cor$Latitude, 
                type = "pie", 
                chartdata = percentuais_c_cor[ ,c("branca", "preta", "amarela", "parda", "ind??gena", "sem resposta", "Total (%)")], 
                colorPalette = colors_c_cor, 
                width = 60, height=80,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = percentuais_c_cor$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")

### Percentuais Regiao Cor

percentuais_r <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                            sheet = "Etnia")
percentuais_r <- percentuais_r[-c(1, 7:60),]

regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_p = c("white", "black", "yellow", "orange", "coral", "gray", "gold") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Percentual de Pessoas em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_r$Longitude, 
                lat = percentuais_r$Latitude, 
                type = "bar", 
                chartdata = percentuais_r[ ,c("branca", "preta", "amarela", "parda", "ind??gena", "sem resposta", "Total (%)")], 
                colorPalette = colors_p, 
                width = 150, height=percentuais_r$`Total (%)`*2,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = percentuais_r$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Total Regiao Cor

total_r_cor <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                                sheet = "Etnia")
total_r_cor <- total_r_cor[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_r_cor = c("white", "black", "yellow", "orange", "coral", "gray", "red") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Total de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = total_r_cor$Longitude, 
                lat = total_r_cor$Latitude, 
                type = "pie", 
                chartdata = total_r_cor[ ,c("Branca", "Preta", "Amarela", "Parda", "Ind??gena", "Sem Resposta", "Total")], 
                colorPalette = colors_r_cor, 
                width = 100, height=150,
                transitionTime = 0,
                showLabels = FALSE,
                layerId = total_r_cor$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Total Regiao Faixa Et??ria

percentuais_re_to <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                                sheet = "Faixa Et??ria")
percentuais_re_to <- percentuais_re_to[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_fe = c("gray", "yellow", "gold", "coral", "orange") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Faixa Et??ria, Total de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_re_to$Longitude, 
                lat = percentuais_re_to$Latitude, 
                type = "pie", 
                chartdata = percentuais_re_to[ ,c("0-6 (pr??-escolar)",	"7-17 (fundamental e m??dio)",	"18-59", "A partir de 60", "Total")], 
                colorPalette = colors_fe, 
                width = 130, height=150,
                transitionTime = 0,
                showLabels = FALSE,
                layerId = percentuais_re_to$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Percentual Regiao Faixa Et??ria

percentuais_re_per <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                                sheet = "Faixa Et??ria")
percentuais_re_per <- percentuais_re_per[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_fe = c("gray", "yellow", "gold", "coral", "orange") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Faixa Et??ria, Percentual de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = percentuais_re_per$Longitude, 
                lat = percentuais_re_per$Latitude, 
                type = "bar", 
                chartdata = percentuais_re_per[ ,c("0-6 (Pr??-Escolar)",	"7-17 (Fundamental e M??dio)",	"De 18 a 59", "A Partir de 60", "Total (%)")], 
                colorPalette = colors_fe, 
                width = 130, height=150,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = percentuais_re_per$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Total Regiao Feminino Cor

total_ref_cor <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                          sheet = "Etnia-2")
total_ref_cor <- total_ref_cor[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_r_cor = c("white", "black", "yellow", "orange", "coral", "gray", "purple") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Total de Moradoras em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = total_ref_cor$Longitude, 
                lat = total_ref_cor$Latitude, 
                type = "bar", 
                chartdata = total_ref_cor[ ,c("Branca", "Preta", "Amarela", "Parda", "Ind??gena", "Sem Resposta", "Total")], 
                colorPalette = colors_r_cor, 
                width = 140, height=180,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = total_ref_cor$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Percentual Regiao Feminino Cor

perc_ref_cor <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                            sheet = "Etnia-2")
perc_ref_cor <- perc_ref_cor[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_r_cor = c("white", "black", "yellow", "orange", "coral", "gray", "purple") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Percentual de Moradoras em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = perc_ref_cor$Longitude, 
                lat = perc_ref_cor$Latitude, 
                type = "bar", 
                chartdata = perc_ref_cor[ ,c("branca", "preta", "amarela", "parda", "ind??gena", "sem resposta", "Total (%)")], 
                colorPalette = colors_r_cor, 
                width = 140, height=180,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = perc_ref_cor$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Total Regiao Masculino Cor

total_rem_cor <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                            sheet = "Etnia-3")
total_rem_cor <- total_rem_cor[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_r_cor = c("white", "black", "yellow", "orange", "coral", "gray", "green") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Total de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = total_rem_cor$Longitude, 
                lat = total_rem_cor$Latitude, 
                type = "bar", 
                chartdata = total_rem_cor[ ,c("Branca", "Preta", "Amarela", "Parda", "Ind??gena", "Sem Resposta", "Total")], 
                colorPalette = colors_r_cor, 
                width = 140, height=180,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = total_rem_cor$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")


### Percentual Regiao Feminino Cor

perc_rem_cor <-  read_xlsx("/Users/wemigliari/Documents/R/tabelas/populacao_rua_polos.xlsx",
                           sheet = "Etnia-3")
perc_rem_cor <- perc_rem_cor[-c(1, 7:60),]


regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                   options = "ENCODING=windows-1252")
regioes <- st_as_sf(regioes)

colors_r_cor = c("white", "black", "yellow", "orange", "coral", "gray", "green") 

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: transparent;
    font-weight: bold;
    font-size: 15px;
  }
"))

title5 <- tags$div(
  tag.map.title, HTML("Regi??es, Percentual de Moradores em Situa????o de Rua")
)  

leaflet(regioes) %>%
  addPolygons( 
    fillColor = "orange", 
    stroke=TRUE, 
    fillOpacity = 0.2, 
    color="black", 
    weight=0.3
  )%>%
  addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMinicharts(lng = perc_rem_cor$Longitude, 
                lat = perc_rem_cor$Latitude, 
                type = "bar", 
                chartdata = perc_rem_cor[ ,c("branca", "preta", "amarela", "parda", "ind??gena", "sem resposta", "Total (%)")], 
                colorPalette = colors_r_cor, 
                width = 140, height=180,
                transitionTime = 0,
                showLabels = TRUE,
                layerId = perc_rem_cor$Regi??o)%>%
  setView(-47.9392, -15.7801, zoom = 4.7)%>%
  addControl(title5, position = "topleft", className="map-title")












