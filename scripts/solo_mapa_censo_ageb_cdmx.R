
##############
#Configuración
#rm(list = ls())


library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,
       gmodels,foreign,expss,fishualize,leaflet,plotly,wesanderson,data.table)

##############
#Paqueterías para mapear
library(sf)
p_load(rstudioapi, lintr, raster, viridis, cowplot, rmarkdown, leaflet)

# Restore the object
#datitos_factor_mza<- readRDS(file = "/Users/macsernst/Documents/Data Science/Repos/Gatitos/2021A/censo_2020/datitos_factor_mza.rds")
datitos_factor<- readRDS(file = "/Users/macsernst/Documents/Data Science/Repos/Gatitos/2021A/censo_2020/datitos_factor.rds")

pal <- colorFactor(palette = "inferno",
                   domain = datitos_factor$PC1_q,
                   rev = T,
                   na.color = "#F5F5F3")


label <- paste0("AGEB: ", datitos_factor$CVE_AGEB)
popup <- paste0("<b>Alcaldía: </b>", datitos_factor$NOM_MUN, "<br>",
                "<b>AGEB: </b>", datitos_factor$CVE_AGEB, "<br>",
                "<b>Valor índice: </b>", prettyNum(datitos_factor$PC1, digits=4), "<br>")

leaflet(data = datitos_factor, options = leafletOptions(minZoom = 5)) %>%
  setMaxBounds(lng1 = -99.35, lat1 = 19.10, lng2 = -98.94, lat2 = 19.6) %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner") %>% 
  #Estrato social
  addPolygons(
    label = label,
    popup = popup,
    weight = .6,
    color = "white",
    fillColor = ~pal(PC1_q),
    fillOpacity = 0.7,
    #color = "#939486",
    opacity = 0.7,
    group = "Estrato Social",
    highlightOptions = 
      highlightOptions(
        color = "black", weight = 4, bringToFront = TRUE),
  ) %>% 
  addLegend("bottomright", pal = pal, values = ~PC1_q,
            title = "Puntaje en índice",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )

