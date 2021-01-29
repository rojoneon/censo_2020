######################################################################
# Mapa de estratificación social de la CDMX, a nivel colonias
#Basdao en metodología y argumentación de acá: https://bit.ly/3qTpz6B
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################




#Configuración----
rm(list = ls())

library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,
       gmodels,foreign,expss,fishualize,leaflet,plotly,wesanderson,data.table)


#Paqueterías para mapear
p_load(sf, ggmap, ggspatial, colorspacem, rstudioapi, lintr, raster, viridis, cowplot, rmarkdown )
theme_set(theme_bw())
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.7, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(.3, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8, hjust = 0,
                                 color = "#939486"),
      plot.title = element_text(size = 12, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 9, hjust = 0.5,
                                   color = "#939486",
                                   margin = margin( b = -0.1,
                                                    #t = -0.1,
                                                    l = 2,
                                                    unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 8,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}


#####Mapa CDMX con terrain----

#register_google(key="")
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x4BA9C5&size=480x360"
# Get the basemap
cdmx <- get_googlemap(
  c(lon=-99.1276627,lat=19.4284706),
  zoom = 10, crop=T, 
  scale = 2, 
  #color="bw", 
  maptype="terrain", # can change to terrain
  style = s)

#ggmap(cdmx)  
gg <- ggmap(cdmx) 

#Preparar los datos
colonias_cdmx <- st_read("~/Documents/Encuestas/Censo 2020/marco_geoestadistico/09_ciudaddemexico/conjunto_de_datos/09colonias_manzanas.geojson")
glimpse(colonias_cdmx)
table(colonias_cdmx$nombre)
summary(colonias_cdmx$nombre)

mun_cdmx <- st_read("~/Documents/Encuestas/Censo 2020/marco_geoestadistico/09_ciudaddemexico/conjunto_de_datos/09mun.geojson")

datos_mza <- read_csv(file = "~/Documents/Encuestas/Censo 2020/AGEB/RESAGEBURB_09CSV20.csv")
glimpse(datos_mza)

datos_manzana <- datos_mza %>% 
  dplyr::rename(CVE_ENT=ENTIDAD,
                CVE_MUN=MUN,
                CVE_LOC=LOC,
                CVE_AGEB=AGEB,
                CVE_MZA=MZA) %>% 
  mutate( CVE_MZA=as.numeric(CVE_MZA)) %>% 
  dplyr::filter(CVE_MZA!=0)
glimpse(datos_manzana)

datitos_mza <- colonias_cdmx %>% 
  mutate( CVE_MZA=as.numeric(CVE_MZA)) %>% 
  left_join(datos_manzana)

glimpse(datitos_mza)
st_geometry(datitos_mza) <- NULL

colonias_cdmx_datos <-
  datitos_mza %>% 
  mutate(
  POBTOT=as.numeric(POBTOT, na.rm = TRUE),
  GRAPROES=as.numeric(GRAPROES, na.rm = TRUE),
  PSINDER=as.numeric(PSINDER, na.rm = TRUE),
  PAFIL_IPRIV=as.numeric(PAFIL_IPRIV, na.rm = TRUE),
  PRO_OCUP_C=as.numeric(PRO_OCUP_C, na.rm = TRUE),
  VPH_INTER=as.numeric(VPH_INTER, na.rm = TRUE),
  TVIVPARHAB=as.numeric(TVIVPARHAB, na.rm = TRUE)
    ) %>% 
    group_by(cve_col) %>% 
    summarise(
      POBTOT = sum(POBTOT, na.rm = TRUE),
      #GRAPROES = weighted.mean(GRAPROES,POBTOT)
      GRAPROES = mean(GRAPROES, na.rm = TRUE),
      PSINDER = sum(PSINDER, na.rm = TRUE),
      PAFIL_IPRIV = sum(PAFIL_IPRIV, na.rm = TRUE),
      PRO_OCUP_C = mean(PRO_OCUP_C, na.rm = TRUE),
      VPH_INTER = sum(VPH_INTER, na.rm = TRUE),
      TVIVPARHAB = sum(TVIVPARHAB, na.rm = TRUE),
      ) %>% 
    ungroup() %>% 
    mutate(
      PSINDER_porcen= PSINDER/POBTOT*100,
      PAFIL_IPRIV_porcen= PAFIL_IPRIV/POBTOT*100,  
      VPH_INTER_porcen= VPH_INTER/TVIVPARHAB*100    
    )

glimpse(colonias_cdmx_datos)


colonias_adip <- st_read("~/Documents/Encuestas/ADIP/Colonias/fixed_colonias_cdmx.geojson")
glimpse(colonias_adip)

dim(colonias_adip)
dim(colonias_cdmx_datos)

datitos_colonias <- colonias_adip %>% 
  left_join(colonias_cdmx_datos)

glimpse(datitos_colonias)







#Análisis de componentes princiaples----

pca_matrix <- datitos_colonias %>% 
  dplyr::select(GRAPROES,PSINDER_porcen,PAFIL_IPRIV_porcen,
                PRO_OCUP_C,VPH_INTER_porcen) 
st_geometry(pca_matrix) <- NULL
glimpse(pca_matrix)

res.pca <- pca_matrix %>% 
  drop_na() %>% 
  prcomp( scale = TRUE)

library(factoextra)
fviz_eig(res.pca)  

factor_cdmx <- predict(res.pca, newdata = pca_matrix)
summary(factor_cdmx)
dim(factor_cdmx)
dim(datitos_colonias)

colonias_factor_previo <- cbind(datitos_colonias,factor_cdmx)
st_geometry(colonias_factor_previo) <- NULL
glimpse(colonias_factor_previo)

colonias_factor <- colonias_adip %>% 
  left_join(colonias_factor_previo)

glimpse(colonias_factor)





#Clases del  índice de estratifiación social ----


#Clase que qiuero
no_classes <- 5
# Extraer cuantiles
q1 <- colonias_factor %>%
  pull(PC1) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric

# Así se crean las etiquetas
labels <- imap_chr(q1, function(., idx){
  return(paste0(round(q1[idx] , digits=1),
                "",
                " – ",
                round(q1[idx + 1] , digits=1),
                ""))
})


# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels

# Crear la variable
colonias_factor  %<>%
  mutate(PC1_q = cut(PC1,
                     breaks = q1,
                     labels = labels,
                     include.lowest = T))
hist(colonias_factor$PC1)
glimpse(colonias_factor)
table(colonias_factor$PC1_q)

colonias_factor$estrato=factor(colonias_factor$PC1_q,
                               levels = c("-6.4 – -1.8",
                                          "-1.8 – -0.2",
                                          "-0.2 – 0.7",
                                          "0.7 – 1.6",
                                          "1.6 – 5.8"),
                               labels = c("Medio Alto/Alto", "Medio", "Medio Bajo", "Bajo", "Muy Bajo"))
#Nota: Al nivel más alto le llamo "medio alto/alto", porque sería dificil argumentar
  #que el 20% más alto es estrato alto, en términos de estratificación social.

table(colonias_factor$estrato)



#Mapa 6 PC1 o índice de estratifiación social----
#La numeración consecutiva viene de los 6 mapas hechos en el scritp censo_2020_cdmx.r

gg +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  geom_sf(
    data = colonias_factor,
    aes(
      fill = PC1_q
    ),
    # use thin white stroke
    color = "#F5F5F3",
    size = 0.4,
    inherit.aes = FALSE
  ) +
  # Viridis color scale
  scale_fill_viridis(
    option = "magma",
    alpha = 0.99, 
    begin = 0, 
    end = 1,
    discrete = T, 
    direction = 1, 
    na.translate=FALSE,
    guide = guide_legend(
      keyheight = unit(4, units = "mm"),
      title.position = "top",
      reverse = F 
    )) +
  # Utilizar un borde más grueso para los municipios
  geom_sf(
    data = mun_cdmx,
    fill = "transparent",
    color = "black",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Índice de estratificación social, CDMX",
       subtitle = "Según Colonia Censo de Población y Vivienda-2020",
       caption = "Nota: El índice se calcula con el método de componentes principales,
         utilizando cinco distintas variables.
         Fuente: Elaborado por Máximo E. Jaramillo Molina 
(@rojo_neon) con datos de CPV-2020.",
       fill = "Puntaje en índice 
         (Quintiles)"
  ) +
  # Hacer un pequeño zoom
  coord_sf(
    xlim = c(-99.35, -98.94), 
    ylim = c(19.16,19.63), 
    expand = FALSE) +
  # add the theme
  theme_map() 

rm(colonias_adip,colonias_cdmx,colonias_cdmx_datos,datos_manzana,datos_mza,factor_cdmx,datitos_mza,datitos_colonias)    
    
#saveRDS(colonias_factor, file = "colonias_factor.rds")


