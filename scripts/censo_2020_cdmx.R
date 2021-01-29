######################################################################
# Mapa de estratificación social de la CDMX, a nivel AGEB y manzanas
#Basdao en metodología y argumentación de acá: https://bit.ly/3qTpz6B
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################



##############
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

#####Mapa CDMX con terrain-----

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

#Preparar los datos ----

#Datos cartográficos del Margo Geoestadístico Nacional
#Descarguen acá: https://www.inegi.org.mx/contenidos/productos//prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469_s.zip
#Convertí las capas .shp del marco geoestadístico 2020 a geojson en qgis

colonias_cdmx <- st_read("~/Documents/Encuestas/Censo 2020/marco_geoestadistico/09_ciudaddemexico/conjunto_de_datos/09colonias_manzanas.geojson")
table(colonias_cdmx$nombre)
summary(colonias_cdmx$nombre)

mun_cdmx <- st_read("~/Documents/Encuestas/Censo 2020/marco_geoestadistico/09_ciudaddemexico/conjunto_de_datos/09mun.geojson")

ageb_cdmx <- st_read("~/Documents/Encuestas/Censo 2020/marco_geoestadistico/09_ciudaddemexico/conjunto_de_datos/09a.geojson")
glimpse(ageb_cdmx)

datos_mza <- read_csv(file = "~/Documents/Encuestas/Censo 2020/AGEB/RESAGEBURB_09CSV20.csv")
glimpse(datos_mza)

datos_ageb <- datos_mza %>% 
  dplyr::rename(CVE_ENT=ENTIDAD,
                CVE_MUN=MUN,
                CVE_LOC=LOC,
                CVE_AGEB=AGEB,
                CVE_MZA=MZA) %>% 
  dplyr::filter(NOM_LOC=="Total AGEB urbana")

glimpse(datos_ageb)

datitos <- ageb_cdmx %>% 
  left_join(datos_ageb)



  
  #Mapa 1 GRAPROES ----
  
  table(datitos$GRAPROES)
  datitos %<>% 
    mutate(GRAPROES=as.numeric(GRAPROES, na.rm = TRUE))
  summary(datitos$GRAPROES)           
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos %>%
    pull(GRAPROES) %>%
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
  datitos  %<>%
    mutate(GRAPROES_q = cut(GRAPROES,
                   breaks = q1,
                   labels = labels,
                   include.lowest = T))

  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos,
      aes(
        fill = GRAPROES_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
      alpha = 0.99, 
      begin = 0, 
      end = 1,
      discrete = T, 
      direction = -1, 
      na.translate=FALSE,
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        title.position = "top",
        reverse = T 
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
         title = "Grado promedio de escolaridad, CDMX",
         subtitle = "Según AGEB. Censo de Población y Vivienda-2020",
         caption = "Fuente: Elaborado por Máximo E. Jaramillo Molina 
(@rojo_neon) con datos de CPV-2020.",
         fill = "Grado promedio 
         (Quintiles)"
    ) +
    # Hacer un pequeño zoom
    coord_sf(
      xlim = c(-99.35, -98.94), 
      ylim = c(19.16,19.63), 
      expand = FALSE) +
    # add the theme
    theme_map() 
  
  

  
  #Mapa 2 PSINDER ----
  
  datitos %<>% 
    mutate(PSINDER=as.numeric(PSINDER, na.rm = TRUE))
  summary(datitos$PSINDER)           
  
  datitos %<>% 
    filter(!is.na(POBTOT)) %>% 
    mutate(PSINDER_porcen= PSINDER/POBTOT*100)
  summary(datitos$PSINDER_porcen)     
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos %>%
    pull(PSINDER_porcen) %>%
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
  datitos  %<>%
    mutate(PSINDER_porcen_q = cut(PSINDER_porcen,
                            breaks = q1,
                            labels = labels,
                            include.lowest = T))
  
  table(datitos$PSINDER_porcen_q)
  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos,
      aes(
        fill = PSINDER_porcen_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
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
         title = "Pob. sin afiliación a servicios de salud, CDMX",
         subtitle = "Según AGEB. Censo de Población y Vivienda-2020",
         caption = "Fuente: Elaborado por Máximo E. Jaramillo Molina 
(@rojo_neon) con datos de CPV-2020.",
         fill = "Porcentaje de pob. 
         (Quintiles)"
    ) +
    # Hacer un pequeño zoom
    coord_sf(
      xlim = c(-99.35, -98.94), 
      ylim = c(19.16,19.63), 
      expand = FALSE) +
    # add the theme
    theme_map() 
  
  
  
  
  
  #Mapa 3 PAFIL_IPRIV ----
  
  datitos %<>% 
    mutate(PAFIL_IPRIV=as.numeric(PAFIL_IPRIV, na.rm = TRUE))
  summary(datitos$PAFIL_IPRIV)           
  
  datitos %<>% 
    filter(!is.na(POBTOT)) %>% 
    mutate(PAFIL_IPRIV_porcen= PAFIL_IPRIV/POBTOT*100)
  summary(datitos$PAFIL_IPRIV_porcen)     
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos %>%
    pull(PAFIL_IPRIV_porcen) %>%
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
  datitos  %<>%
    mutate(PAFIL_IPRIV_porcen_q = cut(PAFIL_IPRIV_porcen,
                                  breaks = q1,
                                  labels = labels,
                                  include.lowest = T))
  
  table(datitos$PAFIL_IPRIV_porcen_q)
  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos,
      aes(
        fill = PAFIL_IPRIV_porcen_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
      alpha = 0.99, 
      begin = 0, 
      end = 1,
      discrete = T, 
      direction = -1, 
      na.translate=FALSE,
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        title.position = "top",
        reverse = T 
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
         title = "Pob. afiliación a salud privada, CDMX",
         subtitle = "Según AGEB. Censo de Población y Vivienda-2020",
         caption = "Fuente: Elaborado por Máximo E. Jaramillo Molina 
(@rojo_neon) con datos de CPV-2020.",
         fill = "Porcentaje de pob. 
         (Quintiles)"
    ) +
    # Hacer un pequeño zoom
    coord_sf(
      xlim = c(-99.35, -98.94), 
      ylim = c(19.16,19.63), 
      expand = FALSE) +
    # add the theme
    theme_map() 
  
  
  
  
  
  
  #Mapa 4 Jefatura femenina del hogar ----
  #No encontré la variable
  
  #Mapa 5 PRO_OCUP_C ----
  
  datitos %<>% 
    mutate(PRO_OCUP_C=as.numeric(PRO_OCUP_C, na.rm = TRUE))
  summary(datitos$PRO_OCUP_C)           
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos %>%
    pull(PRO_OCUP_C) %>%
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
  datitos  %<>%
    mutate(PRO_OCUP_C_q = cut(PRO_OCUP_C,
                                      breaks = q1,
                                      labels = labels,
                                      include.lowest = T))
  
  table(datitos$PRO_OCUP_C_q)
  hist(datitos$PRO_OCUP_C)
  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos,
      aes(
        fill = PRO_OCUP_C_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
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
         title = "Promedio de ocupantes por habitación, CDMX",
         subtitle = "Según AGEB. Censo de Población y Vivienda-2020",
         caption = "Fuente: Elaborado por Máximo E. Jaramillo Molina 
(@rojo_neon) con datos de CPV-2020.",
         fill = "Prom. ocupantes. 
         (Quintiles)"
    ) +
    # Hacer un pequeño zoom
    coord_sf(
      xlim = c(-99.35, -98.94), 
      ylim = c(19.16,19.63), 
      expand = FALSE) +
    # add the theme
    theme_map() 
  
  
  
  
  
  
  #Mapa 6 VPH_INTER ----
  
  datitos %<>% 
    mutate(VPH_INTER=as.numeric(VPH_INTER, na.rm = TRUE),
           TVIVPARHAB=as.numeric(TVIVPARHAB, na.rm = TRUE))
  summary(datitos$VPH_INTER)  
  summary(datitos$TVIVPARHAB)  
  
  datitos %<>% 
    filter(!is.na(TVIVPARHAB)) %>% 
    mutate(VPH_INTER_porcen= VPH_INTER/TVIVPARHAB*100)
  summary(datitos$VPH_INTER_porcen)     
  
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos %>%
    pull(VPH_INTER_porcen) %>%
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
  datitos  %<>%
    mutate(VPH_INTER_porcen_q = cut(VPH_INTER_porcen,
                              breaks = q1,
                              labels = labels,
                              include.lowest = T))
  
  table(datitos$VPH_INTER_porcen_q)
  hist(datitos$VPH_INTER_porcen)
  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos,
      aes(
        fill = VPH_INTER_porcen_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
      alpha = 0.99, 
      begin = 0, 
      end = 1,
      discrete = T, 
      direction = -1, 
      na.translate=FALSE,
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        title.position = "top",
        reverse = T 
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
         title = "Porcentaje de viviendas con internet, CDMX",
         subtitle = "Según AGEB. Censo de Población y Vivienda-2020",
         caption = "Fuente: Elaborado por Máximo E. Jaramillo Molina 
(@rojo_neon) con datos de CPV-2020.",
         fill = "Porcentaje de vivs. 
         (Quintiles)"
    ) +
    # Hacer un pequeño zoom
    coord_sf(
      xlim = c(-99.35, -98.94), 
      ylim = c(19.16,19.63), 
      expand = FALSE) +
    # add the theme
    theme_map() 
  
  
  
  
  
  #Análisis de componentes princiaples----
  
  pca_matrix <- datitos %>% 
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
  dim(datitos)
  
  datitos_factor <- cbind(datitos,factor_cdmx)
  glimpse(datitos_factor)
  
  
  
  
  
  
  
  
  #Mapa 6 PC1 o índice de estratifiación social ----
  
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos_factor %>%
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
  datitos_factor  %<>%
    mutate(PC1_q = cut(PC1,
                                    breaks = q1,
                                    labels = labels,
                                    include.lowest = T))
  hist(datitos_factor$PC1)
  #saveRDS(datitos_factor, file = "www/datitos_factor.rds")
  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos_factor,
      aes(
        fill = PC1_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
      alpha = 0.99, 
      begin = 0, 
      end = 1,
      discrete = T, 
      direction = -1, 
      na.translate=FALSE,
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        title.position = "top",
        reverse = T 
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
         subtitle = "Según AGEB. Censo de Población y Vivienda-2020",
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
  
  
  
  
  
  
  #Mismo análisis, por manzanas ----
  
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
  
  mza_cdmx <- st_read("~/Documents/Encuestas/Censo 2020/marco_geoestadistico/09_ciudaddemexico/conjunto_de_datos/09m.geojson")
  glimpse(mza_cdmx)
  
  datitos_mza <- mza_cdmx %>% 
    mutate( CVE_MZA=as.numeric(CVE_MZA)) %>% 
    left_join(datos_manzana)

  datitos_mza %<>% 
    mutate(
      POBTOT=as.numeric(POBTOT, na.rm = TRUE),
      
      GRAPROES=as.numeric(GRAPROES, na.rm = TRUE),
      
      PSINDER=as.numeric(PSINDER, na.rm = TRUE),
      PSINDER_porcen= PSINDER/POBTOT*100,
      
      PAFIL_IPRIV=as.numeric(PAFIL_IPRIV, na.rm = TRUE),
      PAFIL_IPRIV_porcen= PAFIL_IPRIV/POBTOT*100,
      
      PRO_OCUP_C=as.numeric(PRO_OCUP_C, na.rm = TRUE),
      
      VPH_INTER=as.numeric(VPH_INTER, na.rm = TRUE),
      TVIVPARHAB=as.numeric(TVIVPARHAB, na.rm = TRUE),
      VPH_INTER_porcen= VPH_INTER/TVIVPARHAB*100
      )
    
    pca_matrix <- datitos_mza %>% 
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
  dim(datitos_mza)
  
  datitos_factor_mza <- cbind(datitos_mza,factor_cdmx)
  glimpse(datitos_factor_mza)
  
  
  
  
  
  
  #Mapa 7 PC1 o índice de estratifiación social ----
  
  
  #Clase que qiuero
  no_classes <- 5
  # Extraer cuantiles
  q1 <- datitos_factor_mza %>%
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
  datitos_factor_mza  %<>%
    mutate(PC1_q = cut(PC1,
                       breaks = q1,
                       labels = labels,
                       include.lowest = T))
  hist(datitos_factor_mza$PC1)
  
  gg +
    # use the "alpha hack" (as the "fill" aesthetic is already taken)
    scale_alpha(name = "",
                range = c(0.6, 0),
                guide = F) + # suppress legend
    geom_sf(
      data = datitos_factor_mza,
      aes(
        fill = PC1_q
      ),
      # use thin white stroke
      color = "white",
      size = 0.00,
      lwd = 0,
      inherit.aes = FALSE
    ) +
    # Viridis color scale
    scale_fill_viridis(
      option = "inferno",
      alpha = 0.99, 
      begin = 0, 
      end = 1,
      discrete = T, 
      direction = -1, 
      na.translate=FALSE,
      guide = guide_legend(
        keyheight = unit(4, units = "mm"),
        title.position = "top",
        reverse = T 
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
         subtitle = "Según manzanas Censo de Población y Vivienda-2020",
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
  
  
  
  
  #Leaflet
  
  pal <- colorQuantile(palette = "inferno",
                       n = 10,
                       domain = datitos_factor_mza$PC1,
                       rev = F,
                       na.color = "#F5F5F3")
  
  pal <- colorFactor(palette = "inferno",
                                 domain = datitos_factor_mza$PC1_q,
                                 rev = T,
                                 na.color = "#F5F5F3")
  
  
  
  label <- paste0("AGEB: ", datitos_factor_mza$CVE_AGEB)
  popup <- paste0("<b>Alcaldía: </b>", datitos_factor_mza$NOM_MUN, "<br>",
                  "<b>Valor índice: </b>", datitos_factor_mza$PC1, "<br>")
  
  leaflet(data = datitos_factor_mza, options = leafletOptions(minZoom = 5)) %>%
    setMaxBounds(lng1 = -99.35, lat1 = 19.10, lng2 = -98.94, lat2 = 19.6) %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner") %>% 
    #Estrato social
    addPolygons(
      label = label,
      popup = popup,
      weight = 0.1,
      color = "white",
      fillColor = ~pal(PC1_q),
      fillOpacity = 0.8,
      #color = "#939486",
      opacity = 0.8,
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
  
  
  #saveRDS(datitos_factor_mza, file = "www/datitos_factor_mza.rds")
  
  
  
  
  