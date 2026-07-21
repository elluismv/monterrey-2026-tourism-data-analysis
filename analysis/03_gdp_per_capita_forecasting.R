library(classInt)
library(corrplot)
library(dlookr)
library(dplyr)
library(forecast)
library(foreign)
library(forcats)
library(ggmap)
library(ggplot2)
library(gridExtra)
library(htmltools)
library(jsonlite)
library(knitr)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafsync)
library(lubridate)
library(mapview)
library(openxlsx)
library(patchwork)
library(RColorBrewer)
library(rgeoda)
library(rlang)
library(scales)
library(sf)
library(SnowballC)
library(sp)
library(spdep)
library(stringr)
library(syuzhet)
library(tigris)
library(tidyr)
library(tidyverse)
library(tm)
library(tmap)
library(viridis)
library(wordcloud)
library(wordcloud2)
library(googleway) 
library(readxl)
library(purrr)
library(spatialreg)
library(stargazer)
library(regclass)
library(raster)
library(spgwr)


tmap_mode("view")

#----DF'S-------

df7 <-  read_excel("Encuesta_Eventos_Nuevo_Leon.xlsx",sheet = "Turismo_Muni")
df7$metro <- as.factor(df7$metro)
df7$atraccion_turistica <- as.factor(df7$atraccion_turistica)

df_mx_mun_map <- st_read("mx_maps/mx_mpios/Mexican Municipalities.shp")
nuevo_leon_map <- df_mx_mun_map %>%
  filter(CVE_ENT == 19)

df_denue_turismo_CA <- read_sf("INEGI_DENUE_caracteristicas/INEGI_DENUE_19052025.shp", options = "ENCODING=LATIN1")
df_denue_turismo_CO <- read_sf("INEGI_DENUE_conexas/INEGI_DENUE_19052025.shp", options = "ENCODING=LATIN1")

#----Config-------

mun_glosario <- data.frame(
  CVEGEO = sprintf("190%02d", 1:51),
  NOM_MUN = c(
    "Abasolo", "Agualeguas", "Los Aldamas", "Allende", "Anáhuac", "Apodaca", "Aramberri", "Bustamante",
    "Cadereyta Jiménez", "El Carmen", "Cerralvo", "Ciénega de Flores", "China", "Doctor Arroyo",
    "Doctor Coss", "Doctor González", "Galeana", "García", "San Pedro Garza García", "General Bravo",
    "General Escobedo", "General Terán", "General Treviño", "General Zaragoza", "General Zuazua",
    "Guadalupe", "Los Herreras", "Higueras", "Hualahuises", "Iturbide", "Juárez", "Lampazos de Naranjo",
    "Linares", "Marín", "Melchor Ocampo", "Mier y Noriega", "Mina", "Montemorelos", "Monterrey",
    "Parás", "Pesquería", "Los Ramones", "Rayones", "Sabinas Hidalgo", "Salinas Victoria",
    "San Nicolás de los Garza", "Hidalgo", "Santa Catarina", "Santiago", "Vallecillo", "Villaldama"
  )
)

mun_glosario$CVEGEO <- as.numeric(mun_glosario$CVEGEO)


nuevo_leon_map <- nuevo_leon_map %>%
  left_join(mun_glosario, by = c("IDUNICO" = "CVEGEO"))

#codelags <- c(2199, 2200, 2201, 2202, 2203, 2204, 2205, 2206, 2208, 2211, 2221, 2245)
codes <- unique(df7$municipio)

df_zona_tur <- nuevo_leon_map %>%
  filter(NOM_MUN %in% codes)

#---- Unidades Económicas Turísticas-------
##----CA-----
UE_turismo_CA <- df_denue_turismo_CA %>% 
  st_drop_geometry() %>%
  count(municipio, `nombre_act`) %>%
  pivot_wider(names_from = `nombre_act`, values_from = n, values_fill = 0)

colnames(UE_turismo_CA)

UE_turismo_CA <- UE_turismo_CA %>%
  mutate(n_alojamientos = 
           `Hoteles sin otros servicios integrados` +
           `Pensiones y casas de huéspedes` +
           `Hoteles con otros servicios integrados` +
           `Moteles` +
           `Cabañas, villas y similares`)

UE_turismo_CA <- UE_turismo_CA %>%
  mutate(n_servicios_bienes_raices = 
           `Inmobiliarias y corredores de bienes raíces` +
           `Servicios de administración de bienes raíces`)

UE_turismo_CA <- UE_turismo_CA %>%
  rename(
    n_tiendas_artesanias = `Comercio al por menor en tiendas de artesanías`,
    n_agencias_viajes = `Agencias de viajes`
  )

colnames(UE_turismo_CA)

df7 <- df7 %>%
  left_join(UE_turismo_CA %>% dplyr::select(municipio, n_agencias_viajes,
                                            n_tiendas_artesanias, n_alojamientos, 
                                            n_servicios_bienes_raices), 
            by = "municipio")

##----CO-----
UE_turismo_CO <- df_denue_turismo_CO %>% 
  st_drop_geometry() %>%
  count(municipio, `nombre_act`) %>%
  pivot_wider(names_from = `nombre_act`, values_from = n, values_fill = 0)

UE_turismo_CO <- UE_turismo_CO %>%
  mutate(
    n_alimentos_bebidas = 
      `Cafeterías, fuentes de sodas, neverías, refresquerías y similares` +
      `Restaurantes con servicio de preparación de alimentos a la carta o de comida corrida` +
      `Restaurantes con servicio de preparación de pizzas, hamburguesas, hot dogs y pollos rostizados para llevar` +
      `Restaurantes con servicio de preparación de antojitos` +
      `Restaurantes con servicio de preparación de tacos y tortas` +
      `Servicios de preparación de otros alimentos para consumo inmediato`
  )

UE_turismo_CO <- UE_turismo_CO %>%
  rename(
    n_tiendas_conveniencia = `Comercio al por menor en minisupers`,
    n_tiendas_ropa = `Comercio al por menor de ropa, excepto de bebé y lencería`
  )

colnames(UE_turismo_CO)


df7 <- df7 %>%
  left_join(UE_turismo_CO %>% dplyr::select(municipio,n_tiendas_conveniencia, n_tiendas_ropa,
                                            n_alimentos_bebidas),by = "municipio")


##----DF Final-----

colSums(is.na(df7))

df7 <- df7 %>%
  mutate(across(
    c(n_agencias_viajes, n_tiendas_artesanias, n_alojamientos,n_servicios_bienes_raices),
    ~replace_na(., 0)
  ))

df7 <- df7 %>%
  mutate(densidad_alojamientos = n_alojamientos / extension,
         densidad_alimentos_bebidas = n_alimentos_bebidas / extension,
         densidad_tiendas_ropa = n_tiendas_ropa / extension,
         densidad_tiendas_conveniencia = n_tiendas_conveniencia / extension)


df7 <- df7 %>%
  mutate(infraestructura_turistica = 
           densidad_alojamientos + densidad_alimentos_bebidas + 
           densidad_tiendas_conveniencia)


selected_vars <- df7 %>%
  dplyr::select(
    extension,
    densidad_poblacion,
    pib_municipal,
    pib_turistico,
    pib_turistico_per_capita,
    pct_pobreza,
    tasa_criminalidad,
    ingreso_hogar_,
    ingreso_per_capita,
    n_alojamientos,
    n_alimentos_bebidas,
    n_servicios_bienes_raices,
    n_tiendas_conveniencia,
    densidad_alojamientos,
    densidad_alimentos_bebidas,
    infraestructura_turistica,
  )

plot_normality(selected_vars)

df7 <- df7 %>%
  mutate(densidad_turismo_comerciales = (n_tiendas_ropa + n_tiendas_artesanias)/extension)

zona_tur_map <- df_zona_tur %>%
  left_join(df7, by = c("NOM_MUN" = "municipio"))

#----EDA---------------

ggplot(df7, aes(x=ingreso_hogar_, y=pib_turistico_per_capita)) +
  geom_point() +
  scale_x_log10()

#----Mapas---------------

centroides <- st_centroid(df_zona_tur)

leaflet(data = nuevo_leon_map) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "orange",
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    label = ~NOM_MUN,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLabelOnlyMarkers(
    data = centroides,
    label = ~NOM_MUN,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'center',
      textOnly = TRUE,
      style = list("font-size" = "5px", "color" = "black", "font-weight" = "bold")
    )
  )
leaflet(data = df_zona_tur) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "coral",
    weight = 1,
    color = "white",
    fillOpacity = 0.8,
    label = ~NOM_MUN,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )%>%
  addLabelOnlyMarkers(
    data = centroides,
    label = ~NOM_MUN,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'center',
      textOnly = TRUE,
      style = list("font-size" = "8px", "color" = "black", "font-weight" = "bold")
    )
  )


zona_tur_map_coords <- zona_tur_map %>%
  select(n_alojamientos) %>%
  mutate(centroide = st_centroid(geometry)) %>%
  mutate(
    Longitud = st_coordinates(centroide)[, 1],
    Latitud = st_coordinates(centroide)[, 2]
  ) %>%
  st_drop_geometry()

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = zona_tur_map_coords,
             lng = ~Longitud, lat = ~Latitud,
             blur = 25, max = 0.001, radius = 15,
             group = "Alojamientos")%>%
  addPolygons(
    data = df_zona_tur,
    color = "steelblue", weight = 2, fill = FALSE,
    label = ~NOM_MUN,  # Si tienes esa columna
    group = "Municipios"
  ) 

zona_tur_map_coords <- zona_tur_map %>%
  select(n_alimentos_bebidas) %>%
  mutate(centroide = st_centroid(geometry)) %>%
  mutate(
    Longitud = st_coordinates(centroide)[, 1],
    Latitud = st_coordinates(centroide)[, 2]
  ) %>%
  st_drop_geometry()

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = zona_tur_map_coords,
             lng = ~Longitud, lat = ~Latitud,
             blur = 25, max = 0.001, radius = 15,
             group = "Alojamientos")%>%
  addPolygons(
    data = df_zona_tur,
    color = "steelblue", weight = 2, fill = FALSE,
    label = ~NOM_MUN,  # Si tienes esa columna
    group = "Municipios"
  ) 


##----GEO ESPACIAL---------------
tm_shape(df_zona_tur) +
  tm_polygons(
    col = "coral",               
    border.col = "white",        
    lwd = 1,                     
    alpha = 0.8,                 
    id = "NOM_MUN",              
    popup.vars = "NOM_MUN"       
  ) +
  tm_layout(
    main.title = "Zonas Turísticas",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "pib_turistico_per_capita", palette="Blues", style="quantile", title="PIB Turístico per cápita") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "PIB Turístico Per Cápita",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

mapview(
  zona_tur_map,
  zcol = "pib_turistico_per_capita",   # variable a mapear por color
  layer.name = "PIB Turístico PC",
  col.regions = RColorBrewer::brewer.pal(6, "Blues"),
  at = quantile(zona_tur_map$pib_turistico_per_capita, probs = seq(0, 1, length.out = 6), na.rm = TRUE),
  label = zona_tur_map$NOM_MUN,
  legend = TRUE
)

tm_shape(zona_tur_map) + 
  tm_polygons(col = "densidad_poblacion", palette="PurpOr", style="quantile", title="Habitantes/km2") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Densidad de Población",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "pct_pobreza", palette="RdPu", style="quantile", title="% Pobreza") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "% Poblacion en Pobreza",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "ingreso_per_capita", palette="BuGn", style="quantile", title="Ingreso Anual") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Estimación Ingreso per Cápita",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "tasa_criminalidad", palette="OrYel", style="quantile", title="Crímenes/10,000") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Tasa Criminalidad",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )


tm_shape(zona_tur_map) + 
  tm_polygons(col = "densidad_alojamientos", palette="TealGrn", style="quantile", title="Alojamientos/km2") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Densidad de Alojamientos",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "densidad_alimentos_bebidas", palette="Magenta", style="quantile", title="Establecimientos/km2") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Densidad de Establecimientos Comida & Bebidas",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )



#----Matrices Queen & Rook-------
swm_queen  <- poly2nb(df_zona_tur, queen=TRUE)
rswm_queen <- nb2listw(swm_queen, style="W", zero.policy = TRUE)

summary(rswm_queen)


swm_rook  <- poly2nb(df_zona_tur, queen=FALSE)
rswm_rook <- nb2listw(swm_rook, style="W", zero.policy = TRUE)


#sf::st_is_valid(zona_tur_map)


variables <- c("poblacion", "extension", "densidad_pop", "criminalidad", "tasa_criminalidad",
               "pib_turistico", "pib_turistico_pct", "pib_turistico_per_capita", "n_alojamientos",
               "n_bares", "n_top_plazas_gm", "metro", "atraccion_turistica", "n_agencias_viaje",
               "n_tiendas_artesanias", "n_inmobiliarias", "n_parques_acu_bal", "n_trans_colectivo_fijo",
               "n_admin_bienes_raices", "n_trans_turistico_tierra", "n_tiendas_ropa",
               "n_tiendas_conveniencia", "n_alimentos_bebidas")

df_zona_tur_a <- as(df_zona_tur, "Spatial")
df_zona_tur_centroid <- coordinates(df_zona_tur_a) 
plot(df_zona_tur_a,border="blue",axes=F,las=1, main="Zona Turísitca Nuevo León - Queen SWM")
plot(df_zona_tur_a,col="grey",border=grey(0.9),axes=T,add=T) 
plot(rswm_queen,coords=df_zona_tur_centroid,col="darkorange",add=T) 

nb_lines <- nb2lines(swm_queen, coords = df_zona_tur_centroid, as_sf = TRUE)
centroides <- st_centroid(df_zona_tur)


tm_shape(df_zona_tur) +
  tm_polygons(border.col = "gray") +
  tm_shape(nb_lines) +
  tm_lines(col = "coral", lwd = 2.0) +
  tm_shape(centroides) +
  tm_dots(size = 0.5, col = "darkred", border.col = "black")


#Matriz de Distancias por k
#nb_dist <- dnearneigh(df_zona_tur_centroid, 0, 30000)  # distancia en metros (ajusta según tus datos)
#lw_dist <- nb2listw(nb_dist, style = "W", zero.policy = TRUE)

#----Distribución-------
df7 %>%
  dplyr::select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribución de Variables Numéricas", x = "Valor", y = "Frecuencia")


#----Variables Normalidad & Moran's-------
#-- Pib turistico per capita (variable de respuesta) no tiene autocorrelación espacial y (normalidad: con log())
hist(log(zona_tur_map$pib_turistico_per_capita))
shapiro.test(log(zona_tur_map$pib_turistico_per_capita))
moran.test(log(zona_tur_map$pib_turistico_per_capita), rswm_queen)

#--Densidad de población tiene autocorrelación espacial y (normalidad: No, aun con transformaciones)
hist(log(zona_tur_map$poblacion))
shapiro.test((zona_tur_map$poblacion))
moran.test(log(zona_tur_map$poblacion), rswm_queen)

#--Porcentaje pobreza tiene autocorrelación espacial y (normalidad: con log())
hist(log(zona_tur_map$pct_pobreza))
shapiro.test(log(zona_tur_map$pct_pobreza))
moran.test((zona_tur_map$pct_pobreza), rswm_queen)

#--Tasa criminalidad tiene autocorrelación espacial al 90% de confianza y (Normalidad: Sí)
hist((zona_tur_map$tasa_criminalidad))
shapiro.test((zona_tur_map$tasa_criminalidad))
moran.test((zona_tur_map$tasa_criminalidad), rswm_queen)

#--Ingreso per cápita  tiene autocorrelación espacial y (Normalidad: con log())
hist((zona_tur_map$ingreso_per_capita))
shapiro.test(log(zona_tur_map$ingreso_per_capita))
moran.test((zona_tur_map$ingreso_per_capita), rswm_queen)

#--Alojamientos y Alimentos y Bebidas tienen autocorrelación espacial  y (Normalidad: No, aun con transformaciones)
moran.test((zona_tur_map$densidad_alojamientos), rswm_queen)
moran.test((zona_tur_map$densidad_alimentos_bebidas), rswm_queen)

zona_tur_map$densidad_alojamientos[zona_tur_map$densidad_alojamientos ==0] <- 0.001
shapiro.test(log(zona_tur_map$densidad_alojamientos))


#--MiniSupers, Tiendas Ropa, Agencias de Viajes, Tiendas Artesanias, tienen autocorrelación espacial  y (Normalidad: No, aun con transformaciones)
shapiro.test((zona_tur_map$desdid))
shapiro.test((zona_tur_map$n_tiendas_ropa))
shapiro.test((zona_tur_map$n_agencias_viajes))
shapiro.test((zona_tur_map$n_tiendas_artesanias))


zona_tur_map$pib_turistico_per_capita_lag <- lag.listw(rswm_queen, zona_tur_map$pib_turistico_per_capita, zero.policy=TRUE)
zona_tur_map$densidad_poblacion_lag <- lag.listw(rswm_queen, zona_tur_map$densidad_poblacion, zero.policy=TRUE)
zona_tur_map$pct_pobreza_lag <- lag.listw(rswm_queen, zona_tur_map$pct_pobreza, zero.policy=TRUE)
zona_tur_map$tasa_criminalidad_lag <- lag.listw(rswm_queen, zona_tur_map$tasa_criminalidad, zero.policy=TRUE)
zona_tur_map$ingreso_per_capita_lag <- lag.listw(rswm_queen, zona_tur_map$ingreso_per_capita, zero.policy=TRUE)
zona_tur_map$densidad_alojamientos_lag <- lag.listw(rswm_queen, zona_tur_map$densidad_alojamientos, zero.policy=TRUE)
zona_tur_map$densidad_alimentos_bebidas_lag <- lag.listw(rswm_queen, zona_tur_map$densidad_alimentos_bebidas, zero.policy=TRUE)
zona_tur_map$n_tiendas_convenienciam_lag <- lag.listw(rswm_queen, zona_tur_map$n_tiendas_conveniencia, zero.policy=TRUE)

tm_shape(zona_tur_map) + 
  tm_polygons(col = "pib_turistico_per_capita_lag", palette="Blues", style="quantile", title="PIB Turístico PC") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "PIB Turístico Per Cápita Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "densidad_poblacion_lag", palette="PurpOr", style="quantile", title="Habitantes/km2") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Densidad de Población Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "pct_pobreza_lag", palette="RdPu", style="quantile", title="% Pobreza") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "% Poblacion en Pobreza Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "ingreso_per_capita_lag", palette="BuGn", style="quantile", title="Ingreso Anual") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Estimación Ingreso per Cápita Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "tasa_criminalidad_lag", palette="OrYel", style="quantile", title="Crímenes/10,000") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Tasa Criminalidad Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )


tm_shape(zona_tur_map) + 
  tm_polygons(col = "densidad_alojamientos_lag", palette="TealGrn", style="quantile", title="Alojamientos/km2") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Densidad de Alojamientos Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(zona_tur_map) + 
  tm_polygons(col = "densidad_alimentos_bebidas_lag", palette="Magenta", style="quantile", title="Establecimientos/km2") +
  tm_text("NOM_MUN", size = "AREA", auto.placement = TRUE) +
  tm_layout(
    main.title = "Densidad de Establecimientos Comida & Bebidas Lag",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

#sswm_dist <- distance_weights(zona_tur_map, dist_thres = 0.4)

queen_w<-queen_weights(zona_tur_map)
lisa_pib_dist <- local_moran(queen_w, zona_tur_map["pib_turistico_per_capita"])
zona_tur_map$cluster_pib_turistico_per_capita <- as.factor(lisa_pib_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_pib_turistico_per_capita) <- lisa_pib_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_pib_turistico_per_capita)) +
  ggtitle(label = "Clúster PIB Turístico Per Cápita", subtitle = "Zona Turística") +
  theme_minimal()

lisa_densidad_poblacion_dist <- local_moran(queen_w, zona_tur_map["densidad_poblacion"])
zona_tur_map$cluster_densidad_poblacion <- as.factor(lisa_densidad_poblacion_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_densidad_poblacion) <- lisa_densidad_poblacion_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_densidad_poblacion)) +
  ggtitle(label = "Clúster Densidad de Poblacion", subtitle = "Zona Turística") +
  theme_minimal()

lisa_pct_pobreza_dist <- local_moran(queen_w, zona_tur_map["pct_pobreza"])
zona_tur_map$cluster_pct_pobreza <- as.factor(lisa_pct_pobreza_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_pct_pobreza) <- lisa_pct_pobreza_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_pct_pobreza)) +
  ggtitle(label = "Clúster Población en Pobreza", subtitle = "Zona Turística") +
  theme_minimal()

lisa_tasa_criminalidad_dist <- local_moran(queen_w, zona_tur_map["tasa_criminalidad"])
zona_tur_map$cluster_tasa_criminalidad <- as.factor(lisa_tasa_criminalidad_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_tasa_criminalidad) <- lisa_tasa_criminalidad_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_tasa_criminalidad)) +
  ggtitle(label = "Clúster Tasa de Criminalidad", subtitle = "Zona Turística") +
  theme_minimal()

lisa_ingreso_per_capita_dist <- local_moran(queen_w, zona_tur_map["ingreso_per_capita"])
zona_tur_map$cluster_ingreso_per_capita <- as.factor(lisa_ingreso_per_capita_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_ingreso_per_capita) <- lisa_ingreso_per_capita_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_ingreso_per_capita)) +
  ggtitle(label = "Clúster Ingreso Per Cápita", subtitle = "Zona Turística") +
  theme_minimal()

lisa_densidad_alojamientos_dist <- local_moran(queen_w, zona_tur_map["densidad_alojamientos"])
zona_tur_map$cluster_densidad_alojamientos <- as.factor(lisa_densidad_alojamientos_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_densidad_alojamientos) <- lisa_densidad_alojamientos_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_densidad_alojamientos)) +
  ggtitle(label = "Clúster Densidad de Alojamientos", subtitle = "Zona Turística") +
  theme_minimal()

lisa_densidad_alimentos_beidas_dist <- local_moran(queen_w, zona_tur_map["densidad_alimentos_bebidas"])
zona_tur_map$cluster_densidad_alimentos_bebidas <- as.factor(lisa_densidad_alimentos_beidas_dist$GetClusterIndicators())
levels(zona_tur_map$cluster_densidad_alimentos_bebidas) <- lisa_densidad_alimentos_beidas_dist$GetLabels()

ggplot(data = zona_tur_map) +
  geom_sf(aes(fill = cluster_densidad_alimentos_bebidas)) +
  ggtitle(label = "Clúster Densidad Alimentos & Bebidas", subtitle = "Zona Turística") +
  theme_minimal()

#----Matriz Correlación-------


# Multicolinealidad: densidad alojamiento y bienees raices, densidad_alimentos_bebidas  y densidad población

#colnames(df7)
df7$densidad_alojamientos[df7$densidad_alojamientos ==0] <- 0.001
zona_tur_map$densidad_alojamientos[zona_tur_map$densidad_alojamientos ==0] <- 0.001


df7$densidad_turismo_comerciales[df7$densidad_turismo_comerciales ==0] <- 0.001
df7$densidad_tiendas_conveniencia[df7$densidad_tiendas_conveniencia ==0] <- 0.001
df7$densidad_tiendas_ropa[df7$densidad_tiendas_ropa ==0] <- 0.001
df7$n_servicios_bienes_raices[df7$n_servicios_bienes_raices ==0] <- 0.001
df7$tasa_criminalidad <- df7$tasa_criminalidad * 10

df_num <- df7[, !names(df7) %in% c("municipio_id","municipio","extension","poblacion","metro","atraccion_turistica")]

matriz_cor <- cor(selected_vars, use = "complete.obs")
corrplot(
  matriz_cor,
  method = "color",             
  type = "upper",               
  order = "hclust",              
  col = colorRampPalette(c("red", "white", "steelblue"))(200),
  addCoef.col = "black",         
  tl.col = "black",              
  tl.cex = 0.9,                  
  number.cex = 0.7,             
  diag = FALSE                  
)

#----Modelos Predictivos-------

lm <- lm(log(pib_turistico_per_capita) ~ 
     log(ingreso_per_capita) +
     log(pct_pobreza) +
      tasa_criminalidad +
     log(densidad_alojamientos) +
     log(densidad_alimentos_bebidas) +
     n_servicios_bienes_raices,
   data = df7)

summary(lm)
AIC(lm)
VIF(lm)
moran.test(lm$residuals, rswm_queen) 

lm.LMtests(lm,rswm_queen,test=c("RLMlag"))
lm.LMtests(lm,rswm_queen,test=c("RLMerr"))

#Los residuos del modelo OLS no muestran dependencia espacial (Moran’s I = -0.08772771, p = 0.6834).
#Su AIC es de 130.758. No existe razón o estructura espacial en los datos que deba ser corregida o modelada mediante regresión espacial
#La especifiación de nuestro modelo Lineal es correcta o suficiente.



spatial_autoregressive <- lagsarlm(log(pib_turistico_per_capita) ~  
                                     log(ingreso_per_capita) + 
                                     log(pct_pobreza) + 
                                     log(densidad_alojamientos) + 
                                     log(densidad_alimentos_bebidas) + 
                                     tasa_criminalidad, 
                                   data = df7, listw = rswm_queen, Durbin = FALSE)
summary(spatial_autoregressive)
moran.test(exp(spatial_autoregressive$residuals), rswm_queen) 

#El efecto espacial (rho) no es significativo, lo cual indica que no hay evidencia suficiente 
#de que el PIB turístico de un municipio esté influido por los valores de los municipios vecinos.



spatial_error<-errorsarlm(log(pib_turistico_per_capita) ~
                            log(ingreso_per_capita) + 
                            log(pct_pobreza) + 
                            log(densidad_alojamientos) +  
                            log(densidad_alimentos_bebidas) + 
                            tasa_criminalidad, 
                          data = df7, listw = rswm_queen, Durbin = FALSE)
summary(spatial_error)
moran.test(exp(spatial_error$residuals), rswm_queen)
df_zona_tur$spatial_error_residuals <- exp(spatial_error$residuals)
mapview(df_zona_tur, zcol = "spatial_error_residuals", col.regions=brewer.pal(5, "Reds"))


spatial_durbin <- lagsarlm(log(pib_turistico_per_capita) ~
                             log(ingreso_per_capita) + 
                             log(pct_pobreza) + 
                             log(densidad_alojamientos) +  
                             log(densidad_alimentos_bebidas) +
                             tasa_criminalidad,
                           data = df7, rswm_queen, type="mixed")
summary(spatial_durbin)

moran.test(exp(spatial_durbin$residuals), rswm_queen)
df_zona_tur$spatial_durbin_residuals <- exp(spatial_durbin$residuals)
df_zona_tur$pred_pib_turistico_pc <- exp(fitted.values(spatial_durbin))
mapview(df_zona_tur, zcol = "spatial_durbin_residuals", col.regions=brewer.pal(5, "Reds"))

tm_shape(df_zona_tur) +
  tm_polygons(col = "pred_pib_turistico_pc", palette="Blues", style="quantile", n=8, title="PIB Turístico pc ($)") +
  tm_layout(
    main.title = "Predicted Durbin PIB Turístico Per Cápita",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(df_zona_tur) +
  tm_polygons(col = "spatial_durbin_residuals", palette="RdPu", style="quantile", n=8, title="PIB Turístico pc ($)") +
  tm_layout(
    main.title = "Predicted Durbin Residuals",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

library(jtools)
export_summs(lm, spatial_autoregressive, spatial_error, spatial_durbin)

#----Modelo GWR-------

library(GWmodel)
library(SpatialML)
library(sf)
library(sp)
library(spgwr)
library(spdep)

## ----Regresió Lineal----
bw1 <- bw.gwr(log(pib_turistico_per_capita) ~ 
                log(ingreso_per_capita) +
                log(pct_pobreza) +
                tasa_criminalidad +
                log(densidad_alojamientos) +
                log(densidad_alimentos_bebidas) +
                n_servicios_bienes_raices, approach = "AIC", adaptive = T, data=zona_tur_map)


m.gwr <- gwr.basic(log(pib_turistico_per_capita) ~ 
                     log(ingreso_per_capita) +
                     log(pct_pobreza) +
                     log(crimenes) +
                     log(densidad_alojamientos) +
                     log(densidad_alimentos_bebidas) +
                     n_servicios_bienes_raices, adaptive = T, data=zona_tur_map, bw = bw1)

m.gwr
gwr_sf = st_as_sf(m.gwr$SDF)
gwr_sf$y_predicted <- gwr_sf$yhat

moran.test(exp(gwr_sf$residual), rswm_queen)
#mapview(gwr_sf, zcol="y_predicted", col.regions=brewer.pal(5, "Oranges"))

## ----Random Forest----
coords <- coordinates(df_zona_tur_a)

grf_data <- df7 %>% dplyr::select(pib_turistico_per_capita,ingreso_per_capita,pct_pobreza,crimenes,densidad_alojamientos,densidad_alimentos_bebidas,n_servicios_bienes_raices)
grf_data <- grf_data %>%  mutate(across(c(pib_turistico_per_capita,ingreso_per_capita,pct_pobreza,crimenes,densidad_alojamientos,densidad_alimentos_bebidas), function(x) log(x)))
formula_grf<-"pib_turistico_per_capita ~ ingreso_per_capita + pct_pobreza + crimenes + densidad_alojamientos + densidad_alimentos_bebidas + n_servicios_bienes_raices" ### GRF model specification


bwgrf <- grf.bw(formula = formula_grf, dataset = grf_data, kernel = "adaptive", coords = coords, bw.min = 18, bw.max = 25, step = 1, trees = 500, mtry = NULL, importance = "impurity", forests = FALSE, weighted = TRUE, verbose = TRUE)
grf_model <- grf(formula = formula_grf, dframe = grf_data, bw=bwgrf$Best.BW, ntree = 500, mtry = 3, kernel = "adaptive", forests = TRUE, coords = coords)

zona_tur_map$grf_predicteddv <- grf_model$LGofFit$LM_yfitPred
tm_shape(zona_tur_map) +
  tm_polygons(col = "grf_predicteddv", palette="Blues", style="quantile", n=8, title="PIB Turístico pc ($)") +
  tm_layout(
    main.title = "Predicted PIB Turístico per cápita",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

zona_tur_map$grf_localR2 <- grf_model$LGofFit$LM_Rsq100
tm_shape(zona_tur_map) +
  tm_polygons(col = "grf_localR2", palette="Greens", style="quantile", n=8, title="R2") +
  tm_layout(
    main.title = "Predicted Local R2",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )


tm_shape(gwr_sf) +
  tm_polygons(col = "y_predicted", palette="Blues", style="quantile", n=8, title="PIB Turístico pc ($)") +
  tm_layout(
    main.title = "Predicted PIB Turístico per cápita",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(gwr_sf) +
  tm_polygons(col = "Local_R2", palette="Greens", style="quantile", n=8, title="R2") +
  tm_layout(
    main.title = "Predicted Local R2",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(gwr_sf) +
  tm_polygons(col = "residual", palette="Greens", style="quantile", n=8, title="R2") +
  tm_layout(
    main.title = "Predicted Local R2",
    title.size = 1.2,
    legend.outside = TRUE,
    asp = 0,
    bg.color = "white"
  )

tm_shape(gwr_sf) +
  tm_polygons(col = "log(ingreso_per_capita)", palette="Greens", style="quantile", n=8, title="R2") +
  tm_layout(title= 'Estimated Local R2',  title.position = c('right', 'top'))

tm_shape(gwr_sf) +
  tm_polygons(col = "log(pct_pobreza)", palette="Greens", style="quantile", n=8, title="R2") +
  tm_layout(title= 'Estimated Local R2',  title.position = c('right', 'top'))

tm_shape(gwr_sf) +
  tm_polygons(col = "log(pct_pobreza)", palette="Greens", style="quantile", n=8, title="R2") +
  tm_layout(title= 'Estimated Local R2',  title.position = c('right', 'top'))
