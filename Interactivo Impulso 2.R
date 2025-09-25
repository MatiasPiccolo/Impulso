library(readxl)
library(sf)
library(tidyverse)
library(tmap)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(patchwork)

# Leemos las bases de datos y obtenemos los data.frames
municipios2 <- st_read("C:/Users/lucho/Desktop/Matias/Laboral/PBA/Mapas/limites-partidos/shp")
impulso2 <- read_excel("C:/Users/lucho/Desktop/Matias/Laboral/PBA/Mapas/Impulso.xlsx", sheet = "Impulso2")

#Transformo el nombre de los municipios para tenerlos a todos con letra mayuscula
municipios2 <- municipios2 %>%
  mutate (nam = toupper(nam))

#Uno los datos de la base de datos de Impulso en las líneas correspondientes a cada municipio
municipios_join2 <- municipios2 %>%
  left_join(impulso2, by = c ("nam" = "Localidad"))

#Si la cantidad es nula los elimino de la lista (EJ:islas)
municipios_join2 <- municipios_join2 %>%
  filter(!is.na(Cantidad))


####################################################################################


# Identificar los municipios manualmente
outliers <- c("GENERAL PUEYRREDÓN", "BAHÍA BLANCA", "LA PLATA")
nulos <- c("COLÓN","GENERAL LAVALLE", "JOSÉ C. PAZ", "MAGDALENA", "MAR QUICHITA","MONTE HERMOSO",
           "GENERAL PINTO", "RAUCH", "RAMALLO", "RIVADAVIA", "ADOLFO ALSINA",
           "ROQUE PÉREZ", "BALCARCE", "GENERAL PAZ", "MALVINAS ARGENTINAS", "MARCOS PAZ",
           "SALLIQUELÓ", "BRANDSEN", "FLORENTINO AMEGHINO", "PELLEGRINI", "SAN FERNANDO", "TAPALQUÉ", "PUNTA INDIO") 

municipios_join2 <- municipios_join2 %>%
  mutate(grupo_monto = case_when(
    nam %in% outliers ~ "outlier",
    nam %in% nulos ~ "nulo",
    TRUE ~ "normal"
  ))

####################################################################################

#Creo el vector de municipios pertenecientes al AMBA
amba_partidos <- c(
  "ALMIRANTE BROWN", "AVELLANEDA", "BERAZATEGUI", "BERISSO", "BRANDSEN",
  "CAMPANA", "CAÑUELAS", "ENSENADA", "ESCOBAR", "ESTEBAN ECHEVERRÍA",
  "EXALTACIÓN DE LA CRUZ", "EZEIZA", "FLORENCIO VARELA", "GENERAL LAS HERAS",
  "GENERAL RODRÍGUEZ", "GENERAL SAN MARTÍN", "HURLINGHAM", "ITUZAINGÓ",
  "JOSÉ C. PAZ", "LA MATANZA", "LA PLATA", "LANÚS", "LOMAS DE ZAMORA",
  "LUJÁN", "MALVINAS ARGENTINAS", "MARCOS PAZ", "MERLO", "MORENO",
  "MORÓN", "QUILMES", "PILAR", "PRESIDENTE PERÓN", "SAN FERNANDO",
  "SAN ISIDRO", "SAN MIGUEL", "SAN VICENTE", "TIGRE", "TRES DE FEBRERO",
  "VICENTE LÓPEZ", "ZÁRATE"
)


#Creo el dataframe específico del AMBA
amba <- municipios_join2 %>% filter(nam %in% amba_partidos)

#Creo una nueva variable para los municipios que pertenecen al AMBA para el dataframe general
municipios_join2 <- municipios_join2 %>%
  mutate(region = if_else (nam %in% amba_partidos, "AMBA", nam))

normales2 <- municipios_join2 %>% filter(grupo_monto == "normal")
rango_normales2 <- range(normales2$Cantidad, na.rm = TRUE)

# Crear una columna para la etiqueta (Cantidad formateada si querés)
municipios_join2 <- municipios_join2 %>%
  mutate(
    etiqueta = scales::comma(Cantidad, big.mark = ".", decimal.mark = ",")
  )


library(ggiraph)

# Construyes el ggplot pero con geom_sf_interactive
p2 <- ggplot() +
  # Municipios normales con escala según monto y tooltip interactivo
  geom_sf_interactive(
    data = normales2,
    aes(
      fill = Cantidad,
      data_id = nam,  # Esto conecta cada municipio con la interactividad
      tooltip = paste0(
        "Municipio: ", nam,
        "\nSección: ", Seccion,
        "\nCréditos: ", Cantidad,
        "\nMonto: ", number(Monto, prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 0.01))),
    color = "gray20"
  ) +
  
  # Outliers pintados encima en magenta con tooltip
  geom_sf_interactive(
    data = filter(municipios_join2, grupo_monto == "outlier"),
    aes(
      data_id = nam,
      tooltip = paste0(
        "Municipio: ", nam,
        "\nSección: ", Seccion,
        "\nCréditos: ", Cantidad,
        "\nMonto: ", number (Monto, prefix ="$", big.mark =".", decimal.mark=",", accuracy=0.01))),
    fill = "#e81f76", color = "gray20"
  ) +
  
  # Nulos pintados encima en blanco con tooltip
  geom_sf_interactive(
    data = filter(municipios_join2, grupo_monto == "nulo"),
    aes(
      data_id = nam,
      tooltip = paste0(
        "Municipio: ", nam,
        "\nSección: ", Seccion,
        "\nCréditos: ", Cantidad,
        "\nMonto: ", number (Monto, prefix ="$", big.mark =".", decimal.mark=",", accuracy=0.01))),
    fill = "#ffffff", color = "gray20"
  ) +
  
  scale_fill_gradientn(
    colours = c("#deebf7", "#3182bd"),
    limits = rango_normales2,
    oob = scales::squish,
    name = "Créditos",
  ) +
  
  labs(
    title = "Créditos otorgados por municipio",
    subtitle = "Impulso II. Período 2024-2025",
    caption = "Fuente: Dirección de Promoción y Desarrollo de Inversiones"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 10, hjust = 0, color = "gray60"),
    plot.margin = margin(10, 10, 100, 10)  # Top, right, bottom, left
  )

# Renderizas con girafe para que funcione la interactividad
girafe(
  ggobj = p2,
  options = list(
    opts_hover(css = "fill-opacity:1;stroke:black;stroke-width:2px;"),
    opts_hover_inv(css = "fill-opacity:0.2;")
  )
)

library(htmlwidgets)

# Crear el objeto interactivo
mapa_interactivo <- girafe(ggobj = p2, width_svg = 6, height_svg = 5)

# Guardar como archivo HTML
saveWidget(mapa_interactivo, file = "mapa_interactivo_impulso2.html", selfcontained = TRUE)

####################################################################################

# Crear versión estática del gráfico (sin interactividad)
p2_static <- ggplot() +
  geom_sf(
    data = normales2,
    aes(fill = Cantidad),
    color = "gray20"
  ) +
  geom_sf(
    data = filter(municipios_join2, grupo_monto == "outlier"),
    fill = "#e81f76", color = "gray20"
  ) +
  geom_sf(
    data = filter(municipios_join2, grupo_monto == "nulo"),
    fill = "#ffffff", color = "gray20"
  ) +
  scale_fill_gradientn(
    colours = c("#deebf7", "#3182bd"),
    limits = rango_normales2,
    oob = scales::squish,
    name = "Créditos"
  ) +
  labs(
    title = "Créditos otorgados por municipio",
    subtitle = "Impulso II. Período 2024-2025",
    caption = "Fuente: Dirección de Promoción y Desarrollo de Inversiones"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 10, hjust = 0, color = "gray60"),
    plot.margin = margin(10, 10, 100, 10)
  )


# Guardar como JPG (opcional)
ggsave("mapa_impulso2_static.jpg", plot = p2_static, width = 10, height = 8, dpi = 300)
