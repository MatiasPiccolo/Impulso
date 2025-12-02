library(readxl)
library(sf)
library(tidyverse)
library(tmap)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(patchwork)
library(stringr)

# Leemos las bases de datos
municipios2 <- st_read("C:/Users/Matías Piccolo/Desktop/R/Mapas/limites-partidos/shp")
impulso2 <- read_excel("C:/Users/Matías Piccolo/Desktop/R/Mapas/Impulso.xlsx", sheet = "2023")

# Transformo nombres a mayúsculas
municipios2 <- municipios2 %>% mutate(nam = toupper(nam))

# Uno los datos
municipios_join2 <- municipios2 %>%
  left_join(impulso2, by = c("nam" = "Localidad"))

# Eliminar municipios sin datos
municipios_join2 <- municipios_join2 %>% filter(!is.na(Monto))

####################################################################################

# Identificar outliers y nulos
outliers <- c("GENERAL PUEYRREDÓN", "BAHÍA BLANCA", "LA PLATA")
nulos <- c("COLÓN","GENERAL LAVALLE","JOSÉ C. PAZ","MAGDALENA","MAR CHIQUITA",
           "SAN ANTONIO DE ARECO", "CASTELLI", "LAPRIDA","HIPÓLITO YRIGOYEN", "TORDILLO")

municipios_join2 <- municipios_join2 %>%
  mutate(grupo_monto = case_when(
    nam %in% outliers ~ "outlier",
    nam %in% nulos ~ "nulo",
    TRUE ~ "normal"
  ))

####################################################################################

# Municipios del AMBA
amba_partidos <- c(
  "ALMIRANTE BROWN","AVELLANEDA","BERAZATEGUI","BERISSO","BRANDSEN","CAMPANA","CAÑUELAS",
  "ENSENADA","ESCOBAR","ESTEBAN ECHEVERRÍA","EXALTACIÓN DE LA CRUZ","EZEIZA","FLORENCIO VARELA",
  "GENERAL LAS HERAS","GENERAL SAN MARTÍN","GENERAL RODRÍGUEZ","HURLINGHAM","ITUZAINGÓ",
  "JOSÉ C. PAZ","LA MATANZA","LA PLATA","LANÚS","LOMAS DE ZAMORA","LUJÁN","MALVINAS ARGENTINAS",
  "MARCOS PAZ","MERLO","MORENO","MORÓN","QUILMES","PILAR","PRESIDENTE PERÓN","SAN FERNANDO",
  "SAN ISIDRO","SAN MIGUEL","SAN VICENTE","TIGRE","TRES DE FEBRERO","VICENTE LÓPEZ","ZÁRATE"
)

municipios_join2 <- municipios_join2 %>%
  mutate(region = if_else(nam %in% amba_partidos, "AMBA", nam))

normales2 <- municipios_join2 %>% filter(grupo_monto == "normal")

# Rango para la escala
rango_normales_monto2 <- range(normales2$Monto, na.rm = TRUE)

# Etiqueta numérica original
municipios_join2 <- municipios_join2 %>%
  mutate(etiqueta = scales::comma(Monto, big.mark = ".", decimal.mark = ","))

####################################################################################
# AUTO-PARTIR NOMBRES EN VARIAS LÍNEAS

wrap_label <- function(x, width = 10) {
  stringr::str_wrap(x, width = width)
}

municipios_join2 <- municipios_join2 %>%
  mutate(etiqueta_wrapped = wrap_label(nam, width = 10))

####################################################################################
# MAPA GENERAL
ggplot() +
  geom_sf(data = normales2, aes(fill = Monto), color = "gray20") +
  geom_sf(data = filter(municipios_join2, grupo_monto == "outlier"),
          fill = "#e81f76", color = "gray20") +
  geom_sf(data = filter(municipios_join2, grupo_monto == "nulo"),
          fill = "#ffffff", color = "gray20") +
  
  # Etiquetas
  geom_sf_text(
    data = filter(municipios_join2,
                  grupo_monto %in% c("outlier","normal","nulo"),
                  region != "AMBA"),
    aes(label = etiqueta_wrapped),
    size = 1.5,
    lineheight = 0.9,
    color = "black",
    fontface = "bold"
  ) +
  
  scale_fill_gradientn(
    colours = c("#deebf7", "#3182bd"),
    limits = rango_normales_monto2,
    oob = scales::squish,
    name = NULL,
    labels = label_number(prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 0.01)
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = NA, color = NA)
  )

ggsave("Montos_Impulso1.2_mapa_general.png", width = 8, height = 9, bg = "transparent")

################################################################################


################################################################################
# MAPA SOLO AMBA (NÚMEROS ORDENADOS POR MONTO, INCLUYE LOS DE MONT0 = 0)
################################################################################

# Filtrar AMBA
amba_mapa <- municipios_join2 %>% filter(region == "AMBA")

# Crear ranking (incluye montos = 0)
ranking_montos <- amba_mapa %>%
  st_drop_geometry() %>%
  select(nam, Monto) %>%
  arrange(desc(Monto)) %>%
  mutate(numero = row_number())

# Unir SOLO la columna numero (para evitar Monto.x / Monto.y)
amba_mapa <- amba_mapa %>%
  left_join(ranking_montos %>% select(nam, numero), by = "nam")

# ---------------------------
# MAPA
# ---------------------------

ggplot() +
  geom_sf(data = filter(amba_mapa, grupo_monto == "normal"),
          aes(fill = Monto), color = "gray20") +
  
  geom_sf(data = filter(amba_mapa, grupo_monto == "outlier"),
          fill = "#e81f76", color = "gray20") +
  
  geom_sf(data = filter(amba_mapa, grupo_monto == "nulo"),
          fill = "#ffffff", color = "gray20") +
  
  geom_sf_text(
    data = amba_mapa,
    aes(label = numero),
    size = 5,
    color = "black"
  ) +
  
  scale_fill_gradientn(
    colours = c("#deebf7", "#3182bd"),
    limits = rango_normales_monto2,
    oob = scales::squish,
    name = "Monto"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggsave("Montos_Impulso1_AMBA_numerado.png",
       width = 8, height = 9, bg = "transparent")

################################################################################
# TABLA ORDENADA POR MONTO (POR LAS DUDAS)
################################################################################

amba_mapa %>% 
  st_drop_geometry() %>%
  select(numero, nam, Monto) %>% 
  arrange(numero)

library(htmlwidgets)
library(ggiraph)

# ===========================
# MAPA GENERAL INTERACTIVO
# ===========================

p_general <- ggplot() +
  
  # Municipios normales
  geom_sf_interactive(
    data = normales2,
    aes(
      fill = Monto,
      data_id = nam,
      tooltip = paste0(
        "<b>", nam, "</b>",
        "\nSección: ", Seccion,
        "\nCréditos: ", Cantidad,
        "\nMonto: ", number(Monto, prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 0.01))),
    color = "gray20"
  ) +
  
  # Municipios outlier
  geom_sf_interactive(
    data = filter(municipios_join2, grupo_monto == "outlier"),
    aes(
      data_id = nam,
      tooltip = paste0(
        "<b>", nam, "</b>",
        "\nSección: ", Seccion,
        "\nCréditos: ", Cantidad,
        "\nMonto: ", number(Monto, prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 0.01)
      )
    ),
    fill = "#e81f76", color = "gray20"
  ) +
  
  # Municipios nulos
  geom_sf_interactive(
    data = filter(municipios_join2, grupo_monto == "nulo"),
    aes(
      data_id = nam,
      tooltip = paste0(
        "<b>", nam, "</b>",
        "\nSección: ", Seccion,
        "\nCréditos: ", Cantidad,
        "\nMonto: ", number(Monto, prefix = "$", big.mark = ".", decimal.mark = ",", accuracy = 0.01)
      )
    ),
    fill = "#ffffff", color = "gray20"
  ) +
  
  
  # Escala idéntica al mapa estático
  scale_fill_gradientn(
    colours = c("#deebf7", "#3182bd"),
    limits = rango_normales_monto2,
    oob = scales::squish,
    name = "Monto\n(en millones de $)",
    labels = label_number(prefix = "$", big.mark = ".", decimal.mark = ",")
  ) +
  
  
  labs(
    title = "Crédito Impulso al Comercio Bonaerense",
    subtitle = "Etapa 1 (2023). Por monto, en millones de pesos.",
    caption = "Fuente: Dirección de Promoción y Desarrollo de Inversiones"
  ) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.4, "cm"),
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = NA, color = NA),
    plot.subtitle = element_text(size = 8),   # <<--- tamaño más chico
    plot.title = element_text(size = 15, hjust = 0),
    plot.caption = element_text(size = 6, hjust = 0)
  )

# ==== Interactividad ====

mapa_interactivo_general <- girafe(
  ggobj = p_general,
  options = list(
    opts_hover(css = "fill-opacity:1;stroke:black;stroke-width:2px;"),
    opts_hover_inv(css = "fill-opacity:0.2;")
  )
)

# Mostrar en el visor
mapa_interactivo_general

# ===== Guardar en HTML =====
saveWidget(
  mapa_interactivo_general,
  file = "mapa_general_interactivo_impulso1.html",
  selfcontained = TRUE
)
