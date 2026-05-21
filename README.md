# Mapas Interactivos – Programa Impulso al Comercio Bonaerense

Este repositorio contiene visualizaciones interactivas de los créditos otorgados por el programa **Impulso al Comercio Bonaerense** y el programa **Emprendé tu Comercio** en diferentes etapas.

---

## 📍 Mapas disponibles

- [Impulso 1 (2023)](https://matiaspiccolo.github.io/Impulso/mapa_general_interactivo_impulso1.html)
- [Impulso 2 (2024-2025)](https://matiaspiccolo.github.io/Impulso/mapa_general_interactivo_impulso2.html)
- [Impulso 3 (2025-2026)](https://matiaspiccolo.github.io/Impulso/mapa_general_interactivo_impulso3b.html)
- [Emprendé 1 (2024)](https://matiaspiccolo.github.io/Impulso/mapa_general_interactivo_emprende1.html)
- [Emprendé 2 (2025)](https://matiaspiccolo.github.io/Impulso/mapa_general_interactivo_emprende2.html)
- [IMPULSO](https://matiaspiccolo.github.com/Impulso/mapa_general_interactivo_IMPULSO.html)
- [EMPRENDE](https://matiaspiccolo.github.com/Impulso/mapa_general_interactivo_EMPRENDE_total.html)
---

## 📂 Archivos incluidos

- `Impulso.xlsx`: Base de datos de créditos (usada en ambos mapas)
- `impulso1.R`: Script R para procesar y visualizar los datos de 2023
- `impulso2.R`: Script R para procesar y visualizar los datos de 2024 y 2025
- `mapa_interactivo_impulso1.html`: Mapa interactivo de 2023
- `mapa_interactivo_impulso2.html`: Mapa interactivo de 2024 y 2025

---

## 📦 Requisitos para correr los scripts

Paquetes necesarios en R:

```r
install.packages(c("sf", "readxl", "tmap", "ggplot2", "ggiraph", "patchwork", "RColorBrewer", "scales", "tidyverse"))
