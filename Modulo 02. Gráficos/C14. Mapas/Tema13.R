#==================================================================#
# Diplomado: Bases de datos y ténicas de graficación               #
# Modulo: Gráficos                                                 #
# Tema: Mapas usando capas                                         #
# Código de R                                                      #
# Fecha: Octubre 2025                                              #
# Material complementario                                          #
# Elaboro: Alexis Adonai Morales Alberto                           #
# SciData                                                          #
#==================================================================#

# Borrar objetos en la consola ----

rm(list = ls())

# Verificación de la existencia de pacman ----

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado")
} else {
  install.packages("pacman", dependencies = T)
}

# Paqueterías a utilizar ----

pacman::p_load(
  "tidyverse",
  "dplyr",
  "readxl",
  "openxlsx",
  "haven",
  "foreign",
  "ggthemes",
  "cowplot",
  "png",
  "grid",
  "ggtext",
  "extrafont",
  "sf"
)

# Cargar el mapa ----

Mapa <- read_sf("Mapas/mg_2024_integrado/conjunto_de_datos/00ent.shp")

# Cargar puntos de los sismos ----

Sismos <- read.csv("Bases de datos/Sismos_2000_2025.csv", skip = 4)
Sismos <- Sismos[1:(dim(Sismos)[1]-7),]

## Transformar columna de magnitud en númerico ----

Sismos <- Sismos %>% 
  mutate(Magnitud = as.numeric(Magnitud))

# Generar capa de sismos mayores de 5.5 grados y menos de 200 km de referencia de localización ----

Sismos_ct <- Sismos %>% 
  filter(Magnitud >= 5.5) %>% 
  st_as_sf(., coords = c("Longitud", "Latitud"), crs = 4326) %>% 
  mutate(km_epicentro = str_extract(Referencia.de.localizacion ,
                                    "\\d+(?:\\.\\d+)?(?=\\s*km)")) %>% 
  filter(km_epicentro < 200)

# Estructura incial ---

ggplot()+
  geom_sf(data = Mapa)+
  geom_sf(data = Sismos_ct, 
          aes(size = Magnitud))

# Estructura con etiqueas por cuartiles ----

## Construir saltos ----

Sismos_qt <- quantile(Sismos_ct$Magnitud,
                      c(0, 0.25, 0.5, 0.75, 1))

Sismos_ct <- Sismos_ct %>% 
  mutate(Magnitud_qt = cut(Magnitud,
                           breaks = Sismos_qt,
                           include.lowest = T,
                           dig.lab = 3))

## Construir etiquetas ----

unique(Sismos_ct$Magnitud_qt)

Etiquetas <- paste0(
  sprintf("%.1f", Sismos_qt)[1:4],
  " - ",
  sprintf("%.1f", Sismos_qt)[2:5]
)

## Procesar gráfico ----

Fuente <- "sans"

Salida <- {ggplot()+
  geom_sf(data = Mapa,
          fill = "#E0E7FF",
          color = "black")+
  geom_sf(data = Sismos_ct, 
          aes(size = Magnitud_qt),
          shape = 21,        
          color = "black",   
          fill = "red4")+
  scale_size_discrete(label = Etiquetas)+
  labs(
    title = "<b style = 'color:#1C69A8'> Sismos de mágnitud </b><b style = 'color:#E7180B'> mayores a 5.5 grados Richter</b><br><b style = 'color:#1C69A8'> y que tengan menos de </b><b style = 'color:#E7180B'> 200 km de distancia al punto de referencia </b>",
    subtitle = "<i style = 'color:#E7180B'> a nivel nacional del 2000 al 2025 </i>",
    caption = "<span style = 'color:#E7180B'> Fuente. Servicio Sismológico Nacional (2025)</span>",
    x = "",
    y = "",
    size = "<b style = 'color:#1C69A8'> Escala<br>Richter</b>"
  )+
  theme(plot.title = element_markdown(size = 16,
                                      family = Fuente,
                                      hjust = 0,
                                      margin = margin(
                                        t = 0, r = 0, b = 0, l = -100
                                      )),
        plot.subtitle = element_markdown(size = 14,
                                         family = Fuente,
                                         hjust = 0,
                                         margin = margin(
                                           t = 0, r = 0, b = 0, l = -100
                                         )),
        plot.caption = element_markdown(hjust = 0,
                                        size = 12,
                                        family = Fuente,
                                        margin = margin(
                                          t = 0, r = 0, b = 0, l = -100
                                        )),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.05, 0.05),
        legend.justification = c(0, 0),
        legend.background = element_rect(fill = "white"),
        legend.title = element_markdown(size = 12,
                                        family = Fuente),
        legend.text = element_markdown(size = 12,
                                       family = Fuente,
                                       face = "bold",
                                       color = "#1C69A8"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 10, r = 100, b = 10, l = 150))} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo.png",
             x = 0.87,
             y = 0.92,
             hjust = 0.5, 
             vjust = 0.5,
             width = 0.09) 

Salida

ggsave(Salida,
       filename = "Salidas/Mapa_2_capas.png",
       width = 13,
       height = 7,
       dpi = 500,
       units = "in")
