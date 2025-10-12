#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Mapas temáticos                              #
# Sesión: 14                                         #
# Fecha: 09/10/2025                                  #
# Instructor: Alexis Adonai Morales Albero           #
# SciData                                            #
#====================================================#

# Limpieza inicial de consola ----

rm(list = ls())

# Condicional de existencia de pacman ----

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado")
} else{
  install.packages("pacman", dependencies = T)
}


# Llamado e instalación de paquetes ----

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

# Carga de fuentes de Windows (linux, MacOS) ----

loadfonts(device = "win")
fonts()

# Carga del mapa ----

Mapa_mex <- read_sf("Mapas/Mexico_Entidades/00ent.shp")

str(Mapa_mex)

# Carga de datos (los que se van a unir al mapa) ----

Accidentes <- read_excel("Bases de datos/Accidentes de tránsito por tipo a nivel entidad y región 2024.xlsx",
                         sheet = "Datos por entidad")

# Unir los accidentes al mapa ----

Map_data <- Mapa_mex %>% 
  left_join(Accidentes,
            by = c("NOMGEO" = "Entidades"))

class(Map_data)

# Estructura general (cuando el dato es continuo) ----

Map_data %>% 
  ggplot()+
  geom_sf(aes(fill = Total))

# Estructura general (cuando el dato esta en quintiles ) -----

## Vamos a crear la columa de quintiles dentro del mapa -----

Quintiles <- quantile(Map_data$Total,
                      c(0, 0.2, 0.4, 0.6, 0.8, 1))

Rangos <- paste0(
  scales::comma(Quintiles[1:5]),
  " - ",
  scales::comma(Quintiles[2:6])
)

Map_data <- Map_data %>% 
  mutate(Quintil = cut(x = Total,
                       breaks = Quintiles,
                       diag.lab = 3,
                       include.lowest = T))

unique(Map_data$Quintil)

## Se realiza gráfico ----

Map_data %>% 
  ggplot()+
  geom_sf(aes(fill = Quintil),
          color = "black")+
  scale_fill_manual(
    values = c(
      "[1.8e+03,4.38e+03]" = "#3B688C",
      "(9.92e+03,1.54e+04]" = "#3B8C5F",
      "(4.38e+03,6.69e+03]" = "#8C5F3B",
      "(1.54e+04,7.62e+04]" = "#8C3B68",
      "(6.69e+03,9.92e+03]" = "#8C3B3F"
    ),
    labels = Rangos
  )

# Estrcutra con tema y titulos ------

Fuente <- "Montserrat"

Salida <- {Map_data %>% 
  ggplot()+
  geom_sf(aes(fill = Quintil),
          color = "black")+
  scale_fill_manual(
    values = c(
      "[1.8e+03,4.38e+03]" = "#3B688C",
      "(9.92e+03,1.54e+04]" = "#3B8C5F",
      "(4.38e+03,6.69e+03]" = "#8C5F3B",
      "(1.54e+04,7.62e+04]" = "#8C3B68",
      "(6.69e+03,9.92e+03]" = "#8C3B3F"
    ),
    labels = Rangos
  )+
  labs(
    title = "<b style = 'color:#1C69A8'> Número de </b><b style = 'color:#E7180B'> accidentes terrestres</b><b style = 'color:#1C69A8'> por </b><b style = 'color:#E7180B'> entidad federativa </b>",
    subtitle = "<i style = 'color:#E7180B'> registros durante el 2024 </i>",
    caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Accidentes  terrestres, (2025)</span>",
    x = "",
    y = "",
    fill = "<b style = 'color:#1C69A8'> Número de<br>accidentes</b>"
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
        plot.margin = margin(t = 10, r = 100, b = 10, l = 100))} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo2.png",
             x = 0.87,
             y = 0.92,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

Salida


ggsave(Salida,
       filename = "Salidas/Mapa_temático.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")
