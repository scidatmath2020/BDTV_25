#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Gráfico de radar                             #
# Sesión: 12                                         #
# Fecha: 07/10/2025                                  #
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

# Instalación de ggradar (disponible solo el repo git) ----

remotes::install_github("ricardo-bion/ggradar")

# Llamado e instalación de paquetes ----

pacman::p_load(
  "tidyverse",
  "dplyr",
  "tseries",
  "readxl",
  "openxlsx",
  "haven",
  "foreign",
  "lubridate",
  "ggthemes",
  "cowplot",
  "png",
  "grid",
  "ggtext",
  "extrafont",
  "ggrepel",
  "ggradar"
)

# Carga de fuentes de Windows (linux, MacOS) ----

loadfonts(device = "win")
fonts()

# Carga de datos -----

Hojas <- excel_sheets("Bases de datos/Accidentes de tránsito por tipo a nivel entidad y región 2024.xlsx")

Data <- read_excel("Bases de datos/Accidentes de tránsito por tipo a nivel entidad y región 2024.xlsx",
                   sheet = "Datos por entidad")

# Revisión del tipo de dato en cada columna ----

str(Data)

# Transformar datos a manera de poder gráficar ----

Data1 <- Data %>% 
  filter(!Entidades == "Nacional") %>% 
  select(-Entidades, -Total) %>% 
  group_by(Región) %>% 
  summarise(across(everything(), ~sum(.,na.rm = T))) %>% 
  ungroup() %>% 
  pivot_longer(-Región, names_to = 'Clase', values_to = "Accidentes") %>% 
  pivot_wider(names_from = Región,
              values_from = Accidentes)


# Estructura básica con ggradar ----

Data1 %>% 
  ggradar(
    font.radar = "roboto",
    grid.label.size = 8,
    axis.label.size = 6,
    group.point.size = 3,
  )
  
# Uniendo elementos de tema y labs -----

Fuente <- "Goudy Old Style"

Salida <- {Data1 %>% 
  ggradar(
    font.radar = "Goudy Old Style",
    grid.label.size = 6,
    axis.label.size = 6,
    group.point.size = 3
  )+
  scale_colour_manual(
    values = c("Fatal" = "#007A74",
               "No fatal" = "#FF7C1F",
               "Solo daños" = "#00067A")
  )+
  labs(
    title = "<b style = 'color:#1C69A8'> Accidentes terrestres registrados </b><b style = 'color:#E7180B'> según clase</b><b style = 'color:#1C69A8'> y </b><b style = 'color:#E7180B'> por región </b>",
    subtitle = "<i style = 'color:#E7180B'> durante 2024</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Accidentes de vehículos terrestres. (2025)</span>",
    x = "",
    y = "",
    color = ""
  )+
  theme(plot.title = element_markdown(size = 16,
                                      family = Fuente,
                                      margin = margin(
                                        t = 0, r = 0, b = 0, l = -150
                                      )),
        plot.subtitle = element_markdown(size = 14,
                                         family = Fuente,
                                         margin = margin(
                                           t = 0, r = 0, b = 0, l = -150
                                         )),
        plot.caption = element_markdown(hjust = 0,
                                        size = 12,
                                        family = Fuente,
                                        margin = margin(
                                          t = 0, r = 0, b = 0, l = -150
                                        )),
        axis.text = element_text(color = "#1C69A8",
                                 face = "bold",
                                 size = 13,
                                 family = Fuente),
        legend.position = "bottom",
        legend.text = element_markdown(size = 12,
                                       family = Fuente,
                                       color = "#1C69A8"),
        plot.margin = margin(t = 10, r = 200, b = 10, l = 200))} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo2.png",
             x = 0.87,
             y = 0.94,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

ggsave(Salida,
       filename = "Salidas/Gráfico_radar.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")
