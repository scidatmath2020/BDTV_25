#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Gráfico lineal                               #
# Sesión: 08                                         #
# Fecha: 01/10/2025                                  #
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
  "extrafont"
)

# Carga de fuentes de Windows (linux, MacOS) ----

# Instalación de fuentes (solo se hace una vez en la versión R) 

font_import()

# Llamado de fuentes 

loadfonts(device = "win")
fonts()

# Carga de datos ----

Hojas <- excel_sheets("Bases de datos/Ejemplo_lineal.xlsx")

Data <- read_excel("Bases de datos/Ejemplo_lineal.xlsx",
                   sheet = "Datos")

# Revisar el tipo de variables del dataframe ----

str(Data)

# Crear una variable temporal ----

Data <- Data %>% 
  mutate(Año2 = seq(as.Date("2011-01-01"),
                    as.Date("2025-01-01"),
                    by = "12 month"))

# Gráficos de tiempo: usando variable de caracter -----

Data %>% 
  ggplot(aes(x = Año, y=Porcentaje, group = 1))+
  geom_line()


# Gráficos de tiempo: usando variable de fecha -----

Fuente <- "Goudy Old Style"

Data %>% 
  ggplot(aes(x = Año2, y=Porcentaje))+
  geom_line(color = "#1C69A8",
            linewidth = 2.5)+
  geom_point(size = 4,
             color = "#1C69A8")+
  geom_text(aes(label = round(Porcentaje, 1)),
            vjust = -1,
            family = Fuente)+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(limits = c(0, 100))+
  labs(
    title = "<b style = 'color:#E7180B'> Población de 18 años y más</b><b style = 'color:#1C69A8'> que confia en los jueces</b>",
    subtitle = "<i style = 'color:#E7180B'> del 2011 al 2024</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Encuesta Nacional de Segurdad Urbana (ENSU). 2025.</span>",
    x = "<span style = 'color:#E7180B'> Año</span>",
    y = ""
  )+
  theme(plot.title = element_markdown(size = 16,
                                      family = Fuente),
        plot.subtitle = element_markdown(size = 14,
                                         family = Fuente),
        plot.caption = element_markdown(hjust = 0,
                                        size = 12,
                                        family = Fuente),
        axis.title.x = element_markdown(size = 13,
                                        family = Fuente),
        axis.text = element_text(color = "#1C69A8",
                                 face = "bold",
                                 size = 13,
                                 family = Fuente),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = alpha("#7CB342",0.5),
                                          linetype = "dashed"),
        panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "#7CB342")) 


Salida <- ggdraw(
  Data %>% 
    ggplot(aes(x = Año2, y=Porcentaje))+
    geom_line(color = "#1C69A8",
              linewidth = 2.5)+
    geom_point(size = 4,
               color = "#1C69A8")+
    geom_text(aes(label = round(Porcentaje, 1)),
              vjust = -1,
              family = Fuente)+
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y")+
    scale_y_continuous(limits = c(0, 100))+
    labs(
      title = "<b style = 'color:#E7180B'> Población de 18 años y más</b><b style = 'color:#1C69A8'> que confia en los jueces</b>",
      subtitle = "<i style = 'color:#E7180B'> del 2011 al 2024</i>",
      caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Encuesta Nacional de Segurdad Urbana (ENSU). 2025.</span>",
      x = "<span style = 'color:#E7180B'> Año</span>",
      y = ""
    )+
    theme(plot.title = element_markdown(size = 16,
                                        family = Fuente),
          plot.subtitle = element_markdown(size = 14,
                                           family = Fuente),
          plot.caption = element_markdown(hjust = 0,
                                          size = 12,
                                          family = Fuente),
          axis.title.x = element_markdown(size = 13,
                                          family = Fuente),
          axis.text = element_text(color = "#1C69A8",
                                   face = "bold",
                                   size = 13,
                                   family = Fuente),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = alpha("#7CB342",0.5),
                                            linetype = "dashed"),
          panel.background = element_rect(fill = "white"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(color = "#7CB342")) 
)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 


ggsave(Salida,
       filename = "Salidas/Gráfico_lineal.png",
       width = 10,
       height = 5,
       dpi = 500,
       units = "in")




