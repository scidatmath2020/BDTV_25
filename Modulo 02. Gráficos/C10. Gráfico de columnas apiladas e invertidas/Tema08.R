#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Gráfico de barras apiladas e invertidas      #
# Sesión: 10                                         #
# Fecha: 03/10/2025                                  #
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

loadfonts(device = "win")
fonts()

# Definir fuente para gráfico ----

Fuente <- "Goudy Old Style"

# Tema para gráfico de columnas (barras) apiladas ----

Tema1 <- theme(plot.title = element_markdown(size = 16,
                                             family = Fuente),
               plot.subtitle = element_markdown(size = 14,
                                                family = Fuente),
               plot.caption = element_markdown(hjust = 0,
                                               size = 12,
                                               family = Fuente),
               axis.title.x = element_markdown(size = 13,
                                               family = Fuente),
               axis.text.x = element_text(color = "#1C69A8",
                                        face = "bold",
                                        size = 13,
                                        family = Fuente,
                                        angle = 90,
                                        hjust = 1,
                                        vjust = -0.1),
               axis.text.y = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               axis.ticks.x = element_blank(),
               axis.ticks.y = element_blank(),
               legend.position = "bottom",
               legend.text = element_markdown(size = 12,
                                              family = Fuente,
                                              color = "#1C69A8"),
               legend.title = element_markdown(size = 13,
                                               family = Fuente))


# Tema para gráfico de columnas (barras) invertidas ----

Tema2 <- theme(plot.title = element_markdown(size = 15,
                                             family = Fuente,
                                             margin = margin(
                                               t = 0, r = 0, b = 0, l = -150
                                             )),
               plot.subtitle = element_markdown(size = 13,
                                                family = Fuente,
                                                margin = margin(
                                                  t = 0, r = 0, b = 0, l = -150
                                                )),
               plot.caption = element_markdown(hjust = 0,
                                               size = 11,
                                               family = Fuente,
                                               margin = margin(
                                                 t = 0, r = 0, b = 0, l = -150
                                               )),
               axis.title.x = element_markdown(size = 12,
                                               family = Fuente),
               axis.text.y = element_text(color = "#1C69A8",
                                          face = "bold",
                                          size = 12,
                                          family = Fuente),
               axis.text.x = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               panel.grid.major.y = element_blank(),
               panel.background = element_rect(fill = "white"),
               axis.ticks.x = element_blank(),
               axis.ticks.y = element_blank(),
               legend.position = "bottom",
               legend.text = element_markdown(size = 11,
                                              family = Fuente,
                                              color = "#1C69A8"),
               legend.title = element_markdown(size = 12,
                                               family = Fuente),
               legend.margin = margin(
                 t = 0, r = 0, b = 0, l = -100
               ))


# Importación de los datos ----

Data <- read_excel("Bases de datos/Ingresos_penales.xlsx")

# Ordenar entidades federativas como factor (de menor a mayor) ----

Entidades <- Data %>% 
  filter(!Entidad == "Estados Unidos Mexicanos") %>% 
  arrange(desc(Total)) %>% 
  pull(Entidad)

Entidades <- c( "Estados Unidos Mexicanos",
                Entidades)

Data$Entidad <- factor(Data$Entidad,
                       level = Entidades)


# Convertir en porcentajes Hombres y Mujeres ----

Data <- Data %>% 
  mutate(across(c(
    "Hombres",
    "Mujeres"
  ),~./Total))

# Estructura básica del gráfico de columnas apiladas ----

Salida <- {Data %>% 
  pivot_longer(cols = c(Hombres, Mujeres),
               names_to = "Sexo",
               values_to = "Porcentaje") %>% 
  mutate(Sexo = factor(Sexo, levels = c("Mujeres", "Hombres"))) %>% 
  ggplot(aes(x = Entidad, y = Porcentaje,
             fill = Sexo))+
  geom_col()+
  geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1,
                                        suffix= " ")),
            position = position_stack(vjust = 0.5),
            family = Fuente,
            fontface = "bold",
            size = 3.5)+
  scale_y_continuous(limits = c(0,1.15))+
  geom_text(aes(x = Entidad, y = 1,
                label = scales::comma(Total, big.mark = " ")),
            angle = 90,
            hjust = -0.1,
            family = Fuente,
            fontface = "bold",
            size = 3.5)+
  scale_fill_manual(values = c(
    "Mujeres" = "#9C6DB0",
    "Hombres"  = "#81B06D"
  ))+
  labs(
    title = "<b style = 'color:#1C69A8'> Personas que ingresan a los </b><b style = 'color:#E7180B'> centros penitenciarios</b><b style = 'color:#1C69A8'> según su </b><b style = 'color:#E7180B'> sexo</b><b style = 'color:#1C69A8'> por</b><b style = 'color:#E7180B'> entidad federativa</b>",
    subtitle = "<i style = 'color:#E7180B'> durante el año 2024 <br> porcentajes y absolutos </i>",
    caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Censo Nacional de Sistemas Penitenciarios Estatales, (2025)</span>",
    x = "",
    y = "",
    fill = "<span style = 'color:#1C69A8'> Sexo:</span>"
  )+
  Tema1} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

Salida  

ggsave(Salida,
       filename = "Salidas/Gráfico_columnas_apiladas.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")



# Estructura básica del gráfico de columnas apiladas invertidas ----

Salida2 <- {Data %>% 
    pivot_longer(cols = c(Hombres, Mujeres),
                 names_to = "Sexo",
                 values_to = "Porcentaje") %>% 
    mutate(Sexo = factor(Sexo, levels = c("Mujeres", "Hombres"))) %>% 
    ggplot(aes(x = reorder(Entidad, Total), y = Porcentaje,
               fill = Sexo))+
    geom_col()+
    geom_text(aes(label = scales::percent(Porcentaje, accuracy = 0.1,
                                          suffix= " ")),
              position = position_stack(vjust = 0.5),
              family = Fuente,
              fontface = "bold",
              size = 3.5)+
    scale_y_continuous(limits = c(0,1.15))+
    geom_text(aes(x = Entidad, y = 1,
                  label = scales::comma(Total, big.mark = " ")),
              family = Fuente,
              fontface = "bold",
              size = 3.5,
              hjust = -0.1)+
    scale_fill_manual(values = c(
      "Mujeres" = "#9C6DB0",
      "Hombres"  = "#81B06D"
    ))+
    coord_flip()+
    labs(
      title = "<b style = 'color:#1C69A8'> Personas que ingresan a los </b><b style = 'color:#E7180B'> centros penitenciarios</b><b style = 'color:#1C69A8'> según su </b><b style = 'color:#E7180B'> sexo</b><b style = 'color:#1C69A8'> por</b><b style = 'color:#E7180B'> entidad federativa</b>",
      subtitle = "<i style = 'color:#E7180B'> durante el año 2024 <br> porcentajes y absolutos </i>",
      caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Censo Nacional de Sistemas Penitenciarios Estatales, (2025)</span>",
      x = "",
      y = "",
      fill = "<span style = 'color:#1C69A8'> Sexo:</span>"
    )+
    Tema2} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.97,
             hjust = 0.7, vjust = 0.5,
             width = 0.12) 

Salida2  

ggsave(Salida2,
       filename = "Salidas/Gráfico_columnas_apiladas_invertidas.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")
