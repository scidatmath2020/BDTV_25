#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Gráfico de dispersión                        #
# Sesión: 09                                         #
# Fecha: 02/10/2025                                  #
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

# Carga de datos -----

Data <- read.csv("Bases de datos/ElectricCarData_Clean.csv")

# Revisión del tipo de dato en cada columna ----

str(Data)

# Generación de gráfico de dispersión ----

## Estructura -----

punto_max <- Data %>% 
  filter(Range_Km == max(Range_Km))

punto_max <- Data %>% 
  filter(Range_Km > 550)

punto_min <- Data %>% 
  filter(Range_Km == min(Range_Km)) %>% 
  slice_head(n = 1)

Data %>% 
  ggplot(aes(x = TopSpeed_KmH, y = Range_Km))+
  geom_smooth(method = "lm",
              se = FALSE,
              color = "#E7180B")+
  geom_text(data = punto_max,
            aes(label = paste(Brand, "-", Model)),
            vjust = -1, hjust = -0.1,
            color = "black", fontface = "bold")+
  annotate("segment",
           x = punto_max$TopSpeed_KmH+10,
           y = punto_max$Range_Km+10,
           xend =  punto_max$TopSpeed_KmH+2,
           yend = punto_max$Range_Km+2,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "black",
           size = 1.3)+
  geom_text(data = punto_min,
            aes(label = paste(Brand, "-", Model)),
            vjust = -1, hjust = -0.1,
            color = "black", fontface = "bold")+
  annotate("segment",
           x = punto_min$TopSpeed_KmH+10,
           y = punto_min$Range_Km+10,
           xend =  punto_min$TopSpeed_KmH+2,
           yend = punto_min$Range_Km+2,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "black",
           size = 1.3)+
  geom_point(shape = 17, size =3,
             color = "#1C69A8")+
  scale_x_continuous(breaks = seq(100,440,20),
                     limits = c(100,440))+
  scale_y_continuous(breaks = seq(50,1000,50),
                     limits = c(50,1000))


## Salida general ----

Fuente <- "Goudy Old Style"

Salida <- Data %>% 
  ggplot(aes(x = TopSpeed_KmH, y = Range_Km))+
  geom_smooth(method = "lm",
              se = FALSE,
              color = "#E7180B")+
  geom_text(data = punto_max,
            aes(label = paste(Brand, "-", Model, 
                              "\n",
                              "Vel.Máx:",
                              TopSpeed_KmH,
                              "Bat.:",
                              Range_Km)),
            vjust = -0.5, hjust = -0.1,
            color = "black", fontface = "bold",
            family = Fuente)+
  annotate("segment",
           x = punto_max$TopSpeed_KmH+10,
           y = punto_max$Range_Km+10,
           xend =  punto_max$TopSpeed_KmH+2,
           yend = punto_max$Range_Km+2,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "black",
           size = 1.3)+
  geom_text(data = punto_min,
            aes(label = paste(Brand, "-", Model, 
                              "\n",
                              "Vel.Máx:",
                              TopSpeed_KmH,
                              "Bat.:",
                              Range_Km)),
            vjust = -0.2, hjust = -0.2,
            color = "black", fontface = "bold",
            family = Fuente)+
  annotate("segment",
           x = punto_min$TopSpeed_KmH+10,
           y = punto_min$Range_Km+10,
           xend =  punto_min$TopSpeed_KmH+2,
           yend = punto_min$Range_Km+2,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "black",
           size = 1.3)+
  geom_point(shape = 17, size =3,
             color = "#1C69A8")+
  scale_x_continuous(breaks = seq(100,440,20),
                     limits = c(100,450))+
  scale_y_continuous(breaks = seq(50,1000,50),
                     limits = c(50,1050))+
  labs(
    title = "<b style = 'color:#1C69A8'> Vehículos eléctricos según su </b><b style = 'color:#E7180B'> velocidad máxima</b><b style = 'color:#1C69A8'> y </b><b style = 'color:#E7180B'> autonomía de batería</b>",
    subtitle = "<i style = 'color:#E7180B'> registros del 2024</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. Kaggle. EVs - One Electric Vehicle Dataser-Smaller. (2024)</span>",
    x = "<span style = 'color:#E7180B'> Velocidad máxima registrada</span>",
    y = "<span style = 'color:#E7180B'> Autonomía de la batería</span>"
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
        axis.title.y = element_markdown(size = 13,
                                        family = Fuente),
        axis.text = element_text(color = "#1C69A8",
                                 face = "bold",
                                 size = 13,
                                 family = Fuente),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = alpha("#7CB342",0.3),
                                        linetype = "dashed"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "#7CB342")) 


Salida <- ggdraw(Salida)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

Salida

ggsave(Salida,
       filename = "Salidas/Gráfico_dispersión1.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")



## Salida general 2 con leyenda (usamos otra variable más) ----

Fuente <- "Goudy Old Style"

Salida2 <- Data %>% 
  ggplot(aes(x = TopSpeed_KmH, y = Range_Km,
            color = PowerTrain))+
  geom_smooth(method = "lm",
              se = FALSE,
              color = "#E7180B")+
  geom_text(data = punto_max,
            aes(label = paste(Brand, "-", Model, 
                              "\n",
                              "Vel.Máx:",
                              TopSpeed_KmH,
                              "Bat.:",
                              Range_Km)),
            vjust = -0.5, hjust = -0.1,
            color = "black", fontface = "bold",
            family = Fuente)+
  annotate("segment",
           x = punto_max$TopSpeed_KmH+10,
           y = punto_max$Range_Km+10,
           xend =  punto_max$TopSpeed_KmH+2,
           yend = punto_max$Range_Km+2,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "black",
           size = 1.3)+
  geom_text(data = punto_min,
            aes(label = paste(Brand, "-", Model, 
                              "\n",
                              "Vel.Máx:",
                              TopSpeed_KmH,
                              "Bat.:",
                              Range_Km)),
            vjust = -0.2, hjust = -0.2,
            color = "black", fontface = "bold",
            family = Fuente)+
  annotate("segment",
           x = punto_min$TopSpeed_KmH+10,
           y = punto_min$Range_Km+10,
           xend =  punto_min$TopSpeed_KmH+2,
           yend = punto_min$Range_Km+2,
           arrow = arrow(length = unit(0.2, "cm")),
           color = "black",
           size = 1.3)+
  geom_point(shape = 17, size =3)+
  scale_x_continuous(breaks = seq(100,440,20),
                     limits = c(100,450))+
  scale_y_continuous(breaks = seq(50,1000,50),
                     limits = c(50,1050))+
  scale_color_manual(values = c(
    "AWD" = "#009485",
    "FWD" = "#940007",
    "RWD" = "#000F94"
  ))+
  labs(
    title = "<b style = 'color:#1C69A8'> Vehículos eléctricos según su </b><b style = 'color:#E7180B'> velocidad máxima</b><b style = 'color:#1C69A8'>, </b><b style = 'color:#E7180B'> autonomía de batería</b><b style = 'color:#1C69A8'> y</b><b style = 'color:#E7180B'> tipo de tracción</b>",
    subtitle = "<i style = 'color:#E7180B'> registros del 2024</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. Kaggle. EVs - One Electric Vehicle Dataser-Smaller. (2024)</span>",
    x = "<span style = 'color:#E7180B'> Velocidad máxima registrada</span>",
    y = "<span style = 'color:#E7180B'> Autonomía de la batería</span>",
    color = "<span style = 'color:#1C69A8'> Tipo de tracción:</span>"
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
        axis.title.y = element_markdown(size = 13,
                                        family = Fuente),
        axis.text = element_text(color = "#1C69A8",
                                 face = "bold",
                                 size = 13,
                                 family = Fuente),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = alpha("#7CB342",0.3),
                                        linetype = "dashed"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "#7CB342"),
        legend.position = "bottom",
        legend.text = element_markdown(size = 12,
                                       family = Fuente,
                                       color = "#1C69A8"),
        legend.title = element_markdown(size = 13,
                                        family = Fuente)) 


Salida2 <- ggdraw(Salida2)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

Salida2

ggsave(Salida2,
       filename = "Salidas/Gráfico_dispersión2.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")




