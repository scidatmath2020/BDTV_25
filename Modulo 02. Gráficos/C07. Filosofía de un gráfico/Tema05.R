#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Filosofía de un gráfico                      #
# Sesión: 07                                         #
# Fecha: 30/09/2025                                  #
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
  "grid"
)

# Importación de los datos para este ejemplo -----

# NOTA: Por lo regular los formatos para gráficos son largos
# sin embargo no todos los gráficos ocupan la estructura de
# esta sesión, el ejemplo es para entender la filosofía 
# de un gráfico 

Data <- read_excel("Bases de datos/Ejemplo_base.xlsx")

## Verificar los formatos de las variables ----

str(Data)

## Seleccionar 5 más altos datos ----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5)

## En caso de querer 5 más bajos ----

Data %>% 
  arrange(Percepción_percent) %>% 
  slice_head(n = 5)


# Creación de un gráfico -----

## Indicar el uso de los datos -----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot()

## Asociar la variables con los ejes -----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = Espacio, y = Percepción_percent))


## Ordenar los elementos de X (ordenadas) -----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))

## Agregar geometría (en este caso geom_col) -----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))+
  geom_col()

## Añadir colores a las barras -----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))+
  geom_col(fill = "#1D8348")


## Añadir etiquetas en la barra y configurar posición vertical ----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))+
  geom_col(fill = "#1D8348")+
  geom_text(aes(label = round(Percepción_percent,1),
                vjust = -1))

## Modificar limites del eje Y para que salgan bien los textos -----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))+
  geom_col(fill = "#1D8348")+
  geom_text(aes(label = round(Percepción_percent,1),
                vjust = -1))+
  scale_y_continuous(limits = c(0,90))

## Agregar titulos ----

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))+
  geom_col(fill = "#1D8348")+
  geom_text(aes(label = round(Percepción_percent,1),
                vjust = -1))+
  scale_y_continuous(limits = c(0,90))+
  labs(title = "Porcentaje de percepción de seguridad según espacio",
       subtitle = "personas mayores de 18 años",
       caption = "Fuente. INEGI. Encuesta Nacional de Seguridad Urbana. (ENSU), 2025.",
       x = "",
       y = "")

## Modificación del tema ---- 

Data %>% 
  arrange(desc(Percepción_percent)) %>% 
  slice_head(n = 5) %>% 
  ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
             y = Percepción_percent))+
  geom_col(fill = "#1D8348")+
  geom_text(aes(label = round(Percepción_percent,1),
                vjust = -1),
            size = 5)+
  scale_y_continuous(limits = c(0,90))+
  labs(title = "Porcentaje de percepción de seguridad según espacio",
       subtitle = "personas mayores de 18 años",
       caption = "Fuente. INEGI. Encuesta Nacional de Seguridad Urbana. (ENSU), 2025.",
       x = "",
       y = "")+
  theme(plot.title = element_text(hjust = 0,
                                  face = "bold.italic",
                                  size = 14),
        plot.subtitle = element_text(hjust = 0,
                                     face = "italic",
                                     size = 13),
        plot.caption = element_text(hjust = 0,
                                    face = "plain",
                                    size = 11),
        axis.text.x = element_text(size = 12,
                                   face = "bold",
                                   color = "black"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = alpha("#83481D",.4)))

## Extra: Añadir logo ----

logo <- readPNG("Imagenes/logo2.png")
logo_grob <- rasterGrob(logo, interpolate = T)

ggdraw(Data %>% 
         arrange(desc(Percepción_percent)) %>% 
         slice_head(n = 5) %>% 
         ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
                    y = Percepción_percent))+
         geom_col(fill = "#1D8348")+
         geom_text(aes(label = round(Percepción_percent,1),
                       vjust = -1),
                   size = 5)+
         scale_y_continuous(limits = c(0,90))+
         labs(title = "Porcentaje de percepción de seguridad según espacio",
              subtitle = "personas mayores de 18 años",
              caption = "Fuente. INEGI. Encuesta Nacional de Seguridad Urbana. (ENSU), 2025.",
              x = "",
              y = "")+
         theme(plot.title = element_text(hjust = 0,
                                         face = "bold.italic",
                                         size = 14),
               plot.subtitle = element_text(hjust = 0,
                                            face = "italic",
                                            size = 13),
               plot.caption = element_text(hjust = 0,
                                           face = "plain",
                                           size = 11),
               axis.text.x = element_text(size = 12,
                                          face = "bold",
                                          color = "black"),
               panel.grid = element_blank(),
               axis.ticks = element_blank(),
               axis.text.y = element_blank(),
               panel.background = element_blank(),
               plot.background = element_rect(fill = alpha("#83481D",.4))))+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2)

## Salida final ----

Salida_final <- ggdraw(
  Data %>% 
    arrange(desc(Percepción_percent)) %>% 
    slice_head(n = 5) %>% 
    ggplot(aes(x = reorder(Espacio, -Percepción_percent), 
               y = Percepción_percent))+
    geom_col(fill = "#1D8348")+
    geom_text(aes(label = round(Percepción_percent,1),
                  vjust = -1),
              size = 5)+
    scale_y_continuous(limits = c(0,90))+
    labs(title = "Porcentaje de percepción de seguridad según espacio",
         subtitle = "personas mayores de 18 años",
         caption = "Fuente. INEGI. Encuesta Nacional de Seguridad Urbana. (ENSU), 2025.",
         x = "",
         y = "")+
    theme(plot.title = element_text(hjust = 0,
                                    face = "bold.italic",
                                    size = 14),
          plot.subtitle = element_text(hjust = 0,
                                       face = "italic",
                                       size = 13),
          plot.caption = element_text(hjust = 0,
                                      face = "plain",
                                      size = 11),
          axis.text.x = element_text(size = 12,
                                     face = "bold",
                                     color = "black"),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = alpha("#83481D",.4)))
)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2)

## Exportamos ----

ggsave(Salida_final,
       filename = "Salidas/Gráfico1.png",
       width = 10,
       height = 7,
       dpi = 500,
       units = "in")

