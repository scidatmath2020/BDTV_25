#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Gráfico circular/tarta/pastel                #
# Sesión: 11                                         #
# Fecha: 06/10/2025                                  #
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
  "extrafont",
  "ggrepel"
)

# Carga de fuentes de Windows (linux, MacOS) ----

loadfonts(device = "win")
fonts()

# Carga de datos -----

Hojas <- excel_sheets("Bases de datos/Accidentes_2024.xlsx")

Data <- read_excel("Bases de datos/Accidentes_2024.xlsx",
                   sheet = "Datos")

# Revisión del tipo de dato en cada columna ----

str(Data)

# Estructura general del gráfico ----

Data %>% 
  filter(!Eventos == "Total de eventos (Absoluto)") %>% 
  mutate(Eventos = gsub("\\(Absoluto\\)", "", Eventos),
         Eventos = trimws(Eventos)) %>% 
  arrange(desc(Eventos)) %>% 
  ggplot(aes(x = "", y = Relativos, fill = Eventos))+
  geom_col(width = 1)+
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = c(
    "Fatal" = "#950797",
    "No fatal" = "#4D0797",
    "Solo daños" = "#070997"
  ))+
  geom_label_repel(aes(y = cumsum(Relativos) - Relativos/2, 
                       label = paste0(Eventos,
                                      "\n",
                                      scales::comma(
                                        Absolutos,
                                        big.mark = " "
                                      ),
                                      "\n",
                                      scales::percent(Relativos, 
                                                      accuracy = 0.1,
                                                      suffix = "%"))),
                   nudge_x = 0.7,
                   direction = "y",
                   hjust = 0.5,
                   segment.colour = "white",
                   segment.size = 0.6,
                   box.padding = 0.4,
                   point.padding = 0.3,
                   show.legend = F,
                   color = "white")


# Salida con tema y titulos ----

Fuente <- "Goudy Old Style"

Salida <- {Data %>% 
  filter(!Eventos == "Total de eventos (Absoluto)") %>% 
  mutate(Eventos = gsub("\\(Absoluto\\)", "", Eventos),
         Eventos = trimws(Eventos)) %>% 
  arrange(desc(Eventos)) %>% 
  ggplot(aes(x = "", y = Relativos, fill = Eventos))+
  geom_col(width = 1)+
  coord_polar(theta = "y", start = 0)+
  scale_fill_manual(values = c(
    "Fatal" = "#950797",
    "No fatal" = "#4D0797",
    "Solo daños" = "#070997"
  ))+
  geom_label_repel(aes(y = cumsum(Relativos) - Relativos/2, 
                       label = paste0(Eventos,
                                      "\n",
                                      scales::comma(
                                        Absolutos,
                                        big.mark = " "
                                      ),
                                      "\n",
                                      scales::percent(Relativos, 
                                                      accuracy = 0.1,
                                                      suffix = "%"))),
                   nudge_x = 0.7,
                   direction = "y",
                   hjust = 0.5,
                   segment.colour = "white",
                   segment.size = 1,
                   box.padding = 0.8,
                   point.padding = 0.7,
                   show.legend = F,
                   color = "white",
                   family = Fuente,
                   fontface = "bold",
                   size = 5)+
  labs(
    title = "<b style = 'color:#1C69A8'> Accidentes terrestres registrados por tipo de</b><b style = 'color:#E7180B'> clase</b><b style = 'color:#1C69A8'> a nivel nacional </b>",
    subtitle = "<i style = 'color:#E7180B'> durante 2024 <br>(absolutos y relativos)</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. INEGI. Accidentes de tránsito (2025)</span>",
    x = "",
    y = ""
  )+
  theme(plot.title = element_markdown(size = 17,
                                      family = Fuente,
                                      margin = margin(
                                        t = 0, r = 0, b = 0, l = -150
                                      )),
        plot.subtitle = element_markdown(size = 15,
                                         family = Fuente,
                                         margin = margin(
                                           t = 0, r = 0, b = 0, l = -150
                                         )),
        plot.caption = element_markdown(hjust = 0,
                                        size = 13,
                                        family = Fuente,
                                        margin = margin(
                                          t = 0, r = 0, b = 0, l = -150
                                        )),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "grey"),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "grey"),
        plot.margin = margin(t = 10, r = 200, b = 10, l = 200),
        legend.position = "none")} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo2.png",
             x = 0.87,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

ggsave(Salida,
       filename = "Salidas/Gráfico_circular.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")


