#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Gráfico de nube de palabras y flujos sinkey  #
# Sesión: 14                                         #
# Fecha: 08/10/2025                                  #
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

## ggsankey es repo de github ----

remotes::install_github("davidsjoberg/ggsankey")

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
  "ggwordcloud",
  "ggsankey",
  "shadowtext"
)

# Carga de fuentes de Windows (linux, MacOS) ----

loadfonts(device = "win")
fonts()

#===========================================================================#
# Gráfico de nube de puntos                                                 #
#===========================================================================#

# Datos a carga o utilizar -----

Data <- thankyou_words_small

# Estructura básica -----

# Lista formas (shape): circle, cardioid, diamond, star,
# square, triangle-forward, "triangle-upright

set.seed(123)
Data %>% 
  ggplot(aes(label = word, size = native_speakers))+
  geom_text_wordcloud(shape = "square")+
  scale_size_area(max_size = 30)

# Utilizando una figura ----

imagen_png <- png::readPNG(system.file("extdata/hearth.png",
                                       package = "ggwordcloud",
                                       mustWork = T))


set.seed(123)
Data %>% 
  ggplot(aes(label = word, size = native_speakers))+
  geom_text_wordcloud(mask = imagen_png)+
  scale_size_area(max_size = 30)

# Cambiar color de palabras -----

set.seed(123)
Data %>% 
  ggplot(aes(label = word, size = native_speakers))+
  geom_text_wordcloud(shape = "square",
                      color = "blue4")+
  scale_size_area(max_size = 30)

# Cambiar color de palabras segun variable cualitativa-----

set.seed(123)
Data %>% 
  ggplot(aes(label = word, size = native_speakers, color = name))+
  geom_text_wordcloud(shape = "square")+
  scale_size_area(max_size = 30)


# Cambiar color de palabras segun variable continua -----

set.seed(123)
Data %>% 
  ggplot(aes(label = word, size = native_speakers, color = native_speakers))+
  geom_text_wordcloud(shape = "square")+
  scale_size_area(max_size = 30)+
  scale_color_gradient(low = "#09AE95", high = "#AE0922")


# Salida con temas y títulos ----

Fuente <- "Tahoma"

set.seed(123)
Salida <- Data %>% 
  ggplot(aes(label = word, size = native_speakers, color = native_speakers))+
  geom_text_wordcloud(shape = "square",
                      family = Fuente)+
  scale_size_area(max_size = 30)+
  scale_color_gradient(low = "#09AE95", high = "#AE0922")+
  labs(
    title = "<b style = 'color:#1C69A8'> Idiomas más frecuentes que se hablan en el mundo </b><b style = 'color:#E7180B'> según palabra gracias</b><b style = 'color:#1C69A8'> y </b><b style = 'color:#E7180B'> por lenguaje </b>",
    subtitle = "<i style = 'color:#E7180B'> a mayor tamaño, mayor frecuencia</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. thankyou words small de ggwordcloud</span>",
    x = "",
    y = "",
    color = ""
  )+
  theme(plot.title = element_markdown(size = 16,
                                      family = Fuente,
                                      hjust = 0),
        plot.subtitle = element_markdown(size = 14,
                                         family = Fuente,
                                         hjust = 0),
        plot.caption = element_markdown(hjust = 0,
                                        size = 12,
                                        family = Fuente),
        axis.text = element_text(color = "#1C69A8",
                                 face = "bold",
                                 size = 13,
                                 family = Fuente),
        legend.position = "bottom",
        legend.text = element_markdown(size = 12,
                                       family = Fuente,
                                       color = "#1C69A8"),
        panel.background = element_rect(fill = "white"))

Salida 

ggsave(Salida,
       filename = "Salidas/Gráfico_nube_puntos.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")


#===========================================================================#
# Gráfico de flujos (diagrama de Sinkey)                                    #
#===========================================================================#

# Datos a utilizar ----

ruta <- "https://github.com/holtzy/R-graph-gallery/blob/master/DATA/summer_movie_genres.csv?raw=true"
ruta2 <- "https://github.com/holtzy/R-graph-gallery/blob/master/DATA/summer_movies.csv?raw=true"

summer_movie_genres <- read_csv(ruta)
summer_movies <- read_csv(ruta2)

# Procesamiento para base del gráfico ----

summer_genres <- summer_movies %>%
  filter(title_type == "movie") %>%
  select(tconst, primary_title, year, runtime_minutes, average_rating) %>%
  mutate(decade = factor(year %/% 10 * 10)) %>%
  left_join(summer_movie_genres) %>%
  group_by(decade) %>%
  mutate(decade_n = n()) %>%
  ungroup() %>%
  group_by(decade, genres) %>%
  summarise(
    score = median(average_rating, na.rm = TRUE),
    n = n(),
    decade_n,
    prop = n / decade_n
  ) %>%
  ungroup() %>%
  distinct() %>%
  filter(!is.na(decade) & !is.na(genres) & !is.na(score))

# Estructura general ----

p <- summer_genres %>% 
  ggplot(aes(x = decade, node = genres, fill = genres,
             value = prop, label = genres))+
  geom_sankey_bump()

## Etiquetas del gráfico ----

g_labs_start <- ggplot_build(p) %>%
  .$data %>%
  .[[1]] %>%
  group_by(label) %>%
  filter(x == min(x)) %>%
  reframe(
    x,
    y = mean(y)
  ) %>%
  left_join(summer_genres %>%
              group_by(genres) 
            %>% filter(as.numeric(decade) == min(as.numeric(decade))),
            by = c("label" = "genres"))

g_labs_end <- ggplot_build(p) %>%
  .$data %>%
  .[[1]] %>%
  group_by(label) %>%
  filter(x == max(x)) %>%
  reframe(
    x,
    y = mean(y)
  ) %>%
  left_join(summer_genres %>% 
              group_by(genres) %>%
              filter(as.numeric(decade) == max(as.numeric(decade))),
            by = c("label" = "genres"))


## Estructura general + etiquetas ----

### Definción de paletas ----

pal <- c(
  "Comedy"="#FDA638",
  "Drama"="#459395",
  "Romance"="#EB7C69",
  rep("#866f85",22)
)

na_col <- "#866f85"

ggplot()+
  geom_sankey_bump(data = summer_genres,
                   aes(x = decade, node = genres, fill = genres_group,
                       value = prop, label = genres))+
  geom_shadowtext(data = g_labs_start, 
                  aes(x, y, label = paste(label, "·", n),
                      color = if_else(label %in% c("Drama",
                                                   "Comedy",
                                                   "Romance"), label, NA)),
                  hjust = 1, nudge_x = -0.1, bg.color = "grey99", fontface = "bold") +
  geom_shadowtext(data = g_labs_end, 
                  aes(x, y, label = paste(label, "·", n), 
                      color = if_else(label %in% c("Drama",
                                                   "Comedy", 
                                                   "Romance"), label, NA)), 
                  hjust = 0, nudge_x = 0.1, bg.color = "grey99") +
  scale_color_manual(values = pal, na.value = na_col) +
  scale_fill_manual(values = pal, na.value = na_col) +
  coord_cartesian(clip = "off")
  
# Salida con titulos y temas ----

Salida <- {ggplot()+
  geom_sankey_bump(data = summer_genres,
                   aes(x = decade, node = genres, fill = genres_group,
                       value = prop, label = genres))+
  geom_shadowtext(data = g_labs_start, 
                  aes(x, y, label = paste(label, "·", n),
                      color = if_else(label %in% c("Drama",
                                                   "Comedy",
                                                   "Romance"), label, NA)),
                  hjust = 1, nudge_x = -0.1, bg.color = "grey99", fontface = "bold") +
  geom_shadowtext(data = g_labs_end, 
                  aes(x, y, label = paste(label, "·", n), 
                      color = if_else(label %in% c("Drama",
                                                   "Comedy", 
                                                   "Romance"), label, NA)), 
                  hjust = 0, nudge_x = 0.1, bg.color = "grey99") +
  scale_color_manual(values = pal, na.value = na_col) +
  scale_fill_manual(values = pal, na.value = na_col) +
  coord_cartesian(clip = "off")+
  labs(
    title = "<b style = 'color:#E7180B'> Evolción del número de películas en el cine según</b><b style = 'color:#1C69A8'> genero</b>",
    subtitle = "<i style = 'color:#E7180B'> de 1920 a 2020 por decada</i>",
    caption = "<span style = 'color:#E7180B'> Fuente. Elaboración propia con datos de GitHub.</span>",
    x = "<span style = 'color:#E7180B'> Decada </span>",
    y = ""
  )+
  theme(plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 14),
        plot.caption = element_markdown(hjust = 0,
                                        size = 12),
        axis.title.x = element_markdown(size = 13),
        axis.text.x = element_text(color = "#1C69A8",
                                 face = "bold",
                                 size = 13,
                                 family = Fuente),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = alpha("#90A1B9",0.5),
                                          linetype = "dashed"),
        panel.background = element_rect(fill = "white"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#90A1B9"),
        legend.position = "none",
        plot.margin = margin(10, 50, 10, 30))} %>% 
  ggdraw(.)+
  draw_image("Imagenes/logo2.png",
             x = 0.93,
             y = 0.95,
             hjust = 0.7, vjust = 0.5,
             width = 0.2) 

ggsave(Salida,
       filename = "Salidas/Gráfico_flujo_sankey.png",
       width = 12,
       height = 7,
       dpi = 500,
       units = "in")
