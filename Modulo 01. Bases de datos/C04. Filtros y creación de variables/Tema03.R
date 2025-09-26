#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Filtro, agrupamientos y resumenes            #
# Sesión: 04                                         #
# Fecha: 25/09/2025                                  #
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
  "haven",
  "foreign",
  "lubridate"
)

# Importación de datos ----

INDFC_E <- read.csv("Bases de datos/IDEFC_NM_ago25.csv",
                    check.names = FALSE,
                    encoding = "latin1")

# Filtros ----

# Operador pipe = ctrl+shift+m %>% es equivalente a |>

# View() sirve para visualizar temporalmente la tabla o la base con la 
# operación que se realizó, para continuar realizando operaciones hay que
# quitar View()

## Valor exacto ----

unique(INDFC_E$`Subtipo de delito`)

INDFC_E %>% 
  filter(`Subtipo de delito` == "Homicidio doloso") %>% 
  View()

INDFC_E %>% 
  filter(`Subtipo de delito` == "Robo de autopartes") %>% 
  View()

## Filtro "O" (|) ----

INDFC_E %>% 
  filter(`Subtipo de delito` == "Homicidio doloso" | `Subtipo de delito` == "Homicidio culposo") %>% 
  View()

## NOTA: El filtro de "o" sirve para una misma variable o para dos, sin embargo
## para el caso de una variable y si son dos o más elementos podemos usar el filtro
## de varios valores unicos y se utiliza %in% seguido de un c() con los elementos 
## a filtrar 

INDFC_E %>% 
  filter(`Subtipo de delito` %in% c("Homicidio doloso",
                                    "Homicidio culposo")) %>% 
  View()

### Ejemplo de "o" usando dos variables ----

INDFC_E %>% 
  filter(Entidad == "Aguascalientes" | `Subtipo de delito` == "Homicidio doloso")%>% 
  View()


## Filtro "Y" con & ---

## NOTA: El filtro de Y, se usa para seleccionar valores exactos (unicos) de dos o más variables

INDFC_E %>% 
  filter(Entidad == "Aguascalientes" & `Subtipo de delito` == "Homicidio doloso")%>% 
  View()

INDFC_E %>% 
  filter(Entidad == "Aguascalientes" & `Tipo de delito` == "Homicidio")%>% 
  View()

INDFC_E %>% 
  filter(Entidad == "Aguascalientes" & `Bien jurídico afectado` == "El patrimonio")%>% 
  View()

INDFC_E %>% 
  filter(Año == "2015", Entidad == "Aguascalientes" & 
           `Bien jurídico afectado` == "El patrimonio")%>% 
  View()

INDFC_E %>% 
  filter(Año == "2015", Entidad == "Aguascalientes" & 
           `Subtipo de delito` == "Homicidio doloso") %>% 
  View()


## Filtros especiales ----

### Cuando un valor número es menor o igual a ----

INDFC_E %>% 
  filter(Año <= 2022) %>% 
  View()

### Cuando un valor número se encuentra entre dos valores ----

INDFC_E %>% 
  filter(Año >= 2022 & Año <= 2024) %>% 
  View()

### Un rango de valores números y un valor exacto (categorico) ----

INDFC_E %>% 
  filter((Año >= 2022 & Año <= 2024) & Entidad == "Aguascalientes") %>% 
  View()

### Un rango de valores números y multiples categorias -----

INDFC_E %>% 
  filter((Año >= 2022 & Año <= 2024) & Entidad %in% c("Ciudad de México",
                                                      "México")) %>% 
  View()



# Creación o trasnformación de columnas ----

# NOTA: El comando mutate sirve para crear columnas dentro de un dataframe
# esta columna puede ser una manipulación interna de las otras columnas
# del propio data frame, o bien, se puede crear valores mediante un vector,
# o transformando una variable, etc, etc.

## Ejemplo 1: Sumar todas las columnas de los meses como "anual" (creación de columna) ----

### Primero extraer los nombres de las columnas de los meses corrrespondientes 

names(INDFC_E)[8:19]

### Posteriormente se usará el comando mutate junto con rowSums, seguido de across 

INDFC_E <- INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T))

## Ejemplo 2: Modificar la clave de entidad donde valores < 10 empiecen con 0 (trasnformar) ----

### Ejemplos son guardar 

INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  View()


INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(nchar(Clave_Ent) == 1,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  View()


### Ejemplo guardado

INDFC_E <- INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent))
