#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Importación y tipos de datos                 #
# Sesión: 03                                         #
# Fecha: 24/09/2025                                  #
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
  "lubridate",
  "sf"
)


# Datos transversales ----

## ENVIPE ----

ENVIPE_TSDEM <- read.csv("Bases de datos/TSDem.csv")

## Mostrar primeros 5 filas ----

head(ENVIPE_TSDEM, 5)

### Revisar nombres de variables de la base ----

colnames(ENVIPE_TSDEM) 
names(ENVIPE_TSDEM)

### Revisión del tipó de variables ----

str(ENVIPE_TSDEM)

### Remover la base de datos ----

rm(list = "ENVIPE_TSDEM")

# Datos de series de tiempo ----

## Tipo de cambio Diario Oficial de la Nación ----

TC_DOF <- read.csv("Bases de datos/TC_DOF.csv")

## Primeras 5 filas ----

head(TC_DOF, 5)

## Revisar nombres de columas ----

names(TC_DOF)
colnames(TC_DOF)

## Revisar el tipo de variable ----

str(TC_DOF)

## Modificar la fecha en formato Date ----

TC_DOF <- TC_DOF |>
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y"))

rm(list = "TC_DOF")

# Datos de panel -----

INDFC_EM <- read.csv("Bases de datos/IDEFC_NM_ago25.csv",
                     encoding = "latin1")

## Primeras 5 filas ----

head(INDFC_EM, 5)

## Revisar nombres de columas ----

names(INDFC_EM)
colnames(INDFC_EM)

## Revisar el tipo de variable ----

str(INDFC_EM)

### Remover la base de datos ----

rm(list = "INDFC_EM")


# Datos cartográficos (mapa de méxico por entidad) -----

Mapa_mex <- read_sf("Mapas/Mexico_Entidades/00ent.shp")

## Primeras 5 filas ----

head(Mapa_mex, 5)

## Revisar nombres de columas ----

names(Mapa_mex)
colnames(Mapa_mex)

## Revisar el tipo de variable ----

str(Mapa_mex)

## Remover el mapa ----

rm(list = "Mapa_mex")
