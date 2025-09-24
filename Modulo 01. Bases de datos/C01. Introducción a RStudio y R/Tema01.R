#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Introducción a R                             #
# Sesión: 02                                         #
# Fecha: 23/09/2025                                  #
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
  "agricolae",
  "tseries"
)

# Tipo de objetos en R (básicos) ----

## Valores ----

x <- 10
x
class(x)

nombre <- "Alexis"
nombre
class(nombre)

logico <- FALSE
logico
class(logico)

## Vector o listas de valores ----

edades <- c(25,30,45,80,15)
edades
class(edades)

nombres <- c("Alexis", "Francisco", "Víctor", "Eduardo",
             "Pamela")
nombres
class(nombres)

logicos <- c(T, F, T, F, F)
logicos
class(logicos)

## Data frames (Marco de datos)  ----

Gustos <- data.frame(
  "Edad" = c(25,30,45,80,15),
  "Nombre" = c("Alexis", "Francisco", "Víctor", "Eduardo",
               "Pamela"),
  "Fumador" = c(T, F, T, F, F)
)

class(Gustos)
str(Gustos)

## Objetos list -----

Lista_objetos <- list(
  "Valor" = x,
  "Vector_lista" = edades,
  "Data_frame" = Gustos
)

str(Lista_objetos)

