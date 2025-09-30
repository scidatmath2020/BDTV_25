#=========================================================#
# Diplomado: Bases de datos y técnicas de gráficación     #
# Proyecto integrador 1: Exportación de datos con formato #
# Sesión: 06                                              #
# Fecha: 29/09/2025                                       #
# Instructor: Alexis Adonai Morales Albero                #
# SciData                                                 #
#=========================================================#

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
  "readxl",
  "openxlsx",
  "haven",
  "foreign",
  "lubridate"
)

# Carga de datos ----

Ingresos_STC <- read.csv(
  "Bases de datos/ingresosstc_2023_07.csv"
)

# Revisar el tipo de dato que contiene cada variable del dataframe -----

## Con str ----

str(Ingresos_STC)

## glimpse (de dplyr) -----

glimpse(Ingresos_STC)

## Con class individual ------

class(Ingresos_STC$fecha)

# Convertir variable de fecha en tipo date ----

Ingresos_STC <- Ingresos_STC %>% 
  mutate(Año_substr = substr(fecha, 1,4),
         fecha = as.Date(fecha, format = "%Y-%m-%d"),
         Año_formato = format(fecha, "%Y"))


# Nivel de ingresos del STC por tipo de ingreso y por año ----

NISTC_TP <- Ingresos_STC %>% 
  mutate(tipo_ingreso = recode(
    tipo_ingreso,
    "QR/Validador" = "QR/Validación"
  )) %>% 
  filter(Año_substr %in% c(as.character(2015:2022))) %>% 
  group_by(Año_substr, tipo_ingreso) %>% 
  summarise(Total_ingresos = sum(ingreso, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Año_substr) %>% 
  bind_rows(summarise(.,
                      tipo_ingreso = "Total",
                      Total_ingresos = sum(Total_ingresos, na.rm=T))) %>% 
  ungroup() %>% 
  mutate(tipo_ingreso = factor(tipo_ingreso,
                               levels = c(
                                 "Total", "Boletos",
                                 "Recargas", "Tarjetas",
                                 "QR/Validación"
                               ))) %>% 
  arrange(Año_substr, tipo_ingreso) %>% 
  pivot_wider(names_from = tipo_ingreso,
              values_from = Total_ingresos) %>% 
  mutate(Boletos_rel = Boletos/Total,
         Recargas_rel = Recargas/Total,
         Tarjetas_rel = Tarjetas/Total,
         `QR/Validación_rel` = `QR/Validación`/Total) %>% 
  select(all_of(c(
    "Año_substr", "Total",
    "Boletos","Boletos_rel",
    "Recargas", "Recargas_rel",
    "Tarjetas", "Tarjetas_rel",
    "QR/Validación","QR/Validación_rel"
  )))


# Exportación de datos ----

## Con formato ----

archivo <- "Salidas/PI1_Ingresos_Metro.xlsx"

hojas <- excel_sheets(archivo)

hoja <- "Con formato"

## Nombres de columnas que van en formato de % ----

columnas_decimal <- names(NISTC_TP)[str_detect(names(NISTC_TP), "_rel")]
columnas_anio <- "Año_substr"
columnas_monetarias <- names(NISTC_TP)[!str_detect(names(NISTC_TP), "_rel")][-1]

## Cargar el libro de trabajo ----

wb <- loadWorkbook(archivo)

## Posicionar fila de inicio ----

fila_inicio <- 10
columna_inicio <- 2

estilo_año <- createStyle(numFmt = "yyyy")
estilo_monetario <- createStyle(numFmt = "#,##0.0")
estilo_decimal <- createStyle(numFmt = "0.0%")

datos <- NISTC_TP

# Escribir celda por celda
for (i in seq_len(nrow(datos))) {
  for (j in seq_len(ncol(datos))) {
    valor <- datos[i, j]
    fila <- fila_inicio + i - 1
    col <- columna_inicio + j - 1
    nombre_col <- names(datos)[j]
    
    # Intentar convertir a número
    valor_numerico <- suppressWarnings(as.numeric(valor))
    
    # === LÓGICA DE ESCRITURA ===
    if (nombre_col %in% columnas_anio) {
      # Año (mantener formato fecha pero solo año)
      writeData(wb, hoja, valor, startRow = fila, startCol = col, colNames = FALSE)
      addStyle(wb, hoja, estilo_año, rows = fila, cols = col, stack = TRUE)
      
    } else if (nombre_col %in% columnas_monetarias) {
      # Monetario
      writeData(wb, hoja, valor_numerico, startRow = fila, startCol = col, colNames = FALSE)
      addStyle(wb, hoja, estilo_monetario, rows = fila, cols = col, stack = TRUE)
      
    } else if (nombre_col %in% columnas_decimal) {
      # Decimal
      writeData(wb, hoja, valor_numerico, startRow = fila, startCol = col, colNames = FALSE)
      addStyle(wb, hoja, estilo_decimal, rows = fila, cols = col, stack = TRUE)
      
    } else {
      # Si no cae en ninguno, escribir como está
      writeData(wb, hoja, valor, startRow = fila, startCol = col, colNames = FALSE)
    }
  }
}


# Crear estilo con relleno blanco
relleno_blanco <- createStyle(fgFill = "#FFFFFF")

# Aplicar relleno blanco a un rango amplio (ajústalo si necesitas más filas o columnas)

for (i in 1:length(hojas)) {
  addStyle(
    wb, hojas[i],
    style = relleno_blanco,
    rows = 1:1000,
    cols = 1:50,
    gridExpand = TRUE,
    stack = TRUE
  )
  
}

# Guardar
saveWorkbook(wb, archivo, overwrite = TRUE)



## Sin formato -----

## Sin formato ----

### Configuración de hoja ----

archivo <- "Salidas/PI1_Ingresos_Metro.xlsx"

hojas <- excel_sheets(archivo)

hoja <- "Sin formato"

### Leemos archivo ----

wb <- loadWorkbook(archivo)

### Guardado de datos en el libro ----

writeData(
  wb,
  sheet = hoja,
  x = datos,
  startRow = 1,
  startCol = 1,
  colNames = TRUE
)

# Crear estilo con relleno blanco
relleno_blanco <- list(
  createStyle(fgFill = "#FFFFFF"),
  createStyle()
)

# Aplicar relleno blanco a un rango amplio (ajústalo si necesitas más filas o columnas)

for (i in 1:length(hojas)) {
  addStyle(
    wb, hojas[i],
    style = relleno_blanco[[i]],
    rows = 1:1000,
    cols = 1:50,
    gridExpand = TRUE,
    stack = TRUE
  )
  
}

# Guardar
saveWorkbook(wb, archivo, overwrite = TRUE)







