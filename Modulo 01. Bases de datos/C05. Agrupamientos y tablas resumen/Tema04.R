#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Agrupamientos y resumenes                    #
# Sesión: 05                                         #
# Fecha: 26/09/2025                                  #
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
  "lubridate"
)


# Importación de datos ----

INDFC_E <- read.csv("Bases de datos/IDEFC_NM_ago25.csv",
                    check.names = FALSE,
                    encoding = "latin1")

# Verificar si hay presencia de NA's en la base ----

length(INDFC_E[is.na(INDFC_E)])

# Operaciones agrupadas y tablas de resumen ----

## Objetivo: Crear una tabla sobre homicidios dolosos del 2024 por entidad federativa ----

### Sin crear objetos y exportando salida ----

INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  select(all_of(c("Año",
                  "Clave_Ent",
                  "Entidad",
                  "Subtipo de delito",
                  "Anual"))) %>%  
  filter(Año == 2024 & `Subtipo de delito` == "Homicidio doloso") %>% 
  group_by(Año, Clave_Ent, Entidad) %>% 
  summarise(Homicidios_doloso = sum(Anual),
            Promedio_anual = mean(Anual)) %>% 
  ungroup() %>% 
  bind_rows(
    summarise(.,
              Año = 2024,
              Clave_Ent = "00",
              Entidad = "Nacional",
              Homicidios_doloso = sum(Homicidios_doloso),
              Promedio_anual = sum(Promedio_anual))
  ) %>% 
  arrange(Clave_Ent) %>% 
  mutate(Hom_dol_dia = Homicidios_doloso/365,
         Hom_dol_hora = Hom_dol_dia/24) %>% 
  write.csv(., file = "Salidas/Homicidios_2024_Entidad.csv",
            row.names = F,
            fileEncoding = "UTF-8")
  

INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  select(all_of(c("Año",
                  "Clave_Ent",
                  "Entidad",
                  "Subtipo de delito",
                  "Anual"))) %>%  
  filter(Año == 2024 & `Subtipo de delito` == "Homicidio doloso") %>% 
  group_by(Año, Clave_Ent, Entidad) %>% 
  summarise(Homicidios_doloso = sum(Anual),
            Promedio_anual = mean(Anual)) %>% 
  ungroup() %>% 
  bind_rows(
    summarise(.,
              Año = 2024,
              Clave_Ent = "00",
              Entidad = "Nacional",
              Homicidios_doloso = sum(Homicidios_doloso),
              Promedio_anual = sum(Promedio_anual))
  ) %>% 
  arrange(Clave_Ent) %>% 
  mutate(Hom_dol_dia = Homicidios_doloso/365,
         Hom_dol_hora = Hom_dol_dia/24) %>% 
  write.xlsx(., file = "Salidas/Homicidios_2024_Entidad.xlsx")

### Creando objetos y exportar salida -----

Salida <- INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  select(all_of(c("Año",
                  "Clave_Ent",
                  "Entidad",
                  "Subtipo de delito",
                  "Anual"))) %>%  
  filter(Año == 2024 & `Subtipo de delito` == "Homicidio doloso") %>% 
  group_by(Año, Clave_Ent, Entidad) %>% 
  summarise(Homicidios_doloso = sum(Anual),
            Promedio_anual = mean(Anual)) %>% 
  ungroup() %>% 
  bind_rows(
    summarise(.,
              Año = 2024,
              Clave_Ent = "00",
              Entidad = "Nacional",
              Homicidios_doloso = sum(Homicidios_doloso),
              Promedio_anual = sum(Promedio_anual))
  ) %>% 
  arrange(Clave_Ent) %>% 
  mutate(Hom_dol_dia = Homicidios_doloso/365,
         Hom_dol_hora = Hom_dol_dia/24) 

write.csv(Salida, 
          file = "Salidas/Homicidios_2024_Entidad.csv",
          row.names = F,
          fileEncoding = "UTF-8")

write.xlsx(Salida, file = "Salidas/Homicidios_2024_Entidad.xlsx")


## Objetivo: Crear una tabla sobre homicidios dolosos del a nivel nacional de 2015 a 2024 ----

INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  select(all_of(c("Año",
                  "Clave_Ent",
                  "Entidad",
                  "Subtipo de delito",
                  "Anual"))) %>%  
  filter((Año >= 2015 & Año <= 2024) & `Subtipo de delito` == "Homicidio doloso") %>% 
  group_by(Año) %>% 
  summarise(Homicidios_doloso = sum(Anual),
            Promedio_anual = mean(Anual)) %>% 
  ungroup() %>% 
  mutate(Hom_dol_dia = Homicidios_doloso/365,
         Hom_dol_hora = Hom_dol_dia/24) %>% 
  write.xlsx(., file = "Salidas/HomicidiosNac_2015_2024.xlsx")


## Objetivo: Crear una tabla sobre homicidios dolosos por entidad y a nivel nacional de 2015 a 2024 ----

INDFC_E %>% 
  mutate(Anual = rowSums(across(names(INDFC_E)[8:19]), na.rm=T),
         Clave_Ent = ifelse(Clave_Ent < 10,
                            paste0("0", Clave_Ent),
                            Clave_Ent)) %>% 
  select(all_of(c("Año",
                  "Clave_Ent",
                  "Entidad",
                  "Subtipo de delito",
                  "Anual"))) %>%  
  filter((Año >= 2015 & Año <= 2024) & `Subtipo de delito` == "Homicidio doloso") %>% 
  group_by(Año, Clave_Ent, Entidad) %>% 
  summarise(Homicidios_doloso = sum(Anual),
            Promedio_anual = mean(Anual)) %>% 
  ungroup() %>% 
  group_by(Año) %>% 
  bind_rows(
    summarise(.,
              Clave_Ent = "00",
              Entidad = "Nacional",
              Homicidios_doloso = sum(Homicidios_doloso),
              Promedio_anual = sum(Promedio_anual))
  ) %>% 
  arrange(Año, Clave_Ent) %>% 
  mutate(Hom_dol_dia = Homicidios_doloso/365,
         Hom_dol_hora = Hom_dol_dia/24) %>% 
  write.xlsx(., file = "Salidas/HomicidiosNacEnt_2015_2024.xlsx")




