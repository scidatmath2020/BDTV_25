#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Aplicaciones de Shiny                        #
# Subtema: Construcción de la aplicación final       #
# Sesión: 20                                         #
# Fecha: 20/10/2025                                  #
# Instructor: Alexis Adonai Morales Albero           #
# SciData                                            #
#====================================================#

# Limpieza inicial de consola 

rm(list = ls())

# Condicional de existencia de pacman 

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado")
} else{
  install.packages("pacman", dependencies = T)
}


# Llamado e instalación de paquetes 
pacman::p_load(
  "tidyverse",
  "shiny",
  "shinyWidgets",
  "shinydashboard",
  "DT",
  "data.table",
  "plotly"
)

#==============================================================#
# Datos para la aplicación                                  ----
#==============================================================#

## Delitos ----

delitos_data <- read.csv(
  "E:/SciData/Bases de datos y técnicas de graficación/BDTG_SEP25/Bases de datos/Estatal-Delitos-2015-2025_sep2025.csv",
  check.names = F,
  fileEncoding = "latin1"
)

Meses <- names(delitos_data)[8:19]

delitos_data <- delitos_data %>% 
  mutate(Total = rowSums(across(Meses), na.rm=T)) %>% 
  select(Año,
         Clave_Ent,
         Entidad,
         `Subtipo de delito`,
         Total) %>% 
  group_by(Año,
           Clave_Ent,
           Entidad,
           `Subtipo de delito`) %>% 
  summarise(Total = sum(Total, na.rm=T)) %>% 
  ungroup() %>%
  group_by(Año, `Subtipo de delito`) %>% 
  bind_rows(summarise(.,
                      Clave_Ent = 0,
                      Entidad = "Nacional",
                      Total = sum(Total, na.rm=T))) %>% 
  ungroup() %>% 
  arrange(Año, Clave_Ent)



## Víctimas ----

victimas_data <- read.csv("E:/SciData/Bases de datos y técnicas de graficación/BDTG_SEP25/Bases de datos/Estatal-Víctimas-2015-2025_sep2025.csv",
                          check.names = F,
                          fileEncoding = "latin1")


Meses <- names(victimas_data)[10:21]

victimas_data <- victimas_data %>% 
  mutate(Total = rowSums(across(Meses), na.rm=T)) %>% 
  select(Año,
         Clave_Ent,
         Entidad,
         `Subtipo de delito`,
         Sexo,
         Total) %>% 
  group_by(Año,
           Clave_Ent,
           Entidad,
           `Subtipo de delito`,
           Sexo) %>% 
  summarise(Total = sum(Total, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(Año, Sexo, `Subtipo de delito`) %>% 
  bind_rows(summarise(.,
                      Clave_Ent = 0,
                      Entidad = "Nacional",
                      Total = sum(Total, na.rm=T))) %>% 
  ungroup() %>% 
  arrange(Año, Clave_Ent)


## Población ----

Población <- read.csv(
  "E:/SciData/Bases de datos y técnicas de graficación/BDTG_SEP25/Bases de datos/Pob_mitad_año.csv"
)


Población_hist <- Población %>% 
  group_by(ANIO,
           ENTIDAD,
           CVE_GEO,
  ) %>% 
  summarise(Poblacion = sum(POBLACION, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(ANIO) %>% 
  bind_rows(summarise(.,
                      ENTIDAD = "Nacional",
                      CVE_GEO = 0,
                      Poblacion = sum(Poblacion, na.rm=T))) %>% 
  ungroup() %>% 
  arrange(ANIO, CVE_GEO)


Población_hist_sex <- Población %>% 
  group_by(ANIO,
           ENTIDAD,
           CVE_GEO,
           SEXO
  ) %>% 
  summarise(Poblacion = sum(POBLACION, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(ANIO, SEXO) %>% 
  bind_rows(summarise(.,
                      ENTIDAD = "Nacional",
                      CVE_GEO = 0,
                      Poblacion = sum(Poblacion, na.rm=T))) %>% 
  ungroup() %>% 
  arrange(ANIO, CVE_GEO)


#==============================================================#
# Interfaz del usuario (UI)                                ----
#==============================================================#

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div(
      HTML("
       <h3 style = '
          color: white;
          font-weight: bold;
          font-size: 22px;
          margin: 0;
          line-height: 1.2;
          text-shadow: 1px 1px 2px #000000;
          '>
          Incidencia delictiva en México
        </h3>
      ")
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        tags$img(
          src = "logo2.png",
          height = "45px",
          style = "
          position: absolute;
          right: 25px;
          top: 12px;
          object-fit: contain;
          background-color: white;
          padding: 4px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.3)
          "
        )
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Delitos", tabName = "delitos", icon = icon("gavel")),
      menuItem("Víctimas", tabName = "victimas", icon = icon("users"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
      body {
        background-color: #f0f4f7;
      }
      .info-box {
        background-color: white;
        border-radius: 10px;
        box-shadow: 2px 2px 5px #ccc;
      }
    ")),
    tags$head(
      tags$style(HTML("
       /* --- Barra superior --- */
       .main-header .navbar {
          background-color: #003366 !important;
          height: 80px; /* un poco más alto para dar espacio al título */
          align-items: center;
       }
       
       /* --- Área del título --- */
       .main-header .logo {
          background-color: #003366 !important;
          white-space: normal !important;
          height: 80px !important;
          displayu: flex;
          align-items: center;
          justify-content: center;
       }
       
       /* --- Fondo general del cuerpo --- */
       .content-wrapper, .right-side {
          background-color: #f4f6f9;
          padding-top: 20px !important;
          margin-top: 0px !important; /* Baja el contenido para que no corte */
       }
       
       /* --- Sidebar --- */
       .main-sidebar{
          padding-top: 80px !important;
       }
       
       /* --- Texto --- */
       body, label, input, button, select {
          font-family: 'Segoe UI', sans-serif;
       }
       
       */ --- Ajuste visual para pestañas --- */
       .nav-tabs-custom{
          margin-top: 0px
       }
       
      "))
    ),
    tabItems(
      tabItem(
        tabName = "delitos",
        tabsetPanel(
          tabPanel(
            "Gráfico lineal",
            fluidRow(
              column(3,
                     pickerInput("delito_lineal", "Tipo de delito:",
                                 choices = unique(delitos_data$`Subtipo de delito`)),
                     pickerInput("entidad_lineal", "Entidad federativa:",
                                 choices = unique(delitos_data$Entidad)),
                     pickerInput("año_lineal", "Año",
                                 choices = sort(unique(delitos_data$Año)),
                                 selected = 2025)),
              column(9,
                     fluidRow(
                       infoBoxOutput("total_anual_delitos", width = 4),
                       infoBoxOutput("variacion_anual_delitos", width = 4),
                       infoBoxOutput("tasa_delitos", width = 4)
                     ),
                     plotlyOutput("graf_lineal_delitos", height = "600px")
                
              )
            )
          ),
          tabPanel(
            "Gráfico de barras",
            column(2,
                   pickerInput("entidad_lineal", "Entidad federativa:",
                               choices = unique(delitos_data$Entidad)),
                   pickerInput("año_lineal", "Año:",
                               choices = sort(unique(delitos_data$Año)), selected = 2025)
                   ),
            column(9,
                   fluidRow(
                     infoBoxOutput("Delito_mas_frecuente", width = 4),
                     infoBoxOutput("Total_de_delitos", width = 4),
                     infoBoxOutput("Var_delitos", width = 4)
                   ),
                   plotlyOutput("graf_barras_delit", height = "600px")
                   
                   )
          )
        )
      ),
      
      tabItem(
        tabName = "victimas",
        tabsetPanel(
          tabPanel(
            "Gráfico lineal",
            fluidRow(
              column(3,
                     pickerInput("victima_lineal", "Tipo de delito:",
                                 choices = unique(victimas_data$`Subtipo de delito`)),
                     pickerInput("entidad_victima_lineal", "Entidad federativa:",
                                 choices = unique(victimas_data$Entidad)),
                     pickerInput("año_victima_lineal", "Año",
                                 choices = sort(unique(victimas_data$Año)),
                                 selected = 2025)),
              column(9,
                     fluidRow(
                       infoBoxOutput("total_anual_victimas", width = 4),
                       infoBoxOutput("variacion_anual_victimas", width = 4),
                       infoBoxOutput("tasa_victimas", width = 4)
                     ),
                     plotlyOutput("graf_lineal_victimas", height = "400px")
                     
              )
            )
          ),
          tabPanel(
            "Gráfico de barras",
            column(2,
                   pickerInput("entidad_lineal_vic", "Entidad federativa:",
                               choices = unique(victimas_data$Entidad)),
                   pickerInput("año_lineal_vic", "Año:",
                               choices = sort(unique(victimas_data$Año)), selected = 2025)
            ),
            column(9,
                   fluidRow(
                     infoBoxOutput("Delito_mas_frecuente_vic", width = 4),
                     infoBoxOutput("Total_de_victimas", width = 4),
                     infoBoxOutput("Var_delitos_vic", width = 4)
                   ),
                   plotlyOutput("graf_barras_victima", height = "400px")
                   
            )
          )
        )
      )
      
    )
  )
)


#==============================================================#
# Servidor (sever)                                          ----
#==============================================================#

server <- function(input, output, session){
  
  #==============================#
  # Población                    #
  #==============================#
  
  poblacion_act <- reactive({
    Población_hist
  })
  
  # ============================ #
  # DELITOS - Gráfico lineal     #
  # ============================ #
  
  datos_delitos <- reactive({
    delitos_data %>%
      filter(`Subtipo de delito` == input$delito_lineal,
             Entidad == input$entidad_lineal)
  })
  
  resumen_delitos <- reactive({
    
    anio_sel <- as.numeric(input$año_lineal)
    
    actual <- datos_delitos() %>%
      filter(Año == anio_sel) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    anterior <- datos_delitos() %>%
      filter(Año == (anio_sel - 1)) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    variacion <- if (length(anterior) > 0) ((actual - anterior) / anterior) * 100 else NA
    
    variacion[is.nan(variacion)] <- 0
    
    tasa <- datos_delitos() %>%
      filter(Año == anio_sel) %>%
      group_by(Año, Clave_Ent, Entidad) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>%
      ungroup() %>% 
      inner_join(poblacion_act(),
                 by = c("Año" = "ANIO",
                        "Clave_Ent" = "CVE_GEO",
                        "Entidad" = "ENTIDAD")) %>% 
      mutate(Tasa = (Total/Poblacion)*100000) %>% 
      pull(Tasa)
    
    list(actual = actual, variacion = variacion, tasa = tasa)
    
  })
  
  
  output$total_anual_delitos <- renderInfoBox({
    infoBox("Total del año",
            value = format(resumen_delitos()$actual, big.mark = " "),
            icon = icon("chart-line"),
            color = "light-blue",
            fill = TRUE)
  })
  
  output$variacion_anual_delitos <- renderInfoBox({
    infoBox("Variación anual",
            value = paste0(round(resumen_delitos()$variacion, 2), " %"),
            icon = icon("percent"),
            color = ifelse(resumen_delitos()$variacion >= 0, "red", "green"),
            fill = TRUE)
  })
  
  output$tasa_delitos <- renderInfoBox({
    infoBox("Tasa por 100 mil hab.",
            value = round(resumen_delitos()$tasa, 1),
            icon = icon("users"),
            color = "aqua",
            fill = TRUE)
  })
  
  output$graf_lineal_delitos <- renderPlotly({
    df <- datos_delitos()
    
    plot_ly(
      data = df,
      x = ~Año,
      y = ~Total,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#1B87B6", width = 3),
      marker = list(color = "#B64A1B", size = 8),
      text = ~paste(
        "<b>Delito:</b>", input$delito_lineal, "<br>",
        "<b>Año:</b>", Año, "<br>",
        "<b>Total:</b>", format(Total, big.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b style='color:#003366;'>Tendencia de ", input$delito_lineal, "</b><br>",
            "<span style='font-size:14px; color:#666;'>Entidad: ", input$entidad_lineal, "</span>"
          ),
          font = list(family = "Arial", size = 18),
          x = 0.05,
          y = 0.95
        ),
        xaxis = list(
          title = "Año",
          tickmode = "linear",
          title_standoff = 25,
          rangeslider = list(
            visible = TRUE,
            thickness = 0.08,                  # grosor del slider
            bgcolor = "rgba(0,0,0,0.07)",      # color de sombra
            bordercolor = "rgba(0,0,0,0.2)"    # borde tenue
          ),
          rangeselector = list(visible = FALSE) # desactiva botones de rango
        ),
        yaxis = list(title = "Total de delitos"),
        hovermode = "x",  # ← "x" da la banda vertical (área)
        hoverlabel = list(
          bgcolor = "rgba(30,144,255,0.8)",
          font = list(color = "white", size = 13),
          bordercolor = "rgba(255,255,255,0)"
        ),
        plot_bgcolor = "#f9f9f9",
        paper_bgcolor = "#f9f9f9",
        annotations = list(
          # Pie de gráfico
          list(
            text = "<i>Fuente: SESNSP. Incidencia delictiva estatal del fuero común, 2015-2025.</i>",
            xref = "paper", yref = "paper",
            x = 0, y = -0.32,
            showarrow = FALSE,
            font = list(size = 12, color = "#666")
          )
        ),
        margin = list(b = 150) # espacio para el pie
      )
  })
  
  
  # ============================ #
  # DELITOS - Gráfico barras     #
  # ============================ #
  
  datos_delitos_bar <- reactive({
    delitos_data %>%
      filter(Entidad == input$entidad_lineal)
  })
  
  resumen_delitos_bar <- reactive({
    
    anio_sel_bar <- as.numeric(input$año_lineal)
    
    actual_bar <- datos_delitos_bar() %>%
      filter(Año == anio_sel_bar) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    anterior_bar <- datos_delitos_bar() %>%
      filter(Año == (anio_sel_bar - 1)) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    variacion_bar <- if (length(anterior_bar) > 0) ((actual_bar - anterior_bar) / anterior_bar) * 100 else NA
    
    variacion_bar[is.nan(variacion_bar)] <- 0
    
    delito_bar <- datos_delitos_bar() %>%
      filter(Año == anio_sel_bar) %>% 
      arrange(desc(Total)) %>% 
      slice(1) %>% 
      pull(`Subtipo de delito`)
    
    list(actual_bar = actual_bar, variacion_bar = variacion_bar, 
         delito_bar = delito_bar)
    
  })
  
  
  output$Total_de_delitos <- renderInfoBox({
    infoBox("Total de delitos",
            value = format(resumen_delitos_bar()$actual_bar, big.mark = " "),
            icon = icon("chart-line"),
            color = "light-blue",
            fill = TRUE)
  })
  
  output$Var_delitos <- renderInfoBox({
    infoBox("Variación anual",
            value = paste0(round(resumen_delitos_bar()$variacion_bar, 2), " %"),
            icon = icon("percent"),
            color = ifelse(resumen_delitos_bar()$variacion_bar >= 0, "red", "green"),
            fill = TRUE)
  })
  
  output$Delito_mas_frecuente <- renderInfoBox({
    infoBox("Delito más frecuente",
            value = paste(resumen_delitos_bar()$delito_bar),
            icon = icon("gavel"),
            color = "aqua",
            fill = TRUE)
  })
  
  output$graf_barras_delit <- renderPlotly({
    
    
    df_bar <- datos_delitos_bar() %>% 
      filter(Año == as.numeric(input$año_lineal)) %>% 
      arrange(desc(Total)) %>% 
      slice(1:10)
    
    df_bar$`Subtipo de delito` <- factor(df_bar$`Subtipo de delito`, levels = df_bar$`Subtipo de delito`[order(df_bar$Total)])
    
    
    plot_ly(
      data = df_bar,
      x = ~Total,
      y = ~`Subtipo de delito`,
      type = "bar",
      marker = list(color = "#B64A1B"),
      text = ~paste(
        format(Total, big.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b style='color:#003366;'>Principales 10 delitos ", input$año_lineal, "</b><br>",
            "<span style='font-size:14px; color:#666;'>Entidad: ", input$entidad_lineal, "</span>"
          ),
          font = list(family = "Arial", size = 18),
          x = 0.05,
          y = 0.95
        ),
        xaxis = list(
          title = "Frecuencia de delitos"   # borde tenue
        ),
        yaxis = list(title = "Principales delitos"),
        plot_bgcolor = "#f9f9f9",
        paper_bgcolor = "#f9f9f9",
        annotations = list(
          # Pie de gráfico
          list(
            text = "<i>Fuente: SESNSP. Incidencia delictiva estatal del fuero común, 2015-2025.</i>",
            xref = "paper", yref = "paper",
            x = 0, y = -0.32,
            showarrow = FALSE,
            font = list(size = 12, color = "#666")
          )
        ),
        margin = list(b = 20) # espacio para el pie
      )
  })
  
  
  
  # -----------------------------
  # VÍCTIMAS - Gráfico lineal
  # -----------------------------
  datos_victimas <- reactive({
    victimas_data %>%
      filter(`Subtipo de delito` == input$victima_lineal,
             Entidad == input$entidad_victima_lineal)
  })
  
  resumen_victimas <- reactive({
    anio_sel <- as.numeric(input$año_victima_lineal)
    
    actual <- datos_victimas() %>%
      filter(Año == anio_sel) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    anterior <- datos_victimas() %>%
      filter(Año == (anio_sel - 1)) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    variacion <- if (length(anterior) > 0) ((actual - anterior) / anterior) * 100 else NA
    
    variacion[is.nan(variacion)] <- 0
    
    tasa <- datos_victimas() %>%
      filter(Año == anio_sel) %>%
      group_by(Año, Clave_Ent, Entidad) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>%
      ungroup() %>% 
      inner_join(poblacion_act(),
                 by = c("Año" = "ANIO",
                        "Clave_Ent" = "CVE_GEO",
                        "Entidad" = "ENTIDAD")) %>% 
      mutate(Tasa = (Total/Poblacion)*100000) %>% 
      pull(Tasa)
    
    list(actual = actual, variacion = variacion, tasa = tasa)
  })
  
  
  output$total_anual_victimas <- renderInfoBox({
    infoBox("Total del año",
            value = format(resumen_victimas()$actual, big.mark = ","),
            icon = icon("chart-line"),
            color = "light-blue",
            fill = TRUE)
  })
  
  output$variacion_anual_victimas <- renderInfoBox({
    infoBox("Variación anual",
            value = paste0(round(resumen_victimas()$variacion, 2), " %"),
            icon = icon("percent"),
            color = ifelse(resumen_victimas()$variacion >= 0, "green", "red"),
            fill = TRUE)
  })
  
  output$tasa_victimas <- renderInfoBox({
    infoBox("Tasa por 100 mil hab.",
            value = round(resumen_victimas()$tasa, 2),
            icon = icon("users"),
            color = "aqua",
            fill = TRUE)
  })
  
  output$graf_lineal_victimas <- renderPlotly({
    
    df <- datos_victimas()
    
    # Aseguramos que la variable Sexo sea factor con orden consistente
    df$Sexo <- factor(df$Sexo, levels = c("Hombre", "Mujer", "No identificado"))
    
    plot_ly(
      data = df,
      x = ~Año,
      y = ~Total,
      color = ~Sexo,
      colors = c("#1B87B6", "#B64A1B"),  # azul para hombres, naranja para mujeres
      type = "scatter",
      mode = "lines+markers",
      line = list(width = 3),
      marker = list(size = 8),
      text = ~paste(
        "<b>Delito:</b>", input$victima_lineal, "<br>",
        "<b>Sexo:</b>", Sexo, "<br>",
        "<b>Año:</b>", Año, "<br>",
        "<b>Total:</b>", format(Total, big.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b style='color:#003366;'>Víctimas registradas del delito: ", input$input$victima_lineal, "</b><br>",
            "<span style='font-size:14px; color:#666;'>Entidad: ", input$entidad_victima_lineal, "</span>"
          ),
          font = list(family = "Arial", size = 18),
          x = 0.05,
          y = 0.98,  # Aumentado para separar del gráfico
          pad = list(b = 20, t = 10)  # Padding adicional
        ),
        xaxis = list(
          title = "Año",
          tickmode = "linear",
          title_standoff = 25,
          rangeslider = list(
            visible = TRUE,
            thickness = 0.08,
            bgcolor = "rgba(0,0,0,0.07)",
            bordercolor = "rgba(0,0,0,0.2)"
          ),
          rangeselector = list(visible = FALSE)
        ),
        yaxis = list(
          title = "Total de delitos",
          title_standoff = 25
        ),
        hovermode = "x unified",
        hoverlabel = list(
          bgcolor = "rgba(30,144,255,0.8)",
          font = list(color = "white", size = 13),
          bordercolor = "rgba(255,255,255,0)"
        ),
        legend = list(
          title = list(text = "<b>Sexo</b>"),
          orientation = "h",
          x = 0.4, 
          y = -0.25,  # Bajado para separar del rangeslider
          font = list(size = 12)
        ),
        plot_bgcolor = "#f9f9f9",
        paper_bgcolor = "#f9f9f9",
        annotations = list(
          list(
            text = "<i>Fuente: SESNSP. Incidencia delictiva estatal del fuero común, 2015-2025.</i>",
            xref = "paper", yref = "paper",
            x = 0, y = -0.45,  # Bajado para separar de la leyenda y rangeslider
            showarrow = FALSE,
            font = list(size = 12, color = "#666")
          )
        ),
        margin = list(
          pad = 10  # padding general
        )
      )
  })
  
 
  # ============================ #
  # Victimas - Gráfico barras     #
  # ============================ #
  
  datos_victimas_bar <- reactive({
    victimas_data %>%
      filter(Entidad == input$entidad_lineal_vic)
  })
  
  
  resumen_victimas_bar <- reactive({
    
    anio_sel_bar <- as.numeric(input$año_lineal_vic)
    
    actual_bar <- datos_victimas_bar() %>%
      filter(Año == anio_sel_bar) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    anterior_bar <- datos_victimas_bar() %>%
      filter(Año == (anio_sel_bar - 1)) %>%
      group_by(Año) %>% 
      summarise(Total = sum(Total, na.rm=T)) %>% 
      pull(Total)
    
    variacion_bar <- if (length(anterior_bar) > 0) ((actual_bar - anterior_bar) / anterior_bar) * 100 else NA
    
    variacion_bar[is.nan(variacion_bar)] <- 0
    
    victima_bar <- datos_victimas_bar() %>%
      filter(Año == anio_sel_bar) %>% 
      arrange(desc(Total)) %>% 
      slice(1) %>% 
      pull(`Subtipo de delito`)
    
    list(actual_bar = actual_bar, variacion_bar = variacion_bar, 
         victima_bar = victima_bar)
    
  })
  
  
  output$Total_de_victimas <- renderInfoBox({
    infoBox("Total de victimas",
            value = format(resumen_victimas_bar()$actual_bar, big.mark = " "),
            icon = icon("chart-line"),
            color = "light-blue",
            fill = TRUE)
  })
  
  output$Var_delitos_vic <- renderInfoBox({
    infoBox("Variación anual",
            value = paste0(round(resumen_victimas_bar()$variacion_bar, 2), " %"),
            icon = icon("percent"),
            color = ifelse(resumen_victimas_bar()$variacion_bar >= 0, "red", "green"),
            fill = TRUE)
  })
  
  output$Delito_mas_frecuente_vic <- renderInfoBox({
    infoBox("Delito más frecuente",
            value = paste(resumen_victimas_bar()$victima_bar),
            icon = icon("gavel"),
            color = "aqua",
            fill = TRUE)
  })
  
  output$graf_barras_victima <- renderPlotly({
    
    
    df_bar_vic <- datos_victimas_bar() %>% 
      filter(Año == as.numeric(input$año_lineal_vic)) %>% 
      arrange(desc(Total)) %>% 
      slice(1:5)
    
    plot_ly(
      data = df_bar_vic,
      x = ~Total,
      y = ~`Subtipo de delito`,
      type = "bar",
      marker = list(color = "#B64A1B"),
      text = ~paste(
        format(Total, big.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0(
            "<b style='color:#003366;'>Principales 5 delitos en víctimas para ", input$año_lineal_vic, "</b><br>",
            "<span style='font-size:14px; color:#666;'>Entidad: ", input$entidad_lineal_vic, "</span>"
          ),
          font = list(family = "Arial", size = 18),
          x = 0.05,
          y = 0.95
        ),
        xaxis = list(
          title = "Número de víctimas"   # borde tenue
        ),
        yaxis = list(title = "Principales delitos"),
        plot_bgcolor = "#f9f9f9",
        paper_bgcolor = "#f9f9f9",
        annotations = list(
          # Pie de gráfico
          list(
            text = "<i>Fuente: SESNSP. Incidencia delictiva estatal del fuero común, 2015-2025.</i>",
            xref = "paper", yref = "paper",
            x = 0, y = -0.32,
            showarrow = FALSE,
            font = list(size = 12, color = "#666")
          )
        ),
        margin = list(b = 20) # espacio para el pie
      )
  })
  
  
  
  
}



#==============================================================#
# Ejecución de APP (Shiny)                                          ----
#==============================================================#

shinyApp(ui, server)