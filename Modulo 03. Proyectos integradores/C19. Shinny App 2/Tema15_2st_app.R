#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Aplicaciones de Shiny                        #
# Subtema: Construcción de la aplicación final       #
# Sesión: 19                                         #
# Fecha: 17/10/2025                                  #
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
  "DT",
  "ggthemes",
  "data.table",
  "ggtext"
)

#==============================================================#
# Datos para la aplicación                                  ----
#==============================================================#

Delitos <- read.csv("E:/SciData/Bases de datos y técnicas de graficación/BDTG_SEP25/Bases de datos/Estatal-Delitos-2015-2025_sep2025.csv",
                    check.names = F,
                    fileEncoding = "latin1")

Delitos2 <- Delitos %>% 
  select(-Clave_Ent,
         -Entidad,
         -`Bien jurídico afectado`,
         -`Tipo de delito`,
         -`Subtipo de delito`,
         - Modalidad
         ) %>% 
  data.table() %>% 
  melt(id.vars = c("Año"),
       value.name = "Delitos",
       variable.name = "Meses") %>% 
  group_by(Año) %>% 
  summarise(Delitos = sum(Delitos, na.rm=T))
  

Victimas <- read.csv("E:/SciData/Bases de datos y técnicas de graficación/BDTG_SEP25/Bases de datos/Estatal-Víctimas-2015-2025_sep2025.csv",
                     check.names = F,
                     fileEncoding = "latin1")

Victimas2 <- Victimas %>% 
  select(-Clave_Ent,
         -Entidad,
         -`Bien jurídico afectado`,
         -`Tipo de delito`,
         -`Subtipo de delito`,
         - Modalidad,
         -Sexo,
         -`Rango de edad`
  ) %>% 
  data.table() %>% 
  melt(id.vars = c("Año"),
       value.name = "Víctimas",
       variable.name = "Meses") %>% 
  group_by(Año) %>% 
  summarise(Víctimas = sum(Víctimas, na.rm=T))


#==============================================================#
# Interfaz del usuario (UI)                                ----
#==============================================================#

ui <- fluidPage(
  titlePanel(
    div(
      style = "display: flex; align-items: center; justify-content: center;",
      tags$img(
        src = "logo2.png",
        height = "60px",
        style = "margin-right: 15px"
      ),
      span(
        style = "text-align: center; font-size: 28px; font-weight: bold; color: #2C3E50;",
        "Análisis de Delitos y Víctimas en México de 2015 a 2025"
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width=2,
      h4("Navegación"),
      radioButtons(
        inputId = "pagina",
        label = NULL,
        choices = c("Delitos",
                    "Víctimas"),
        selected = "Delitos"
      )
    ),
    mainPanel(
      width = 10,
      uiOutput("contenido_pagina")
    )
  )
)


#==============================================================#
# Servidor (server)                                         ----
#==============================================================#

server <- function(input, output, session){
  
  # Contenido del UI en server ya que se evalua el cambio de página
  
  output$contenido_pagina <- renderUI({
    if(input$pagina == "Delitos"){
      tabsetPanel(
        tabPanel("Gráfico",
                 h3("Gráfico de los delitos"),
                 plotOutput("plot_delitos", height = "480px")),
        tabPanel("Tabla de datos",
                 h3("Tabla de los delitos"),
                 DTOutput("tabla_delitos"))
      )
    } else if(input$pagina == "Víctimas"){
      tabsetPanel(
        tabPanel("Gráfico",
                 h3("Gráfico de las víctimas"),
                 plotOutput("plot_victimas", height = "480px")),
        tabPanel("Tabla de datos",
                 h3("Tabla de las víctimas"),
                 DTOutput("tabla_victimas"))
      )
    }
  })
  
  # Estructura de salidas (del UI)
  
  Delit <- reactive({
    Delitos2
  })
  
  Vic <- reactive({
    Victimas2
  })
  
  output$plot_delitos <- renderPlot({
    
    Delit() %>% 
      ggplot(aes(x = Año, y=Delitos))+
      geom_line(color = "#1C69A8",
                linewidth = 2.5)+
      geom_point(size = 4,
                 color = "#1C69A8")+
      geom_text(aes(label = scales::comma(Delitos, big.mark = " ")),
                vjust = -1)+
      scale_x_continuous(breaks = seq(2015,2025,1))+
      labs(
        title = "<b style = 'color:#E7180B'> Nivel de delitos del fuero común registrados</b><b style = 'color:#1C69A8'> en las carpetas de investigación</b>",
        subtitle = "<i style = 'color:#E7180B'> del 2015 al 2025 <sup>1</sup></i>",
        caption = paste0("<span style = 'color:#E7180B'>",
                         "Fuente. SESNSP Incidencia delictiva.<br>",
                         "1",
                         str_dup("<span> </span>", 6),
                         "Para el año 2025 los datos son hasta el mes de septiembre</span>"),
        x = "<span style = 'color:#E7180B'> Año</span>",
        y = ""
      )+
      theme(plot.title = element_markdown(size = 16),
            plot.subtitle = element_markdown(size = 14),
            plot.caption = element_markdown(hjust = 0,
                                            size = 12),
            axis.title.x = element_markdown(size = 13),
            axis.text.y = element_blank(),
            axis.text.x = element_text(color = "#1C69A8",
                                       face = "bold",
                                       size = 13),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = alpha("#7CB342",0.5),
                                              linetype = "dashed"),
            panel.background = element_rect(fill = "white"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "#7CB342"))
    
  })
  
  output$tabla_delitos <- renderDT({
    datatable(Delit(), 
              rownames = F, 
              extensions = "Buttons",
              options = list(pageLength = 10,
                             dom = "Bfrtip",
                             buttons = list(
                               list(extend = "copy"),
                               list(extend = "csv", filename = "Delitos"),
                               list(extend = "excel", filename = "Delitos"),
                               list(extend = "pdf", filename = "Delitos"),
                               list(extend = "print")
                             )))
  })
  
  output$plot_victimas <- renderPlot({
    
    Vic() %>% 
      ggplot(aes(x = Año, y=Víctimas))+
      geom_line(color = "#1C69A8",
                linewidth = 2.5)+
      geom_point(size = 4,
                 color = "#1C69A8")+
      geom_text(aes(label = scales::comma(Víctimas, big.mark = " ")),
                vjust = -1)+
      scale_x_continuous(breaks = seq(2015,2025,1))+
      labs(
        title = "<b style = 'color:#E7180B'> Número de víctimas registras en los delitos de </b><b style = 'color:#1C69A8'>las carpetas de investigación</b>",
        subtitle = "<i style = 'color:#E7180B'> del 2015 al 2025 <sup>1</sup></i>",
        caption = paste0("<span style = 'color:#E7180B'>",
                         "Fuente. SESNSP Incidencia delictiva.<br>",
                         "1",
                         str_dup("<span> </span>", 6),
                         "Para el año 2025 los datos son hasta el mes de septiembre</span>"),
        x = "<span style = 'color:#E7180B'> Año</span>",
        y = ""
      )+
      theme(plot.title = element_markdown(size = 16),
            plot.subtitle = element_markdown(size = 14),
            plot.caption = element_markdown(hjust = 0,
                                            size = 12),
            axis.title.x = element_markdown(size = 13),
            axis.text.y = element_blank(),
            axis.text.x = element_text(color = "#1C69A8",
                                       face = "bold",
                                       size = 13),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = alpha("#7CB342",0.5),
                                              linetype = "dashed"),
            panel.background = element_rect(fill = "white"),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "#7CB342")) 
    
  })
  
  output$tabla_victimas <- renderDT({
    datatable(Vic(), rownames = F, 
              extensions = "Buttons",
              options = list(pageLength = 10,
                             dom = "Bfrtip",
                             buttons = list(
                               list(extend = "copy"),
                               list(extend = "csv", filename = "Víctimas"),
                               list(extend = "excel", filename = "Víctimas"),
                               list(extend = "pdf", filename = "Víctimas"),
                               list(extend = "print")
                             )))
  })
  
}



#==============================================================#
# Ejecución de app                                         ----
#==============================================================#

shinyApp(ui = ui, server = server)

