#====================================================#
# Diplomado: Bases de datos y técnicas de gráficación#
# Tema: Aplicaciones de Shiny                        #
# Subtema: Mi primer aplicación de Shiny             #
# Sesión: 18                                         #
# Fecha: 16/10/2025                                  #
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
  "ggthemes"
)

#==============================================================#
# Datos para la aplicación                                  ----
#==============================================================#

Data <- mtcars %>% 
  rownames_to_column(var = "modelo") %>% 
  mutate(cil = as.factor(cyl),
         am = as.factor(am)) 


#==============================================================#
# Interfaz del usuario (UI)                                ----
#==============================================================#

ui <- fluidPage(
  titlePanel("Mi primera app de shiny - SciData"),
  sidebarLayout(
    sidebarPanel(
      helpText("Controles básicos para filtrar y visualizar datos."),
      selectInput(
        inputId = "var_y",
        label = "Variable a mostrar (eje Y):",
        choices = c("mpg", "hp", "wt", "qsec"),
        selected = "mpg"
      ),
      selectInput(
        inputId = "color_by",
        label = "Color por:",
        choices = c("cil" = "cil", "am" = "am"),
        selected = "cil"
      ),
      sliderInput(
        inputId = "hp_range",
        label = "Rango de HP:",
        min = min(Data$hp),
        max = max(Data$hp),
        value = c(min(Data$hp), max(Data$hp))
      ),
      br(),
      actionButton("reset", "Restablecer filtros")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico",
                 br(),
                 plotOutput("plot_main", height = "450px")
        ),
        tabPanel("Tabla",
                 br(),
                 DTOutput("table_main"))
      )
    )
  )
)


#==============================================================#
# Servidor (server)                                         ----
#==============================================================#

server <- function(input, output, session){
  
  # Filtrado con base en los rangos de hp (del UI)
  
  datos_filtrados <- reactive({
    req(input$var_y)
    Data %>% 
      filter(hp >= input$hp_range[1], hp <= input$hp_range[2])
  })
  
  # Observador para el botón de reset 
  
  observeEvent(input$reset, {
    updateSelectInput(session, "var_y", selected = "mpg")
    updateSelectInput(session, "color_by", selected = "cil")
    updateSliderInput(session, "hp_range",
                      value = c(min(Data$hp), max(Data$hp)))
  })
  
  # Gráfico principal 
  
  output$plot_main <- renderPlot({
    d <- datos_filtrados()
    ggplot(d, aes_string(x = "modelo",
                         y = input$var_y,
                         fill = input$color_by))+
      geom_col(position = position_dodge())+
      coord_flip()+
      labs(
        x = NULL,
        y = input$var_y,
        fill = input$color_by,
        title = paste("Distribución de", input$var_y, "por modelo")
      )+
      theme_economist_white()
  })
  
  # Tabla interactiva 
  
  output$table_main <- renderDT({
    d <- datos_filtrados() %>% 
      select(modelo, cyl, hp, mpg, wt, qsec, am)
    datatable(d, rownames = F, options = list(pageLength = 10))
  })
  
}



#==============================================================#
# Ejecución de app                                         ----
#==============================================================#

shinyApp(ui = ui, server = server)
