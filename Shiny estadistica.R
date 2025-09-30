library(shiny)
library(ggplot2)
library(DT)

# UI
ui <- fluidPage(
  titlePanel("Análisis de Varianza (ANOVA)"),
  
  tabsetPanel(
    # Tab 1: Teoría
    tabPanel("Teoría",
             fluidRow(
               column(12,
                      h3("¿Qué es ANOVA?"),
                      p("El Análisis de Varianza (ANOVA) es una técnica estadística que permite comparar las medias de tres o más grupos para determinar si existen diferencias significativas entre ellos."),
                      
                      h4("Hipótesis"),
                      tags$ul(
                        tags$li(HTML("<b>H₀:</b> Todas las medias poblacionales son iguales (μ₁ = μ₂ = ... = μₖ)")),
                        tags$li(HTML("<b>H₁:</b> Al menos una media difiere de las demás"))
                      ),
                      
                      h4("Supuestos del ANOVA"),
                      tags$ol(
                        tags$li("Independencia de las observaciones"),
                        tags$li("Normalidad: Los datos en cada grupo siguen una distribución normal"),
                        tags$li("Homocedasticidad: Las varianzas de los grupos son iguales")
                      ),
                      
                      h4("Estadístico F"),
                      p("El estadístico F se calcula como:"),
                      withMathJax(),
                      p("$$F = \\frac{\\text{Varianza entre grupos}}{\\text{Varianza dentro de grupos}} = \\frac{MSB}{MSW}$$"),
                      
                      p("Donde:"),
                      tags$ul(
                        tags$li("MSB = Cuadrado medio entre grupos"),
                        tags$li("MSW = Cuadrado medio dentro de grupos")
                      ),
                      
                      h4("Tabla ANOVA"),
                      tableOutput("tabla_anova_teorica")
               )
             )
    ),
    
    # Tab 2: Simulación
    tabPanel("Simulación",
             sidebarLayout(
               sidebarPanel(
                 h4("Configuración de Grupos"),
                 sliderInput("n_grupos", "Número de grupos:", 
                             min = 2, max = 5, value = 3, step = 1),
                 
                 sliderInput("n_obs", "Observaciones por grupo:", 
                             min = 5, max = 50, value = 20, step = 5),
                 
                 hr(),
                 
                 uiOutput("parametros_grupos"),
                 
                 hr(),
                 
                 sliderInput("alpha", "Nivel de significancia (α):", 
                             min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                 
                 actionButton("simular", "Generar Datos", 
                              class = "btn-primary btn-block"),
                 
                 br(),
                 downloadButton("descargar_datos", "Descargar Datos")
               ),
               
               mainPanel(
                 h4("Gráfico de Cajas"),
                 plotOutput("boxplot", height = "350px"),
                 
                 h4("Tabla ANOVA"),
                 tableOutput("tabla_anova"),
                 
                 h4("Resultados"),
                 verbatimTextOutput("resultados"),
                 
                 h4("Comparaciones Post-Hoc (Tukey HSD)"),
                 verbatimTextOutput("tukey_test"),
                 
                 h4("Datos Generados"),
                 DTOutput("tabla_datos")
               )
             )
    ),
    
    # Tab 3: Cargar Datos
    tabPanel("Cargar Datos",
             sidebarLayout(
               sidebarPanel(
                 h4("Cargar archivo CSV"),
                 fileInput("archivo", "Seleccionar archivo CSV:",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 
                 checkboxInput("header", "El archivo tiene encabezado", TRUE),
                 
                 radioButtons("sep", "Separador:",
                              choices = c(Coma = ",", Punto_y_coma = ";", Tabulador = "\t"),
                              selected = ","),
                 
                 hr(),
                 
                 uiOutput("select_variables"),
                 
                 sliderInput("alpha_carga", "Nivel de significancia (α):", 
                             min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                 
                 actionButton("analizar", "Analizar", class = "btn-primary btn-block")
               ),
               
               mainPanel(
                 h4("Vista previa de datos"),
                 DTOutput("vista_previa"),
                 
                 h4("Gráfico de Cajas"),
                 plotOutput("boxplot_carga", height = "350px"),
                 
                 h4("Tabla ANOVA"),
                 tableOutput("tabla_anova_carga"),
                 
                 h4("Resultados"),
                 verbatimTextOutput("resultados_carga"),
                 
                 h4("Comparaciones Post-Hoc (Tukey HSD)"),
                 verbatimTextOutput("tukey_carga")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tabla teórica ANOVA
  output$tabla_anova_teorica <- renderTable({
    data.frame(
      "Fuente de Variación" = c("Entre grupos", "Dentro de grupos", "Total"),
      "Suma de Cuadrados" = c("SSB", "SSW", "SST"),
      "Grados de Libertad" = c("k - 1", "N - k", "N - 1"),
      "Cuadrado Medio" = c("MSB = SSB/(k-1)", "MSW = SSW/(N-k)", ""),
      "F" = c("F = MSB/MSW", "", "")
    )
  }, striped = TRUE, hover = TRUE)
  
  # UI dinámica para parámetros de grupos
  output$parametros_grupos <- renderUI({
    n <- input$n_grupos
    lapply(1:n, function(i) {
      tagList(
        h5(paste("Grupo", i)),
        sliderInput(paste0("media_", i), "Media:", 
                    min = 0, max = 100, value = 50 + (i-1)*5, step = 1),
        sliderInput(paste0("sd_", i), "Desviación estándar:", 
                    min = 1, max = 20, value = 10, step = 1)
      )
    })
  })
  
  # Datos simulados
  datos_sim <- eventReactive(input$simular, {
    n_grupos <- input$n_grupos
    n_obs <- input$n_obs
    
    datos_list <- lapply(1:n_grupos, function(i) {
      media <- input[[paste0("media_", i)]]
      sd <- input[[paste0("sd_", i)]]
      valores <- rnorm(n_obs, mean = media, sd = sd)
      data.frame(
        Grupo = paste("Grupo", i),
        Valor = valores
      )
    })
    
    do.call(rbind, datos_list)
  })
  
  # Boxplot simulación
  output$boxplot <- renderPlot({
    datos <- datos_sim()
    
    ggplot(datos, aes(x = Grupo, y = Valor, fill = Grupo)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
      theme_minimal() +
      labs(title = "Comparación de Grupos",
           x = "Grupo",
           y = "Valor") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  # ANOVA simulación
  resultado_anova <- reactive({
    datos <- datos_sim()
    aov(Valor ~ Grupo, data = datos)
  })
  
  # Tabla ANOVA simulación
  output$tabla_anova <- renderTable({
    anova_result <- summary(resultado_anova())[[1]]
    
    df_anova <- data.frame(
      "Fuente" = c("Entre grupos", "Dentro de grupos"),
      "SC" = round(anova_result$`Sum Sq`, 3),
      "gl" = anova_result$Df,
      "CM" = round(anova_result$`Mean Sq`, 3),
      "F" = round(c(anova_result$`F value`[1], NA), 3),
      "p-valor" = round(c(anova_result$`Pr(>F)`[1], NA), 4)
    )
    
    df_anova
  }, striped = TRUE, hover = TRUE, digits = 4)
  
  # Resultados simulación
  output$resultados <- renderPrint({
    anova_result <- summary(resultado_anova())[[1]]
    p_valor <- anova_result$`Pr(>F)`[1]
    f_valor <- anova_result$`F value`[1]
    alpha <- input$alpha
    
    cat("Estadístico F:", round(f_valor, 4), "\n")
    cat("p-valor:", round(p_valor, 4), "\n")
    cat("Nivel de significancia (α):", alpha, "\n\n")
    
    if (p_valor < alpha) {
      cat("CONCLUSIÓN: Se rechaza H₀\n")
      cat("Existe evidencia suficiente para concluir que al menos una media difiere de las demás.\n")
    } else {
      cat("CONCLUSIÓN: No se rechaza H₀\n")
      cat("No existe evidencia suficiente para concluir que las medias difieren.\n")
    }
  })
  
  # Test de Tukey simulación
  output$tukey_test <- renderPrint({
    tukey_result <- TukeyHSD(resultado_anova())
    print(tukey_result)
  })
  
  # Tabla de datos simulación
  output$tabla_datos <- renderDT({
    datatable(datos_sim(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Descargar datos
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("datos_anova_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datos_sim(), file, row.names = FALSE)
    }
  )
  
  # CARGAR DATOS
  datos_cargados <- reactive({
    req(input$archivo)
    
    tryCatch({
      df <- read.csv(input$archivo$datapath,
                     header = input$header,
                     sep = input$sep)
      return(df)
    },
    error = function(e) {
      return(NULL)
    })
  })
  
  # Vista previa
  output$vista_previa <- renderDT({
    req(datos_cargados())
    datatable(datos_cargados(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Seleccionar variables
  output$select_variables <- renderUI({
    req(datos_cargados())
    df <- datos_cargados()
    
    tagList(
      selectInput("var_grupo", "Variable de grupo (categórica):",
                  choices = names(df)),
      selectInput("var_valor", "Variable de respuesta (numérica):",
                  choices = names(df))
    )
  })
  
  # Datos para análisis
  datos_analisis <- eventReactive(input$analizar, {
    req(datos_cargados(), input$var_grupo, input$var_valor)
    
    df <- datos_cargados()
    datos <- data.frame(
      Grupo = as.factor(df[[input$var_grupo]]),
      Valor = as.numeric(df[[input$var_valor]])
    )
    
    datos <- datos[complete.cases(datos), ]
    return(datos)
  })
  
  # Boxplot carga
  output$boxplot_carga <- renderPlot({
    datos <- datos_analisis()
    
    ggplot(datos, aes(x = Grupo, y = Valor, fill = Grupo)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 2) +
      theme_minimal() +
      labs(title = "Comparación de Grupos",
           x = "Grupo",
           y = "Valor") +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  # ANOVA carga
  resultado_anova_carga <- reactive({
    datos <- datos_analisis()
    aov(Valor ~ Grupo, data = datos)
  })
  
  # Tabla ANOVA carga
  output$tabla_anova_carga <- renderTable({
    anova_result <- summary(resultado_anova_carga())[[1]]
    
    df_anova <- data.frame(
      "Fuente" = c("Entre grupos", "Dentro de grupos"),
      "SC" = round(anova_result$`Sum Sq`, 3),
      "gl" = anova_result$Df,
      "CM" = round(anova_result$`Mean Sq`, 3),
      "F" = round(c(anova_result$`F value`[1], NA), 3),
      "p-valor" = round(c(anova_result$`Pr(>F)`[1], NA), 4)
    )
    
    df_anova
  }, striped = TRUE, hover = TRUE, digits = 4)
  
  # Resultados carga
  output$resultados_carga <- renderPrint({
    anova_result <- summary(resultado_anova_carga())[[1]]
    p_valor <- anova_result$`Pr(>F)`[1]
    f_valor <- anova_result$`F value`[1]
    alpha <- input$alpha_carga
    
    cat("Estadístico F:", round(f_valor, 4), "\n")
    cat("p-valor:", round(p_valor, 4), "\n")
    cat("Nivel de significancia (α):", alpha, "\n\n")
    
    if (p_valor < alpha) {
      cat("CONCLUSIÓN: Se rechaza H₀\n")
      cat("Existe evidencia suficiente para concluir que al menos una media difiere de las demás.\n")
    } else {
      cat("CONCLUSIÓN: No se rechaza H₀\n")
      cat("No existe evidencia suficiente para concluir que las medias difieren.\n")
    }
  })
  
  # Tukey carga
  output$tukey_carga <- renderPrint({
    tukey_result <- TukeyHSD(resultado_anova_carga())
    print(tukey_result)
  })
}

# Run app
shinyApp(ui = ui, server = server)

