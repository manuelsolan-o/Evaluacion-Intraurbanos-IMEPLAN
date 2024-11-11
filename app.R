# Lista de paquetes necesarios
required_packages <- c("shiny", "sf", "bslib", "qgisprocess", "terra", "dplyr")

# Función para instalar paquetes faltantes
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Instalar los paquetes faltantes
install_if_missing(required_packages)

# Cargar las librerías necesarias
library(shiny)
library(sf)
library(bslib) # Para agregar temas personalizados

# Cargar el script de funciones
source("aux_evaluacion.R")

library(qgisprocess)  # Conexión con QGIS
library(terra)        # Manipulación de raster
library(sf)           # Manipulación de shapefiles
library(dplyr)        # Manipulación de datos

evaluacion <- function(
    equip_layer_path,
    nombre_equipamiento,
    radio,
    pesos_equipamientos,
    manzanas_layer_path,
    pesos_manzanas,
    constante_norm
) {
  # Paso 1: Calcular la oferta
  oferta <- calcular_oferta(
    equip_layer_path = equip_layer_path,
    nombre_equipamiento = nombre_equipamiento,
    radio = radio,
    pesos_equipamientos = pesos_equipamientos
  )
  
  # Paso 2: Calcular la demanda
  demanda <- calcular_demanda(
    manzanas_layer_path = manzanas_layer_path,
    nombre_equipamiento = nombre_equipamiento,
    radio = radio,
    pesos_manzanas = pesos_manzanas
  )
  
  # Paso 3: Calcular el índice normalizado
  indice <- indice_norm(
    nombre_equipamiento = nombre_equipamiento,
    constante_norm = constante_norm
  )
  
  # Paso 4: Generar los resultados finales
  resultado <- results(
    nombre_equipamiento = nombre_equipamiento,
    manzanas_layer_path = manzanas_layer_path
  )
  
}

# Define UI para la aplicación con tema
ui <- fluidPage(
  # Añadir un tema Bootstrap con bslib
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#007bff",
    secondary = "#6c757d"
  ),
  
  # Banner con el logo de la empresa
  tags$div(
    style = "text-align: center; padding: 10px; background-color: #f8f9fa;",
    tags$img(src = "LOGO IMEPLAN MR.png", height = "60px", alt = "Logo")
  ),
  
  titlePanel("Evaluación de Equipamientos Intraurbanos"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("equip_layer_path", "Selecciona el archivo de equipamiento (.zip)", accept = ".zip"),
      textInput("nombre_equipamiento", "Nombre del Equipamiento", value = "preparatorias"),
      numericInput("radio", "Radio de Análisis (en metros)", value = 1100),
      textInput("pesos_equipamientos", "Pesos de Equipamientos", value = "EXISTENTES"),
      fileInput("manzanas_layer_path", "Selecciona el archivo de manzanas (.zip)", accept = ".zip"),
      textInput("pesos_manzanas", "Pesos de Manzanas", value = "demanda"),
      numericInput("constante_norm", "Constante de Normalización", value = -2.5),
      actionButton("run_evaluation", "Ejecutar Evaluación")
    ),
    
    mainPanel(
      # Estilos de CSS para la barra de progreso
      tags$style(
        HTML("
          .shiny-progress {
            position: fixed;
            top: 50%;
            right: 25%;
            transform: translate(50%, -50%);
            width: 40%;
            z-index: 9999;
          }
          .shiny-progress .bar { 
            height: 40px; 
          }
          .shiny-progress .progress-text { 
            font-size: 20px; 
          }
        ")
      ),
      verbatimTextOutput("result_output")
    )
  )
)

# Define la lógica del servidor
server <- function(input, output) {
  observeEvent(input$run_evaluation, {
    withProgress(message = "Ejecutando Evaluación", value = 0, {
      # Paso 1: Descomprimir el archivo de equipamiento
      incProgress(0.2, detail = "Descomprimiendo archivo de equipamiento...")
      equip_zip <- input$equip_layer_path$datapath
      equip_dir <- tempfile()
      unzip(equip_zip, exdir = equip_dir)
      
      equip_shp <- list.files(equip_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
      
      # Paso 2: Descomprimir el archivo de manzanas
      incProgress(0.2, detail = "Descomprimiendo archivo de manzanas...")
      manzanas_zip <- input$manzanas_layer_path$datapath
      manzanas_dir <- tempfile()
      unzip(manzanas_zip, exdir = manzanas_dir)
      
      manzanas_shp <- list.files(manzanas_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
      
      # Verificar si se encontraron los archivos .shp
      if (length(equip_shp) == 0 || length(manzanas_shp) == 0) {
        output$result_output <- renderPrint("Error: No se encontró el archivo .shp dentro del zip")
        return(NULL)
      }
      
      # Paso 3: Ejecutar la función de evaluación
      incProgress(0.4, detail = "Ejecutando función de evaluación...")
      resultado <- tryCatch({
        evaluacion(
          equip_layer_path = equip_shp,
          nombre_equipamiento = input$nombre_equipamiento,
          radio = input$radio,
          pesos_equipamientos = input$pesos_equipamientos,
          manzanas_layer_path = manzanas_shp,
          pesos_manzanas = input$pesos_manzanas,
          constante_norm = input$constante_norm
        )
      }, error = function(e) {
        paste("Error en la ejecución:", e$message)
      })
      
      # Paso 4: Mostrar el resultado
      incProgress(0.2, detail = "Mostrando el resultado...")
      output$result_output <- renderPrint({
        resultado
      })
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
