#!/usr/bin/env Rscript

# Script para ejecutar la aplicación Shiny
# Usage: Rscript run_app.R

library(shiny)

# Ejecutar la aplicación
cat("Iniciando aplicación Shiny: Análisis de Ingresos por Género en Chile\n")
cat("La aplicación se abrirá en su navegador web predeterminado\n")
cat("Para detener la aplicación, presione Ctrl+C en la terminal\n\n")

runApp("app.R", launch.browser = TRUE)