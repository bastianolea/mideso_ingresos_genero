library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(scales)
library(tidyr)
options(scipen = 9999)

# Helper function to replace janitor::clean_names
clean_names <- function(data) {
  names(data) <- tolower(names(data))
  names(data) <- gsub("[^a-z0-9_]", "_", names(data))
  data
}

# Load data
ingresos <- read_csv2("datos/mideso_ingresos_genero.csv") |> 
  select(-1)

# Color scheme from original code
color_claro = "#6C8CB4"
color_oscuro = "#121A2B"
color_detalle_oscuro = "#0d1b2a"
color_detalle_claro = "#415a77"
color_texto = "#A5B7CAFF"
color_femenino = "#986DB4"
color_masculino = "#5AB395"

# Set theme
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(color = color_texto),
      plot.background = element_rect(fill = color_oscuro, color = NA),
      panel.background = element_rect(fill = color_oscuro, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(color = "#415a77"),
      axis.text.y = element_text(color = "#415a77"),
      axis.title.x = element_text(color = "#415a77"),
      axis.title.y = element_text(color = "#415a77"),
      plot.title = element_text(hjust = 0.5, face = "bold", color = "#1b263b")
    ) +
    theme(axis.text.x = element_blank()) +
    theme(panel.grid = element_blank(),
          axis.ticks.x = element_blank()) 
)

# Function to create national plot (region level)
crear_grafico_nacional <- function(data, variable_seleccionada) {
  datos <- data |> 
    filter(nivel == "region",
           variable == variable_seleccionada) |> 
    mutate(region = fct_reorder(region, valor, .desc = TRUE)) |> 
    arrange(region)
  
  datos |> 
    ggplot() +
    aes(region, valor) +
    # promedio
    geom_hline(yintercept = mean(datos$valor, na.rm = TRUE), 
               linetype = "dotted", color = color_detalle_claro) +
    # segmento de regiones
    geom_segment(aes(xend = region, yend = 0), 
                 linewidth = 6, color = color_detalle_claro, alpha = 0.2) +
    geom_point(size = 6, color = color_claro) +
    geom_point(size = 10, color = color_claro, alpha = 0.2) +
    # texto cifras
    geom_text(aes(label = valor |> round(0) |> signif(4) |> format(trim = T, big.mark = ".", decimal.mark = ",")),
              hjust = 0, vjust = 0.5, color = color_texto, 
              nudge_x = nrow(datos)*0.018, nudge_y = max(datos$valor)*0.04,
              size = 3, angle = 45) +
    # texto regiones
    geom_text(aes(label = region),
              angle = 90, hjust = 1, vjust = 0.5, fontface = "bold",
              nudge_y = -max(datos$valor)*0.04,
              size = 2.7, color = color_texto) +
    # escalas
    scale_y_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(c(0, 0.1))) +
    scale_x_discrete(labels = label_wrap(20),
                     expand = expansion(c(0.03, 0.1))) +
    labs(x = NULL, y = variable_seleccionada, title = "Ingresos por Región")
}

# Function to create national gender plot
crear_grafico_nacional_genero <- function(data, variable_seleccionada) {
  datos <- data |> 
    filter(nivel == "region sexo",
           variable == variable_seleccionada) |> 
    mutate(region = fct_reorder(region, valor, .desc = TRUE)) |> 
    arrange(region)
  
  datos_wide <- datos |> 
    pivot_wider(names_from = genero, values_from = valor) |> 
    clean_names() |> 
    mutate(brecha = (masculino/femenino)-1) |> 
    mutate(mitad = (masculino+femenino)/2) |> 
    rowwise() |> 
    mutate(mayor = max(c(masculino, femenino))) |>
    mutate(menor = min(c(masculino, femenino))) |>
    group_by(region) |> 
    slice_max(brecha, n = 5, with_ties = FALSE)
  
  datos |> 
    ggplot() +
    aes(region, valor, color = genero) +
    # promedios por género
    geom_hline(yintercept = c(mean(datos_wide$femenino), mean(datos_wide$masculino)), 
               color = c(color_femenino, color_masculino),
               linetype = "dotted") +
    # segmento de regiones
    geom_segment(aes(xend = region, yend = 0), 
                 linewidth = 6, color = color_detalle_claro, alpha = 0.2) +
    # segmento entre géneros
    geom_segment(data = datos_wide, inherit.aes = F,
                 aes(x = region, xend = region, y = mayor, yend = menor), 
                 linewidth = 1, color = color_detalle_claro, alpha = 0.5) +
    geom_point(size = 6) +
    geom_point(size = 10, alpha = 0.2, show.legend = F) +
    # texto cifras
    geom_label(data = datos_wide, inherit.aes = F,
              aes(label = brecha |> percent(accuracy = 0.1, prefix = "+"),
                  region, mayor),
                    hjust = 0.5, color = color_texto, fill = color_oscuro, 
              label.size = 0, label.padding = unit(0.1, "lines"),
              nudge_y = max(datos$valor)*0.05,
                    size = 3) +
    # texto regiones
    geom_text(data = datos_wide, inherit.aes = F,
              aes(label = region,
                  region, menor),
              angle = 90, hjust = 1, vjust = 0.5, fontface = "bold",
              nudge_y = -max(datos$valor)*0.04,
              size = 2.7, color = color_texto) +
    # escalas
    scale_y_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(c(0, 0))) +
    scale_x_discrete(labels = label_wrap(20),
                     expand = expansion(c(0.03, 0.1))) +
    scale_color_manual(values = c("Femenino" = color_femenino, "Masculino" = color_masculino)) +
    labs(x = NULL, y = variable_seleccionada, title = "Ingresos por Región y Género") +
    coord_cartesian(clip = "off") +
    guides(color = guide_legend(position = "top", title = NULL,
                                theme = theme(legend.text = element_text(margin = margin(l = 1, r = 6))),
                                override.aes = list(size = 4)))
}

# Function to create regional plot (comuna level)
crear_grafico_regional <- function(data, variable_seleccionada, region_seleccionada) {
  datos <- data |> 
    filter(nivel == "comuna",
           region == region_seleccionada,
           variable == variable_seleccionada) |> 
    mutate(comuna = fct_reorder(comuna, valor, .desc = TRUE)) |> 
    arrange(comuna) |> 
    mutate(id = 1:n())
  
  if(nrow(datos) == 0) return(NULL)
  
  datos <- datos |> 
    mutate(grupo = case_when(id <= 8 ~ "Mayores ingresos",
                             id > max(id)-8 ~ "Menores ingresos")) |> 
    filter(!is.na(grupo))
  
  n_comunas_post <- if_else(nrow(datos) < 8, nrow(datos), 8)
  
  datos |> 
    ggplot() +
    aes(comuna, valor) +
    # promedio
    geom_hline(yintercept = mean(datos$valor, na.rm = TRUE), 
               linetype = "dotted", color = color_detalle_claro) +
    # segmento de regiones
    geom_segment(aes(xend = comuna, yend = 0), 
                 linewidth = 6, color = color_detalle_claro, alpha = 0.2) +
    geom_point(size = 6, color = color_claro) +
    geom_point(size = 10, color = color_claro, alpha = 0.2) +
    # texto cifras
    geom_text(aes(label = valor |> round(0) |> signif(4) |> format(trim = T, big.mark = ".", decimal.mark = ",")),
              hjust = 0, vjust = 0.5, color = color_texto, 
              nudge_x = nrow(datos)*0.018, nudge_y = max(datos$valor)*0.04,
              size = 3, angle = 45) +
    # texto regiones
    geom_text(aes(label = comuna),
              angle = 90, hjust = 1, vjust = 0.5, fontface = "bold",
              nudge_y = -max(datos$valor)*0.04,
              size = 2.7, color = color_texto) +
    # escalas
    scale_y_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(c(0, 0.1))) +
    scale_x_discrete(labels = label_wrap(20),
                     expand = expansion(c(0.05, 0.03))) +
    labs(x = NULL, y = variable_seleccionada, 
         title = paste("Ingresos por Comuna -", region_seleccionada)) +
    # facetas 
    facet_wrap(~grupo, nrow = 1, scales = "free_x") +
    theme(strip.text = element_text(color = color_texto, face = "italic")) +
    theme(panel.spacing.x = unit(12, "mm")) +
    coord_cartesian(clip = "off", 
                    xlim = c(1, n_comunas_post+0.2))
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(paste0("
      body { 
        background-color: ", color_oscuro, "; 
        color: ", color_texto, ";
      }
      .well { 
        background-color: ", color_detalle_oscuro, "; 
        border: 1px solid ", color_detalle_claro, ";
      }
      .selectize-input { 
        background: ", color_detalle_oscuro, "; 
        color: ", color_texto, ";
        border: 1px solid ", color_detalle_claro, ";
      }
      .selectize-dropdown { 
        background: ", color_detalle_oscuro, "; 
        color: ", color_texto, ";
        border: 1px solid ", color_detalle_claro, ";
      }
    ")))
  ),
  
  titlePanel("Análisis de Ingresos por Género en Chile"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", 
                  "Seleccionar Variable:",
                  choices = sort(unique(ingresos$variable[!is.na(ingresos$variable)])),
                  selected = "Promedio ingreso imponible de la población en edad de trabajar"),
      
      conditionalPanel(
        condition = "input.mostrar_regional == true",
        selectInput("region", 
                    "Seleccionar Región:",
                    choices = sort(unique(ingresos$region[!is.na(ingresos$region)])),
                    selected = "Metropolitana de Santiago")
      ),
      
      checkboxInput("mostrar_regional", 
                    "Mostrar análisis regional (por comuna)", 
                    value = FALSE),
      
      hr(),
      p("Esta aplicación muestra diferentes visualizaciones de los datos de ingresos por género en Chile."),
      p("Los gráficos incluyen:"),
      tags$ul(
        tags$li("Análisis nacional por región"),
        tags$li("Análisis nacional por región y género"),
        tags$li("Análisis regional por comuna (opcional)")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Nacional", 
                 h3("Análisis Nacional por Región"),
                 plotOutput("plot_nacional", height = "600px")),
        
        tabPanel("Nacional por Género", 
                 h3("Análisis Nacional por Región y Género"),
                 plotOutput("plot_nacional_genero", height = "600px")),
        
        tabPanel("Regional", 
                 h3("Análisis Regional por Comuna"),
                 conditionalPanel(
                   condition = "input.mostrar_regional == false",
                   p("Selecciona 'Mostrar análisis regional' en el panel lateral para ver este gráfico.")
                 ),
                 conditionalPanel(
                   condition = "input.mostrar_regional == true",
                   plotOutput("plot_regional", height = "600px")
                 ))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$plot_nacional <- renderPlot({
    req(input$variable)
    crear_grafico_nacional(ingresos, input$variable)
  })
  
  output$plot_nacional_genero <- renderPlot({
    req(input$variable)
    crear_grafico_nacional_genero(ingresos, input$variable)
  })
  
  output$plot_regional <- renderPlot({
    req(input$variable, input$region, input$mostrar_regional)
    if(input$mostrar_regional) {
      crear_grafico_regional(ingresos, input$variable, input$region)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)