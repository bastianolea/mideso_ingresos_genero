library(shiny)
library(bslib)
library(dplyr)
library(arrow)
library(scales)
library(ggplot2)

ingresos <- read_parquet("mideso_ingresos_genero.parquet")
variables <- unique(ingresos$variable)

color_claro = "#6C8CB4"
color_oscuro = "#1b263b"
color_oscuro = "#121A2B"
color_detalle_oscuro = "#0d1b2a"
color_detalle_claro = "#415a77"
color_texto = "#A5B7CAFF"

color_femenino = "#986DB4"
color_masculino = "#5AB395"

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      text = element_text(color = color_texto),
      plot.background = element_rect(fill = color_oscuro, color = NA),
      panel.background = element_rect(fill = color_oscuro, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(color = color_detalle_claro),
      axis.text.y = element_text(color = color_detalle_claro),
      axis.title.x = element_text(color = color_detalle_claro),
      axis.title.y = element_text(color = color_detalle_claro),
      plot.title = element_text(hjust = 0.5, face = "bold", color = color_oscuro)
    ) +
    theme(axis.text.x = element_blank()) +
    theme(panel.grid = element_blank(),
          axis.ticks.x = element_blank()) 
)

number_options(decimal.mark = ",", big.mark = ".") # opciones de números grandes

ui <- page_fluid(
  theme = bs_theme(
    fg = color_texto,
    bg = color_oscuro
  ),
  
  selectInput("variable", 
              "Seleccione variable:", 
              choices = variables, 
              selected = "Promedio ingreso imponible de la población en edad de trabajar"
  ),
  
  plotOutput("grafico_nacional"),
  
  h2("Región"),
  
  selectInput("region", 
              "Seleccione región:", 
              choices = unique(ingresos$region), 
              selected = "Metropolitana de Santiago"
  ),
  
  plotOutput("grafico_region")
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # .variable <- "Mediana del ingreso imponible de los asalariados dependientes"
  # .variable <- "Promedio ingreso imponible de la población en edad de trabajar"
  
  ingresos_variable <- reactive({
    ingresos |> 
      filter(variable == input$variable)
  })
  
  ingresos_nacional <- reactive({
    ingresos_variable() |> 
      filter(nivel == "region") |> 
      mutate(region = fct_reorder(region, valor,.desc = TRUE)) |> 
      arrange(region)
  })
  
  
  output$grafico_nacional <- renderPlot({
    ingresos_nacional() |> 
      ggplot() +
      aes(region, valor) +
      # promedio
      geom_hline(yintercept = mean(ingresos_nacional()$valor, na.rm = TRUE), 
                 linetype = "dotted", color = color_detalle_claro) +
      # segmento de regiones
      geom_segment(aes(xend = region, yend = 0), 
                   linewidth = 6, color = color_detalle_claro, alpha = 0.2) +
      geom_point(size = 6, color = color_claro) +
      geom_point(size = 10, color = color_claro, alpha = 0.2) +
      # texto cifras
      geom_text_repel(aes(label = valor |> round(0) |> signif(4) |> format(trim = T, big.mark = ".", decimal.mark = ",")),
                      hjust = 0, vjust = 0.5, color = color_texto, 
                      nudge_x = nrow(ingresos_nacional())*0.018, nudge_y = max(ingresos_nacional()$valor)*0.04,
                      size = 3, angle = 45, direction = "y",
                      box.padding = 0.2, point.padding = 9, xlim = c(0, 20)) +
      # texto regiones
      geom_text(aes(label = region),
                angle = 90, hjust = 1, vjust = 0.5, fontface = "bold",
                nudge_y = -max(ingresos_nacional()$valor)*0.04,
                size = 2.7, color = color_texto) +
      # escalas
      scale_y_continuous(labels = label_number(accuracy = 1),
                         expand = expansion(c(0, 0.1))) +
      scale_x_discrete(labels = label_wrap(20),
                       expand = expansion(c(0.03, 0.1))) +
      labs(x = NULL, y = input$variable)
  })
  
  
  
  ingresos_region <- reactive({
    ingresos_variable() |> 
      filter(region == input$region) |> 
      mutate(region = fct_reorder(region, valor,.desc = TRUE))
  })
  
  
  ingresos_comunas_region <- reactive({
    ingresos_region() |> 
      filter(nivel == "comuna") |> 
      arrange(comuna) |> 
      mutate(id = 1:n())
  })
  
  
  
  
  output$grafico_region <- renderPlot({
    
    datos <- ingresos_comunas_region() |> 
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
      geom_text_repel(aes(label = valor |> round(0) |> signif(4) |> format(trim = T, big.mark = ".", decimal.mark = ",")),
                      hjust = 0, vjust = 0.5, color = color_texto, 
                      nudge_x = nrow(datos)*0.018, nudge_y = max(datos$valor)*0.04,
                      size = 3, angle = 45, direction = "y",
                      box.padding = 0.2, point.padding = 9, xlim = c(0, 20)) +
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
      labs(x = NULL, y = input$variable) +
      # facetas 
      facet_wrap(~grupo, nrow = 1, scales = "free_x") +
      theme(strip.text = element_text(color = color_texto, face = "italic")) +
      theme(panel.spacing.x = unit(12, "mm")) +
      coord_cartesian(clip = "off", 
                      xlim = c(1, n_comunas_post+0.2)) +
      # texto separador entre facetas
      geom_text(data = tibble(comuna = 8.9, valor = mean(datos$valor),
                              grupo = "Mayores ingresos"),
                label = rep(". ", 3) |> paste(collapse =""),
                color = color_detalle_claro,
                hjust = 0.5, angle = 90) +
      theme(plot.margin = margin(4, 30, 4, 4))
  })
  
}


shinyApp(ui = ui, server = server)
