library(shiny)
library(bslib)
library(dplyr)
library(arrow)
library(scales)
library(ggplot2)
library(forcats)
library(ggrepel)
library(tidyr)
library(stringr)
library(janitor)
library(shinycssloaders)

ingresos <- read_parquet("mideso_ingresos_genero.parquet")
variables <- unique(ingresos$variable)

color_claro = "#6C8CB4"
color_oscuro_b = "#1b263b"
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
    theme(panel.grid.major.y = element_line(colour = "#162033FF"),
          axis.ticks.x = element_blank()) 
)
# col_mix(color_oscuro, color_oscuro_b)

number_options(decimal.mark = ",", big.mark = ".") # opciones de números grandes

# agrega opciones para shinycssloaders con el color del tema
options(spinner.color = color_texto, 
        spinner.color.background = color_oscuro, 
        spinner.size = 0.6)


ui <- page_fluid(
  
  ## tema ----
  theme = bs_theme(
    fg = color_texto,
    bg = color_oscuro,
    primary = color_claro,
    font_scale = 0.9
  ),
  
  includeCSS("styles.css"),
  
  # borde de selector
  tags$style(
    HTML(".selectize-input {
             border: 1px solid", color_detalle_claro, "!important;
             background-color:", color_claro, "!important;
             color:", color_oscuro, "!important;
             }")),
  
  
  
  # header ----
  
  div(style = css(max_width = "1000px", margin = "auto", padding = "12px"),
      
      h2("Comparación de ingresos, Chile", 
         style = css(font_weight = "bold")),
      
      div(style = "font-size: 90%;",
          markdown("[_Bastián Olea Herrera_](https://bastianolea.rbind.io)")
      ),
      
      p("Esta plataforma visualiza datos de", strong("ingresos a nivel regional, comunal, y por género."), "Su objetivo es visibilizar diferencias de los ingresos recibidos por las y los trabajadores, permitiendo compararlos por regiones y comunas, así como también producir comparaciones de brechas de género entre",
        span("hombres", class = "text-label masc"),
        "y",
        span("mujeres.", class = "text-label fem")),
      
      markdown("Los datos provienen del Ministerio de Desarrollo Social y Familia (Mideso), a partir de registros administrativos del año 2023."),
      
      markdown("La fuente de los datos está en el [Banco Integrado de Datos](https://bidat.gob.cl/details/ficha/dataset/5384a6c7-628f-45b6-a918-6b28402affbb?page=1) de Mideso, y el código para procesar los datos y generar esta plataforma se encuentra en [éste repositorio](https://github.com/bastianolea/mideso_ingresos_genero)."),
      
      br(),
      
      
      
      # nacional ----
      
      selectInput("variable", 
                  strong("Seleccione una variable de ingreso:"), 
                  choices = variables, 
                  selected = "Promedio ingreso imponible de la población en edad de trabajar",
                  width = "100%"
      ),
      
      navset_pill(
        nav_panel("Nacional", 
                  div(style = "overflow-x: scroll;",
                      div(class = "grafico",
                          plotOutput("grafico_nacional") |> withSpinner()
                      ))
        ),
        nav_panel("Nacional por género", 
                  div(style = "overflow-x: scroll;",
                      div(class = "grafico",
                          plotOutput("grafico_nacional_genero") |> withSpinner()
                      ))
        )
      ),
      
      
      # regional ----
      h2("Región"),
      
      selectInput("region", 
                  strong("Seleccione una región:"), 
                  choices = unique(ingresos$region), 
                  selected = "Metropolitana de Santiago",
                  width = "100%"
      ),
       
      
      h3(textOutput("nombre_region")),
      
      
      
      navset_pill(
        nav_panel("Regional", 
                  div(style = "overflow-x: scroll;",
                      div(class = "grafico",
                          plotOutput("grafico_region") |> withSpinner()
                      ))
        ),
        nav_panel("Regional por género", 
                  div(style = "overflow-x: scroll;",
                      div(class = "grafico",
                          plotOutput("grafico_region_genero") |> withSpinner()
                      ))
        )
      ),
      
      ## firma ----
      
      ### firma ----
      div(style = "width: 100%;",
          div(style = "font-size: 90%; padding: 28px;",
              hr(),
              markdown("Desarrollado y programado por [Bastián Olea Herrera](https://bastianolea.rbind.io) en R."),
              
              markdown("Puedes explorar otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
              
              markdown("Código de fuente de esta app y del procesamiento de los datos [disponible en GitHub.](https://github.com/bastianolea/mideso_ingresos_genero)")
          )
          
      #     #### cafecito ----
      #     div(
      #       style = "width; 100%; margin: auto; padding: 28px",
      #       
      #       tags$style(HTML(".cafecito:hover {opacity: 75%; transition: 0.3s; color: black !important;} .cafecito a:hover {color: black}")),
      #       
      #       div(class = "cafecito",
      #           style = "transform:scale(0.6);",
      #           tags$body(HTML('<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="bastimapache" data-color="#FFDD00" data-emoji=""  data-font="Bree" data-text="Regálame un cafecito" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>'))
      #       )
      # )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # input$variable <- "Mediana del ingreso imponible de los asalariados dependientes"
  # input$variable <- "Promedio ingreso imponible de la población en edad de trabajar"
  
  
  ## variable ----
  ingresos_variable <- reactive({
    ingresos |> 
      filter(variable == input$variable)
  })
  
  nombre_variable <- reactive(
    str_wrap(input$variable, 40)
  )
  
  ## nacional ----
  
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
      geom_text(aes(label = region_corta),
                angle = 90, hjust = 1, vjust = 0.5, fontface = "bold",
                nudge_y = -max(ingresos_nacional()$valor)*0.04,
                size = 2.7, color = color_texto) +
      coord_cartesian(clip = "off") +
      # escalas
      scale_y_continuous(labels = label_number(accuracy = 1),
                         expand = expansion(c(0, 0.1))) +
      scale_x_discrete(labels = label_wrap(20),
                       expand = expansion(c(0.03, 0.1))) +
      labs(x = "Regiones del país", y = nombre_variable())
  })
  
  
  ## nacional género ----
  
  ingresos_nacional_genero <- reactive({
    ingresos_variable() |> 
      filter(nivel == "region sexo") |> 
      mutate(region = fct_reorder(region, valor,.desc = TRUE)) |> 
      arrange(region)
  })
  
  ingresos_nacional_genero_wide <- reactive({
    ingresos_nacional_genero() |> 
      tidyr::pivot_wider(names_from = genero, values_from = valor) |> 
      clean_names() |> 
      mutate(brecha = (masculino/femenino)-1) |> 
      mutate(mitad = (masculino+femenino)/2) |> 
      rowwise() |> 
      mutate(mayor = max(c(masculino, femenino))) |>
      mutate(menor = min(c(masculino, femenino))) |>
      group_by(region) |> 
      slice_max(brecha, n = 5, with_ties = FALSE)
  })
  
  output$grafico_nacional_genero <- renderPlot({
    datos <- ingresos_nacional_genero()
    datos_wide <- ingresos_nacional_genero_wide()
    
    datos |> 
      # slice(1:10) |> 
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
                aes(label = region_corta,
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
      labs(x = "Regiones del país, por género", y = nombre_variable()) +
      coord_cartesian(clip = "off") +
      guides(color = guide_legend(position = "bottom", title = NULL,
                                  theme = theme(legend.text = element_text(margin = margin(l = 1, r = 6))),
                                  override.aes = list(size = 4)))
  })
  
  
  ## regional ----
  
  ingresos_region <- reactive({
    ingresos_variable() |> 
      filter(region == input$region) |> 
      mutate(region = fct_reorder(region, valor,.desc = TRUE))
  })
  
  
  ingresos_comunas_region <- reactive({
    ingresos_region() |> 
      filter(nivel == "comuna") |> 
      mutate(comuna = fct_reorder(comuna, valor,.desc = TRUE)) |> 
      arrange(comuna) |> 
      mutate(id = 1:n())
  })
  
  ### texto ----
  
  nombre_region <- reactive({
    # poner "de" o "del" en regiones que lo llevan
    region_prefijo <- case_when(input$region %in% c("Metropolitana de Santiago") ~ "Región",
                                input$region %in% c("Libertador General Bernardo O'Higgins", "Maule", "Biobío") ~ "Región del",
                                .default = "Región de")
    
    nombre_region <- paste(region_prefijo, input$region)
    return(nombre_region)
  })
  
  output$nombre_region <- renderText(nombre_region())
  
  ### gráfico ----
  output$grafico_region <- renderPlot({
    # browser()
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
      labs(x = paste("Comunas de la", 
                     str_replace(nombre_region(), "Región", "región")), 
           y = nombre_variable()) +
      # facetas 
      facet_wrap(~grupo, nrow = 1, scales = "free_x") +
      theme(strip.text = element_text(color = color_texto, face = "bold.italic")) +
      theme(panel.spacing.x = unit(24, "mm")) +
      coord_cartesian(clip = "off", 
                      xlim = c(1, n_comunas_post+0.2)) +
      # texto separador entre facetas
      geom_text(data = tibble(comuna = 9.3, valor = mean(datos$valor),
                              grupo = "Mayores ingresos"),
                label = rep(". ", 18) |> paste(collapse =""),
                color = color_detalle_claro,
                hjust = 0.5, angle = 90) +
      theme(plot.margin = margin(4, 30, 4, 4))
  })
  
  
  ## regional género ----
  
  ingresos_comunas_region_genero <- reactive({
    ingresos_region() |> 
      filter(nivel == "comuna sexo") |> 
      group_by(comuna) |> 
      mutate(promedio = mean(valor)) |> 
      mutate(comuna = fct_reorder(comuna, promedio,.desc = TRUE)) |> 
      arrange(desc(promedio)) |> 
      ungroup() |> 
      mutate(id = dense_rank(desc(promedio)))
  })
  
  
  output$grafico_region_genero <- renderPlot({
    datos <- ingresos_comunas_region_genero()
    
    datos <- datos |> 
      mutate(grupo = case_when(id <= 8 ~ "Mayores ingresos",
                               id > max(id)-8 ~ "Menores ingresos")) |> 
      filter(!is.na(grupo))
    
    datos_wide <- datos |> 
      tidyr::pivot_wider(names_from = genero, values_from = valor) |> 
      clean_names() |> 
      mutate(brecha = (masculino/femenino)-1) |> 
      mutate(mitad = (masculino+femenino)/2) |> 
      rowwise() |> 
      mutate(mayor = max(c(masculino, femenino))) |>
      mutate(menor = min(c(masculino, femenino))) |>
      group_by(region)
    
    n_comunas_post <- if_else(nrow(datos) < 8, nrow(datos), 8)
    
    datos |> 
      ggplot() +
      aes(comuna, valor, color = genero) +
      # promedio
      geom_hline(yintercept = mean(datos$valor, na.rm = TRUE), 
                 linetype = "dotted", color = color_detalle_claro) +
      # segmento de regiones
      geom_segment(aes(xend = comuna, yend = 0), 
                   linewidth = 6, color = color_detalle_claro, alpha = 0.2) +
      # segmento entre géneros
      geom_segment(data = datos_wide, inherit.aes = F,
                   aes(x = comuna, xend = comuna, y = mayor, yend = menor), 
                   linewidth = 1, color = color_detalle_claro, alpha = 0.5) +
      geom_point(size = 6) +
      geom_point(size = 10, alpha = 0.2) +
      # # texto cifras
      # geom_text_repel(aes(label = valor |> round(0) |> signif(4) |> format(trim = T, big.mark = ".", decimal.mark = ",")),
      #                 hjust = 0, vjust = 0.5, color = color_texto, 
      #                 nudge_x = nrow(datos)*0.018, nudge_y = max(datos$valor)*0.04,
      #                 size = 3, angle = 45, direction = "y",
      #                 box.padding = 0.2, point.padding = 9, xlim = c(0, 20)) +
      # texto cifras
      geom_label(data = datos_wide, inherit.aes = F,
                 aes(label = brecha |> percent(accuracy = 0.1, prefix = "+"),
                     comuna, mayor),
                 hjust = 0.5, color = color_texto, fill = color_oscuro, 
                 label.size = 0, label.padding = unit(0.1, "lines"),
                 nudge_y = max(datos$valor)*0.05,
                 size = 3) +
      # # texto regiones
      # geom_text(aes(label = comuna),
      #           angle = 90, hjust = 1, vjust = 0.5, fontface = "bold",
      #           nudge_y = -max(datos$valor)*0.04,
      #           size = 2.7, color = color_texto) +
      # texto regiones
      geom_text(data = datos_wide, inherit.aes = F,
                aes(label = comuna_corta,
                    comuna, menor),
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
      scale_color_manual(values = c("Femenino" = color_femenino, "Masculino" = color_masculino)) +
      guides(color = guide_legend(position = "bottom", title = NULL,
                                  theme = theme(legend.text = element_text(margin = margin(l = 1, r = 6))),
                                  override.aes = list(size = 4))) +
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
