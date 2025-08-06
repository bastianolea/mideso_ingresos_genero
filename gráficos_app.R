library(dplyr)
library(readr)
library(forcats)
library(scales)
options(scipen = 9999)
options(pillar.sigfig = 6)
number_options(decimal.mark = ",", big.mark = ".") # opciones de números grandes

library(camcorder)
library(ggplot2)
library(ggrepel)

ingresos <- read_csv2("datos/mideso_ingresos_genero.csv") |> 
  select(-1)

# Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)
# Porcentaje de asalariados formales dependientes con ingreso imponible igual al ingreso mínimo
# Mediana del ingreso imponible de los asalariados dependientes
# Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana
# Promedio ingreso imponible de la población en edad de trabajar


# temas ----

color_claro = "#6C8CB4"
# col_saturate(color_claro, amount = 10) |> show_col()
color_oscuro = "#1b263b"
# color_oscuro |> show_col()
# color_oscuro |> col_saturate(5) |> col_darker(5) |> show_col()
color_oscuro = "#121A2B"
color_detalle_oscuro = "#0d1b2a"
color_detalle_claro = "#415a77"
color_texto = "#A5B7CAFF"

# c("#0d1b2a","#1b263b","#415a77","#778da9","#e0e1dd")

color_femenino = "#986DB4"
# color_masculino = "#6DB49C"
color_masculino = "#5AB395"

c(color_masculino |> col_darker(4) |> col_saturate(5),
  color_masculino) |> show_col()

# col_mix("#e0e1dd", 
#         "#778da9", 
#         amount = 0.5) |> 
#   col_saturate(amount = 10) |> scales::show_col()

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

# nacional ----
camcorder::gg_record(dir = "gráficos/grabación_2/",
                     width = 8,
                     height = 6,
                     device = "jpeg")

.variable <- "Mediana del ingreso imponible de los asalariados dependientes"
.variable <- "Promedio ingreso imponible de la población en edad de trabajar"

datos <- ingresos |> 
  filter(nivel == "region",
         variable == .variable) |> 
  mutate(region = fct_reorder(region, valor,.desc = TRUE)) |> 
  arrange(region)


datos |> 
  # slice(1:10) |> 
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
  geom_text_repel(aes(label = valor |> round(0) |> signif(4) |> format(trim = T, big.mark = ".", decimal.mark = ",")),
            hjust = 0, vjust = 0.5, color = color_texto, 
            nudge_x = nrow(datos)*0.018, nudge_y = max(datos$valor)*0.04,
            size = 3, angle = 45, direction = "y",
            box.padding = 0.2, point.padding = 9, xlim = c(0, 20)) +
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
  labs(x = NULL, y = .variable)


camcorder::gg_playback(image_resize = 1280, 
                       last_as_first = FALSE,
                       first_image_duration = 5,
                       last_image_duration = 15,
                       frame_duration = 0.1,
                       background = color_oscuro)



# regiones ----
camcorder::gg_record(dir = "gráficos/grabación_3/",
                     width = 8,
                     height = 6,
                     device = "jpeg")

.variable <- "Mediana del ingreso imponible de los asalariados dependientes"
.variable <- "Promedio ingreso imponible de la población en edad de trabajar"

unique(ingresos$region)
.region <- "Valparaíso"
.region <- "Biobío" 
.region <- "Metropolitana de Santiago" 
.region <- "Ñuble"
.region <- "Arica y Parinacota"
.region <- "Tarapacá"


datos <- ingresos |> 
  filter(nivel == "comuna",
         region == .region,
         variable == .variable) |> 
  mutate(comuna = fct_reorder(comuna, valor,.desc = TRUE)) |> 
  arrange(comuna) |> 
  mutate(id = 1:n())


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
  labs(x = NULL, y = .variable) +
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
  

camcorder::gg_playback(image_resize = 1280, 
                       last_as_first = F,
                       first_image_duration = 5,
                       last_image_duration = 15,
                       frame_duration = 0.1,
                       background = color_oscuro)



# nacional género ----

camcorder::gg_record(dir = "gráficos/grabación_4/",
                     width = 8,
                     height = 6,
                     device = "jpeg")

.variable <- "Mediana del ingreso imponible de los asalariados dependientes"
.variable <- "Promedio ingreso imponible de la población en edad de trabajar"

datos <- ingresos |> 
  filter(nivel == "region sexo",
         variable == .variable) |> 
  mutate(region = fct_reorder(region, valor,.desc = TRUE)) |> 
  arrange(region)

datos_wide <- datos |> 
tidyr::pivot_wider(names_from = genero, values_from = valor) |> 
  clean_names() |> 
  mutate(brecha = (masculino/femenino)-1) |> 
  mutate(mitad = (masculino+femenino)/2) |> 
  rowwise() |> 
  mutate(mayor = max(c(masculino, femenino))) |>
  mutate(menor = min(c(masculino, femenino))) |>
  group_by(region) |> 
  slice_max(brecha, n = 5, with_ties = FALSE)

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
  labs(x = NULL, y = .variable) +
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(position = "top", title = NULL,
                              theme = theme(legend.text = element_text(margin = margin(l = 1, r = 6))),
                              override.aes = list(size = 4)))


camcorder::gg_playback(image_resize = 1280, 
                       last_as_first = F,
                       first_image_duration = 5,
                       last_image_duration = 15,
                       frame_duration = 0.1,
                       background = color_oscuro)
