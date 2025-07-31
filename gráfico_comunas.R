library(dplyr)
library(readr)
library(ggplot2)
library(scales)

number_options(decimal.mark = ",", big.mark = ".") # opciones de números grandes

ingresos <- read_csv2("datos/mideso_ingresos_genero.csv")


ingresos

library(camcorder)

camcorder::gg_record(dir = "gráficos/grabación/",
                     width = 8,
                     height = 8,
                     device = "jpeg")

ingresos |> 
  distinct(variable)



# gráfico ----
ingresos_filt <- ingresos |> 
  select(-1) |> 
  filter(variable == "Promedio ingreso imponible de la población en edad de trabajar") |>
  mutate(region = stringr::str_remove(region,
                                      c(" del General Carlos Ibáñez del Campo",
                                      " y de la Antártica Chilena",
                                      "Libertador General Bernardo ",
                                      " de Santiago") |> stringr::str_c(collapse = "|"))
         )
  # filter(region %in% c("Coquimbo", "Los Ríos", "Ñuble"))
# filter(region %in% c("Coquimbo"))
ingresos_filt |> distinct(region)

ingresos_wide <- ingresos_filt |> 
  tidyr::pivot_wider(names_from = genero, values_from = valor) |> 
  clean_names() |> 
  mutate(brecha = (masculino/femenino)-1) |> 
  mutate(mitad = (masculino+femenino)/2) |> 
  rowwise() |> 
  mutate(mayor = max(c(masculino, femenino))) |> 
  group_by(region) |> 
  slice_max(brecha, n = 5, with_ties = FALSE)

comunas_filtrar <- ingresos_wide |> pull(comuna)

ingresos_filt |> 
  filter(comuna %in% comunas_filtrar) |> 
  ggplot() +
  aes(x = valor, y = comuna, color = genero) +
  geom_segment(data = ingresos_wide,inherit.aes = F,
               aes(x = masculino, xend = femenino, y = comuna, yend = comuna),
               linewidth = 0.8) +
  geom_point(size = 2) + geom_point(size = 4, alpha = 0.2, show.legend = F) +
  geom_point(data = ingresos_wide,inherit.aes = F,
             aes(x = mitad, y = comuna),
             shape = "|", size = 4) +
  geom_text(data = ingresos_wide,inherit.aes = F,
             aes(x = mayor+60000, y = comuna,
                 label = percent(prefix = "+", brecha, accuracy = 0.1, drop0trailing = TRUE)), 
            size = 2.6, hjust = 0) +
  scale_x_continuous(labels = label_currency(scale = 0.001, drop0trailing = TRUE),
                     name = "Ingresos (miles de pesos)") +
  guides(color = guide_legend(position = "top")) +
  coord_cartesian(clip = "off") +
  facet_wrap(~region, scales = "free_y") +
  theme_minimal() +
  labs(y = NULL, color = "Género:") +
  theme(strip.text = element_text(face = "bold", size = 11),
        panel.spacing.x = unit(8, "mm"))
