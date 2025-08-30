
library(dplyr)
library(readxl)
library(janitor)
# https://bidat.gob.cl/details/ficha/dataset/5384a6c7-628f-45b6-a918-6b28402affbb?page=1

# Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)
# Porcentaje de asalariados formales dependientes con ingreso imponible igual al ingreso mínimo
# Mediana del ingreso imponible de los asalariados dependientes
# Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana
# Promedio ingreso imponible de la población en edad de trabajar

# cargar ----
ruta_datos <- "datos/originales/indicadores_ingreso_anual_2023.xlsx"

# Porcentaje de asalariados formales dependientes con ingreso imponible igual al ingreso mínimo			
ingreso_minimo <- read_xlsx(ruta_datos,
                            sheet = 4,
                            skip = 2) |> 
  clean_names() |> 
  # filter(nivel == "comuna sexo") |> 
  filter(nivel %in% c("region", "comuna", "region sexo", "comuna sexo")) |> 
  mutate(variable = "Porcentaje de asalariados formales dependientes con ingreso imponible igual al ingreso mínimo")

# Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)	
ingreso_cotizados <- read_xlsx(ruta_datos,
                               sheet = 3,
                               skip = 2) |> 
  clean_names() |> 
  # filter(nivel == "comuna sexo") |> 
  filter(nivel %in% c("region", "comuna", "region sexo", "comuna sexo")) |> 
  mutate(variable = "Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)")

# Mediana del ingreso imponible de los asalariados dependientes	
ingreso_mediana <- read_xlsx(ruta_datos,
                             sheet = 5,
                             skip = 2) |> 
  clean_names() |> 
  # filter(nivel == "comuna sexo") |> 
  filter(nivel %in% c("region", "comuna", "region sexo", "comuna sexo")) |> 
  mutate(variable = "Mediana del ingreso imponible de los asalariados dependientes")

# Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana
ingreso_menor_mediana <- read_xlsx(ruta_datos,
                                   sheet = 6,
                                   skip = 2) |> 
  clean_names() |> 
  # filter(nivel == "comuna sexo") |> 
  filter(nivel %in% c("region", "comuna", "region sexo", "comuna sexo")) |> 
  mutate(variable = "Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana")

# Promedio ingreso imponible de la población en edad de trabajar			
ingreso_edad <- read_xlsx(ruta_datos,
                          sheet = 7,
                          skip = 2) |> 
  clean_names() |> 
  # filter(nivel == "comuna sexo") |> 
  filter(nivel %in% c("region", "comuna", "region sexo", "comuna sexo")) |> 
  mutate(variable = "Promedio ingreso imponible de la población en edad de trabajar")



# unir ----
ingresos <- bind_rows(ingreso_minimo,
                      ingreso_cotizados,
                      ingreso_mediana,
                      ingreso_menor_mediana,
                      ingreso_edad)


# comunas ----
ingresos_region <- ingresos |> 
  filter(nivel %in% c("region", "region sexo"))

ingresos_comuna <- ingresos |> 
  filter(nivel %in% c("comuna", "comuna sexo"))

cut_comunas <- readRDS("datos/cut_comunas.rds")


ingresos_comuna_2 <- ingresos_comuna |> 
  # corregir_comunas
  rename(comuna = desagregacion1,
         genero = desagregacion2) |> 
  mutate(comuna = case_match(comuna,
                             "Juan Fernandez" ~ "Juan Fernández",
                             "San Pedro de la Paz" ~ "San Pedro de La paz",
                             "O Higgins" ~ "O'Higgins",
                             "Treguaco" ~ "Trehuaco",
                             .default = comuna)) |> 
  # agregar datos de comunas
  left_join(cut_comunas, 
            join_by(comuna))


ingresos_region_2 <- ingresos_region |> 
  rename(region = desagregacion1,
         genero = desagregacion2) |> 
  # ordenar regiones
  mutate(codigo_region = case_match(region,
                                    "Tarapacá" ~ 1,
                                    "Antofagasta" ~ 2,
                                    "Atacama" ~ 3,
                                    "Coquimbo" ~ 4,
                                    "Valparaíso" ~ 5,
                                    "Libertador Bernardo O'Higgins" ~ 6,
                                    "Maule" ~ 7,
                                    "Bío Bío" ~ 8,
                                    "La Araucanía" ~ 9,
                                    "Los Lagos" ~ 10,
                                    "Aysén del General Carlos Ibañez del Campo" ~ 11,
                                    "Magallanes y la Antártica Chilena" ~ 12,
                                    "Metropolitana" ~ 13,
                                    "Los Ríos" ~ 14,
                                    "Arica y Parinacota" ~ 15,
                                    "Ñuble" ~ 16)) |> 
  select(-region) |>
  left_join(cut_comunas |> distinct(codigo_region, region),
            join_by(codigo_region))



ingresos_2 <- bind_rows(ingresos_comuna_2, ingresos_region_2)

ingresos_2 |> distinct(region)


# limpiar ----
ingresos_3 <- ingresos_2 |> 
  # recodificar
  # mutate(genero = case_match(desagregacion2, 
  #                            "Masculino" ~ "masculino",
  #                            "Femenino" ~ "femenino")) |> 
  
  # select(-nivel) |> 
  rename(valor = x2023) |> 
  # ordenar
  relocate(codigo_region, region, codigo_comuna, comuna, genero, variable, valor) |> 
  # ordenar regiones
  mutate(codigo_region = case_match(region,
                                    "Tarapacá" ~ 1,
                                    "Antofagasta" ~ 2,
                                    "Atacama" ~ 3,
                                    "Coquimbo" ~ 4,
                                    "Valparaíso" ~ 5,
                                    "Libertador Bernardo O'Higgins" ~ 6,
                                    "Maule" ~ 7,
                                    "Bío Bío" ~ 8,
                                    "La Araucanía" ~ 9,
                                    "Los Lagos" ~ 10,
                                    "Aysén del General Carlos Ibañez del Campo" ~ 11,
                                    "Magallanes y la Antártica Chilena" ~ 12,
                                    "Metropolitana" ~ 13,
                                    "Los Ríos" ~ 14,
                                    "Arica y Parinacota" ~ 15,
                                    "Ñuble" ~ 16)) |> 
  mutate(region = factor(region),
         region = forcats::fct_reorder(region, codigo_region)) |> 
  # acortar región
  mutate(region_corta = stringr::str_remove(region,
                                            c(" del General Carlos Ibáñez del Campo",
                                              " y de la Antártica Chilena",
                                              "Libertador General Bernardo ") |> stringr::str_c(collapse = "|"))
  ) |> 
  mutate(comuna_corta = case_when(comuna == "Pedro Aguirre Cerda" ~ "PAC",
                                  comuna == "San Pedro de La paz" ~ "San Pedro",
                                  .default = comuna))


# revisar ----
ingresos_3 |> 
  count(genero)

n_distinct(ingresos_3$comuna)

ingresos <- ingresos_3

# guardar ----
write.csv2(ingresos, "datos/mideso_ingresos_genero.csv")

arrow::write_parquet(ingresos, "app/mideso_ingresos_genero.parquet")
