
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
  filter(nivel == "comuna sexo") |> 
  mutate(variable = "Porcentaje de asalariados formales dependientes con ingreso imponible igual al ingreso mínimo")

# Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)	
ingreso_cotizados <- read_xlsx(ruta_datos,
                               sheet = 3,
                               skip = 2) |> 
  clean_names() |> 
  filter(nivel == "comuna sexo") |> 
  mutate(variable = "Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)")

# Mediana del ingreso imponible de los asalariados dependientes	
ingreso_mediana <- read_xlsx(ruta_datos,
                             sheet = 5,
                             skip = 2) |> 
  clean_names() |> 
  filter(nivel == "comuna sexo") |> 
  mutate(variable = "Mediana del ingreso imponible de los asalariados dependientes")

# Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana
ingreso_menor_mediana <- read_xlsx(ruta_datos,
                                   sheet = 6,
                                   skip = 2) |> 
  clean_names() |> 
  filter(nivel == "comuna sexo") |> 
  mutate(variable = "Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana")

# Promedio ingreso imponible de la población en edad de trabajar			
ingreso_edad <- read_xlsx(ruta_datos,
                          sheet = 7,
                          skip = 2) |> 
  clean_names() |> 
  filter(nivel == "comuna sexo") |> 
  mutate(variable = "Promedio ingreso imponible de la población en edad de trabajar")



# unir ----
ingresos <- bind_rows(ingreso_minimo,
                      ingreso_cotizados,
                      ingreso_mediana,
                      ingreso_menor_mediana,
                      ingreso_edad)


# comunas ----
cut_comunas <- readRDS("datos/cut_comunas.rds")


ingresos_2 <- ingresos |> 
  # corregir_comunas
  rename(comuna = desagregacion1) |> 
  mutate(comuna = case_match(comuna,
                             "Juan Fernandez" ~ "Juan Fernández",
                             "San Pedro de la Paz" ~ "San Pedro de La paz",
                             "O Higgins" ~ "O'Higgins",
                             "Treguaco" ~ "Trehuaco",
                             .default = comuna)) |> 
  # agregar datos de comunas
  left_join(cut_comunas, 
            join_by(comuna))



# limpiar ----
ingresos_3 <- ingresos_2 |> 
  # recodificar
  # mutate(genero = case_match(desagregacion2, 
  #                            "Masculino" ~ "masculino",
  #                            "Femenino" ~ "femenino")) |> 
  rename(genero = desagregacion2) |> 
  select(-nivel) |> 
  rename(valor = x2023) |> 
  # ordenar
  relocate(codigo_region, region, codigo_comuna, comuna, genero, variable, valor)


# revisar ----
ingresos_3 |> 
  count(genero)

n_distinct(ingresos_3$comuna)


# guardar ----
ingresos_3 |> 
  write.csv2("datos/mideso_ingresos_genero.csv")
