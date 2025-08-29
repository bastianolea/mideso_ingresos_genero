# Aplicación Shiny: Análisis de Ingresos por Género en Chile

Esta aplicación interactiva permite explorar los datos de ingresos por género en Chile de forma visual e intuitiva.

## Funcionalidades

### 1. Selección de Variables
La aplicación permite seleccionar entre 5 variables de ingreso:
- Mediana del ingreso imponible de los asalariados dependientes
- Porcentaje de asalariados formales dependientes con ingreso imponible igual al ingreso mínimo
- Porcentaje de asalariados formales dependientes con ingreso imponible menor al 50% de la mediana
- Promedio ingreso imponible de asalariados formales dependientes (meses cotizados)
- Promedio ingreso imponible de la población en edad de trabajar

### 2. Tipos de Análisis

#### Nacional
- **Análisis Nacional por Región**: Muestra los valores de la variable seleccionada para todas las regiones de Chile
- **Análisis Nacional por Región y Género**: Incluye la comparación entre géneros (Masculino/Femenino) con cálculo de brechas salariales

#### Regional
- **Análisis Regional por Comuna**: Cuando se activa la opción "Mostrar análisis regional", permite seleccionar una región específica y ver el análisis por comunas dentro de esa región, divididas en "Mayores ingresos" y "Menores ingresos"

### 3. Filtros Interactivos
- **Variable**: Dropdown para seleccionar la variable de ingreso
- **Región**: Dropdown para seleccionar la región (cuando está activo el análisis regional)
- **Checkbox**: Para activar/desactivar el análisis regional por comunas

## Cómo Usar la Aplicación

1. **Ejecutar la aplicación**:
   ```r
   shiny::runApp("app.R")
   ```

2. **Navegar por las pestañas**:
   - **Nacional**: Vista general por regiones
   - **Nacional por Género**: Comparación de géneros por regiones
   - **Regional**: Análisis detallado por comunas (requiere activar la casilla correspondiente)

3. **Interactuar con los controles**:
   - Seleccione diferentes variables para ver cómo cambian los gráficos
   - Active el análisis regional para explorar datos a nivel comunal
   - Cambie de región para ver diferentes áreas geográficas

## Características Técnicas

- **Framework**: Shiny para R
- **Visualización**: ggplot2 con tema personalizado
- **Datos**: Basado en el archivo `datos/mideso_ingresos_genero.csv`
- **Interactividad**: Reactiva a los cambios de inputs del usuario
- **Diseño**: Interfaz responsive con tema oscuro

## Dependencias

```r
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(scales)
library(tidyr)
```

## Estilo Visual

La aplicación mantiene el estilo visual del código original con:
- Tema oscuro
- Colores específicos para géneros (Masculino: verde, Femenino: púrpura)
- Gráficos consistentes con el análisis original
- Tipografía y espaciado optimizados para la visualización