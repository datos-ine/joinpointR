# joinpointR
<img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->
<!-- badges: end -->

El objetivo de **joinpointR** es ajustar modelos de regresión joinpoint por grupos y generar resúmenes en formato *tidy* del Cambio Porcentual Anual (APC) y del Cambio Porcentual Anual Promedio (AAPC), facilitando el análisis de tendencias en estudios epidemiológicos.

---

## Instalación
Podés instalar la versión en desarrollo desde GitHub con:

```r
# install.packages("pak")
pak::pak("datos-ine/joinpointR")
```
Repositorio: https://github.com/datos-ine/joinpointR

## Flujo de trabajo
El paquete propone un flujo simple y reproducible:

* Ajustar modelos joinpoint por grupo
* Extraer APC por segmento
* Calcular AAPC
* Generar tablas resumen (tidy o formateadas)

### Funciones principales:
`model_jp()` → ajusta modelos joinpoint por grupo
`get_apc()` → obtiene APC por segmento
`get_aapc()` → calcula AAPC
`summary_jp()` → genera tablas resumen (tibble o flextable)

## Ejemplo completo
```r
library(joinpointR)
library(dplyr)

# Datos de ejemplo
df <- tibble(
  anio = rep(2000:2010, 2),
  tasa = c(runif(11, 10, 20), runif(11, 5, 15)),
  sexo = rep(c("Varones", "Mujeres"), each = 11)
)

# 1. Ajustar modelos joinpoint
mods <- model_jp(
  data = df,
  value = "tasa",
  time = "anio",
  group = "sexo"
)

# 2. Extraer APC por segmento
get_apc(mods[[1]])

# 3. Calcular AAPC
get_aapc(mods[[1]])

# 4. Tabla resumen (formato tidy)
tabla <- summary_jp(mods)

tabla
```

### Tabla formateada
```r
summary_jp(mods, flextable = TRUE)
```

Devuelve una tabla lista para exportar a Word o informes mediante `flextable`.

### Salida

La tabla generada incluye:

* Número de joinpoints (JP)
* Períodos de cada segmento
* APC por segmento
* Intervalos de confianza al 95%
* AAPC (tendencia global)
* Estructura de la salida (tibble)

La función `summary_jp()` devuelve un tibble con las siguientes columnas:

* Grupo (y opcionalmente Subgrupo)
* JP
* Periodo
* APC
* IC
* AAPC

Esto permite:

* Exportar a Excel
* Graficar tendencias
* Integrar en otros análisis

## Dependencias
El paquete utiliza:

* `segmented` para regresión joinpoint
* `dplyr`, `purrr`, `tidyr`, `tibble` para manipulación de datos
* `flextable` para tablas formateadas

## Notas
* La variable respuesta se transforma logarítmicamente
* La selección de modelos se basa en el Criterio de Información Bayesiano (BIC)
* Los resultados se devuelven en formato tidy para facilitar su uso en análisis reproducibles
* La creación de tablas formateadas es opcional `(flextable = TRUE)`

## Licencia
MIT License

## Autora
Tamara Ricardo