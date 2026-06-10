# joinpointR

## 🇪🇸 Español

El objetivo de **joinpointR** es ajustar modelos de regresión *joinpoint* por grupos y generar resúmenes en formato *tidy* del Cambio Porcentual Anual (APC) y del Cambio Porcentual Anual Promedio (AAPC), facilitando el análisis de tendencias en estudios epidemiológicos.

---

## 🇬🇧 English

The goal of **joinpointR** is to fit *joinpoint regression models* by groups and generate tidy summaries of the Annual Percent Change (APC) and the Average Annual Percent Change (AAPC), facilitating trend analysis in epidemiological studies.

---

## Installation / Instalación

You can install the development version from GitHub / Podés instalar la versión en desarrollo desde GitHub:

```r
# install.packages("pak")
pak::pak("datos-ine/joinpointR")
```

Repository: https://github.com/datos-ine/joinpointR

## Workflow / Flujo de trabajo

The package provides a simple and reproducible workflow / El paquete propone un flujo simple y reproducible:

* Fit joinpoint models by group / Ajustar modelos joinpoint por grupo
* Extract APC by segment / Extraer APC por segmento
* Compute AAPC / Calcular AAPC
* Generate summary tables / Generar tablas resumen
* Generate summary plots / Generar gráficos de resumen

## Main functions / Funciones principales
* `model_jp()` → fits joinpoint models by group / ajusta modelos joinpoint por grupo
* `get_apc()` → extracts APC by segment / extrae APC por segmento
* `get_aapc()` → computes AAPC / computa AAPC
* `summary_jp()` → generates summary tables (tibble) / genera tablas resumen (tibble)
* `jp_to_ft()` → transforms summary tables into flextable objects / transforma tablas de resumen a objetos flextable
* `gg_jpoint()`→ generates summary plots / genera gráficos de resumen

## Example / Ejemplo
```r
library(joinpointR)
library(dplyr)

data("hiv_data")

mods <- model_jp(
  data = hiv_data,
  value = hiv_rate,
  time = year,
  group = "region",
  step = TRUE
)

# APC (only works when class segmented lm)
get_apc(mods$Central, digits = 1, time = "year", dec = ".")

# AAPC with 95% CI
get_aapc(mods$Central, show_ci = TRUE)

# AAPC with significance stars
get_aapc(mods$Central, show_ci = FALSE)

# Summary Table
summary_jp(mods)

# Transform to flextable
summary_jp(mods) |>
jp_to_ft()

# Generate summary plot
gg_jpoint(mods)
```

### Formatted table / Tabla formateada
```r
# English (default)
summary_jp(mods) |>
jp_to_ft()

# Spanish
summary_jp(mods) |>
jp_to_ft(lan = "es")

```

Returns a table ready for reporting (e.g., Word) using flextable. / Devuelve una tabla lista para exportar a Word o informes mediante `flextable`.

### Output/Salida

The generated table includes / La tabla generada incluye:

* Number of joinpoints / Número de joinpoints (JP)
* Time periods for each segment / Períodos de cada segmento
* APC per segment / por segmento
* 95% Confidence intervals / Intervalos de confianza al 95%
* AAPC (global tendency / tendencia global)

### Plots / Gráficos
```r
# Plot results
mods |>
  gg_jpoint(obs = TRUE, jp = TRUE)

# Stack plots
mods |>
  gg_jpoint(obs = TRUE, jp = TRUE, facets = "none")

# Hide observed
mods |>
  gg_jpoint(obs = FALSE, jp = TRUE, facets = "none")

# Hide joinpoints
mods |>
  gg_jpoint(obs = TRUE, jp = FALSE, facets = "none")
```

## Dependencies / Dependencias
The package uses / El paquete utiliza:

* `segmented` for fitting jointpoint regression models / para regresión joinpoint
* `dplyr`, `purrr`, `tidyr`, `tibble` for data management / para manipulación de datos
* `ggplot2` for plotting results / para graficar resultados
* `flextable` for summary tables / para tablas formateadas

## Notes / Notas
* The response variable is log-transformed / La variable respuesta se transforma logarítmicamente
* Model selection is based on the Bayesian Information Criterion (BIC) / La selección de modelos se basa en el Criterio de Información Bayesiano (BIC)
* When `step = FALSE` fits a joinpoint regression model with the number of joinpoints specified in `k`/ Cuando `step = FALSE` ajusta una regresión joinpoint para el número de joinpoints especificados en `k`.
* Results are returned in tidy format / Los resultados se devuelven en formato tidy para facilitar su uso en análisis reproducibles

## Licence / Licencia
MIT License

## Author / Autora
Tamara Ricardo
Instituto Nacional de Epidemiología (INE), Argentina
ORCID: https://orcid.org/0000-0002-0921-2611
