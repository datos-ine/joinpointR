# joinpointR
<img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->
<!-- badges: end -->

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

## Main functions / Funciones principales
* `model_jp()` → fit joinpoint models by group / ajusta modelos joinpoint por grupo
* `get_apc()` → extract APC by segment / extrae APC por segmento
* `get_aapc()` → compute AAPC / computa AAPC
* `summary_jp()` → generate summary tables (tibble or flextable) / genera tablas resumen (tibble o flextable)

## Example / Ejemplo
```r
library(joinpointR)
library(dplyr)

df <- tibble(
  year = rep(2000:2010, 2),
  rate = c(runif(11, 10, 20), runif(11, 5, 15)),
  group = rep(c("Male", "Female"), each = 11)
)

mods <- model_jp(
  data = df,
  value = "rate",
  time = "year",
  group = "group"
)

# APC (only works when class segmented lm)
get_apc(mods$Male, digits = 1, time = "year", dec = ".") # Will generate an error

get_apc(mods$Female, digits = 1, time = "year", dec = ".")

# AAPC with 95% CI
get_aapc(mods$Male, show_ci = TRUE)

# AAPC with significance stars
get_aapc(mods$Male, show_ci = FALSE)

# Summary Table
summary_jp(mods)
```

### Formatted table / Tabla formateada
```r
# English (default)
summary_jp(mods, ft = TRUE, lan = "en", var1 = "Sex")

# Spanish
summary_jp(mods, ft = TRUE, lan = "es", var1 = "Sexo")

```

Returns a table ready for reporting (e.g., Word) using flextable. / Devuelve una tabla lista para exportar a Word o informes mediante `flextable`.

### Output/Salida

The generated table includes / La tabla generada incluye:

* Number of joinpoints / Número de joinpoints (JP)
* Time periods for each segment / Períodos de cada segmento
* APC per segment / por segmento
* 95% Confidence intervals / Intervalos de confianza al 95%
* AAPC (global tendency / tendencia global)

## Dependencies / Dependencias
The package uses / El paquete utiliza:

* `segmented` for fitting jointpoint regression models / para regresión joinpoint
* `dplyr`, `purrr`, `tidyr`, `tibble` for data management / para manipulación de datos
* `flextable` for summary tables / para tablas formateadas

## Notes / Notas
* The response variable is log-transformed / La variable respuesta se transforma logarítmicamente
* Model selection is based on the Bayesian Information Criterion (BIC) / La selección de modelos se basa en el Criterio de Información Bayesiano (BIC)
* Results are returned in tidy format / Los resultados se devuelven en formato tidy para facilitar su uso en análisis reproducibles
* Formatted tables are optional / La creación de tablas formateadas es opcional `(ft = TRUE)`

## Licence / Licencia
MIT License

## Author / Autora
Tamara Ricardo
Instituto Nacional de Epidemiología (INE), Argentina
ORCID: https://orcid.org/0000-0002-0921-2611