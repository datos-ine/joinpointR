
# joinpointR 2.0 <img src="man/figures/logo.png" align="right" height="139" alt="" />


## 🇬🇧 English

The goal of **joinpointR** is to fit *joinpoint regression models* by groups and generate tidy summaries of the Annual Percent Change (APC) and the Average Annual Percent Change (AAPC), facilitating trend analysis in epidemiological studies.

---

## Installation

The development version of `joinpointR` can be installed from Github using the command:

``` r
remotes::install_github("https://github.com/datos-ine/joinpointR")
```

## Workflow
The package provides a simple and reproducible workflow:
* Fit joinpoint models by group using the grid-search method.
* Generate summary tables with the fitted joinpoints, annual percent change (APC) and its confidence interval (CI), and average annual percent change (AAPC) and its CI.
* Optionally, extract the APC, AAPC and Bayesian Information Criteria of a single model or a list of models.
* Generate summary plots.

## Main functions
* `model_jp_grid()` or `model_jp()` → Fits joinpoint regression models by groups of up to two categorical variables.
* `get_summary()`, `get_apc()`, and `get_aapc()` → Returns a table with summary statistics for a model or a list of models of class `"model_jp"`.
* `gg_jpoint()` → Generate summary plots for a model or a list of models of class `"model_jp"`.

## Example
``` r
# Load packages
library(joinpointR)

# Load data
data(hiv_data)

data_mod <- hiv_data |>
    dplyr::filter(admin == "ARG")

# Fit the joinpoint models
mods <- model_jp_grid(data = data_mod, rate = "hiv_rate", time = "year", group = "sex")

# BIC of the model
bic_jp(mods)

# Summary table
get_summary(mods)

# Plot results
gg_jpoint(mods)
``` 
## Notes
* The response variable is log-transformed.
* Model selection is based on the Bayesian Information Criterion (BIC). It can be changed to the penalized BIC (BIC3) or the weighted BIC (WBIC) using the argument `method`.
* Summary table results are returned in tidy format and can be transformed to `flextable` objects using the argument `as.ft = TRUE`.

## 🇪🇸 Español

El objetivo de **joinpointR** es ajustar modelos de regresión *joinpoint* por grupos y generar resúmenes en formato *tidy* del Cambio Porcentual Anual (APC) y del Cambio Porcentual Anual Promedio (AAPC), facilitando el análisis de tendencias en estudios epidemiológicos.

## Instalación

La versión en desarrollo de `joinpointR` se puede descargar desde Github con el comando:

``` r
remotes::install_github("https://github.com/datos-ine/joinpointR")
```

## Flujo de trabajo
El paquete cuenta con un flujo de trabajo simple y reproducible:
* Ajusta modelos de regresión joinpoint usando el método de *grid-search*.
* Genera tablas de resumen con los joinpoints detectados, el cambio porcentual anual (APC) y su intervalo de confianza (IC), y el cambio porcentual anual promedio (AAPC) y su IC.
* Opcionalmente, se puede extraer el APC, AAPC y Criterio de Información Bayesiano de un modelo o lista de modelos.
* Genera gráficos de resumen.

## Funciones principales
* `model_jp_grid()` y `model_jp()` → Ajustan modelos de regresión joinpoint según niveles de hasta dos variables categóricas.
* `get_summary()`, `get_apc()`, y `get_aapc()` → Devuelven una tabla resumen para un modelo o lista de modelos de clase `"model_jp"`.
* `gg_jpoint()` → Genera gráficos de resumen para un modelo o lista de modelos de clase `"model_jp"`.

## Ejemplo

``` r
# Cargar paquetes
library(joinpointR)

# Cargar datos
data(hiv_data)

data_mod <- hiv_data |>
    dplyr::filter(admin == "ARG")

# Ajustar modelos
mods <- model_jp_grid(data = data_mod, rate = "hiv_rate", time = "year", group = "sex")

# BIC
bic_jp(mods)

# Tabla resumen
get_summary(mods)

# Graficar resultados
gg_jpoint(mods)
``` 

## Notas
* La variable respuesta se transforma logarítmicamente.
* La selección de modelos está basada en el Criterio de Información Bayesian(BIC). Se puede cambiar al BIC penalizado (BIC3) o BIC ponderado (WBIC) usando el argumento `method`.
* Las tablas de resumen se presentan en formato *tidy* y pueden transformarse a objetos `flextable` usando el argument `as.ft = TRUE`.

## Licence / Licencia
MIT License

## Author / Autora
Tamara Ricardo
Instituto Nacional de Epidemiología (INE), Argentina
ORCID: https://orcid.org/0000-0002-0921-2611

