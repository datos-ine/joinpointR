#' Modelos de Regresión Joinpoint por grupos
#'
#' Ajusta modelos de regresión lineal segmentada por grupos para tasas estandarizadas por edad,
#' usando un proceso stepwise basado en el Criterio de Información Bayesiano (BIC).
#' Internamente llama la función segmented::selgmented() y aplica transformación logarítmica
#' a la variable respuesta.
#'
#' @param data Dataframe conteniendo las tasas estandarizadas por edad.
#' @param value Variable respuesta (character).
#' @param tiempo Variable de tiempo (character).
#' @param grupo Variable de agrupación (character).
#' @param k Número máximo de joinpoints.
#'
#' @return Lista de modelos por grupo.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' model_jp(data = df, value = "tasa", tiempo = "anio", grupo = "sexo")
#' }
model_jp <- function(data, value, grupo, tiempo, k = 2) {
  # ---- Validaciones ----
  if (!is.data.frame(data)) {
    stop("`data` debe ser un data.frame")
  }

  vars <- c(grupo, tiempo, value)

  if (!all(vars %in% names(data))) {
    stop("Alguna de las variables no existe en `data`")
  }

  if (any(data[[value]] <= 0, na.rm = TRUE)) {
    stop("La variable respuesta debe ser > 0 para aplicar log()")
  }

  # ---- Fórmula ----
  formula <- stats::reformulate(
    termlabels = tiempo,
    response = paste0("log(", value, ")")
  )

  # ---- Modelado ----
  grupos <- unique(data[[grupo]])

  modelos <- data |>
    dplyr::group_by(.data[[grupo]]) |>
    dplyr::group_map(
      ~ segmented::selgmented(
        olm = stats::lm(formula, data = .x),
        Kmax = k,
        type = "bic",
        th = 2,
        stop.if = 4,
        check.dslope = TRUE
      )
    )

  # ---- Nombrar salida ----
  rlang::set_names(modelos, grupos)
}
