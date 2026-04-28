#' Cambio Porcentual Anual Promedio (AAPC)
#'
#' Calcula el Cambio Porcentual Anual Promedio (AAPC) y su intervalo de confianza al 95%.
#'
#' @param mod Modelo de regresión joinpoint (objeto segmented o lm).
#' @param digits Número de decimales a mostrar (integer).
#'
#' @return String con AAPC e intervalo de confianza al 95%.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' get_aapc(mod, digits = 1)
#' }
get_aapc <- function(mod, digits = 1) {
  # ---- Validaciones ----
  if (!inherits(mod, c("segmented", "lm"))) {
    stop("`mod` debe ser un objeto de clase 'segmented' o 'lm'")
  }

  # ---- Helper de formato ----
  fmt <- function(x, y, z) {
    paste0(
      scales::percent(x, accuracy = 10^-digits, decimal.mark = ","),
      " (IC95%: ",
      scales::number(y, accuracy = 10^-digits, scale = 100, decimal.mark = ","),
      "; ",
      scales::number(z, accuracy = 10^-digits, scale = 100, decimal.mark = ","),
      ")"
    )
  }

  # ---- Cálculo ----
  if (inherits(mod, "segmented")) {
    aapc_obj <- segmented::aapc(mod)

    est <- unname(aapc_obj["Est."])
    lci <- unname(aapc_obj[3])
    uci <- unname(aapc_obj[4])
  } else {
    est <- exp(stats::coef(mod)[2]) - 1
    ci <- exp(stats::confint(mod)[2, ]) - 1

    lci <- ci[1]
    uci <- ci[2]
  }

  # ---- Salida única ----
  fmt(est, lci, uci)
}
