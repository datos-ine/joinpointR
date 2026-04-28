#' Cambio Porcentual Anual Promedio (AAPC)
#'
#' Calcula el Cambio Porcentual Anual Promedio (AAPC) y su intervalo de confianza al 95%.
#'
#' @param mod Modelo de regresión joinpoint (objeto segmented o lm).
#' @param digits Número de decimales a mostrar (integer).
#' @param show_ci Mostrar estrellas de significancia o intervalo de confianza al 95% (logical)
#'
#' @return String con AAPC con significancia o intervalo de confianza.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' get_aapc(mod, digits = 1, show_ci = FALSE)
#' }
get_aapc <- function(mod, digits = 1, show_ci = FALSE) {
  # ---- Validaciones ----
  if (!inherits(mod, c("segmented", "lm"))) {
    stop("`mod` debe ser un objeto de clase 'segmented' o 'lm'")
  }

  # ---- Helpers ----
  fmt_ci <- function(x, y, z) {
    paste0(
      scales::percent(x, accuracy = 10^-digits, decimal.mark = ","),
      " (IC95%: ",
      scales::number(y, accuracy = 10^-digits, scale = 100, decimal.mark = ","),
      "; ",
      scales::number(z, accuracy = 10^-digits, scale = 100, decimal.mark = ","),
      ")"
    )
  }

  fmt_stars <- function(x, stars) {
    paste0(
      scales::percent(x, accuracy = 10^-digits, decimal.mark = ","),
      stars
    )
  }

  get_stars <- function(est, se) {
    z <- abs(est / se)

    if (z > 3.29) {
      "***" # p < 0.001
    } else if (z > 2.58) {
      "**" # p < 0.01
    } else if (z > 1.96) {
      "*" # p < 0.05
    } else {
      ""
    }
  }

  # ---- Cálculo ----
  if (inherits(mod, "segmented")) {
    aapc_obj <- segmented::aapc(mod)

    est <- unname(aapc_obj["Est."])
    se <- unname(aapc_obj["St.Err"])
    lci <- unname(aapc_obj["CI(95%).l"])
    uci <- unname(aapc_obj["CI(95%).u"])
  } else {
    est <- exp(stats::coef(mod)[2]) - 1
    ci <- exp(stats::confint(mod)[2, ]) - 1
    se <- summary(mod)$coefficients[2, "Std. Error"]

    lci <- ci[1]
    uci <- ci[2]
  }

  # ---- Output ----
  if (show_ci) {
    fmt_ci(est, lci, uci)
  } else {
    stars <- get_stars(est, se)

    fmt_stars(est, stars)
  }
}
