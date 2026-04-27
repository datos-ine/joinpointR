#' Cambio Porcentual Anual por segmento (APC)
#'
#' Calcula el Cambio Porcentual Anual (APC) por segmento y su intervalo de confianza al 95%.
#'
#' @param mod Modelo de regresión joinpoint (objeto segmented).
#' @param digits Número de decimales a mostrar (integer).
#' @param time Variable de tiempo usada en el modelo (character).
#'
#' @return Vector de strings con APC e IC95% por segmento.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' get_apc(mod, digits = 1, time = "anio")
#' }
get_apc <- function(mod, digits = 1, time = "anio") {
  fmt <- function(x, y, z) {
    paste0(
      scales::number(
        x,
        accuracy = 10^-digits,
        decimal.mark = ",",
        suffix = "%"
      ),
      " (IC95%: ",
      scales::number(y, accuracy = 10^-digits, decimal.mark = ","),
      ", ",
      scales::number(z, accuracy = 10^-digits, decimal.mark = ","),
      ")"
    )
  }

  segmented::slope(mod, APC = TRUE)[[time]] |>
    as.data.frame() |>
    dplyr::as_tibble() |>
    purrr::pmap_chr(~ fmt(..1, ..2, ..3))
}
