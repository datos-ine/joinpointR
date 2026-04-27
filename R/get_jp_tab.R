#' Tabla Joinpoint
#'
#' Genera una tabla formateada con APC, AAPC, períodos y número de joinpoints
#' a partir de una lista de modelos joinpoint.
#'
#' @param mod Lista de modelos (salida de model_jp()).
#'
#' @return Objeto flextable listo para exportar.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' get_jp_tab(mod)
#' }
get_jp_tab <- function(mod) {
  df <- mod |>
    purrr::map_dfr(
      ~ {
        if ("segmented" %in% class(.x)) {
          # ---- Tiempo ----
          tiempo <- names(.x$model)[2]

          # ---- Cortes ----
          cortes <- sort(c(
            min(.x$model[[tiempo]]),
            .x$psi[, "Est."],
            max(.x$model[[tiempo]])
          ))

          # ---- APC ----
          segmented::slope(.x, APC = TRUE)[[tiempo]] |>
            tibble::as_tibble() |>
            dplyr::rename(est = 1, ic_l = 2, ic_u = 3) |>
            dplyr::mutate(
              JP = dplyr::if_else(
                dplyr::row_number() == 1,
                nrow(.x$psi),
                NA_real_
              ),
              Periodo = paste(
                round(head(cortes, -1)),
                round(tail(cortes, -1)),
                sep = "-"
              ),
              APC = purrr::pmap_chr(
                list(est, ic_l, ic_u),
                ~ paste0(
                  scales::number(
                    ..1,
                    accuracy = .1,
                    decimal.mark = ",",
                    suffix = "%"
                  ),
                  " (IC95%:",
                  scales::number(..2, accuracy = .1, decimal.mark = ","),
                  "; ",
                  scales::number(
                    ..3,
                    accuracy = .1,
                    decimal.mark = ",",
                    suffix = "%"
                  ),
                  ")"
                )
              ),
              AAPC = dplyr::if_else(
                dplyr::row_number() == 1,
                get_aapc(.x),
                NA_character_
              )
            ) |>
            dplyr::select(-est, -dplyr::starts_with("ic"))
        } else {
          tibble::tibble(
            JP = 0,
            Periodo = NA_character_,
            APC = NA_character_,
            AAPC = get_aapc(.x)
          )
        }
      },
      .id = "Grupo"
    ) |>
    tidyr::separate_wider_delim(
      Grupo,
      names = c("Grupo", "Subgrupo"),
      delim = "_",
      too_few = "align_start"
    )

  # ---- Flextable ----
  # ---- Flextable ----
  ft <- flextable::flextable(df)

  # ocultar Subgrupo si está vacío
  if ("Subgrupo" %in% names(df) && all(is.na(df$Subgrupo))) {
    ft <- flextable::delete_columns(ft, "Subgrupo")
    cols_merge <- "Grupo"
  } else {
    cols_merge <- c("Grupo", "Subgrupo")
  }

  ft |>
    flextable::merge_v(j = cols_merge) |>
    flextable::bold(part = "header") |>
    flextable::autofit()
}
