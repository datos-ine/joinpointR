#' Tabla Resumen Joinpoint
#'
#' Genera una tabla con APC, AAPC, períodos y número de joinpoints
#' a partir de una lista de modelos joinpoint.
#'
#' @param mods Lista de modelos (salida de model_jp()).
#' @param ft Logical. Si TRUE devuelve una flextable, si FALSE un tibble.
#'
#' @return Un tibble o un objeto flextable.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' summary_jp(mods, ft = TRUE)
#' }
summary_jp <- function(mods, ft = FALSE) {
  df <- mods |>
    purrr::map_dfr(
      ~ {
        if ("segmented" %in% class(.x)) {
          # ---- Tiempo ----
          time <- names(.x$model)[2]

          # ---- Cortes ----
          breaks <- sort(c(
            min(.x$model[[time]]),
            .x$psi[, "Est."],
            max(.x$model[[time]])
          ))

          # ---- APC ----
          segmented::slope(.x, APC = TRUE)[[time]] |>
            tibble::as_tibble() |>
            dplyr::mutate(dplyr::across(
              dplyr::where(is.numeric),
              .fns = ~ round(.x, 2)
            )) |>
            dplyr::rename(APC = 1) |>

            # ---- IC 95% ----
            tidyr::unite(c(2, 3), col = "IC", sep = ", ") |>

            # ---- JP ----
            dplyr::mutate(
              JP = dplyr::if_else(
                dplyr::row_number() == 1,
                nrow(.x$psi),
                NA_real_
              ),

              # ---- Periodos ----
              Periodo = paste(
                round(head(breaks, -1)),
                round(tail(breaks, -1)),
                sep = "-"
              ),

              # ---- AAPC ----
              AAPC = dplyr::if_else(
                dplyr::row_number() == 1,
                get_aapc(.x),
                NA_character_
              )
            )
        } else {
          tibble::tibble(
            JP = 0,
            Periodo = NA_character_,
            APC = NA_real_,
            AAPC = get_aapc(.x)
          )
        }
      },
      .id = "grupo"
    ) |>

    # ---- Separación robusta ----
    (\(df) {
      if (any(stringr::str_detect(df$grupo, "_"))) {
        tidyr::separate_wider_delim(
          df,
          grupo,
          names = c("Grupo", "Subgrupo"),
          delim = "_",
          too_few = "align_start"
        )
      } else {
        dplyr::rename(df, Grupo = grupo)
      }
    })()

  # ---- Salida ----
  if (!ft) {
    return(df)
  }

  # ---- Flextable ----
  ft <- flextable::flextable(df)

  # ocultar Subgrupo si vacío
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
