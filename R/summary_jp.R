#' Tabla Resumen Joinpoint
#'
#' Genera una tabla con APC, AAPC, períodos y número de joinpoints
#' a partir de una lista de modelos joinpoint.
#'
#' @param mods Lista de modelos (salida de model_jp()).
#' @param digits Integer. Número de decimales a mostrar en la tabla.
#' @param ft Logical. Si TRUE devuelve una flextable, si FALSE un tibble.
#' @param var1 Character. Variable de agrupamiento
#' @param var2 Character. Segunda variable de agrupamiento (opcional).
#'
#' @return Un tibble o un objeto flextable.
#' @author Tamara Ricardo
#' @export
#'
#' @examples
#' \dontrun{
#' summary_jp(mods, digits = 1, ft = TRUE, var1 = "Grupo", var2 = "Subgrupo")
#' }
summary_jp <- function(
  mods,
  digits = 1,
  ft = FALSE,
  var1 = "Grupo",
  var2 = "Subgrupo"
) {
  get_sig <- function(mod) {
    if ("segmented" %in% class(mod)) {
      aapc_obj <- segmented::aapc(mod)

      est <- aapc_obj[1]
      se <- aapc_obj[2]

      tval <- est / se

      # grados de libertad del modelo original
      df <- mod$df.residual

      pval <- 2 * (1 - stats::pt(abs(tval), df = df))
    } else {
      pval <- summary(mod)$coefficients[2, 4]
    }

    dplyr::case_when(
      pval < 0.001 ~ "***",
      pval < 0.01 ~ "**",
      pval < 0.05 ~ "*",
      TRUE ~ ""
    )
  }

  df <- mods |>
    purrr::map_dfr(
      ~ {
        if ("segmented" %in% class(.x)) {
          time <- names(.x$model)[2]

          # ---- Puntos de corte -----
          breaks <- sort(c(
            min(.x$model[[time]]),
            .x$psi[, "Est."],
            max(.x$model[[time]])
          ))

          # ---- APC ----
          segmented::slope(.x, APC = TRUE)[[time]] |>
            tibble::as_tibble() |>
            dplyr::rename(APC = 1, ic_l = 2, ic_u = 3) |>

            # Redondear
            dplyr::mutate(
              dplyr::across(
                dplyr::where(is.numeric),
                ~ round(.x, digits)
              )
            ) |>

            # ---- IC 95% ----
            tidyr::unite(
              c(ic_l, ic_u),
              col = "IC",
              sep = "; "
            ) |>

            # ---- Joinpoints ----
            dplyr::mutate(
              JP = dplyr::if_else(
                dplyr::row_number() == 1,
                nrow(.x$psi),
                NA_real_
              ),

              # ---- Periodos -----
              Periodo = paste(
                round(head(breaks, -1)),
                round(tail(breaks, -1)),
                sep = "-"
              ),
              .before = APC
            ) |>

            # ---- AAPC ----
            dplyr::mutate(
              AAPC = dplyr::if_else(
                dplyr::row_number() == 1,
                get_aapc(.x, show_ci = FALSE),
                NA_character_
              )
            )
        } else {
          tibble::tibble(
            JP = 0,
            Periodo = NA_character_,
            APC = NA_real_,
            IC = NA_character_,
            AAPC = get_aapc(.x)
          )
        }
      },
      .id = "grupo"
    ) |>

    # ---- separación dinámica ----
    (\(df) {
      if (any(stringr::str_detect(df$grupo, "_"))) {
        suppressWarnings(
          tidyr::separate_wider_delim(
            df,
            grupo,
            names = c(var1, var2),
            delim = "_",
            too_few = "align_start"
          )
        )
      } else {
        dplyr::rename(df, !!var1 := grupo)
      }
    })()

  if (!ft) {
    return(df)
  }

  # ---- flextable ----
  ft_obj <- flextable::flextable(df)

  cols_merge <- intersect(
    c(var1, var2),
    names(ft_obj$body$dataset)
  )

  if (
    var2 %in%
      names(ft_obj$body$dataset) &&
      all(is.na(ft_obj$body$dataset[[var2]]))
  ) {
    ft_obj <- flextable::delete_columns(ft_obj, var2)
    cols_merge <- var1
  }

  ft_obj |>
    flextable::merge_v(j = cols_merge) |>
    flextable::bold(part = "header") |>
    flextable::colformat_num(
      j = names(df)[sapply(df, is.numeric)],
      decimal.mark = ",",
      big.mark = "."
    ) |>
    flextable::autofit() |>
    flextable::add_body_row(
      top = FALSE,
      values = list(
        "*** P<0,001; ** P<0,01; * P<0,05;\n JP: cantidad de joinpoints; APC: cambio porcentual anual; IC: intervalo de confianza al 95%; AAPC: cambio porcentual anual promedio (95% IC)."
      ),
      colwidths = ncol(ft_obj$body$dataset)
    ) |>
    flextable::hline_bottom(border = officer::fp_border(width = 0))
}
