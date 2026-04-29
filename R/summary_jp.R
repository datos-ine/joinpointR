#' Summary Table For Joinpoint Regression Models
#'
#' Generates a table displaying the number of joinpoints, time breaks, APC and its 95% confidence
#' interval, and AAPC and its statistical significance from a list of joinpoint models.
#'
#' @param mods List of joinpoint regression models (model_jp() output).
#' @param digits Number of decimal places to display (integer).
#' @param var1 Character. Name of the grouping variable.
#' @param var2 Character. Name of the subgrouping variable (optional).
#' @param ft Logical. If TRUE returns a flextable object, if FALSE returns a tibble.
#' @param lan Language of output: "en" (English) or "es" (Spanish).
#'
#' @return A tibble or a flextable object.
#' @author Tamara Ricardo
#' @export
#' @examples
#' library(dplyr)
#' # Load example data
#' data("plant", package = "segmented")
#'
#' names(plant)
#'
#' # Fit the joinpoint models
#' mods <- model_jp(data = plant, value = "y", time = "time", group = "group", k = 2, test = TRUE)
#'
#' summary_jp(mods, digits = 1, var1 = "group", ft = FALSE, lan = "en")

summary_jp <- function(
  mods,
  digits = 1,
  var1 = "group",
  var2 = "subgroup",
  ft = FALSE,
  lan = c("en", "es")
) {
  lan <- match.arg(lan)
  dec <- if (lan == "en") "." else ","

  df <- purrr::map_dfr(
    mods,
    function(mod) {
      if (inherits(mod, "segmented")) {
        time <- names(mod$model)[2]

        breaks <- sort(c(
          min(mod$model[[time]]),
          mod$psi[, "Est."],
          max(mod$model[[time]])
        ))

        n_jp <- nrow(mod$psi)

        apc_tbl <- segmented::slope(mod, APC = TRUE)[[time]] |>
          tibble::as_tibble() |>
          dplyr::rename(APC = 1, CI_l = 2, CI_u = 3) |>
          dplyr::mutate(
            dplyr::across(where(is.numeric), ~ round(.x, digits)),
            JP = ifelse(dplyr::row_number() == 1, n_jp, NA_real_),
            Period = paste(
              round(head(breaks, -1)),
              round(tail(breaks, -1)),
              sep = "-"
            ),
            AAPC = ifelse(
              dplyr::row_number() == 1,
              get_aapc(mod, show_ci = FALSE, dec = dec),
              NA_character_
            )
          ) |>
          tidyr::unite("CI", CI_l, CI_u, sep = "; ")

        apc_tbl
      } else {
        tibble::tibble(
          JP = 0,
          Period = NA_character_,
          APC = NA_real_,
          CI = NA_character_,
          AAPC = get_aapc(mod, show_ci = FALSE, dec = dec)
        )
      }
    },
    .id = "group"
  )

  # ---- split grouping ----
  df <- if (any(stringr::str_detect(df$group, "_"))) {
    tidyr::separate_wider_delim(
      df,
      group,
      names = c(var1, var2),
      delim = "_",
      too_few = "align_start"
    )
  } else {
    dplyr::rename(df, !!var1 := group)
  }

  # ---- language ----
  if (lan == "es") {
    df <- df |>
      dplyr::rename(
        Periodo = Period,
        IC = CI
      )

    footnote_txt <- paste0(
      "* p < 0,05\n",
      "JP: cantidad de joinpoints; APC: cambio porcentual anual; ",
      "IC: intervalo de confianza al 95%; AAPC: cambio porcentual anual promedio (IC95%)."
    )
  } else {
    footnote_txt <- paste0(
      "* p < 0.05\n",
      "JP: number of joinpoints; APC: annual percent change; ",
      "CI: 95% confidence interval; AAPC: average annual percent change (95% CI)."
    )
  }

  if (!ft) {
    return(df)
  }

  # ---- flextable ----
  ft_obj <- flextable::flextable(df)

  cols_merge <- intersect(c(var1, var2), names(df))

  if (var2 %in% names(df) && all(is.na(df[[var2]]))) {
    ft_obj <- flextable::delete_columns(ft_obj, var2)
    cols_merge <- var1
  }

  ft_obj |>
    flextable::merge_v(j = cols_merge) |>
    flextable::bold(part = "header") |>
    flextable::colformat_num(
      j = names(df)[sapply(df, is.numeric)],
      decimal.mark = dec,
      big.mark = ""
    ) |>
    flextable::add_body_row(
      top = FALSE,
      values = list(footnote_txt),
      colwidths = ncol(df)
    ) |>
    flextable::hline_bottom(border = officer::fp_border(width = 0)) |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::fontsize(size = 12, part = "all") |>
    flextable::align(align = "left", part = "all") |>
    flextable::autofit()
}
