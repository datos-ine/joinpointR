##' Fit a Joinpoint regression model
#'
#' @keywords internal
# Fit the linear model ---------------------------------------------------
fit_jp <- function(x, y, joinpoints = NULL) {
  dat <- data.frame(
    y = y,
    time = x
  )

  if (length(joinpoints) > 0) {
    for (i in seq_along(joinpoints)) {
      dat[[paste0("U", i, ".time")]] <-
        pmax(x - joinpoints[i], 0)
    }
  }

  stats::lm(
    y ~ .,
    data = dat
  )
}

#' Fit Joinpoint models by groups
#'
#' @keywords internal
#'
fit_jp_groups <- function(
  data,
  groups,
  fit_group
) {
  # Fit models ----------------------------------------------
  data_split <- data |>
    dplyr::group_by(.jp_group) |>
    dplyr::group_split()

  mods <- purrr::map(
    data_split,
    fit_group
  )

  # Name models ---------------------------------------------
  names(mods) <- as.character(
    groups
  )

  # Message -------------------------------------------------
  purrr::iwalk(
    mods,
    ~ {
      jp <- .x$joinpoints

      message(
        if (is.na(.y)) {
          "Model"
        } else {
          .y
        },
        " | Joinpoint(s): ",
        if (length(jp) == 0) {
          "No joinpoints detected"
        } else {
          paste(
            scales::number(
              jp,
              big.mark = ""
            ),
            collapse = "; "
          )
        }
      )
    }
  )

  # Return ---------------------------------------------------
  mods
}
