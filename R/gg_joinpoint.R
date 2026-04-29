#' Plot results For Joinpoint Regression Models
#'
#' Generates a ggplot objec displaying the number observed points, slope and joinpoints from a list of joinpoint models
#'
#' @param mods List of joinpoint regression models (model_jp() output).
#'
#' @return A ggplot2 object.
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
#' # Plot results
#' gg_joinpoint(mods, obs = TRUE, jp = TRUE, facets = FALSE)
#'
#' # Facets by group
#' gg_joinpoint(mods, obs = TRUE, jp = TRUE, facets = TRUE)

gg_joinpoint <- function(mods, obs = TRUE, jp = TRUE, facets = FALSE) {
  # ---- Detect time variable ----
  time <- names(mods[[1]]$model)[2]

  # ---- Plot data ----
  df <- purrr::map_df(
    mods,
    ~ tibble::tibble(
      time = .x$model[[time]],
      obs = .x$model[[1]],
      fit = stats::predict(.x)
    ),
    .id = "group"
  )

  # ---- Joinpoints ----
  jp_df <- purrr::map_df(
    mods,
    ~ {
      if (inherits(.x, "segmented") && !is.null(.x$psi)) {
        tibble::tibble(jp = .x$psi[, "Est."])
      } else {
        NULL
      }
    },
    .id = "group"
  )

  # ---- Base plot ----
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = obs, color = group)) +
    ggplot2::geom_line(ggplot2::aes(y = fit), linewidth = 1)

  # ---- Observaciones ----
  if (obs) {
    p <- p + ggplot2::geom_point(size = 2.5, alpha = 0.75)
  }

  # ---- Joinpoints ----
  if (jp && nrow(jp_df) > 0) {
    p <- p +
      ggplot2::geom_vline(
        data = jp_df,
        ggplot2::aes(xintercept = jp),
        color = "darkgrey",
        lwd = 2,
        alpha = 0.5
      )
  }

  # ---- Facets ----
  if (facets) {
    p <- p + ggplot2::facet_wrap(~group)
  }

  # ---- Final ----
  p +
    ggplot2::labs(
      x = NULL,
      y = "log(rate)"
    ) +
    ggplot2::theme_minimal()
}
