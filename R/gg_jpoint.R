#' Plot Joinpoint Regression Models
#'
#' @description
#' Creates a ggplot showing observed values, fitted joinpoint regression lines,
#' and optional joinpoints.
#'
#' @param mods A list of models returned by \code{model_jp_d()} or
#' \code{model_jp_grid()}.
#' @param geom Character. Determines how the model results are displayed.
#' \code{geom = "line"} displays the fitted regression lines;
#' \code{geom = "linepoint"} displays the fitted regression lines together
#' with the observed data points; \code{geom = "area"} display the fitted values
#' as a smoothed area. Defaults to \code{"line"}.
#' @param jp Logical. Whether to display the locations of the estimated
#'  joinpoint(s) as vertical lines. Defaults to \code{TRUE}.
#' @param facets Character. Determines the facet layout.  \code{facets = "wrap"}
#' displays the results using one facet per grouping level; \code{facets = "grid"}
#' displays the results by group and subgroup; and \code{facets = "grid2"} reverses
#'  the order of the grouping variables. This argument is ignored when only one
#'  model is provided.
#' @param lwd Numeric. Width of the fitted regression lines. Defaults to 1 point.
#' @param psize Numeric. Size of the observed data points. Defaults to 2.5 points.
#' @param alpha Numeric. Transparency of the observed data points. Defaults to 0.75.
#' @param cbpal.name Character. Name of the colorblind-friendly palette to use.
#' Defaults to \code{"viridis"}.
#' @param ncol.wrap Integer. Number of columns to display when \code{facets = "wrap"}.
#' Defaults to 4.
#'
#' @return
#' A \code{ggplot2} object showing observed values, fitted joinpoint regression
#' lines, and optional joinpoints.
#'
#' @details
#' Available colorblind-friendly palettes can be checked using \code{plot_cbpal()}.
#'
#' @examples
#' # Load packages
#' library(dplyr)
#'
#' # Load data
#' data(hiv_data)
#'
#' # Create a reduced dataset
#' data <- hiv_data |>
#' filter(between(admin, "ARG", "Chubut"))
#'
#' # Fit models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year, group = c("admin", "sex"))
#'
#' # Plot results
#' gg_jpoint(mods = mods, geom = "linepoint", jp = TRUE)
#'
#' # Plot results as area
#' gg_jpoint(mods = mods, geom = "area", jp = TRUE)
#'
#' ## Plot results as line and reverse the facets
#' gg_jpoint(mods = mods, geom = "line", facets = "grid2", jp = TRUE, cbpal.name = "managua")
#'
#' @export

gg_jpoint <- function(
  mods,
  geom = c("line", "linepoint", "area"),
  jp = TRUE,
  facets = c("wrap", "grid", "grid2"),
  lwd = 1,
  psize = 2.5,
  alpha = 0.75,
  cbpal.name = "viridis",
  ncol.wrap = 4
) {
  #  ============================================================
  # ---- Set defaults ----
  #  ============================================================
  # --- Number of models ---
  n_mod <- length(mods)

  # --- Geometries ---
  geom <- match.arg(geom)

  # --- Palette name ---
  cbpal <- match.arg(
    cbpal.name,
    choices = cbpal_list |> dplyr::pull(name)
  )

  # --- Facets ---
  if (n_mod > 1) {
    facets <- match.arg(facets)
  } else {
    facets <- "none"
  }

  #  ============================================================
  # ---- Validations ----
  #  ============================================================
  # ---- Joinpoints ----
  if (!jp) {
    message("Joinpoint(s) will not be displayed.")
  }

  # ---- Facets ----
  if (n_mod == 1) {
    message(
      "Argument 'facets' was ignored because only one model was supplied."
    )
  }

  #  ============================================================
  # ---- Generate data ----
  #  ============================================================
  data <- purrr::map(
    mods,
    ~ tibble::tibble(
      time = .x$time,
      log_rate = .x$log_rate,
      fitted = stats::fitted(.x$fit)
    )
  ) |>
    purrr::list_rbind(names_to = "group_var") |>
    tidyr::separate_wider_delim(
      group_var,
      names = c("group", "subgroup"),
      delim = "_",
      too_few = "align_start",
      cols_remove = FALSE
    )

  #  ============================================================
  # ---- Generate joinpoint data ----
  #  ============================================================
  jp_data <- purrr::map(
    mods,
    ~ tibble::tibble(
      jp = .x$joinpoints
    )
  ) |>
    purrr::list_rbind(names_to = "group_var") |>
    tidyr::separate_wider_delim(
      group_var,
      names = c("group", "subgroup"),
      delim = "_",
      too_few = "align_start",
      cols_remove = FALSE
    )

  #  ============================================================
  # ---- Base plot layout ----
  #  ============================================================
  g <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = time,
      y = log_rate
    )
  ) +

    # --- LABELS ---
    ggplot2::labs(
      x = NULL,
      y = "log(rate)"
    ) +

    # --- Theme ---
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90)
    )

  #  ============================================================
  # ---- Facets ----
  #  ============================================================
  if (facets == "wrap") {
    g <- g +
      ggplot2::facet_wrap(~group_var, ncol = ncol.wrap)
  } else if (facets == "grid") {
    g <- g +
      ggplot2::facet_grid(group ~ subgroup)
  } else if (facets == "grid2") {
    g <- g +
      ggplot2::facet_grid(subgroup ~ group)
  }

  #  ============================================================
  # ---- Geometries ----
  #  ============================================================
  if (geom %in% c("line", "linepoint")) {
    # --- Add the regression line ---
    g <- g +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = fitted,
          color = if (facets == "grid2") subgroup else group
        ),
        lwd = lwd
      )

    if (geom == "linepoint") {
      g <- g +
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            color = if (facets == "grid2") subgroup else group
          ),
          size = psize,
          alpha = alpha
        )
    }
  } else if (geom == "area") {
    g <- g +
      ggplot2::geom_area(
        mapping = ggplot2::aes(
          y = fitted,
          fill = if (facets == "grid2") subgroup else group
        ),
        stat = "smooth",
        method = "loess",
        formula = 'y ~ x',
        alpha = 0.5,
        color = "grey20"
      )
  }

  #  ============================================================
  # ---- Joinpoints ----
  #  ============================================================
  if (jp) {
    g <- g +
      ggplot2::geom_vline(
        data = jp_data,
        mapping = ggplot2::aes(xintercept = jp),
        lwd = 1.5,
        color = "darkgrey",
        alpha = 0.75
      )
  }

  #  ============================================================
  # ---- Return ----
  #  ============================================================
  if (geom == "area") {
    g + scale_cbpal_fill(palette = cbpal)
  } else {
    g + scale_cbpal_color(palette = cbpal)
  }
}
