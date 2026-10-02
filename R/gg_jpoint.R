#' Plot Joinpoint Regression Models
#'
#' @description
#' Creates a \code{ggplot2} visualization of joinpoint regression models,
#' showing observed values, fitted regression lines, and optionally the
#' estimated joinpoints and Average Annual Percent Change (AAPC).
#'
#' @param mods A list of models of class \code{model_jp}, returned by
#' \code{model_jp_grid()}.
#'
#' @param geom Character. Determines how the model results are displayed.
#' \code{geom = "line"} displays the fitted regression lines;
#' \code{geom = "linepoint"} displays the fitted regression lines together
#' with the observed data points; and \code{geom = "area"} displays the
#' fitted values as a smoothed area. Defaults to \code{"linepoint"}.
#'
#' @param facets Character. Determines the facet layout. \code{facets = "wrap"}
#' displays one facet for each grouping level; \code{facets = "grid"}
#' displays groups in rows and subgroups in columns; and
#' \code{facets = "grid2"} displays subgroups in rows and groups in columns.
#' This argument is ignored when only one model is provided.
#'
#' @param color.by Character. Determines whether the data should be colored using
#' the grouping variable (\code{"group"}), the time period (\code{"period"}), or
#' the rate trend (\code{"trend"}). Defaults to \code{"group"} for
#' \code{geom = "linepoint"} and to \code{"period"} for \code{geom = "area"} and
#' \code{geom = "line"}.
#'
#' @param exp Logical. Whether to display observed and fitted values on the
#' original rate scale. Defaults to \code{FALSE}.
#'
#' @param jp Logical. Whether to display the estimated joinpoint(s) as
#' vertical lines. Defaults to \code{TRUE}.
#'
#' @param aapc Logical. Whether to display a label with the Average Annual
#' Percent Change (AAPC) and its statistical significance. Defaults to
#' \code{FALSE}.
#'
#' @param cbpal Character. Name of the colorblind-friendly palette to use.
#' Defaults to \code{"viridis"}.
#'
#' @param ... Additional arguments passed to \code{ggplot2}.
#'
#' @return A \code{ggplot2} object showing observed values, fitted joinpoint
#' regression lines, and optionally the estimated joinpoints and AAPC.
#'
#' @details
#' Available colorblind-friendly palettes can be checked using
#' \code{plot_cbpal()}.
#'
#' @examples
#' # Load packages
#' library(dplyr)
#'
#' # Create an example dataset
#' data <- hiv_data |>
#' filter(between(admin, "ARG", "Chubut"))
#'
#' # Fit models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year,
#' group = c("admin", "sex"))
#'
#' # Plot results
#' gg_jpoint(mods = mods, jp = TRUE)
#'
#' # Plot results as area and show AAPC
#' gg_jpoint(mods = mods, aapc = TRUE)
#'
#' ## Display as rates
#' gg_jpoint(mods = mods, exp = TRUE, facets = "grid2")
#'
#' @export
#'
gg_jpoint <- function(
  mods,
  geom = c("linepoint", "line", "area"),
  facets = c("wrap", "grid", "grid2"),
  color.by = c("group", "period", "trend"),
  exp = FALSE,
  jp = TRUE,
  aapc = FALSE,
  cbpal = NULL,
  ...
) {
  # =============================================================
  # ---- Set defaults ----
  # =============================================================
  # ---- Number of models ----
  n_mod <- length(mods)

  # ---- Geometries ----
  geom <- match.arg(geom)

  # ---- Palette name ----
  if (missing(cbpal) || is.null(cbpal)) {
    cbpal <- "viridis"
  } else {
    cbpal <- match.arg(cbpal, choices = dplyr::pull(cbpal_list, name))
  }

  # ---- Facets layout ----
  if (n_mod > 1) {
    facets <- match.arg(facets)
  } else {
    facets <- "none"
  }

  # ---- Color layout ----
  color.by <- match.arg(color.by)

  # ---- Additional arguments ----
  geom_args <- purrr::list_modify(
    .x = list(
      lwd = 1,
      size = 2.5,
      alpha = 0.75,
      ncol = 4,
      angle = 90,
      text.size = 8,
      hjust = 1,
      vjust = 0.5,
      border.color = NA,
      reverse = FALSE,
      date_breaks = "2 years",
      name = if (color.by == "period") {
        "Period"
      } else if (color.by == "trend") {
        "Trend"
      } else {
        "Group"
      }
    ),
    !!!rlang::list2(...)
  )

  # =============================================================
  # ---- Validations ----
  # =============================================================
  # ---- Joinpoints ----
  if (!jp) {
    message("Position of joinpoints will not be displayed.")
  }

  # ---- Facets ----
  if (n_mod == 1) {
    message(
      "Argument 'facets' was ignored because only one model was supplied."
    )
  }

  # =============================================================
  # ---- Generate data ----
  # =============================================================
  data <- purrr::map(
    .x = mods,
    .f = ~ tibble::tibble(
      time = .x$time,
      log_rate = .x$log_rate,
      rate = exp(log_rate),
      fitted = stats::fitted(.x$fit),
      exp_fitted = exp(fitted),
      period = findInterval(.x$time, .x$joinpoints) + 1,
      jp_pos = .x$joinpoints[period]
    )
  ) |>

    # --- List to tibble ---
    purrr::list_rbind(names_to = "group_var") |>

    # --- Convert to date ---
    dplyr::mutate(
      dplyr::across(
        .cols = c(time, jp_pos),
        .fns = ~ lubridate::ymd(paste0(.x, "01-01"), quiet = TRUE)
      )
    ) |>

    # --- Separate grouping variable ---
    tidyr::separate_wider_delim(
      group_var,
      names = c("group", "subgroup"),
      delim = "_",
      too_few = "align_start",
      cols_remove = FALSE
    ) |>

    # --- Add trend per period and group ---
    dplyr::mutate(
      trend = dplyr::if_else(
        (dplyr::last(fitted) - dplyr::first(fitted)) >= 0,
        "Asc.",
        "Desc."
      ),
      .by = c(group_var, period)
    ) |>

    # --- Add area breaks ---
    dplyr::mutate(
      breaks = dplyr::coalesce(dplyr::lag(period), period),
      .by = group_var
    )

  # =============================================================
  # ---- Color scheme ----
  # =============================================================
  plot_color <- if (color.by == "group") {
    if (facets == "grid2") data$subgroup else data$group
  } else if (color.by == "period") {
    if (geom == "area") {
      factor(data$breaks)
    } else {
      factor(data$period)
    }
  } else {
    factor(data$trend)
  }

  # =============================================================
  # ---- Base plot layout ----
  # =============================================================
  g <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = time,
      y = if (exp) rate else log_rate,
    )
  ) +

    # --- X axis date format ---
    ggplot2::scale_x_date(
      date_labels = "%Y",
      date_breaks = geom_args$date_breaks
    ) +

    # --- Axis labels ---
    ggplot2::labs(
      x = NULL,
      y = if (exp) "rate" else "log(rate)"
    ) +

    # --- Theme ---
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = geom_args$angle)
    )

  # =============================================================
  # ---- Facets ----
  # =============================================================
  if (facets == "wrap") {
    g <- g +
      ggplot2::facet_wrap(~group_var, ncol = geom_args$ncol)
  } else if (facets == "grid") {
    g <- g +
      ggplot2::facet_grid(group ~ subgroup)
  } else if (facets == "grid2") {
    g <- g +
      ggplot2::facet_grid(subgroup ~ group)
  }

  # =============================================================
  # ---- Show joinpoints ----
  # =============================================================
  if (jp) {
    g <- g +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = jp_pos
        ),
        lwd = 0.75,
        color = "darkgrey",
        linetype = "dashed",
        alpha = 0.75,
        na.rm = TRUE
      )
  }

  # =============================================================
  # ---- Geom: Linepoint (default) ----
  # =============================================================
  if (geom == "linepoint") {
    g <- g +
      # --- Fitted lines ---
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = if (exp) exp_fitted else fitted,
          group = group_var,
          color = plot_color
        ),
        lwd = geom_args$lwd
      ) +

      # --- Observed points ---
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          color = plot_color
        ),
        size = geom_args$size,
        alpha = geom_args$alpha
      )
  }

  # =============================================================
  # ---- Geom: Area ----
  # =============================================================
  if (geom == "area") {
    g <- g +
      ggplot2::geom_area(
        mapping = ggplot2::aes(
          y = if (exp) exp_fitted else fitted,
          fill = plot_color,
          group = group_var
        ),
        position = "identity",
        alpha = geom_args$alpha,
        color = "grey20"
      )
  }

  # =============================================================
  # ---- Geom: line ----
  # =============================================================
  if (geom == "line") {
    g <- g +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = if (exp) exp_fitted else fitted,
          color = plot_color,
          group = group_var
        ),
        lwd = geom_args$lwd
      )
  }

  # =============================================================
  # ---- Add AAPC ---
  # =============================================================
  if (aapc) {
    aapc <- get_aapc(mods) |>

      # --- Select columns ---
      dplyr::select(group_var = model, aapc, aapc_sig) |>

      # --- Separate grouping variable ---
      tidyr::separate_wider_delim(
        group_var,
        names = c("group", "subgroup"),
        delim = "_",
        too_few = "align_start",
        cols_remove = FALSE
      )

    g <- g +
      ggplot2::geom_label(
        data = aapc,
        ggplot2::aes(
          x = max(data$time),
          y = min(data$log_rate),
          label = paste0("AAPC: ", round(aapc, 2), aapc_sig)
        ),
        size = geom_args$text.size,
        size.unit = "pt",
        vjust = geom_args$vjust,
        hjust = geom_args$hjust,
        alpha = 0.8,
        border.color = geom_args$border.color
      )
  }

  # =============================================================
  # ---- Return ----
  # =============================================================
  if (geom == "area") {
    g <- g +
      scale_cbpal_fill(
        palette = cbpal,
        reverse = geom_args$reverse,
        name = geom_args$name
      )
  } else {
    g <- g +
      scale_cbpal_color(
        palette = cbpal,
        reverse = geom_args$reverse,
        name = geom_args$name
      )
  }
  return(g)
}
