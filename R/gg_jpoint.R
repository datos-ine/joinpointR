#' Plot Joinpoint Regression Models
#'
#' @description
#' Creates a \code{ggplot2} visualization of joinpoint regression models,
#' showing observed values, fitted regression lines, and optionally the
#' estimated joinpoints and Average Annual Percent Change (AAPC).
#'
#' @param models A model or a list of models of class \code{model_jp}, returned
#' by \code{model_jp_grid()}.
#'
#' @param geom Character. Determines how the model results are displayed.
#' \code{geom = "line"} displays the fitted regression lines;
#' \code{geom = "linepoint"} displays the fitted regression lines together
#' with the observed data points; and \code{geom = "area"} displays the
#' fitted lines with background color depending on the grouping variable,
#' time period or trend. Defaults to \code{"linepoint"}.
#'
#' @param facets Character. Determines the facet layout. \code{facets = "wrap"}
#' displays one facet for each grouping level; \code{facets = "grid"}
#' displays groups in rows and subgroups in columns; and
#' \code{facets = "grid2"} displays subgroups in rows and groups in columns.
#' This argument is ignored when only one model is provided.
#'
#' @param color.by Character. Determines whether the data should be colored using
#' the grouping variable (\code{"group"}), the time periods (\code{"period"}),
#' the time segments (\code{"segment"}), or the APC trend (\code{"trend"}).
#' Defaults to \code{"group"} for \code{geom = "linepoint"}, to \code{"period"}
#' for \code{geom = "area"}, and to \code{"segment"} for \code{geom = "line"}.
#'
#' @param jp Logical. Whether to display the estimated joinpoint(s) as
#' vertical lines. Defaults to \code{TRUE}.
#'
#' @param aapc Logical. Whether to display a label with the Average Annual
#' Percent Change (AAPC) and its statistical significance. Defaults to
#' \code{FALSE}.
#'
#' @param time.var Character. Determines the class of the time variable,
#' among \code{"year"} and \code{"yearmon"}. Defaults to \code{"year"}.
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
#' # Create an example dataset
#' data <- hiv_data |>
#' dplyr::filter(admin == "Catamarca")
#'
#' # Fit models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year,
#' group = "sex")
#'
#' # Plot results with AAPC
#' gg_jpoint(models = mods, aapc = TRUE)
#'
#' @name gg_jpoint
#' @aliases gg_jpoint gg_jpoint_area gg_jpoint_line
#' @export
#'
gg_jpoint <- function(
  models,
  geom = c("linepoint", "line", "area"),
  facets = c("wrap", "grid", "grid2"),
  color.by = NULL,
  jp = TRUE,
  aapc = FALSE,
  time.var = c("year", "yearmon"),
  ...
) {
  # =============================================================
  # ---- Set defaults ----
  # =============================================================
  # ---- Number of models ----
  n_mod <- length(models)

  # ---- Geometries ----
  geom <- match.arg(geom)

  # --- Time variable ---
  time.var <- match.arg(time.var)

  # ---- Facets layout ----
  if (n_mod > 1) {
    facets <- match.arg(facets)
  } else {
    facets <- "none"
  }

  # ---- Color layout ----
  color.by <- if (geom == "area" && is.null(color.by)) {
    "period"
  } else if (geom == "line" && is.null(color.by)) {
    "segment"
  } else {
    match.arg(
      color.by,
      choices = c("group", "period", "segment", "trend")
    )
  }

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
      date_breaks = ggplot2::waiver(),
      name = if (color.by == "period") {
        "Period"
      } else if (color.by == "segment") {
        "Segment"
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
  # ---- Plot data ----
  # =============================================================
  dat <- purrr::map(
    .x = models,
    .f = ~ tibble::tibble(
      time = .x$time,
      log_rate = .x$log_rate,
      fitted = stats::fitted(.x$fit),
      n_jp = length(.x$joinpoints),
      segment = findInterval(.x$time, .x$joinpoints) + 1,
      jp_pos = .x$joinpoints[segment]
    )
  ) |>

    # --- List to tibble ---
    purrr::list_rbind(names_to = "group_var") |>

    # --- Format grouping variable ---
    dplyr::mutate(
      group_var = stringr::str_replace_all(
        .data$group_var,
        c("\\." = " ", "_" = ": ")
      )
    ) |>

    # --- Separate grouping variable ---
    tidyr::separate_wider_delim(
      .data$group_var,
      delim = ": ",
      names = c("group", "subgroup"),
      too_few = "align_start",
      too_many = "merge",
      cols_remove = FALSE
    ) |>

    # --- Add AAPC ---
    dplyr::left_join(
      get_summary(models, hide = "ci"),
      by = c("group_var", "n_jp", "segment")
    ) |>

    # --- Add trend ---
    dplyr::mutate(
      trend = dplyr::case_when(
        .data$apc > 0 & .data$apc_sig == "*" ~ "Increasing",
        .data$apc < 0 & .data$apc_sig == "*" ~ "Decreasing",
        .default = "Stable"
      )
    ) |>

    # --- Add begin and end ---
    dplyr::mutate(
      begin = stringr::str_remove(.data$period, "-.*") |> as.numeric(),
      end = stringr::str_remove(.data$period, ".*-") |> as.numeric()
    )

  # ---- Convert to date format ----
  if (time.var == "year") {
    dat <- dat |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(.data$time, .data$jp_pos, .data$begin, .data$end),
          .fns = ~ lubridate::ymd(paste0(.x, "01-01"), quiet = TRUE)
        )
      )
  } else if (time.var == "yearmon") {
    dat <- dat |>
      dplyr::mutate(
        dplyr::across(
          .cols = c(.data$time, .data$jp_pos),
          .fns = ~ lubridate::ymd(paste0(.x, "01"), quiet = TRUE)
        )
      )
  }

  # =============================================================
  # ---- Color scheme ----
  # =============================================================
  dat <- dat |>
    dplyr::mutate(
      plot_color = dplyr::case_when(
        color.by == "group" & facets == "grid2" ~ .data$subgroup,
        color.by == "group" ~ .data$group,
        color.by == "period" ~ .data$period,
        color.by == "trend" ~ .data$trend,
        .default = as.character(.data$segment)
      )
    )

  # =============================================================
  # ---- Base plot layout ----
  # =============================================================
  g <- ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(
      x = .data$time,
      y = .data$log_rate,
    )
  ) +

    # --- X axis date format ---
    ggplot2::scale_x_date(
      date_labels = if (time.var == "year") "%Y" else "%Y%m",
      date_breaks = geom_args$date_breaks
    ) +

    # --- Axis labels ---
    ggplot2::labs(
      x = NULL,
      y = "log(rate)"
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
  # ---- Geom: Line and Linepoint ----
  # =============================================================
  if (geom %in% c("line", "linepoint")) {
    g <- g +
      # --- Fitted lines ---
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = .data$fitted,
          group = .data$group_var,
          color = .data$plot_color
        ),
        lwd = geom_args$lwd
      )
  }

  if (geom == "linepoint") {
    g <- g +
      # --- Observed points ---
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          color = .data$plot_color
        ),
        size = geom_args$size,
        alpha = geom_args$alpha
      )
  }

  # =============================================================
  # ---- Geom: Area ----
  # =============================================================
  if (geom == "area") {
    dat_area <- dat |>
      dplyr::distinct(
        .data$group_var,
        .data$period,
        .data$segment,
        .data$trend,
        .keep_all = TRUE
      )

    g <- g +
      ggplot2::geom_rect(
        data = dat_area,
        mapping = ggplot2::aes(
          ymin = -Inf,
          ymax = Inf,
          xmin = .data$begin,
          xmax = .data$end,
          fill = .data$plot_color
        ),
        alpha = geom_args$alpha,
        inherit.aes = FALSE
      ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = .data$fitted,
          group = .data$group_var
        )
      )
  }

  # =============================================================
  # ---- Show joinpoints ----
  # =============================================================
  if (jp) {
    g <- g +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = .data$jp_pos
        ),
        lwd = 0.75,
        color = "darkgrey",
        linetype = "dashed",
        alpha = 0.75,
        na.rm = TRUE
      )
  }

  # =============================================================
  # ---- Add AAPC ---
  # =============================================================
  if (aapc) {
    g <- g +
      ggplot2::geom_label(
        ggplot2::aes(
          x = max(.data$time),
          y = min(.data$log_rate),
          label = paste0("AAPC: ", round(.data$aapc, 2), .data$aapc_sig)
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
  return(
    if (geom == "area") {
      g +
        scale_cbpal_fill(
          palette = "viridis",
          name = geom_args$name
        )
    } else {
      g +
        scale_cbpal_color(
          palette = "viridis",
          name = geom_args$name
        )
    }
  )
}


# =============================================================
# ---- Shortcuts ----
# =============================================================
#' Geom area
#' @rdname gg_jpoint
#' @export
#'
gg_jpoint_area <- function(
  models,
  ...
) {
  gg_jpoint(
    models,
    geom = "area",
    ...
  )
}

#' Geom line
#' @rdname gg_jpoint
#' @export
#'
gg_jpoint_line <- function(
  models,
  ...
) {
  gg_jpoint(
    models,
    geom = "line",
    ...
  )
}
