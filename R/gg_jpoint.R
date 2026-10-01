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
#' @param jp Logical. Whether to display the estimated joinpoint(s) as
#' vertical lines. Defaults to \code{TRUE}.
#'
#' @param facets Character. Determines the facet layout. \code{facets = "wrap"}
#' displays one facet for each grouping level; \code{facets = "grid"}
#' displays groups in rows and subgroups in columns; and
#' \code{facets = "grid2"} displays subgroups in rows and groups in columns.
#' This argument is ignored when only one model is provided.
#'
#' @param aapc Logical. Whether to display a label with the Average Annual
#' Percent Change (AAPC) and its statistical significance. Defaults to
#' \code{FALSE}.
#'
#' @param exp Logical. Whether to display observed and fitted values on the
#' original rate scale. Defaults to \code{FALSE}.
#'
#' @param lwd Numeric. Width of the fitted regression lines. Defaults to
#' \code{1}.
#'
#' @param psize Numeric. Size of the observed data points. Defaults to
#' \code{2.5}.
#'
#' @param alpha Numeric. Transparency of the observed data points.
#' Defaults to \code{0.75}.
#'
#' @param ncol.wrap Integer. Number of columns to display when
#' \code{facets = "wrap"}. Defaults to \code{4}.
#'
#' @param date.breaks Character. Date interval used for the x-axis.
#' Defaults to \code{"2 years"}.
#'
#' @param text.size Numeric. Text size in points used to display the AAPC
#' label. Ignored if \code{aapc = FALSE}. Defaults to \code{8}.
#'
#' @param cbpal Character. Name of the colorblind-friendly palette to use.
#' Defaults to \code{"viridis"}.
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
#' # Load data
#' data(hiv_data)
#'
#' # Create a reduced dataset
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
#' gg_jpoint(mods = mods, geom = "area", aapc = TRUE)
#'
#' ## Display as rates
#' gg_jpoint(mods = mods,  exp = TRUE, facets = "grid2")
#'
#' @export
#'
gg_jpoint <- function(
  mods,
  geom = c("linepoint", "line", "area"),
  jp = TRUE,
  facets = c("wrap", "grid", "grid2"),
  aapc = FALSE,
  exp = FALSE,
  lwd = 1,
  psize = 2.5,
  alpha = 0.75,
  ncol.wrap = 4,
  date.breaks = "2 years",
  text.size = 8,
  cbpal = "viridis"
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
    cbpal,
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
      fitted = stats::fitted(.x$fit),
      period = findInterval(.x$time, .x$joinpoints) + 1
    )
  ) |>
    # --- List to tibble ---
    purrr::list_rbind(names_to = "group_var") |>

    # --- Separate grouping variable ---
    tidyr::separate_wider_delim(
      group_var,
      names = c("group", "subgroup"),
      delim = "_",
      too_few = "align_start",
      cols_remove = FALSE
    ) |>

    # --- Convert to date format ---
    dplyr::mutate(time = lubridate::ymd(paste0(time, "-01-01")))

  #  ============================================================
  # ---- Exponentiate data ----
  #  ============================================================
  if (exp) {
    data <- data |>
      dplyr::mutate(
        rate = exp(log_rate),
        fitted_rate = exp(fitted)
      )
  }

  #  ============================================================
  # ---- Generate joinpoint data ----
  #  ============================================================
  jp_data <- purrr::map(
    mods,
    ~ tibble::tibble(
      jp = .x$joinpoints,
    )
  ) |>
    # --- List to tibble ---
    purrr::list_rbind(names_to = "group_var") |>

    # --- Separate grouping variable ---
    tidyr::separate_wider_delim(
      group_var,
      names = c("group", "subgroup"),
      delim = "_",
      too_few = "align_start",
      cols_remove = FALSE
    ) |>

    # --- Convert to date format ---
    dplyr::mutate(jp = lubridate::ymd(paste0(jp, "-01-01")))

  #  ============================================================
  # ---- Base plot layout ----
  #  ============================================================
  g <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = time,
      y = if (exp) rate else log_rate
    )
  ) +

    # --- X as date ---
    ggplot2::scale_x_date(
      date_breaks = date.breaks,
      date_labels = "%Y"
    ) +

    # --- LABELS ---
    ggplot2::labs(
      x = NULL,
      y = if (exp) "rate" else "log(rate)"
    ) +

    # --- Theme ---
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = if (geom == "line") {
        ggplot2::element_text()
      } else {
        ggplot2::element_blank()
      },
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
  # ---- Joinpoints ----
  #  ============================================================
  if (jp) {
    g <- g +
      ggplot2::geom_vline(
        data = jp_data,
        mapping = ggplot2::aes(xintercept = jp),
        lwd = 0.75,
        color = "darkgrey",
        linetype = "dashed",
        alpha = 0.75
      )
  }

  #  ============================================================
  # ---- Geometries ----
  #  ============================================================
  if (geom == "line") {
    g <- g +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = fitted,
          color = factor(period),
          group = group_var
        ),
        lwd = lwd
      )
  } else if (geom == "linepoint") {
    g <- g +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = if (exp) fitted_rate else fitted,
          color = if (facets == "grid2") subgroup else group
        ),
        lwd = lwd
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          color = if (facets == "grid2") subgroup else group
        ),
        size = psize,
        alpha = alpha
      )
  } else if (geom == "area") {
    g <- g +
      ggplot2::geom_area(
        mapping = ggplot2::aes(
          y = fitted,
          fill = if (facets == "grid2") subgroup else group
        ),
        alpha = 0.5,
        color = "grey20"
      )
  }

  #  ============================================================
  # ---- AAPC ----
  #  ============================================================
  if (aapc) {
    aapc <- get_aapc(mods) |>
      # --- Transform AAPC ---
      dplyr::mutate(aapc = paste0(round(aapc, 2), aapc_sig)) |>

      # --- Select columns ---
      dplyr::select(group_var = model, aapc) |>

      # --- Separate grouping variable ---
      tidyr::separate_wider_delim(
        group_var,
        names = c("group", "subgroup"),
        delim = "_",
        too_few = "align_start",
        cols_remove = FALSE
      )

    g <- g +
      ggplot2::geom_text(
        data = aapc,
        ggplot2::aes(
          x = max(data$time),
          y = min(data$log_rate) - diff(range(data$log_rate)) * 0.10,
          label = paste0("AAPC: ", aapc)
        ),
        size = text.size,
        size.unit = "pt",
        hjust = 1
      ) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::theme(
        plot.margin = ggplot2::margin(
          t = 5.5,
          r = 5.5,
          b = 30,
          l = 5.5
        )
      )
  }

  #  ============================================================
  # ---- Return ----
  #  ============================================================
  if (geom == "area") {
    g + scale_cbpal_fill(palette = cbpal)
  } else {
    g +
      scale_cbpal_color(
        palette = cbpal,
        name = if (geom == "line") "Period" else NULL
      )
  }
}
