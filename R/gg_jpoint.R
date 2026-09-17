#' Plot Joinpoint Regression Models
#'
#' Creates a ggplot showing observed values, fitted joinpoint regression lines,
#' and optional joinpoints.
#'
#' @param mods A list of models returned by \code{model_jp_grid()} or
#' \code{model_jp_step()}.
#' @param geom Character. Determines the geom displayer: `"line"` for regression line,
#' `"linepoint"` for regression line with observed data points, `"area"` for smoothed areas.
#' @param jp  Logical. Whether to display joinpoint position(s) as vertical line(s).
#' Defaults to `TRUE`.
#' @param facets Character. Determines the facet layout: `"wrap"` for faceting
#'  by group,`"grid"` for faceting by group and subgroup, or `"grid2"` for
#' faceting by subgroup and group. When grouping variables are absent shows
#' a single panel plot.
#' @param lwd Numeric. Width of the regression line. Defaults to 1 point.
#' @param psize Numeric. Size of the observed data points. Defaults to 2.5 points.
#' @param palpha Numeric. Controls the transparency of the data points.
#' @param cbpal.name Character. Name of the colorblind-friendly palette to use. Defaults to `"viridis"`.
#' @param cbpal_n Numeric. Maximum number of colors to display in the palette.
#' Defaults to 6.
#'
#' @return
#' A `ggplot` object showing observed values, fitted joinpoint regression
#' lines, and optional joinpoints.
#'
#' @details
#' Available colorblind-friendly palettes can be checked using `plot_cbpal()`.
#'
#' @examples
#' # Load example data
#' data(hiv_data)
#'
#' # Create reduced dataset
#' data <- hiv_data |>
#'   dplyr::filter(dplyr::between(admin, "ARG", "Chubut")) |>
#'   droplevels()
#'
#' # Fit the joinpoint models
#' mods <- model_jp_grid(
#'   data = data ,
#'   rate = hiv_rate,
#'   time = year,
#'   group = c("admin", "sex"),
#'   k = 2
#' )
#'

#'
#' @export
gg_jpoint <- function(
  mods,
  geom = c("line", "linepoint", "area"),
  jp = c("line", "hide"),
  facets = c("wrap", "grid", "grid2"),
  cbpal.name = "viridis",
  cbpal.n = 6,
  lwd = 1,
  psize = 2.5
) {
  #  ============================================================
  # ---- Set defaults ----
  #  ============================================================
  # --- Number of models ---
  n_mod <- length(mods)

  # --- Geometries ---
  geom <- match.arg(geom)

  # --- Plot joinpoint positions ---
  jp <- match.arg(jp)

  # --- Facets ---
  if (n_mod > 1) {
    facets <- match.arg(facets)
  } else {
    facets <- "none"
  }

  # ---- Number of groups ---
  if (facets == "grid2") {
    groups_n <- dplyr::n_distinct(
      gsub(".*_", "", x = names(mods))
    )
  } else {
    groups_n <- dplyr::n_distinct(
      gsub("_.*", "", x = names(mods))
    )
  }

  # --- Number of colors ---
  cbpal_n <- if (groups_n < 3) max(3, groups_n) else min(10, cbpal.n, groups_n)

  #  ============================================================
  # ---- Validations ----
  #  ============================================================
  # ---- Model class ----
  if (class(mods) != "list") {
    stop(
      "The input should be a list of models generated with 'model_jp_grid()' or 'model_jp_step()'.",
      call. = FALSE
    )
  }

  # ---- Joinpoints ----
  if (jp == "line") {
    message("Joinpoint(s) will be displayed as vertical lines.")
  } else if (jp == "bg") {
    message(
      "Period(s) delimited by joinpoint(s) will be displayed as background color(s)."
    )
  } else {
    message("Joinpoint(s) will not be displayed.")
  }

  # ---- Facets ----
  if (facets == "none") {
    message(
      "Argument 'facets' was ignored because only one model was supplied."
    )
  }

  # ---- Number of colors ----
  if (groups_n < 3 || groups_n > 10) {
    warning(
      paste0(
        "The number of colors (",
        groups_n,
        ") is outside the palette color range (3-10). Will default to a maximum of ",
        cbpal_n,
        " colors."
      ),
      call. = FALSE
    )
  }

  #  ============================================================
  # ---- Generate data ----
  #  ============================================================
  # ---- Main plot ----
  plot_data <- purrr::map(
    mods,
    function(x) {
      tibble::tibble(
        time = x$time,
        log_rate = x$log_rate,
        fit = stats::fitted(x$model),
        jp = if (rlang::is_empty(x$joinpoints)) {
          NA
        } else {
          paste(x$joinpoints, collapse = ",")
        }
      )
    }
  ) |>
    # --- List to data frame ---
    purrr::list_rbind(names_to = "group_var") |>

    # --- Create grouping variables ---
    dplyr::mutate(
      group = stringr::str_remove(group_var, "_.*"),
      subgroup = stringr::str_remove(group_var, ".*_"),
      group_var = stringr::str_replace(group_var, "_", ": ")
    )

  # ---- Joinpoint positions ----
  jp_data <- plot_data |>
    # --- Select columns ---
    dplyr::select(group_var, group, subgroup, jp) |>
    # --- Separate joinpoints ---
    tidyr::separate_wider_delim(
      jp,
      delim = ",",
      names_sep = "_",
      too_few = "align_start"
    ) |>
    # --- Switch to long format ---
    tidyr::pivot_longer(cols = tidyr::contains("jp"), values_to = "jp") |>
    # --- JP as numeric ---
    dplyr::mutate(jp = as.numeric(jp)) |>

    # --- Remove NAs ---
    tidyr::drop_na()

  #  ============================================================
  # ---- Generate palette ----
  #  ============================================================
  cbpal <- get_cbpal(n = cbpal_n) |>
    dplyr::filter(name == cbpal.name) |>
    dplyr::pull(color)

  #  ============================================================
  # ---- Plot layout ----
  #  ============================================================
  g <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = time,
      y = log_rate
    )
  ) +

    # --- Axis labels ---
    ggplot2::labs(
      x = NULL,
      y = "log(rate)"
    ) +

    # --- Theme defaults ----
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90)
    )

  # ---- Facets ----
  if (facets == "wrap") {
    g <- g +
      ggplot2::facet_wrap(~group_var)
  } else if (facets == "grid") {
    g <- g +
      ggplot2::facet_grid(group ~ subgroup)
  } else if (facets == "grid2") {
    g <- g +
      ggplot2::facet_grid(subgroup ~ group)
  } else {
    g
  }

  #  ============================================================
  # ---- Plot geometries ----
  #  ============================================================
  # ---- Fitted values ----
  if (geom == "line") {
    g <- g +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = fit,
          color = if (facets == "grid2") subgroup else group
        ),
        lwd = lwd
      )
  } else if (geom == "linepoint") {
    g <- g +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = fit,
          color = if (facets == "grid2") subgroup else group
        ),
        lwd = lwd
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          y = log_rate,
          color = if (facets == "grid2") subgroup else group
        ),
        size = psize,
        alpha = 0.75
      )
  } else {
    g <- g +
      ggplot2::geom_area(
        mapping = ggplot2::aes(
          y = fit,
          fill = if (facets == "grid2") subgroup else group
        ),
        stat = "smooth",
        method = "loess",
        formula = 'y ~ x',
        alpha = 0.5,
        color = "grey20"
      )
  }

  # ---- Joinpoint position ----
  if (jp == "line") {
    g <- g +
      ggplot2::geom_vline(
        data = jp_data,
        mapping = ggplot2::aes(xintercept = jp),
        lwd = 1.5,
        color = "darkgrey",
        alpha = 0.75
      )
  } else {
    g
  }

  #  ============================================================
  # ---- Return ----
  #  ============================================================
  if (geom != "area") {
    g +
      ggplot2::scale_color_manual(
        values = cbpal,
        n = groups_n
      )
  } else {
    g +
      ggplot2::scale_fill_manual(
        values = cbpal,
        n = groups_n
      )
  }
}
