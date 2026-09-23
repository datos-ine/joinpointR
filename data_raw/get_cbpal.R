#' Get colorblind-friendly palette data
#'
#' Retrieves and filters the dataset of colorblind-friendly palettes.
#' @return A \code{\link[tibble]{tibble}} containing the palette data.
#'
#' @keywords internal

cbpal_list <- purrr::map(
    .x = c("cat", "seq", "div"),
    .f = function(type) {
      # --- Get HTML table ---
      cols4all::c4a_table(
        type = type,
        filters = "cbf"
      ) |>
        # --- HTML to dataframe ---
        as.character() |>
        rvest::read_html() |>
        rvest::html_element("table") |>
        rvest::html_table() |>
        # --- Clean column names ---
        janitor::clean_names() |>
        # --- Exclude monochromatic palettes ---
        dplyr::filter_out(hues == "🖌") |>
        # --- Select columns ---
        dplyr::select(series, name, x1:x7)
    }
  ) |>

    # --- Name list elements ---
    purrr::set_names(c("cat", "seq", "div")) |>

    # --- List to data.frame ---
    purrr::list_rbind(names_to = "type") |>

    # --- Remove duplicates ---
    dplyr::distinct(name, .keep_all = TRUE) |>

    # --- Sort palettes by name ---
    dplyr::arrange(name)


# ---- Generate data ----
usethis::use_data(
  cbpal_list,
  overwrite = TRUE
)
  