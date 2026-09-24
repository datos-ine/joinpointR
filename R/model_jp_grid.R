#' Fit Joinpoint Regression Models Using Grid Search
#'
#' @description
#' Fits log-linear joinpoint regression models using grid search and selects
#' the best model according to the Bayesian Information Criterion (BIC).
#'
#' @param data A data frame or tibble containing rates, time points, and
#' optional grouping variables.
#'
#' @param rate Character string specifying the variable with the rates.
#'
#' @param time Character string specifying the variable with the time points.
#'
#' @param group Character vector specifying the name(s) of the variable(s)
#' used to group the data. A maximum of two grouping variables is allowed.
#' Defaults to \code{NULL}.
#'
#' @param jp Integer specifying the maximum number of joinpoints to test.
#' Must be between 0 and 7 (see Details). Defaults to \code{2}.
#'
#' @param min.dist Integer specifying the minimum number of time points
#' required between consecutive joinpoints, as well as between each endpoint
#' of the time series and the nearest joinpoint. Defaults to \code{2}.
#'
#' @param method Character string specifying the method used to calculate the BIC.
#' Options are \code{"bic"} for standard BIC, \code{"bic3"} for penalized BIC (BIC3),
#' or \code{"wbic"} for weighted BIC (WBIC). Defaults to \code{"bic"} (see Details).
#'
#' @return A named list with one element per group, where each element contains:
#' \itemize{
#'   \item \code{fit}: An \code{lm} object corresponding to the selected
#' joinpoint model.
#'   \item \code{joinpoints}: Numeric vector with the estimated joinpoint
#' positions of the selected model.
#'   \item \code{time}: Vector of time points used to fit the model.
#'   \item \code{log_rate}: Vector of log-transformed rates used as the response
#' variable.
#'   \item \code{BIC}: BIC value of the selected model based on the specified
#'     \code{method}.
#'   \item \code{model_sel}: A tibble containing the number and positions of
#' joinpoints, SSE, and BIC for all evaluated candidate models.
#' }
#'
#' @details
#' The maximum recommended number of joinpoints is determined by the number of
#' time points in the series, following the criteria described by Kim et al. (2000):
#'
#' \itemize{
#'   \item 0--6 time points: 0 joinpoints.
#'   \item 7--11 time points: 1 joinpoint.
#'   \item 12--16 time points: 2 joinpoints.
#'   \item 17--21 time points: 3 joinpoints.
#'   \item 22--26 time points: 4 joinpoints.
#'   \item 27--31 time points: 5 joinpoints.
#'   \item 32--36 time points: 6 joinpoints.
#'   \item 37+ time points: 7 joinpoints.
#' }
#'
#' When grouping variables are specified, all groups must contain the same
#' number of time points.
#'
#' @examples
#' # Load data
#' data(hiv_data)
#'
#' # Create a reduced dataset
#' data <- hiv_data |>
#' dplyr::filter(
#' dplyr::between(admin, "ARG", "Chubut"))
#'
#' # Fit the joinpoint models
#' mods <- model_jp_grid(data = data, rate = hiv_rate, time = year, group = c("admin", "sex"))
#'
#' # Select models based on the BIC3
#' mods_bic3 <- update(mods, method = "bic3")
#'
#' @export
model_jp_grid <- function(
    data,
    rate,
    time,
    group = NULL,
    jp = 2,
    min.dist = 2,
    method = "bic"
) {
    # ==========================================================
    # ---- Define variables ----
    # ==========================================================
    rate <- rlang::as_name(rlang::ensym(rate))

    time <- rlang::as_name(rlang::ensym(time))

    # ==========================================================
    # ---- Prepare data ----
    # ==========================================================
    jp_data <- clean_jp_data(
        data = data,
        rate = rate,
        time = time,
        group = group
    )

    data <- jp_data$group_data

    mod_names <- if (!is.null(group)) jp_data$group_levels else "Model"

    # ==========================================================
    # ---- Validations ----
    # ==========================================================
    # ---- Validate method ---
    if (method == "bic3") {
        message("Model selection based on the BIC3.")
    } else if (method == "wbic") {
        message("Model selection based on the WBIC.")
    } else {
        message("Model selection based on the BIC.")
    }

    # ---- Maximum number of joinpoints ----
    if (jp > 7) {
        stop(
            "This function allows to test a maximum of 7 joinpoints.",
            call. = FALSE
        )
    }

    # ---- Recommended number of joinpoints ----
    if (jp > jp_data$k) {
        warning(
            paste0(
                "The recommended number of joinpoints to test for your ",
                jp_data$time_points,
                " time points is ",
                jp_data$k,
                " (check Details)."
            ),
            call. = FALSE
        )
    }

    # ==========================================================
    # ---- Fit models ----
    # ==========================================================
    fit_group <- function(data, group_name) {
        # --------------------------------------------------------
        # ---- Define x and y ----
        # --------------------------------------------------------
        x <- data$time
        y <- data$log_rate

        # --------------------------------------------------------
        # ---- Fit linear joinpoint model ----
        # --------------------------------------------------------
        fit_jp <- function(x, y, k) {
            # ---- Create model data ----
            model_data <- dplyr::bind_cols(
                tibble::tibble(x, y),
                purrr::map(
                    seq_along(k),
                    function(i) {
                        pmax(0, x - k[i])
                    }
                ) |>
                    rlang::set_names(
                        paste0("U.", seq_along(k))
                    )
            )

            # ---- Fit linear model ----
            model <- stats::lm(y ~ ., data = model_data)

            # ------------------------------------------------------
            # ---- Store data used by the model ----
            # ------------------------------------------------------
            model$.jp_time <- x
            model$.jp_log_rate <- y
            model$.jp_k <- k
            model$.jp_fitted_values <- stats::fitted(model)

            model
        }

        # --------------------------------------------------------
        # ---- Validate joinpoints ----
        # --------------------------------------------------------
        valid_jp <- function(jp_values) {
            if (length(jp_values) == 0) {
                return(TRUE)
            }

            idx <- match(jp_values, x)
            if (anyNA(idx)) {
                return(FALSE)
            }

            # --- Distances ---
            all(diff(c(0, sort(idx), length(x) + 1)) - 1 >= min.dist)
        }

        # ========================================================
        # ---- Model results ----
        # ========================================================
        results <- list()

        # ========================================================
        # ---- Model with 0 joinpoints ----
        # ========================================================
        joinpoints <- numeric(0)

        mod <- fit_jp(
            x = x,
            y = y,
            k = joinpoints
        )

        results[[length(results) + 1]] <- tibble::tibble(
            n_jp = 0,
            jp1 = NA_real_,
            jp2 = NA_real_,
            jp3 = NA_real_,
            jp4 = NA_real_,
            jp5 = NA_real_,
            jp6 = NA_real_,
            jp7 = NA_real_,
            SSE = sum(stats::residuals(mod)^2)
        ) |>
            dplyr::bind_cols(calc_bic_jp(mod))

        # ========================================================
        # ---- Model with 1 to k joinpoints ----
        # ========================================================
        if (jp >= 1) {
            for (n_jp in seq_len(jp)) {
                # ----------------------------------------------------
                # Generate all combinations
                # ----------------------------------------------------
                jp_grid <- utils::combn(
                    sort(unique(x)),
                    n_jp,
                    simplify = FALSE
                ) |>
                    purrr::keep(
                        valid_jp
                    )

                # ----------------------------------------------------
                # Fit every candidate
                # ----------------------------------------------------
                if (length(jp_grid) > 0) {
                    for (jp_values in jp_grid) {
                        mod <- fit_jp(
                            x = x,
                            y = y,
                            k = jp_values
                        )

                        jp_values_out <- rep(
                            NA_real_,
                            7
                        )

                        jp_values_out[
                            seq_along(jp_values)
                        ] <- jp_values

                        results[[length(results) + 1]] <-
                            tibble::tibble(
                                n_jp = n_jp,
                                jp1 = jp_values_out[1],
                                jp2 = jp_values_out[2],
                                jp3 = jp_values_out[3],
                                jp4 = jp_values_out[4],
                                jp5 = jp_values_out[5],
                                jp6 = jp_values_out[6],
                                jp7 = jp_values_out[7],
                                SSE = sum(stats::residuals(mod)^2)
                            ) |>
                            dplyr::bind_cols(calc_bic_jp(mod))
                    }
                }
            }
        }

        # ========================================================
        # ---- Full list of models ----
        # ========================================================
        results <- dplyr::bind_rows(results)

        if (method == "wbic") {
            results <- results |> dplyr::arrange(WBIC)
        } else if (method == "bic3") {
            results <- results |> dplyr::arrange(BIC3)
        } else {
            results <- results |> dplyr::arrange(BIC)
        }
        #     dplyr::arrange(BIC)

        # ========================================================
        # ---- Select best model ----
        # ========================================================
        if (method == "wbic") {
            best <- results |>
                dplyr::slice_min(
                    WBIC,
                    n = 1,
                    with_ties = FALSE
                )
        } else if (method == "bic3") {
            best <- results |>
                dplyr::slice_min(
                    BIC3,
                    n = 1,
                    with_ties = FALSE
                )
        } else {
            best <- results |>
                dplyr::slice_min(
                    BIC,
                    n = 1,
                    with_ties = FALSE
                )
        }

        # ========================================================
        # ---- Selected joinpoints ----
        # ========================================================
        joinpoints <- best |>
            dplyr::select(
                dplyr::starts_with("jp")
            ) |>
            unlist(
                use.names = FALSE
            ) |>
            stats::na.omit() |>
            as.numeric()

        # ========================================================
        # ---- Final model ----
        # ========================================================
        model <- fit_jp(
            x = x,
            y = y,
            k = joinpoints
        )

        # ========================================================
        # ---- Results of the best model ----
        # ========================================================
        list(
            fit = model,
            joinpoints = joinpoints,
            time = x,
            log_rate = y,
            BIC = if (method == "wbic") {
                best$WBIC
            } else if (method == "bic3") {
                best$BIC3
            } else {
                best$BIC
            },
            model_sel = results
        )
    }

    # ==========================================================
    # ---- Fit models by group ----
    # ==========================================================
    mods <- purrr::imap(
        data,
        fit_group
    )

    mods <- mods |>
        purrr::set_names(nm = mod_names)

    # ==========================================================
    # ---- Message ----
    # ==========================================================
    purrr::iwalk(
        mods,
        ~ {
            jp <- .x$joinpoints

            message(paste0(
                "Model: ",
                .y,
                "| Joinpoints: ",
                if (length(jp) > 0) {
                    paste(jp, collapse = ", ")
                } else {
                    "None detected"
                },
                if (method == "wbic") {
                    "| WBIC: "
                } else if (method == "bic3") {
                    " | BIC3: "
                } else {
                    " | BIC: "
                },
                round(.x$BIC, 3)
            ))
        }
    )

    # ==========================================================
    # ---- Return ----
    # ==========================================================
    res <- structure(mods, class = "model_jp")

    attr(res, "call") <- match.call()

    res
}


# ---- Use model_jp ----
#' @export
model_jp <- function(
    data,
    ...
) {
    model_jp_grid(
        data,
        ...
    )
}

#' @export
update.model_jp <- function(mod, ...) {
    # --- Extract the model call ---
    call <- attr(mod, "call")

    # --- Capture new arguments ---
    extras <- rlang::enquos(...)

    if (length(extras) > 0) {
        for (name in names(extras)) {
            call[[name]] <- rlang::quo_get_expr(extras[[name]])
        }
    }

    # --- Reevaluate the call ---
    eval(call, envir = parent.frame())
}
