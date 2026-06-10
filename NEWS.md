# joinpointR 1.0.0

## Improvements
- Added package vignettes.
- Added the example dataset `vih_data`.
- `model_jp()` now accepts unquoted response and time variables. Messages and outputs have also been simplified.
- `get_apc()` and `get_aapc()` now accept either a list of models or an individual model.
- `summary_jp()` now returns a tibble; the flextable format has been moved to `as_ft_jp()`.
- `gg_jpoint()` now supports faceting by one or two grouping variables, or no faceting at all. It also includes several colorblind-friendly palettes.

# joinpointR 0.6.2


## Improvements

- Added the `step` argument to `model_jp()`.
- Added support for multiple grouping variables in `model_jp()`. Internally,
  the function creates a grouping variable based on their interaction.

## Bug fixes
- Fixed issues when merging grouping variables.
- Fixed output issues in `get_apc()`.


# joinpointR 0.5.0

## Improvements

- Added `gg_jpoint()`.
- Improved language handling in `summary_jp()`.
