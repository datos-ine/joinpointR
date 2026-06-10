# Load packages ----------------------------------------------------------
library(dplyr)
library(tidyr)

# Generate data ----------------------------------------------------------
set.seed(123)

hiv_data <- expand_grid(
  year = 2010:2025,
  sex = c("Male", "Female"),
  tibble(
    region = c("North", "Central", "South", "East", "West"),
    slope1 = c(-6, -5, 3, -7, 2),
    slope2 = c(4, 3, -4, 2, -5)
  )
) |>
  mutate(
    trend = if_else(
      year <= 2019,
      slope1 * (year - 2015),
      slope1 * 4 + slope2 * (year - 2019)
    ),
    hiv_rate = 100 +
      trend +
      if_else(sex == "Male", 10, 0) +
      rnorm(n(), 0, 0.8)
  ) |>

  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  select(year, region, sex, hiv_rate)


usethis::use_data(hiv_data, overwrite = TRUE)
