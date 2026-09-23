# Load raw data ----------------------------------------------------------
data_raw <- readr::read_csv(
  "data_raw/tasa_vih.csv",
  locale = readr::locale(decimal_mark = ",")
)

# Clean data -------------------------------------------------------------
hiv_data <- data_raw |>
  # ---- Select columns ----
  dplyr::select(
    admin = jurisdiccion,
    year = anio,
    sex = sexo,
    hiv_rate = jurisdiccion_tasa_vih
  ) |>

  # --- Change variable labels ----
  dplyr::mutate(
    # Jurisdiction
    admin = dplyr::case_when(
      admin == "Crdoba" ~ "Córdoba",
      admin == "Entre Ros" ~ "Entre Ríos",
      admin == "Neuqun" ~ "Neuquén",
      admin == "Ro Negro" ~ "Río Negro",
      admin == "Tucumn" ~ "Tucumán",
      .default = admin
    ),

    # Sex
    sex = dplyr::case_when(
      sex == "ambos_sexos" ~ "Both sexes",
      sex == "mujeres" ~ "Female",
      .default = "Male"
    ),

    # Character variables as factor
    dplyr::across(.cols = c(admin, sex), .fns = ~ factor(.x))
  ) |>

  # Arrange data
  dplyr::arrange(admin, sex, year)


# ---- Generate dataset ----
usethis::use_data(hiv_data, overwrite = TRUE)
