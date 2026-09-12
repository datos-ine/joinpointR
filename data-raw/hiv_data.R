# Load packages ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)


# Load raw data ----------------------------------------------------------
data_raw <- read_csv("data-raw/tasa_vih.csv", locale = locale(decimal_mark = ",")) 

# Clean data -------------------------------------------------------------
hiv_data <- data_raw |>
  # Select columns
 select( 
  admin = jurisdiccion,
  year = anio,
  sex = sexo,
  hiv_rate = jurisdiccion_tasa_vih
 ) |> 

  # Change factor labels
  mutate(admin = case_when(
    admin == "Crdoba" ~ "Córdoba",
    admin == "Entre Ros" ~ "Entre Ríos",
    admin == "Neuqn" ~ "Neuquén",
    admin == "Ro Negro" ~ "Río Negro",
    admin == "Tucumn" ~ "Tucumán",
    .default = admin
  )) |>   
  mutate(sex = case_when(
    sex == "ambos_sexos" ~ "Both",
    sex == "mujeres" ~ "Female",
    .default = "Male"
  )) |> 

  mutate(across(.cols = c(admin, sex), .fns = ~factor(.x)))


usethis::use_data(hiv_data, overwrite = TRUE)
