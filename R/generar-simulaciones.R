library(tidyverse)
library(devtools)
library(furrr)

#devtools::install_github("cotecora-team-2/quickcountmx")
library(quickcountmx)

estados <- c("COL", "NAY", "MICH", "CHIH", "ZAC")
data("arrivals_tbl")
model <- fit_model(arrivals_tbl, estados)
datos <- filter(arrivals_tbl, state_abbr %in% estados)
datos_nuevos <- filter(conteo_2018, state_abbr %in% estados)
# simular para todas las casillas
set.seed(6102)
plan(multicore, workers = 6)
sims <- future_map(1:500,
    ~ simulate_arrivals(.x, model, new_data_tbl = datos_nuevos, hour_censoring = Inf),
    .options = furrr_options(seed = TRUE))

# esta tabla tiene repeticiopnes de tiempos de llegada para todas las casillas
# con esto evitamos hacer el bootstrap paramétrico repetidamente después
sims <- sims %>%
  bind_rows()
readr::write_csv(sims, file = "./datos/simulaciones_completo.csv")
readr::write_rds(sims, file = "./datos/simulaciones_completo.rds")
