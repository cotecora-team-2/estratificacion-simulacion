library(tidyverse)
library(devtools)
#devtools::install_github("cotecora-team-2/quickcountmx")
library(quickcountmx)

sims <- read_rds(file = "./datos/simulaciones_completo.rds")

# Un estado
sims_estado <- filter(sims, state_abbr == "CHIH")

# Marco Chihuahua
data("conteo_2018")
marco_estado <- conteo_2018 %>%
  filter(state_abbr == "CHIH") %>%
  mutate(estrato_df = ID_DISTRITO)
nrow(marco_estado)

muestras_sim <- map_df(1:100,
                       function(x) {
                         muestra <- select_sample_prop(marco_estado, stratum = ID_DISTRITO, frac = 0.05)
                         muestra$id <- x
                         muestra_tiempos <- left_join(muestra, sims_estado, by = c("CLAVE_CASILLA", "state_abbr", "id"))
                         muestra_tiempos
                       })

# porcentaje recibido
muestras_sim <- muestras_sim %>%
  group_by(id) %>%
  mutate(prop_obs = percent_rank(time))

estratos_tbl <- marco_estado %>% group_by(ID_DISTRITO) %>% count()
partidos <- c("AMLO", "JAMK", "RAC", "CAND_IND_02", "CNR")

estimaciones_sin_censura <-
  muestras_sim %>% split(.$id) %>%
  map_df(~(ratio_estimation(.x, stratum = ID_DISTRITO,
                            data_stratum = estratos_tbl, n_stratum = n,
                            parties = any_of(partidos), std_errors = FALSE)), .id = "id")
#estimaciones_sin_censura

estimaciones_censuradas <-
  muestras_sim %>%
  group_by(id) %>%
  filter(prop_obs <= 0.5) %>%
  split(.$id) %>%
  map_df(~(ratio_estimation(.x, stratum = ID_DISTRITO,
                            data_stratum = estratos_tbl, n_stratum = n,
                            parties = any_of(partidos), std_errors = FALSE)), .id = "id")

#estimaciones_censuradas

est_tbl <- left_join(estimaciones_censuradas %>% rename(prop_cens = prop),
                     estimaciones_sin_censura %>% rename(prop_sin_cens = prop),
                     by = c("id", "party"))

ggplot(est_tbl, aes(x = prop_sin_cens, y = prop_cens, colour = party)) +
  geom_point() + geom_abline()

est_tbl %>%
  pivot_longer(cols = starts_with("prop"), names_to = "tipo", values_to = "prop") %>%
  group_by(party, tipo) %>%
  summarise(media = mean(prop), ee =  sd(prop))
