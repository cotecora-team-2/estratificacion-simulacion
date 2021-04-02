library(tidyverse)
michoacan <- read_csv("datos/Gobernador2015Michoacan_Casillas_con_stratos.csv")
chihuahua <- read_csv("datos/Gobernador2016Chihuahua_Casillas_con_stratos.csv")
colima <- read_csv("datos/Gobernador2016Colima_Casillas_con_stratos.csv")
zacatecas <- read_csv("datos/Gobernador2016Colima_Casillas_con_stratos.csv")
nayarit <- read_csv("datos/Gobernador2016Colima_Casillas_con_stratos.csv")

generar_muestras_con_censura <- function(marco_muestral,
                                         n,
                                         fracc_muestreo,
                                         censuras,
                                         total_votos=VOTOS_TOTAL,
                                         variable_censura){
  variable_c <- quo(1-!!enquo(variable_censura)/(!!enquo(total_votos)+0.1))
  censuras %>% map_df(~{
    censura <- .x
    1:n %>% map_df(~{
      muestras <- marco_muestral %>%
        group_by(estrato_df) %>%
        sample_frac(size=fracc_muestreo) %>%
        tidyr::nest(`muestras aleatorias`=!NOMBRE_ESTADO) %>%
        mutate(frac_muestreo=fracc_muestreo,
               censura=censura,
               `muestras censuradas`=map2(`muestras aleatorias`,
                                          censura, ~{
                                            .x  %>% ungroup() %>%
                                              mutate(auxiliar=!!variable_c) %>%
                                              sample_frac(size = 1-.y, weight = auxiliar)
                                          }))
    })

  }
  )
}
set.seed(2021)
# Michoacan
muestras_michoacan <- generar_muestras_con_censura(michoacan,
                                                   n = 100,
                                                   fracc_muestreo = 0.065,
                                                   censuras = c(.5,.6,.7,.8,.9),
                                                   variable_censura = PRD)
saveRDS(muestras_michoacan, file = "datos/muestras/michoacan.rds")
# Chihuahua
muestras_chihuahua <- generar_muestras_con_censura(chihuahua,
                                                   n = 100,
                                                   fracc_muestreo = 0.06,
                                                   censuras =  c(.5,.6,.7,.8,.9),
                                                   variable_censura = PAN)
saveRDS(muestras_chihuahua, file = "datos/muestras/chihuahua.rds")
# Colima
muestras_colima <- generar_muestras_con_censura(colima,
                                                n = 100,
                                                fracc_muestreo = 0.2,
                                                censuras =  c(.5,.6,.7,.8,.9),
                                                total_votos = VOTOS_TOTAL_17ene16,
                                                variable_censura = PRI)
saveRDS(muestras_colima, file = "datos/muestras/colima.rds")
# Zacatecas
muestras_zacatecas <- generar_muestras_con_censura(zacatecas,
                                                   n = 100,
                                                   fracc_muestreo = 0.15,
                                                   censuras = c(.5,.6,.7,.8,.9),
                                                   total_votos = VOTOS_TOTAL_17ene16,
                                                   variable_censura = PRI)
saveRDS(muestras_zacatecas, file = "datos/muestras/zacatecas.rds")
# Nayarit
muestras_nayarit <- generar_muestras_con_censura(nayarit,
                                                 n = 100,
                                                 fracc_muestreo = .2,
                                                 censuras =  c(.5,.6,.7,.8,.9),
                                                 total_votos = VOTOS_TOTAL_17ene16,
                                                 variable_censura = PAN)
saveRDS(muestras_nayarit, file = "datos/muestras/nayarit.rds")


