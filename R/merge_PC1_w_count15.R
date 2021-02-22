library(dplyr)
library(ggfortify)
library(broom)
filenames <- c("Colima","Chihuahua","Michoacan","Nayarit","Zacatecas")
edos_ids_str <-c("06","08","16","18","32")
edos_ids <-c(6,8,16,18,32)
getwd()

get_state_data <- function(state) {
  path <- "../datos/Votacion_15estados"
  filename <- list.files(path = path, pattern=paste("^Gobernador.+",state,".+",sep=""))
  con15 <- read.csv(paste(path,filename,sep="/"), sep="|")

  con15
}

cont_state_15 <- filenames %>%
  map_dfr(~ get_state_data(.))

# Colima
cont_state_15 <- filenames[1] %>%
  map_dfr(~ get_state_data(.))

t1 <- cont_state_15 %>%
  mutate(CAND1 = PAN) %>%
  mutate(CAND2 = reduce(select(.,contains("PRI")|contains("PVEM")|contains("PT")|contains("NVA_ALIANZA")), `+`)) %>%
  mutate(CAND3 = MORENA) %>%
  select(ID_ESTADO, ID_DISTRITO_15 = ID_DISTRITO, SECCION_15 = SECCION, ID_DTTO_FEDERAL_jul2020, SECCION_jul2020, CAND1, CAND2, CAND3)

# Chihuahua
cont_state_15 <- filenames[2] %>%
  map_dfr(~ get_state_data(.))

t2 <- cont_state_15 %>%
  mutate(CAND1 = PAN) %>%
  mutate(CAND2 = reduce(select(.,contains("PRI")|contains("PVEM")|contains("PT")|contains("NVA_ALIANZA")), `+`)) %>%
  mutate(CAND3 = MORENA) %>%
  select(ID_ESTADO, ID_DISTRITO_15 = ID_DISTRITO, SECCION_15 = SECCION, ID_DTTO_FEDERAL_jul2020, SECCION_jul2020, CAND1, CAND2, CAND3)

# Michoacan
cont_state_15 <- filenames[3] %>%
  map_dfr(~ get_state_data(.))

t3 <- cont_state_15 %>%
  mutate(CAND1 = PAN) %>%
  mutate(CAND2 = PRI) %>%
  mutate(CAND3 = MORENA) %>%
  select(ID_ESTADO, ID_DISTRITO_15 = ID_DISTRITO, SECCION_15 = SECCION, ID_DTTO_FEDERAL_jul2020, SECCION_jul2020, CAND1, CAND2, CAND3)

# Nayarit
cont_state_15 <- filenames[4] %>%
  map_dfr(~ get_state_data(.))

t4 <- cont_state_15 %>%
  mutate(CAND1 = reduce(select(.,contains("PAN")|contains("PRD")|contains("PT")|contains("PRS")), `+`)) %>%
  mutate(CAND2 = reduce(select(.,contains("PRI")|contains("PVEM")|contains("NVA_ALIANZA")), `+`)) %>%
  mutate(CAND3 = MORENA) %>%
  select(ID_ESTADO, ID_DISTRITO_15 = ID_DISTRITO, SECCION_15 = SECCION, ID_DTTO_FEDERAL_jul2020, SECCION_jul2020, CAND1, CAND2, CAND3)

# Zacatecas
cont_state_15 <- filenames[5] %>%
  map_dfr(~ get_state_data(.))

t5 <- cont_state_15 %>%
  mutate(CAND1 = reduce(select(.,contains("PAN")|contains("PRD")), `+`)) %>%
  mutate(CAND2 = reduce(select(.,contains("PRI")|contains("PVEM")|contains("NVA_ALIANZA")), `+`)) %>%
  mutate(CAND3 = MORENA) %>%
  select(ID_ESTADO, ID_DISTRITO_15 = ID_DISTRITO, SECCION_15 = SECCION, ID_DTTO_FEDERAL_jul2020, SECCION_jul2020, CAND1, CAND2, CAND3)

cont_15 <- do.call("rbind", list(t1,t2,t3,t4,t5))

path <- "../datos/inegi_distrito_seccion_20.csv"
var_inegi <- read.csv(path,sep=",")


conteo_15_inegi_10_edos_5_all <- left_join(cont_15, var_inegi,
               by=c("ID_ESTADO" = "ENTIDAD", "ID_DTTO_FEDERAL_jul2020" = "DISTRITO_20", "SECCION_jul2020" = "SECCION_20"))

# 1a componente principal
conteo_15_inegi_10_edos_5_all <- conteo_15_inegi_10_edos_5_all %>%
  nest(data = everything()) %>%
  mutate(pca = map(data, ~ prcomp(.x %>% select(CAND1,CAND2,CAND3),
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

conteo_15_inegi_10_edos_5_all <- select(conteo_15_inegi_10_edos_5_all, pca_aug) %>%
  unnest(cols = pca_aug) %>%
  select(-.fittedPC2,-.fittedPC3)

write_csv(conteo_15_inegi_10_edos_5_all, file = "../datos/conteo_15_inegi_10_sin_imputar.csv")
