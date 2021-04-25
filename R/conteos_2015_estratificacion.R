library("quickcountmx")
library("tidyverse")

filenames <- c("Colima","Chihuahua","Michoacan","Nayarit","Zacatecas")

df_marg <- read.csv("datos/conteo_15_inegi_10_sin_imputar.csv")
df_get_casillas <- read.csv("datos/info_p_estrat_2015_w_CAND1-3_from_Votacion_15estados.csv")

marg <- df_get_casillas %>%
  merge(df_marg, by = c("ID_ESTADO", "ID_DISTRITO_15", "SECCION_15", "CAND1", "CAND2", "CAND3")) %>%
  #mutate(clave=paste(ID_ESTADO,ID_DISTRITO_15,SECCION_15,CAND1,CAND2,CAND3,sep="_")) %>%
  select(-.rownames) %>% unique() %>%
  select(ID_ESTADO,ID_DISTRITO_15,CABECERA_DISTRITAL,SECCION_15,CASILLA,LISTA_NOMINAL_15,.fittedPC1)

get_state_data <- function(state) {
  path <- "datos/Votacion_15estados"
  filename <- list.files(path = path, pattern=paste("^Gobernador.+",state,".+",sep=""))
  con15 <- read.csv(paste(path,filename,sep="/"), sep="|")

  con15
}

# Colima
cont_state_15 <- get_state_data("Colima")

t1 <- cont_state_15 %>%
  left_join(marg, by = c("ID_ESTADO" = "ID_ESTADO",
                     "ID_DISTRITO" = "ID_DISTRITO_15",
                     "CABECERA_DISTRITAL" = "CABECERA_DISTRITAL",
                     "SECCION" = "SECCION_15",
                     "CASILLA" = "CASILLA",
                     "LISTA_NOMINAL_17ene16" = "LISTA_NOMINAL_15"))

# Chihuahua
cont_state_15 <- get_state_data("Chihuahua")

t2 <- cont_state_15 %>%
  left_join(marg, by = c("ID_ESTADO" = "ID_ESTADO",
                         "ID_DISTRITO" = "ID_DISTRITO_15",
                         "CABECERA_DISTRITAL" = "CABECERA_DISTRITAL",
                         "SECCION" = "SECCION_15",
                         "CASILLA" = "CASILLA",
                         "LISTA_NOMINAL" = "LISTA_NOMINAL_15"))




# Michoacan
cont_state_15 <- get_state_data("Michoacan")

t3 <- cont_state_15 %>%
  left_join(marg, by = c("ID_ESTADO" = "ID_ESTADO",
                         "ID_DISTRITO" = "ID_DISTRITO_15",
                         "CABECERA_DISTRITAL" = "CABECERA_DISTRITAL",
                         "SECCION" = "SECCION_15",
                         "CASILLA" = "CASILLA",
                         "LISTA_NOMINAL" = "LISTA_NOMINAL_15"))


# Nayarit
cont_state_15 <- get_state_data("Nayarit")

t4 <- cont_state_15 %>%
  left_join(marg, by = c("ID_ESTADO" = "ID_ESTADO",
                         "ID_DISTRITO" = "ID_DISTRITO_15",
                         "CABECERA_DISTRITAL" = "CABECERA_DISTRITAL",
                         "SECCION" = "SECCION_15",
                         "CASILLA" = "CASILLA",
                         "LISTA_NOMINAL" = "LISTA_NOMINAL_15"))


# Zacatecas
cont_state_15 <- get_state_data("Zacatecas")

t5 <- cont_state_15 %>%
  left_join(marg, by = c("ID_ESTADO" = "ID_ESTADO",
                         "ID_DISTRITO" = "ID_DISTRITO_15",
                         "CABECERA_DISTRITAL" = "CABECERA_DISTRITAL",
                         "SECCION" = "SECCION_15",
                         "CASILLA" = "CASILLA",
                         "LISTA_NOMINAL" = "LISTA_NOMINAL_15"))


estratif_conf <- crossing(num_gpos_marginacion = 2) %>%
  mutate(estratificacion_num = row_number()) %>%
    transpose()

marco_conf_xedo <- function(conteo) {
  map_df(estratif_conf, function(x){
    num_gpos_marginacion <- x$num_gpos_marginacion
    estratificacion_num <- x$estratificacion_num
    marco <- conteo %>%
      group_by(ID_ESTADO) %>%
      mutate(estratificacion_num = estratificacion_num) %>%
      mutate(indice_grupo = ntile(.fittedPC1, num_gpos_marginacion)) %>%
      mutate(estrato_df = interaction(ID_DISTRITO, indice_grupo))
  })
}

write_csv(marco_conf_xedo(t1), file = "datos/Gobernador2016Colima_Casillas_con_stratos.csv")
write_csv(marco_conf_xedo(t3), file = "datos/Gobernador2015Michoacan_Casillas_con_stratos.csv")
write_csv(marco_conf_xedo(t4), file = "datos/Gobernador2017Nayarit_Casillas_con_stratos.csv")
write_csv(marco_conf_xedo(t5), file = "datos/Gobernador2016Zacatecas_Casillas_con_stratos.csv")

get_summary <- function(df) {
  df %>%
     group_by(ID_ESTADO, ID_DISTRITO, indice_grupo, estrato_df) %>%
     count()
}

write_csv(get_summary(marco_conf_xedo(t1)), file = "datos/Resumen2016Colima_Casillas_con_stratos.csv")
write_csv(get_summary(marco_conf_xedo(t3)), file = "datos/Resumen2015Michoacan_Casillas_con_stratos.csv")
write_csv(get_summary(marco_conf_xedo(t4)), file = "datos/Resumen2017Nayarit_Casillas_con_stratos.csv")
write_csv(get_summary(marco_conf_xedo(t5)), file = "datos/Resumen2016Zacatecas_Casillas_con_stratos.csv")


estratif_conf_ch <- crossing(
  num_gpos_ln = 2) %>%
  mutate(estratificacion_num = row_number()) %>%
  transpose()

marco_conf_ch <- function(conteo) {
  map_df(estratif_conf_ch, function(x){
    num_gpos_ln <- x$num_gpos_ln
    estratificacion_num <- x$estratificacion_num
    marco <- conteo %>%
      group_by(ID_ESTADO) %>%
      mutate(estratificacion_num = estratificacion_num) %>%
      mutate(indice_grupo = ntile(LISTA_NOMINAL, num_gpos_ln)) %>%
      mutate(estrato_df = interaction(ID_DISTRITO, indice_grupo))
  })
}

write_csv(marco_conf_ch(t2), file = "datos/Gobernador2016Chihuahua_Casillas_con_stratos.csv")
write_csv(get_summary(marco_conf_ch(t2)), file = "datos/Resumen2016Chihuahua_Casillas_con_stratos.csv")
