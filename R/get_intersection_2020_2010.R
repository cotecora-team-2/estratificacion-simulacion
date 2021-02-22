library(tidyverse)
library(sf)
getwd()

edos_ids <-c("06","08","16","18","32")

path <- "../datos/shapefiles/Shp2020" #SECCION.dbf"

path2 <- "../datos/shapefiles/inegi_vivienda_2010"

prj_lcc <- "+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

get_df <- function(state) {
  paths_shps <- state %>%
    map_chr(~ list.files(dir(path, full.names = TRUE, pattern = .x), full.names = TRUE, pattern = ".shp", recursive = TRUE), stringsAsFactors = FALSE)
  paths_inegi_shps <- state %>%
    map_chr(~ list.files(dir(path2, full.names = TRUE, pattern = .x), full.names = TRUE, pattern = ".shp", recursive = TRUE), stringsAsFactors = FALSE)

  df_secc_20 <- map_df(paths_shps, st_read, quiet = TRUE,
                    stringsAsFactors = FALSE)

  df_secc_20_lcc <- df_secc_20 %>%
    sf::st_transform(prj_lcc) %>%
    mutate(valid = st_is_valid(geometry)) %>%
    filter(!is.na(valid)) %>%
    mutate(DISTRITO_20 = DISTRITO_F) %>%
    mutate(SECCION_20 = SECCION) %>%
    select(-DISTRITO_F,-SECCION,-MUNICIPIO,-TIPO,-CONTROL,-DISTRITO_L,-ID,-valid)

  inegi_wgs <- map_df(paths_inegi_shps, st_read, quiet = TRUE,
                    stringsAsFactors = FALSE)
  inegi_lcc <- inegi_wgs %>%
    sf::st_transform(prj_lcc) %>% # transformar a coordenadas planares
    mutate(n_overlaps = lengths(st_overlaps(geometry))) %>%
    mutate(DISTRITO_10 = DISTRITO) %>%
    mutate(SECCION_10 = SECCION) %>%
    select(-DISTRITO,-SECCION)

  df_secc_20_inegi_lcc <- df_secc_20_lcc %>%
    st_join(inegi_lcc, join = st_intersects, largest=TRUE) %>%
    st_drop_geometry()

  df_secc_20_inegi_lcc
}

df_secc_20_inegi_lcc <- edos_ids %>%
  map_dfr(~ get_df(.))

df_secc_20_inegi_df <- df_secc_20_inegi_lcc %>%
  select(ENTIDAD = ENTIDAD.x, DISTRITO_20, SECCION_20, DISTRITO_10, SECCION_10,
         OCUPVIVPAR:VPH_SNBIEN) %>%
  distinct() %>%
  group_by(ENTIDAD, DISTRITO_20, SECCION_20)

write_csv(df_secc_20_inegi_df, file = "../datos/inegi_distrito_seccion_20.csv")


