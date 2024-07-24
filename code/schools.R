schools_geo = readRDS("data/SCHOOLS_geo.Rds")
schools_year = readRDS("data/SCHOOLS_year.Rds")

summary(schools_year$DGEEC_id) # 83 NAs (no school ID)

tm_shape(schools_geo) + tm_dots()

# Not needed
# library(readxl)
# dgeec_schools = read_xlsx("../internal/DGEEC_AlunosMatriculados_Continente2122.xlsx")
# 
# dgeec_lma = dgeec_schools |> 
#   filter(`NUTS II (2013)` == "Área Metropolitana de Lisboa")
# dgeec_lma_minimal = dgeec_lma |> 
#   select(DGEEC_id = `CÓDIGO DGEEC ESCOLA`,
#          ORIENTAÇÃO,`NÍVEL DE  ENSINO`,
#          ESCOLA)
# dgeec_lma_minimal = distinct(dgeec_lma_minimal)
# 
# schools_with_dgeec = left_join(schools_year, dgeec_lma_minimal)

# schools_both = left_join(schools_year, schools_geo, by = c("DGEEC_id" = "CÓDIGO DGEEC ESCOLA"))
# no_geo = schools_both |> 
#   filter(is.na(ESCOLA_nome))
# summary(schools_both$DGEEC_id) # still 83 NAs (no school ID)

schools_both = inner_join(schools_year, schools_geo, by = c("DGEEC_id" = "CÓDIGO DGEEC ESCOLA"))
schools_both = st_as_sf(schools_both)

by_ciclo = schools_both |> 
  group_by(DGEEC_id, ESCOLA, MUNICIPIO, PUBLIC_PRIVATE, CICLO_ESTUDOS) |> 
  summarise(STUDENTS = sum(STUDENTS))

lisbon_c1 = by_ciclo |> 
  filter(MUNICIPIO == "Lisboa",
         CICLO_ESTUDOS == "1.º Ciclo")
lisbon_c2 = by_ciclo |> 
  filter(MUNICIPIO == "Lisboa",
         CICLO_ESTUDOS == "2.º Ciclo")
lisbon_c3 = by_ciclo |> 
  filter(MUNICIPIO == "Lisboa",
         CICLO_ESTUDOS == "3.º Ciclo")

tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(lisbon_c1) + tm_dots(size = "STUDENTS")
tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(lisbon_c2) + tm_dots(size = "STUDENTS")
tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(lisbon_c3) + tm_dots(size = "STUDENTS")
