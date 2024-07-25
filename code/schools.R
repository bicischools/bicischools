library(tidyverse)
library(tmap)
tmap_mode("view")

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

# 64 schools contain some students not assigned to a year group
# We can use the school names to deduce year groups for a lot of these schools
# Although perhaps these are mature students??
table(by_ciclo$CICLO_ESTUDOS, useNA = "ifany")
# 1.º Ciclo 2.º Ciclo 3.º Ciclo Secondary      <NA> 
#   891       320       359       234        64 

no_ciclo = by_ciclo |> 
  filter(is.na(CICLO_ESTUDOS))
no_ciclo$basica = grepl("Básica", no_ciclo$ESCOLA)
no_ciclo$secundaria = grepl("Secundária", no_ciclo$ESCOLA)
no_ciclo = no_ciclo |> 
  mutate(
    basic_only = case_when(basica & !secundaria ~ "yes", TRUE ~ "unclear"),
    basic_and_secondary = case_when(basica & secundaria ~ "yes", TRUE ~ "unclear"),
    secondary_only = case_when(!basica & secundaria ~ "yes", TRUE ~ "unclear")
  )

# now we can estimate numbers of children in each year group accordingly
no_ciclo = no_ciclo |> 
  mutate(
    ciclo_1_students = case_when(basic_only == "yes" ~ round(STUDENTS / 9 * 4),
                                 TRUE ~ 0),
    ciclo_2_students = case_when(basic_only == "yes" ~ round(STUDENTS / 9 * 2),
                                 basic_and_secondary == "yes" ~ round(STUDENTS / 8 * 2),
                                 TRUE ~ 0),
    ciclo_3_students = case_when(basic_only == "yes" ~ round(STUDENTS / 9 * 3),
                                 basic_and_secondary == "yes" ~ round(STUDENTS / 8 * 3),
                                 TRUE ~ 0),
  )

extra_students = no_ciclo |> 
  filter(ciclo_3_students > 0)

##################################################

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

saveRDS(lisbon_c1, "data/lisbon_c1.Rds")
saveRDS(lisbon_c2, "data/lisbon_c2.Rds")
saveRDS(lisbon_c3, "data/lisbon_c3.Rds")

c1 = by_ciclo |> 
  filter(CICLO_ESTUDOS == "1.º Ciclo")
c2 = by_ciclo |> 
  filter(CICLO_ESTUDOS == "2.º Ciclo")
c3 = by_ciclo |> 
  filter(CICLO_ESTUDOS == "3.º Ciclo")

saveRDS(c1, "data/c1.Rds")
saveRDS(c2, "data/c2.Rds")
saveRDS(c3, "data/c3.Rds")

ggplot(c1, aes(STUDENTS)) + 
  geom_histogram() +
  ggtitle("1.º Ciclo, Lisbon Metropolitan Area") + 
  xlab("Students")

ggplot(c2, aes(STUDENTS)) +  
  geom_histogram() +
  ggtitle("2.º Ciclo, Lisbon Metropolitan Area") + 
  xlab("Students")

ggplot(c3, aes(STUDENTS)) + 
  geom_histogram() +
  ggtitle("3.º Ciclo, Lisbon Metropolitan Area") + 
  xlab("Students")
