library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
tmap_mode("view")

data_dir = "../internal"
data_dir_onedrive = "C:/Users/georl_admin/University of Leeds/Joseph Talbot - BiciSchools"
if (dir.exists(data_dir_onedrive)) {
  data_dir = data_dir_onedrive
  # files_in_data_dir = list.files("data", full.names = TRUE)
  # files_in_onedrive = file.path(data_dir_onedrive, basename(files_in_data_dir))
  # file.exists(files_in_onedrive)
  # file.copy(files_in_data_dir, files_in_onedrive)
}
schools_basic = read_sf(glue::glue("{data_dir}/SCHOOLS_basicsec.gpkg"))

View(schools_basic)
table(schools_basic$Nivel, useNA = "ifany")
# Basico Secundario 
# 106         20 
summary(schools_basic$Alunos)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.0   151.8   234.0   355.6   476.8  1758.0 

ggplot(schools_basic |> filter(Nivel == "Basico"), aes(Alunos)) + 
  geom_histogram() + 
  ggtitle("Primary school") + 
  xlab("Students")

ggplot(schools_basic |> filter(Nivel == "Secundario"), aes(Alunos)) + 
  geom_histogram(binwidth = 100) +
  ggtitle("Secondary school") + 
  xlab("Students")

tm_shape(schools_basic) + tm_dots("Nivel", size = "Alunos")

schools_primary = schools_basic |> 
  filter(Nivel == "Basico")

# Doesn't work:
# schools_1_ciclo = read_sf(glue::glue("{data_dir}/escolas-publicas-1-ciclo.geojson"))


# Census data for Lisbon Metropolitan Area (very detailed geog) -----------

bgri = read_sf(glue::glue("{data_dir}/BGRI21_170/BGRI21_170.gpkg"))

bgri_lisbon = bgri |> 
  filter(DTMN21 == 1106)
# tm_shape(bgri) + tm_polygons("N_INDIVIDUOS_0_14") # slow
# tm_shape(bgri_lisbon) + tm_polygons()

# Not needed
# # District and region polygons for continental Portugal
# # from https://forest-gis.com/shapefiles-de-portugal/
# # via https://web.archive.org/web/20211121211516/http://mapas.dgterritorio.pt/ATOM-download/CAOP-Cont/Cont_AAD_CAOP2020.zip
# cont = read_sf(glue::glue("{data_dir}/Cont_AAD_CAOP2020/Cont_AAD_CAOP2020.shp")
# 
# tm_shape(cont) + tm_polygons("Concelho")
# 
# st_crs(cont)
# st_crs(bgri)
# 
# lisboa = cont |> 
#   filter(Concelho == "Lisboa")
# tm_shape(lisboa) + tm_polygons()
# 
# bgri_lisboa = bgri[lisboa, ]

tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(schools_primary) + tm_dots(size = "Alunos")


# DGEEC school data -------------------------------------------------------
# Nationwide data on every Portuguese with n. per age group
library(readxl)
dgeec_schools = read_xlsx(glue::glue("{data_dir}/DGEEC_AlunosMatriculados_Continente2122.xlsx"))

dgeec_lma = dgeec_schools |> 
  filter(`NUTS II (2013)` == "Área Metropolitana de Lisboa")
dgeec_lisbon = dgeec_lma |> 
  filter(MUNICÍPIO == "Lisboa")

length(unique(dgeec_lisbon$ESCOLA))
# [1] 390

table(dgeec_lisbon$`CICLO DE ESTUDOS`, useNA = "ifany")
# 1.º Ciclo 2.º Ciclo 3.º Ciclo      <NA> 
#   1418       401       705      2732

# We want schools for children only
table(dgeec_lisbon$ORIENTAÇÃO, useNA = "ifany")
# Programas educativos orientados para adultos       Programas educativos orientados para crianças 
# 271                                                 556 
# Programas educativos orientados para jovens Programas educativos orientados para jovens/adultos 
# 4358                                                  71 

dgeec_totals = dgeec_lisbon |> 
  filter(`NÍVEL DE  ENSINO` == "Ensino básico",
         ORIENTAÇÃO == "Programas educativos orientados para jovens"
         ) |> 
  group_by(ESCOLA, `CICLO DE ESTUDOS`, ORIENTAÇÃO) |> 
  summarise(
    Alunos = sum(`NÚMERO DE ALUNOS MATRICULADOS`)
    )

table(dgeec_totals$`CICLO DE ESTUDOS`, useNA = "ifany")
# 1.º Ciclo 2.º Ciclo 3.º Ciclo 
# 186        91       104 

ggplot(dgeec_totals |> filter(`CICLO DE ESTUDOS` == "1.º Ciclo"), aes(Alunos)) + 
  geom_histogram() +
  ggtitle("1.º Ciclo, Lisbon City") + 
  xlab("Students")

ggplot(dgeec_totals |> filter(`CICLO DE ESTUDOS` == "2.º Ciclo"), aes(Alunos)) + 
  geom_histogram() +
  ggtitle("2.º Ciclo, Lisbon City") + 
  xlab("Students")

ggplot(dgeec_totals |> filter(`CICLO DE ESTUDOS` == "3.º Ciclo"), aes(Alunos)) + 
  geom_histogram() +
  ggtitle("3.º Ciclo, Lisbon City") + 
  xlab("Students")

# There are a lot of very small schools. Any school with <100 students will have little chance of bike buses.
summary(dgeec_totals$Alunos)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    63.0   135.0   157.9   229.0   743.0 

# Next, use public schools only and see if the schools are larger
table(dgeec_lisbon$NATUREZA, useNA = "ifany")
# Privado dependente do estado         Privado independente                      Público 
# 252                         2415                         2589 

dgeec_public = dgeec_lisbon |> 
  filter(`NÍVEL DE  ENSINO` == "Ensino básico",
         ORIENTAÇÃO == "Programas educativos orientados para jovens",
         NATUREZA == "Público"
         ) |> 
  group_by(ESCOLA, `CICLO DE ESTUDOS`, ORIENTAÇÃO) |> 
  summarise(
    Alunos = sum(`NÚMERO DE ALUNOS MATRICULADOS`)
  )


table(dgeec_public$`CICLO DE ESTUDOS`, useNA = "ifany")
# 1.º Ciclo 2.º Ciclo 3.º Ciclo 
# 89        42        56 

ggplot(dgeec_public |> filter(`CICLO DE ESTUDOS` == "1.º Ciclo"), aes(Alunos)) + 
  geom_histogram() +
  ggtitle("1.º Ciclo, Lisbon City") + 
  xlab("Students")

ggplot(dgeec_public |> filter(`CICLO DE ESTUDOS` == "2.º Ciclo"), aes(Alunos)) + 
  geom_histogram() +
  ggtitle("2.º Ciclo, Lisbon City") + 
  xlab("Students")

ggplot(dgeec_public |> filter(`CICLO DE ESTUDOS` == "3.º Ciclo"), aes(Alunos)) + 
  geom_histogram() +
  ggtitle("3.º Ciclo, Lisbon City") + 
  xlab("Students")

# There are still some tiny schools, but the mean size has increased
summary(dgeec_public$Alunos)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.0   115.0   178.0   193.2   261.0   603.0 


# School locations --------------------------------------------------------

# Location data from https://github.com/carrismetropolitana/datasets/blob/latest/facilities/schools/schools.csv
# This covers the whole Lisbon Metropolitan Area

school_geo = read_csv(glue::glue("{data_dir}/schools.csv"))
# View(school_geo)
# summary(school_geo$id)

school_geom = st_as_sf(school_geo, coords = c("lon", "lat"), crs = 4326)

tm_shape(school_geom) + tm_dots()

# Lisbon City only
school_geom_filtered = school_geom |> 
  filter(municipality_name == "Lisboa",
         university == 0,
         pre_school == 0
         )

# Join with dgeec data based on school names
schools_joined = inner_join(dgeec_totals, school_geom, by = c("ESCOLA" = "name")) # only 7 matches

# clean names
school_geom_clean = school_geom_filtered |> 
  mutate(name_clean = name)

# School address database from https://www.gesedu.pt/PesquisaRede
# This includes the same school ID codes as `school_geom`
rede_escolas = read_xlsx(glue::glue("{data_dir}/RedeEscolas.xlsx"))
dim(rede_escolas)
# [1] 190  19

# inner or right join??
rede_joined = right_join(rede_escolas, school_geom, by = c("CODIGO" = "id"))

# Check - every school in `rede_escolas` can be found in `school_geom`
dim(rede_joined)
# [1] 190  54

# Try joining now
join_schools = left_join(dgeec_totals, rede_joined, by = c("ESCOLA" = "NOME"))
join_schools = st_as_sf(join_schools)

# there are still 135 schools missing locations
sum(is.na(join_schools$CODIGO))
# [1] 135

# Schools without location
without_location = join_schools |> 
  filter(is.na(CODIGO))

# We're missing locations for 42% of students
sum(join_schools$Alunos)
# [1] 60146
sum(without_location$Alunos)
# [1] 25332
sum(without_location$Alunos)/sum(join_schools$Alunos)*100
# [1] 42.11751

with_location = join_schools |> 
  filter(!is.na(CODIGO))
tm_shape(with_location) + tm_dots()

# tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
#   tm_shape(with_location) + tm_dots(
#     # col = "CICLO DE ESTUDOS", 
#     # palete = c(`1.º Ciclo` = "blue", `2.º Ciclo` = "green", `3.º Ciclo` = "yellow"),
#     size = "Alunos")
# tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
#   tm_shape(with_location) + tm_dots(shape = "CICLO DE ESTUDOS", size = 1)


# without_joined = inner_join(without_location, rede_joined, by = c("ESCOLA" = "name"))


# Divide into the 3 ciclos ------------------------------------------------

ciclo_1 = with_location |> 
  filter(`CICLO DE ESTUDOS` == "1.º Ciclo")
ciclo_2 = with_location |> 
  filter(`CICLO DE ESTUDOS` == "2.º Ciclo")
ciclo_3 = with_location |> 
  filter(`CICLO DE ESTUDOS` == "3.º Ciclo")

tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(ciclo_1) + tm_dots(size = "Alunos")
tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(ciclo_2) + tm_dots(size = "Alunos")
tm_shape(bgri_lisbon) + tm_polygons("N_INDIVIDUOS_0_14") +
  tm_shape(ciclo_3) + tm_dots(size = "Alunos")

# We're especially missing ciclo 2 and 3 schools
sum(ciclo_1$Alunos) # 4 school years
# [1] 21622 
sum(ciclo_2$Alunos) # 2 school years
# [1] 5691
sum(ciclo_3$Alunos) # 3 school years
# [1] 7501

saveRDS(ciclo_1, "ciclo-1.Rds")
saveRDS(ciclo_2, "ciclo-2.Rds")
saveRDS(ciclo_3, "ciclo-3.Rds")
