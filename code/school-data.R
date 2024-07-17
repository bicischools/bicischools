library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
tmap_mode("view")

schools_basic = read_sf("../internal/SCHOOLS_basicsec.gpkg")

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
# schools_1_ciclo = read_sf("../internal/escolas-publicas-1-ciclo.geojson")


# Census data for Lisbon Metropolitan Area (very detailed geog) -----------

bgri = read_sf("../internal/BGRI21_170/BGRI21_170.gpkg")

tm_shape(bgri) + tm_polygons("N_INDIVIDUOS_0_14")

bgri_lisbon = bgri |> 
  filter(DTMN21 == 1106)
tm_shape(bgri_lisbon) + tm_polygons()

# Not needed
# # District and region polygons for continental Portugal
# # from https://forest-gis.com/shapefiles-de-portugal/
# # via https://web.archive.org/web/20211121211516/http://mapas.dgterritorio.pt/ATOM-download/CAOP-Cont/Cont_AAD_CAOP2020.zip
# cont = read_sf("../internal/Cont_AAD_CAOP2020/Cont_AAD_CAOP2020.shp")
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

library(readxl)
dgeec_schools = read_xlsx("../internal/DGEEC_AlunosMatriculados_Continente2122.xlsx")

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
table(dgeec_totals$ORIENTAÇÃO, useNA = "ifany")
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

school_geo = read_sf("../internal/schools.csv")
View(school_geo)
summary(school_geo$id)

# Join with dgeec data based on school names
