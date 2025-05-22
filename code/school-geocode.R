# aim: geocode missing schools

library(tidyverse)
library(sf)
library(mapview)

# Load data ---------------------------------------------------------------

# AML
AMLgeo = readRDS(url(
  "https://github.com/U-Shift/biclar/releases/download/0.0.1/MUNICIPIOSgeo.Rds"
))
AMLgeo$Concelho[AMLgeo$Concelho == "Setubal"] = "Setúbal"
Concelhos_list = unique(AMLgeo$Concelho)

# TML school coordinates
schools_official_coors = read.csv(
  "https://github.com/carrismetropolitana/datasets/raw/latest/facilities/schools/schools.csv"
)
schools_official_coors = st_as_sf(
  schools_official_coors,
  coords = c("lon", "lat"),
  crs = 4326
)
mapview(schools_official_coors)
names(schools_official_coors)

# Groupping with data with addresses
# https://www.gesedu.pt/PesquisaRede and Select "Todas" (all), and in the bottom there will be a "Exportar Excel" green Burton after "unidades orgânicas"
groupping_dgeec = readxl::read_xlsx("data_original/RedeUO.xlsx")
groupping_dgeec = groupping_dgeec |> filter(CONCELHO %in% Concelhos_list)

# Schools DGEEC with enrolled students per year
# available at https://www.dgeec.medu.pt/api/ficheiros/65708d9fcc461a4d46cd9a13
schools_dgeec = readxl::read_xlsx(
  "data_original/DGEEC_AlunosMatriculados_Continente2122.xlsx"
)
schools_dgeec = schools_dgeec |> filter(MUNICÍPIO %in% Concelhos_list)
names(schools_dgeec)
# table(schools_dgeec$`ANO LETIVO`) # 2021/2022
table(schools_dgeec$`NÍVEL DE  ENSINO`)
schools_dgeec_totals = schools_dgeec |>
  filter(`NÍVEL DE  ENSINO` %in% c("Ensino básico", "Ensino secundário")) |>
  group_by(
    `CÓDICO DGEEC ENTIDADE`,
    ENTIDADE,
    `CÓDIGO DGEEC ESCOLA`,
    ESCOLA,
    `CÓDIGO DGEEC AGRUPAMENTO`,
    AGRUPAMENTO,
    `CÓDIGO DGEEC ESCOLA SEDE`,
    `ESCOLA SEDE`,
    MUNICÍPIO,
    `CICLO DE ESTUDOS`
  ) |>
  summarise(Alunos = sum(`NÚMERO DE ALUNOS MATRICULADOS`)) |>
  ungroup()


# Try to match ------------------------------------------------------------

schools_dgeec_escola = schools_dgeec_totals |>
  select(`CÓDIGO DGEEC ESCOLA`, ESCOLA, MUNICÍPIO) |>
  distinct() |>
  mutate(ESCOLA_nome = str_split_fixed(ESCOLA, pattern = ", ", n = 2))
schools_dgeec_escola$ESCOLA_local = schools_dgeec_escola$ESCOLA_nome[, 2]
schools_dgeec_escola$ESCOLA_nome = schools_dgeec_escola$ESCOLA_nome[, 1]

schools_dgeec_escola_coors = schools_dgeec_escola |>
  left_join(
    schools_official_coors |> select(name, municipality_name, geometry),
    by = c("ESCOLA_nome" = "name", "MUNICÍPIO" = "municipality_name")
  ) |>
  st_as_sf()


schools_dgeec_escola_coors_OK = schools_dgeec_escola_coors |>
  filter(!st_is_empty(geometry)) # 93%
schools_dgeec_escola_coors_MISSING = schools_dgeec_escola_coors |>
  filter(st_is_empty(geometry)) |>
  filter(!is.na(ESCOLA))


# geocode missing ones ----------------------------------------------------

library(ggmap)
register_google(Sys.getenv("GOOGLE_KEY"))

schools_dgeec_escola_coors_MISSING$ESCOLA_nome[
  schools_dgeec_escola_coors_MISSING$ESCOLA_nome == "Colégio Art & Manha"
] = "Colégio Art e Manha"
schools_dgeec_escola_coors_MISSING$address = paste0(
  schools_dgeec_escola_coors_MISSING$ESCOLA_nome,
  ", ",
  schools_dgeec_escola_coors_MISSING$MUNICÍPIO,
  ", Portugal"
)

googlemaps = ggmap::geocode(schools_dgeec_escola_coors_MISSING$address)
schools_dgeec_escola_coors_MISSING$lat = googlemaps$lat
schools_dgeec_escola_coors_MISSING$lon = googlemaps$lon

schools_dgeec_escola_coors_MISSING = schools_dgeec_escola_coors_MISSING |>
  st_drop_geometry() |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
mapview(schools_dgeec_escola_coors_MISSING)

# bind OK and geocoded ----------------------------------------------------

schools_dgeec_escola_coors = rbind(
  schools_dgeec_escola_coors_OK,
  schools_dgeec_escola_coors_MISSING |>
    select(dput(names(schools_dgeec_escola_coors_OK)))
)

length(unique(schools_dgeec_escola_coors$`CÓDIGO DGEEC ESCOLA`)) # 1169
length(unique(schools_dgeec_totals$`CÓDIGO DGEEC ESCOLA`)) # 1170

schools_dgeec_geo = schools_dgeec_totals |>
  select(`CÓDIGO DGEEC ESCOLA`, MUNICÍPIO) |>
  left_join(
    schools_dgeec_escola_coors |>
      select(`CÓDIGO DGEEC ESCOLA`, ESCOLA_nome, geometry),
    by = "CÓDIGO DGEEC ESCOLA"
  ) |>
  select(`CÓDIGO DGEEC ESCOLA`, ESCOLA_nome, MUNICÍPIO, geometry) |>
  distinct() |>
  filter(!is.na(ESCOLA_nome)) |>
  st_as_sf()

names(schools_dgeec_geo)["CÓDIGO DGEEC ESCOLA"] = "DGEEC_id"

SCHOOLS_geo = schools_dgeec_geo
mapview(SCHOOLS_geo, zcol = "MUNICÍPIO")

# Save --------------------------------------------------------------------

SCHOOLS_year = schools_dgeec |>
  filter(`NÍVEL DE  ENSINO` %in% c("Ensino básico", "Ensino secundário")) |>
  group_by(
    `CÓDIGO DGEEC ESCOLA`,
    ESCOLA,
    MUNICÍPIO,
    NATUREZA,
    `CICLO DE ESTUDOS`,
    `ANO DE ESCOLARIDADE`
  ) |>
  summarise(Alunos = sum(`NÚMERO DE ALUNOS MATRICULADOS`)) |>
  ungroup()

names(SCHOOLS_year) = c(
  "DGEEC_id",
  "ESCOLA",
  "MUNICIPIO",
  "PUBLIC_PRIVATE",
  "CICLO_ESTUDOS",
  "ANO_ESCOLARIDADE",
  "STUDENTS"
)

# change CICLO_ESTUDOS to "Secondary" if ANO_ESCOLARIDADE is 10, 11 or 12
SCHOOLS_year$CICLO_ESTUDOS[
  SCHOOLS_year$ANO_ESCOLARIDADE %in% c("10.º Ano", "11.º Ano", "12.º Ano")
] = "Secondary"

saveRDS(SCHOOLS_geo, "data/SCHOOLS_geo.Rds")
saveRDS(SCHOOLS_year, "data/SCHOOLS_year.Rds")

SCHOOLS_geo = readRDS("data/SCHOOLS_geo.Rds")
SCHOOLS_year = readRDS("data/SCHOOLS_year.Rds")


# simple stats ------------------------------------------------------------

SCHOOLS_year |>
  group_by(PUBLIC_PRIVATE) |>
  summarise(Alunos = sum(STUDENTS)) |>
  knitr::kable(caption = "Number of students by school type")

SCHOOLS_year |>
  group_by(CICLO_ESTUDOS) |>
  summarise(Alunos = sum(STUDENTS)) |>
  knitr::kable(caption = "Number of students by school level")
