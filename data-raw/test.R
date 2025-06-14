library(tidyverse)
library(sf)

bgri = sf::read_sf("../internal/BGRI21_170.gpkg")
bgri_4326 = sf::st_transform(bgri, 4326)
bgri_4326 = sf::st_make_valid(bgri_4326)

# # Restrict to Lisbon city only
# bgri_4326 = bgri_4326 |>
#   filter(DTMN21 == 1106)
# schools_c1 = readRDS("data/lisbon_c1.Rds")

# Restrict to Almada only
bgri_4326 = bgri_4326 |>
  filter(DTMN21 == 1503)
schools_c1 = readRDS("data/c1.Rds")
schools_c1 = schools_c1 |>
  filter(MUNICIPIO == "Almada")

# See schools.qmd
# schools_c1 = readRDS("data/c1_public.Rds")
schools_c1 = schools_c1 |>
  mutate(
    n_pupils = STUDENTS
  ) |>
  select(DGEEC_id, n_pupils)

home_zones = bgri_4326 |>
  select(OBJECTID, N_INDIVIDUOS_0_14)


results <- sim_schools(
  origins = home_zones,
  destinations = schools_c1,
  model = "gravity",
  max_dist = 3000,
  balancing = "destinations",
  d = distance_euclidean,
  m = origin_N_INDIVIDUOS_0_14,
  n = destination_n_pupils,
  constraint_production = origin_N_INDIVIDUOS_0_14,
  beta = 0.99944,
  keep_cols = T
)


results |>
  st_drop_geometry() |>
  summarise(across(modelled_trips, sum), .by = c(O, origin_N_INDIVIDUOS_0_14))


origin_only <- results |>
  st_drop_geometry() |>
  select(O, origin_N_INDIVIDUOS_0_14) |>
  unique()

dest_only <- results |>
  st_drop_geometry() |>
  select(D, destination_n_pupils) |>
  unique()

results$modelled_trips |> sum(na.rm = TRUE)


origin_only |> pull(origin_N_INDIVIDUOS_0_14) |> sum()
sum(home_zones$N_INDIVIDUOS_0_14)

dest_only |> pull(destination_n_pupils) |> sum()
sum(schools_c1$n_pupils)

results$O |> unique() |> length()

results$D |> unique() |> length()


unique_destination <- results$D |> unique() |> head(1)


routes <- results |> filter(D == unique_destination) |> bici_routes_osrm()

results_unique <- results |>
  filter(D == unique_destination)
routes_cyclestreets <- results_unique |>
  bici_routes_cyclestreets()


nrow(results_unique)

# Save the processed minimal datasets with usethis::use_data
# Give the datasets more general and clear names
od_data_almada <- results_unique |>
  select(O, D, modelled_trips) |>
  rename(
    origin = O,
    destination = D,
    trips = modelled_trips
  )

schools_lisbon <- schools_c1 |>
  mutate(
    id = DGEEC_id,
    n_pupils = n_pupils,
    geometry,
    .keep = "used" 
  )

origins_lisbon <- bgri_4326 |>
  select(OBJECTID, N_INDIVIDUOS_0_14) |>
  rename(
    id = OBJECTID,
    pupils = N_INDIVIDUOS_0_14
  )

library(tmap)
tmap_mode("view")

tmap::tm_shape(schools_lisbon) +
  tmap::tm_dots(fill = "red", size = 1) +
  tmap::tm_shape(origins_lisbon) +
  tmap::tm_dots() +
  tmap::tm_shape(od_data_almada) +
  tmap::tm_lines(col = "grey")

usethis::use_data(
  od_data_almada,
  schools_lisbon,
  origins_lisbon,
  overwrite = TRUE
)
list.files("data")
