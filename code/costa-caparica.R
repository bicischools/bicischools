library(sf)
library(tmap)
tmap_mode("view")
library(dplyr)

# Input SIM model results
res_output = read_sf("res_output_almada.geojson")

schools_c1 = readRDS("data/c1.Rds")
schools_c1 = schools_c1 |>
  filter(MUNICIPIO == "Almada")

schools_c1$case_study = grepl("Escola Básica n.º 2 da Costa da Caparica, Almada", schools_c1$ESCOLA)
case_study = schools_c1[which(schools_c1$case_study),]

# Get OD desire lines for Costa da Caparica school
res_cc = res_output |> 
  filter(
    D == case_study$DGEEC_id
    # , !is.na(trips_modelled)
    )

tm_shape(res_cc) + tm_lines()

# Convert desire lines to routes

# Create route network