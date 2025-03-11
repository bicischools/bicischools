library(sf)
library(tmap)
tmap_mode("view")
library(dplyr)

res_output = read_sf("res_output_almada.geojson")

schools_c1 = readRDS("data/c1.Rds")
schools_c1 = schools_c1 |>
  filter(MUNICIPIO == "Almada")

schools_c1$case_study = grepl("Escola Básica n.º 2 da Costa da Caparica, Almada", schools_c1$ESCOLA)
case_study = schools_c1[which(schools_c1$case_study),]

res_cc = res_output |> 
  filter(
    D == case_study$DGEEC_id,
    !is.na(trips_modelled)
    )

tm_shape(res_cc) + tm_lines()

# Why is res_output$trips_modelled always NA?