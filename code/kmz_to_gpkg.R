# aim: extract all the lines from a kmz file and save them in a geopackage file

library(sf)
library(tidyverse)
library(tmap)
tmap_mode("view")
library(mapview)

layers = st_layers("data/ComboiosBicicleta_Lisbon_2024.kmz")
comboios = st_read("data/ComboiosBicicleta_Lisbon_2024.kmz",
                   layer = layers$name[1]) |>
  mutate(name = layers$name[1]) |> 
  st_collection_extract("LINESTRING")

for(i in layers$name){
  comboios_i = st_read("data/ComboiosBicicleta_Lisbon_2024.kmz",
          layer = i) |>
    mutate(name = i) |> 
    st_collection_extract("LINESTRING")
  comboios = comboios |> bind_rows(comboios_i)
}

comboios = comboios |> 
  distinct() |>
  select(name, geometry) 
# class(comboios)

st_write(comboios, "data/comboios.gpkg")

comboios = st_read("data/comboios.gpkg")
mapview(comboios, zcol = "name")
tm_shape(comboios) + tm_lines(col = "name", lwd = 2)
