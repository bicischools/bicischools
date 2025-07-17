## code to prepare `example_school_mcr` dataset goes here

library(tidyverse)

zones = zonebuilder::zb_zone(
    "Manley Park Primary School",
    n_circles = 2
)

study_area = sf::st_union(zones)

mapview::mapview(zones)

osm_data = osmactive::get_travel_network(
    place = study_area,
    boundary = study_area,
    boundary_type = "clipsrc"
)

osm_drive = osmactive::get_driving_network(osm_data)

mapview::mapview(osm_drive, zcol = "maxspeed")


usethis::use_data(example_school_mcr, overwrite = TRUE)

