## code to prepare `example_school_mcr` dataset goes here

library(tidyverse)
library(osmactive)
library(tmap)


# Rough catchment for Manley Park Primary School
zones = zonebuilder::zb_zone(
    "Whalley Range Sure Start Centre",
    n_circles = 2,
    distance = 0.5
)

study_area = sf::st_union(zones)

mapview::mapview(zones)

library(osmactive)


osm_data = osmactive::get_travel_network(
    place = study_area,
    boundary = study_area,
    boundary_type = "clipsrc"
)



osm_drive = osmactive::get_driving_network(osm_data)

mapview::mapview(osm_drive, zcol = "maxspeed")

# Get Level of Service

cycle_net = osmactive::get_cycling_network(osm_data)
cycle_net = distance_to_road(cycle_net, osm_drive)
cycle_net = classify_cycle_infrastructure(cycle_net, include_mixed_traffic = TRUE)

cycle_net = estimate_traffic(cycle_net)
cycle_net$AADT = npt_to_cbd_aadt_numeric(cycle_net$assumed_volume)
cycle_net = level_of_service(cycle_net)
plot(cycle_net["Level of Service"])
mapview::mapview(cycle_net["Level of Service"])

names(cycle_net)
cycle_net_cols_to_keep = c(
  "osm_id",
  "name",
  "highway",
  "maxspeed",
  "bicycle",
  "cycleway_chars",
  "lanes",
  "segregated",
  "surface"
)
cycle_net = cycle_net |> 
  select(all_of(cycle_net_cols_to_keep))

# usethis::use_data(example_school_mcr, overwrite = TRUE)

# Create random home locations within the zonebuilder area
cycle_net_joined = sf::st_union(cycle_net)
points = sf::st_sample(cycle_net_joined, size = 1000)

centre = sf::st_centroid(zones)[1,]

tm_shape(cycle_net_joined) + tm_lines() +
  tm_shape(points) + tm_dots(size = 1) +
  tm_shape(centre) + tm_dots(size = 2)

# Generate routes to school from these points
# school_manc = osmextract::oe_get("College Road, Whalley Range, Manchester, M16 0AA")
# number of pupils = 430

school_name = "Manley Park Primary School"
xlfile = "data-raw/edubasealldata20250724.xlsx"
extract_students = function(school_name) {
  x = readxl::read_xlsx(xlfile)
  x = x |> filter(EstablishmentName == school_name)
  number_of_pupils = x$NumberOfPupils
}
n = extract_students(school_name)




