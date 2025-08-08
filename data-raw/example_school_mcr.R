## code to prepare `example_school_mcr` dataset goes here

library(tidyverse)
library(osmactive)
library(tmap)
library(sf)


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

points = st_as_sf(points)
points = points |> 
  filter(!st_is_empty(points))

centre = sf::st_centroid(zones)[1,]

tm_shape(cycle_net_joined) + tm_lines() +
  tm_shape(points) + tm_dots(size = 1) +
  tm_shape(centre) + tm_dots(size = 2)

# Get number of students
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

# Create desire lines
od = points |>
  mutate(d = centre$geometry)

od$geometry =
  Map(st_union, od$x, od$d) |>
  st_as_sfc(crs = st_crs(od)) |>
  st_cast("LINESTRING")

od = od |>
  mutate(
    desire_line_length = units::drop_units(st_length(geometry))
  )
od = od |> 
  filter(desire_line_length < 5000)

od = od |> 
  mutate(trips = nrow(od)/n)

# Fix the geometry
od_standard = od |> 
  st_drop_geometry() |> 
  mutate(id = row_number()) |> 
  select(id, trips, desire_line_length, geometry)
od_standard = st_as_sf(od_standard)

# The desire lines still look strange!
tm_shape(od_standard) + tm_lines()


# Generate routes to school from these points
# Create routes
library(stplanr)
plan = "quietest"

routes_plan = stplanr::route(
  l = od_standard,
  route_fun = cyclestreets::journey,
  plan = plan
)
tm_shape(routes_plan) + tm_lines("trips")

routes_plan = routes_plan |>
  group_by(route_number) |>
  mutate(route_hilliness = weighted.mean(gradient_smooth, distances)) |>
  ungroup()
class(routes_plan$route_number) = "character"
class(routes_plan$length) = "numeric"
class(routes_plan$quietness) = "numeric"
assign(paste0("routes_", plan, "_all"), routes_plan)
routes_plan = routes_plan |>
  filter(length < 5000)
assign(x = paste0("routes_", plan), value = routes_plan)
saveRDS(routes_plan, paste0("./data/routes-", plan, "-manchester.Rds"))

cycle_bus_routes(routes = routes_plan)

routes_plan_all = get(paste0("routes_", plan, "_all"))
route_summaries = routes_plan_all |>
  group_by(route_number) |>
  summarise(
    trips = mean(trips),
    length = mean(length),
    desire_line_length = mean(desire_line_length)
  )
assign(paste0("route_summaries_all_", plan), route_summaries)

# Rnet and PCT uptake
library(pct)
routes_plan = get(x = paste0("routes_", plan))
routes_plan_pct = routes_plan |>
  group_by(route_number) |>
  mutate(
    pcycle_godutch = pct::uptake_pct_godutch_school2(
      case_when(length > 30000 ~ 30000, TRUE ~ length),
      route_hilliness
    ),
    bicycle_godutch = pcycle_godutch * trips
  )
assign(paste0("routes_", plan, "_pct"), routes_plan_pct)
rnet_plan_raw = routes_plan_pct |>
  overline(
    attrib = c(
      "trips",
      "bicycle_godutch",
      "quietness",
      "gradient_smooth"
    ),
    fun = list(sum = sum, mean = mean)
  )
rnet_plan = rnet_plan_raw |>
  transmute(
    trips = trips_sum,
    bicycle_godutch = bicycle_godutch_sum,
    quietness = round(quietness_mean),
    gradient = round(gradient_smooth_mean * 100)
  )
assign(x = paste0("rnet_", plan), value = rnet_plan)

tm_shape(rnet_quietest) +
  tm_lines(
    "bicycle_godutch",
    palette = "viridis",
    lwd = 2,
    breaks = c(0, 5, 10, 100)
  ) +
  tm_shape(centroids_5km) +
  tm_bubbles("trips") +
  tm_shape(school) +
  tm_bubbles(col = "green")

quietness_breaks = c(0, 25, 50, 75, 100)
pal = c('#882255', '#CC6677', '#44AA99', '#117733')

routes_plan_pct = get(paste0("routes_", plan, "_pct"))
route_summaries = routes_plan_pct |>
  group_by(id, route_number, trips, bicycle_godutch, length) |>
  summarise() |>
  ungroup()
assign(paste0("route_summaries_", plan), route_summaries)

quiet_join = route_summaries_quietest |>
  sf::st_drop_geometry() |>
  select(id, route_number, bicycle_godutch)



