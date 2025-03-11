library(sf)
library(tmap)
tmap_mode("view")
library(dplyr)
library(stplanr)

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

res_cc = res_cc |> 
  mutate(
    desire_line_length = st_length(res_cc),
    desire_line_length = units::drop_units(desire_line_length)
  )

# Convert desire lines to routes
plan = "quiet"
plans = c("quiet", "fast")

for(plan in plans) {
  location = paste0("../internal/routes-", plan, "-costa-da-caparica.Rds")
  routes_plan_location = location
  if (file.exists(routes_plan_location)) {
    routes_plan = readRDS(routes_plan_location)
  } else {
    plan_name = paste0(plan, "est")
    routes_plan = route(l = res_cc, route_fun = cyclestreets::journey, plan = plan_name)
    routes_plan = routes_plan |> 
      group_by(route_number) |> 
      mutate(route_hilliness = weighted.mean(gradient_smooth, distances)) |> 
      ungroup()
    saveRDS(routes_plan, paste0("../internal/routes-", plan, "-costa-da-caparica.Rds"))
  }
  class(routes_plan$route_number) = "character"
  class(routes_plan$length) = "numeric"
  class(routes_plan$quietness) = "numeric"
  assign(paste0("routes_", plan, "_all"), routes_plan)
  routes_plan = routes_plan |> 
    filter(length < 5000) 
  assign(x = paste0("routes_", plan), value = routes_plan)
}

for(plan in plans) {
  routes_plan_all = get(paste0("routes_", plan, "_all"))
  route_summaries = routes_plan_all |> 
    group_by(route_number) |> 
    summarise(trips_modelled = mean(trips_modelled),
              length = mean(length),
              desire_line_length = mean(desire_line_length)
    )
  assign(paste0("route_summaries_all_", plan), route_summaries)
}

# median route length for all students living within 5km euclidean distance of the school
library(matrixStats)
weightedMedian(route_summaries_all_quiet$length, route_summaries_all_quiet$trips_modelled)
# [1] 814.0006
weightedMedian(route_summaries_all_fast$length, route_summaries_all_fast$trips_modelled)
# [1] 785.9941

# median desire line length for all students living within 5km euclidean distance of the school
library(matrixStats)
weightedMedian(res_cc$desire_line_length, res_cc$trips_modelled)
# [1] 564.7516

# trips_modelled within 5km euclidean distance
under_5 = route_summaries_all_quiet |> 
  filter(desire_line_length < 5000)
sum(under_5$trips_modelled)
# [1] 238

# PCT cycle uptake --------------------------------------------------------

library(pct)

for(plan in plans) {
  routes_plan = get(x = paste0("routes_", plan))
  routes_plan_pct = routes_plan |> 
    group_by(route_number) |> 
    mutate(
      pcycle_godutch = pct::uptake_pct_godutch_school2(
        case_when(length > 30000 ~ 30000, TRUE ~ length),
        route_hilliness),
      bicycle_godutch = pcycle_godutch * trips_modelled
    )
  assign(paste0("routes_", plan, "_pct"), routes_plan_pct)
  rnet_plan_raw = routes_plan_pct |> 
    overline(attrib = c("trips_modelled", "bicycle_godutch", "quietness", "gradient_smooth"), 
             fun = list(sum = sum, mean = mean))
  rnet_plan = rnet_plan_raw |> 
    transmute(trips_modelled = trips_modelled_sum, 
              bicycle_godutch = bicycle_godutch_sum,
              quietness = round(quietness_mean),
              gradient = round(gradient_smooth_mean*100))
  assign(x = paste0("rnet_", plan), value = rnet_plan)
}

tm_shape(rnet_quiet) +
  tm_lines("bicycle_godutch", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100)) 
tm_shape(rnet_fast) +
  tm_lines("bicycle_godutch", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100))

m2 = tm_shape(rnet_quiet |> rename(`Potential cyclists` = bicycle_godutch)) +
  tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100))
m6 = tm_shape(rnet_fast |> rename(`Potential cyclists` = bicycle_godutch)) +
  tm_lines("Potential cyclists", palette = "viridis", lwd = 2, breaks = c(0, 5, 10, 100))

quietness_breaks = c(0, 25, 50, 75, 100)
pal = c('#882255','#CC6677', '#44AA99', '#117733')

m9 = tm_shape(rnet_quiet |> rename(`Cycle friendliness` = quietness)) +
  tm_lines("Cycle friendliness", palette = pal, lwd = 3, breaks = quietness_breaks)
m10 = tm_shape(rnet_fast |> rename(`Cycle friendliness` = quietness)) +
  tm_lines("Cycle friendliness", palette = pal, lwd = 3, breaks = quietness_breaks)

# Explore results ---------------------------------------------------------

summary(routes_quiet$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 144    1890    3024    2909    3856    4967 
summary(routes_fast$length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 144    1496    2525    2680    3637    4918 


# Total number of students cycling to school under Go Dutch
for(plan in plans) {
  routes_plan_pct = get(paste0("routes_", plan, "_pct"))
  route_summaries = routes_plan_pct |> 
    group_by(OBJECTID,
             route_number, 
             trips_modelled, 
             bicycle_godutch, 
             length
    ) |> 
    summarise() |> 
    ungroup()
  assign(paste0("route_summaries_", plan), route_summaries)
}

quiet_join = route_summaries_quiet |> 
  sf::st_drop_geometry() |> 
  select(OBJECTID, route_number, bicycle_godutch)
centroids_quiet_5km = inner_join(centroids_5km, quiet_join, by = "OBJECTID")

fast_join = route_summaries_fast |> 
  sf::st_drop_geometry() |> 
  select(OBJECTID, route_number, bicycle_godutch)
centroids_fast_5km = inner_join(centroids_5km, fast_join, by = "OBJECTID")


m0 = tm_shape(zone_centroids |> rename(`Number of students` = trips_modelled)) + tm_bubbles("Number of students", alpha = 0.3) +
  tm_shape(school) + tm_bubbles(col = "green")
m1 = tm_shape(centroids_quiet_5km |> rename(`'Go Dutch' cycling potential` = bicycle_godutch)) + tm_bubbles("'Go Dutch' cycling potential", alpha = 0.3) + 
  tm_shape(school) + tm_bubbles(col = "green") +
  tm_shape(quiet_few) + tm_lines(lwd = 2)
m5 = tm_shape(centroids_fast_5km |> rename(`'Go Dutch' cycling potential` = bicycle_godutch)) + tm_bubbles("'Go Dutch' cycling potential", alpha = 0.3) + 
  tm_shape(school) + tm_bubbles(col = "green") +
  tm_shape(fast_few) + tm_lines(lwd = 2)

# n students within 5km route distance of school
sum(route_summaries_quiet$trips_modelled)
# [1] 140
sum(route_summaries_fast$trips_modelled)
# [1] 148

# n_cyclists under go dutch
sum(route_summaries_quiet$bicycle_godutch)
# [1] 40.38902
sum(route_summaries_fast$bicycle_godutch)
# [1] 40.47256

