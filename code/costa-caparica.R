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
# [1] 

# median desire line length for all students living within 5km euclidean distance of the school
library(matrixStats)
weightedMedian(res_cc$desire_line_length, res_cc$trips_modelled)
# [1] 564.7516

# trips_modelled within 5km euclidean distance
under_5 = route_summaries_all_quiet |> 
  filter(desire_line_length < 5000)
sum(under_5$trips_modelled)
# [1] 238

# Create route network