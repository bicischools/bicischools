library(stplanr)
library(tidyverse)
library(tmap)
tmap_mode("view")

res_constrained = readRDS("data/res_constrained.Rds")
schools_c1 = readRDS("data/lisbon_c1_public.Rds")

summary(res_constrained$trips)
larger_flows = res_constrained |> 
  filter(trips > 1) |> 
  select(-geom, -interaction)
dim(larger_flows)
# [1] 4883   9

fast_routes = route(l = larger_flows, route_fun = cyclestreets::journey, plan = "fastest")
tm_shape(fast_routes) + tm_lines("trips", palette = "viridis") +
  tm_shape(schools_c1) + tm_dots()

saveRDS(fast_routes, "data/routes-fast.Rds")

quiet_routes = route(l = larger_flows, route_fun = cyclestreets::journey, plan = "quietest")
tm_shape(quiet_routes) + tm_lines("trips", palette = "viridis") +
  tm_shape(schools_c1) + tm_dots()

saveRDS(quiet_routes, "data/routes-quiet.Rds")

# How to change the network to exclude forest paths?
# osmextract::oe_get()

tm_shape(quiet_routes) + tm_lines(lwd = "trips", col = "quietness", palette = "viridis") +
  tm_shape(schools_c1) + tm_dots()
