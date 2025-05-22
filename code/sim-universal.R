remotes::install_cran("simodels")
library(tidyverse)
library(patchwork)

bgri = sf::read_sf("../internal/BGRI21_170/BGRI21_170.gpkg")
bgri_4326 = sf::st_transform(bgri, 4326)
bgri_4326 = sf::st_make_valid(bgri_4326)

# Run for each municipality
bgri_4326 = bgri_4326 |>
  filter(DTMN21 == 1503)
schools_c1 = readRDS("data/c1.Rds")
schools_c1 = schools_c1 |>
  filter(MUNICIPIO == "Almada")

schools_c1 = schools_c1 |>
  mutate(
    n_pupils = STUDENTS
  ) |>
  select(DGEEC_id, n_pupils)

home_zones = bgri_4326 |>
  select(OBJECTID, N_INDIVIDUOS_0_14)

g1 = home_zones |>
  ggplot() +
  geom_sf() +
  labs(title = "Zones")
g2 = schools_c1 |>
  ggplot() +
  geom_sf() +
  labs(title = "Destinations (Schools)")
g1 + g2

zone_overestimate_factor =
  (sum(home_zones$N_INDIVIDUOS_0_14) /
    sum(schools_c1$n_pupils))
zone_overestimate_factor


home_zones = home_zones |>
  dplyr::mutate(
    pupils_estimated = N_INDIVIDUOS_0_14 / zone_overestimate_factor
  )

sum(home_zones$pupils_estimated)
sum(schools_c1$n_pupils)

max_dist = 3000 # meters
od_from_si = simodels::si_to_od(home_zones, schools_c1, max_dist = max_dist)

m1 = od_from_si |>
  ggplot() +
  geom_sf(alpha = 0.1)
# m1 # This figure is too computationally intensive

# using testing OD dataset based on the case study school, with destination code matching the one for Adriano Correia school in `schools_geo`
case_study_school = readRDS("data/zone_counts.Rds")
od_observed = case_study_school |>
  sf::st_drop_geometry() |>
  mutate(D = 1106908) |>
  select(OBJECTID, D, n_students)
names(od_observed)[1:3] = c("O", "D", "frequency")
# join od_observed to od_from_si:
od_from_si = left_join(
  od_from_si,
  od_observed
)

names(od_from_si)

gravity_model = function(beta, d, m, n) {
  m * n * exp(-beta * d / 1000)
}
# perform SIM
od_res = simodels::si_calculate(
  od_from_si,
  fun = gravity_model,
  d = distance_euclidean,
  m = origin_pupils_estimated,
  n = destination_n_pupils,
  #   constraint_production = origin_all,
  beta = 0.8
)

interaction_overestimate_factor = sum(schools_c1$n_pupils) /
  sum(od_res$interaction)
od_res = od_res |>
  dplyr::mutate(
    interaction = interaction * interaction_overestimate_factor
  )

res_o = od_res |>
  group_by(O) |>
  summarise(
    Observed = first(origin_pupils_estimated),
    Modelled = sum(interaction),
    Type = "Origin"
  )
res_d = od_res |>
  group_by(D) |>
  summarise(
    Observed = first(destination_n_pupils),
    Modelled = sum(interaction),
    Type = "Destination"
  )

res_combined = bind_rows(res_o, res_d) |>
  # Create ordered factor with types:
  mutate(
    Type = factor(Type, levels = c("Origin", "Destination"))
  )
g_combined = res_combined |>
  ggplot() +
  geom_point(aes(x = Observed, y = Modelled)) +
  geom_smooth(aes(x = Observed, y = Modelled), method = "lm") +
  facet_wrap(~Type, scales = "free") +
  labs(
    title = "Model fit at origin and destination levels (unconstrained)",
    x = "Observed",
    y = "Modelled"
  )
g_combined

# Aim: create function that takes in od_res and returns a ggplot object
plot_od_fit = function(od_res, title = "(unconstrained)") {
  res_o = od_res |>
    group_by(O) |>
    summarise(
      Observed = first(origin_pupils_estimated),
      Modelled = sum(interaction),
      Type = "Origin"
    )
  res_d = od_res |>
    group_by(D) |>
    summarise(
      Observed = first(destination_n_pupils),
      Modelled = sum(interaction),
      Type = "Destination"
    )
  res_combined = bind_rows(res_o, res_d) |>
    # Create ordered factor with types:
    mutate(
      Type = factor(Type, levels = c("Origin", "Destination"))
    )
  rsq_o = cor(res_o$Observed, res_o$Modelled, use = "complete.obs")^2
  rsq_d = cor(res_d$Observed, res_d$Modelled, use = "complete.obs")^2
  rsq_summary = data.frame(
    rsq = c(rsq_o, rsq_d) |> round(3),
    Type = c("Origin", "Destination")
  )
  g_combined = res_combined |>
    left_join(rsq_summary) |>
    # Add rsquared info:
    mutate(Type = paste0(Type, " (R-squared: ", rsq, ")")) |>
    # Update factor so it's ordered (reverse order):
    mutate(
      Type = factor(Type, levels = unique(Type))
    ) |>
    ggplot() +
    geom_point(aes(x = Observed, y = Modelled)) +
    geom_smooth(aes(x = Observed, y = Modelled), method = "lm") +
    facet_wrap(~Type, scales = "free") +
    labs(
      title = paste0("Model fit at origin, destination and OD levels ", title),
      x = "Observed",
      y = "Modelled"
    )
  g_combined
}
# Test it:
g = plot_od_fit(od_res)


objective_function = function(beta) {
  od_res = simodels::si_calculate(
    od_from_si,
    fun = gravity_model,
    d = distance_euclidean,
    m = origin_pupils_estimated,
    n = destination_n_pupils,
    beta = beta
  )
  interaction_overestimate_factor = sum(schools_c1$n_pupils) /
    sum(od_res$interaction)
  od_res = od_res |>
    dplyr::mutate(
      trips = interaction * interaction_overestimate_factor
    )
  sum((od_res$trips - od_res$frequency)^2, na.rm = TRUE)
}
# Try it with beta of 0.8:
objective_function(0.8)

# Optimise it:
beta_opt = optimise(objective_function, c(0.1, 1))
beta_new = beta_opt$minimum
beta_new


res_optimised = simodels::si_calculate(
  od_from_si,
  fun = gravity_model,
  d = distance_euclidean,
  m = origin_pupils_estimated,
  n = destination_n_pupils,
  beta = beta_new
)
# cor(res_optimised$frequency, res_optimised$interaction, use = "complete.obs")^2

res_constrained = simodels::si_calculate(
  od_from_si,
  fun = gravity_model,
  d = distance_euclidean,
  m = origin_pupils_estimated,
  n = destination_n_pupils,
  constraint_production = origin_pupils_estimated,
  beta = beta_new
)
# We need to remove the rows within 0 origin_pupils_estimated to avoid NaN errors
res_constrained = res_constrained |>
  filter(origin_pupils_estimated > 0)
# res_constrained = res_constrained |>
#   mutate(interaction = tidyr::replace_na(interaction, 0))

# Aim: check totals per origin
sum(res_constrained$interaction) == sum(schools_c1$n_pupils)
res_constrained = res_constrained |>
  mutate(trips = interaction)
res_constrained_o = res_constrained |>
  sf::st_drop_geometry() |>
  group_by(O) |>
  summarise(
    trips = sum(interaction)
  )
res_constrained_o$trips |> sum()

## Doubly-constrained model

res_doubly_constrained = res_constrained |>
  group_by(D) |>
  mutate(
    observed_group = first(destination_n_pupils),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )
# summary(res_doubly_constrained)
sum(res_doubly_constrained$interaction) == sum(res_constrained$interaction)

res_doubly_constrained_2 = res_doubly_constrained |>
  group_by(O) |>
  mutate(
    observed_group = first(origin_pupils_estimated),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )

res_doubly_constrained_3 = res_doubly_constrained_2 |>
  group_by(D) |>
  mutate(
    observed_group = first(destination_n_pupils),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )

res_doubly_constrained_4 = res_doubly_constrained_3 |>
  group_by(O) |>
  mutate(
    observed_group = first(origin_pupils_estimated),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )
res_doubly_constrained_5 = res_doubly_constrained_4 |>
  group_by(D) |>
  mutate(
    observed_group = first(destination_n_pupils),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )

res_doubly_constrained_6 = res_doubly_constrained_5 |>
  group_by(O) |>
  mutate(
    observed_group = first(origin_pupils_estimated),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )
res_doubly_constrained_7 = res_doubly_constrained_6 |>
  group_by(D) |>
  mutate(
    observed_group = first(destination_n_pupils),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )

res_doubly_constrained_8 = res_doubly_constrained_7 |>
  group_by(O) |>
  mutate(
    observed_group = first(origin_pupils_estimated),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )
res_doubly_constrained_9 = res_doubly_constrained_8 |>
  group_by(D) |>
  mutate(
    observed_group = first(destination_n_pupils),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )

names(res_doubly_constrained_9)

res_output = res_doubly_constrained_9 |>
  select(O, D, trips_modelled = interaction)
summary(res_output)

sf::write_sf(res_output, "res_output_almada.geojson", delete_dsn = TRUE)
res_output |>
  sf::st_drop_geometry() |>
  write_csv("res_output_almada.csv")

system("gh release list")
system("gh release create v0.1.0 res_output.geojson res_output.csv")
