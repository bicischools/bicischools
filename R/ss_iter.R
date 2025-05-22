si_init <- results


si_constrain <- function(si_init,
                         max_iters = 25,
                         max_error = 0.1,
                         result_col = "modelled_trips",
                         origin_col = names(si_init)[grep("origin",names(si_init))], 
                         destination_col = names(si_init)[grep("destination",names(si_init))],
                         zero_seed = 1e-8){
  
  # Seeding zeros 
  si_init[[result_col]][si_init[[result_col]] == 0] <- zero_seed
  si_init[[origin_col]][si_init[[origin_col]] == 0] <- zero_seed
  si_init[[destination_col]][si_init[[destination_col]] == 0] <- zero_seed
  
  
  
  origin_col <- rlang::data_sym(origin_col)
  destination_col <- rlang::data_sym(destination_col)
  result_col_1 <- rlang::data_sym(result_col)
  
  
  
  si_init_non_geom <- si_init |> st_drop_geometry()
  
  
  iters <- list()
  iters[[1]] <- si_init_non_geom
  # for(i in 1:max_iters){
  for(i in 1:1){
    iters[[i+1]] <- iters[[i]] |>
      group_by(O) |> 
      mutate(
        modelled_O = sum(!!result_col_1, na.rm = T),
        o_adj_factor = !!origin_col / modelled_O,
        !!result_col := !!origin_col / o_adj_factor
        ) |>
      group_by(D) |>
      mutate(
        modelled_D = sum(!!result_col_1),
        d_adj_factor = !!destination_col / modelled_D,
        !!result_col := !!destination_col * d_adj_factor
      ) |> 
      ungroup()
          
  }
  
  )
  
  
}

iters[[2]] |> 
  ggplot(aes(d_adj_factor))+
  geom_histogram()+
  scale_x_log10()


iters[[2]] |> select(D,destination_n_pupils) |> unique() |>  pull(destination_n_pupils) |> sum()


res_doubly_constrained_8 = res_doubly_constrained_7 
res_doubly_constrained_9 = res_doubly_constrained_8 |>
  group_by(D) |>
  mutate(
    observed_group = first(destination_n_pupils),
    modelled_group = sum(interaction),
    modelled_overestimate_factor = modelled_group / observed_group,
    interaction = interaction / modelled_overestimate_factor
  )

si_constrain(results)
