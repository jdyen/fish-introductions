# analyses of four species to assess intraspecific shifts in size distributions
#    following introductions of exotic or native translocated species
# SATR Salmo trutta
# LUGR Luciobarbus graellsii
# BAME Barbus meridionalis
# PAMY Parachondrostoma miegii

# need a few packages to make this work
library(greta.fda)
library(greta)
library(qs)

# source some helper functions
source("code/helpers.R")

# load the data we need
source("code/load-species-data.R")

# settings
n_samples <- 10000
warmup <- 10000
thin <- 8

# fit models for each species
mod_full <- mod_exotic <- mod_transloc <- list()
for (i in seq_along(data_set)) {
  
  # fit a full model
  mod_full[[i]] <- fit_functional_model(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                                          ph + nutrients + deadwood + rba +
                                          pa_exo + pa_nattras + abun_exo + abun_nattras +
                                          (1 | basin) + (1 | year),
                                        data = data_set[[i]],
                                        hmc_settings = list(n_samples = n_samples,
                                                            warmup = warmup,
                                                            thin = thin))
  
  # fit a model with exotic species trophic levels (restricted to sites with observed exotic species)
  mod_exotic[[i]] <- fit_functional_model(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                                            ph + nutrients + deadwood + rba +
                                            wtl_exo + pa_exo + abun_exo +
                                            (1 | basin) + (1 | year),
                                          data = data_exo_tl[[i]],
                                          hmc_settings = list(n_samples = n_samples,
                                                              warmup = warmup,
                                                              thin = thin))
  
  # fit a model with translocated species trophic levels (restricted to sites with observed translocated species)
  mod_transloc[[i]] <- fit_functional_model(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                                              ph + nutrients + deadwood + rba +
                                              wtl_nattras + pa_nattras + abun_nattras +
                                              (1 | basin) + (1 | year),
                                            data = data_nattras_tl[[i]],
                                            hmc_settings = list(n_samples = n_samples,
                                                                warmup = warmup,
                                                                thin = thin))
  
  # save outputs
  qs::qsave(mod_full, file = "outputs/species_fitted_full.qs")
  qs::qsave(mod_exotic, file = "outputs/species_fitted_exotic.qs")
  qs::qsave(mod_transloc, file = "outputs/species_fitted_transloc.qs")
  
}
