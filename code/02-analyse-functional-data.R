# need a few packages to make this work
library(greta.fda)
library(greta)
library(qs)

# source some helper functions
source("code/helpers.R")

# load the data we need
source("code/load-data.R")

# settings
n_samples <- 10000
warmup <- 10000
thin <- 8
n_cv <- 10

# initialise some empty output objects
r2 <- list()
r2cv <- list()
beta_estimates <- list()

# calculate VIFs for all predictors
mod_vif <- lme4::lmer(
  richness ~ elevation + water_temp + water_depth + water_vel + water_conduct +
    ph + nutrients + deadwood + rba +
    pa_exo + pa_nattras + abun_exo + abun_nattras +
    (1 | basin) + (1 | year),
  data = data_set
)
vif_est <- car::vif(mod_vif)
write.csv(vif_est, file = "outputs/vif-calc.csv")

# fit a full model
mod_full <- fit_functional_model(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                                   ph + nutrients + deadwood + rba +
                                   pa_exo + pa_nattras + abun_exo + abun_nattras +
                                   (1 | basin) + (1 | year),
                                 data = data_set,
                                 hmc_settings = list(n_samples = n_samples,
                                                     warmup = warmup,
                                                     thin = thin))

# save outputs to a new object for plotting
r2$full <- mod_full$r2
beta_estimates$full <- mod_full$beta

# perform hierarchical partitioning (fit all combos of models)
form_list <- list(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba +
                    pa_exo + pa_nattras + abun_exo + abun_nattras + (1 | basin) + (1 | year),
                  isd ~ pa_nattras + abun_nattras + (1 | basin) + (1 | year),
                  isd ~ pa_exo + abun_exo + (1 | basin) + (1 | year),
                  isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba + (1 | basin) + (1 | year),
                  isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba +
                    pa_nattras + abun_nattras + (1 | basin) + (1 | year),
                  isd ~ pa_exo + pa_nattras + abun_exo + abun_nattras + (1 | basin) + (1 | year),
                  isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba +
                    pa_exo + abun_exo + (1 | basin) + (1 | year),
                  isd ~ (1 | basin) + (1 | year))
for (i in seq_along(form_list)) {
  mod_tmp <- fit_functional_model(form_list[[i]],
                                  data = data_set,
                                  hmc_settings = list(n_samples = n_samples,
                                                      warmup = warmup,
                                                      thin = thin))
  r2[length(r2) + 1] <- mod_tmp$r2
  names(r2)[length(r2)] <- paste0("hp_full", i)
}

# cross validate the full model
mod_full_cv <- validate(mod_full, n_cv = n_cv, data = data_set)

# store CV outputs
r2cv$full <- mod_full_cv$r2

# fit a model with exotic species trophic levels (restricted to sites with observed exotic species)
mod_exotic <- fit_functional_model(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                                     ph + nutrients + deadwood + rba +
                                     wtl_exo + pa_exo + abun_exo +
                                     (1 | basin) + (1 | year),
                                   data = data_exo_tl,
                                   hmc_settings = list(n_samples = n_samples,
                                                       warmup = warmup,
                                                       thin = thin))

# save outputs to a new object for plotting
r2$exotic <- mod_exotic$r2
beta_estimates$exotic <- mod_exotic$beta

# perform hierarchical partitioning (fit all combos of models)
form_list <- list(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba +
                    wtl_exo + pa_exo + abun_exo + (1 | basin) + (1 | year),
                  isd ~ wtl_exo + pa_exo + abun_exo + (1 | basin) + (1 | year),
                  isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba + (1 | basin) + (1 | year),
                  isd ~ (1 | basin) + (1 | year))
for (i in seq_along(form_list)) {
  mod_tmp <- fit_functional_model(form_list[[i]],
                                  data = data_exo_tl,
                                  hmc_settings = list(n_samples = n_samples,
                                                      warmup = warmup,
                                                      thin = thin))
  r2[length(r2) + 1] <- mod_tmp$r2
  names(r2)[length(r2)] <- paste0("hp_exotic", i)
}

# validate!
mod_exotic_cv <- validate(mod_exotic, n_cv = n_cv, data = data_exo_tl)

# store CV outputs
r2cv$exotic <- mod_exotic_cv$r2

# fit a model with translocated species trophic levels (restricted to sites with observed translocated species)
mod_transloc <- fit_functional_model(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                                       ph + nutrients + deadwood + rba +
                                       wtl_nattras + pa_nattras + abun_nattras +
                                       (1 | basin) + (1 | year),
                                     data = data_nattras_tl,
                                     hmc_settings = list(n_samples = n_samples,
                                                         warmup = warmup,
                                                         thin = thin))

# save outputs to a new object for plotting
r2$translocated <- mod_transloc$r2
beta_estimates$translocated <- mod_transloc$beta

# perform hierarchical partitioning (fit all combos of models)
form_list <- list(isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba +
                    wtl_nattras + pa_nattras + abun_nattras + (1 | basin) + (1 | year),
                  isd ~ wtl_nattras + pa_nattras + abun_nattras + (1 | basin) + (1 | year),
                  isd ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                    ph + nutrients + deadwood + rba + (1 | basin) + (1 | year),
                  isd ~ (1 | basin) + (1 | year))
for (i in seq_along(form_list)) {
  mod_tmp <- fit_functional_model(form_list[[i]],
                                  data = data_nattras_tl,
                                  hmc_settings = list(n_samples = n_samples,
                                                      warmup = warmup,
                                                      thin = thin))
  r2[length(r2) + 1] <- mod_tmp$r2
  names(r2)[length(r2)] <- paste0("hp_nattras", i)
}

# validate!
mod_transloc_cv <- validate(mod_transloc, n_cv = n_cv, data = data_nattras_tl)

# store CV outputs
r2cv$translocated <- mod_transloc_cv$r2

# combine and save all outputs
saveRDS(list(r2 = r2, r2cv = r2cv, beta = beta_estimates), file = "outputs/hp_functional_fitted_cv.rds")
