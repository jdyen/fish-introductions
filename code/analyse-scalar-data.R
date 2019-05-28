# we want to tell R which directory we"re working in
setwd("~/Dropbox/research/fish-introductions/")

# need a few packages to make this work
library(greta.fda)

# source some helper functions
source("code/helpers.R")

# load the data we need
source("code/load-data.R")

# mcmc settings
n_samples <- 20000
warmup <- 20000
thin <- 4
n_cv <- 10

# initialise some empty output objects
r2 <- list()
beta_estimates <- list()
r2cv <- list()

# fit simple models (richness)
mod <- fit_scalar_model(richness ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                          ph + nutrients + deadwood + rba +
                          pa_exo + pa_nattras +
                          abun_exo + abun_nattras +
                          rich_exo + rich_nattras +
                          (1 | basin),
                        data = data_set,
                        hmc_settings = list(n_samples = n_samples,
                                            warmup = warmup,
                                            thin = thin))

# save relevant outputs from full model
beta_estimates$richness_pa <- mod$beta
r2$richness_pa <- mod$r2

# validate!
mod_cv <- validate(mod, n_cv = n_cv, data = data_set)
r2cv$richness_pa <- mod_cv$r2

# fit simple models (abundance)
mod <- fit_scalar_model(abundance ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                          ph + nutrients + deadwood + rba +
                          pa_exo + pa_nattras +
                          abun_exo + abun_nattras +
                          rich_exo + rich_nattras +
                          (1 | basin),
                        data = data_set,
                        hmc_settings = list(n_samples = n_samples,
                                            warmup = warmup,
                                            thin = thin))

# save relevant outputs from full model
beta_estimates$abundance_pa <- mod$beta
r2$abundance_pa <- mod$r2

# validate!
mod_cv <- validate(mod, n_cv = n_cv, data = data_set)
r2cv$abundance_pa <- mod_cv$r2

# repeat with TL predictors (exotic)
mod <- fit_scalar_model(richness ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                          ph + nutrients + deadwood + rba +
                          wtl_exo + pa_exo + abun_exo + rich_exo +
                          (1 | basin),
                        data = data_exo_tl,
                        hmc_settings = list(n_samples = n_samples,
                                            warmup = warmup,
                                            thin = thin))

# save relevant outputs from full model
beta_estimates$richness_wtl_ex <- mod$beta
r2$richness_wtl_ex <- mod$r2

# validate!
mod_cv <- validate(mod, n_cv = n_cv, data = data_exo_tl)
r2cv$richness_wtl_ex <- mod_cv$r2

# and same for abundance
mod <- fit_scalar_model(abundance ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                          ph + nutrients + deadwood + rba +
                          wtl_exo + pa_exo + abun_exo + rich_exo +
                          (1 | basin),
                        data = data_exo_tl,
                        hmc_settings = list(n_samples = n_samples,
                                            warmup = warmup,
                                            thin = thin))

# save relevant outputs from full model
beta_estimates$abundance_wtl_ex <- mod$beta
r2$abundance_wtl_ex <- mod$r2

# validate!
mod_cv <- validate(mod, n_cv = n_cv, data = data_exo_tl)
r2cv$abundance_wtl_ex <- mod_cv$r2

# now with TL predictors for translocated natives
mod <- fit_scalar_model(richness ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                          ph + nutrients + deadwood + rba +
                          wtl_nattras + pa_nattras + abun_nattras + rich_nattras +
                          (1 | basin),
                        data = data_nattras_tl,
                        hmc_settings = list(n_samples = n_samples,
                                            warmup = warmup,
                                            thin = thin))

# save relevant outputs from full model
beta_estimates$richness_wtl_transloc <- mod$beta
r2$richness_wtl_transloc <- mod$r2

# validate!
mod_cv <- validate(mod, n_cv = n_cv, data = data_nattras_tl)
r2cv$richness_wtl_transloc <- mod_cv$r2

# and same for abundance
mod <- fit_scalar_model(abundance ~ elevation + water_temp + water_depth + water_vel + water_conduct +
                          ph + nutrients + deadwood + rba +
                          wtl_nattras + pa_nattras + abun_nattras + rich_nattras +
                          (1 | basin),
                        data = data_nattras_tl,
                        hmc_settings = list(n_samples = n_samples,
                                            warmup = warmup,
                                            thin = thin))

# save relevant outputs from full model
beta_estimates$abundance_wtl_transloc <- mod$beta
r2$abundance_wtl_transloc <- mod$r2

# validate!
mod_cv <- validate(mod, n_cv = n_cv, data = data_nattras_tl)
r2cv$abundance_wtl_transloc <- mod_cv$r2

# save outputs
saveRDS(list(beta = beta_estimates, r2 = r2, r2cv = r2cv), file = "outputs/scalar_fitted_cv.rds")
