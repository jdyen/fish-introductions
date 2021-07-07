# calculate deviance r2 for Poisson RVs
r2_deviance <- function(observed, fitted) {
  
  # calculate the first part of the likelihood, replacing 0s with a value of 1
  int_term <- c(observed) / c(fitted)
  int_term <- ifelse(int_term == 0, 1, int_term)
  
  # repeat for the null model
  mean_obs <- mean(c(observed))
  int_term2 <- c(observed) / mean_obs
  int_term2 <- ifelse(int_term2 == 0, 1, int_term2)
  
  # calculate full deviance
  dev_full <- 2 * sum(c(observed) * log(int_term) - 
                        (c(observed) - c(fitted)))
  
  # calculate null deviance
  dev_null <- 2 * sum(c(observed) * log(int_term2) - 
                        (c(observed) - mean_obs))

  # return deviance r2  
  1 - dev_full / dev_null
   
}

# calculate regression parameters for functional models
get_fda_betas <- function(x, basis, npred, summarise = TRUE) {
  
  # create empty output object
  out <- vector("list", length = npred + 1)
  
  # average samples over all chains
  x_ave <- do.call(abind, list(x, along = 3))
  x_ave <- apply(x_ave, c(1, 2), mean)
  x_ave <- x_ave[, grep("mu\\[", colnames(x_ave), invert = TRUE)]
  
  # pull out alpha and beta params
  int_draws <- x_ave[, grep("alpha\\[", colnames(x_ave))]
  slope_draws <- x_ave[, grep("beta\\[", colnames(x_ave))]
  
  # convert from basis coefs to full curves
  out[[1]] <- int_draws %*% as.matrix(basis)
  for (i in seq_len(length(out) - 1))
    out[[i + 1]] <- slope_draws[, grep(paste0("\\[", i, ","), colnames(slope_draws))] %*% as.matrix(basis)

  if (summarise)
    out <- lapply(out, function(x) apply(x, 2, quantile, p = c(0.025, 0.1, 0.5, 0.9, 0.975)))
  
  # return outputs    
  out
  
}

# function to fit a functional regression model
fit_functional_model <- function(formula, data,
                                 hmc_settings = list(),
                                 spline_settings = list(),
                                 priors = list()) {
  
  # unpack hmc settings
  hmc_set <- list(n_samples = 1000,
                  warmup = 1000,
                  thin = 1)
  hmc_set[names(hmc_settings)] <- hmc_settings
  
  # unpack spline settings
  spline_set <- list(df = 8, degree = 3)
  spline_set[names(spline_settings)] <- spline_settings

  # unpack priors
  prior_set <- list(alpha_sd = 2, beta_sd = 2, sigma_sd = 1)  
  prior_set[names(priors)] <- priors
  
  # let's prepare a function regression model by creating an fda_response object
  fda_response <- fda_response(formula,
                               data = data,
                               spline_settings = spline_set,
                               priors = prior_set)
  
  # extract fda_response components
  mu <- fda_response$mu
  alpha <- fda_response$alpha
  beta <- fda_response$beta
  gamma <- fda_response$gamma
  sigma_main <- fda_response$sigma_main
  sigma_bins <- fda_response$sigma_bins
  sigma_gamma <- fda_response$sigma_gamma
  
  # set likelihoods
  distribution(data$isd) <- poisson(exp(fda_response$mu))
  
  # define greta model
  mod <- model(mu, alpha, beta, sigma_main, sigma_bins, sigma_gamma)
  
  # sample from model
  samples <- mcmc(mod, n_samples = hmc_set$n_samples, warmup = hmc_set$warmup, thin = hmc_set$thin)
  
  # pull out estimates of full parameters
  beta_estimates <- get_fda_betas(samples,
                                  fda_response$spline_basis,
                                  nrow(fda_response$beta))
  
  # summarise fitted model
  mod_summary <- summary(samples)
  
  # remove samples to save memory
  rm(samples)
  
  # extract mean fitted values
  fitted_mean <- exp(matrix(mod_summary$statistics[grep("mu\\[", rownames(mod_summary$statistics)), "Mean"], ncol = ncol(data$isd)))
  
  # r2 calculation
  r2 <- r2_deviance(data$isd, fitted_mean)
  
  # compile outputs into a list
  out <- list(r2 = r2, beta = beta_estimates, fda_response = fda_response, hmc_settings = hmc_set)
  
  # set a class so we can have default functions on top of this
  class(out) <- c("fda_fitted", class(out))  
  
  # return outputs
  out
  
}

# function to fit a scalar regression model
fit_scalar_model <- function(formula,
                             data,
                             hmc_settings = list(),
                             priors = list()) {
  
  # unpack data
  resp <- all.vars(formula)[1]
  y <- get(resp, envir = as.environment(data), inherits = TRUE)
  formula_tmp <- update.formula(formula, isd ~ .)
  fda_tmp <- fda_response(formula_tmp, data = data)
  x <- as.matrix(fda_tmp$data$x)
  x <- cbind(rep(1, nrow(x)), x)
  z <- as.matrix(fda_tmp$data$z)

  # unpack hmc settings
  hmc_set <- list(n_samples = 1000,
                  warmup = 1000,
                  thin = 1)
  hmc_set[names(hmc_settings)] <- hmc_settings
  
  # unpack priors
  prior_set <- list(beta_sd = 2, sigma_sd = 0.5)  
  prior_set[names(priors)] <- priors
  
  beta <- normal(0, prior_set$beta_sd, dim = ncol(x))
  if (!is.null(z)) {
    nz <- ncol(z)
    sigma_gamma <- normal(0, prior_set$sigma_sd, dim = nz, truncation = c(0, Inf))
    gamma <- vector("list", length = nz)
    for (i in seq_len(nz)) {
      gamma[[i]] <- normal(0, sigma_gamma[i], dim = max(z[, i]))
      mu <- x %*% beta + gamma[[i]][z[, i]]
    }
  } else {
    mu <- x %*% beta
  }
  distribution(y) <- poisson(exp(mu))
  mod <- model(mu, beta)
  draws <- mcmc(mod,
                n_samples = hmc_set$n_samples,
                warmup = hmc_set$warmup,
                thin = hmc_set$thin)
  
  # summarise the fitted model, saving relevant outputs
  mod_summary <- summary(draws, quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975))
  fitted <- mod_summary$statistics[grep("mu", rownames(mod_summary$statistics)), "Mean"]
  
  # r2 calculation
  r2 <- r2_deviance(y, exp(fitted))
  
  # pull out estimates of full parameters
  beta_estimates <- mod_summary$quantiles[grep("beta", rownames(mod_summary$quantiles)), ]

  # work out variable names for fixed effects
  all_var_names <- attributes(terms(formula))$term.labels
  fixed <- grep("\\|", all_var_names, invert = TRUE)
  if (length(fixed) == 0)
    beta_estimates <- matrix(beta_estimates, nrow = 1)
  rownames(beta_estimates) <- c("intercept", all_var_names[fixed])

  # collate data to return
  if (!is.null(z)) {
    data_out <- list(y = y, x = x, z = as.matrix(z))
  } else {
    data_out <- list(y = y, x = x)
  }
  
  # compile outputs into a list
  out <- list(r2 = r2,
              beta = beta_estimates,
              formula = formula,
              data = data_out,
              hmc_settings = hmc_set,
              priors = prior_set)
  
  # set a class so we can have default functions on top of this
  class(out) <- c("scalar_fitted", class(out))  
  
  # return outputs
  out
  
}

# calculate fold IDs for each observation
calculate_cv_idx <- function(n_cv, n_obs) {
  
  cv_size <- floor(n_obs / n_cv)
  cv_vec <- sample(seq_len(n_obs), size = n_obs, replace = FALSE)
  cv_list <- vector("list", length = n_cv)
  for (i in seq_len(n_cv)) {
    if (i < n_cv) {
      start <- (i - 1) * cv_size + 1
      end <- i * cv_size
      cv_list[[i]] <- cv_vec[start:end]
    } else {
      start <- (i - 1) * cv_size + 1
      cv_list[[i]] <- cv_vec[start:length(cv_vec)]
    }
  }
  
  # return outputs
  cv_list
  
}

# create a list of data with the holdout folds in each element
prepare_cv_data <- function(idx, data) {
  
  # pull out data without fold i
  if (!is.null(dim(data$elevation))) {
  data <- list(isd = data$isd[-idx, ],
               richness = data$richness[-idx],
               abundance = data$abundance[-idx],
               site = data$site[-idx],
               basin = data$basin[-idx],
               year = data$year[-idx],
               stream_order = data$stream_order[-idx],
               elevation = data$elevation[-idx, ],
               water_temp = data$water_temp[-idx, ],
               water_depth = data$water_depth[-idx, ],
               water_vel = data$water_vel[-idx, ],
               water_conduct = data$water_conduct[-idx, ],
               ph = data$ph[-idx, ],
               nutrients = data$nutrients[-idx, ],
               rba = data$rba[-idx, ],
               riparian = data$riparian[-idx, ],
               deadwood = data$deadwood[-idx, ],
               native_riparian = data$native_riparian[-idx, ],
               natural_hydrol = data$natural_hydrol[-idx, ],
               habitat_diversity = data$habitat_diversity[-idx, ],
               macrophyte_cover = data$macrophyte_cover[-idx, ],
               wtl_exo = data$wtl_exo[-idx, ],
               wtl_nattras = data$wtl_nattras[-idx, ],
               pa_exo = data$pa_exo[-idx, ],
               pa_nattras = data$pa_nattras[-idx, ],
               abun_exo = data$abun_exo[-idx, ],
               abun_nattras = data$abun_nattras[-idx, ],
               rich_exo = data$rich_exo[-idx, ],
               rich_nattras = data$rich_nattras[-idx, ])
  } else {
    data <- list(isd = data$isd[-idx, ],
                 richness = data$richness[-idx],
                 abundance = data$abundance[-idx],
                 site = data$site[-idx],
                 basin = data$basin[-idx],
                 year = data$year[-idx], 
                 stream_order = data$stream_order[-idx],
                 elevation = data$elevation[-idx],
                 water_temp = data$water_temp[-idx],
                 water_depth = data$water_depth[-idx],
                 water_vel = data$water_vel[-idx],
                 water_conduct = data$water_conduct[-idx],
                 ph = data$ph[-idx],
                 nutrients = data$nutrients[-idx],
                 rba = data$rba[-idx],
                 riparian = data$riparian[-idx],
                 deadwood = data$deadwood[-idx],
                 native_riparian = data$native_riparian[-idx],
                 natural_hydrol = data$natural_hydrol[-idx],
                 habitat_diversity = data$habitat_diversity[-idx],
                 macrophyte_cover = data$macrophyte_cover[-idx],
                 wtl_exo = data$wtl_exo[-idx],
                 wtl_nattras = data$wtl_nattras[-idx],
                 pa_exo = data$pa_exo[-idx],
                 pa_nattras = data$pa_nattras[-idx],
                 abun_exo = data$abun_exo[-idx],
                 abun_nattras = data$abun_nattras[-idx],
                 rich_exo = data$rich_exo[-idx],
                 rich_nattras = data$rich_nattras[-idx])
  }
  
  # return outputs
  data
  
}

# create a list of data with the holdout folds in each element
prepare_cv_newdata <- function(idx, data) {
  
  # pull out data without fold i
  if (!is.null(dim(data$elevation))) {
    data <- list(isd = data$isd[idx, ],
                 richness = data$richness[idx],
                 abundance = data$abundance[idx],
                 site = data$site[idx],
                 basin = data$basin[idx],
                 year = data$year[idx],
                 stream_order = data$stream_order[idx],
                 elevation = data$elevation[idx, ],
                 water_temp = data$water_temp[idx, ],
                 water_depth = data$water_depth[idx, ],
                 water_vel = data$water_vel[idx, ],
                 water_conduct = data$water_conduct[idx, ],
                 ph = data$ph[idx, ],
                 nutrients = data$nutrients[idx, ],
                 rba = data$rba[idx, ],
                 riparian = data$riparian[idx, ],
                 deadwood = data$deadwood[idx, ],
                 native_riparian = data$native_riparian[idx, ],
                 natural_hydrol = data$natural_hydrol[idx, ],
                 habitat_diversity = data$habitat_diversity[idx, ],
                 macrophyte_cover = data$macrophyte_cover[idx, ],
                 wtl_exo = data$wtl_exo[idx, ],
                 wtl_nattras = data$wtl_nattras[idx, ],
                 pa_exo = data$pa_exo[idx, ],
                 pa_nattras = data$pa_nattras[idx, ],
                 abun_exo = data$abun_exo[idx, ],
                 abun_nattras = data$abun_nattras[idx, ],
                 rich_exo = data$rich_exo[idx, ],
                 rich_nattras = data$rich_nattras[idx, ])
  } else {
    data <- list(isd = data$isd[idx, ],
                 richness = data$richness[idx],
                 abundance = data$abundance[idx],
                 site = data$site[idx],
                 basin = data$basin[idx],
                 year = data$year[idx], 
                 stream_order = data$stream_order[idx],
                 elevation = data$elevation[idx],
                 water_temp = data$water_temp[idx],
                 water_depth = data$water_depth[idx],
                 water_vel = data$water_vel[idx],
                 water_conduct = data$water_conduct[idx],
                 ph = data$ph[idx],
                 nutrients = data$nutrients[idx],
                 rba = data$rba[idx],
                 riparian = data$riparian[idx],
                 deadwood = data$deadwood[idx],
                 native_riparian = data$native_riparian[idx],
                 natural_hydrol = data$natural_hydrol[idx],
                 habitat_diversity = data$habitat_diversity[idx],
                 macrophyte_cover = data$macrophyte_cover[idx],
                 wtl_exo = data$wtl_exo[idx],
                 wtl_nattras = data$wtl_nattras[idx],
                 pa_exo = data$pa_exo[idx],
                 pa_nattras = data$pa_nattras[idx],
                 abun_exo = data$abun_exo[idx],
                 abun_nattras = data$abun_nattras[idx],
                 rich_exo = data$rich_exo[idx],
                 rich_nattras = data$rich_nattras[idx])
  }
  
  # return outputs
  data
  
}

# function to predict from a fitted fda_response model
predict.fda_fitted <- function(object, newdata = NULL, type = c("link", "response"), ...) {
  
  # check if link or response or neither (default to link)
  if (length(type) == 2)
    type <- "link"
  
  # predict fitted values (without random curves) if newdata aren't provided
  if (is.null(newdata)) {
    newdata <- as.matrix(object$fda_response$data$x)
  } else {
    if (!is.matrix(newdata)) {
      # reformat to fda format if newdata isn't already a matrix
      fda_tmp <- fda_response(object$fda_response$formula,
                              data = newdata,
                              spline_settings = object$fda_response$spline_settings,
                              priors = object$fda_response$priors)
      newdata <- as.matrix(fda_tmp$data$x)
    }
  }
  
  # extract coefficients and predictors
  newdata <- cbind(rep(1, nrow(newdata)), newdata)
  betas <- object$beta
  
  # calculate median fitted values
  out <- newdata %*% t(sapply(betas, function(x) x[3, ]))

  # convert to link scale if required
  if (type == "response")
    out <- exp(out)
  
  # return outputs
  out
   
}

# function to predict from a fitted fda_response model
predict.scalar_fitted <- function(object, newdata = NULL, type = c("link", "response"), ...) {
  
  # check if link or response or neither (default to link)
  if (length(type) == 2)
    type <- "link"
  
  # predict fitted values (without random curves) if newdata aren't provided
  if (is.null(newdata)) {
    newdata <- as.matrix(object$data$x)
  } else {
    if (!is.matrix(newdata)) {
      # reformat to fda format if newdata isn't already a matrix
      formula_tmp <- update.formula(object$formula, isd ~ .)
      fda_tmp <- fda_response(formula_tmp, data = newdata)
      newdata <- as.matrix(fda_tmp$data$x)
      newdata <- cbind(rep(1, nrow(newdata)), newdata)
    }
  }

  # extract coefficients and predictors
  betas <- object$beta[, 3]
  
  # calculate median fitted values
  out <- newdata %*% betas

  # convert to link scale if required
  if (type == "response")
    out <- exp(out)
  
  # return outputs
  out
  
}

validate <- function(object, n_cv = 10, data) {

  # work out folds and create data lists
  if ("scalar_fitted" %in% class(object)) {
    n_obs <- length(object$data$y)
  } else {
    n_obs <- nrow(object$fda_response$data$y)
  }
  cv_list <- calculate_cv_idx(n_cv, n_obs)
  
  # prepare a list of data for each fold
  data_list <- lapply(cv_list, prepare_cv_data, data)
  
  # prepare a list of newdata for predictions
  newdata_list <- lapply(cv_list, prepare_cv_newdata, data)
  
  # fit the model n_cv times  
  mod_cv <- lapply(seq_len(n_cv), cv_inner,
                   object,
                   data_list,
                   newdata_list)
  
  # extract the observed and predicted values
  if ("scalar_fitted" %in% class(object)) {
    observed <- do.call(c, lapply(mod_cv, function(x) x$observed))
  } else {
    observed <- do.call(rbind, lapply(mod_cv, function(x) x$observed))
  }
  predicted <- do.call(rbind, lapply(mod_cv, function(x) x$predicted))
  
  # calculate r2 value
  r2 <- r2_deviance(observed, predicted)
  
  # return outputs
  list(r2 = r2, observed = observed, predicted = predicted)
  
}

cv_inner <- function(i, obj, data_list, newdata_list, mod_fn) {
  
  # fit the model
  if ("scalar_fitted" %in% class(obj)) {
    mod_tmp <- fit_scalar_model(obj$formula,
                                data = data_list[[i]],
                                hmc_settings = obj$hmc_settings,
                                priors = obj$priors) 
    observed <- get(all.vars(obj$formula)[1], envir = as.environment(newdata_list[[i]]), inherits = TRUE)
  } else {
    mod_tmp <- fit_functional_model(obj$fda_response$formula,
                                    data = data_list[[i]],
                                    hmc_settings = obj$hmc_settings,
                                    spline_settings = obj$fda_response$spline_settings,
                                    priors = obj$fda_response$priors)
    observed <- newdata_list[[i]]$isd
  }
  
  # return the predictions  
  predicted <- predict(mod_tmp, newdata_list[[i]], type = "response")

  # collate and return outputs
  list(predicted = predicted, observed = observed)
  
}
