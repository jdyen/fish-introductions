# we need to load the data
fish_sizes <- read.csv("data/bd-fish-size.csv", stringsAsFactors = FALSE)
predictors <- read.csv("data/bd-predictors.csv", stringsAsFactors = FALSE)
rba <- read.csv("data/bd-rba.csv", stringsAsFactors = FALSE)

# add rba values to predictors
predictors$rba <- rba$RBA[match(predictors$Code1, rba$CODE1)]

# which species do we want?
sp_list <- c("SATR", "LUGR", "BAME", "PAMY")

# for each species, want to create a data set everywhere they are native
data_nattras_tl <- data_exo_tl <- data_set <- list()
for (i in seq_along(sp_list)) {
  
  # now we want to pull out the native species; these are our response
  fish_sp <- fish_sizes[fish_sizes$Species == sp_list[i], ]
  fish_native <- fish_sp[fish_sizes$STATUS == "N", ]
  
  # we have our individual sizes, let"s bin them so we can analyse them as distributions
  hist_fn <- function(x, breaks) {
    hist(x, breaks = breaks, plot = FALSE)$counts
  }
  breaks <- exp(seq(log(min(fish_native$W_g, na.rm = TRUE)),
                    log(max(fish_native$W_g, na.rm = TRUE)),
                    length = 21))
  fish_isd <- tapply(fish_native$W_g, fish_native$Code1, hist_fn,
                     breaks = breaks)
  fish_isd <- do.call(rbind, fish_isd)
  fish_rich <- tapply(fish_native$Species, fish_native$Code1, function(x) length(unique(x)))
  fish_abund <- tapply(fish_native$Species, fish_native$Code1, length)
  
  # we can match the predictors to our response data by matching the Code1 column
  predictors_sorted <- predictors[match(rownames(fish_isd), predictors$Code1), ]
  
  # this is missing some, remove them for now
  is_missing <- is.na(predictors_sorted$NativeRiparian_cover)
  predictors_sorted <- predictors_sorted[!is_missing, ]
  fish_isd <- fish_isd[!is_missing, ]
  fish_rich <- fish_rich[!is_missing]
  fish_abund <- fish_abund[!is_missing]
  
  # we should put everything into a clean data set
  data_set[[i]] <- list(isd = fish_isd,
                        richness = fish_rich,
                        abundance = fish_abund,
                        year = predictors_sorted$Year,
                        site = predictors_sorted$Code1,
                        basin = predictors_sorted$Basin,
                        stream_order = predictors_sorted$Stream_order,
                        elevation = scale(predictors_sorted$Elevation),
                        water_temp = scale(predictors_sorted$Water_temperature),
                        water_depth = scale(predictors_sorted$Water_depth),
                        water_vel = scale(predictors_sorted$Water_velocity),
                        water_conduct = scale(predictors_sorted$Water_conductivity),
                        ph = scale(predictors_sorted$pH),
                        nutrients = scale(predictors_sorted$Toxic_Nutrients + predictors_sorted$Other_Nutrients),
                        deadwood = scale(predictors_sorted$Dead_wood),
                        rba = scale(predictors_sorted$rba),
                        riparian = scale(predictors_sorted$AllRiparian_cover),
                        native_riparian = scale(predictors_sorted$NativeRiparian_cover),
                        natural_hydrol = scale(predictors_sorted$Natural_hydrology),
                        habitat_diversity = scale(predictors_sorted$Habitat_diversity),
                        macrophyte_cover = scale(predictors_sorted$Macrophyte_cover),
                        wtl_exo = scale(predictors_sorted$WTL_exo),
                        wtl_nattras = scale(predictors_sorted$WTL_nattras),
                        pa_exo = scale(predictors_sorted$PA_exo),
                        pa_nattras = scale(predictors_sorted$PA_nattras),
                        abun_exo = scale(predictors_sorted$Abun_exo),
                        abun_nattras = scale(predictors_sorted$Abun_nattras),
                        rich_exo = scale(predictors_sorted$Rich_exo),
                        rich_nattras = scale(predictors_sorted$Rich_nattras))
  
  # create two data subsets, one each for exotic and translocated species with WTL estimates
  subset_fn <- function(x, subset) {
    if (!is.null(dim(x))) {
      if (length(dim(x)) > 1) {
        x <- x[subset, ]
      } else {
        x <- x[subset]
      }
    } else {
      x <- x[subset]
    }
    x
  }
  data_exo_tl[[i]] <- lapply(data_set[[i]], subset_fn, !is.na(data_set[[i]]$wtl_exo))
  data_nattras_tl[[i]] <- lapply(data_set[[i]], subset_fn, !is.na(data_set[[i]]$wtl_nattras))
  
}
