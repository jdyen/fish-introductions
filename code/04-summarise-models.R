# load required packages
library(hier.part)

# source some helper functions
source("code/load-data.R")
source("code/helpers.R")
source("code/plot-helpers.R")

# set plot type and main settings
plot_type <- "jpeg"
file_type <- "jpg"
unit_set <- "in"
width_set <- 7.2
height_set <- 7
res_set <- 300

# load fitted models
mod_scalar <- readRDS("outputs/scalar_fitted_cv.rds")
mod_functional <- readRDS("outputs/functional_fitted_cv.rds")

# load observed fish sizes to calculate plot limits
fish_sizes <- read.csv("data/bd-fish-size.csv", stringsAsFactors = FALSE)
fish_native <- fish_sizes[fish_sizes$STATUS == "N", ]
breaks <- exp(seq(log(min(fish_native$W_g, na.rm = TRUE)),
                  log(max(fish_native$W_g, na.rm = TRUE)),
                  length = 21))

# plot scalar model effects: start with effects of introduced species
get(plot_type)(file = paste0("outputs/plots/Fig_scalar.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 1), mar = c(4.7, 4.1, 2.1, 0.8))
beta_vals <- mod_scalar$beta
idx_sub <- list(11:14, 11)
main_labels <- letters[1:2]
label_set <- rep(c("Exotic", "Transl."), 3)
for (i in 1:2) {
  
  plot_vals <- beta_vals[[i]][idx_sub[[1]], ]
  plot_vals <- rbind(
    plot_vals,
    beta_vals[[i + 2]][idx_sub[[2]], ],
    beta_vals[[i + 4]][idx_sub[[2]], ]
  )
  main_var <- c("Presence", "Abundance", "Trophic level")
  barplot_mod(plot_vals, var_names = label_set, var_names_lower = main_var, xlab = "")
  
  mtext(main_labels[i], side = 3, line = 0.5, adj = 0, cex = 1.5)

}
dev.off()

# change dims for Fig. 2
width_set <- 7.2
height_set <- 7

# plot scalar model effects: now plot remaining predictors
get(plot_type)(file = paste0("outputs/plots/Fig_env_scalar.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 1), mar = c(4.7, 4.1, 2.1, 0.8))
beta_vals <- mod_scalar$beta[1:2]
idx_sub <- 2:10
main_labels <- c("Species richness", "Abundance")
main_labels <- letters[1:2]
label_set <- c("Elev.", "Water temp.", "Depth",
               "Water vel.", "Cond.", "pH", "Nutr.",
               "Deadwood", "RBA")
for (i in seq_along(beta_vals)) {
  
  plot_vals <- beta_vals[[i]][idx_sub, ]
  barplot_mod(plot_vals, var_names = label_set)
  
  mtext(main_labels[i], side = 3, line = 0.5, adj = 0, cex = 1.5)
  
}
dev.off()

# change plot dimensions for functional plots
width_set <- 7.2
height_set <- 7

# plot the estimated functional effects from the full model
#   -subset to exotic and translocated species predictors first
get(plot_type)(file = paste0("outputs/plots/FigISD.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 2), mar = c(4.7, 4.1, 2.1, 0.8))
beta_vals <- mod_functional$beta$full
idx_sub <- 11:14
main_labels <- letters[1:4]
for (i in seq_along(beta_vals[idx_sub])) {
  
  # pull out mid, lower, and upper lines to plot
  plot_vals <- beta_vals[[idx_sub[i]]]
  
  # plot them!
  xplot <- breaks[-length(breaks)] + diff(breaks)
  plot(plot_vals[3, ] ~ xplot, type = "n",
       bty = "l", las = 1,
       xlab = "", ylab = "",
       ylim = range(plot_vals), log = "x")
  polygon(c(xplot, rev(xplot)),
          c(plot_vals[1, ], rev(plot_vals[5, ])),
          border = NA, col = scales::alpha("gray50", 0.5))
  polygon(c(xplot, rev(xplot)),
          c(plot_vals[2, ], rev(plot_vals[4, ])),
          border = NA, col = scales::alpha("gray30", 0.7))
  lines(plot_vals[3, ] ~ xplot, lwd = 2, col = "gray30")
  lines(c(1e-15, max(xplot) + 100), c(0, 0), lty = 2)
  
  mtext(main_labels[i], side = 3, line = 0.5, adj = 0, cex = 1.5)
  mtext("Size (g)", side = 1, line = 2.5, adj = 0.5, cex = 1)
  mtext("Effect", side = 2, line = 2.5, adj = 0.5, cex = 1)
  
}
dev.off()

# plot the estimated functional effects from the exotic species model
#   -subset to exotic species predictors first
width_set <- 7.2 # change dimensions again
get(plot_type)(file = paste0("outputs/plots/Fig4.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 1), mar = c(4.7, 4.1, 2.1, 0.8))
idx_sub <- 11
beta_vals <- list(mod_functional$beta$exotic[[idx_sub]],
                  mod_functional$beta$translocated[[idx_sub]])
main_labels <- letters[1:2]

for (i in seq_along(beta_vals)) {
  
  # pull out mid, lower, and upper lines to plot
  plot_vals <- beta_vals[[i]]
  
  # plot them!
  xplot <- breaks[-length(breaks)] + diff(breaks)
  plot(plot_vals[3, ] ~ xplot, type = "n",
       bty = "l", las = 1,
       xlab = "", ylab = "",
       ylim = range(plot_vals), log = "x")
  polygon(c(xplot, rev(xplot)),
          c(plot_vals[1, ], rev(plot_vals[5, ])),
          border = NA, col = scales::alpha("gray50", 0.5))
  polygon(c(xplot, rev(xplot)),
          c(plot_vals[2, ], rev(plot_vals[4, ])),
          border = NA, col = scales::alpha("gray30", 0.7))
  lines(plot_vals[3, ] ~ xplot, lwd = 2, col = "gray30")
  lines(c(1e-15, max(xplot) + 100), c(0, 0), lty = 2)
  
  mtext(main_labels[i], side = 3, line = 0.5, adj = 0, cex = 1.5)
  mtext("Size (g)", side = 1, line = 2.5, adj = 0.5, cex = 1)
  mtext("Effect", side = 2, line = 2.5, adj = 0.5, cex = 1)
  
}

dev.off()

# plot the estimated functional effects from the full model
#   -subset to remaining predictors
width_set <- 7.2
get(plot_type)(file = paste0("outputs/plots/Fig_env_ISD.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(3, 3), mar = c(4.9, 4.1, 3.1, 0.8))
beta_vals <- mod_functional$beta$full
idx_sub <- 2:10
label_set <- c("Elevation", "Water temperature", "Water depth",
               "Water velocity", "Conductivity", "pH", "Nutrients",
               "Deadwood", "RBA")
for (i in seq_along(idx_sub)) {
  
  # pull out mid, lower, and upper lines to plot
  plot_vals <- beta_vals[[idx_sub[i]]]
  
  # plot them!
  xplot <- breaks[-length(breaks)] + diff(breaks)
  plot(plot_vals[3, ] ~ xplot, type = "n",
       bty = "l", las = 1,
       xlab = "", ylab = "",
       ylim = range(plot_vals), log = "x")
  polygon(c(xplot, rev(xplot)),
          c(plot_vals[1, ], rev(plot_vals[5, ])),
          border = NA, col = scales::alpha("gray50", 0.5))
  polygon(c(xplot, rev(xplot)),
          c(plot_vals[2, ], rev(plot_vals[4, ])),
          border = NA, col = scales::alpha("gray30", 0.7))
  lines(plot_vals[3, ] ~ xplot, lwd = 2, col = "gray30")
  lines(c(1e-15, max(xplot) + 100), c(0, 0), lty = 2)
  
  mtext(label_set[i], side = 3, line = 1, adj = 1, cex = 0.95)
  mtext("Size (g)", side = 1, line = 2.5, adj = 0.5, cex = 1)
  mtext("Effect", side = 2, line = 2.5, adj = 0.5, cex = 1)
  
}
dev.off()

# additional code to calculate hierarchical partitioning results
functional_fitted <- readRDS("outputs/hp_functional_fitted_cv.rds")
scalar_fitted <- readRDS("outputs/hp_scalar_fitted_cv.rds")

# full model, hp_full1 to hp_full8, in following order:
#   full, nattras, exotic, env, env+nattras, exo+nattras, env+exo, int
# need to reorder these to put them in ascending order
order <- c(8, 2, 3, 4, 6, 5, 7, 1)
# and can now calculate hp values
func_full_hp <- partition(functional_fitted$r2[2:9][order], pcan = 3)

# exotic species only model
#   hp_exotic1 to hp_exotic4:
#   full, exotic, env, null
order <- c(4, 2, 3, 1)
func_exotic_hp <- partition(functional_fitted$r2[11:14][order], pcan = 2)

# nattras species only model
#   hp_nattras1 to hp_nattras4:
#   full, nattras, env, null
order <- c(4, 2, 3, 1)
func_nattras_hp <- partition(functional_fitted$r2[16:19][order], pcan = 2)

# repeat for scalar models
# full model for richness
ids <- c(2:9)
order <- c(8, 2, 3, 4, 6, 5, 7, 1)
scalar_rich_hp <- partition(scalar_fitted$r2[ids][order], pcan = 3)

# full model for abundance
ids <- c(11:18)
order <- c(8, 2, 3, 4, 6, 5, 7, 1)
scalar_abun_hp <- partition(scalar_fitted$r2[ids][order], pcan = 3)

# exotic only model for richness
ids <- c(20:23)
order <- c(4, 2, 3, 1)
scalar_exo_rich_hp <- partition(scalar_fitted$r2[ids][order], pcan = 2)

# exotic only model for abundance
ids <- c(25:28)
order <- c(4, 2, 3, 1)
scalar_exo_abun_hp <- partition(scalar_fitted$r2[ids][order], pcan = 2)

# nattras only model for richness
ids <- c(30:33)
order <- c(4, 2, 3, 1)
scalar_nattras_rich_hp <- partition(scalar_fitted$r2[ids][order], pcan = 2)

# nattras only model for abundance
ids <- c(35:38)
order <- c(4, 2, 3, 1)
scalar_nattras_abun_hp <- partition(scalar_fitted$r2[ids][order], pcan = 2)

# additional plot of variable correlation structure
vars_to_use <- c("elevation",
                 "water_temp",
                 "water_depth",
                 "water_vel",
                 "water_conduct",
                 "ph",
                 "nutrients",
                 "deadwood",
                 "rba",
                 "pa_exo",
                 "pa_nattras",
                 "abun_exo",
                 "abun_nattras")

test_vars <- sapply(vars_to_use, function(x) data_set[[x]])

corrs <- cor(test_vars)

png(file = "outputs/plots/correlations.png", width = 1080, height = 900,
    pointsize = 16)
par(mar = c(7.1, 7.1, 1.1, 1.1))
image(corrs, xaxt = "n", yaxt = "n")
axis(2, at = seq(0, 1, length = 13), labels = vars_to_use, las= 2)
axis(1, at = seq(0, 1, length = 13), labels = vars_to_use, las= 2)

xgrid <- c(expand.grid(seq(0, 1, length = 13), seq(0, 1, length = 13)))
text(x = xgrid$Var1,
     y = xgrid$Var2,
     labels = round(c(corrs), 2))
dev.off()

## Additional code to summarise intraspecific analyses
# set plot type and main settings
plot_type <- "jpeg"
file_type <- "jpg"
unit_set <- "in"
width_set <- 10
height_set <- 7
res_set <- 300

# load fitted models
mod_full <- qs::qread("outputs/species_fitted_full.qs")
mod_exotic <- qs::qread("outputs/species_fitted_exotic.qs")
mod_transloc <- qs::qread("outputs/species_fitted_transloc.qs")

# load observed fish sizes to calculate plot limits
fish_sizes <- read.csv("data/bd-fish-size.csv", stringsAsFactors = FALSE)
fish_native <- fish_sizes[fish_sizes$STATUS == "N", ]
breaks <- exp(seq(log(min(fish_native$W_g, na.rm = TRUE)),
                  log(max(fish_native$W_g, na.rm = TRUE)),
                  length = 21))

# change plot dimensions for functional plots
width_set <- 7.2
height_set <- 7

# plot the estimated functional effects from the full model
#   -subset to exotic and translocated species predictors first
sp_list <- c("SATR", "LUGR", "BAME", "PAMY")
for (sp in seq_along(mod_full)) {
  get(plot_type)(file = paste0("outputs/plots/Fig_", sp_list[sp], "_", "full.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
  par(mfrow = c(2, 2), mar = c(4.9, 4.1, 3.1, 0.8))
  beta_vals <- mod_full[[sp]]$beta
  idx_sub <- 11:14
  main_labels <- c("Presence of exotic species", "Presence of translocated species",
                   "Abundance of exotic species", "Abundance of translocated species")
  for (i in seq_along(beta_vals[idx_sub])) {
    
    # pull out mid, lower, and upper lines to plot
    plot_vals <- beta_vals[[idx_sub[i]]]
    
    # plot them!
    xplot <- breaks[-length(breaks)] + diff(breaks)
    plot(plot_vals[3, ] ~ xplot, type = "n",
         bty = "l", las = 1,
         xlab = "", ylab = "",
         ylim = range(plot_vals), log = "x")
    polygon(c(xplot, rev(xplot)),
            c(plot_vals[1, ], rev(plot_vals[5, ])),
            border = NA, col = scales::alpha("gray50", 0.5))
    polygon(c(xplot, rev(xplot)),
            c(plot_vals[2, ], rev(plot_vals[4, ])),
            border = NA, col = scales::alpha("gray30", 0.7))
    lines(plot_vals[3, ] ~ xplot, lwd = 2, col = "gray30")
    lines(c(1e-15, max(xplot) + 100), c(0, 0), lty = 2)
    
    mtext(main_labels[i], side = 3, line = 1, adj = 1, cex = 1.1)
    mtext("Size (g)", side = 1, line = 2.5, adj = 0.5, cex = 1)
    mtext("Effect", side = 2, line = 2.5, adj = 0.5, cex = 1)
    
  }
  dev.off()
  
  # plot the estimated functional effects from the exotic species model
  #   -subset to exotic species predictors first
  get(plot_type)(file = paste0("outputs/plots/Fig_", sp_list[sp], "_", "exotic.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
  par(mfrow = c(3, 1), mar = c(4.9, 4.1, 3.1, 0.8))
  beta_vals <- mod_exotic[[sp]]$beta
  idx_sub <- 11:13
  main_labels <- c("Weighted trophic level of exotic species",
                   "Presence of exotic species",
                   "Abundance of exotic species")
  for (i in seq_along(beta_vals[idx_sub])) {
    
    # pull out mid, lower, and upper lines to plot
    plot_vals <- beta_vals[[idx_sub[i]]]
    
    # plot them!
    xplot <- breaks[-length(breaks)] + diff(breaks)
    plot(plot_vals[3, ] ~ xplot, type = "n",
         bty = "l", las = 1,
         xlab = "", ylab = "",
         ylim = range(plot_vals), log = "x")
    polygon(c(xplot, rev(xplot)),
            c(plot_vals[1, ], rev(plot_vals[5, ])),
            border = NA, col = scales::alpha("gray50", 0.5))
    polygon(c(xplot, rev(xplot)),
            c(plot_vals[2, ], rev(plot_vals[4, ])),
            border = NA, col = scales::alpha("gray30", 0.7))
    lines(plot_vals[3, ] ~ xplot, lwd = 2, col = "gray30")
    lines(c(1e-15, max(xplot) + 100), c(0, 0), lty = 2)
    
    mtext(main_labels[i], side = 3, line = 1, adj = 1, cex = 0.9)
    mtext("Size (g)", side = 1, line = 2.5, adj = 0.5, cex = 1)
    mtext("Effect", side = 2, line = 2.5, adj = 0.5, cex = 1)
    
  }
  dev.off()
  
  # plot the estimated functional effects from the translocated species model
  #   -subset to translocated species predictors first
  get(plot_type)(file = paste0("outputs/plots/Fig_", sp_list[sp], "_", "transloc.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
  par(mfrow = c(3, 1), mar = c(4.9, 4.1, 3.1, 0.8))
  beta_vals <- mod_transloc[[sp]]$beta
  idx_sub <- 11:13
  main_labels <- c("Weighted trophic level of translocated species",
                   "Presence of translocated species",
                   "Abundance of translocated species")
  for (i in seq_along(beta_vals[idx_sub])) {
    
    # pull out mid, lower, and upper lines to plot
    plot_vals <- beta_vals[[idx_sub[i]]]
    
    # plot them!
    xplot <- breaks[-length(breaks)] + diff(breaks)
    plot(plot_vals[3, ] ~ xplot, type = "n",
         bty = "l", las = 1,
         xlab = "", ylab = "",
         ylim = range(plot_vals), log = "x")
    polygon(c(xplot, rev(xplot)),
            c(plot_vals[1, ], rev(plot_vals[5, ])),
            border = NA, col = scales::alpha("gray50", 0.5))
    polygon(c(xplot, rev(xplot)),
            c(plot_vals[2, ], rev(plot_vals[4, ])),
            border = NA, col = scales::alpha("gray30", 0.7))
    lines(plot_vals[3, ] ~ xplot, lwd = 2, col = "gray30")
    lines(c(1e-15, max(xplot) + 100), c(0, 0), lty = 2)
    
    mtext(main_labels[i], side = 3, line = 1, adj = 1, cex = 0.9)
    mtext("Size (g)", side = 1, line = 2.5, adj = 0.5, cex = 1)
    mtext("Effect", side = 2, line = 2.5, adj = 0.5, cex = 1)
    
  }
  dev.off()
  
}
