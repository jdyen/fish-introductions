# set a working directory
setwd("~/Dropbox/research/fish-introductions/")

# source some helper functions
source("code/plot-helpers.R")

# set plot type and main settings
plot_type <- "jpeg"
file_type <- "jpg"
unit_set <- "in"
width_set <- 10
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
get(plot_type)(file = paste0("outputs/plots/Fig1.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(3, 2), mar = c(4.9, 4.1, 3.1, 0.8))
beta_vals <- mod_scalar$beta
idx_sub <- list(11:16, 11:16, 11:14, 11:14, 11:14, 11:14)
main_labels <- c("Species richness (all species)", "Abundance (all species)",
                 "Species richness (exotic species)", "Abundance (exotic species)",
                 "Species richness (translocated species)", "Abundance (translocated species)")
label_set <- list(c("Exotic", "Transl.", "Exotic", "Transl.", "Exotic", "Transl."),
                  c("Exotic", "Transl.", "Exotic", "Transl.", "Exotic", "Transl."),
                  c("Trophic level", "Presence", "Abundance", "Richness"),
                  c("Trophic level", "Presence", "Abundance", "Richness"),
                  c("Trophic level", "Presence", "Abundance", "Richness"),
                  c("Trophic level", "Presence", "Abundance", "Richness"))
for (i in seq_along(beta_vals)) {
  
  plot_vals <- beta_vals[[i]][idx_sub[[i]], ]
  main_var <- NULL
  if(i < 3)
    main_var <- c("Presence", "Abundance", "Richness")
  barplot_mod(plot_vals, var_names = label_set[[i]], var_names_lower = main_var)
  
  mtext(main_labels[i], side = 3, line = 1, adj = 1, cex = 1.1)

}
dev.off()

# change dims for Fig. 2
width_set <- 7
height_set <- 8

# plot scalar model effects: now plot remaining predictors
get(plot_type)(file = paste0("outputs/plots/Fig2.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 1), mar = c(4.9, 4.1, 3.1, 0.8))
beta_vals <- mod_scalar$beta[1:2]
idx_sub <- 2:10
main_labels <- c("Species richness", "Abundance")
label_set <- c("Elev.", "Water temp.", "Depth",
               "Water vel.", "Cond.", "pH", "Nutr.",
               "Deadwood", "RBA")
for (i in seq_along(beta_vals)) {
  
  plot_vals <- beta_vals[[i]][idx_sub, ]
  barplot_mod(plot_vals, var_names = label_set)
  
  mtext(main_labels[i], side = 3, line = 1, adj = 1, cex = 1.1)
  
}
dev.off()

# plot intercepts and/or estimated size distributions at multiple levels of predictors
#### TO ADD

# change plot dimensions for functional plots
width_set <- 7
height_set <- 7

# plot the estimated functional effects from the full model
#   -subset to exotic and translocated species predictors first
get(plot_type)(file = paste0("outputs/plots/Fig3.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(3, 2), mar = c(4.9, 4.1, 3.1, 0.8))
beta_vals <- mod_functional$beta$full
idx_sub <- 11:16
main_labels <- c("Presence of exotic species", "Presence of translocated species",
                 "Abundance of exotic species", "Abundance of translocated species",
                 "Richness of exotic species", "Richness of translocated species")
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
get(plot_type)(file = paste0("outputs/plots/Fig4.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 2), mar = c(4.9, 4.1, 3.1, 0.8))
beta_vals <- mod_functional$beta$exotic
idx_sub <- 11:14
main_labels <- c("Weighted trophic level of exotic species",
                 "Presence of exotic species",
                 "Abundance of exotic species",
                 "Richness of exotic species")
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
get(plot_type)(file = paste0("outputs/plots/Fig5.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
par(mfrow = c(2, 2), mar = c(4.9, 4.1, 3.1, 0.8))
beta_vals <- mod_functional$beta$translocated
idx_sub <- 11:14
main_labels <- c("Weighted trophic level of translocated species",
                 "Presence of translocated species",
                 "Abundance of translocated species",
                 "Richness of translocated species")
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

# plot the estimated functional effects from the full model
#   -subset to remaining predictors
get(plot_type)(file = paste0("outputs/plots/Fig6.", file_type), width = width_set, height = height_set, units = unit_set, res = res_set)
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
