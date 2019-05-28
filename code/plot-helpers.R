# barplot funtion
barplot_mod <- function(x, var_names, ...) {
  
  nplot <- nrow(x)
  ylims <- range(x)
  xlims <- c(0.5, nplot + 0.5)
  plot(x[, 3] ~ seq_len(nplot),
       type = "n", bty = "l", xaxt = "n", yaxt = "n",
       xlim = xlims, ylim = ylims,
       xlab = "Variables", ylab = "Estimate", ...)
  for (i in seq_len(nplot)) {
    lines(c(i, i), c(x[i, 1], x[i, 5]), lwd = 2)
    lines(c(i, i), c(x[i, 2], x[i, 4]), lwd = 4)
  }
  points(x[, 3] ~ seq_len(nplot), pch = 16)
  lines(c(0, nplot + 1), c(0, 0), lty = 2, lwd = 2)
  axis(1, at = seq_len(nplot), labels = var_names, ...)
  axis(2, las = 1)
  
}