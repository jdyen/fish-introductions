# barplot funtion
barplot_mod <- function(x, var_names, var_names_lower = NULL, ...) {
  
  nplot <- nrow(x)
  ylims <- range(x)
  xlims <- c(0.5, nplot + 0.5)
  plot(x[, 3] ~ seq_len(nplot),
       type = "n", bty = "l", xaxt = "n", yaxt = "n",
       xlim = xlims, ylim = ylims,
       xlab = "", ylab = "", ...)
  for (i in seq_len(nplot)) {
    lines(c(i, i), c(x[i, 1], x[i, 5]), lwd = 2)
    lines(c(i, i), c(x[i, 2], x[i, 4]), lwd = 4)
  }
  var_line <- 2.8
  if (!is.null(var_names_lower)) {
    var_line <- 3.5 
    for (i in seq_along(var_names_lower))
      mtext(var_names_lower[i], side = 1, at = (2 * i - 0.5), line = 2.15, cex = 0.7)
  }
  mtext("Variables", side = 1, adj = 0.5, line = var_line)
  mtext("Estimate", side = 2, adj = 0.5, line = 2.75)
  points(x[, 3] ~ seq_len(nplot), pch = 16)
  lines(c(0, nplot + 1), c(0, 0), lty = 2, lwd = 2)
  axis(1, at = seq_len(nplot), labels = var_names, tick = TRUE, cex.axis = ifelse(length(var_names) > 6, 0.75, 1), ...)
  axis(2, las = 1)
  
}