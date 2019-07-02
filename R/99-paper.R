source("R/common.R")
source("R/std.R")

library(rio)

measure_prettynames <- c(ap = "AP", ndcg = "nDCG")
col_prettynames <- c(robust2004 = "Robust", terabyte2006 = "Terabyte")
std_cols <- c("raw" = "black", "z-std" = "#e41a1c", "N-std" = "#377eb8",
              "U-std" = "#e41a1c", "E-std" = "#4daf4a")
std_ltys <- c("raw" = 2, "z-std" = 2, "N-std" = 1, "U-std" = 1, "E-std" = 1)

path_out <- "out/figs"
dir.create(path_out, recursive = TRUE)

# Effect of standardization ########################################################################

# Mapping from raw to standardized -----------------------------------------------------------------

x <- import("data/robust2004_ap.csv")[51:99,] # only 2004 topics
x01 <- matrix(seq(0, 1, length.out = 100), nrow = nrow(x), ncol = 100, byrow = TRUE)

for(std in list(std_N, std_U, std_E)) {
  y <- std$standardize(std$factors(x), x)
  y01 <- std$standardize(std$factors(x), x01)

  my.dev.new(file.path(path_out, paste0(tolower(std$name), ".pdf")), num = 3, ratio = .56)
  plot(NA, xlim = 0:1, ylim = 0:1,
       xlab = "Raw score x", ylab = "Standardized score y", main = std$name)
  for(i in 1:nrow(x)){
    points(sort(x[i,]), sort(y[i,]), col ="#00000077", cex = .4)
    lines(sort(x01[i,]), sort(y01[i,]), type = "l", lty = 2)
  }
  my.dev.off()
}

# Per topic distributions --------------------------------------------------------------------------

plot_dist <- function(x, main, ylim = 0:1, ylab = "Standardized score y") {
  boxplot(t(x), xlim = c(1, nrow(x)), cex = .4, medlwd = 1, ylim = ylim,
          main = main, ylab = ylab, xlab = "", axes = FALSE, xaxs="i", mgp = c(1.5,.5,0))
  axis(2)
  box()
  title(xlab = "Topics", mgp = c(.5,0,0))
  points(rowMeans(x), pch = 19, cex = .4)
}

x <- as.matrix(read.csv("data/robust2004_ap.csv"))[51:99,] # only 2004 topics

my.dev.new(file.path(path_out, "hist-raw.pdf"), num = 4, ratio = .68, mar = c(-1,0,0,0))
plot_dist(x[order(rowMeans(x)),], main = "Raw scores", ylab = "Raw score x")
my.dev.off()

for(std in list(std_N, std_U, std_E)) {
  y <- std$standardize(std$factors(x), x)
  print(std$name)
  print(range(apply(y,1,mean)))
  print(range(apply(y,1,sd)))
  my.dev.new(file.path(path_out, paste0("hist-", tolower(std$name), ".pdf")),
             num = 4, ratio = .66, mar = c(-1,0,0,0))
  plot_dist(y[order(rowMeans(x)),], main = std$name)
  my.dev.off()
}

# Within-collection comparisons ####################################################################

for(collection in .COLLECTIONS) {
  for(measure in .MEASURES) {
    # tau ------------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("within_tau_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/01-within/", paste0(collection, "_", measure), "tau.csv"))
    r2 <- apply(r, 2, density)
    ylim <- c(0, max(sapply(r2, function(rr) max(rr$y)))+.5)

    plot(NA, xlim = c(0.85, 1), ylim = ylim, yaxs = "i", xaxs ="i",
         xlab = expression(tau), ylab = "Density", bty = "l",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    for(std in colnames(r)[-1]) {
      lines(r2[[std]], col = std_cols[std], lty = std_ltys[std], lwd = 1)
      rug(mean(r[,std]), col = std_cols[std], lty = std_ltys[std], lwd = 1, ticksize = .05)
    }
    legend("topleft", colnames(r)[-1], lwd = 1, cex = .8,
           lty = std_ltys[colnames(r)[-1]], col = std_cols[colnames(r)[-1]], bty = "n")

    my.dev.off()

    # tauAP ----------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("within_tauAP_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/01-within/", paste0(collection, "_", measure), "tauAP.csv"))
    r2 <- apply(r, 2, density)
    ylim <- c(0, max(sapply(r2, function(rr) max(rr$y)))+.5)

    plot(NA, xlim = c(0.75, 1), ylim = ylim, yaxs = "i", xaxs ="i",
         xlab = expression(tau[ap]), ylab = "Density", bty = "l",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    for(std in colnames(r)[-1]) {
      lines(r2[[std]], col = std_cols[std], lty = std_ltys[std], lwd = 1)
      rug(mean(r[,std]), col = std_cols[std], lty = std_ltys[std], lwd = 1, ticksize = .05)
    }
    legend("topleft", colnames(r)[-1], lwd = 1, cex = .8,
           lty = std_ltys[colnames(r)[-1]], col = std_cols[colnames(r)[-1]], bty = "n")

    my.dev.off()

    # power ----------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("within_power_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/01-within/", paste0(collection, "_", measure), "power.csv"))

    plot(NA, xlim = range(r$alpha), ylim = range(r[,-1]), yaxs = "i", xaxs ="i",
         xlab = expression("Significance level "*alpha), ylab = "Power", bty = "l", log = "x",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    for(std in colnames(r)[-1]) {
      lines(r$alpha, r[,std], col = std_cols[std], lty = std_ltys[std], lwd = 1)
    }
    legend("bottomright", colnames(r)[-1], lwd = 1, bty = "n", cex = .8,
           lty = std_ltys[colnames(r)[-1]], col = std_cols[colnames(r)[-1]])
    axis(1, at = r$alpha, rep("", length(r$alpha)))

    my.dev.off()
  }
}

# Between-collection comparisons ###################################################################

for(collection in .COLLECTIONS) {
  for(measure in .MEASURES) {
    # tau ------------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("between_tau_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/02-between/", paste0(collection, "_", measure), "tau.csv"))
    r2 <- apply(r, 2, density)
    ylim <- c(0, max(sapply(r2, function(rr) max(rr$y)))+.5)

    plot(NA, xlim = c(.65, .95), ylim = ylim, yaxs = "i", xaxs ="i",
         xlab = expression(tau), ylab = "Density", bty = "l",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    for(std in colnames(r)) {
      lines(r2[[std]], col = std_cols[std], lty = std_ltys[std], lwd = 1)
      rug(mean(r[,std]), col = std_cols[std], lty = std_ltys[std], lwd = 1, ticksize = .05)
    }
    legend("topleft", colnames(r), lwd = 1, cex = .8,
           lty = std_ltys[colnames(r)], col = std_cols[colnames(r)], bty = "n")

    my.dev.off()

    # tauAP ----------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("between_tauAP_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/02-between/", paste0(collection, "_", measure), "tauAP.csv"))
    r2 <- apply(r, 2, density)
    ylim <- c(0, max(sapply(r2, function(rr) max(rr$y)))+.5)

    plot(NA, xlim = c(.5, .9), ylim = ylim, yaxs = "i", xaxs ="i",
         xlab = expression(tau[ap]), ylab = "Density", bty = "l",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    for(std in colnames(r)) {
      lines(r2[[std]], col = std_cols[std], lty = std_ltys[std], lwd = 1)
      rug(mean(r[,std]), col = std_cols[std], lty = std_ltys[std], lwd = 1, ticksize = .05)
    }
    legend("topleft", colnames(r), lwd = 1, cex = .8,
           lty = std_ltys[colnames(r)], col = std_cols[colnames(r)], bty = "n")

    my.dev.off()

    # type 1 ---------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("between_type1_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/02-between/", paste0(collection, "_", measure), "type1.csv"))

    plot(NA, xlim = range(r$alpha), ylim = range(r$alpha), yaxs = "i", xaxs ="i",  bty = "l",
         xlab = expression("Significance level "*alpha), ylab = "Type I error rate", log = "xy",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    abline(0:1, col = "darkgrey")
    for(std in colnames(r)[-1]) {
      lines(r$alpha, r[,std], col = std_cols[std], lty = std_ltys[std], lwd = 1)
    }
    axis(1, at = r$alpha, rep("", length(r$alpha)))
    axis(2, at = r$alpha, rep("", length(r$alpha)))
    legend("topleft", colnames(r)[-1], lwd = 1, cex = .8,
           lty = std_ltys[colnames(r)[-1]], col = std_cols[colnames(r)[-1]], bty = "n")

    my.dev.off()

    # power ----------------------------------------------------------------------------------------

    my.dev.new(file.path(path_out, paste0("between_power_", collection, "_", measure, ".pdf")),
               num = 4, ratio = .66)
    r <- import(file.path("out/02-between/", paste0(collection, "_", measure), "power.csv"))

    plot(NA, xlim = range(r$alpha), ylim = range(r[,-1]), yaxs = "i", xaxs ="i",
         xlab = expression("Significance level "*alpha), ylab = "Power", bty = "l", log = "x",
         main = paste(col_prettynames[collection], "-", measure_prettynames[measure]))
    for(std in colnames(r)[-1]) {
      lines(r$alpha, r[,std], col = std_cols[std], lty = std_ltys[std], lwd = 1)
    }
    legend("bottomright", colnames(r)[-1], lwd = 1, bg = "white", box.lty = 0, cex = .8,
           lty = std_ltys[colnames(r)[-1]], col = std_cols[colnames(r)[-1]])
    axis(1, at = r$alpha, rep("", length(r$alpha)))

    my.dev.off()
  }
}

