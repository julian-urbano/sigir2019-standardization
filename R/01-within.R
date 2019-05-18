source("R/common.R")
source("R/std.R")

library(ircor)
library(rio)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

#' compute statistics between raw scores x and standardized scores y
within_collection <- function(x, y) {
  n_s <- ncol(x) # number of systems
  means_x <- colMeans(x)
  means_y <- colMeans(y)

  # correlations (b for ties)
  tau <- tau_b(means_x, means_y)
  tauAP <- tauAP_b(means_x, means_y)
  r <- cor(means_x, means_y, method = "pearson")

  # paired t-test between every pair of systems
  power <- unlist(sapply(1:(n_s-1), function(i) {
    sapply((i+1):n_s, function(j) {
      t.test(y[,i], y[,j], paired = TRUE)$p.value
    })
  }))
  power <- ecdf(power)(.ALPHAS)

  list(tau = unname(tau), tauAP = unname(tauAP), r = unname(r),
       power = unname(power))
}

# Compute ------------------------------------------------------------------------------------------

path_out <- "scratch/01-within"

for(batch in 1:.BATCHES) {
  for(collection in .COLLECTIONS) {
    for(measure in .MEASURES) {
      collmea <- paste0(collection, "_", measure)
      path_out_collmea <- file.path(path_out, collmea)

      x <- import(file.path("data", paste0(collmea, ".csv")))
      n_t <- nrow(x)
      n <- 50

      for(std in stds) {
        path_out_collmea_std <- file.path(path_out_collmea, std$name)
        dir.create(path_out_collmea_std, recursive = TRUE)
        cat(path_out_collmea_std, "\n")

        # Compute std factors
        std_f <- std$factors(x)
        y <- std$standardize(std_f, x)

        res <- foreach(trial = 1:.TRIALS, .packages = "ircor", .combine = function(a,b) {
          l <- list()
          for(name in names(a)){
            l[[name]] <- rbind(a[[name]], b[[name]])
          }
          l
        }) %dopar% {
          set.seed(batch*.TRIALS + trial)
          # sample a subset of n topics
          i <- sample(1:n_t, n)
          x_trial <- x[i,]
          y_trial <- y[i,]

          within_collection(x_trial, y_trial)
        }

        for(name in names(res))
          export(round(res[[name]], .SIGNIF), file.path(path_out_collmea_std, paste0(name, ".csv")),
                 append = TRUE)
      }
    }
  }
}

stopImplicitCluster()

# Reduce -------------------------------------------------------------------------------------------

path_in <- path_out
path_out <- "out/01-within"

for(collection in .COLLECTIONS) {
  for(measure in .MEASURES) {
    collmea <- paste0(collection, "_", measure)
    path_in_collmea <- file.path(path_in, collmea)
    path_out_collmea <- file.path(path_out, collmea)
    dir.create(path_out_collmea, recursive = TRUE)

    r <- sapply(stds, function(std) {
      import(file.path(path_in_collmea, std$name, "tau.csv"))[,1]
    })
    export(r, file.path(path_out_collmea, "tau.csv"))

    r <- sapply(stds, function(std) {
      import(file.path(path_in_collmea, std$name, "tauAP.csv"))[,1]
    })
    export(r, file.path(path_out_collmea, "tauAP.csv"))

    r <- sapply(stds, function(std) {
      import(file.path(path_in_collmea, std$name, "r.csv"))[,1]
    })
    export(r, file.path(path_out_collmea, "r.csv"))

    r <- sapply(stds, function(std) {
      r <- import(file.path(path_in_collmea, std$name, "power.csv"))
      colMeans(r)
    })
    r <- cbind(alpha = .ALPHAS, r)
    export(r, file.path(path_out_collmea, "power.csv"))
  }
}
