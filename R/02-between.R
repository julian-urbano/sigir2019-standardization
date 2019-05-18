source("R/common.R")
source("R/std.R")

library(ircor)
library(rio)
library(doParallel)
stopImplicitCluster()
registerDoParallel(cores = .CORES)

#' compute statistics between two sets of scores
between_collection <- function(y1, y2) {
  n_s <- ncol(y1) # number of systems
  means_x <- colMeans(y1)
  means_y <- colMeans(y2)

  # correlations (b for ties)
  tau <- tau_b(means_x, means_y)
  tauAP <- tauAP_b(means_x, means_y)
  r <- cor(means_x, means_y, method = "pearson")

  # unpaired t-test between same system on different collections
  type1 <- sapply(1:n_s, function(i) {
    t.test(y1[,i], y2[,i])$p.value
  })
  type1 <- ecdf(type1)(.ALPHAS)

  # unpaired t-test between every system on collection 1, with every other system on collection 2
  power <- as.vector(sapply(1:n_s, function(i) {
    sapply((1:n_s)[-i], function(j) {
      t.test(y1[,i], y2[,j])$p.value
    })
  }))
  power <- ecdf(power)(.ALPHAS)

  list(tau = unname(tau), tauAP = unname(tauAP), r = unname(r),
       type1 = unname(type1), power = unname(power))
}

# Compute ------------------------------------------------------------------------------------------

path_out <- "scratch/02-between"

for(batch in 1:.BATCHES) {
  for(collection in "terabyte2006") {
    for(measure in .MEASURES) {
      collmea <- paste0(collection, "_", measure)
      path_out_collmea <- file.path(path_out, collmea)

      x <- import(file.path("data", paste0(collmea, ".csv")))
      n_t <- nrow(x)
      n <- min(floor(n_t / 2), 50)

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
          i <- sample(1:n_t, n*2)
          y1_trial <- y[i[1:n],]
          y2_trial <- y[i[-1:-n],]

          between_collection(y1_trial, y2_trial)
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
path_out <- "out/02-between"

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
      r <- import(file.path(path_in_collmea, std$name, "type1.csv"))
      colMeans(r)
    })
    r <- cbind(alpha = .ALPHAS, r)
    export(r, file.path(path_out_collmea, "type1.csv"))

    r <- sapply(stds, function(std) {
      r <- import(file.path(path_in_collmea, std$name, "power.csv"))
      colMeans(r)
    })
    r <- cbind(alpha = .ALPHAS, r)
    export(r, file.path(path_out_collmea, "power.csv"))
  }
}
