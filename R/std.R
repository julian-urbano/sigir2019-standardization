#' No standardization
#'
#' y = (x - mu) / sigma
std_raw <- list(
  name = "raw",

  factors = function(std_scores) {
    NULL
  },

  standardize = function(factors, eval_scores) {
    as.matrix(eval_scores)
  }
)

#' z-std standardization
#'
#' y = (x - mu) / sigma
std_z <- list(
  name = "z-std",

  factors = function(std_scores) {
    std_scores <- as.matrix(std_scores)

    # estimate mu and sigma for each topic
    apply(std_scores, 1, function(x) {
      mu <- mean(x)
      sigma <- sd(x)

      if(sigma == 0) { # if no variability, don't standardize
        mu <- 0
        sigma = 1
      }
      list(mu = mu, sigma = sigma)
    })
  },

  standardize = function(factors, eval_scores) {
    eval_scores <- as.matrix(eval_scores)

    # apply std factors to the eval systems
    m <- eval_scores # create a copy and update there
    for(i in 1:nrow(eval_scores)) {
      mu <- factors[[i]][["mu"]]
      sigma <- factors[[i]][["sigma"]]

      m[i,] <- (eval_scores[i,] - mu) / sigma
    }
    m
  }
)

#' N-std standardization (Webber et al)
#'
#' y = F_norm(x ; mu , sigma^2)
std_N <- list(
  name = "N-std",

  factors = function(std_scores) {
    # estimate mu and sigma for each topic
    std_z$factors(std_scores)
  },

  standardize = function(factors, eval_scores) {
    eval_scores <- as.matrix(eval_scores)

    # apply std factors to the eval systems
    m <- eval_scores # create a copy and update there
    for(i in 1:nrow(eval_scores)) {
      mu <- factors[[i]][["mu"]]
      sigma <- factors[[i]][["sigma"]]

      m[i,] <- pnorm(eval_scores[i,],
                     mean = mu,
                     sd = sigma)
    }
    m
  }
)

#' U-std standardization (Sakai)
#'
#' y = F_unif(x ; mu - sigma*B/A , mu + sigma*(1-B)/A)
std_U <- list(
  name = "U-std",

  factors = function(std_scores) {
    # estimate mu and sigma for each topic
    std_z$factors(std_scores)
  },

  standardize = function(factors, eval_scores) {
    eval_scores <- as.matrix(eval_scores)

    # apply std factors to the eval systems
    A <- .15
    B <- .5
    m <- eval_scores # create a copy and update there
    for(i in 1:nrow(eval_scores)) {
      mu <- factors[[i]][["mu"]]
      sigma <- factors[[i]][["sigma"]]

      m[i,] <- punif(eval_scores[i,],
                     min = mu - sigma*B/A,
                     max = mu + sigma*(1-B)/A)
    }
    m
  }
)

#' E-std standardization (Urbano et al)
#'
#' y = F_ecdf(x ; x_1 ... x_n)
std_E <- list(
  name = "E-std",

  factors = function(std_scores) {
    std_scores <- as.matrix(std_scores)

    # estimate F
    apply(std_scores, 1, function(x) {
      ecdf(x)
    })
  },

  standardize = function(factors, eval_scores) {
    eval_scores <- as.matrix(eval_scores)

    # apply std factors to the eval systems
    m <- eval_scores # create a copy and update there
    for(i in 1:nrow(eval_scores)) {
      m[i,] <- factors[[i]](eval_scores[i,])
    }
    m
  }
)

stds <- list("raw" = std_raw, "N-std" = std_N, "U-std" = std_U, "z-std" = std_z, "E-std" = std_E)
