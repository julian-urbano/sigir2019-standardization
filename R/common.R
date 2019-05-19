.CORES <- parallel::detectCores()

.COLLECTIONS <- c("robust2004", "terabyte2006")
.MEASURES <- c("ap", "ndcg")

.ALPHAS <- c(1:9*1e-3, 1:9*1e-2, .1) # significance levels
.N_TOPICS <- 50 # how many topics to sample
# Run in .BATCHES batches of .TRIALS trials each.
# Running only one batch takes too long because data have different structures.
.BATCHES <- 10
.TRIALS <- 500
.SIGNIF <- 6 # How many decimal digits to save in output files.

# Plots ############################################################################################

my.dev.width <- function(num=1){
  return(16 / num)
}
my.dev.par <- function(mar = 0, mgp = 0, ...){
  par(mar = c(2.5,2.5,1.8,0.8) + mar, mgp = c(1.6,.6,0) + mgp, ...)
}
my.dev.abline <- function(col="darkgrey", lwd=1, lty=2, ...){
  abline(col=col, lwd=lwd, lty=lty, ...)
}
my.dev.set.pdf <- function() {
  .GlobalEnv$my.dev.new <- function(file, num, ratio=.82, ...){
    width <- my.dev.width(num)
    height <- width*ratio
    pdf(file=file, width=width, height=height)
    my.dev.par(...)
  }
  .GlobalEnv$my.dev.off <- function(...) { off <- capture.output(dev.off(...)) }
}
my.dev.set.win <- function() {
  .GlobalEnv$my.dev.new <- function(file, num, ratio=.82, ...){
    width <- my.dev.width(num)
    height <- width*ratio
    #dev.new(width=width, height=height)
    my.dev.par(...)
  }
  .GlobalEnv$my.dev.off <- function(){}
}
my.dev.set.pdf()
#my.dev.set.win()
my.axis <- function(side, at, labels, ...) {
  if(missing(labels))
    labels <- as.character(at)
  for(i in seq_along(at))
    axis(side = side, at = at[i], labels = labels[i], tick = FALSE, ...)
  axis(side = side, at = at, labels = NA, ...)
}
