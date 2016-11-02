# 


# Fix timezones
# Not exactly precise--I'm making some strong assumptions regarding data input errors.
fixtzs <- function(tz) {
  olsons <- c(ADT = "America/Blanc-Sablon", 
              AKDT = "America/Anchorage", 
              AKST = "America/Anchorage", 
              AST = "America/Blanc-Sablon",
              CAST = "America/Chicago", 
              CDT = "America/Chicago", 
              CETDST = "America/Chicago", 
              CST = "America/Chicago", 
              DST = "America/New_York", 
              EDT = "America/New_York",
              HDT = "America/New_York", 
              MDT = "America/Denver", 
              PDT = "America/Los_Angeles",
              PST = "America/Los_Angeles")
  out <- plyr::revalue(tz, replace = olsons, warn_missing = FALSE)
  out
}

#' Least-squares slope coefficient for y ~ x
#' @param x independent variable
#' @param y dependent variable
slope <- function(x, y)
  cor(x, y) * sd(y) / sd(x)
senslope <- function(x, y) 
  zyp.sen(y ~ x, data.frame(x = x, y = y))[["coefficients"]][[2]]

#' calculate dH/dW for various width quantiles
#' 
#' Returns a single number that is the average slope for a given range of width percentiles
#' 
#' @param H stage height or eleveation
#' @param W stream width
#' @param lwr_p Fraction of widths smaller than range considered
#' @param upr_p Fraction of widths larger than range considered
#' 
dHdW <- function(H, W, lwr_p, upr_p) {
  ords <- order(W)
  H <- H[ords]
  W <- W[ords]

  minw <- quantile(W, lwr_p)
  maxw <- quantile(W, upr_p)
  
  inds <- W <= maxw & W >= minw
  out <- senslope(x = W[inds], y = H[inds])
  out
}