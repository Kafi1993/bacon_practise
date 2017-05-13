#' inchToFeet
#' 
#' Transforms inches to feet and remaining inches
#' @author joschkacr
#' @param inches Numerical vector, an inch value to be transformed to feet and inch.
#' @export

# transforming inch to feet and a rest
inchToFeet <- function(inches){
  
  # compute feet and inch values
  inch_remain <- inches %% 12
  feet <- (inches-inch_remain)/12
  
  # combine and return data
  feet_inch <- list(feet, inch_remain)
  feet_inch
  
  return(feet_inch)
}