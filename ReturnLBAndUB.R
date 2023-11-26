ReturnLBAndUB <- function(which_model) {
  ### Return lower and upper bounds for model ###
  if (helpers$which_model=="base_model") {
    lb <- c(0, 0, 0, 0)
    ub <- c(1, 1, 1, 1)
  } else if (helpers$which_model=="base0_model") {
    lb <- c(0, 0, 0)
    ub <- c(1, 1, 1)
  }
list("lb"=lb, "ub"=ub)  
}