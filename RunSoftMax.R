RunSoftMax <- function(values, beta) {
  ###  Take in a vector of two values and return a vector of choice probabilities ###
  
  # Create the numerator for values 1 and 2  
  numerator_1 <- exp(beta*values[1])
  numerator_2 <- exp(beta*values[2])
  
  # Create the denominator so we can normalize and get a choice prob \in [0, 1]
  denom <-  sum(exp(beta*values[1]), exp(beta*values[2]))

# Return vector of probabilities by normalizing each value by the denominator. 
c(numerator_1/denom, 
  numerator_2/denom)
}