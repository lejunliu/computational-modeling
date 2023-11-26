RunRescorlaWagner <- function(values, alpha, action, reward) {
  ### Update the value of the chosen action value after reward  ####
  
  # Wilson-Collins Box 1, Equation rew
  values[action] <- values[action] + alpha * (reward - values[action])
  
# Send out the updated values 
values
}