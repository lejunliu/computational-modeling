RunSoftMax <- function(action_values, beta=50) {
  
  ### Runs softmax choice function and returns ###
  num_1 <- exp(beta*action_values[1])
  num_2 <- exp(beta*action_values[2])
  num_3 <- exp(beta*action_values[3])
  denom <- sum(exp(beta*action_values[1]), exp(beta*action_values[2]), exp(beta*action_values[3]))
  
  prob_val1 <- num_1/denom
  prob_val2 <- num_2/denom
  prob_val3 <- num_3/denom
  
list(prob_val1, prob_val2, prob_val3)  
}

RunLSESoftmax <- function(action_values, # vector to apply softmax to 
                          debug_info, 
                          verbose=0, # for printouts
                          beta=50,
                          identifier='default' # optional ident for debugging on spec SM calls
) {
  
  ### Performs softmax using log sum exp trick. Returns softmax(vec) ###
  # Pull out the index of the max term, breaking ties by just taking first
  beta <- as.numeric(beta)
  mi <- which(action_values == max(action_values))[1]
  
  # Demonominator (constant)
  term2 <- beta * action_values[mi] + # max term 
    log(sum(exp(beta * action_values - beta * action_values[mi])))
  
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(action_values))
  # Numerators
  term1_vec <- beta * action_values
  
  # Calc softmax for each elem 
  for (i in seq_along(action_values)) sm_vec[i] <- term1_vec[i] - term2
  
  # Break likelihood in case of over/underflow
  if (any(sm_vec > 0)) sm_vec <- rep(NA, 3)
  if (verbose) cat('\n Log liks', sm_vec)
  
exp(sm_vec) # Return as probabilities  
}

LearnByRLStandard <- function(reward, stim, a, Q_mat_rl, alpha, debug_info) {
  ### Calculate RPE and update Q value matrix ###
  
  rpe <- (reward - Q_mat_rl[stim, a])
  Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe
  
list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
}

LearnByRLCooperative <- function(reward, stim, a, Q_mat_rl, Q_wm_sa, alpha, debug_info, eta=.5) {
  ### Cooperative RLWM model where the expectation is jointly formed by the Q_RL(s,a) and Q_WM(s,a) ###
  
  rl_contribution <- (1-eta)*Q_mat_rl[stim, a]
  wm_contribution <- eta*Q_wm_sa
  joint_expectation <- rl_contribution + wm_contribution
  
  rpe <- (reward - joint_expectation)
  #browser(expr=abs(rpe) > 1)
  Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe
  
  # if (rpe < 0) {
  #   cat("\n\nRL contribution", rl_contribution)
  #   cat("\nQ WM", wm_contribution)
  #   cat("\nJoint expectation", joint_expectation)
  #   cat("\nRPE", rpe)
  # }
  
list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
}

LearnByRLCooperativeAltModel <- function(reward, stim, a, Q_mat_rl, Q_joint_sa, alpha, debug_info, eta=.5) {
  ### Cooperative RLWM model for Model 2 where joint Q value is explicitly computed ###
  
  rpe <- (reward - Q_joint_sa)
  Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe

  if (rpe < 0) {
    cat("\n\nJoint q val", Q_joint_sa)
    cat("\nRPE", rpe)
  }
  
list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
}
