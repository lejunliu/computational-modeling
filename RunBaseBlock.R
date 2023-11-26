RunBaseBlock <- function(parameters, block, helpers, kappa=3) {
  ### Run through one block of RLWM Base Model ###
  
  # Unpack lists by assigning to function environment.. # 
  list2env(helpers, environment())
  if (sim_opt=="opt") { # Hack to allow optim to work 
    pars <- list() 
    for (i in 1:length(parameter_names)) pars[[parameter_names[[i]]]] <- parameters[i]
    parameters <- pars  
  }
  list2env(parameters, environment())
  
  # Block contingencies and data structure #
  these_stim <- block$seq
  these_ca <- block$actionseq # correct action 
  n_s <- max(these_stim)
  if (sim_opt == "opt") {
    these_rewards <- as.numeric(block$rewards)
    these_actions <- as.numeric(block$actions)
  }
  # Structure for storing where we are so can debug model functions at specific points 
  bl <- unique(block$block)
  if (turn_on_debug_info) {
    debug_info <- list()
    debug_info[["block"]] <- bl
    debug_info[["set_size"]] <- n_s  
  } else {
    debug_info <- NULL
  }
  
  testit::assert("Must loop through one block at a time", length(bl)==1)
  Q_mat_rl <- matrix(1/3, n_s, 3)
  Q_mat_wm <- matrix(1/3, n_s, 3)
  Q_mat_wm_init_values <- matrix(1/3, n_s, 3)
  
  # Preallocate storers #
  actions <- rep(NA, length(these_stim))
  corrects <- rep(NA, length(these_stim))
  rewards <- rep(NA, length(these_stim))
  rpes <- rep(NA, length(these_stim))
  rpe_grp <- rep("zero", length(these_stim))
  neg_log_liks <- rep(NA, length(these_stim)) # For optimization
  if (store_final_q_values && sim_opt=="sim") {
    Q_mat_RL_array <- array(dim=c(length(Q_mat_rl), length(these_stim)))
    Q_mat_WM_array <- array(dim=c(length(Q_mat_wm), length(these_stim)))
  }
  for (tib in 1:nrow(block)) {
    if (turn_on_debug_info) debug_info[["trial"]] <- tib
    # Pull trial-level information 
    stim <- these_stim[tib]
    correct_action <- these_stim[tib] 
    
    # CHOICE: Get the action values from RL and WM this trial.. # 
    rl_values <- Q_mat_rl[stim, ]
    wm_values <- Q_mat_wm[stim, ]
    
    # .. pass each through softmax .. (beta currently hard coded as 50)
    # Check this **
    rl_liks <- unlist(RunLSESoftmax(rl_values), debug_info)
    wm_liks <- unlist(RunLSESoftmax(wm_values), debug_info)
    # rl_liks <- unlist(RunSoftMax(rl_values), debug_info)
    # wm_liks <- unlist(RunSoftMax(wm_values), debug_info)
    # .. weight WM based on capacity and set size .. # 
    # Recalculate WM weight every trial (Masters et al. 20 notation)
    #if (sim_opt=="opt") kappa <- 3 # Eventually replace with looping for empirical opt
    if (tib==1) W_wm <- rho * min(1, kappa/n_s) 
    probs <- W_wm*wm_liks + (1-W_wm)*rl_liks
    trial_nlls <- -log(probs)
    
    testit::assert("Probabilities must sum to 1", 
                   sum(probs) > .99 && sum(probs) < 1.01)
    
    # If simulating, simulate an action..
    if (sim_opt=="sim") {
      rand_value <- runif(1, 0, 1)
      if (rand_value < probs[1]) {
        action <- 1
      } else if (rand_value < probs[1] + probs[2]) {
        action <- 2
      } else {
        action <- 3
      }
      # ..and reward contingent on that action.
      if (action==correct_action) {
        reward <- 1
        correct <- 1
      } else{
        reward <- 0
        correct <- 0
      }
    } else { # If optimizing, just pull from data 
      reward <- these_rewards[tib]
      action <- these_actions[tib]
    }
    
    # LEARN # 
    # Reinforcement learning 
    # Learn
    rl_out <- LearnByRL(reward, stim, action, Q_mat_rl, adj_alpha, debug_info)
    Q_mat_rl <- rl_out[["Q_mat_tplus1"]]
    rpe <- rl_out[["rpe"]]
    
    # Working memory 
    if (sim_opt=="sim") {
      Q_mat_wm[stim, action] <- correct  
    } else { # May eventually need to replace this 
      Q_mat_wm[stim, action] <- reward
    }
    
    
    # Decay working memory # 
    Q_mat_wm <- Q_mat_wm + phi*(Q_mat_wm_init_values-Q_mat_wm)
    
    # STORE # 
    actions[tib] <- action
    rewards[tib] <- reward
    if (sim_opt=="sim") corrects[tib] <- correct
    rpes[tib] <- rpe
    neg_log_liks[tib] <- trial_nlls[action] # For optimization
    # Store Q value matrices so that each column corresponds to 
    # a time step. 
    if (store_all_q_values) {
      Q_mat_RL_array[, tib] <- matrix(Q_mat_rl, ncol=1)
      Q_mat_WM_array[, tib] <- matrix(Q_mat_wm, ncol=1)  
    }
    
  }
  # Code rpe group
  rpe_grp[which(rpes < 0)] <- "negative"
  rpe_grp[which(rpes > 0)] <- "positive"
  
  # NOTE that in these, and for the full-block Q values 
  # below, the Q values are stored so that the first set size/n_s
  # values correspond to the first ACTION, the second 
  # n_s to th second action, and third to third action 
  # For instance when set size = 2, the first 2 values 
  # are for action 1, the second 2 values are for action 2, 
  # and the last 2 values for action 3
  if (store_final_q_values && sim_opt=="sim") {
    final_Qmat_RL <- matrix(Q_mat_rl, ncol=1)
    final_Qmat_WM <- matrix(Q_mat_wm, ncol=1)
  } else {
    final_Qmat_RL <- NULL
    final_Qmat_WM <- NULL
  }
  
if (sim_opt=="sim") {
  out <- list(
                list(
                  "trial"=1:tib,
                  "set_size"=n_s,
                  "actions"=actions, 
                  "rewards"=rewards, 
                  "corrects"=corrects, 
                  "rpes"=as.numeric(rpes), 
                  "rpe_grp"=rpe_grp  
                ),
                list(
                  "final_Qmat_RL"=final_Qmat_RL,
                  "final_Qmat_WM"=final_Qmat_WM  
                )
              )  
} else {
  # If optimization, output negative log likelihood so can
  # minimize this 
  out <- sum(neg_log_liks) 
}
out
}

