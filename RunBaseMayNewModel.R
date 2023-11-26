RunBaseMayNewModel <- function(parameters, block, helpers, prior_Q_RL_values=NULL, eta=.5) {
  ### Second of new sims from May 2023 — runs through one block of task and applies the new RLWM 
  # model with distinct discrete kapppa pun and kappa rew terms ###
  
  # Unpack lists by assigning to function environment.. # 
  list2env(helpers, environment())
  if (sim_opt=="opt") { # Hack to allow optim to work 
    pars <- list() 
    for (i in 1:length(parameter_names)) pars[[parameter_names[[i]]]] <- parameters[i]
    parameters <- pars  
  }
  
  list2env(parameters, environment())
  block_condition <- unique(block$block_type)
  
  reward_contingencies <- c(-1, 0, 1)
  
  # Block contingencies and data structure #
  these_stim <- as.numeric(block$within_ss_id)
  these_ca <- as.numeric(block$best_key) # correct action 
  these_ma <- as.numeric(block$middle_key) # middle action 
  
  n_s <- max(these_stim)
  
  if (sim_opt == "opt") {
    these_rewards <- as.numeric(block$rewards)
    these_actions <- as.numeric(block$actions)
  }
  
  # Structure for storing where we are so can debug model functions at specific points 
  bl <- unique(block$block)
  if (turn_on_debug_info) {
    #si <- block$stim_iter # For debugging 
    debug_info <- list()
    debug_info[["block"]] <- bl
    debug_info[["set_size"]] <- n_s  
  } else {
    debug_info <- NULL
  }
  
  Q_init <- 1/3
  testit::assert("Must loop through one block at a time", length(bl)==1)
  Q_mat_rl <- matrix(1/3, n_s, 3)
  Q_mat_wm <- matrix(1/3, n_s, 3)
  Q_mat_wm_init_values <- matrix(1/3, n_s, 3)
  
  # Preallocate storers #
  actions <- rep(NA, length(these_stim))
  corrects <- rep(NA, length(these_stim))
  picked_middles <- rep(NA, length(these_stim))
  picked_worsts <- rep(NA, length(these_stim))
  rewards <- rep(NA, length(these_stim))
  rpes <- rep(NA, length(these_stim))
  rpe_grp <- rep("zero", length(these_stim))
  neg_log_liks <- rep(NA, length(these_stim)) # For optimization
  if (store_final_q_values && sim_opt=="sim") {
    Q_mat_RL_array <- array(dim=c(length(Q_mat_rl), length(these_stim)))
    Q_mat_WM_array <- array(dim=c(length(Q_mat_wm), length(these_stim)))
  }
  
  # For the task type with repeats, if we're in phase 2, 
  # restore the Q_RL values from the prior phase 
  if ("phase" %in% names(block)) { 
    
    if (block$phase[1]==2) {
      
      images_this_block <- unique(block$image_idx)
      
      # Loop through stim in this block  
      for (st in seq_along(images_this_block)) {
        # Pull from the Q value matrix created after phase 1
        this_stim_info <- prior_Q_RL_values %>% 
          filter(image_idx == images_this_block[st])
        stim_task_id <- this_stim_info$image_idx
        stim_row <- this_stim_info$within_block_idx
        bk_Qrl <- this_stim_info$best_key_Q_rl_value
        mk_Qrl <- this_stim_info$middle_key_Q_rl_value
        wk_Qrl <- this_stim_info$worst_key_Q_rl_value
        # Pull the info about this stimulus from the current block for indexing (redundant, so just need first row)
        this_image_info <- block[block$image_idx == stim_task_id, 
                                 c("worst_key", "middle_key", "best_key")][1, ]
        # And recreate the Q_mat_rl  # Note: an easier way to do this that would probably help with speed is storing the end stage q values with a pointer to the unique stims in the block and then just pullig that matrix. This works for now but may want to redo 
        Q_mat_rl[stim_row, this_image_info$best_key] <- bk_Qrl
        Q_mat_rl[stim_row, this_image_info$middle_key] <- mk_Qrl
        Q_mat_rl[stim_row, this_image_info$worst_key] <- wk_Qrl
      }
    }
    
  }
  
  for (tib in 1:nrow(block)) {
    
    if (turn_on_debug_info) debug_info[["trial"]] <- tib
    # Pull trial-level information 
    stim <- these_stim[tib]
    correct_action <- these_ca[tib] #these_stim[tib] 
    middle_action <- these_ma[tib]
    
    # Change to standard model: 
    # - Discrete kappa_{pun} and kappa_{rew}
    # - Operates on whole Q_mat_wm — prioritizing the kappa highest absolute items 
    # For instance, if kappa_{pun}=3 and kappa_{rew}=2, it will prioritize the 3 
    # highest-absolute value items lower than the Q_init (no info), and 2 higher than the initial value.
    # This operates on the whole matrix of Q_mat_wm, and prioritizes based on magnitude. The others go 
    # to Q_init 
    
    ## DELETE THIS NOTE?: Doesn't really seem to be saying anything 
    # Note that we are modeling the number of Q_{sm}(s,a)'s the person can keep in mind, with the 
    # assumption of prioritization by magnitude — in higher set sizes, there will already be more 
    # s,a's that move away from 0 (in this variant of the task, where there are 2 non-zero s,a's). 
    # Thus, unlike the standard model, we don't need to divide by the set size, because the limitations 
    # of higher set size is already built in 
    ## ^DELETE?
    
    pun_inds <- which(Q_mat_wm < Q_init)
    rew_inds <- which(Q_mat_wm > Q_init)
    
    neg_values <- Q_mat_wm[pun_inds]
    
    # If the length is lower than capacity, set some to the initial value ie. no information 
    if (length(neg_values) > kappa_pun) {
      
      sorted_neg_values <- sort(neg_values)
      neg_values_to_set_to_init <- setdiff(sorted_neg_values, sort(neg_values)[1:kappa_pun])
      
      neg_indices_to_set_to_init <- unlist(lapply(neg_values_to_set_to_init, function(x) which(x==Q_mat_wm)))
      # Set these to init value in matrix 
      for (i in 1:length(neg_indices_to_set_to_init)) {
        Q_mat_wm[neg_indices_to_set_to_init[i]] <- Q_init
      }
    } else {
      #browser(expr=tib>10)
    }
    
    pos_values <- Q_mat_wm[rew_inds]
    
    if (length(pos_values) > kappa_rew) {
      
      sorted_pos_values <- sort(pos_values, decreasing=TRUE)
      pos_values_to_set_to_init <- setdiff(sorted_pos_values, sort(pos_values, decreasing=TRUE)[1:kappa_rew])
      
      
      pos_indices_to_set_to_init <- unlist(lapply(pos_values_to_set_to_init, function(x) which(x==Q_mat_wm)))
      # Set these to init value in matrix 
      for (j in 1:length(pos_indices_to_set_to_init)) {
        Q_mat_wm[pos_indices_to_set_to_init[j]] <- Q_init
      }
      
    }
    
    # CHOICE: Get the action values from RL and WM this trial.. # 
    rl_values <- Q_mat_rl[stim, ]
    wm_values <- Q_mat_wm[stim, ]
    
    ## Create joint Q-values based on RL and WM (we'll only use this for softmax
    # and forming the joint expection for calculating RPE, so we only need 
    # values relative to the stim ie. Q-joint is not a persisting variable) 
    q_joint_ls <- foreach(i = 1:3) %do% {
      if (wm_values[i] != Q_init) {
        this_Q_joint <- rho * wm_values[i] + (1-rho) * rl_values[i]
      } else {
        this_Q_joint <- rl_values[i]
      }
      this_Q_joint
    }
    q_joint_values <- unlist(q_joint_ls)
    
    probs <- unlist(RunLSESoftmax(q_joint_values, debug_info, beta=beta))
    
    trial_nlls <- -log(probs)
    
    testit::assert("Probabilities must sum to 1", 
                   sum(probs) > .99 && sum(probs) < 1.01)
    
    #debug_info$middle_pick <- 0
    
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
      # .. and reward conditional on that action 
      if (action == correct_action) {
        reward <- reward_contingencies[3]
        correct <- 1
        picked_middle <- 0
        picked_worst <- 0
      } else if (action==middle_action) {
        reward <- reward_contingencies[2]
        correct <- 0
        picked_middle <- 1
        picked_worst <- 0
        debug_info$middle_pick <- 1
      } else {
        reward <- reward_contingencies[1]
        correct <- 0
        picked_middle <- 0
        picked_worst <- 1
      }
    } else { # If optimizing, just pull from data 
      reward <- these_rewards[tib]
      action <- these_actions[tib]
    }
    
    # LEARN # 
    # Reinforcement learning 
    # Learn
    Q_joint <- q_joint_values[action]
    rl_out <- LearnByRLCooperativeAltModel(reward, stim, action, Q_mat_rl, Q_joint, alpha, debug_info, eta)
    
    Q_mat_rl <- rl_out[["Q_mat_tplus1"]]
    rpe <- rl_out[["rpe"]]
    
    # Working memory 
    if (sim_opt=="sim") {
      Q_mat_wm[stim, action] <- reward # NOTE CHANGE   
    } else { 
      Q_mat_wm[stim, action] <- reward
    }
    
    #print(Q_mat_wm)
    # Decay working memory # 
    Q_mat_wm <- Q_mat_wm + phi*(Q_mat_wm_init_values-Q_mat_wm)
    
    # STORE # 
    actions[tib] <- action
    rewards[tib] <- reward
    if (sim_opt=="sim") {
      corrects[tib] <- correct
      picked_middles[tib] <- picked_middle
      picked_worsts[tib] <- picked_worst
    }
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
  
  # n iterations with this stimulus
  stim_iter <- sapply(1:length(these_stim), function(i) sum(these_stim[i] == these_stim[1:i]))
  
  if ("phase" %in% names(block)) { 
    if (block$phase[1]==2) {
      stim_iter <- stim_iter + 5
    }
  }
  
  ## Store the final Q-values for the best, middle, and worst action, for each stimulus, along with how
  # often each of the keys was picked ##
  # The Q-mat is ordered by within_ss_id, which is the within-block index identifying different
  # stimuli, although these are arbitrary 
  bdt <- data.table(block) # For speed 
  bdt$actions <- actions
  
  # Loop through the Q_mats for each stim  
  value_actionfreq_summarizer <- foreach(r = 1:n_s) %do% { 
    # Get the row of Q_rl values for this stim 
    this_stim_data <- bdt[bdt$within_ss_id==r, ]
    
    # Get the best middle and worst keys for this stim 
    bk <- unique(this_stim_data$best_key)
    mk <- unique(this_stim_data$middle_key)
    wk <- unique(this_stim_data$worst_key)
    
    # Summarize the actions for this stim 
    this_stim_actions <- table(this_stim_data$actions)
    
    best_key_table_index <- which(names(this_stim_actions)==as.character(bk))
    middle_key_table_index <- which(names(this_stim_actions)==as.character(mk))
    worst_key_table_index <- which(names(this_stim_actions)==as.character(wk))
    
    if (length(best_key_table_index)) {
      best_key_frequency <- as.numeric(this_stim_actions[best_key_table_index])
    } else {
      best_key_frequency <- 0
    }
    
    if (length(middle_key_table_index)) {
      middle_key_frequency <- as.numeric(this_stim_actions[middle_key_table_index])
    } else {
      middle_key_frequency <- 0
    }
    
    if (length(worst_key_table_index)) {
      worst_key_frequency <- as.numeric(this_stim_actions[worst_key_table_index])
    } else {
      worst_key_frequency <- 0
    }
    
    this_Qrl_row <- Q_mat_rl[r, ]
    
    as_out <- data.table(
                          # This gives the task-wide stimulus ID. Used to associate stim to their 
                          # RL q-values in phase 
                          this_stim_data$image_idx[1],
                          # This gives the idx within the block. This is needed in phase 2
                          # to put the Q_RL values into the appropriate row  
                          this_stim_data$within_ss_id[1],
                          # Q values 
                          this_Qrl_row[bk], this_Qrl_row[mk], this_Qrl_row[wk], 
                          # Frequencies of selecting them  
                          best_key_frequency, middle_key_frequency, worst_key_frequency)

  as_out
  } %>% bind_rows() %>% setNames(c("image_idx", "within_block_idx",
                                   "best_key_Q_rl_value", "middle_key_Q_rl_value", "worst_key_Q_rl_value", 
                                   "best_key_n_times_selected", "middle_key_n_times_selected", "worst_key_n_times_selected"))
  
  value_actionfreq_summarizer$block <- unique(block$block)
  value_actionfreq_summarizer$set_size <- n_s
  
if (sim_opt=="sim") {
  out <- 
    list(
    "sim_trial_wise_data"=data.frame(
      "trial"=1:tib,
      "set_size"=n_s,
      "actions"=actions,
      "rewards"=rewards,
      "corrects"=corrects,
      "stim_iter"=stim_iter,
      "picked_middle"=picked_middles,
      "picked_worst"=picked_worsts,
      "rpes"=as.numeric(rpes),
      "rpe_grp"=rpe_grp,
      "block_condition"=block_condition,
      "alpha"=alpha,
      "rho"=rho,
      "phi"=phi#,
      #"kappa"=kappa
      ),
      "Qrl_vals_and_action_freqs"=value_actionfreq_summarizer
    )
  
} else {
  # If optimization, output negative log likelihood so can
  # minimize this 
  out <- sum(neg_log_liks) 
}
out
}
## For testing phase 1 
#tmp <- lapply(split(task_df, task_df$block), function(x) RunBase0BlockAltTask(parameters, x, helpers)) 

## For testing phase 2 
# task_df_2 <- CreateTaskDf2() # New task type with repeats of the same stim in different phases 
# task_df_2_p1 <- task_df_2 %>% filter(phase==1)
# 
# ## Do the first phase 
# p1_out <- lapply(split(task_df_2_p1, task_df_2_p1$block), function(x) RunBase0BlockAltTask(parameters, x, helpers))
# # Get the Q_rl vals from phase 1
# Q_vals_sum <- lapply(p1_out, function(x) {
#   x$Qrl_vals_and_action_freqs
# }) %>% bind_rows()
# 
# ## Do the 2nd phase, which has access to the Q_rl vals, but where WM repr's have decayed 
# task_df_2_p2 <- task_df_2 %>% filter(phase==2)
# p2_out <-
#   lapply(split(task_df_2_p2, task_df_2_p2$block), function(x) RunBase0BlockAltTask(parameters, x, helpers,
#                                                                                    # Input the Q_RL values from block1,
#                                                                                    # as these (but not WM) are retained
#                                                                                    prior_Q_RL_values=Q_vals_sum))