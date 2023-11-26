q_values <- c(0, 0)
t <- 1
for (t in 1:trials) {
  
  # Choose via the softmax function. 
  choice_probs <- RunSoftMax(q_values, beta)
  
  # Simulate an action probability...  
  random_action_prob <- runif(n=1, min=0, max=1)
  
  # ... and use it to simulate an action (choice of bandit)
  if (choice_probs[1] > random_action_prob) {
    action <- 1
    simmed_actions[t] <- action
    simmed_corrects[t] <- 0 # Womp, chose the non-optimal bandit (p(rew)=.2) 
  } else {
    action <- 2
    simmed_actions[t] <- action
    simmed_corrects[t] <- 1 # Nice, chose the optimal bandit (p(rew)=.8)
  }
  
  # Find out whether or not we get reward. 
  random_reward_prob <- runif(n=1, min=0, max=1)
  
  if (bandit_probs[action] > random_reward_prob) {
    reward <- 1
    simmed_rewards[t] <- reward
  } else {
    reward <- 0
    simmed_rewards[t] <- reward
  }
  
  # Learn from the experience so we can improve!  
  q_values <- RunRescorlaWagner(q_values, alpha, action, reward)
  
  # Onward to trial t+1! (unless it's t=1000)
}