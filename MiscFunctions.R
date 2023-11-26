PlotWAltPars <- function(pars) {
  ### Performance plots w pars allowed to vary ###
  sample_tc <- sample(unique(df$subj), 10) # Sample 10 sets of task contingencies 
  
  sim_out <- 
    foreach (k=1:5) %do% {
      all_outs <- 
        foreach (i=1:10) %do% {
          so <- data.table(RunSim(df %>% filter(subj == sample_tc[i]), pars, helpers, kappa=k) %>% bind_rows(), 
                           "tc"=i,
                           "kappa"=k) # Track diff sim / task contingency set
          so
        } %>% bind_rows()
    } %>% bind_rows()
  
  sim_out$correct <- as.numeric(sim_out$corrects)
  sim_out$set_size <- as.numeric(sim_out$set_size)
  
  middle <- list() # For plots marginalizing over stim iter
  high <- list() # For stim iter plots 
  for (k in 1:5) {
    
    summs <- sim_out %>% filter(kappa==k) %>% group_by(set_size) %>% summarize(m=mean(correct))
    
    pcor_ss <- sim_out %>% filter(kappa==k) %>% group_by(stim_iter, set_size) %>% 
      summarize(m=mean(correct))
    
    middle[[k]] <- 
      ggplot(summs, aes(x=set_size, y=m)) + 
      geom_hline(yintercept=seq(.4, .8, .1), size=3, alpha=.5) +
      geom_bar(stat="identity", fill="white", color="black", alpha=.5) + 
      ga + ap + ylim(0, 1) + 
      ylab("proportion correct") + xlab("set size") + ggtitle(k) + tp
    
    high[[k]] <- 
      ggplot(pcor_ss, aes(x=stim_iter, y=m, group=as.factor(set_size), color=as.factor(set_size))) + 
      geom_line(size=2) + ylim(0, 1) + 
      geom_hline(yintercept=c(.6, .8), linetype="dotted") +
      geom_vline(xintercept=c(2, 5, 8, 10), linetype="dotted") +
      geom_hline(yintercept=.33, size=1.5, color="gray57") + # chance line 
      geom_point(aes(fill=as.factor(set_size)), color="black", size=6, pch=21) +
      ga + ap + lp +
      xlab("stim iteration") + ylab("proportion correct") + ggtitle(k) + tp  
  }
list(middle, high)
}

TryPR <- function(sim_dataset, true_pars, kappas_to_try, helpers, true_kappa) {
  ### Attempt par recovery with error handling ###
  
  helpers$sim_opt <- "opt"
  
  res <- 
    tryCatch(error=function(cnd) NA,
             # Try each kappa
             foreach (i=1:length(kappas_to_try)) %do% RunOpt(sim_dataset, true_pars, helpers, kappas_to_try[i])
    )
  
  if (length(res) > 1) {
    nlls <- unlist(map(res, 2))
    # Of the 5 runs, which had the lowest kappa?
    win_idx <- which(nlls == min(nlls))
    d <- 
      data.table(t(c(
        # Ground truth pars 
        as.numeric(true_pars), true_kappa,
        # Recovered pars in the run with lowest nll 
        res[[win_idx]]$par,
        # "Recovered kappa" (one with lowest nll)
        kappas_to_try[win_idx],  
        # Other info 
        res[[win_idx]]$message, 
        res[[win_idx]]$value
      ))) %>% setNames(
        c(
          paste0("true_", names(true_pars)), "true_kappa",
          paste0("recovered_", names(true_pars)), "recovered_kappa",
          "message", "nll"
        ))
    error_record <- "ok"
  } else {
    d <- c()
    error_record <- "ERROR"
  }  
  
list(d, error_record)
}

