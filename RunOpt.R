RunOpt <- function(this_subj, parameters, helpers, kappa=3) {
  ### Set up to run all model blocks ###
  LoopForOpt <- function(parameters, this_subj, helpers, kappa) {
    # Find negative log likelihood for all blocks #
    if (helpers$which_model=="base_model") {
    
    opt_out <-
      lapply(split(this_subj, this_subj$block), function(x) RunBaseBlock(parameters, x, helpers, kappa)) 
    
    } else if (helpers$which_model=="base0_model") {
    
    opt_out <-
        lapply(split(this_subj, this_subj$block), function(x) RunBase0Block(parameters, x, helpers, kappa)) 
    
    }
  # Return summed nll to optimizaiton function #
  sum(as.numeric(unlist(opt_out)))
  }
  
    # Lower and upper bounds 
    # Note these exclude kappa, which will be hard coded for par recov runs 
    lb_ub <- ReturnLBAndUB(helpers$which_model)
    helpers$parameter_names <- names(parameters) #[-c(which(names(parameters)=="kappa"))]
    
    params <- unlist(foreach (1:length(lb_ub$lb)) %do% runif(1, 0, 1))
    
    res <-
      optim(par = params,
            fn = function(params) { LoopForOpt(params, this_subj, helpers, kappa) },
            gr = NULL,
            method = c("L-BFGS-B"),
            lower = lb_ub$lb,
            upper = lb_ub$ub,
            hessian = FALSE,
            control=list(trace=0)#,
            #control=list(maxit=10000)
    )
res  
}