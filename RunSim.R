RunSim <- function(this_subj, parameters, helpers, kappa=3) {
  ### Set up to run all model blocks ###
  if (helpers$which_model=="base0_model") {
    
    block_outs <- lapply(split(this_subj, this_subj$block), function(x) {
      
      block_out <- RunBase0Block(parameters, x, helpers, kappa)
      
      # Package out the part that fits nicely in a data table
      dtout <- do.call(cbind, block_out[[1]]) 
      data.table(dtout, "block"=x$block, "seq"=x$seq, "actionseq"=x$action_seq, "stim_iter"=x$stim_iter)
      
    })
  #browser()  
  a <- 3  
  }
  
  if (helpers$which_model=="base_model") {
    
    block_outs <- lapply(split(this_subj, this_subj$block), function(x) {
      
      block_out <- RunBaseBlock(parameters, x, helpers, kappa)
      # Package out the part that fits nicely in a data table
      dtout <- do.call(cbind, block_out[[1]]) 
      
      data.table(dtout, "block"=x$block, "seq"=x$seq, "actionseq"=x$action_seq, "stim_iter"=x$stim_iter)
    })
  }

block_outs  
}





## For later
#if (helpers$store_final_q_values) {
# set_size <- max(x$seq)
# if (set_size==2) mat_out
# ** To do: Send out Q matrices   
#}