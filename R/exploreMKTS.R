
# exploreMKTS FUNCTION 

# This function summarizes the trend analysis for an indicator
# - Compute the n and % of pixels with tau positive, negative and 0. 
# - Compute the n and % of pixels with tau pos and significative
# - Compute the n and % of pixels with tau neg and significative
# The function' arguments are: 
# * x is a dataframe (an output of mannKen function from library(wq))
# * alpha value (significance treshold)

# version 1 
# Author: Perez-Luque AJ (@ajpelu)
# Date: 2015 Feb 

exploreMKTS <- function(x, alpha){
  # Evaluate default value alpha
  if(missing(alpha)){
    alpha <- 0.05
    alpha }
  
  # n 
  n <- nrow(x)
  
  # filter NA (RPerez) 
  x <- x[which(!is.na(x$tau)),]
  
  # Positive TAU ------------------------------
  # pixels with positive tau
  tau_pos <- x[x$tau >0,]
  n_tau_pos <- nrow(tau_pos) 
  # % pixels with positive tau
  pct_tau_pos <- round((n_tau_pos / n)*100,2)
  
  # tau pos and sig
  tpsig <- tau_pos[ tau_pos$p_value < alpha , ]
  n_tau_pos_sig <- nrow(tpsig)
  pct_tau_pos_sig <- round((n_tau_pos_sig / n_tau_pos)*100,2)
  # -------------------------------------------
  
  # Negative TAU ------------------------------
  # pixels with negative tau
  tau_neg <- x[x$tau <0,]
  n_tau_neg <- nrow(tau_neg) 
  # % pixels with negative tau
  pct_tau_neg <- round((n_tau_neg / n)*100,2)
  
  # tau neg and sig
  tnsig <- tau_neg[ tau_neg$p_value < alpha , ]
  n_tau_neg_sig <- nrow(tnsig)
  pct_tau_neg_sig <- round((n_tau_neg_sig / n_tau_neg)*100,2)
  # -------------------------------------------
  
  # TAU == 0 ----------------------------------
  tau_0 <- x[x$tau ==0,]
  n_tau_0 <- nrow(tau_0) 
  # % pixels with tau 0
  pct_tau_0 <- round((n_tau_0 / n)*100,2)
  
  # OUTPUT ------------------------------------
  summary <- as.data.frame(rbind(
    c('tau_pos',n_tau_pos,pct_tau_pos),
    c('tau_pos_sig',n_tau_pos_sig, pct_tau_pos_sig),
    c('tau_neg', n_tau_neg, pct_tau_neg),
    c('tau_neg_sig', n_tau_neg_sig, pct_tau_neg_sig),
    c('tau_0', n_tau_0, pct_tau_0)))
  names(summary)<- c('variable','n_pixel','pct_pixel')
  
  list(summary=summary, tau_pos=tau_pos, tau_neg=tau_neg)
}   
