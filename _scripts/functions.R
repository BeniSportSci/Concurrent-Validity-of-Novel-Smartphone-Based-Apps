## ---------------------------
##
## Project name: Concurrent Validity of Novel Smartphone-Based Apps Monitoring Barbell Velocity in Powerlifting Exercises
##
## Purpose of script: gather all functions used in analysis & figure scripts
##
## Author: Benedikt Mitter PhD
##
## Date Created: 2024-Jan-08
##
## Copyright (c) Benedikt Mitter, 2024
## Email: benedikt.mitter@rise-world.com
##
## ---------------------------

#### address ####

address <- function(x){
  out <- eval(parse(text = paste0(as.character(x))))
  return(out)
}


#### bayesian.R2 ####

bayesian.R2 <- function(pred, res, adjR2 = FALSE, N, k){
  require(matrixStats)
  var_pred <- matrixStats::rowVars(pred)
  var_res <- matrixStats::rowVars(res)
  R2 <- var_pred/(var_pred + var_res)
  
  if (adjR2 == TRUE){
    R2 <- 1 - ((1-R2^2)*(N-1))/(N-k-1)
  }
  
  return(R2)
}


#### cor2cov ####

cor2cov <- function(R,S){
  
  out <- diag(S) %*% R %*% diag(S)
  return(out)
  
}


#### darken ####

darken = function(color, amount = 30){
  
  out <- rep(NA, length(color))
  
  for (i in 1:length(color)){
    out[i] <- colorRampPalette(c(color[i], "black"))(100)[amount]
  }
  
  return(out)
  
}


#### diagnose.model ####

diagnose.model <- function(model, plot_pars = NA, save_as = NA, dir = NA, ex = NA, device = NA){
  
  require(rstan)
  require(posterior)
  require(ggplot2)
  require(xlsx)
  chain_diagnostics <- posterior::summarise_draws(model, "rhat", "ess_bulk", "ess_tail")
  Rhat_max <- chain_diagnostics[which.max(chain_diagnostics$rhat),]
  ess_bulk_min <- chain_diagnostics[which.min(chain_diagnostics$ess_bulk),]
  ess_tail_min <- chain_diagnostics[which.min(chain_diagnostics$ess_tail),]
  diagnostic_table <- data.frame(diagnostic = c("max Rhat", 
                                                "min bluk ess", 
                                                "min tail ess"), 
                                 parameter = c(unlist(Rhat_max[1]),
                                               unlist(ess_bulk_min[1]),
                                               unlist(ess_tail_min[1])),
                                 value = c(unlist(Rhat_max[2]),
                                           unlist(ess_bulk_min[3]),
                                           unlist(ess_tail_min[4]))
                                 )
  
  divergences <- get_num_divergent(model)
  bfmi <- if (length(get_low_bfmi_chains(model)) == 0) {0} else {get_low_bfmi_chains(model)}
  divergence_table <- data.frame(diagnostic = c("divergences", 
                                                "BFMI"),
                                 value = c(divergences,
                                           bfmi)
    
  )
  
  if (!is.na(save_as) & !is.na(dir)){
    chain_diagnostics <- na.omit(chain_diagnostics)
    chain_diagnostics <- chain_diagnostics[order(-chain_diagnostics$ess_bulk),]
    if (!dir.exists(dir) & dir.exists(getwd())){
      dir.create(dir, recursive = TRUE)
    }
    path <- paste0(dir, "/", save_as, ".xlsx")
    xlsx::write.xlsx(chain_diagnostics, file = path, sheetName = "Full Table", col.names = TRUE)
    xlsx::write.xlsx(diagnostic_table, file = path, sheetName = "Table Summary", append = TRUE, col.names = TRUE)
    xlsx::write.xlsx(divergence_table, file = path, sheetName = "Divergences", append = TRUE, col.names = TRUE)
    
    traceplot <- rstan::traceplot(fit, par = plot_pars, nrow = length(plot_pars), inc_warmup = FALSE)
    ggsave(filename = paste0(dir, "/", save_as, ".png"), plot = traceplot, device="png", dpi = 1200
           , bg ="white"
    )
  }
  
  print(divergence_table)
  print(diagnostic_table)
  
  if (divergences > 0){
    print(paste0("Attention! ", ex, "_", device, " resulted in ", divergences, " divergences."))
  }
  
}


#### posterior.summary ####

posterior.summary <- function(x, point = "mean", uncertainty = "hdi", ci = 0.95, digits = 2, correct = 0){
  
  if (!require(bayestestR)) install.packages('bayestestR')
  library(bayestestR)
  
  if (point == "mean"){
    pe <- mean(x)
  } else if (point == "median"){
    pe <- median(x)
  } else if (point == "map"){
    pe <- map_estimate(x)
  }
  
  pe <- format(round(pe, digits), nsmall = digits)[1]
  
  if (uncertainty == "hdi"){
    interval <- round(hdi(x, ci = ci), digits)
    interval <- paste0("[", format(interval[[2-correct]], nsmall = digits), ", ", format(interval[[3-correct]], nsmall = digits), "]")
  } else if (point == "ci"){
    interval <- round(quantile(x, probs = c((1-ci)/2, (1+ci)/2)), digits)
    interval <- paste0("[", format(interval[1], nsmall = digits), ", ", format(interval[2], nsmall = digits), "]")
  } 
  
  
  output <- paste0(pe, " ", interval)
  
  return(output)
}


#### r.na.stan ####

r.na.stan <- function(data, exp.len = nrow(data), rep.val = 999){
  
  require(dplyr)
  
  out <- list()
  
  d <- data %>%
    as.matrix() %>%
    unname() 
  
  if (sum(complete.cases(d)) == 0){
    d <- matrix(NA, nrow(d), ncol(d))
    out$complete <- 0
  } else if (sum(complete.cases(d)) == 1){
    d <- d[complete.cases(d), ]
    out$complete <- 1
  } else {
    d <- d[complete.cases(d), ]
    out$complete <- d %>%
      nrow()
  }
    
  if ((out$complete < exp.len) & (out$complete != 0)){
    d <- rbind(d, matrix(rep.val, exp.len - out$complete, ncol(data)))
  }
  if (is.na(rep.val) == FALSE){
    d <- d %>%
      replace(is.na(d), rep.val)
  }
    out$data <- d
  
  return(out)
  
}


#### rmse ####

rmse <- function(xPred, x, na.rm = TRUE){
  
  df <- data.frame(xPred = as.vector(xPred), x = as.vector(x))
  if (na.rm == TRUE){
    df <- df[complete.cases(df), ]
  }
  N <- nrow(df)
  
  out <- sqrt(sum((df$xPred - df$x)^2) / N)
  
  return(out)
  
}


#### run.model ####

run.model <- function(model, datalist, nChains = 4, nIter = 4000, nWarmup = floor(nIter/2), nThin = 1, adapt_delta = 0.8, init = "random", seed = NA, save_as = NA){
  
  require(rstan)
  
  # Stan setup
  rstan_options(auto_write = TRUE)
  if (parallel::detectCores() > nChains){
    options(mc.cores = nChains)
  } else {
    options(mc.cores = (parallel::detectCores()-1))
  }
  
  # clean object names
  if (exists("fit.tmp")){
    rm(fit)
  }
  
  # Stan import
  cat("Estimating", model, "model... \n")
  startTime = Sys.time()
  cat("Calling", nChains, "simulations in Stan... \n")
  
  fit.tmp = rstan::stan(model, 
             data    = datalist, 
             chains  = nChains,
             iter    = nIter,
             warmup  = nWarmup,
             thin    = nThin,
             control = list(adapt_delta = adapt_delta),
             init    = init,
             seed    = seed
  )
  
  cat("Finishing", model, "model simulation ... \n")
  endTime = Sys.time()
  cat("It took",as.character.Date(endTime - startTime), "\n")
  
  return(fit.tmp)
  
  # safe model
  if (!is.na(save_as)){
    save(fit.tmp, file = paste0(save_as))
  }

}


#### scale.format ####

scale.format <- function(x) sprintf("%.2f", x)