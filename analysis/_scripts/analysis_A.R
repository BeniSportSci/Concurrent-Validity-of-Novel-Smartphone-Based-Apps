## ---------------------------
##
## Project name: Concurrent Validity of Novel Smartphone-Based Apps Monitoring Barbell Velocity in Powerlifting Exercises
##
## Purpose of script: analysis script (data structuring, modeling, calculation of statistics)
##
## Author: Benedikt Mitter PhD
##
## Date Created: 2024-Jan-08
##
## Copyright (c) Benedikt Mitter, 2024
## Email: benedikt.mitter@rise-world.com
##
## ---------------------------

#### Libraries ####
library(bayestestR)
library(mvtnorm)
library(readxl)
library(rstan)
library(tidyverse)

# funchir::stale_package_check("./_scripts/figures.R")


#### Setup ####

# Stan
rstan_options(auto_write = TRUE)
options(mc.cores = 4) # number of cores for sampling
nChains = 4
nIter = 8000
nWarmup = floor(nIter/2)

# Functions
source("./_scripts/functions.R")


#### Data import & structure ####

# import
files <- list.files(path = "./_data")
slot <- 1
data_raw <- read_xlsx(
  path = paste0("./_data/", files[slot]), 
  col_names = TRUE
)

# refactor
data_raw <- data_raw %>% 
  mutate(across(1:4, as.factor)) %>% # convert to factors
  as.data.frame() %>%
  mutate_at(grep("d_", names(data_raw)), ~./1000) # convert ROM to m

# filter
data_raw <- data_raw %>%
  select(-c(
    "exercise_3"
  )) %>%
  filter(!is.na(data_raw$v_vicL)) #get only cases with Vicon reference

# split by exercise
exercises <- c("sq", "bp", "su", "co")
for (i in 1:length(exercises)){
  
  tmp <- data_raw %>%
    filter(exercise_4 == exercises[i]) 
  
  assign(paste0("data_", exercises[i]), tmp)
  
}
data_dl <- rbind(data_co, data_su)

exercises <- c("sq",
               "bp",
               "dl")
nExercises <- length(exercises)


nDevices <- 4
devices <- data_raw %>%
  select((ncol(data_raw) - nDevices + 1):ncol(data_raw)) %>%
  colnames() %>%
  stringr::str_sub(-4,-1)


#### Fit model_A1.stan ####

pars <- c("alpha_rec", 
          "beta_rec", 
          "sigma_rec", 
          "tau_u[1]", 
          "tau_u[2]")

repCount <- list()

for (e in 1:nExercises){
  
  # extract data objects
  data <- address(paste0("data_", exercises[e]))
  Subjects <- unique(data$ID)
  nSubjects <- length(Subjects)
  nReps <- n_distinct(data$load)
  
  compReps <- matrix(0, nSubjects, nDevices, dimnames = list(1:nSubjects, devices))
  
  # create 3-dimensional data arrays
  vVic <- array(0, dim = c(nReps, nSubjects, nDevices))
  vDev <- vVic
  
  for (d in 1:nDevices){
    for (s in 1:nSubjects){
      
      tmp <- data %>%
        filter(ID == Subjects[s]) %>%
        select(paste0("v_", devices[d]), if (devices[d] == "repO"){"v_vicR"} else{"v_vicL"}) %>%
        r.na.stan(., exp.len = nReps, rep.val = 999)
      compReps[s, d] <- tmp$complete
      vDev[, s, d] <- tmp$data[, 1]
      vVic[, s, d] <- tmp$data[, 2]
    }
  }
  
  repCount[[e]] <- compReps
  
  # check whether compReps < 3 at any point
  sum(compReps < 3) > 0
  
  
  # define prior scale
  scale <- 5 
  
  for (d in 1:nDevices){
    
    # data list
    dataList <- list(
      nSubjects = nSubjects,
      nReps = nReps,
      compReps = compReps[, d],
      vVic = vVic[, , d],
      vDev = vDev[, , d],
      
      prior_a = c(0,scale),
      prior_b = c(0,scale),
      
      prior_sigma = c(0,scale),
      prior_tau1 = c(0,scale),
      prior_tau2 = c(0,scale),
      
      prior_eta = 1
    )
    
    #### Modeling ####
    set.seed(NULL)
    fit <- run.model("_scripts/model_A1.stan", datalist = dataList, nChains = nChains, nIter = nIter, nWarmup = nWarmup, adapt_delta = 0.95)
    
    # safe model for backup
    save("fit", file = paste0("./_output/analysisA1/fit_", exercises[e], "_", devices[d]))
    
    # model diagnostics
    diagnose.model(fit, plot_pars = pars, save_as = paste0("diagnostics_", exercises[e], "_", devices[d]), dir = paste0("./_output/analysisA1/", exercises[e], "/", devices[d]), ex = exercises[e], device = devices[d])
    
  }

}



#### Refit single models to optimize model diagnostics ####

# select exercises and devices that require remodeling
ex <- list(
  # sq = c()
  bp = c("myLi")
  # dl = c()
)

# choose a higher acceptance rate for remodeling (should be between 0.95 and 1, where larger rates may increase sampling time)
delta <- 0.999

exercises2 <- names(ex)
nExercises2 <- length(exercises2)

for (e in 1:nExercises2){
  
  # extract data objects
  data <- address(paste0("data_", exercises2[e]))
  Subjects <- unique(data$ID)
  nSubjects <- length(Subjects)
  nReps <- n_distinct(data$load)
  
  devices2 <- ex[[exercises2[e]]]
  nDevices2 <- length(devices2)

  # create 3-dimensional data arrays
  vVic <- array(0, dim = c(nReps, nSubjects, nDevices2))
  vDev <- vVic
  
  compReps <- matrix(0, nSubjects, nDevices2, dimnames = list(1:nSubjects, devices2))
  
  for (d in 1:nDevices2){
    for (s in 1:nSubjects){
      
      tmp <- data %>%
        filter(ID == Subjects[s]) %>%
        select(paste0("v_", devices2[d]), if (devices[d] == "repO"){"v_vicR"} else{"v_vicL"}) %>%
        r.na.stan(., exp.len = nReps, rep.val = 999)
      compReps[s, d] <- tmp$complete
      vDev[, s, d] <- tmp$data[, 1]
      vVic[, s, d] <- tmp$data[, 2]
    }
  }
  
  # define prior scale
  scale <- 5 
  
  for (d in 1:nDevices2){
    
    # data list
    dataList <- list(
      nSubjects = nSubjects,
      nReps = nReps,
      compReps = compReps[, d],
      vVic = vVic[, , d],
      vDev = vDev[, , d],
      
      prior_a = c(0,scale),
      prior_b = c(0,scale),
      
      prior_sigma = c(0,scale),
      prior_tau1 = c(0,scale),
      prior_tau2 = c(0,scale),
      
      prior_eta = 1
    )
    
    #### Modeling ####
    set.seed(NULL)
    fit <- run.model("_scripts/model_A1.stan", datalist = dataList, nChains = nChains, nIter = nIter, nWarmup = nWarmup, adapt_delta = delta)
    
    # safe model for backup
    save("fit", file = paste0("./_output/analysisA1/fit_", exercises2[e], "_", devices2[d]))
    
    # model diagnostics
    diagnose.model(fit, plot_pars = pars, save_as = paste0("diagnostics_", exercises2[e], "_", devices2[d]), dir = paste0("./_output/analysisA1/", exercises2[e], "/", devices2[d]), ex = exercises2[e], device = devices2[d])
    
  }
  
}



#### RMSE of PPDs ####

# Note: 
# RMSE was calculated from posterior predictive distributions (PPDs) to account for the following sources of uncertainty: 
#   - uncertainty due to between-subject variance (sampling uncertainty)
#   - uncertainty due to within-subject variance (sampling uncertainty)
#   - uncertainty in parameter estimation

iter <- nChains * (nIter - nWarmup)
comb <- rep(NA, nExercises * nDevices)
for (e in 1:nExercises){
  for (d in 1:nDevices){
    comb[(e - 1) * nDevices + d] <- paste0(exercises[e], "_", devices[d])
  }
}

listRMSE <- list()
# listRMSE2 <- matrix(NA, iter, nExercises * nDevices, dimnames = list(NULL, comb))

for (e in 1:nExercises){
  
  # extract data objects
  data <- address(paste0("data_", exercises[e]))
  Subjects <- unique(data$ID)
  nSubjects <- length(Subjects)
  nReps <- n_distinct(data$load)
  
  for (d in 1:nDevices){
    
    # create 3-dimensional data arrays
    vVic <- matrix(0, nReps, nSubjects)
    
    for (s in 1:nSubjects){
      
      tmp <- data %>%
        filter(ID == Subjects[s]) %>%
        select(paste0("v_", devices[d]), "v_vicL") %>%
        r.na.stan(., exp.len = nReps, rep.val = NA)
      vVic[, s] <- tmp$data[, 2]
    }
    
    file <- paste0("./_output/analysisA1/fit_", exercises[e], "_", devices[d])
    load(file = file)
    
    draws <- rstan::extract(fit, inc_warmup =FALSE)
    
    sPar <- list()
    tau <- draws$tau_u_rec
    sigma <- draws$sigma_rec
    alpha <- draws$alpha_rec
    beta <- draws$beta_rec
    
    tmp <- rep(NA, iter)
    
    for (i in 1:iter){ # accounting for uncertainty in parameter estimation
      R <- draws$Sigma_corr[i, , ]
      Omega <- cor2cov(R, tau[i, ])
      sPar[[i]] <- rmvnorm(nSubjects, c(alpha[i], beta[i]), Omega) # accounting for between-subject variance
      vDevPred <- matrix(NA, nReps, nSubjects)
      
      for (s in 1:nSubjects){
        
        for (r in 1:nReps){
          
          if (!is.na(vVic[r, s])){
            vDevPred[r, s] <- rnorm(1, (sPar[[i]][s, 1] + sPar[[i]][s, 2] * vVic[r, s]), sigma[i]) # accounting for within-subject variance
          }
          
        }
        
      }
      
      tmp[i] <- rmse(vDevPred, vVic, na.rm = TRUE)

    }
    
    assign("tmp", list(tmp))
    names(tmp) <- paste0(exercises[e], "_", devices[d])
    
    listRMSE <- c(listRMSE, tmp)
    
  }
  
  print(paste0("Completed RMSE calculation for ", exercises[e], "."))
  
}

save("listRMSE", file = "./_output/analysisA1/listRMSE")



#### Fit model_A2.stan ####

pars <- c("alpha_rec", 
          "beta_rec", 
          "sigma_rec", 
          "tau_u[1]", 
          "tau_u[2]")

for (e in 1:nExercises){
  
  # extract data objects
  data <- address(paste0("data_", exercises[e]))
  Subjects <- unique(data$ID)
  nSubjects <- length(Subjects)
  nReps <- n_distinct(data$load)
  
  for (d in 1:nDevices){
    
    compReps <- rep(0, nSubjects)
    
    # create 3-dimensional data arrays
    vel <- array(0, dim = c(nReps, nSubjects, 2))
    
    for (s in 1:nSubjects){
      
      tmp <- data %>%
        filter(ID == Subjects[s]) %>%
        select(if (devices[d] == "repO"){"v_vicR"} else{"v_vicL"}, paste0("v_", devices[d])) %>%
        r.na.stan(., exp.len = nReps, rep.val = 999)
      compReps[s] <- tmp$complete
      vel[, s, 1] <- tmp$data[, 1]
      vel[, s, 2] <- tmp$data[, 2]
      
    }
    
    tmp <- vel %>%
      as.vector() %>%
      replace(which(vel %in% 999), NA)
    
    mean_vel <- mean(tmp, na.rm = TRUE)
    
    sd_vel <- sd(tmp, na.rm = TRUE)
    
    # define prior scale
    scale <- 5 
    
    # data list
    dataList <- list(
      nSubjects = nSubjects,
      nReps = nReps,
      compReps = compReps,
      vel = vel,
      mean_vel = mean_vel,
      sd_vel = sd_vel,
      
      prior_a = c(0,scale),
      prior_b = c(0,scale),
      
      prior_sigma = c(0,scale),
      prior_tau1 = c(0,scale),
      prior_tau2 = c(0,scale),
      
      prior_eta = 1
    )
    
    # Modeling
    set.seed(NULL)
    fit <- run.model("_scripts/model_A2.stan", datalist = dataList, nIter = 8000, adapt_delta = 0.95)
    
    # safe model for backup
    save("fit", file = paste0("./_output/analysisA2/fit_", exercises[e], "_", devices[d]))
    
    # model diagnostics
    diagnose.model(fit, plot_pars = pars, save_as = paste0("diagnostics_", exercises[e], "_", devices[d]), dir = paste0("./_output/analysisA2/", exercises[e], "/", devices[d]), ex = exercises[e], device = devices[d])

  }
  
}


#### Refit single models to optimize model diagnostics ####

# select exercise
ex <- list(
  # sq = c()
  # bp = c("myLi")
  dl = c("metr")
)

delta <- 0.999

exercises2 <- names(ex)
nExercises2 <- length(exercises2)

for (e in 1:nExercises2){
  
  # extract data objects
  data <- address(paste0("data_", exercises2[e]))
  Subjects <- unique(data$ID)
  nSubjects <- length(Subjects)
  nReps <- n_distinct(data$load)
  
  devices2 <- ex[[exercises2[e]]]
  nDevices2 <- length(devices2)

  for (d in 1:nDevices2){
    
    compReps <- rep(0, nSubjects)
    
    # create 3-dimensional data arrays
    vel <- array(0, dim = c(nReps, nSubjects, 2))
    
    for (s in 1:nSubjects){
      
      tmp <- data %>%
        filter(ID == Subjects[s]) %>%
        select(if (devices2[d] == "repO"){"v_vicR"} else{"v_vicL"}, paste0("v_", devices2[d])) %>%
        r.na.stan(., exp.len = nReps, rep.val = 999)
      compReps[s] <- tmp$complete
      vel[, s, 1] <- tmp$data[, 1]
      vel[, s, 2] <- tmp$data[, 2]
      
    }
    
    tmp <- vel %>%
      as.vector() %>%
      replace(which(vel %in% 999), NA)
    
    mean_vel <- mean(tmp, na.rm = TRUE)
    
    sd_vel <- sd(tmp, na.rm = TRUE)
    
    # define prior scale
    scale <- 5 
    
    # data list
    dataList <- list(
      nSubjects = nSubjects,
      nReps = nReps,
      compReps = compReps,
      vel = vel,
      mean_vel = mean_vel,
      sd_vel = sd_vel,
      
      prior_a = c(0,scale),
      prior_b = c(0,scale),
      
      prior_sigma = c(0,scale),
      prior_tau1 = c(0,scale),
      prior_tau2 = c(0,scale),
      
      prior_eta = 1
    )
    
    # Modeling
    set.seed(NULL)
    fit <- run.model("_scripts/model_A2.stan", datalist = dataList, nIter = 8000, adapt_delta = delta)
    
    # safe model for backup
    save("fit", file = paste0("./_output/analysisA2/fit_", exercises2[e], "_", devices2[d]))
    
    # model diagnostics
    diagnose.model(fit, plot_pars = pars, save_as = paste0("diagnostics_", exercises2[e], "_", devices2[d]), dir = paste0("./_output/analysisA2/", exercises2[e], "/", devices2[d]), ex = exercises2[e], device = devices2[d])
    
  }
  
}


#### Extract SMB ####

listSMB <- list()

for (e in 1:nExercises){
  for (d in 1: nDevices){
    file <- paste0("./_output/analysisA2/fit_", exercises[e], "_", devices[d])
    load(file = file)
    draws <- rstan::extract(fit, inc_warmup =FALSE)
    
    assign("tmp", list(apply(draws$beta, 1L, c)))
    names(tmp) <- paste0(exercises[e], "_", devices[d])
    
    listSMB <- c(listSMB, tmp)
  }
}

save("listSMB", file = "./_output/analysisA2/listSMB")


#### Specific probabilistic analyses ####

# setup
par_list <- data.frame(
  par = c("SMB", "int", "slo", "RMSE"),
  label = c(
    "beta",
    "alpha_rec",
    "beta_rec",
    NA
  ),
  center = c(0, 0, 1, 0)
)

# specify combination of parameter-exercise-device according to the following structure
check <- list(
  # SMB = list(
  #   sq = c("repO", "qwik", "metr", "myLi"),
  #   bp = c("repO", "qwik", "metr", "myLi"),
  #   dl = c("repO", "qwik", "metr", "myLi")
  # ),
  int = list(
    sq = c("repO", "qwik", "metr", "myLi"),
    bp = c("repO", "qwik", "metr", "myLi"),
    dl = c("repO", "qwik", "metr", "myLi")
  ),
  slo = list(
    sq = c("repO", "qwik", "metr", "myLi"),
    bp = c("repO", "qwik", "metr", "myLi"),
    dl = c("repO", "qwik", "metr", "myLi")
  )
  # ,
  # RMSE = list(
  #   bp = c("qwik")
  # )
) %>%
  vctrs::list_drop_empty()

exercises2 <- check %>%
  unlist(recursive = FALSE) %>%
  names() %>%
  gsub("SMB.", "", .) %>%
  gsub("int.", "", .) %>%
  gsub("slo.", "", .) %>%
  gsub("RMSE.", "", .) %>%
  unique()

check_length <- length(check)

for (i in 1:check_length){
  
  par <- names(check)[i]
  
  if (par == "RMSE"){
    load(file = "./_output/analysisA1/listRMSE")
  } 
  
  for (e in 1:length(exercises2)){
    
    devices2 <- address(paste0("check$", par, "$", exercises2[e]))
    
    for (d in 1:length(devices2)){
      
      if (par == "RMSE"){
        p <- listRMSE[[paste0(exercises2[e], "_", devices2[d])]]
      } else{
        
        if (par == "SMB"){
          file <- paste0("./_output/analysisA2/fit_", exercises2[e], "_", devices2[d])
        } else{
          file <- paste0("./_output/analysisA1/fit_", exercises2[e], "_", devices2[d])
        }
        
        load(file = file)
        
        draws <- rstan::extract(fit, inc_warmup =FALSE)
        
        p <- address(paste0("draws$", par_list$label[par_list$par==par]))
      }
      
      lower <- rope(p, c(-Inf, par_list$center[par_list$par==par]), ci = 1)$ROPE_Percentage * 100
      higher <- rope(p, c(par_list$center[par_list$par==par], Inf), ci = 1)$ROPE_Percentage * 100
      
      cat("For", crayon::bgCyan(devices2[d]), "in the", crayon::blue(exercises2[e]), ", p(", crayon::yellow(par), "<", par_list$center[par_list$par==par], ") =", crayon::red(round(lower, 2)), "% and p(", crayon::yellow(par), "> ", par_list$center[par_list$par==par], ") =", crayon::green(round(higher, 2)), "%.\n")
      
    }
    
  }
  
}
