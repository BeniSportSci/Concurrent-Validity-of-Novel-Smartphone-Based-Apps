## ---------------------------
##
## Project name: Concurrent Validity of Novel Smartphone-Based Apps Monitoring Barbell Velocity in Powerlifting Exercises
##
## Purpose of script: figure script (plotting, presentation of results)
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
# library(extrafont)
library(ggplot2)
library(ggpubr)
library(readxl)
# library(rstan)
library(tidyverse)
# loadfonts(device = "win")

# funchir::stale_package_check("./_scripts/figures.R")


#### Setup ####

# Functions
source("./_scripts/functions.R")

# Output Specs
digits <- c(2, 2, 2, 2, 2)
LoC <- 0.95
pe <- "mean"
CI <- "hdi"

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
  mutate_at(grep("d_", names(data_raw)), ~./1000) %>% # convert ROM to m
  filter(!is.na(data_raw$v_vicL)) # get only cases with Vicon reference

# missing data long
data_miss <- data_raw %>%
  mutate(d_vic = rowMeans(select(., starts_with("d_vic")))) %>%
  mutate(v_vic = rowMeans(select(., starts_with("v_vic")))) %>%
  select(-c(
    "v_vicR",
    "v_vicL",
    "d_vicR",
    "d_vicL",
    "load",
    "exercise_4"
  ))

data_miss[, 3:(ncol(data_miss)-2)] <- data_miss[, 3:(ncol(data_miss)-2)] %>%
  replace(!is.na(.), 0) %>%
  replace(is.na(.), 1)

colnames(data_miss)[3:(2+nDevices)] <- gsub("v_", "", colnames(data_miss)[3:(2+nDevices)])

# filter
data_raw <- data_raw %>%
  select(-c(
    "d_vicL",
    "d_vicR", 
    "exercise_4"
  )) 

exercises <- c("sq",
               "bp",
               "dl")
exercises_full <- c(
  "Squat",
  "Bench Press",
  "Deadlift"
)
nExercises <- length(exercises)


nDevices <- 4
devices <- data_raw %>%
  select((ncol(data_raw) - nDevices + 1):ncol(data_raw)) %>%
  colnames() %>%
  stringr::str_sub(-4,-1)

# missing data short
data_miss_short <- data.frame(
  device = factor(1:4, labels = devices),
  sq = rep(NA, nDevices),
  bp = rep(NA, nDevices),
  dl = rep(NA, nDevices)
)

data_miss_short_r <- data_miss_short

for (e in 1:nExercises){
  
  total <- data_miss %>%
    filter(exercise_3 == exercises[e]) %>%
    nrow()
  
  for (d in 1:nDevices){
    
    count <- data_miss %>%
      filter(exercise_3 == exercises[e]) %>%
      select(devices[d]) %>%
      sum()
    
    data_miss_short[
      grepl(devices[d], data_miss_short$device), 
      grepl(exercises[e], colnames(data_miss_short))
      ] <- count
    
    data_miss_short_r[
      grepl(devices[d], data_miss_short_r$device), 
      grepl(exercises[e], colnames(data_miss_short_r))
    ] <- (count / total * 100) %>%
      round(digits = 1)
      # formatC(digits = 1, format = "f")
    
  }
  
}

data_miss_short <- data_miss_short %>%
  gather(exercise, freq, sq:dl, factor_key = TRUE)
data_miss_short_r <- data_miss_short_r %>%
  gather(exercise, freq, sq:dl, factor_key = TRUE) %>%
  mutate(lab = formatC(freq, digits = 1, format = "f"))



#### Plot Missing Reps ####

colors <- c("#616691", "#138086", "#DC8665", "#EEB462")
text_size <- 9

figure_tmp <- ggplot(data = data_miss_short_r, aes(x = exercise, y = freq)) +
  theme_bw() + 
  theme(
    text = element_text(family = "Arial"),
    legend.position = "None",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = text_size, hjust = 0.5, margin = margin(t = 10)),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(),
    axis.title.y = element_text(size = text_size, margin = margin(r = 5)),
    axis.text.y = element_text(size = text_size, hjust = 0.5, margin = margin(r = 0)),
    strip.placement = "outside", 
    strip.background = element_rect(fill = "white", color = "white"),
    
    strip.text = element_text(size = text_size, margin = margin(t = 5, b = 2, l = 4))
  ) +
  coord_cartesian(xlim = c(0.2, 3.5), ylim = c(0, 100), expand = FALSE) +
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = darken(colors, 30)) + 
  scale_x_discrete(labels = c("S", "B", "D")) +
  scale_y_continuous(limits = c(0, 100), name = "Rate (%)") +
  geom_col(aes(color = device, fill = device), alpha = 0.6) +
  geom_text(aes(y = freq, label = lab, color = device), vjust=-2, size=text_size/4) +
  facet_grid(~device,
             space = "free_x",
             switch = "x",
             labeller = as_labeller(c(
               repO = "RepOne", 
               qwik = "Qwik VBT", 
               metr = "Metric VBT", 
               myLi = "MyLift"
               ))
             )

output_width <- 90 # in mm
output_height <- 90

ggsave(filename = paste0("./_plots/Missed_Reps.png"), plot = figure_tmp,  
       width = output_width, height = output_height,   
       units = "mm", device="png", dpi = 600
       , bg ="white"
)


#### Plot Stats ####

# Presets

posteriorList <- c("SMB", "intercept", "slope", "Rsq", "RMSE")
nPosteriorList <- length(posteriorList)
xlabs <- c(
  "SMB", 
  "Intercept", 
  "Slope",
  expression(R^2), 
  "RMSE"
)
ylabs <- c(
  "Squat",
  "Bench Press",
  "Deadlift"
)
dlabs <- c(
  "RO", 
  "QW", 
  "MT", 
  "ML"
)



load(file = "./_output/analysisA1/listRMSE")
load(file = "./_output/analysisA2/listSMB")


# Stats

plotData <- list()

for (e in 1:nExercises){
  
  for (d in 1:nDevices){
    
    tmp <- list()
    
    file <- paste0("./_output/analysisA1/fit_", exercises[e], "_", devices[d])
    load(file = file)
    
    draws <- rstan::extract(fit, inc_warmup =FALSE)
    iter <- dim(draws$Sigma_corr)[1]
    
    # SMB
    SMB <- address(paste0("listSMB$", exercises[e], "_", devices[d]))
    mSMB <- mean(SMB)
    hSMB <- hdi(SMB, ci = LoC)
    lSMB <- hSMB$CI_low
    uSMB <- hSMB$CI_high
    
    # intercept
    Intercept <- apply(draws$alpha_rec, 1L, c)
    mIntercept <- mean(Intercept)
    hIntercept <- hdi(Intercept, ci = LoC)
    lIntercept <- hIntercept$CI_low
    uIntercept <- hIntercept$CI_high
    
    # slope
    Slope <- apply(draws$beta_rec, 1L, c)
    mSlope <- mean(Slope)
    hSlope <- hdi(Slope, ci = LoC)
    lSlope <- hSlope$CI_low
    uSlope <- hSlope$CI_high
    
    # R²
    v_pred <- apply(draws$v_pred, 1L, c) %>%
      replace(which(. %in% 999), NA) %>%
      as.data.frame() %>%
      filter(complete.cases(.)) %>%
      t()
    Residuals <- apply(draws$residuals, 1L, c) %>%
      replace(which(. %in% 999), NA) %>%
      as.data.frame() %>%
      filter(complete.cases(.)) %>%
      t()
    Rsq <- bayesian.R2(v_pred, Residuals)
    mRsq <- mean(Rsq)
    hRsq <- hdi(Rsq, ci = LoC)
    lRsq <- hRsq$CI_low
    uRsq <- hRsq$CI_high
    
    # RMSE
    RMSE <- address(paste0("listRMSE$", exercises[e], "_", devices[d]))
    mRMSE <- mean(RMSE)
    hRMSE <- hdi(RMSE, ci = LoC)
    lRMSE <- hRMSE$CI_low
    uRMSE <- hRMSE$CI_high
    
    statTable <- as.data.frame(
      matrix(c(
                mSMB, lSMB, uSMB, 
                mIntercept, lIntercept, uIntercept, 
                mSlope, lSlope, uSlope, 
                mRsq, lRsq, uRsq, 
                mRMSE, lRMSE, uRMSE
              ), 
            nrow = 3, ncol = nPosteriorList,
            dimnames = list(
              c("mean", "low", "high"), posteriorList)
            )
      )
    
    tmp[[length(tmp) + 1]] <- statTable
    names(tmp)[length(tmp)] <- "statTable"
    
    # ROPE
    if (devices[d] == "repO"){
      
      sesoiSMB <- max(abs(lSMB), abs(uSMB))
      sesoiInt <- max(abs(lIntercept), abs(uIntercept))
      sesoiSlo <- max(abs(lSlope - 1), abs(uSlope - 1))
      sesoiRsq <- lRsq
      sesoiRMSE <- uRMSE
      
      ropeRange <- data.frame(
        SMB = c(-sesoiSMB, sesoiSMB),
        intercept = c(-sesoiInt, sesoiInt),
        slope = c(1 - sesoiSlo, 1 + sesoiSlo),
        Rsq = c(sesoiRsq, 1),
        RMSE = c(0, sesoiRMSE)
      )
      
      tmp[[length(tmp) + 1]] <- ropeRange
      names(tmp)[length(tmp)] <- "ropeRange"
      
      ropeSMB <- NA
      ropeInt <- NA
      ropeSlo <- NA
      ropeRsq <- NA
      ropeRMSE <- NA
      
    } else{
      
      ropeSMB <- as.numeric(rope(SMB, range = ropeRange$SMB, ci = 1)) * 100
      ropeInt <- as.numeric(rope(Intercept, range = ropeRange$intercept, ci = 1)) * 100
      ropeSlo <- as.numeric(rope(Slope, range = ropeRange$slope, ci = 1)) * 100
      ropeRsq <- as.numeric(rope(Rsq, range = ropeRange$Rsq, ci = 1)) * 100
      ropeRMSE <- as.numeric(rope(RMSE, range = ropeRange$RMSE, ci = 1)) * 100
      
    }
    
    ropePct <- c(ropeSMB, ropeInt, ropeSlo, ropeRsq, ropeRMSE)
    tmp[[length(tmp) + 1]] <- ropePct
    names(tmp)[length(tmp)] <- "ropePct"
    
    statSum <- rep(NA, nPosteriorList)
    for (s in 1:nPosteriorList){
      statSum[s] <- paste0(format(round(statTable[1, s], digits[s]), nsmall = digits[s]), " [", format(round(statTable[2, s], digits[s]), nsmall = digits[s]), ", ", format(round(statTable[3, s], digits[s]), nsmall = digits[s]), "]")
    }
    tmp[[length(tmp) + 1]] <- statSum
    names(tmp)[length(tmp)] <- "statSum"
    
    plotData[[length(plotData) + 1]] <- tmp
    names(plotData)[length(plotData)] <- paste0(exercises[e], "_", devices[d])
    
  }
  
}

# str(plotData)


#### Forest Plots ####

output_width <- 90 # in mm
output_height <- 180

center <- c(0, 0, 1, 1, 0)
frame <- list(
  limits = data.frame(
    SMB = c(-1.5, 1), 
    Int = c(-0.2, 0.2),
    Slo = c(0.3, 1.3),
    Rsq = c(0.6, 1),
    RMSE = c(0, 0.25)
  )
)

nbreaks <- c(6, 5, 6, 5, 6)
for (s in 1:nPosteriorList){
  frame[[length(frame) + 1]] <- seq(
    from = frame$limits[1, s], 
    to = frame$limits[2, s], 
    length.out = nbreaks[s]
  )
  names(frame)[length(frame)] <- posteriorList[s]
}

plotAdjust <- data.frame(
  sq = c(1, 1, 0, 1), # t, r, b, l
  bp = c(0, 1, 0, 1),
  dl = c(0, 1, 1, 1)
)

coordAdjust <- data.frame(
  sq = c(0.5, 4.2),
  bp = c(0.5, 4.5),
  dl = c(0, 4.5)
)

hJust <- c(0.55, 0.5, 0.55)

shapes <- c(73, rep(23, 3))
# colors <-  c("black", colorRampPalette(c("grey30", "grey70"))(3)) # for greyscale
colors <- c("#616691", "#138086", "#DC8665", "#EEB462")
text_size <- 9
shape_size <- 3
sizes <- c(shape_size + 2, rep(shape_size, 3))
stat_size <- 2.5


theme_fp <- theme_bw() + 
  theme(
    text = element_text(family = "Arial"),
    legend.position = "None",
    panel.border = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title.y = element_text(size = text_size, margin = margin(r = 10)),
    axis.text.y = element_text(size = text_size, hjust = 0.5, margin = margin(r = -5)),
    panel.grid = element_blank()
  )



defaultW <- getOption("warn")
options(warn = -1)



for (s in 1:nPosteriorList){
  
  rope <- matrix(NA, 3, 2)
  
  for (e in 1:nExercises){
    
    data <- data.frame(
      device = factor(1:4, labels = rev(devices), levels = 4:1),
      mean = rep(NA, nDevices),
      LL = rep(NA, nDevices),
      UL = rep(NA, nDevices),
      pct = rep(NA, nDevices),
      statSum = rep(NA, nDevices)
    )
    for (d in 1:nDevices){
      data[which(data$device == devices[d]), 2:4] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statTable$", posteriorList[s]))
      if (d > 1){
        data[which(data$device == devices[d]), 5] <- paste0(
          "'p('*Theta~epsilon~'R) ='~",
          round(
            address(paste0("plotData$", exercises[e], "_", devices[d], "$ropePct[", s, "]")),
            1
          ),
          "*'%'"
        )
      } else{
        data[which(data$device == devices[d]), 5] <- ""
      }
      data[which(data$device == devices[d]), 6] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statSum[", s, "]"))
    }
    
    
    rope <- address(paste0("plotData$", exercises[e], "_repO$ropeRange$", posteriorList[s]))
    
    plot_tmp <- ggplot(data = data, aes(x = mean, y = as.numeric(device))) +
      theme_fp +
      theme(
        axis.title.y = element_text(hjust = hJust[e]),
        axis.text.y = element_text(color = rev(colors)),
        plot.margin = unit(plotAdjust[, e], 'lines')
      ) +
      coord_cartesian(ylim = coordAdjust[,e], xlim = c(frame$limits[1, s], frame$limits[2, s] + abs(diff(frame$limits[[s]])) * 0.45)) +
      scale_y_continuous(
        breaks = 1:4,
        labels = rev(dlabs)
      ) +
      scale_x_continuous(
        limits = c(frame$limits[1, s], frame$limits[2, s] + abs(diff(frame$limits[[s]])) * 0.45),
        breaks = address(paste0("frame$", posteriorList[s]))
      ) +
      ylab(label = ylabs[e]) +
      annotate("rect", 
               xmin = rope[1], xmax = rope[2], ymin = -0.5, ymax = 5.5, 
               alpha = 0.3, 
               fill = "grey60" # 09a5bd
      ) +
      geom_segment(
        aes(x = center[s], xend = center[s], y = -0.5, yend = 5.5),
        color = "black",
        linetype = "22"
      ) +
      geom_errorbarh(
        aes(xmin = LL, xmax = UL),
        linewidth = 1,
        height = 0,
        color = colors
        ) +
      geom_point(
        size = sizes, 
        stroke = 1,
        shape = shapes,
        color = "black",
        fill = colors
      ) +
      geom_text(
        data = data,
        aes(label = pct), parse = TRUE,
        nudge_x = frame$limits[2, s] - data$mean + abs(diff(frame$limits[[s]])) * 0.275,
        nudge_y = abs(diff(coordAdjust[, e])) * -0.04,
        size = stat_size,
        color = colors
      ) +
      geom_text(
        data = data,
        aes(label = statSum),
        nudge_x = frame$limits[2, s] - data$mean + abs(diff(frame$limits[[s]])) * 0.275,
        nudge_y = abs(diff(coordAdjust[, e])) * 0.04,
        size = stat_size,
        color = colors
      )

    if (e == nExercises){
      plot_tmp <- plot_tmp +
        theme(
          axis.line.x = element_line(),
          axis.text.x = element_text(family = "Arial", size = text_size),
          axis.title.x = element_text(family = "Arial", size = text_size, margin = margin(t = 9))
        ) +
        xlab(xlabs[s])
    }
    
    assign(paste0("plot", e), plot_tmp)
    
  }
  
  figure_tmp <- ggarrange(plot1, plot2, plot3, 
                          ncol = 1, heights = c(1, 1, 1.25))
  
  ggsave(filename = paste0("./_plots/Forest_", posteriorList[s], ".png"), plot = figure_tmp,  
         width = output_width, height = output_height,   
         units = "mm", device="png", dpi = 600
         , bg ="white"
  )
  
}

options(warn = defaultW)


#### Scatter Plots Horizontal ####

colors <- c("#616691", "#138086", "#DC8665", "#EEB462")
text_size <- 9
shape_size <- 2

theme_sp <- theme_bw() + 
  theme(
    text = element_text(family = "Arial"),
    axis.line = element_blank(),
    axis.title = element_text(size = text_size),
    axis.text = element_text(size = text_size - 1, hjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(size = text_size + 2, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = text_size - 1),
    legend.box.spacing = unit(0, "pt")
  )


frame <- list(
  limits = data.frame(
    sq = c(0, 1.25), 
    bp = c(0, 0.75),
    dl = c(0, 1.00)
  )
)

nbreaks <- c(6, 6, 6)
for (e in 1:nExercises){
  frame[[length(frame) + 1]] <- seq(
    from = frame$limits[1, e], 
    to = frame$limits[2, e], 
    length.out = nbreaks[e]
  ) %>%
    head(-1) %>%
    tail(-1)
  names(frame)[length(frame)] <- exercises[e]
}

for (e in 1:nExercises){
  
  data1 <- data_raw %>%
    filter(exercise_3 == exercises[e]) %>% 
    select(-c("v_vicR", "v_repO")) %>%
    gather(tool, practical, v_qwik:v_myLi, factor_key = TRUE) %>%
    rename(criterion = v_vicL, exercise = exercise_3)
  
  
  data2 <- data_raw %>%
    filter(exercise_3 == exercises[e]) %>% 
    select(-c("v_vicL", "v_qwik", "v_metr", "v_myLi")) %>%
    gather(tool, practical, v_repO, factor_key = TRUE) %>%
    rename(criterion = v_vicR, exercise = exercise_3)
  
  data <- rbind(data2, data1)
  
  data_model <- data.frame(
    tool = as.factor(levels(data$tool)),
    int = rep(NA, nDevices),
    slo = rep(NA, nDevices)
  )
  for (d in 1:nDevices){
    data_model[d, 2] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statTable$intercept[1]"))
    data_model[d, 3] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statTable$slope[1]"))
  }
  
  plot_tmp <- ggplot(data = data, aes(x = criterion, y = practical, color = tool)) +
    theme_sp +
    theme(
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.title.y = element_text(margin = margin(r = 9))
    ) +
    coord_fixed() +
    scale_x_continuous(
      labels = scale.format,
      expand = c(0, 0),
      limits = address(paste0("frame$limits$", exercises[e])),
      breaks = address(paste0("frame$", exercises[e]))
    ) +
    scale_y_continuous(
      labels = scale.format,
      expand = c(0, 0),
      limits = address(paste0("frame$limits$", exercises[e])),
      breaks = address(paste0("frame$", exercises[e]))
    ) +
    labs(
      title = exercises_full[e],
      x = expression("Criterion"~"(ms"^-1*")"), 
      y = expression("Practical"~"(ms"^-1*")")
    ) +
    scale_color_manual(
      values = colors,
      labels = c("RepOne", "Qwik", "Metric VBT", "MyLift")
      ) +
    geom_point(
      size = shape_size,
      alpha = 0.3
    ) +
    unlist(
      pmap(
        list(colors = colors, int = data_model$int, slo = data_model$slo), 
        function(colors, int, slo){
          stat_function(fun = function(x){
            int + slo * x
          }, 
          xlim = address(paste0("frame$limits$", exercises[e])) * 1.2, 
          color = colors, 
          size = 1.5,
          alpha = 0.8
          )
        }
      )
    ) +
    unlist(
      pmap(
        list(colors = colors, int = data_model$int, slo = data_model$slo), 
        function(colors, int, slo){
          stat_function(fun = function(x){
            int + slo * x
          }, 
          xlim = address(paste0("frame$limits$", exercises[e])) * 1.2, 
          color = "white", 
          size = 1,
          alpha = 1
          )
        }
      )
    ) +
    unlist(
      pmap(
        list(colors = colors, int = data_model$int, slo = data_model$slo), 
        function(colors, int, slo){
          stat_function(fun = function(x){
            int + slo * x
          }, 
          xlim = address(paste0("frame$limits$", exercises[e])) * 1.2, 
          color = colors, 
          size = 1,
          alpha = 0.5
          )
        }
        )
      ) +
    annotate("segment", 
             x = address(paste0("frame$limits$", exercises[e]))[1], xend = address(paste0("frame$limits$", exercises[e]))[2], y = address(paste0("frame$limits$", exercises[e]))[1], yend = address(paste0("frame$limits$", exercises[e]))[2], 
             linetype = "32",
             size = 1)
  
  if (e > 1){
    plot_tmp <- plot_tmp +
      theme(
        axis.title.y = element_blank()
      )
  }
  if (e < 3){
    plot_tmp <- plot_tmp +
      theme(
        legend.position = "None"
      )
  }
  
  assign(paste0("plot", e), plot_tmp)

}

figure_tmp <- ggarrange(plot1, plot2, plot3, 
                        ncol = 3,
                        widths = c(1.13, 1, 1.55)
                        )

output_width <- 180 # in mm
output_height <- output_width / 3

ggsave(filename = "./_plots/Scatter.png", plot = figure_tmp,  
       width = output_width, height = output_height,   
       units = "mm", device="png", dpi = 600
       , bg ="white"
)


#### Scatter Plots Vertical ####

colors <- c("#616691", "#138086", "#DC8665", "#EEB462")
text_size <- 9
shape_size <- 1.5
shape_stroke <- 1
shape <- 3
stat_size <- 2.5

theme_sp <- theme_bw() + 
  theme(
    text = element_text(family = "Arial"),
    legend.position = "None",
    axis.line = element_blank(),
    axis.title = element_text(size = text_size),
    axis.text = element_text(size = text_size - 1, hjust = 0.5),
    panel.grid = element_blank(),
    plot.title = element_text(size = text_size + 2, face = "bold"),
  )


frame <- list(
  limits = data.frame(
    sq = c(0, 1.25), 
    bp = c(0, 0.75),
    dl = c(0, 1.00)
  )
)

nbreaks <- c(6, 6, 6)
for (e in 1:nExercises){
  frame[[length(frame) + 1]] <- seq(
    from = frame$limits[1, e], 
    to = frame$limits[2, e], 
    length.out = nbreaks[e]
  ) %>%
    head(-1) %>%
    tail(-1)
  names(frame)[length(frame)] <- exercises[e]
}

for (e in 1:nExercises){
  
  data1 <- data_raw %>%
    filter(exercise_3 == exercises[e]) %>% 
    select(-c("v_vicR", "v_repO")) %>%
    gather(tool, practical, v_qwik:v_myLi, factor_key = TRUE) %>%
    rename(criterion = v_vicL, exercise = exercise_3)
  
  
  data2 <- data_raw %>%
    filter(exercise_3 == exercises[e]) %>% 
    select(-c("v_vicL", "v_qwik", "v_metr", "v_myLi")) %>%
    gather(tool, practical, v_repO, factor_key = TRUE) %>%
    rename(criterion = v_vicR, exercise = exercise_3)
  
  data <- rbind(data2, data1)
  
  data_model <- data.frame(
    tool = as.factor(levels(data$tool)),
    int = rep(NA, nDevices),
    slo = rep(NA, nDevices)
  )
  for (d in 1:nDevices){
    data_model[d, 2] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statTable$intercept[1]"))
    data_model[d, 3] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statTable$slope[1]"))
  }
  
  plot_tmp <- ggplot(data = data, aes(x = criterion, y = practical, color = tool)) +
    theme_sp +
    theme(
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.title.y = element_text(margin = margin(r = 9))
    ) +
    coord_fixed() +
    scale_x_continuous(
      labels = scale.format,
      expand = c(0, 0),
      limits = address(paste0("frame$limits$", exercises[e])),
      breaks = address(paste0("frame$", exercises[e]))
    ) +
    scale_y_continuous(
      labels = scale.format,
      expand = c(0, 0),
      limits = address(paste0("frame$limits$", exercises[e])),
      breaks = address(paste0("frame$", exercises[e]))
    ) +
    labs(
      title = exercises_full[e],
      x = expression("Criterion"~"(ms"^-1*")"), 
      y = expression("Practical"~"(ms"^-1*")")
    ) +
    scale_color_manual(
      values = colors,
      labels = c("RepOne", "Qwik", "Metric VBT", "MyLift")
    ) +
    geom_point(
      size = shape_size,
      stroke = shape_stroke,
      alpha = 0.3,
      shape = shape
    ) +
    unlist(
      pmap(
        list(colors = colors, int = data_model$int, slo = data_model$slo), 
        function(colors, int, slo){
          stat_function(fun = function(x){
            int + slo * x
          }, 
          xlim = address(paste0("frame$limits$", exercises[e])) * 1.2, 
          color = colors, 
          linewidth = 1,
          alpha = 0.6
          )
        }
      )
    ) +
    annotate("segment", 
             x = address(paste0("frame$limits$", exercises[e]))[1], xend = address(paste0("frame$limits$", exercises[e]))[2], y = address(paste0("frame$limits$", exercises[e]))[1], yend = address(paste0("frame$limits$", exercises[e]))[2], 
             linetype = "32",
             size = 1) 
  
  if (e < 3){
    plot_tmp <- plot_tmp +
      theme(
        axis.title.x = element_blank()
      )
  } 
  
  assign(paste0("plotA", e), plot_tmp)
  
  
  statSum <- data.frame(
    name = c(
      "",
      "RO:", 
      "QW:", 
      "MT:", 
      "ML:"
    ),
    int = c("Intercept", rep(NA, nDevices)),
    slo = c("Slope", rep(NA, nDevices)),
    coord = (nDevices + 1):1
  )
  for (d in 1:nDevices){
    statSum[d + 1, 2] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statSum[2]"))
    statSum[d + 1, 3] <- address(paste0("plotData$", exercises[e], "_", devices[d], "$statSum[3]"))
  }
  
  plot_tmp <- ggplot(statSum) +
    theme_sp +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_blank(),
      plot.margin = unit(c(1, 1, 1, 0), 'lines')
    ) +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 6)) +
    geom_text(
      aes(label = name, x = rep(0, 5), y = 5:1), parse = FALSE,
      size = stat_size,
      color = c("black", colors)
    ) + 
    geom_text(
      aes(label = int, x = rep(3, 5), y = coord), parse = FALSE,
      size = stat_size,
      color = c("black", colors)
    ) + 
    geom_text(
      aes(label = slo, x = rep(8, 5), y = coord), parse = FALSE,
      size = stat_size,
      color = c("black", colors)
    )
  
  assign(paste0("plotB", e), plot_tmp)
  
  
}

figure_tmp <- ggarrange(plotA1, plotB1, plotA2, plotB2, plotA3, plotB3, 
                        ncol = 2, nrow = 3,
                        heights = c(1, 1, 1.1, 1, 1, 1)
)

output_width <- 120 # in mm
output_height <- 180

ggsave(filename = "./_plots/Scatter_vert.png", plot = figure_tmp,  
       width = output_width, height = output_height,   
       units = "mm", device="png", dpi = 600
       , bg ="white"
)
