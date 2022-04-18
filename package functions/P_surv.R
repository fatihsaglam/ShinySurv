P_surv <- function(time_1,
                   time_2 = NULL,
                   c_r = NULL,
                   c_l = NULL,
                   group = NULL,
                   distribution = "Weibull",
                   tolerance = 1e-9,
                   maxiter = 200,
                   conf_interval = TRUE,
                   conf_level = 0.95,
                   init_pars = NULL,
                   interval = FALSE,
                   n_boot = 500,
                   bootstrap_type = "NPBoot"){
  ##---------------------------------------------------------------
  ##                          error check                         -
  ##---------------------------------------------------------------
  if (is.null(group)) {
    group <- as.factor(rep("group 1", length(time_1)))
  } else {
    group <- as.factor(group)
  }
  
  if(!(distribution %in% dist_names)) {
    stop(paste0("Wrong distribution name. Available distrbutions are '", paste(dist_names, collapse = "', '"), "'"))
  }
  
  if (is.null(c_r)) {
    c_r <- rep(1, length(time_1))
  }
  
  if (is.null(c_l)) {
    c_l <- rep(1, length(time_1))
  }
  
  if (interval) {
    
    if (is.null(time_1) | (is.null(time_2))) {
      stop("time_1 and time_2 must be provided.")
    }
    
    time <- (time_2 - time_1)/2 + time_1
    time[is.na(time)] <- time_1[is.na(time)]
    c_l <- ifelse(time_1 == 0, 0, 1)
    c_r <- ifelse(is.na(time_2), 0, 1)
    
    # time_1 <- time
    # time_2 <- NULL
  } else {
    if (is.null(time_1) | (is.null(time_1) & is.null(time_2))) {
      stop("time_1 must be provided.")
    }
    
    if (is.null(time_2)) {
      time <- time_1
    } else {
      time <- time_2 - time_1
    }
  }
  
  if (any(time < 0)) {
    stop("Negative time detected. Please check time variables.")
  }
  
  if (conf_interval) {
    if(!(bootstrap_type%in%bootstraps)) {
      stop(paste0("Available bootstrap methods are '", paste(bootstraps, collapse = "' and '"),"'."))
    }
  }
  ##---------------------------------------------------------------
  
  ## group levels
  group_levels <- levels(group)
  
  dist <- dists[dist_names == distribution]
  
  ## likelihood functions
  f_func <- match.fun(FUN = paste0("f_", dist))
  h_func <- match.fun(FUN = paste0("h_", dist))
  F_func <- match.fun(FUN = paste0("F_", dist))
  S_func <- match.fun(FUN = paste0("S_", dist))
  H_func <- match.fun(FUN = paste0("H_", dist))
  q_func <- match.fun(FUN = paste0("q_", dist))
  
  tbls <- list()
  pars <- list()
  pars_boot <- list()
  mean_median_tbls <- list()
  
  n_par <- dists_n_par[dists == dist]
  par_names <- dists_par_names[dists == dist][[1]]
  
  f_funcs <- list()
  h_funcs <- list()
  F_funcs <- list()
  S_funcs <- list()
  H_funcs <- list()
  q_funcs <- list()
  
  range_time <- diff(range(time))
  
  for (i in 1:length(group_levels)) {
    time_temp <- time[group == group_levels[i]]
    c_r_temp <- c_r[group == group_levels[i]]
    c_l_temp <- c_l[group == group_levels[i]]
    
    ## no censored
    nc_i <- which(c_r_temp == 1 & c_l_temp == 1)
    ## right censored
    r_i <- which(c_r_temp == 0)
    ## left censored
    l_i <- which(c_l_temp == 0)
    
    ## parameter estimations
    pars[[i]] <- P_par_est_mle(time = time_temp,
                               nc_i = nc_i,
                               r_i = r_i,
                               l_i = l_i,
                               tolerance = tolerance,
                               maxiter = maxiter,
                               f_func = f_func,
                               F_func = F_func,
                               S_func = S_func,
                               n_par = n_par,
                               init_pars = init_pars,
                               dist = dist)$pars
    
    names(pars[[i]]) <- par_names
    
    t_curve <- seq(0, max(time_temp), length = 30)
    S <- S_func(t_curve, pars[[i]])
    F <- 1 - S
    H <- H_func(t_curve, pars[[i]])
    h <- h_func(t_curve, pars[[i]])
    f <- f_func(t_curve, pars[[i]])
    
    t_loc <- seq(0.001, max(time)*100, diff(range(time))*0.01)
    
    mean <- sum(f_func(t_loc, pars[[i]])*t_loc, na.rm = TRUE)*range_time*0.01
    median <- q_func(0.5, pars[[i]])
    
    if (conf_interval) {
      
      if (bootstrap_type == "JKBoot") {
        m_boot <- P_JKBoot(time_temp = time_temp,
                           t_curve = t_curve,
                           t_loc = t_loc,
                           range_time = range_time,
                           c_r_temp = c_r_temp,
                           c_l_temp = c_l_temp,
                           S_func = S_func,
                           F_func = F_func,
                           H_func = H_func,
                           h_func = h_func,
                           f_func = f_func,
                           q_func = q_func,
                           init_pars = pars[[i]],
                           dist = dist,
                           n_par = n_par,
                           conf_level = conf_level,
                           tolerance = tolerance,
                           maxiter = maxiter)
      }
      
      if (bootstrap_type == "NPBoot") {
        m_boot <- P_NPBoot(time_temp = time_temp,
                           t_curve = t_curve,
                           t_loc = t_loc,
                           range_time = range_time,
                           c_r_temp = c_r_temp,
                           c_l_temp = c_l_temp,
                           S_func = S_func,
                           F_func = F_func,
                           H_func = H_func,
                           h_func = h_func,
                           f_func = f_func,
                           q_func = q_func,
                           init_pars = pars[[i]],
                           dist = dist,
                           n_par = n_par,
                           conf_level = conf_level,
                           tolerance = tolerance,
                           maxiter = maxiter,
                           n_boot = n_boot)
      }
      
      pars_boot[[i]] <- m_boot$pars_M
      S <- m_boot$S
      S_lower <- m_boot$S_lower
      S_upper <- m_boot$S_upper
      F <- m_boot$S
      F_lower <- m_boot$F_lower
      F_upper <- m_boot$F_upper
      H <- m_boot$H
      H_lower <- m_boot$H_lower
      H_upper <- m_boot$H_upper
      h <- m_boot$h
      h_lower <- m_boot$h_lower
      h_upper <- m_boot$h_upper
      f <- m_boot$f
      f_lower <- m_boot$f_lower
      f_upper <- m_boot$f_upper
      
      pars[[i]] <- m_boot$pars
      pars_stderr <- m_boot$pars_stderr
      pars_lower <- m_boot$pars_lower
      pars_upper <- m_boot$pars_upper
      
      mean <- m_boot$mean
      mean_stderr <- m_boot$mean_stderr
      mean_lower <- m_boot$mean_lower
      mean_upper <- m_boot$mean_upper
      median <- m_boot$median
      median_stderr <- m_boot$median_stderr
      median_lower <- m_boot$median_lower
      median_upper <- m_boot$median_upper
      
      
      tbls[[i]] <- data.frame(time = t_curve,
                              S, S_lower, S_upper,
                              F, F_lower, F_upper,
                              H, H_lower, H_upper,
                              h, h_lower, h_upper,
                              f, f_lower, f_upper)
      
      mean_median_tbls[[i]] <- data.frame(Estimate = c(mean, median),
                                          lower =  c(mean_lower, median_lower),
                                          upper = c(mean_upper, median_upper),
                                          stderr = c(mean_stderr, median_stderr),
                                          row.names = c("mean", "median"))
      
      colnames(mean_median_tbls[[i]]) <- c("Estimate",
                                           paste0("lower ", conf_level*100, "% CI"),
                                           paste0("upper ", conf_level*100, "% CI"),
                                           "Std. error")
    
      
      pars[[i]] <- data.frame(Estimate = pars[[i]],
                              lower = pars_lower,
                              upper = pars_upper,
                              sdterr = pars_stderr)
      
      colnames(pars[[i]]) <- c("Estimate",
                               paste0("lower ", conf_level*100, "% CI"),
                               paste0("upper ", conf_level*100, "% CI"),
                               "Std. error")
    } else {
      pars[[i]] <- data.frame(Estimate = pars[[i]],
                              lower = NA,
                              upper = NA,
                              sdterr = NA)
      colnames(pars[[i]]) <- c("Estimate",
                               paste0("lower ", conf_level*100, "% CI"),
                               paste0("upper ", conf_level*100, "% CI"),
                               "Std. error")
      
      tbls[[i]] <- data.frame(time = t_curve, S, F, H, h, f)
      
      mean_median_tbls[[i]] <- data.frame(Estimate = c(mean, median),
                                          lower = NA,
                                          upper = NA,
                                          sdterr = NA,
                                          row.names = c("mean", "median"))
      colnames(mean_median_tbls[[i]]) <- c("Estimate",
                                           paste0("lower ", conf_level*100, "% CI"),
                                           paste0("upper ", conf_level*100, "% CI"),
                                           "Std. error")
    }
  }
  
  names(tbls) <- group_levels
  names(mean_median_tbls) <- group_levels
  names(pars) <- group_levels
  
  if (conf_interval) {
    names(pars_boot) <- group_levels
  }
  
  if (conf_interval) {
    results <- list(mean_median_tbls = mean_median_tbls,
                    life_tables = tbls,
                    pars = pars,
                    pars_boot = pars_boot,
                    distribution = distribution)
  } else {
    results <- list(mean_median_tbls = mean_median_tbls,
                    life_tables = tbls,
                    pars = pars,
                    distribution = distribution)
  }
  return(results)
}
