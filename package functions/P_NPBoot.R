P_NPBoot <- function(time_temp,
                    t_curve,
                    t_loc,
                    range_time,
                    c_r_temp,
                    c_l_temp,
                    S_func,
                    F_func,
                    H_func,
                    h_func,
                    f_func,
                    q_func,
                    init_pars,
                    dist,
                    n_par,
                    conf_level,
                    tolerance,
                    maxiter,
                    n_boot) {

    
  n_temp <- length(time_temp)
  pars_M <- matrix(data = NA, nrow = n_boot, ncol = n_par)
  
  S_all <- matrix(data = NA, nrow = n_boot, ncol = length(t_curve))
  F_all <- matrix(data = NA, nrow = n_boot, ncol = length(t_curve))
  H_all <- matrix(data = NA, nrow = n_boot, ncol = length(t_curve))
  h_all <- matrix(data = NA, nrow = n_boot, ncol = length(t_curve))
  f_all <- matrix(data = NA, nrow = n_boot, ncol = length(t_curve))
  
  mean_all <- numeric(n_boot)
  median_all <- numeric(n_boot)
  
  for (j in 1:n_boot) {
    selected <- sample(1:n_temp, n_temp, replace = TRUE)
    
    time_temp_temp <- time_temp[selected]
    ## no censored
    nc_i_temp <- which(c_r_temp[selected] == 1 & c_l_temp[selected] == 1)
    ## right censored
    r_i_temp <- which(c_r_temp[selected] == 0)
    ## left censored
    l_i_temp <- which(c_l_temp[selected] == 0)
    
    pars_M[j,] <- P_par_est_mle(time = time_temp_temp,
                                nc_i = nc_i_temp,
                                r_i = r_i_temp,
                                l_i = l_i_temp,
                                tolerance = tolerance,
                                maxiter = maxiter,
                                f_func = f_func,
                                F_func = F_func,
                                S_func = S_func,
                                n_par = n_par,
                                init_pars = init_pars,
                                dist = dist)$pars
    
    S_all[j,] <- S_func(t_curve, pars_M[j,])
    F_all[j,] <- 1 - S_all[j,]
    H_all[j,] <- H_func(t_curve, pars_M[j,])
    h_all[j,] <- h_func(t_curve, pars_M[j,])
    f_all[j,] <- f_func(t_curve, pars_M[j,])
    
    mean_all[j] <- sum(f_func(t_loc,
                              pars_M[j,])*t_loc,
                       na.rm = TRUE)*range_time*0.01
    median_all[j] <- q_func(0.5, pars_M[j,])
  }
  
  S_upper_lower <- t(apply(S_all, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
  F_upper_lower <- t(apply(F_all, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
  H_upper_lower <- t(apply(H_all, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
  h_upper_lower <- t(apply(h_all, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
  f_upper_lower <- t(apply(f_all, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
  
  S_lower <- S_upper_lower[,1]
  S_upper <- S_upper_lower[,2]
  S <- colMeans(S_all)
  F_lower <- F_upper_lower[,1]
  F_upper <- F_upper_lower[,2]
  F <- 1 - S
  H_lower <- H_upper_lower[,1]
  H_upper <- H_upper_lower[,2]
  H <- colMeans(H_all)
  h_lower <- h_upper_lower[,1]
  h_upper <- h_upper_lower[,2]
  h <- colMeans(h_all)
  f_lower <- f_upper_lower[,1]
  f_upper <- f_upper_lower[,2]
  f <- colMeans(f_all)
  
  pars_stderr <- apply(pars_M, 2, sd)
  pars_upper_lower <- t(apply(pars_M, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
  pars_lower <- pars_upper_lower[,1]
  pars_upper <- pars_upper_lower[,2]
  pars <- colMeans(pars_M)
  
  mean_stderr <- sd(mean_all)
  mean_lower <- quantile(mean_all, (1 - conf_level)/2, na.rm = TRUE)
  mean_upper <- quantile(mean_all, 1 - (1 - conf_level)/2, na.rm = TRUE)
  mean <- mean(mean_all)
  median_stderr <- sd(median_all)
  median_lower <- quantile(median_all, (1 - conf_level)/2, na.rm = TRUE)
  median_upper <- quantile(median_all, 1 - (1 - conf_level)/2, na.rm = TRUE)
  median <- mean(median_all)
  
  results <- list(pars_M = pars_M,
                  S_lower = S_lower,
                  S_upper = S_upper,
                  S = S,
                  F_lower = F_lower,
                  F_upper = F_upper,
                  F = F,
                  H_lower = H_lower,
                  H_upper = H_upper,
                  H = H,
                  h_lower = h_lower,
                  h_upper = h_upper,
                  h = h,
                  f_lower = f_lower,
                  f_upper = f_upper,
                  f = f,
                  pars_stderr = pars_stderr,
                  pars_lower = pars_lower,
                  pars_upper = pars_upper,
                  pars = pars,
                  mean_stderr = mean_stderr,
                  mean_lower = mean_lower,
                  mean_upper = mean_upper,
                  mean = mean,
                  median_stderr = median_stderr,
                  median_lower = median_lower,
                  median_upper = median_upper,
                  median = median)
  return(results)
}
