P_JKBoot <- function(time_temp,
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
                     maxiter) {
  
  n_temp <- length(time_temp)
  pars_M <- matrix(data = NA, nrow = n_temp, ncol = n_par)
  
  S_all <- matrix(data = NA, nrow = n_temp, ncol = length(t_curve))
  F_all <- matrix(data = NA, nrow = n_temp, ncol = length(t_curve))
  H_all <- matrix(data = NA, nrow = n_temp, ncol = length(t_curve))
  h_all <- matrix(data = NA, nrow = n_temp, ncol = length(t_curve))
  f_all <- matrix(data = NA, nrow = n_temp, ncol = length(t_curve))
  
  mean_all <- numeric(n_temp)
  median_all <- numeric(n_temp)
  
  for (j in 1:n_temp) {
    time_temp_temp <- time_temp[-j]
    ## no censored
    nc_i_temp <- which(c_r_temp[-j] == 1 & c_l_temp[-j] == 1)
    ## right censored
    r_i_temp <- which(c_r_temp[-j] == 0)
    ## left censored
    l_i_temp <- which(c_l_temp[-j] == 0)
    
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
  
  z <- qnorm(1 - (1 - conf_level)/2)
  S_stderr <- apply(S_all, 2, sd)
  F_stderr <- apply(F_all, 2, sd)
  H_stderr <- apply(H_all, 2, sd)
  h_stderr <- apply(h_all, 2, sd)
  f_stderr <- apply(f_all, 2, sd)
  
  S <- colMeans(S_all)
  S_lower <- S - z*S_stderr
  S_upper <- S + z*S_stderr
  F <- colMeans(F_all)
  F_lower <- F - z*F_stderr
  F_upper <- F + z*F_stderr
  H <- colMeans(H_all)
  H_lower <- H - z*H_stderr
  H_upper <- H + z*H_stderr
  h <- colMeans(h_all)
  h_lower <- h - z*h_stderr
  h_upper <- h + z*h_stderr
  f <- colMeans(f_all)
  f_lower <- f - z*f_stderr
  f_upper <- f + z*f_stderr
  
  pars_stderr <- apply(pars_M, 2, sd)
  pars <- colMeans(pars_M)
  pars_lower <- pars - z*pars_stderr
  pars_upper <- pars + z*pars_stderr
  
  mean_stderr <- sd(mean_all)
  mean <- mean(mean_all)
  mean_lower <- mean - z*mean_stderr
  mean_upper <- mean + z*mean_stderr

  median_stderr <- sd(median_all)
  median <- mean(median_all)
  median_lower <- median - z*median_stderr
  median_upper <- median + z*median_stderr
  
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