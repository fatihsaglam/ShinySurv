P_dist_select <- function(time_1,
                          time_2 = NULL,
                          c_r = NULL,
                          c_l = NULL,
                          group = NULL,
                          tolerance = 1e-10,
                          maxiter = 500,
                          init_pars = NULL,
                          interval = FALSE){
  ##---------------------------------------------------------------
  ##                          error check                         -
  ##---------------------------------------------------------------
  if (is.null(group)) {
    group <- as.factor(rep("group 1", length(time_1)))
  } else {
    group <- as.factor(group)
  }

  if (is.null(c_r)) {
    c_r <- rep(1, length(time_1))
  }

  if (is.null(c_l)) {
    c_l <- rep(1, length(time_1))
  }
  ##---------------------------------------------------------------

  if (interval) {

    if (is.null(time_1) | (is.null(time_2))) {
      stop("time_1 and time_2 must be provided.")
    }

    time <- (time_2 - time_1)/2 + time_1
    time[is.na(time)] <- time_1[is.na(time)]
    c_l <- ifelse(time == 0, 0, 1)
    c_r <- ifelse(is.na(time), 0, 1)

    time_1 <- time
    time_2 <- NULL
  } else {
    if (is.null(time_1) | (is.null(time_1) & is.null(time_2))) {
      stop("time_1 must be provided.")
    }
  }

  ##---------------------------------------------------------------
  ##                          error check                         -
  ##---------------------------------------------------------------

  if (is.null(time_2)) {
    time <- time_1
  } else {
    time <- time_2 - time_1
  }

  if (any(time < 0)) {
    stop("Negative time detected. Please check time variables.")
  }
  ##---------------------------------------------------------------

  ## group levels
  group_levels <- levels(group)
  k <- length(group_levels)
  
  pars <- list()
  LLs <- list()
  AICs <- list()
  best_dists <- list()
  best_pars <- list()

  for (i in 1:k) {
    time_temp <- time[group == group_levels[i]]
    c_r_temp <- c_r[group == group_levels[i]]
    c_l_temp <- c_l[group == group_levels[i]]

    ## no censored
    nc_i <- which(c_r_temp == 1 & c_l_temp == 1)
    ## right censored
    r_i <- which(c_r_temp == 0)
    ## left censored
    l_i <- which(c_l_temp == 0)

    pars[[i]] <- list()
    LLs[[i]] <- vector()
    AICs[[i]] <- vector()

    for (j in 1:length(dists)) {
      dist <- dists[j]
      ## likelihood functions
      f_func <- match.fun(FUN = paste0("f_", dist))
      F_func <- match.fun(FUN = paste0("F_", dist))
      S_func <- match.fun(FUN = paste0("S_", dist))
      par_names <- dists_par_names[[j]]
      n_par <- dists_n_par[j]

      ## parameter estimations
      m_par_est <- P_par_est_mle(time = time_temp,
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
                               dist = dist)


      pars[[i]][[j]] <- m_par_est$pars
      LLs[[i]][j] <- m_par_est$LL_final
      names(pars[[i]][[j]]) <- par_names
    }

    AICs[[i]] <- -2*LLs[[i]] + 2*dists_n_par
    names(pars[[i]]) <- dist_names
    names(LLs[[i]]) <- dist_names
    names(AICs[[i]]) <- dist_names

    best_dists[[i]] <- dist_names[sort.int(AICs[[i]], index.return = TRUE, decreasing = FALSE)$ix[1:3]]
    best_pars[[i]] <- pars[[i]][dist_names[sort.int(AICs[[i]], index.return = TRUE, decreasing = FALSE)$ix[1:3]]]
  }

  names(pars) <- group_levels
  names(LLs) <- group_levels
  names(AICs) <- group_levels
  names(best_dists) <- group_levels
  names(best_pars) <- group_levels
  
  results <- list(pars = pars,
                  LLs = LLs,
                  AICs = AICs,
                  top_three_dists = best_dists,
                  top_three_pars = best_pars)
  return(results)
}
