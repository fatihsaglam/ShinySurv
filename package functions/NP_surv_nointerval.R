#### non parametric survival table
NP_surv_nointerval <- function(time_1,
                               time_2 = NULL,
                               c_r = NULL,
                               c_l = NULL,
                               group = NULL,
                               conf_method = "Rothman",
                               conf_level = 0.95) {

  ##---------------------------------------------------------------
  ##                          error check                         -
  ##---------------------------------------------------------------
  if (is.null(group)) {
    group <- as.factor(rep("group 1", length(time_1)))
  } else {
    group <- as.factor(group)
  }

  if (is.null(time_1) | (is.null(time_1) & is.null(time_2))) {
    stop("time_1 must be provided.")
  }

  if (is.null(time_2)) {
    time <- time_1
  } else {
    time <- time_2 - time_1
  }

  if (any(time < 0, na.rm = TRUE)) {
    stop("Negative time detected. Check time variables.")
  }

  if (is.null(c_r)) {
    c_r <- rep(1, length(time_1))
  }

  if (is.null(c_l)) {
    c_l <- rep(1, length(time_1))
  }
  
  if (!(conf_method %in% conf_methods_names)) {
    stop(paste("Available confidence method are '", paste(conf_methods_names, collapse = "' and '"), "'."))
  }
  ##---------------------------------------------------------------
  
  complete_i <- !(rowSums(apply(data.frame(time, c_r, c_l, group), 2, complete.cases)) < 4)

  time <- time[complete_i]
  c_r <- c_r[complete_i]
  c_l <- c_l[complete_i]
  group <- group[complete_i]
  
  conf_method <- conf_methods[conf_methods_names == conf_method]
  
  ## group levels
  group_levels <- levels(group)

  ## empty table list
  tbls_final <- list()
  mean_median_tbls <- list()

  ## each group calculations
  for (i in 1:length(group_levels)) {

    ## group times and censors
    time_temp <- time[group == group_levels[i]]
    c_r_temp <- c_r[group == group_levels[i]]
    c_l_temp <- c_l[group == group_levels[i]]

    ## ordering
    order_i <- sort.int(time_temp, index.return = TRUE)$ix
    t_sorted <- c(0, time_temp[order_i])
    c_r_sorted <- c(1, c_r_temp[order_i])
    c_l_sorted <- c(1, c_l_temp[order_i])

    ## unique ordering
    t <- unique(t_sorted)
    m <- length(t)

    ## not censors
    d <- sapply(t, function(mm) sum(c_r_sorted[t_sorted == mm] == 1 &
                                      c_l_sorted[t_sorted == mm] == 1))
    ## right censors
    r <- sapply(t, function(mm) sum(c_r_sorted[t_sorted == mm] == 0))

    ## left censors
    l <- sapply(t, function(mm) sum(c_l_sorted[t_sorted == mm] == 0))

    ##----------------------------------------------------------------
    ##                    Y of right censored data                   -
    ##----------------------------------------------------------------
    Y <- sum(d + r) - cumsum(d + r)
    Y <- c(sum(d + r), Y[-m])
    ##----------------------------------------------------------------

    ##---------------------------------------------------------------
    ##                  S(t) of right censored data                 -
    ##---------------------------------------------------------------
    S <- cumprod(1 - d/Y)
    S[m] <- 0
    ##---------------------------------------------------------------

    ## left censor loop
    repeat {
      ## S(t_j-1)
      S_prev <- c(1, S[-m])

      ##----------------------------------------------------------------
      ##                            P matrix                           -
      ##----------------------------------------------------------------
      P <- sapply(1 - S, function(mm) (S_prev - S)/mm)
      P[lower.tri(P)] <- 0
      ##----------------------------------------------------------------

      ##---------------------------------------------------------------
      ##                            new d                             -
      ##---------------------------------------------------------------
      d_new <- d + sapply(1:m, function(mm){
        sum(P[mm,]*l, na.rm = TRUE)
      })
      d_new[1] <- 0
      ##---------------------------------------------------------------

      ##---------------------------------------------------------------
      ##                              Y                               -
      ##---------------------------------------------------------------
      Y <- sum(d_new + r) - cumsum(d_new + r)
      Y <- c(sum(d_new + r), Y[-m])
      ##---------------------------------------------------------------

      ##----------------------------------------------------------------
      ##                            S(t) new                           -
      ##----------------------------------------------------------------
      S_new <- cumprod(1 - d_new/Y)
      #S_new[m] <- 0
      ##----------------------------------------------------------------

      ## errors
      diff <- S - S_new
      S <- S_new

      ## stopper
      if (max(abs(diff)) < 1e-7) {
        d <- d_new
        break
      }
    }

    ##---------------------------------------------------------------
    ##              survival variance, standard error               -
    ##---------------------------------------------------------------
    V_S <- S^2*cumsum(d/(Y*(Y-d)))
    SE_S <- sqrt(V_S)
    ##---------------------------------------------------------------

    ##---------------------------------------------------------------
    ##          cumulative hazard, variance, standard error         -
    ##---------------------------------------------------------------
    H <- cumsum(d/Y)
    V_H <- cumsum(d/Y^2)
    SE_H <- sqrt(V_H)
    ##---------------------------------------------------------------

    ##----------------------------------------------------------------
    ##                  survival confidence interval                 -
    ##----------------------------------------------------------------
    ll <- vector(mode = "list", length = 16)
    names(ll) <- c("n",
                   "time",
                   "n.risk",
                   "n.event",
                   "n.censor",
                   "surv",
                   "std.err",
                   "cumhaz",
                   "std.chaz",
                   "type",
                   "logse",
                   "conf.int",
                   "conf.type",
                   "lower",
                   "upper",
                   "call")

    ll$n <- sum(group == group_levels[[i]])
    ll$time <- t
    ll$n.risk <- Y
    ll$n.event <- d
    ll$surv <- S
    ll$std.err <- SE_S
    class(ll) <- "survfit"

    ll <- km.ci::km.ci(survi = ll, conf.level = conf_level, method = conf_method)

    S_lower <- ll$lower
    S_upper <- ll$upper

    if (conf_method %in% c("hall-wellner", "loghall", "epband", "logep")) {
      lleft <- which(d > 0)[1] - 1
      rright <- m - tail(which(d > 0 & Y > d), 1)
      S_lower <- c(rep(1, lleft), S_lower, rep(tail(S_lower, 1), rright))
      S_upper <- c(rep(1, lleft), S_upper, rep(tail(S_upper, 1), rright))
      S_lower[m] <- 0
      S_upper[m] <- 0
    }
    ##----------------------------------------------------------------

    ##---------------------------------------------------------------
    ##            cumulative hazard confidence interval             -
    ##---------------------------------------------------------------
    Z <- qnorm(1 - (1 - conf_level)/2)

    H_lower <- H - Z*SE_H
    H_upper <- H + Z*SE_H

    H_lower[H_lower < 0] <- 0
    ##---------------------------------------------------------------

    tbls_final[[i]] <- data.frame(t, d, r, l, Y, S, S_lower, S_upper, SE_S, H, H_lower, H_upper, SE_H)

    ##----------------------------------------------------------------
    ##                    mean - median estimation                   -
    ##----------------------------------------------------------------
    t_50_i <- min(which(tbls_final[[i]]$S<=0.5))
    t_50 <- tbls_final[[i]]$t[t_50_i]
    f_t_50 <- -(tbls_final[[i]]$S[t_50_i - 1] - tbls_final[[i]]$S[t_50_i + 1])/
      (tbls_final[[i]]$t[t_50_i - 1] - tbls_final[[i]]$t[t_50_i + 1])
    SE_S_t_50 <- tbls_final[[i]]$SE_S[t_50_i]
    SE_t_50 <- 1/f_t_50*SE_S_t_50
    t_50_lower <- t_50 - Z*SE_t_50
    t_50_upper <- t_50 + Z*SE_t_50

    med <- t_50
    med_lower <- t_50_lower
    med_upper <- t_50_upper
    # med <- tbls_final[[i]]$t[min(which(tbls_final[[i]]$S<=0.5))]
    # med_lower <- tbls_final[[i]]$t[min(which(tbls_final[[i]]$S<=0.75))]
    # med_upper <- tbls_final[[i]]$t[max(which(tbls_final[[i]]$S>=0.25))]

    mu_tau <- c(diff(tbls_final[[i]]$t) * tbls_final[[i]]$S[-m],0)
    mean <- sum(mu_tau)
    v_mean_step_1 <- rev(cumsum(rev(mu_tau)))^2*d/(Y*(Y - d))
    v_mean_step_1[is.nan(v_mean_step_1)] <- 0
    V_mean <- sum(v_mean_step_1)
    SE_mean <- sqrt(V_mean)
    mean_lower <- mean - Z*SE_mean
    mean_upper <- mean + Z*SE_mean

    mean_median_tbls[[i]] <- data.frame(estimate = c(mean, med),
                                        lower = c(mean_lower, med_lower),
                                        upper = c(mean_upper, med_upper),
                                        row.names = c("mean", "median"))
    colnames(mean_median_tbls[[i]]) <- c("Estimate",
                                         paste0("lower ", conf_level*100, "% CI"),
                                         paste0("upper ", conf_level*100, "% CI"))


    ##----------------------------------------------------------------
  }
  names(mean_median_tbls) <- group_levels
  names(tbls_final) <- group_levels

  return(list(mean_median_tbls = mean_median_tbls,
              life_tables = tbls_final))
}
