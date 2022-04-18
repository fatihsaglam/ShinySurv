NP_surv_interval <- function(time_1,
                             time_2,
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
  
  if (is.null(time_1) | is.null(time_2)) {
    stop("Both time_1 and time_2 required for interval censoring")
  }
  
  if (length(time_1) != length(time_2)) {
    stop("time_1 and time_2 must be same length")
  }

  if (any(time_2 - time_1) < 0) {
    stop("time_2 must be bigger than time_1")
  }
  
  if (any(time_1 < 0 | time_2 < 0, na.rm = TRUE)) {
    stop("negative time values detected")
  }
  
  if (!(conf_method %in% conf_methods_names)) {
    stop(paste("Available confidence method are '", paste(conf_methods_names, collapse = "' and '"), "'."))
  }

  ##---------------------------------------------------------------

  conf_method <- conf_methods[conf_methods_names == conf_method]
  
  c_l <- ifelse(time_1 == 0, 0, 1)
  c_r <- ifelse((is.na(time_2)) | (time_2 == Inf), 0, 1)
  
  if (any(time_2 - time_1) == 0) {
    time_2[time_2 == 0] <- time_2[time_2 == 0] + 0.5
    time_1[time_1 == 0] <- ifelse((time_1[time_1 == 0] - 0.5) < 0, 0, time_1[time_1 == 0] - 0.5)
  }
  
  ## group levels
  group_levels <- levels(group)

  ## empty table list
  tbls_final <- list()
  mean_median_tbls <- list()

  ## each group calculations
  for (i in 1:length(group_levels)) {

    ## group interval times
    time_1_temp <- time_1[group == group_levels[i]]
    time_2_temp <- time_2[group == group_levels[i]]
    n <- length(time_1_temp)

    ## all times
    tau <- c(time_1_temp, time_2_temp)

    ## ordering
    order_i <- sort.int(tau, na.last = TRUE, index.return = TRUE)$ix
    tau_sorted <- tau[order_i]
    tau_sorted <- tau_sorted[complete.cases(tau_sorted)]
    m <- length(tau_sorted)
    tau_sorted_prev <- c(0, tau_sorted[-m])

    ## tau intervals
    tbl_tau <- data.frame(tau_prev = tau_sorted_prev, tau = tau_sorted)
    tbl_tau <- tbl_tau[apply(tbl_tau, 1, diff) > 0,]
    tbl_tau <- rbind(data.frame(tau_prev = 0, tau = 0), tbl_tau)
    m <- nrow(tbl_tau)

    ## real intervals
    tbl_time <- data.frame(time_1_temp, time_2_temp)
    tbl_time[is.na(tbl_time)] <- Inf

    ##----------------------------------------------------------------
    ##                            A matrix                           -
    ##----------------------------------------------------------------
    A <- matrix(data = NA, nrow = n, ncol = m)
    A <- sapply(1:m, function(mm1) {
      sapply(1:n, function(mm2) {
        if (tbl_tau[mm1,1] >= tbl_time[mm2,1] & tbl_tau[mm1,2] <= tbl_time[mm2,2]) {
          1
        } else {
          0
        }
      })
    })
    ##----------------------------------------------------------------

    ##---------------------------------------------------------------
    ##                          initial S                           -
    ##---------------------------------------------------------------
    S <- 1 - tbl_tau[,1]/max(tbl_tau[,1])
    ##---------------------------------------------------------------

    repeat{
      ## S_(t_j-1)
      S_prev <- c(1, S[-m])

      ## P column matrix
      P <- as.matrix(S_prev - S)

      ##----------------------------------------------------------------
      ##                      calculation of new d                     -
      ##----------------------------------------------------------------
      AP <- A%*%P
      d <- sapply(1:m, function(mm) sum((A[,mm]*P[mm])/(AP)))
      ##----------------------------------------------------------------

      ##---------------------------------------------------------------
      ##                              Y                               -
      ##---------------------------------------------------------------
      Y <- sum(d) - cumsum(d)
      Y <- c(sum(d), Y[-m])
      ##---------------------------------------------------------------

      ##---------------------------------------------------------------
      ##                            new S                             -
      ##---------------------------------------------------------------
      S_new <- cumprod(1 - d/Y)
      #S_new[m] <- 0
      ##---------------------------------------------------------------

      ## errors
      diff <- S - S_new
      S <- S_new

      ## stopper
      if (max(abs(diff)) < 1e-7) {
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
    ll$time <- tbl_tau[,2]
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

    tbls_final[[i]] <- data.frame(t = tbl_tau[,2], d, Y, S, S_lower, S_upper, SE_S, H, H_lower, H_upper, SE_H)

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

