P_par_est_mle <- function(time,
                          nc_i,
                          r_i,
                          l_i,
                          tolerance = 1e-10,
                          maxiter = 200,
                          f_func,
                          F_func,
                          S_func,
                          n_par,
                          init_pars = NULL,
                          dist) {

  ##---------------------------------------------------------------
  ##                          Heuristic                           -
  ##---------------------------------------------------------------
  f_negLL <- function(pars) {
    # NC <- (f_func(time[nc_i], pars = pars, log = TRUE))
    # R <- (S_func(time[r_i], pars = pars, log = TRUE))
    # L <- (F_func(time[l_i], pars = pars, log = TRUE))

    NC <- log((f_func(time[nc_i], pars = pars, log = FALSE)))
    R <- log((S_func(time[r_i], pars = pars, log = FALSE)))
    L <- log((F_func(time[l_i], pars = pars, log = FALSE)))

    NC[is.nan(NC)] <- -1e40
    R[is.nan(R)] <- -1e40
    L[is.nan(L)] <- -1e40

    NC[is.na(NC)] <- -1e40
    R[is.na(R)] <- -1e40
    L[is.na(L)] <- -1e40

    NC[NC == -Inf] <- -1e40
    R[R == -Inf] <- -1e40
    L[L == -Inf] <- -1e40

    NC[NC == Inf] <- 1e40
    R[R == Inf] <- 1e40
    L[L == Inf] <- 1e40

    LL <- sum(NC) + sum(R) + sum(R)
    # print(LL)
    return(-LL)
  }

  f_LL <- function(pars) {
    # NC <- (f_func(time[nc_i], pars = pars, log = TRUE))
    # R <- (S_func(time[r_i], pars = pars, log = TRUE))
    # L <- (F_func(time[l_i], pars = pars, log = TRUE))

    NC <- log((f_func(time[nc_i], pars = pars, log = FALSE)))
    R <- log((S_func(time[r_i], pars = pars, log = FALSE)))
    L <- log((F_func(time[l_i], pars = pars, log = FALSE)))

    NC[is.nan(NC)] <- -1e100
    R[is.nan(R)] <- -1e100
    L[is.nan(L)] <- -1e100

    NC[is.na(NC)] <- -1e100
    R[is.na(R)] <- -1e100
    L[is.na(L)] <- -1e100

    NC[NC == -Inf] <- -1e100
    R[R == -Inf] <- -1e100
    L[L == -Inf] <- -1e100

    NC[NC == Inf] <- 1e100
    R[R == Inf] <- 1e100
    L[L == Inf] <- 1e100


    LL <- sum(NC) + sum(R) + sum(R)
    #print(LL)
    return(LL)
  }

  if (is.null(init_pars)) {
    init_par <- match.fun(FUN = paste0("init_par_", dist))
    init_pars <- init_par(time)
  }

  # m_opt <- optim(par = init_pars,
  #                fn = f_negLL,
  #                # lower = rep(0, n_par),
  #                # upper = rep(1000, n_par),
  #                # method = "Brent",
  #                control = list(maxit = maxiter,
  #                               factr = tolerance))

  m_opt <- nlminb(start = init_pars,
                    objective = f_negLL,
                    control = list(eval.max = maxiter,
                                   iter.max = maxiter,
                                   rel.tol = tolerance))

  names(m_opt) <- c("par", "value", "convergence", "iterations", "evaluations", "message")

  # m_opt <- nlm(p = init_pars,
  #              f = f_negLL,
  #              iterlim = maxiter,
  #              gradtol = tolerance,
  #              steptol = tolerance)
  # names(m_opt) <- c("value", "par", "gradient", "code", "iterations")

  # if(all(strsplit(m_opt$message, "")[[1]][1:5] == c("E", "R", "R", "O", "R"))) {
  #
  #
  # }
  #
  # m_GA <- GA::de(fitness = f_LL,
  #        lower = rep(0, n_par),
  #        upper = rep(2, n_par),
  #        maxiter = maxiter)
  #
  # m_GA@fitnessValue
  # m_GA@solution

  return(list(pars = m_opt$par,
              LL_final = -m_opt$value))
  ##---------------------------------------------------------------
}

##################################################################
##                      Initial Parameters                      ##
##################################################################
init_par_exp <- function(x) {
  1/mean(x)
}

init_par_weibull <- function(x) {
  x <- x[x > 0]
  meanlog <- mean(log(x))
  varlog <- var(log(x))
  shape <- 1.28/sqrt(varlog)
  scale <- exp(meanlog + 0.572/shape)
  return(c(shape = shape, scale = scale))
}

init_par_lnorm <- function(x) {
  n <- length(x)
  log_x <- log(x)
  sdlog <- sqrt((n - 1)/n)*sd(log_x)
  meanlog <- mean(log_x)
  return(c(meanlog = meanlog, sdlog = sdlog))
}

init_par_gamma <- function(x) {
  n <- length(x)
  m <- mean(x)
  v <- (n - 1)/n * var(x)
  return(c(shape = m^2/v, rate = m/v))
}

init_par_gengamma <- function(x) {
  lt <- log(x[x > 0])
  c(mean(lt), sd(lt), 0)
}

init_par_llogis <- function(x) {
  scale <- median(x)
  shape <- 1/log(quantile(x, 0.25)/scale, base = 3)
  if (shape < 0)
    shape <- 1
  c(shape, scale)
}

init_par_pareto <- function(x) {
  m1 <- mean(x)
  m2 <- mean(x^2)
  scale <- (m1 * m2)/(m2 - 2 * m1^2)
  shape <- 2 * (m2 - m1^2)/(m2 - 2 * m1^2)
  return(c(shape = shape, scale = scale))
}


init_par_rayleigh <- function(x) {
  n <- length(x)
  sigma_biased <- sqrt(sum(x^2)/(2*n))
  c(sigma = sigma_biased)
}

init_par_gompertz <- function(x) {
  c(0.001, 1/mean(x))
}
##################################################################

