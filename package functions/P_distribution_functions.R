##---------------------------------------------------------------
##                          Exponential                         -
##---------------------------------------------------------------
f_exp <- function(x, pars, log = FALSE){
  dexp(x, rate = pars, log = log)
}

F_exp <- function(x, pars, log = FALSE){
  pexp(x, rate = pars, log.p = log)
}

S_exp <- function(x, pars, log = FALSE){
  1 - F_exp(x, pars, log)
}

h_exp <- function(x, pars, log = FALSE) {
  f_exp(x, pars, log = FALSE)/
    S_exp(x, pars, log = FALSE)
}

H_exp <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_exp(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_exp <- function(x, pars, log = FALSE){
  qexp(x, pars[1], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            Weibull                           -
##---------------------------------------------------------------
f_weibull <- function(x, pars, log = FALSE) {
  dweibull(x, pars[1], pars[2], log = log)
}

F_weibull <- function(x, pars, log = FALSE) {
  pweibull(x, pars[1], pars[2], log.p = log)
}

S_weibull <- function(x, pars, log = FALSE) {
  1 - F_weibull(x, pars, log = log)
}

h_weibull <- function(x, pars, log = FALSE) {
  f_weibull(x, pars, log = FALSE)/
    S_weibull(x, pars, log = FALSE)
}

H_weibull <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_weibull(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_weibull <- function(x, pars, log = FALSE) {
  qweibull(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            gamma                             -
##---------------------------------------------------------------
f_gamma <- function(x, pars, log = FALSE) {
  dgamma(x, pars[1], pars[2], log = log)
}

F_gamma <- function(x, pars, log = FALSE) {
  pgamma(x, pars[1], pars[2], log.p = log)
}

S_gamma <- function(x, pars, log = FALSE) {
  1 - F_gamma(x, pars, log = log)
}

h_gamma <- function(x, pars, log = FALSE) {
  f_gamma(x, pars, log = FALSE)/
    S_gamma(x, pars, log = FALSE)
}

H_gamma <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_gamma(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_gamma <- function(x, pars, log = FALSE) {
  qgamma(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            log normal                        -
##---------------------------------------------------------------
f_lnorm <- function(x, pars, log = FALSE) {
  dlnorm(x, pars[1], pars[2], log = log)
}

F_lnorm <- function(x, pars, log = FALSE) {
  plnorm(x, pars[1], pars[2], log.p = log)
}

S_lnorm <- function(x, pars, log = FALSE) {
  1 - F_lnorm(x, pars, log = log)
}

h_lnorm <- function(x, pars, log = FALSE) {
  f_lnorm(x, pars, log = FALSE)/
    S_lnorm(x, pars, log = FALSE)
}

H_lnorm <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_lnorm(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_lnorm <- function(x, pars, log = FALSE) {
  qlnorm(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            log logistic                      -
##---------------------------------------------------------------
f_llogis <- function(x, pars, log = FALSE) {
  dllogis(x, pars[1], pars[2], log = log)
}

F_llogis <- function(x, pars, log = FALSE) {
  pllogis(x, pars[1], pars[2], log.p = log)
}

S_llogis <- function(x, pars, log = FALSE) {
  1 - F_llogis(x, pars, log = log)
}

h_llogis <- function(x, pars, log = FALSE) {
  f_llogis(x, pars, log = FALSE)/
    S_llogis(x, pars, log = FALSE)
}

H_llogis <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_llogis(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_llogis <- function(x, pars, log = FALSE) {
  qllogis(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            exponential power                 -
##---------------------------------------------------------------
f_normp <- function(x, pars, log = FALSE) {
  dnormp(x, pars[1], pars[2], pars[3], log = log)
}

F_normp <- function(x, pars, log = FALSE) {
  pnormp(x, pars[1], pars[2], pars[3], log.pr = log)
}

S_normp <- function(x, pars, log = FALSE) {
  1 - F_normp(x, pars, log = log)
}

h_normp <- function(x, pars, log = FALSE) {
  f_normp(x, pars, log = FALSE)/
    S_normp(x, pars, log = FALSE)
}

H_normp <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_normp(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_normp <- function(x, pars, log = FALSE){
  qnormp(x, pars[1], pars[2], pars[3], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            gompertz                 -
##---------------------------------------------------------------
f_gompertz <- function(x, pars, log = FALSE){
  flexsurv::dgompertz(x, pars[1], pars[2], log = log)
}

F_gompertz <- function(x, pars, log = FALSE){
  flexsurv::pgompertz(x, pars[1], pars[2], log.p = log)
}

S_gompertz <- function(x, pars, log = FALSE){
  1 - F_gompertz(x, pars, log = log)
}

h_gompertz <- function(x, pars, log = FALSE) {
  f_gompertz(x, pars, log = FALSE)/
    S_gompertz(x, pars, log = FALSE)
}

H_gompertz <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_gompertz(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_gompertz <- function(x, pars, log = FALSE) {
  flexsurv::qgompertz(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            inverse gaussian                  -
##---------------------------------------------------------------
f_invgauss <- function(x, pars, log = FALSE){
  dinvgauss(x, pars[1], pars[2], log = log)
}

F_invgauss <- function(x, pars, log = FALSE){
  pinvgauss(x, pars[1], pars[2], log.p = log)
}

S_invgauss <- function(x, pars, log = FALSE){
  1 - F_invgauss(x, pars, log = log)
}

h_invgauss <- function(x, pars, log = FALSE) {
  f_invgauss(x, pars, log = FALSE)/
    S_invgauss(x, pars, log = FALSE)
}

H_invgauss <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_invgauss(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_invgauss <- function(x, pars, log = FALSE) {
  qinvgauss(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            pareto                            -
##---------------------------------------------------------------
f_pareto <- function(x, pars, log = FALSE){
  dpareto(x, pars[1], pars[2], log = log)
}

F_pareto <- function(x, pars, log = FALSE){
  ppareto(x, pars[1], pars[2], log.p = log)
}

S_pareto <- function(x, pars, log = FALSE){
  1 - F_pareto(x, pars, log = log)
}

h_pareto <- function(x, pars, log = FALSE) {
  f_pareto(x, pars, log = FALSE)/
    S_pareto(x, pars, log = FALSE)
}

H_pareto <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_pareto(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_pareto <- function(x, pars, log = FALSE) {
  qpareto(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            gengamma                          -
##---------------------------------------------------------------
f_gengamma <- function(x, pars, log = FALSE){
  dgengamma(x, pars[1], pars[2], pars[3], log = log)
}

F_gengamma <- function(x, pars, log = FALSE){
  pgengamma(x, pars[1], pars[2], pars[3], log.p = log)
}

S_gengamma <- function(x, pars, log = FALSE){
  1 - F_gengamma(x, pars, log = log)
}

h_gengamma <- function(x, pars, log = FALSE) {
  f_gengamma(x, pars, log = FALSE)/
    S_gengamma(x, pars, log = FALSE)
}

H_gengamma <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_gengamma(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_gengamma <- function(x, pars, log = FALSE) {
  qgengamma(x, pars[1], pars[2], pars[3], log.p = log)
}
##---------------------------------------------------------------



##---------------------------------------------------------------
##                            rayleigh                            -
##---------------------------------------------------------------
f_rayleigh <- function(x, pars, log = FALSE){
  drayleigh(x, pars[1], log = log)
}

F_rayleigh <- function(x, pars, log = FALSE){
  prayleigh(x, pars[1], log.p = log)
}

S_rayleigh <- function(x, pars, log = FALSE){
  1 - F_rayleigh(x, pars, log = log)
}

h_rayleigh <- function(x, pars, log = FALSE) {
  f_rayleigh(x, pars, log = FALSE)/
    S_rayleigh(x, pars, log = FALSE)
}

H_rayleigh <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_rayleigh(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_rayleigh <- function(x, pars, log = FALSE) {
  qrayleigh(x, pars[1], log.p = log)
}
##---------------------------------------------------------------



##---------------------------------------------------------------
##        Birnbaum-Saunders (fatigue life)                      -
##---------------------------------------------------------------
f_fatigue <- function(x, pars, log = FALSE) {
  dfatigue(x, pars[1], pars[2], pars[3], log = log)
}

F_fatigue <- function(x, pars, log = FALSE) {
  pfatigue(x, pars[1], pars[2], pars[3], log.p = log)
}

S_fatigue <- function(x, pars, log = FALSE) {
  1 - F_fatigue(x, pars, log = log)
}

h_fatigue <- function(x, pars, log = FALSE) {
  f_fatigue(x, pars, log = FALSE)/
    S_fatigue(x, pars, log = FALSE)
}

H_fatigue <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_fatigue(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_fatigue <- function(x, pars, log = FALSE) {
  qfatigue(x, pars[1], pars[2], pars[3], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##                            Frechet                           -
##---------------------------------------------------------------
f_frechet <- function(x, pars, log = FALSE){
  dfrechet(x, pars[1], pars[2], pars[3], log = log)
}

F_frechet <- function(x, pars, log = FALSE){
  pfrechet(x, pars[1], pars[2], pars[3], log.p = log)
}

S_frechet <- function(x, pars, log = FALSE){
  1 - F_frechet(x, pars, log = log)
}

h_frechet <- function(x, pars, log = FALSE) {
  f_frechet(x, pars, log = FALSE)/
    S_frechet(x, pars, log = FALSE)
}

H_frechet <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_frechet(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_frechet <- function(x, pars, log = FALSE) {
  qfrechet(x, pars[1], pars[2], pars[3], log.p = log)
}
##---------------------------------------------------------------


##---------------------------------------------------------------
##            Generalized Pareto distribution                   -
##---------------------------------------------------------------
f_gpd <- function(x, pars, log = FALSE){
  dgpd(x, pars[1], pars[2], pars[3], log = log)
}

F_gpd <- function(x, pars, log = FALSE){
  pgpd(x, pars[1], pars[2], pars[3], log.p = log)
}

S_gpd <- function(x, pars, log = FALSE){
  1 - F_gpd(x, pars, log = log)
}

h_gpd <- function(x, pars, log = FALSE) {
  f_gpd(x, pars, log = FALSE)/
    S_gpd(x, pars, log = FALSE)
}

H_gpd <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_gpd(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_gpd <- function(x, pars, log = FALSE) {
  qgpd(x, pars[1], pars[2], pars[3], log.p = log)
}
##---------------------------------------------------------------




##---------------------------------------------------------------
##            Shifted Gompertz distribution                     -
##---------------------------------------------------------------
f_sgomp <- function(x, pars, log = FALSE){
  dsgomp(x, pars[1], pars[2], log = log)
}

F_sgomp <- function(x, pars, log = FALSE){
  psgomp(x, pars[1], pars[2], log.p = log)
}

S_sgomp <- function(x, pars, log = FALSE){
  1 - F_sgomp(x, pars, log = log)
}

h_sgomp <- function(x, pars, log = FALSE) {
  f_sgomp(x, pars, log = FALSE)/
    S_sgomp(x, pars, log = FALSE)
}

H_sgomp <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_sgomp(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_sgomp <- function(x, pars, log = FALSE) {
  qsgomp(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------



##---------------------------------------------------------------
##                       Power distribution                     -
##---------------------------------------------------------------
f_power <- function(x, pars, log = FALSE){
  dpower(x, pars[1], pars[2], log = log)
}

F_power <- function(x, pars, log = FALSE){
  ppower(x, pars[1], pars[2], log.p = log)
}

S_power <- function(x, pars, log = FALSE){
  1 - F_power(x, pars, log = log)
}

h_power <- function(x, pars, log = FALSE) {
  f_power(x, pars, log = FALSE)/
    S_power(x, pars, log = FALSE)
}

H_power <- function(x, pars, log = FALSE) {
  ran <- 0.01
  t <- seq(0.01, max(x), ran)
  hh <- h_power(x = t, pars = pars)
  hh[is.nan(hh)] <- 0
  HH <- cumsum(hh)*ran
  H <- HH[sapply(x, function(mm) min(which(t >= mm)))]
  return(H)
}

q_power <- function(x, pars, log = FALSE) {
  qpower(x, pars[1], pars[2], log.p = log)
}
##---------------------------------------------------------------

