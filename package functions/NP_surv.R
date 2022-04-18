NP_surv <- function(time_1,
                    time_2 = NULL,
                    c_r = NULL,
                    c_l = NULL,
                    group = NULL,
                    conf_method = "loglog",
                    conf_level = 0.95,
                    interval = FALSE) {
  
  
  if (!interval) {
    mm <- NP_surv_nointerval(time_1 = time_1,
                             time_2 = time_2,
                             c_r = c_r,
                             c_l = c_l,
                             group = group,
                             conf_method = conf_method,
                             conf_level = conf_level)
    return(mm)
  } else{
    mm <- NP_surv_interval(time_1 = time_1,
                           time_2 = time_2,
                           group = group,
                           conf_method = conf_method,
                           conf_level = conf_level)

    return(mm)
  }
}
