NP_compare_curves <- function(object, comp_type = "logrank", p_adj_method = "holm") {
  #### error checking ####
  if (!(p_adj_method %in% p.adjust.methods)) {
    stop(paste0("Wrong adjustment method. Available methods are: ", paste0("'", p.adjust.methods, "'", collapse = ", "), "."))
  }

  compare_test <- getFunction(name = paste0("NP_", comp_type, "_test"))

  k <- length(object$mean_median_tbls)
  group_levels <- names(object$mean_median_tbls)
  
  if (k < 2) {
    return(data.frame())
  }

  ps <- combn(k, 2, FUN = function(mm) {
    mm_new <- list()
    mm_new$mean_median_tbls <- object$mean_median_tbls[mm]
    mm_new$life_tables <- object$life_tables[mm]
    
    p <- compare_test(object = mm_new)$p_sig
    return(p)
  })

  ps_names <- combn(k, 2, FUN = function(mm) {
    paste0(group_levels[mm], collapse = "-")
  })
  names(ps) <- ps_names

  p_adjusted <- p.adjust(ps, method = p.adjust.methods[1])
  results <- data.frame(groups = ps_names, p = p_adjusted)

  return(results)
}

NP_logrank_test <- function(object) {
  group_levels <- names(object$life_tables)
  
  t_all <- c()
  d_all <- c()
  group_all <- c()
  for (i in 1:length(group_levels)) {
    t_all <- c(t_all,  object$life_tables[[i]]$t)
    d_all <- c(d_all,  object$life_tables[[i]]$d)
    group_all <- c(group_all, rep(group_levels[i], length(t_all)))
  }
  
  t_sort_i <- sort.int(t_all, index.return = TRUE)$ix
  t_all <- t_all[t_sort_i]
  d_all <- d_all[t_sort_i]
  group_all <- group_all[t_sort_i]
  
  Y_per_group <- data.frame(matrix(NA, ncol = length(group_levels), nrow = length(t_all)))
  d_per_group <- data.frame(matrix(NA, ncol = length(group_levels), nrow = length(t_all)))
  
  for (i in 1:length(group_levels)) {
    tbl <- object$life_tables[[i]]
    for (j in 1:length(t_all)) {
      t_i_y <- min(which(tbl$t >= t_all[j]))
      t_i_d <- which(tbl$t == t_all[j] & group_all[j] == group_levels[i])
      Y_per_group[j,i] <- tbl$Y[t_i_y]
      d_per_group[j,i] <- ifelse(identical(t_i_d, integer(0)), 0, tbl$d[t_i_d]) 
    }
  }
  
  Y_all <- rowSums(Y_per_group, na.rm = TRUE)
  d_all <- rowSums(d_per_group, na.rm = TRUE)
  
  e_per_group <- apply(Y_per_group, 2, function(m) m*d_all/Y_all)
  U_L <- sum(d_per_group[,1] - e_per_group[,1], na.rm = TRUE)
  v_1 <- apply(Y_per_group, 1, function(m) prod(m, na.rm = TRUE))*d_all*(Y_all- d_all)/
    (Y_all^2*(Y_all - 1))
  U_L_var <- sum(v_1, na.rm = TRUE)
  W_L <- U_L^2/U_L_var
  p_sig <- 1 - pchisq(W_L, 1)
  results <- list(W_L = W_L,
                  p_sig = p_sig)
  return(results)
}

## (Texts in statistical science) Collett, D - Modelling survival data in medical research-CRC Press (2015)
NP_wilcoxon_test <- function(object) {
  group_levels <- names(object$life_tables)
  
  t_all <- c()
  d_all <- c()
  group_all <- c()
  for (i in 1:length(group_levels)) {
    t_all <- c(t_all,  object$life_tables[[i]]$t)
    d_all <- c(d_all,  object$life_tables[[i]]$d)
    group_all <- c(group_all, rep(group_levels[i], length(t_all)))
  }
  
  t_sort_i <- sort.int(t_all, index.return = TRUE)$ix
  t_all <- t_all[t_sort_i]
  d_all <- d_all[t_sort_i]
  group_all <- group_all[t_sort_i]
  
  Y_per_group <- data.frame(matrix(NA, ncol = length(group_levels), nrow = length(t_all)))
  d_per_group <- data.frame(matrix(NA, ncol = length(group_levels), nrow = length(t_all)))
  
  for (i in 1:length(group_levels)) {
    tbl <- object$life_tables[[i]]
    for (j in 1:length(t_all)) {
      t_i_y <- min(which(tbl$t >= t_all[j]))
      t_i_d <- which(tbl$t == t_all[j] & group_all[j] == group_levels[i])
      Y_per_group[j,i] <- tbl$Y[t_i_y]
      d_per_group[j,i] <- ifelse(identical(t_i_d, integer(0)), 0, tbl$d[t_i_d]) 
    }
  }
  
  Y_all <- rowSums(Y_per_group, na.rm = TRUE)
  d_all <- rowSums(d_per_group, na.rm = TRUE)
  
  e_per_group <- apply(Y_per_group, 2, function(m) m*d_all/Y_all)
  U_W <- sum(Y_all*(d_per_group[,1] - e_per_group[,1]), na.rm = TRUE)
  v_1 <- apply(Y_per_group, 1, function(m) prod(m, na.rm = TRUE))*d_all*(Y_all- d_all)/
    (Y_all^2*(Y_all - 1))
  U_W_var <- sum(Y_all^2*v_1, na.rm = TRUE)
  W_W <- U_W^2/U_W_var
  p_sig <- 1 - pchisq(W_W, 1)
  results <- list(W_W = W_W,
                  p_sig = p_sig)
  return(results)
}
## (Texts in statistical science) Collett, D - Modelling survival data in medical research-CRC Press (2015)



