NP_tbls2flex1 <- function(tbls, n_round = 3){
  group_levels <- names(tbls)
  k <- length(group_levels)
  
  tbls_table <- list()
  for (i in 1:k) {
    tbls_table[[i]] <- round(tbls[[i]], n_round)
    tbls_table[[i]]$Group <- group_levels[i]
  }
  
  tbls_table <- do.call(rbind, tbls_table)[,c(4,1:3)]
  tbls_table_flex <- flextable::flextable(tbls_table, cwidth = 1.5625)
  tbls_table_flex <- flextable::hline_top(x = tbls_table_flex, border = fp_border(), part = "header")
  tbls_table_flex <- flextable::hline_top(x = tbls_table_flex, border = fp_border(), part = "body")
  tbls_table_flex <- flextable::hline_bottom(x = tbls_table_flex, border = fp_border(), part = "body")
  tbls_table_flex <- flextable::merge_v(tbls_table_flex,1)
  ss <- ((1:k)-1)*2
  tbls_table_flex <- flextable::hline(x = tbls_table_flex, 
                                      i = ss[ss!=0],
                                      border = fp_border(color = "black", style = "solid"))
  tbls_table_flex <- fix_border_issues(tbls_table_flex)
  return(tbls_table_flex)
}


NP_tbls2flex2 <- function(tbls, n_round = 3){
  group_levels <- names(tbls)
  k <- length(group_levels)
  
  tbls_table <- list()
  tbls_table_flex <- list()
  for (i in 1:k) {
    tbls_table[[i]] <- round(tbls[[i]], n_round)
    tbls_table[[i]]$Group <- group_levels[i]
    
    tbls_table_flex[[i]] <- flextable(tbls_table[[i]])
    tbls_table_flex[[i]] <- hline_top(x = tbls_table_flex[[i]], border = fp_border(), part = "header")
    tbls_table_flex[[i]] <- hline_top(x = tbls_table_flex[[i]], border = fp_border(), part = "body")
    tbls_table_flex[[i]] <- hline_bottom(x = tbls_table_flex[[i]], border = fp_border(), part = "body")
    tbls_table_flex[[i]] <- fontsize(tbls_table_flex[[i]], size = 7, part = "all")
    tbls_table_flex[[i]] <- autofit(tbls_table_flex[[i]], part = "all", add_w = 0, add_h = 0)
    tbls_table_flex[[i]] <- fix_border_issues(tbls_table_flex[[i]])
  }

  return(tbls_table_flex)
}
NP_tbls2flex3 <- function(tbls, n_round = 3){
  tbls <- data.frame(groups = tbls$groups, p = round(tbls$p, digits = n_round))
  tbls_flex <- flextable(tbls)
  tbls_flex <- hline_top(x = tbls_flex, border = fp_border(), part = "header")
  tbls_flex <- hline_top(x = tbls_flex, border = fp_border(), part = "body")
  tbls_flex <- hline_bottom(x = tbls_flex, border = fp_border(), part = "body")
  
  return(tbls_flex)
}

