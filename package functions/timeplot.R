##---------------------------------------------------------------
##                          Censor plot                         -
##---------------------------------------------------------------
timeplot <- function(time_1,
                     time_2 = NULL,
                     c_r = NULL,
                     c_l = NULL,
                     group = NULL) {
  ##---------------------------------------------------------------
  ##                          error check                         -
  ##---------------------------------------------------------------
  if (is.null(group)) {
    group <- as.factor(rep("group 1", length(time_1)))
  } else {
    group <- as.factor(group)
  }
  group_levels <- levels(group)

  if (is.null(time_2)) {
    time_2 <- time_1
    time_1 <- rep(0, length(time_1))
  }

  if (is.null(c_r)) {
    c_r <- rep(1, length(time_1))
  }

  if (is.null(c_l)) {
    c_l <- rep(1, length(time_1))
  }

  time_2[is.na(time_2)] <- Inf
  ##---------------------------------------------------------------

  ## number of data
  n <- length(time_1)

  ## ids
  id <- 1:n

  if(sum(is.infinite(time_2) > 0)) {
    c_r[is.infinite(time_2)] <- 0
    c_l[time_1 == 0] <- 0
  }

  ##---------------------------------------------------------------
  ##                          Orderding                           -
  ##---------------------------------------------------------------
  diff <- time_2 - time_1
  order_i <- sort.int(diff, index.return = TRUE)$ix
  time_2_ordered <- time_2[order_i]
  time_1_ordered <- time_1[order_i]
  c_r_ordered <- c_r[order_i]
  c_l_ordered <- c_l[order_i]
  group_ordered <- group[order_i]
  ##---------------------------------------------------------------

  ## time for lines
  time <- c(time_1_ordered, time_2_ordered)
  ## id for lines
  id2 <- as.factor(rep(1:n, 2))
  ## group for lines
  group2 <- rep(group_ordered, 2)

  ## censors for dots
  r <- factor(rep(c_r_ordered, 1), levels = c(0, 1),
              labels = c("censored", "not censored"))
  l <- factor(rep(c_l_ordered, 1), levels = c(0, 1),
              labels = c("censored", "not censored"))

  ##----------------------------------------------------------------
  ##                              Plot                             -
  ##----------------------------------------------------------------
  print(time_2_ordered)

  p <- ggplot() +
    geom_line(mapping = aes(x = time, y = id2, linetype = group2), size = 0.5,
              show.legend = length(group_levels) > 1) +
    geom_point(mapping = aes(x = time_1_ordered, y = id, shape = l, color = l)) +
    geom_point(mapping = aes(x = time_2_ordered, y = id, shape = r, color = r)) +
    scale_y_discrete(name = "") +
    scale_x_continuous(name = "t") +
    scale_color_d3(name = "") +
    scale_shape_discrete(name = "", solid = FALSE) +
    scale_linetype(name = "Group") +
    theme_bw() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  return(p)
  ##----------------------------------------------------------------
}
##---------------------------------------------------------------
