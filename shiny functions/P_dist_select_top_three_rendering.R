dist_select_dist_select_rendering <- function(data, input){

  ## if error plot blank
  if ("try-error" %in% class(data)) {
    results <- data.frame()
    return(results)
  }

  isolate({
    if (input$dist_select_time_2 == "") {
      results <- data.frame()
      return(results)
    }

    time_2 <- data[,input$dist_select_time_2]
    n <- length(time_2)

    if (input$dist_select_time_1 == "Zero") {
      time_1 <- rep(0, n)
    } else {
      time_1 <- data[,input$dist_select_time_1]
    }

    if (input$dist_select_interval) {
      c_r <- NULL
      c_l <- NULL
    } else {
      if (input$dist_select_c_r == "No censor") {
        c_r <- rep(1, n)
      } else {
        c_r <- data[,input$dist_select_c_r]
      }
      if (input$dist_select_c_l == "No censor") {
        c_l <- rep(1, n)
      } else {
        c_l <- data[,input$dist_select_c_l]
      }
    }

    if (input$dist_select_group == "No group") {
      group <- NULL
    } else {
      group <- as.factor(data[,input$dist_select_group])
    }

    # print(time_1)
    # print(time_2)
    # print(c_r)
    # print(c_l)
    # print(group)

    set.seed(1)
    results <- P_dist_select(
      time_1 = time_1,
      time_2 = time_2,
      c_r = c_r,
      c_l = c_l,
      group = group,
      interval = input$dist_select_interval)

    return(results)
  })
}
