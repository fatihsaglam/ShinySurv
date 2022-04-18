##---------------------------------------------------------------
##                      Survival curve plot                     -
##---------------------------------------------------------------
NP_plot <- function(object,
                    curve_type = "Survival",
                    conf_interval = TRUE,
                    conf_level = 0.95,
                    line_color = "d3",
                    line_size = 0.5,
                    fill_color = "d3",
                    transparency = 0.5,
                    theme_type = "classic",
                    font_name = "Arial",
                    legend_title = "Groups",
                    legend_position = "right",
                    axis_x_title = "t",
                    axis_x_title_face = "plain",
                    axis_x_title_size = 12,
                    axis_x_title_hjust = 0.5,
                    axis_y_title = curve_type,
                    axis_y_title_face = "plain",
                    axis_y_title_size = 12,
                    axis_y_title_hjust = 0.5,
                    title = NULL,
                    title_face = "plain",
                    title_size = 12,
                    title_hjust = 0,
                    subtitle = NULL,
                    subtitle_face = "plain",
                    subtitle_size = 10,
                    subtitle_hjust = 0,
                    caption = NULL,
                    caption_face = "plain",
                    caption_size = 6,
                    caption_hjust = 0){
  
  p <- NP_plot_init(object = object, 
                    curve_type = curve_type, 
                    conf_interval = conf_interval, 
                    conf_level = conf_level)
  
  p <- NP_plot_adjust(p = p, 
                      line_color = line_color,
                      line_size = line_size,
                      fill_color = fill_color,
                      transparency = transparency,
                      theme_type = theme_type,
                      font_name = font_name,
                      legend_title = legend_title,
                      legend_position = legend_position,
                      axis_x_title = axis_x_title,
                      axis_x_title_face = axis_x_title_face,
                      axis_x_title_size = axis_x_title_size,
                      axis_x_title_hjust = axis_x_title_hjust,
                      axis_y_title = axis_y_title,
                      axis_y_title_face = axis_y_title_face,
                      axis_y_title_size = axis_y_title_size,
                      axis_y_title_hjust = axis_y_title_hjust,
                      title = title,
                      title_face = title_face,
                      title_size = title_size,
                      title_hjust = title_hjust,
                      subtitle = subtitle,
                      subtitle_face = subtitle_face,
                      subtitle_size = subtitle_size,
                      subtitle_hjust = subtitle_hjust,
                      caption = caption,
                      caption_face = caption_face,
                      caption_size = caption_size,
                      caption_hjust = caption_hjust)
  
  
  return(p)
}
##---------------------------------------------------------------



NP_plot_init <- function(object,
                         curve_type = "Survival",
                         conf_interval = TRUE,
                         conf_level = 0.95){
  
  
  ##----------------------------------------------------------------
  ##                        error checking                         -
  ##----------------------------------------------------------------
  ## curve types
  if (!(curve_type%in%curve_type_names)) {
    stop(paste("Wrong curve type. Available types:", paste(curve_type_names, collapse = ", ")))
  }
  type <- curve_types[curve_type_names == curve_type]
  ##----------------------------------------------------------------
  
  ## getting life tables
  tables <- object$life_tables
  ## number of groups
  k <- length(tables)
  group_levels <- names(tables)
  
  ## calculating early t for confidence intervals
  ## also h, f, F
  table <- lapply(1:k, function(m) {
    n <- nrow(tables[[m]])
    tables[[m]]$t_early <- c(tables[[m]]$t[-1], Inf)
    tables[[m]]$group <- rep(group_levels[m], n)
    tables[[m]]$h <- c(0, diff(tables[[m]]$H))
    tables[[m]]$f <- c(0, diff(1 - tables[[m]]$S)/diff(tables[[m]]$t))
    tables[[m]]$F <- 1 - tables[[m]]$S
    tables[[m]]$F_lower <- 1 - tables[[m]]$S_upper
    tables[[m]]$F_upper <- 1 - tables[[m]]$S_lower
    tables[[m]]
  })
  
  ## combining tables for ggplot2
  tables_dataframe <- do.call(rbind, table)
  
  #################################################################
  ##                            plots                            ##
  #################################################################
  curve_code <- curve_types[curve_type_names == curve_type]
  
  if (curve_code %in% c("S","F","H")) {
    t <- tables_dataframe$t
    t_early <- tables_dataframe$t_early
    FF <- tables_dataframe[,curve_code]
    FF_lower <- tables_dataframe[,paste0(curve_code, "_lower")]
    FF_upper <- tables_dataframe[,paste0(curve_code, "_upper")]
    Group <- tables_dataframe$group
    
    p <- ggplot()
    
    if (conf_interval) {
      p <- p +
        geom_rect(mapping = aes(xmin = t_early,
                                xmax = t,
                                ymin = FF_lower,
                                ymax = FF_upper,
                                fill = Group),
                  show.legend = ifelse(k > 1, TRUE, FALSE),
                  alpha = 0.5)
    }
    
    p <- p +
      geom_step(mapping = aes(x = t, y = FF, color = Group),
                show.legend = ifelse(k > 1, TRUE, FALSE)) +
      scale_y_continuous(limits = c(0, max(FF)*1.05),
                         oob = squish,
                         name = "") +
      scale_x_continuous(name = "")
  }
  
  if (curve_code %in% c("f", "h")) {
    d <- tables_dataframe$d
    t <- tables_dataframe$t
    ff <- tables_dataframe[,curve_code]
    Group <- tables_dataframe$group
    
    tt <- t[d > 0]
    fff <- ff[d > 0]
    Group <- Group[d > 0]
    
    print(Group)
    
    if (any(table(Group) < 13)) {
      p <- ggplot() +
        geom_smooth(mapping = aes(x = tt, y = fff, color = Group), 
                    se = FALSE, 
                    size = 0.5, 
                    method = "loess")+
        scale_y_continuous(limits = c(0, max(fff)*1.05),
                           oob = squish,
                           name = "") +
        scale_x_continuous(name = "t")
    } else {
      p <- ggplot() +
        geom_smooth(mapping = aes(x = tt, y = fff, color = Group), 
                    se = FALSE, 
                    size = 0.5, 
                    method = "gam", 
                    formula = y ~ s(x, bs = "tp"))+
        scale_y_continuous(limits = c(0, max(fff)*1.05),
                           oob = squish,
                           name = "") +
        scale_x_continuous(name = "t")
    }
  }
  
  return(p)
}

NP_plot_adjust <- function(p,
                           line_color = "d3",
                           line_size = 0.5,
                           fill_color = "d3",
                           transparency = 0.5,
                           theme_type = "bw",
                           font_name = "Arial",
                           legend_title = "Groups",
                           legend_position = "right",
                           axis_x_title = "t",
                           axis_x_title_face = "plain",
                           axis_x_title_size = 12,
                           axis_x_title_hjust = 0.5,
                           axis_y_title = "",
                           axis_y_title_face = "plain",
                           axis_y_title_size = 12,
                           axis_y_title_hjust = 0.5,
                           title = NULL,
                           title_face = "plain",
                           title_size = 12,
                           title_hjust = 0,
                           subtitle = NULL,
                           subtitle_face = "plain",
                           subtitle_size = 10,
                           subtitle_hjust = 0,
                           caption = NULL,
                           caption_face = "plain",
                           caption_size = 6,
                           caption_hjust = 0) {
  
  
  ##----------------------------------------------------------------
  ##                        error checking                         -
  ##----------------------------------------------------------------
  ## colors
  if (!(line_color%in%color_names) | !(fill_color%in%color_names)) {
    stop(paste("Wrong color name. Available colors:", paste(color_names, collapse = ", ")))
  }
  
  ## themes
  if (!(theme_type%in%theme_names)) {
    stop(paste("Wrong theme name. Available themes:", paste(theme_names, collapse = ", ")))
  }
  
  ## legend position
  if (!(legend_position%in%legend_positions)) {
    stop(paste("Wrong legend position. Available positions are:", paste(legend_positions, collapse = ", ")))
  }
  ##----------------------------------------------------------------
  
  ##----------------------------------------------------------------
  ##                            Colors                             -
  ##----------------------------------------------------------------
  scale_color <- match.fun(paste0("scale_color_", line_color))
  scale_fill <- match.fun(paste0("scale_fill_", fill_color))
  ##----------------------------------------------------------------
  
  ##----------------------------------------------------------------
  ##                            themes                             -
  ##----------------------------------------------------------------
  theme_p <- match.fun(FUN = paste0("theme_", theme_type))
  ##----------------------------------------------------------------
  
  p <- p +
    scale_x_continuous(name = axis_x_title) +
    scale_y_continuous(name = axis_y_title, limits = p$scales$scales[[1]]$limits, oob = squish) +
    scale_color(name = legend_title) + 
    scale_fill(name = legend_title) +
    theme_p()
  
  if (length(p$layers) == 1) {
    p$layers[[1]]$aes_params$size <- line_size
  }
  if (length(p$layers) == 2) {
    p$layers[[2]]$aes_params$size <- line_size
    p$layers[[1]]$aes_params$alpha <- transparency
  }
  
  p <- p + 
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme(plot.title = element_text(face = title_face, size = title_size, hjust = title_hjust), 
          plot.subtitle = element_text(face = subtitle_face, size = subtitle_size, hjust = subtitle_hjust), 
          plot.caption = element_text(face = caption_face, size = caption_size, hjust = caption_hjust), 
          axis.title.x = element_text(face = axis_x_title_face, size = axis_x_title_size, hjust = axis_x_title_hjust),
          axis.title.y = element_text(face = axis_y_title_face, size = axis_y_title_size, hjust = axis_y_title_hjust),
          legend.position = legend_position,
          text = element_text(family = font_name))
}
