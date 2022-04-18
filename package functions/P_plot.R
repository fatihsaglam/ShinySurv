P_plot <- function(object,
                   curve_type = "Survival",
                   conf_interval = ifelse(any(names(object) == "pars_boot"), TRUE, FALSE),
                   conf_level = 0.95,
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
                   caption_hjust = 0) {
  
  p <- P_plot_init(object = object, 
                   curve_type = curve_type, 
                   conf_interval = conf_interval, 
                   conf_level = conf_level)
  
  p <- P_plot_adjust(p = p, 
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


P_plot_init <- function(object,
                        curve_type = "Survival",
                        conf_interval = ifelse(any(names(object) == "pars_boot"), TRUE, FALSE),
                        conf_level = 0.95
) {
  
  ##----------------------------------------------------------------
  ##                        error checking                         -
  ##----------------------------------------------------------------
  ## plot types
  if (!(curve_type%in%curve_type_names)) {
    stop(paste("Available types:", paste(curve_type_names, collapse = ", ")))
  }
  curve_type_plot <- curve_types[curve_type_names == curve_type]
  ##----------------------------------------------------------------
  
  life_tables <- object$life_tables
  group_levels <- names(life_tables)
  k <- length(group_levels)
  pars <- object$pars
  pars_boot <- object$pars_boot
  distribution <- object$distribution
  
  dist <- dists[dist_names == distribution]

  #################################################################
  ##                            plots                            ##
  #################################################################
  t <- seq(0, max(sapply(life_tables, function(mm) max(mm$time))), length.out = 250)
  
  FF <- match.fun(FUN = paste0(curve_type_plot, "_", dist))
  tbl_plt <- list()
  for (i in 1:k) {
    
    group <- rep(group_levels[i], length(t))
    curve <- FF(t, pars[[i]][,1])
    
    if (conf_interval) {
      curve_all <- t(apply(pars_boot[[i]], 1, function(m){
        FF(t, m)
      }))
      
      curve_upper_lower <- t(apply(curve_all, 2, function(m) quantile(m, c((1 - conf_level)/2, 1 - (1 - conf_level)/2), na.rm = TRUE)))
      curve_lower <- curve_upper_lower[,1]
      curve_upper <- curve_upper_lower[,2]
      
      tbl_plt[[i]] <- data.frame(time = t, curve, curve_lower, curve_upper, group)
    } else {
      tbl_plt[[i]] <- data.frame(time = t, curve, group)
    }
  }
  tbl_plt <- do.call(rbind, tbl_plt)
  tbl_plt$group <- as.factor(tbl_plt$group)
  
  p <- ggplot() +
    geom_line(mapping = aes(x = tbl_plt$time,
                            y = tbl_plt$curve,
                            color = tbl_plt$group),
              show.legend = ifelse(k > 1, TRUE, FALSE)) +
    scale_y_continuous(limits = c(0, max(curve)*1.05),
                       oob = squish)
  
  if (conf_interval) {
    p <- p + geom_ribbon(mapping = aes(x = tbl_plt$time,
                                       ymin = tbl_plt$curve_lower,
                                       ymax = tbl_plt$curve_upper,
                                       fill = tbl_plt$group),
                         show.legend = ifelse(k > 1, TRUE, FALSE),
                         alpha = 0.5)
  }

  
  #################################################################
  return(p)
}


P_plot_adjust <- function(p,
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
    stop(paste("Available colors:", paste(color_names, collapse = ", ")))
  }
  
  ## themes
  if (!(theme_type%in%theme_names)) {
    stop(paste("Available themes:", paste(theme_names, collapse = ", ")))
  }
  
  if (line_size < 0) {
    line_size <- 0.5
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
  
  
  
  p$layers[[1]]$aes_params$size <- line_size
  if (length(p$layers) == 2) {
    p$layers[[2]]$aes_params$alpha <- transparency
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
  
  return(p)
}
