# PACKAGES ----
library(remotes)
if(!require(Rttf2pt1)) {
  remotes::install_version("Rttf2pt1", version = "1.3.8")
}
library(ggplot2)
library(ggsci)
library(scales)
library(survival)
library(km.ci)
library(flexsurv)
library(extraDistr)
library(shiny)
library(shinyEffects)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(esquisse)
library(datamods)
library(fresh)
library(knitr)
library(kableExtra)
library(tibble)
library(htmltools)
library(shinybusy)
library(shinyEffects)
library(shinyalert)
library(phosphoricons)
library(rmarkdown)
library(bookdown)
library(officer)
library(flextable)

## FILES ----
### Package Files ----
source(file = "package functions/names.R")

source(file = "package functions/NP_plot.R")
source(file = "package functions/NP_surv.R")
source(file = "package functions/NP_surv_interval.R")
source(file = "package functions/NP_surv_nointerval.R")
source(file = "package functions/NP_compare_curves.R")

source(file = "package functions/P_dist_select.R")
source(file = "package functions/P_distribution_functions.R")
source(file = "package functions/P_par_est_mle.R")
source(file = "package functions/P_plot.R")
source(file = "package functions/P_surv.R")
source(file = "package functions/P_JKBoot.R")
source(file = "package functions/P_NPBoot.R")

source(file = "package functions/timeplot.R")

### shiny files ----
source(file = "shiny functions/sidebar.R")

source(file = "shiny functions/P_dist_select_top_three_rendering.R")
source(file = "shiny functions/P_tab_items.R")
source(file = "shiny functions/P_tab.R")
source(file = "shiny functions/P_report_tbls2flex.R")

source(file = "shiny functions/NP_tab_items.R")
source(file = "shiny functions/NP_tab.R")
source(file = "shiny functions/NP_report_tbls2flex.R")

source(file = "shiny functions/datasummary_tab_items.R")
source(file = "shiny functions/datasummary_tab.R")

source(file = "shiny functions/dist_selection_tab_items.R")
source(file = "shiny functions/dist_selection_tab.R")
### shiny files ----
data_int <- read.csv(file = "data/interval censor data example.csv")
data_r <- read.csv(file = "data/right censor data example.csv")
data_l <- read.csv(file = "data/left censor data example.csv")
lung <- survival::lung
lung$status <- lung$status - 1
lung <- lung[complete.cases(lung),]
ovarian <- survival::ovarian

# THEME ----
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E",
    yellow = "#FF7F0EFF"
  ),
  adminlte_sidebar(width = "500px"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)

# HEADER ----
header <- dashboardHeader(
  titleWidth = "500px",
  title = "ShinySurv",
  controlbarIcon = shiny::icon("gears", verify_fa = FALSE))



#  SIDEBAR ----
sidebar <- dashboardSidebar(width = "500px",
                            sidebar, 
                            minified = FALSE
)

# BODY ----
body <- dashboardBody(
  add_loading_state(selector = ".shiny-plot-output", 
                    text = "Please wait...", 
                    svgColor = "steelblue"),
  # setShadow(class = "data_vars"),
  # setShadow(class = "button"),
  # setShadow(id = "P_input_generate"),
  # setShadow(id = "P_input_adjust"),
  # setShadow(id = "P_input_report"),
  add_busy_spinner(spin = "atom"),
  tabBox(width = 12, 
         P_tab,
         NP_tab,
         dist_selection_tab,
         datasummary_tab
  )
)

#  UI ----
ui <- dashboardPage(title = "Survival Analysis", 
                    header = header,
                    sidebar = sidebar,
                    body = body,
                    freshTheme = mytheme,
)

# SERVER ----
server <- function(input, output, session) {
  
  ##  Data Load ----
  reader <- reactive({
    if (input$load_import_data == "Import data") {
      dat <- try(read.csv(file = input$import_data_path$datapath,
                          header = input$header),
                 silent = TRUE)
      if ("try-error" %in% class(dat)) {
        dat <- data.frame()
        return(dat)
      }
      dat <- dat[complete.cases(dat),]
      return(dat)
    }
    
    if (input$load_import_data == "Load data") {
      if (input$loaded_data == "right censored") {
        dat <- data_r
      }
      if (input$loaded_data == "left and right censored") {
        dat <- data_l
      }
      if (input$loaded_data == "interval censored") {
        dat <- data_int
      }
      if (input$loaded_data == "lung") {
        dat <- lung
      }
      if (input$loaded_data == "ovarian") {
        dat <- ovarian
      }
    }
    
    return(dat)
  })
  
  observeEvent(eventExpr = input$import_data_path, {
    output$import_data_preview_import <- renderPrint({
      data <- reader()
      print(tibble(data))
    })
  })
  
  observeEvent(eventExpr = input$loaded_data, {
    output$import_data_preview_load <- renderPrint({
      data <- reader()
      print(tibble(data))
    })
  })  
  
  observeEvent(eventExpr = input$import_data, {
    data <- reader()
    colnames_data <- colnames(data)
    
    if (!is.null(input$data_vars) & length(colnames_data) != 0) {
      updateRadioGroupButtons(session = session, 
                              inputId = "data_vars", 
                              choiceNames = as.tags(lapply(colnames_data, function(m) {
                                paste0("<span class = 'label label-success'>",m,"</span>")
                              })), 
                              choiceValues = colnames_data,
                              disabled = FALSE,
                              selected = colnames_data[1])
    }
  })
  
  observeEvent(eventExpr = input$load_data, {
    data <- reader()
    colnames_data <- colnames(data)
    
    if (!is.null(input$data_vars) & length(colnames_data) != 0) {
      updateRadioGroupButtons(session = session, 
                              inputId = "data_vars", 
                              choiceNames = as.tags(lapply(colnames_data, function(m) {
                                paste0("<span class = 'label label-success'>",m,"</span>")
                              })), 
                              choiceValues = colnames_data,
                              disabled = FALSE, 
                              selected = colnames_data[1])
    }
    
  })
  
  ## Variable imported or loaded ----
  lapply(1:length(surv_var_ids), function(m) {
    id <- paste(surv_var_ids[m])
    label <- paste(surv_var_labels[m])
    add_id <- paste0(id, "_add")
    
    observeEvent(eventExpr = input[[add_id]], {
      
      if ((is.null(input[[id]]) | input[[id]] == "" | input[[id]] == "No Variable") & (!is.null(input$data_vars) & input$data_vars != "No Variable")) {
        updatePickerInput(inputId = id, session = session, choices = input$data_vars, selected = input$data_vars,
                          choicesOpt = list(content = paste0("<span class = 'label label-success'>", input$data_vars, "</span>")))
      } else {
        updatePickerInput(inputId = id, session = session, choices = "No Variable", selected = "No Variable",
                          choicesOpt = list(content = "<span class = 'label label-danger'>No Variable</span>"))
      }
      if ((is.null(input[[id]]) | input[[id]] == "" | input[[id]] == "No Variable") & (!is.null(input$data_vars) & input$data_vars != "No Variable")) {
        updateActionButton(session = session, inputId = add_id, icon = icon("arrow-left"))
      } else {
        updateActionButton(session = session, inputId = add_id, icon = icon("arrow-right"))
      }
      
    })
  })
  
  ## Parametric ----
  ### m_P_surv_f ----
  m_P_surv_f <- reactive({
    data <- reader()
    
    if (is.null(input$time_1) | input$time_1 == "No Variable") {
      time_1 <- NULL
    } else {
      time_1 <- data[,input$time_1]
    }
    if (is.null(input$time_2) | input$time_2 == "No Variable") {
      time_2 <- NULL
    } else {
      time_2 <- data[,input$time_2]
    }
    if (is.null(input$c_r) | input$c_r == "No Variable") {
      c_r <- NULL
    } else {
      c_r <- data[,input$c_r]
    }
    if (is.null(input$c_l) | input$c_l == "No Variable") {
      c_l <- NULL
    } else {
      c_l <- data[,input$c_l]
    }
    if (is.null(input$group) | input$group == "No Variable") {
      group <- NULL
    } else {
      group <- data[,input$group]
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable")) {
      if (is.null(time_1) | input$time_1 == "No Variable") {
        time_1 <- time_2
        time_2 <- NULL
      }
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") & (is.null(time_2) | input$time_2 == "No Variable")) {
      show_alert(
        title = "Error !!",
        text = "At least one time variable must be selected.",
        type = "error"
      )
      return(NULL)
    }
    
    if (input$interval & ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable"))) {
      show_alert(
        title = "Error!!",
        text = "Both time variable must be selecter for interval censored data.",
        type = "error"
      )
      return(NULL)
    }
    
    if ((!is.numeric(time_1) & !is.null(time_1)) | (!is.numeric(time_2) & !is.null(time_2))) {
      show_alert(
        title = "Error!!",
        text = "Time variables must be numeric.",
        type = "error"
      )
      return(NULL)
    }
    
    
    # print(time_1)
    # print(time_2)
    # print(c_r)
    # print(c_l)
    # print(group)
    # print(input$P_input_distribution)
    # print(input$P_input_conf_interval)
    # print(input$interval)
    # print(input$P_input_n_boot)
    # print(input$P_input_bootstrap_type)
    
    m_P_surv <- P_surv(
      time_1 = time_1,
      time_2 = time_2,
      c_r = c_r,
      c_l = c_l,
      group = group,
      distribution = input$P_input_distribution, 
      tolerance = 1e-10, 
      maxiter = 500, 
      conf_interval = input$P_input_conf_interval, 
      conf_level = as.numeric(input$P_input_conf_level), 
      interval = input$interval, 
      n_boot = input$P_input_n_boot, 
      init_pars = NULL, 
      bootstrap_type = bootstraps[bootstrap_names == input$P_input_bootstrap_type]
    )
    
    return(m_P_surv)
  })
  
  ### m_P_plot_f ----
  m_P_plot_f <- reactive({
    
    m_P_surv <- m_P_surv_f()
    if (is.null(m_P_surv)) {
      return(NULL)
    }
    
    m_P_plot <- P_plot(
      object = m_P_surv, 
      curve_type = input$P_input_curve_type,
      conf_interval = input$P_input_conf_interval,
      conf_level = as.numeric(input$P_input_conf_level),
      line_color = input$P_input_line_color,
      line_size = input$P_input_line_size,
      fill_color = input$P_input_fill_color,
      transparency = input$P_input_transparency,
      theme_type = input$P_input_theme_type,
      font_name = input$P_input_font_name,
      legend_title = input$P_input_legend_title,
      legend_position = input$P_input_legend_position,
      axis_x_title = input$P_input_axis_x_title,
      axis_x_title_face = input$P_input_axis_x_title_face,
      axis_x_title_size = input$P_input_axis_x_title_size,
      axis_x_title_hjust = input$P_input_axis_x_title_hjust,
      axis_y_title = input$P_input_axis_y_title,
      axis_y_title_face = input$P_input_axis_y_title_face,
      axis_y_title_size = input$P_input_axis_y_title_size,
      axis_y_title_hjust = input$P_input_axis_y_title_hjust,
      title = input$P_input_title,
      title_face = input$P_input_title_face,
      title_size = input$P_input_title_size,
      title_hjust = input$P_input_title_hjust,
      subtitle = input$P_input_subtitle,
      subtitle_face = input$P_input_subtitle_face,
      subtitle_size = input$P_input_subtitle_size,
      subtitle_hjust = input$P_input_subtitle_hjust,
      caption = input$P_input_caption,
      caption_face = input$P_input_caption_face,
      caption_size = input$P_input_caption_size,
      caption_hjust = input$P_input_caption_hjust
    )
    
    return(m_P_plot)
  })
  
  ### input$P_input_generate ----
  observeEvent(eventExpr = input$P_input_generate, {
    m_P_surv <- m_P_surv_f()
    m_P_plot <- m_P_plot_f()
    
    output$P_output_plot <- renderPlot(
      res = 72,
      type = "cairo",
      {
        m_P_plot
      })
    
    #### output$P_output_means_medians ----
    output$P_output_means_medians <- renderPrint({
      for (i in 1:length(m_P_surv$mean_median_tbls)) {
        cat(names(m_P_surv$mean_median_tbls)[i], ":")
        print(kable(m_P_surv$mean_median_tbls[[i]], format = "pipe", digits = 3))
        cat(rep("=", 30), "\n", sep = "")
      }
    })
    
    #### output$P_output_parameters ----
    output$P_output_parameters <- renderPrint({
      for (i in 1:length(m_P_surv$pars)) {
        cat(names(m_P_surv$pars)[i], ":")
        print(kable(m_P_surv$pars[[i]], format = "pipe", digits = 3))
        cat(rep("=", 30), "\n", sep = "")
      }
    })
    
    #### output$P_output_life_tables ----
    output$P_output_life_tables <- renderPrint({
      for (i in 1:length(m_P_surv$life_tables)) {
        cat(names(m_P_surv$life_tables)[i], ":")
        print(kable(m_P_surv$life_tables[[i]], format = "pipe", digits = 3))
        cat(rep("=", 30), "\n", sep = "")
      }
    })
  })
  
  ### m_P_plot_adjust_f ----
  m_P_plot_adjust_f <- reactive({
    m_P_plot <- m_P_plot_f()
    if (is.null(m_P_plot)) {
      return(NULL)
    }
    
    m_P_plot <- P_plot_adjust(
      p = m_P_plot,
      line_color = input$P_input_line_color,
      line_size = input$P_input_line_size,
      fill_color = input$P_input_fill_color,
      transparency = input$P_input_transparency,
      theme_type = input$P_input_theme_type,
      font_name = input$P_input_font_name,
      legend_title = input$P_input_legend_title,
      legend_position = input$P_input_legend_position,
      axis_x_title = input$P_input_axis_x_title,
      axis_x_title_face = input$P_input_axis_x_title_face,
      axis_x_title_size = input$P_input_axis_x_title_size,
      axis_x_title_hjust = input$P_input_axis_x_title_hjust,
      axis_y_title = input$P_input_axis_y_title,
      axis_y_title_face = input$P_input_axis_y_title_face,
      axis_y_title_size = input$P_input_axis_y_title_size,
      axis_y_title_hjust = input$P_input_axis_y_title_hjust,
      title = input$P_input_title,
      title_face = input$P_input_title_face,
      title_size = input$P_input_title_size,
      title_hjust = input$P_input_title_hjust,
      subtitle = input$P_input_subtitle,
      subtitle_face = input$P_input_subtitle_face,
      subtitle_size = input$P_input_subtitle_size,
      subtitle_hjust = input$P_input_subtitle_hjust,
      caption = input$P_input_caption,
      caption_face = input$P_input_caption_face,
      caption_size = input$P_input_caption_size,
      caption_hjust = input$P_input_caption_hjust
    )
  }) %>%
    bindEvent(input$P_input_adjust)
  
  ### input$P_input_adjust ----
  observeEvent(eventExpr = input$P_input_adjust, {
    output$P_output_plot <- renderPlot(
      res = 72,
      type = "cairo",
      {
        m_P_plot <- m_P_plot_f()
        
        if (is.null(m_P_plot)) {
          return(NULL)
        }
        
        m_P_plot_adjust <- m_P_plot_adjust_f()
        return(m_P_plot_adjust)
      })
  })
  
  ### output$P_input_report ----
  output$P_input_report <- downloadHandler(
    filename = function() {
      file_name <- paste0("my report.docx")
      return(file_name)
    },
    
    content = function(file) {
      params <- list(
        results = m_P_surv_f(),
        plot = m_P_plot_f(),
        curve_type = input$P_curve_type
      )
      
      rmarkdown::render(input = "shiny functions/P_report.Rmd",
                        output_file = file,
                        output_format = word_document2(reference_docx = "style_template.docx"),
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  ### output$P_input_graph_download ----
  output$P_input_graph_download <- downloadHandler(
    filename = function() {
      file_name <- paste0("parametric.", input$P_input_graph_file_type)
      return(file_name)
    },
    
    content = function(file) {
      ggsave(filename = file, 
             plot = m_P_plot_f(),
             device = input$P_input_graph_file_type, 
             width = input$P_input_graph_width, 
             height = input$P_input_graph_height, 
             dpi = input$P_input_graph_dpi)
      
    }
  )
  
  ## Non-Parametric ----
  ### m_NP_surv_f ----
  m_NP_surv_f <- reactive({
    
    data <- reader()
    
    if (is.null(input$time_1) | input$time_1 == "No Variable") {
      time_1 <- NULL
    } else {
      time_1 <- data[,input$time_1]
    }
    if (is.null(input$time_2) | input$time_2 == "No Variable") {
      time_2 <- NULL
    } else {
      time_2 <- data[,input$time_2]
    }
    if (is.null(input$c_r) | input$c_r == "No Variable") {
      c_r <- NULL
    } else {
      c_r <- data[,input$c_r]
    }
    if (is.null(input$c_l) | input$c_l == "No Variable") {
      c_l <- NULL
    } else {
      c_l <- data[,input$c_l]
    }
    if (is.null(input$group) | input$group == "No Variable") {
      group <- NULL
    } else {
      group <- data[,input$group]
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable")) {
      if (is.null(time_1) | input$time_1 == "No Variable") {
        time_1 <- time_2
        time_2 <- NULL
      }
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") & (is.null(time_2) | input$time_2 == "No Variable")) {
      show_alert(
        title = "Error!!",
        text = "At least one time variable must be selected.",
        type = "error"
      )
      return(NULL)
    }
    
    if (input$interval & ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable"))) {
      show_alert(
        title = "Error!!",
        text = "Both time variable must be selecter for interval censored data.",
        type = "error"
      )
      return(NULL)
    }
    
    if ((!is.numeric(time_1) & !is.null(time_1)) | (!is.numeric(time_2) & !is.null(time_2))) {
      show_alert(
        title = "Error!!",
        text = "Time variables must be numeric.",
        type = "error"
      )
      return(NULL)
    }
    
    # print(time_1)
    # print(time_2)
    # print(c_r)
    # print(c_l)
    # print(group)
    # print(input$NP_input_conf_method)
    # print(input$NP_input_conf_level)
    # print(input$interval)
    # 
    
    m_NP_surv <- NP_surv(
      time_1 = time_1,
      time_2 = time_2,
      c_r = c_r,
      c_l = c_l,
      group = group, 
      conf_method = input$NP_input_conf_method,
      conf_level = as.numeric(input$NP_input_conf_level), 
      interval = input$interval
    )
    
    return(m_NP_surv)
  })
  
  ### m_NP_plot_f ----
  m_NP_plot_f <- reactive({
    m_NP_surv <- m_NP_surv_f()
    if (is.null(m_NP_surv)) {
      return(NULL)
    }
    m_NP_plot <- NP_plot(
      object = m_NP_surv, 
      curve_type = input$NP_input_curve_type,
      conf_interval = input$NP_input_conf_interval,
      conf_level = as.numeric(input$NP_input_conf_level),
      line_color = input$NP_input_line_color,
      line_size = input$NP_input_line_size,
      fill_color = input$NP_input_fill_color,
      transparency = input$NP_input_transparency,
      theme_type = input$NP_input_theme_type,
      font_name = input$NP_input_font_name,
      legend_title = input$NP_input_legend_title,
      legend_position = input$NP_input_legend_position,
      axis_x_title = input$NP_input_axis_x_title,
      axis_x_title_face = input$NP_input_axis_x_title_face,
      axis_x_title_size = input$NP_input_axis_x_title_size,
      axis_x_title_hjust = input$NP_input_axis_x_title_hjust,
      axis_y_title = input$NP_input_axis_y_title,
      axis_y_title_face = input$NP_input_axis_y_title_face,
      axis_y_title_size = input$NP_input_axis_y_title_size,
      axis_y_title_hjust = input$NP_input_axis_y_title_hjust,
      title = input$NP_input_title,
      title_face = input$NP_input_title_face,
      title_size = input$NP_input_title_size,
      title_hjust = input$NP_input_title_hjust,
      subtitle = input$NP_input_subtitle,
      subtitle_face = input$NP_input_subtitle_face,
      subtitle_size = input$NP_input_subtitle_size,
      subtitle_hjust = input$NP_input_subtitle_hjust,
      caption = input$NP_input_caption,
      caption_face = input$NP_input_caption_face,
      caption_size = input$NP_input_caption_size,
      caption_hjust = input$NP_input_caption_hjust
    )
    
    return(m_NP_plot)
  })
  
  ### input$NP_input_generate ----
  observeEvent(eventExpr = input$NP_input_generate, {
    m_NP_surv <- m_NP_surv_f()
    m_NP_plot <- m_NP_plot_f()
    
    output$NP_output_plot <- renderPlot(
      res = 72,
      type = "cairo",
      {
        m_NP_plot
      })
    
    #### output$NP_output_means_medians ----
    output$NP_output_means_medians <- renderPrint({
      for (i in 1:length(m_NP_surv$mean_median_tbls)) {
        cat(names(m_NP_surv$mean_median_tbls)[i], ":")
        print(kable(m_NP_surv$mean_median_tbls[[i]], format = "pipe", digits = 3))
        cat(rep("=", 30), "\n", sep = "")
      }
    })
    
    #### output$NP_output_comparison_tests ----
    output$NP_output_comparison_tests <- renderPrint({
      tbl <- NP_compare_curves(object = m_NP_surv,
                               comp_type = input$NP_input_comp_type,
                               p_adj_method = input$NP_input_comp_p_adj_method)
      
      print(kable(tbl, format = "pipe", digits = 3, row.names = FALSE))
    })
    
    #### output$NP_output_life_tables ----
    output$NP_output_life_tables <- renderPrint({
      for (i in 1:length(m_NP_surv$life_tables)) {
        cat(names(m_NP_surv$life_tables)[i], ":")
        print(kable(m_NP_surv$life_tables[[i]], format = "pipe", digits = 3))
        cat(rep("=", 30), "\n", sep = "")
      }
    })
  })
  
  ### m_NP_plot_adjust_f ----
  m_NP_plot_adjust_f <- reactive({
    m_NP_plot <- m_NP_plot_f()
    if (is.null(m_NP_plot)) {
      return(NULL)
    }
    
    m_NP_plot <- NP_plot_adjust(
      p = m_NP_plot,
      line_color = input$NP_input_line_color,
      line_size = input$NP_input_line_size,
      fill_color = input$NP_input_fill_color,
      transparency = input$NP_input_transparency,
      theme_type = input$NP_input_theme_type,
      font_name = input$NP_input_font_name,
      legend_title = input$NP_input_legend_title,
      legend_position = input$NP_input_legend_position,
      axis_x_title = input$NP_input_axis_x_title,
      axis_x_title_face = input$NP_input_axis_x_title_face,
      axis_x_title_size = input$NP_input_axis_x_title_size,
      axis_x_title_hjust = input$NP_input_axis_x_title_hjust,
      axis_y_title = input$NP_input_axis_y_title,
      axis_y_title_face = input$NP_input_axis_y_title_face,
      axis_y_title_size = input$NP_input_axis_y_title_size,
      axis_y_title_hjust = input$NP_input_axis_y_title_hjust,
      title = input$NP_input_title,
      title_face = input$NP_input_title_face,
      title_size = input$NP_input_title_size,
      title_hjust = input$NP_input_title_hjust,
      subtitle = input$NP_input_subtitle,
      subtitle_face = input$NP_input_subtitle_face,
      subtitle_size = input$NP_input_subtitle_size,
      subtitle_hjust = input$NP_input_subtitle_hjust,
      caption = input$NP_input_caption,
      caption_face = input$NP_input_caption_face,
      caption_size = input$NP_input_caption_size,
      caption_hjust = input$NP_input_caption_hjust
    )
  }) %>%
    bindEvent(input$NP_input_adjust)
  
  ### input$NP_input_adjust ----
  observeEvent(eventExpr = input$NP_input_adjust, {
    output$NP_output_plot <- renderPlot(
      res = 72,
      type = "cairo",
      {
        m_NP_plot <- m_NP_plot_f()
        
        if (is.null(m_NP_plot)) {
          return(NULL)
        }
        
        m_NP_plot_adjust <- m_NP_plot_adjust_f()
        return(m_NP_plot_adjust)
      })
  })
  
  ### output$NP_input_report ----
  output$NP_input_report <- downloadHandler(
    filename = function() {
      file_name <- paste0("my report.docx")
      return(file_name)
    },
    
    content = function(file) {
      
      params <- list(
        results = m_NP_surv_f(),
        plot = m_NP_plot_f(),
        curve_type = input$NP_input_curve_type,
        comp_type = input$NP_input_comp_type,
        p_adjust_type = input$NP_input_comp_p_adj_method
      )
      
      rmarkdown::render(input = "shiny functions/NP_report.Rmd",
                        output_file = file,
                        output_format = word_document2(reference_docx = "style_template.docx"),
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  ### output$NP_input_graph_download ----
  output$NP_input_graph_download <- downloadHandler(
    filename = function() {
      file_name <- paste0("non-parametric.", input$NP_input_graph_file_type)
      return(file_name)
    },
    
    content = function(file) {
      ggsave(filename = file, 
             plot = m_NP_plot_f(),
             device = input$NP_input_graph_file_type, 
             width = input$NP_input_graph_width, 
             height = input$NP_input_graph_height, 
             dpi = input$NP_input_graph_dpi)
    }
  )
  
  
  ## Distribution Selection ----
  ### 
  m_P_dist_select_f <- reactive({
    data <- reader()
    
    if (is.null(input$time_1) | input$time_1 == "No Variable") {
      time_1 <- NULL
    } else {
      time_1 <- data[,input$time_1]
    }
    if (is.null(input$time_2) | input$time_2 == "No Variable") {
      time_2 <- NULL
    } else {
      time_2 <- data[,input$time_2]
    }
    if (is.null(input$c_r) | input$c_r == "No Variable") {
      c_r <- NULL
    } else {
      c_r <- data[,input$c_r]
    }
    if (is.null(input$c_l) | input$c_l == "No Variable") {
      c_l <- NULL
    } else {
      c_l <- data[,input$c_l]
    }
    if (is.null(input$group) | input$group == "No Variable") {
      group <- NULL
    } else {
      group <- data[,input$group]
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable")) {
      if (is.null(time_1) | input$time_1 == "No Variable") {
        time_1 <- time_2
        time_2 <- NULL
      }
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") & (is.null(time_2) | input$time_2 == "No Variable")) {
      show_alert(
        title = "Error !!",
        text = "At least one time variable must be selected.",
        type = "error"
      )
      return(NULL)
    }
    
    if (input$interval & ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable"))) {
      show_alert(
        title = "Error!!",
        text = "Both time variable must be selecter for interval censored data.",
        type = "error"
      )
      return(NULL)
    }
    
    if ((!is.numeric(time_1) & !is.null(time_1)) | (!is.numeric(time_2) & !is.null(time_2))) {
      show_alert(
        title = "Error!!",
        text = "Time variables must be numeric.",
        type = "error"
      )
      return(NULL)
    }
    
    m_P_dist_select <- P_dist_select(time_1 = time_1, 
                                     time_2 = time_2, 
                                     c_r = c_r,
                                     c_l = c_l, 
                                     group = group, 
                                     interval = input$interval)
    return(m_P_dist_select)
  })
  
  ### input$dist_selection_report ----
  observeEvent(eventExpr = input$dist_selection_input_report, {
    m_P_dist_select <- m_P_dist_select_f()
    
    #### output$dist_selection_top3_dist ----
    output$dist_selection_top3_dist <- renderPrint({
      if (is.null(m_P_dist_select)) {
        return(data.frame())
      }
      
      for (i in 1:length(m_P_dist_select$top_three_dists)) {
        cat(names(m_P_dist_select$top_three_dists)[i], ":")
        
        print(kable(data.frame(Distribution = m_P_dist_select$top_three_dists[[i]]), format = "pipe", digits = 3))
        cat(rep("=", 30), "\n", sep = "")
      }
      
    })
    
    #### output$dist_selection_top3_dist_pars ----
    output$dist_selection_top3_dist_pars <- renderPrint({
      if (is.null(m_P_dist_select)) {
        return(data.frame())
      }
      
      for (i in 1:length(m_P_dist_select$top_three_pars)) {
        cat(names(m_P_dist_select$top_three_pars)[i], ":")
        
        for (j in 1:length(m_P_dist_select$top_three_pars[[i]])) {
          print(kable(data.frame(pars = m_P_dist_select$top_three_pars[[i]][[j]]), 
                      format = "pipe", 
                      digits = 3, 
                      col.names = names(m_P_dist_select$top_three_pars[[i]])[j]))
        }
        cat(rep("=", 30), "\n", sep = "")
      }
      
    })
    
    #### output$dist_selection_AIC ----
    output$dist_selection_AIC <- renderUI({
      if (is.null(m_P_dist_select)) {
        return(flextable(data.frame(Distrinution = dist_names) %>% htmltools_value()))
      }
      
      m_P_dist_select <- data.frame(m_P_dist_select$AICs)
      m_P_dist_select <- data.frame(Distribution = rownames(m_P_dist_select), m_P_dist_select)
      m_P_dist_select[,-1] <- round(m_P_dist_select[,-1], 3)
      m_P_dist_select
      p <- ncol(m_P_dist_select)
      m_P_dist_select_flex <- flextable(m_P_dist_select)
      
      for (i in 2:p) {
        m <- m_P_dist_select[,i]
        vol <- 1/rank(m)
        m_P_dist_select_flex <- flextable::bg(x = m_P_dist_select_flex, 
                                              j = i, 
                                              bg = spec_color(x = vol,
                                                              begin = 0.2,
                                                              end = 0.8, 
                                                              option = "B",
                                                              direction = -1))
        m_P_dist_select_flex <- flextable::fontsize(x = m_P_dist_select_flex, j = i, size = (vol*20/diff(range(vol)) - min(vol*20/diff(range(vol)))) + 10)
        m_P_dist_select_flex <- flextable::bold(x = m_P_dist_select_flex, j = i)
        m_P_dist_select_flex <- flextable::color(x = m_P_dist_select_flex, j = i, color = "white")
      }
      
      m_P_dist_select_flex <- set_caption(x = m_P_dist_select_flex, caption = "AIC")
      
      m_P_dist_select_flex %>%
        htmltools_value()
    })
    
    #### output$dist_selection_LL ----
    output$dist_selection_LL <- renderUI({
      if (is.null(m_P_dist_select)) {
        return(flextable(data.frame(Distrinution = dist_names) %>% htmltools_value()))
      }
      
      m_P_dist_select <- data.frame(m_P_dist_select$LLs)
      m_P_dist_select <- data.frame(Distribution = rownames(m_P_dist_select), m_P_dist_select)
      m_P_dist_select[,-1] <- round(m_P_dist_select[,-1], 3)
      m_P_dist_select
      p <- ncol(m_P_dist_select)
      m_P_dist_select_flex <- flextable(m_P_dist_select)
      
      for (i in 2:p) {
        m <- m_P_dist_select[,i]
        vol <- rank(m)^3
        m_P_dist_select_flex <- flextable::bg(x = m_P_dist_select_flex, 
                                              j = i, 
                                              bg = spec_color(x = vol,
                                                              begin = 0.2,
                                                              end = 0.8, 
                                                              option = "B",
                                                              direction = -1))
        m_P_dist_select_flex <- flextable::fontsize(x = m_P_dist_select_flex, j = i, 
                                                    size = (vol*20/diff(range(vol)) - min(vol*20/diff(range(vol)))) + 10)
        m_P_dist_select_flex <- flextable::bold(x = m_P_dist_select_flex, j = i)
        m_P_dist_select_flex <- flextable::color(x = m_P_dist_select_flex, j = i, color = "white")
      }
      
      m_P_dist_select_flex <- set_caption(x = m_P_dist_select_flex, caption = "Log-likelihood")
      
      m_P_dist_select_flex %>%
        htmltools_value()
    })
    
  })
  
  ### Data Summary ----
  observe({
    output$datasummary_output_data <- renderDataTable({
      data <- reader()
      data <- data[, colnames(data) %in% c(input$time_1,
                                           input$time_2,
                                           input$group,
                                           input$c_r,
                                           input$c_l), drop = FALSE]
      return(data)
    })
  })
  
  ### m_datasummary_timeplot_f ----
  m_datasummary_timeplot_f <- reactive({
    
    data <- reader()
    
    if (is.null(input$time_1) | input$time_1 == "No Variable") {
      time_1 <- NULL
    } else {
      time_1 <- data[,input$time_1]
    }
    if (is.null(input$time_2) | input$time_2 == "No Variable") {
      time_2 <- NULL
    } else {
      time_2 <- data[,input$time_2]
    }
    if (is.null(input$c_r) | input$c_r == "No Variable") {
      c_r <- NULL
    } else {
      c_r <- data[,input$c_r]
    }
    if (is.null(input$c_l) | input$c_l == "No Variable") {
      c_l <- NULL
    } else {
      c_l <- data[,input$c_l]
    }
    if (is.null(input$group) | input$group == "No Variable") {
      group <- NULL
    } else {
      group <- data[,input$group]
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable")) {
      if (is.null(time_1) | input$time_1 == "No Variable") {
        time_1 <- time_2
        time_2 <- NULL
      }
    }
    
    if ((is.null(time_1) | input$time_1 == "No Variable") & (is.null(time_2) | input$time_2 == "No Variable")) {
      show_alert(
        title = "Error!!",
        text = "At least one time variable must be selected.",
        type = "error"
      )
      return(NULL)
    }
    
    if (input$interval & ((is.null(time_1) | input$time_1 == "No Variable") | (is.null(time_2) | input$time_2 == "No Variable"))) {
      show_alert(
        title = "Error!!",
        text = "Both time variable must be selecter for interval censored data.",
        type = "error"
      )
      return(NULL)
    }
    
    if ((!is.numeric(time_1) & !is.null(time_1)) | (!is.numeric(time_2) & !is.null(time_2))) {
      show_alert(
        title = "Error!!",
        text = "Time variables must be numeric.",
        type = "error"
      )
      return(NULL)
    }
    
    timeplot(time_1 = time_1, 
             time_2 = time_2, 
             c_r = c_r, 
             c_l = c_l, 
             group = group)
    
  })
  
  ### input$datasummary_input_report ----
  observeEvent(eventExpr = input$datasummary_input_report, {
    m_datasummary_timeplot <- m_datasummary_timeplot_f()
    
    #### output$datasummary_output_plot ----
    output$datasummary_output_plot <- renderPlot({
      m_datasummary_timeplot
    })
  })
  
  ### output$datasummary_input_graph_download ----
  output$datasummary_input_graph_download <- downloadHandler(
    filename = function() {
      file_name <- paste0("timeline.", input$datasummary_input_graph_file_type)
      return(file_name)
    },
    
    content = function(file) {
      ggsave(filename = file, 
             plot = m_datasummary_timeplot_f(),
             device = input$datasummary_input_graph_file_type, 
             width = input$datasummary_input_graph_width, 
             height = input$datasummary_input_graph_height, 
             dpi = input$datasummary_input_graph_dpi)
    }
  )
}

# START ----
shinyApp(ui = ui, server = server)

