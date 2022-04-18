P_input_distribution <- selectInput(inputId = "P_input_distribution", label = "Distribution", choices = dist_names, selected = "Weibull")
P_input_curve_type <- selectInput(inputId = "P_input_curve_type", label = "Curve Type", choices = curve_type_names, selected = "Survival")
P_input_conf_interval <- checkboxInput(inputId = "P_input_conf_interval",
                                        label = "Confidence Interval",
                                        value = FALSE, 
                                        width = "100%")
P_input_conf_level <- selectInput(inputId = "P_input_conf_level", label = "Confidence Level", choices = c("0.90", "0.95", "0.99"), selected = "0.95")
P_input_n_boot <- numericInput(inputId = "P_input_n_boot", label = "Number of bootstrap repetation", value = 500, min = 3, max = Inf)
P_input_bootstrap_type <- selectInput(inputId = "P_input_bootstrap_type", label = "bootstrap type", choices = c("Jackknife", "Non-parametric bootstrap"), selected = "Non-parametric bootstrap")
P_input_line_color <- selectInput(inputId = "P_input_line_color", label = "Line color", choices = color_names, selected = "d3")
P_input_line_size <- sliderInput(inputId = "P_input_line_size", label = "Line size", min = 0, max = 3, value = 0.5, step = 0.01)
P_input_fill_color <- selectInput(inputId = "P_input_fill_color", label = "Fill color", choices = color_names, selected = "d3")
P_input_transparency <- sliderInput(inputId = "P_input_transparency", label = "Transparency", min = 0, max = 1, value = 0.5, step = 0.01)
P_input_theme_type <- selectInput(inputId = "P_input_theme_type", label = "Theme", choices = theme_names, selected = "bw")
P_input_font_name <- selectInput(inputId = "P_input_font_name", label = "Font", choices = font_names, selected = "Arial")
P_input_legend_title <- textInput(inputId = "P_input_legend_title", label = "Legend title", value = "Groups")
P_input_legend_position <- selectInput(inputId = "P_input_legend_position", label = "Legend position", choices = legend_positions, selected = "right")
P_input_axis_x_title <- textInput(inputId = "P_input_axis_x_title", label = "Axis X", value = "t")
P_input_axis_x_title_face <- selectInput(inputId = "P_input_axis_x_title_face", label = "Face", choices = face_types, selected = "plain")
P_input_axis_x_title_size <- numericInput(inputId = "P_input_axis_x_title_size", label = "Size", value = 16, min = 0, max = 30, step = 1)
P_input_axis_x_title_hjust <- radioButtons(inputId = "P_input_axis_x_title_hjust", label = "Adjust", selected = 0.5, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1))
P_input_axis_y_title <- textInput(inputId = "P_input_axis_y_title", label = "Axis Y", value = "")
P_input_axis_y_title_face <- selectInput(inputId = "P_input_axis_y_title_face", label = "Face", choices = face_types, selected = "plain")
P_input_axis_y_title_size <- numericInput(inputId = "P_input_axis_y_title_size", label = "Size", value = 16, min = 0, max = 30, step = 1)
P_input_axis_y_title_hjust <- radioButtons(inputId = "P_input_axis_y_title_hjust", label = "Adjust", selected = 0.5, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1))
P_input_title <- textInput(inputId = "P_input_title", label = "Title", value = NULL)
P_input_title_face <- selectInput(inputId = "P_input_title_face", label = "Face", choices = face_types, selected = "plain")
P_input_title_size <- numericInput(inputId = "P_input_title_size", label = "Size", value = 20, min = 0, max = 30, step = 1)
P_input_title_hjust <- radioButtons(inputId = "P_input_title_hjust", label = "Adjust", selected = 0.5, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1), inline = TRUE)
P_input_subtitle <- textInput(inputId = "P_input_subtitle", label = "Subtitle", value = NULL)
P_input_subtitle_face <- selectInput(inputId = "P_input_subtitle_face", label = "Face", choices = face_types, selected = "plain")
P_input_subtitle_size <- numericInput(inputId = "P_input_subtitle_size", label = "Size", value = 10, min = 0, max = 30, step = 1)
P_input_subtitle_hjust <- radioButtons(inputId = "P_input_subtitle_hjust", label = "Adjust", selected = 0, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1), inline = TRUE)
P_input_caption <- textInput(inputId = "P_input_caption", label = "Caption", value = NULL)
P_input_caption_face <- selectInput(inputId = "P_input_caption_face", label = "Face", choices = face_types, selected = "plain")
P_input_caption_size <- numericInput(inputId = "P_input_caption_size", label = "Size", value = 10, min = 0, max = 30, step = 1)
P_input_caption_hjust <- radioButtons(inputId = "P_input_caption_hjust", label = "Adjust", selected = 1, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1), inline = TRUE)
P_input_generate <- actionButton(inputId = "P_input_generate", label = "Generate", style = "border: 1px solid black")
P_input_adjust <- actionButton(inputId = "P_input_adjust", label = "Apply", style = "border: 1px solid black")
P_input_report <- tags$a(class = "btn btn-default shiny-download-link ",
                         id = "P_input_report",
                         download = NA,
                         tags$button(class = "btn btn-default action-button",
                           tags$span(h4(ph("notebook"), "Generate Report", 
                                        style = "margin-top: 0px; padding: 3px 3px 5px 5px; color: black")),
                                     style = "margin-bottom: 0px; border-radius: 8px; padding-bottom: 0px; background: white"),
                         style = "padding: 0px; border-radius: 8px; border: 1px solid black; background: white;")

P_tab_box_upper_style <- 'border: 0px solid black; background: white; border-radius: 12px 12px 12px 12px; margin: 0px'
P_tab_box_lower_style <- "border-top: 1px solid black; background: white; border-radius: 0px 0px 12px 12px;"
P_tab_tabpanel_style <- "margin: 0px; padding: 0px; font-size: 8pt"

P_input_graph_width <- numericInput(inputId = "P_input_graph_width", label = "Width (px)", value = 8, min = 0, max = Inf, step = 1)
P_input_graph_height <- numericInput(inputId = "P_input_graph_height", label = "height (px)", value = 5, min = 0, max = Inf, step = 1)
P_input_graph_dpi <- numericInput(inputId = "P_input_graph_dpi", label = "DPI", value = 300, min = 0, max = Inf, step = 1)
P_input_graph_file_type <- selectInput(inputId = "P_input_graph_file_type", label = "File type", choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"), selected = "png")

P_input_graph_download <- downloadButton(outputId = "P_input_graph_download", label = "Download", icon = icon("download"), style = "border: 1px solid black", tooltip = "Download")
