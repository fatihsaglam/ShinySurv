NP_input_curve_type <- selectInput(inputId = "NP_input_curve_type", label = "Curve Type", choices = curve_type_names, selected = "Survival")
NP_input_conf_interval <- checkboxInput(inputId = "NP_input_conf_interval",
                                       label = "Confidence Interval",
                                       value = FALSE, 
                                       width = "100%")
NP_input_conf_method <- selectInput(inputId = "NP_input_conf_method", label = "CI method", choices = conf_methods_names, selected = "Rothman")
NP_input_conf_level <- selectInput(inputId = "NP_input_conf_level", label = "Confidence Level", choices = c("0.90", "0.95", "0.99"), selected = "0.95")
NP_input_line_color <- selectInput(inputId = "NP_input_line_color", label = "Line color", choices = color_names, selected = "d3")
NP_input_line_size <- sliderInput(inputId = "NP_input_line_size", label = "Line size", min = 0, max = 3, value = 0.5, step = 0.01)
NP_input_fill_color <- selectInput(inputId = "NP_input_fill_color", label = "Fill color", choices = color_names, selected = "d3")
NP_input_transparency <- sliderInput(inputId = "NP_input_transparency", label = "Transparency", min = 0, max = 1, value = 0.5, step = 0.01)
NP_input_theme_type <- selectInput(inputId = "NP_input_theme_type", label = "Theme", choices = theme_names, selected = "bw")
NP_input_font_name <- selectInput(inputId = "NP_input_font_name", label = "Font", choices = font_names, selected = "Arial")
NP_input_legend_title <- textInput(inputId = "NP_input_legend_title", label = "Legend title", value = "Groups")
NP_input_legend_position <- selectInput(inputId = "NP_input_legend_position", label = "Legend position", choices = legend_positions, selected = "right")
NP_input_axis_x_title <- textInput(inputId = "NP_input_axis_x_title", label = "Axis X", value = "t")
NP_input_axis_x_title_face <- selectInput(inputId = "NP_input_axis_x_title_face", label = "Face", choices = face_types, selected = "plain")
NP_input_axis_x_title_size <- numericInput(inputId = "NP_input_axis_x_title_size", label = "Size", value = 16, min = 0, max = 30, step = 1)
NP_input_axis_x_title_hjust <- radioButtons(inputId = "NP_input_axis_x_title_hjust", label = "Adjust", selected = 0.5, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1))
NP_input_axis_y_title <- textInput(inputId = "NP_input_axis_y_title", label = "Axis Y", value = "")
NP_input_axis_y_title_face <- selectInput(inputId = "NP_input_axis_y_title_face", label = "Face", choices = face_types, selected = "plain")
NP_input_axis_y_title_size <- numericInput(inputId = "NP_input_axis_y_title_size", label = "Size", value = 16, min = 0, max = 30, step = 1)
NP_input_axis_y_title_hjust <- radioButtons(inputId = "NP_input_axis_y_title_hjust", label = "Adjust", selected = 0.5, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1))
NP_input_title <- textInput(inputId = "NP_input_title", label = "Title", value = NULL)
NP_input_title_face <- selectInput(inputId = "NP_input_title_face", label = "Face", choices = face_types, selected = "plain")
NP_input_title_size <- numericInput(inputId = "NP_input_title_size", label = "Size", value = 20, min = 0, max = 30, step = 1)
NP_input_title_hjust <- radioButtons(inputId = "NP_input_title_hjust", label = "Adjust", selected = 0.5, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1), inline = TRUE)
NP_input_subtitle <- textInput(inputId = "NP_input_subtitle", label = "Subtitle", value = NULL)
NP_input_subtitle_face <- selectInput(inputId = "NP_input_subtitle_face", label = "Face", choices = face_types, selected = "plain")
NP_input_subtitle_size <- numericInput(inputId = "NP_input_subtitle_size", label = "Size", value = 10, min = 0, max = 30, step = 1)
NP_input_subtitle_hjust <- radioButtons(inputId = "NP_input_subtitle_hjust", label = "Adjust", selected = 0, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1), inline = TRUE)
NP_input_caption <- textInput(inputId = "NP_input_caption", label = "Caption", value = NULL)
NP_input_caption_face <- selectInput(inputId = "NP_input_caption_face", label = "Face", choices = face_types, selected = "plain")
NP_input_caption_size <- numericInput(inputId = "NP_input_caption_size", label = "Size", value = 10, min = 0, max = 30, step = 1)
NP_input_caption_hjust <- radioButtons(inputId = "NP_input_caption_hjust", label = "Adjust", selected = 1, choiceNames = c("Left", "Center", "Right"), choiceValues = c(0, 0.5, 1), inline = TRUE)
NP_input_generate <- actionButton(inputId = "NP_input_generate", label = "Generate", style = "border: 1px solid black")
NP_input_adjust <- actionButton(inputId = "NP_input_adjust", label = "Apply", style = "border: 1px solid black")
NP_input_report <- tags$a(class = "btn btn-default shiny-download-link",
                         id = "NP_input_report",
                         download = NA,
                         tags$button(class = "btn btn-default action-button",
                           tags$span(h4(ph("notebook"), "Generate Report", 
                                        style = "margin-top: 0px; padding: 3px 3px 5px 5px; color: black")),
                           style = "margin-bottom: 0px; border-radius: 8px; padding-bottom: 0px; background: white"),
                         style = "padding: 0px; border-radius: 8px; border: 1px solid black; background: white;")
NP_tab_box_upper_style <- "border: 0px solid black; background: white; border-radius: 12px 12px 12px 12px; margin: 0px"
NP_tab_box_lower_style <- "border-top: 1px solid black; background: white; border-radius: 0px 0px 12px 12px"
NP_tab_tabpanel_style <- "margin: 0px; padding: 0px; font-size: 8pt"


NP_input_graph_width <- numericInput(inputId = "NP_input_graph_width", label = "Width (px)", value = 8, min = 0, max = Inf, step = 1)
NP_input_graph_height <- numericInput(inputId = "NP_input_graph_height", label = "height (px)", value = 5, min = 0, max = Inf, step = 1)
NP_input_graph_dpi <- numericInput(inputId = "NP_input_graph_dpi", label = "DPI", value = 300, min = 0, max = Inf, step = 1)
NP_input_graph_file_type <- selectInput(inputId = "NP_input_graph_file_type", label = "File type", choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"), selected = "png")

NP_input_graph_download <- downloadButton(outputId = "NP_input_graph_download", label = "Download", icon = icon("download"), style = "border: 1px solid black", tooltip = "Download")

NP_input_comp_conf_level <- selectInput(inputId = "NP_input_conf_level", label = "Confidence Level", choices = c("0.90", "0.95", "0.99"), selected = "0.95")

NP_input_comp_type <- selectInput(inputId = "NP_input_comp_type", label = "Comparison Method", choices = c("logrank", "wilcoxon"), selected = "logrank")
NP_input_comp_p_adj_method <- selectInput(inputId = "NP_input_comp_p_adj_method", label = "Adjustment method", choices = p.adjust.methods, selected = "bonferroni")
