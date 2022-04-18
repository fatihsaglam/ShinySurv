datasummary_tab_box_upper_style <- 'border: 0px solid black; background: white; border-radius: 12px 12px 12px 12px; margin: 0px'
datasummary_tab_box_lower_style <- "border-top: 1px solid black; background: white; border-radius: 0px 0px 12px 12px;"

datasummary_input_report <- tags$button(class = "btn btn-default action-button", type = "button", id = "datasummary_input_report",
                                           tags$span(h4(ph("notebook"), "Generate", 
                                                        style = "margin-top: 0px; padding: 3px 3px 5px 5px; color: black")),
                                           style = "margin-bottom: 0px; border-radius: 8px; padding-bottom: 0px; background: white")

datasummary_input_graph_width <- numericInput(inputId = "datasummary_input_graph_width", label = "Width (px)", value = 8, min = 0, max = Inf, step = 1)
datasummary_input_graph_height <- numericInput(inputId = "datasummary_input_graph_height", label = "height (px)", value = 5, min = 0, max = Inf, step = 1)
datasummary_input_graph_dpi <- numericInput(inputId = "datasummary_input_graph_dpi", label = "DPI", value = 300, min = 0, max = Inf, step = 1)
datasummary_input_graph_file_type <- selectInput(inputId = "datasummary_input_graph_file_type", label = "File type", choices = c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg"), selected = "png")

datasummary_input_graph_download <- downloadButton(outputId = "datasummary_input_graph_download", label = "Download", icon = icon("download"), style = "border: 1px solid black", tooltip = "Download")
