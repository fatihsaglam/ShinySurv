NP_tab <- tabPanel(title = h1("Non-Parametric", style = "margin: 0px; padding: 0px"),
                   fluidRow(
                     column(width = 6,
                            box(width = 12, 
                                title = tags$span(h2(tags$img(height = "48px", src = "https://img.icons8.com/dusk/64/000000/settings.png"),  
                                                     style = "margin: 0px; padding: 0px", "Settings")),
                                status = "primary", 
                                solidHeader = TRUE,
                                background = "gray", 
                                
                                column(width = 12,
                                       style = NP_tab_box_upper_style, 
                                       tags$div(
                                         tags$span(class = "pull-left",
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/external-icongeek26-flat-icongeek26/64/000000/external-graph-data-analytics-icongeek26-flat-icongeek26.png"), 
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", 
                                                      strong("Curve Settings"))),
                                         tags$span(class = "pull-right", 
                                                   p(style = "padding-right : 40px; padding-top: 7px", 
                                                     NP_input_generate))
                                       ),
                                       
                                       column(width = 12,
                                              style = NP_tab_box_lower_style,
                                              column(6,
                                                     NP_input_curve_type
                                              ),
                                              column(6,
                                                     NP_input_conf_interval,
                                                     NP_input_conf_level,
                                                     NP_input_conf_method
                                              ),
                                              column(12,
                                                     NP_input_comp_type,
                                                     NP_input_comp_p_adj_method)
                                       )
                                ),
                                
                                
                                column(12,
                                       hr()),
                                
                                column(width = 12,
                                       style = NP_tab_box_upper_style, 
                                       tags$div(
                                         tags$span(class = "pull-left",
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/paint-palette.png"),
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Graph Settings"))),
                                         tags$span(class = "pull-right", p(style = "padding-right : 40px; padding-top: 7px", NP_input_adjust))
                                       ),
                                       
                                       
                                       column(width = 12,
                                              style = NP_tab_box_lower_style,
                                              column(width = 12,
                                                     br()),
                                              tabsetPanel( 
                                                tabPanel(title = p("Curve", style = NP_tab_tabpanel_style), 
                                                         NP_input_line_color,
                                                         NP_input_line_size,
                                                         NP_input_fill_color,
                                                         NP_input_transparency
                                                ),
                                                tabPanel(title = p("Theme-Legend", style = NP_tab_tabpanel_style), 
                                                         NP_input_theme_type,
                                                         NP_input_font_name,
                                                         NP_input_legend_title,
                                                         NP_input_legend_position
                                                ),
                                                tabPanel(title = p("Axis", style = NP_tab_tabpanel_style), 
                                                         column(6,
                                                                NP_input_axis_x_title, 
                                                                NP_input_axis_x_title_face,
                                                                NP_input_axis_x_title_size,
                                                                NP_input_axis_x_title_hjust),
                                                         column(6,
                                                                NP_input_axis_y_title,
                                                                NP_input_axis_y_title_face,
                                                                NP_input_axis_y_title_size,
                                                                NP_input_axis_y_title_hjust)
                                                ),
                                                tabPanel(title = p("Title", style = NP_tab_tabpanel_style), 
                                                         NP_input_title, 
                                                         NP_input_title_face, 
                                                         NP_input_title_size, 
                                                         NP_input_title_hjust
                                                ),
                                                tabPanel(title = p("Subtitle", style = NP_tab_tabpanel_style), 
                                                         NP_input_subtitle,
                                                         NP_input_subtitle_face,
                                                         NP_input_subtitle_size,
                                                         NP_input_subtitle_hjust
                                                ),
                                                tabPanel(title = p("Caption", style = NP_tab_tabpanel_style), 
                                                         NP_input_caption,
                                                         NP_input_caption_face,
                                                         NP_input_caption_size,
                                                         NP_input_caption_hjust)
                                              )
                                       )
                                )
                            )
                     ),
                     column(width = 6,
                            box(width = 12, 
                                title = tags$table(style = "border-collapse: collapse; width = '100%';",
                                                   tags$body(
                                                     tags$tr(
                                                       tags$td(style = "width: 50%; height: 10px",
                                                               tags$span(style = "margin: 0px; padding: 0px", 
                                                                         h2(tags$image(height = "48px", src = "https://img.icons8.com/external-flaticons-lineal-color-flat-icons/64/000000/external-result-marketing-agency-flaticons-lineal-color-flat-icons-2.png"), 
                                                                            style = "margin: 0px; padding: 0px", "Results"))),
                                                       tags$td(style = "width: 20%; text-align: right; height: 10px", 
                                                               NP_input_report)
                                                     )
                                                   )), 
                                status = "primary", 
                                solidHeader = TRUE, 
                                background = "gray", 
                                column(width = 12,
                                       style = NP_tab_box_upper_style, 
                                       tags$div(
                                         tags$span(class = "pull-left",
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/external-wanicon-lineal-color-wanicon/64/000000/external-graph-virus-transmission-wanicon-lineal-color-wanicon.png"), 
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Graph"))),
                                         tags$span(class = "pull-right", 
                                                   p(style = "padding-right : 40px; padding-top: 0px",
                                                     dropMenu(tag = actionButton(inputId = "NP_input_graph_button_tag", icon = icon("download"), label = ""),
                                                              placement = "left",
                                                              tagList(NP_input_graph_width,
                                                                      NP_input_graph_height,
                                                                      NP_input_graph_dpi,
                                                                      NP_input_graph_file_type,
                                                                      NP_input_graph_download)
                                                     )))
                                       ),
                                       
                                       column(width = 12,
                                              style = NP_tab_box_lower_style,
                                              plotOutput(outputId = "NP_output_plot")
                                       )
                                ),
                                
                                
                                column(width = 12,
                                       hr()),
                                
                                column(width = 12,
                                       style = NP_tab_box_upper_style, 
                                       tags$div(
                                         tags$span(class = "pull-left",
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/goodnotes.png"), 
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Means and Medians"))),
                                       ),
                                       
                                       column(width = 12,
                                              style = NP_tab_box_lower_style,
                                              column(12,
                                                     br()),
                                              verbatimTextOutput(outputId = "NP_output_means_medians", placeholder = TRUE)
                                       )
                                ),
                                
                                
                                column(width = 12,
                                       hr()),
                                
                                column(width = 12,
                                       style = NP_tab_box_upper_style, 
                                       tags$div(
                                         tags$span(class = "pull-left",
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/goodnotes.png"), 
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Comparison Tests"))),
                                       ),
                                       
                                       column(width = 12,
                                              style = NP_tab_box_lower_style,
                                              column(12,
                                                     br()),
                                              verbatimTextOutput(outputId = "NP_output_comparison_tests", placeholder = TRUE)
                                       )
                                ),
                                
                                
                                column(width = 12,
                                       hr()),
                                
                                column(width = 12,
                                       style = NP_tab_box_upper_style, 
                                       tags$div(
                                         tags$span(class = "pull-left",
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/goodnotes.png"), 
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Life Tables"))),
                                       ),
                                       
                                       column(width = 12,
                                              style = NP_tab_box_lower_style,
                                              column(12,
                                                     br()),
                                              verbatimTextOutput(outputId = "NP_output_life_tables", placeholder = TRUE)
                                       )
                                )
                                
                            )
                     )
                   )
)
