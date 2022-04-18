P_tab <- tabPanel(title = h1("Parametric", style = "margin: 0px; padding: 0px"),
                  fluidRow(
                    column(width = 6,
                           box(width = 12, 
                               title = tags$span(h2(tags$img(height = "48px", src = "https://img.icons8.com/dusk/64/000000/settings.png"),   
                                                    style = "margin: 0px; padding: 0px", "Settings")),
                               status = "primary", 
                               solidHeader = TRUE,
                               background = "gray", 
                               
                               column(width = 12,
                                      style = P_tab_box_upper_style, 
                                      tags$div(
                                        tags$span(class = "pull-left",
                                                  h3(tags$img(height = "32px", src = "https://img.icons8.com/external-icongeek26-flat-icongeek26/64/000000/external-graph-data-analytics-icongeek26-flat-icongeek26.png"), 
                                                     style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", 
                                                     strong("Curve Settings"))),
                                        tags$span(class = "pull-right", 
                                                  p(style = "padding-right : 40px; padding-top: 7px", 
                                                    P_input_generate))
                                      ),
                                      
                                      column(width = 12,
                                             style = P_tab_box_lower_style,
                                             column(6,
                                                    column(12,
                                                           hr()),
                                                    
                                                    P_input_distribution,
                                                    P_input_curve_type
                                             ),
                                             column(6,
                                                    P_input_conf_interval,
                                                    P_input_conf_level,
                                                    P_input_n_boot,
                                                    P_input_bootstrap_type
                                             )
                                      ),
                               ),
                               
                               
                               column(12,
                                      hr()),
                               
                               column(width = 12,
                                      style = P_tab_box_upper_style, 
                                      tags$div(
                                        tags$span(class = "pull-left",
                                                  h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/paint-palette.png"), 
                                                     style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", 
                                                     strong("Graph Settings"))),
                                        tags$span(class = "pull-right", 
                                                  p(style = "padding-right : 40px; padding-top: 7px", 
                                                    P_input_adjust))
                                      ),
                                      
                                      column(width = 12,
                                             style = P_tab_box_lower_style,
                                             column(width = 12,
                                                    br()),
                                             tabsetPanel( 
                                               tabPanel(title = p("Curve", style = P_tab_tabpanel_style), 
                                                        P_input_line_color,
                                                        P_input_line_size,
                                                        P_input_fill_color,
                                                        P_input_transparency
                                               ),
                                               tabPanel(title = p("Theme-Legend", style = P_tab_tabpanel_style), 
                                                        P_input_theme_type,
                                                        P_input_font_name,
                                                        P_input_legend_title,
                                                        P_input_legend_position
                                               ),
                                               tabPanel(title = p("Axis", style = P_tab_tabpanel_style), 
                                                        column(6,
                                                               P_input_axis_x_title, 
                                                               P_input_axis_x_title_face,
                                                               P_input_axis_x_title_size,
                                                               P_input_axis_x_title_hjust),
                                                        column(6,
                                                               P_input_axis_y_title,
                                                               P_input_axis_y_title_face,
                                                               P_input_axis_y_title_size,
                                                               P_input_axis_y_title_hjust)
                                               ),
                                               tabPanel(title = p("Title", style = P_tab_tabpanel_style), 
                                                        P_input_title, 
                                                        P_input_title_face, 
                                                        P_input_title_size, 
                                                        P_input_title_hjust
                                               ),
                                               tabPanel(title = p("Subtitle", style = P_tab_tabpanel_style), 
                                                        P_input_subtitle,
                                                        P_input_subtitle_face,
                                                        P_input_subtitle_size,
                                                        P_input_subtitle_hjust
                                               ),
                                               tabPanel(title = p("Caption", style = P_tab_tabpanel_style), 
                                                        P_input_caption,
                                                        P_input_caption_face,
                                                        P_input_caption_size,
                                                        P_input_caption_hjust)
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
                                                              P_input_report)
                                                    )
                                                  )), 
                               status = "primary", 
                               solidHeader = TRUE, 
                               background = "gray", 
                               column(width = 12,
                                      style = P_tab_box_upper_style, 
                                      tags$div(
                                        tags$span(class = "pull-left",
                                                  h3(tags$img(height = "32px", src = "https://img.icons8.com/external-wanicon-lineal-color-wanicon/64/000000/external-graph-virus-transmission-wanicon-lineal-color-wanicon.png"), 
                                                     style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Graph"))),
                                        tags$span(class = "pull-right", 
                                                  p(style = "padding-right : 40px; padding-top: 0px",
                                                    dropMenu(tag = actionButton(inputId = "P_input_graph_button_tag", icon = icon("download"), label = ""),
                                                             placement = "left",
                                                             tagList(P_input_graph_width,
                                                                     P_input_graph_height,
                                                                     P_input_graph_dpi,
                                                                     P_input_graph_file_type,
                                                                     P_input_graph_download)
                                                    )))
                                      ),
                                      column(width = 12,
                                             style = P_tab_box_lower_style,
                                             plotOutput(outputId = "P_output_plot")
                                      )
                               ),
                               
                               
                               column(width = 12,
                                      hr()),
                               
                               column(width = 12,
                                      style = P_tab_box_upper_style, 
                                      tags$div(
                                        tags$span(class = "pull-left",
                                                  h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/goodnotes.png"), 
                                                     style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Means and Medians"))),
                                      ),
                                      
                                      column(width = 12,
                                             style = P_tab_box_lower_style,
                                             column(12,
                                                    br()),
                                             verbatimTextOutput(outputId = "P_output_means_medians", placeholder = TRUE)
                                      )
                               ),
                               
                               
                               column(width = 12,
                                      hr()),
                               
                               column(width = 12,
                                      style = P_tab_box_upper_style, 
                                      tags$div(
                                        tags$span(class = "pull-left",
                                                  h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/goodnotes.png"), 
                                                     style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Parameters"))),
                                      ),
                                      column(width = 12,
                                             style = P_tab_box_lower_style,
                                             column(12,
                                                    br()),
                                             verbatimTextOutput(outputId = "P_output_parameters", placeholder = TRUE)
                                      )
                               ),
                               
                               column(width = 12,
                                      hr()),
                               
                               column(width = 12,
                                      style = P_tab_box_upper_style, 
                                      tags$div(
                                        tags$span(class = "pull-left",
                                                  h3(tags$img(height = "32px", src = "https://img.icons8.com/dusk/64/000000/goodnotes.png"), 
                                                     style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Life Tables"))),
                                      ),
                                      
                                      column(width = 12,
                                             style = P_tab_box_lower_style,
                                             column(12,
                                                    br()),
                                             verbatimTextOutput(outputId = "P_output_life_tables", placeholder = TRUE)
                                      )
                               )
                               
                           )
                    )
                  )
)
