datasummary_tab <- tabPanel(title = h1("Data Summary", style = "margin: 0px; padding: 0px"),
                            fluidRow(
                              column(6,
                                     box(
                                       width = 12, 
                                       title = tags$table(style = "border-collapse: collapse; width = '100%';",
                                                          tags$body(
                                                            tags$tr(
                                                              tags$td(style = "width: 50%; height: 10px",
                                                                      tags$span(style = "margin: 0px; padding: 0px", 
                                                                                h2(tags$image(height = "48px", src = "https://img.icons8.com/dusk/64/000000/data-sheet.png"), 
                                                                                   style = "margin: 0px; padding: 0px", "Dataset")))
                                                            )
                                                          )), 
                                       status = "primary", 
                                       solidHeader = TRUE, 
                                       background = "gray", 
                                       column(12,
                                              style = datasummary_tab_box_upper_style, 
                                              dataTableOutput(outputId = "datasummary_output_data")
                                       )
                                     )
                              ),
                              column(6,
                                     box(width = 12, 
                                         title = tags$table(style = "border-collapse: collapse; width = '100%';",
                                                            tags$body(
                                                              tags$tr(
                                                                tags$td(style = "width: 50%; height: 10px",
                                                                        tags$span(style = "margin: 0px; padding: 0px", 
                                                                                  h2(tags$image(height = "48px", 
                                                                                                src = "https://img.icons8.com/dusk/64/000000/timeline.png"), 
                                                                                     style = "margin: 0px; padding: 0px", "Timeline"))),
                                                                tags$td(style = "width: 5%; text-align: right; height: 10px", 
                                                                        datasummary_input_report
                                                                )
                                                              )
                                                            )), 
                                         status = "primary", 
                                         solidHeader = TRUE, 
                                         background = "gray", 
                                         column(width = 12,
                                                style = datasummary_tab_box_upper_style, 
                                                tags$div(
                                                  tags$span(class = "pull-left",
                                                            h3(tags$img(height = "32px", src = "https://img.icons8.com/external-wanicon-lineal-color-wanicon/64/000000/external-graph-virus-transmission-wanicon-lineal-color-wanicon.png"), 
                                                               style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", strong("Graph"))),
                                                  tags$span(class = "pull-right", 
                                                            p(style = "padding-right : 40px; padding-top: 0px",
                                                              dropMenu(tag = actionButton(inputId = "datasummary_input_graph_button_tah", icon = icon("download"), label = ""),
                                                                       placement = "left",
                                                                       tagList(datasummary_input_graph_width,
                                                                               datasummary_input_graph_height,
                                                                               datasummary_input_graph_dpi,
                                                                               datasummary_input_graph_file_type,
                                                                               datasummary_input_graph_download)
                                                              )))
                                                ),
                                                column(width = 12,
                                                       style = datasummary_tab_box_lower_style,
                                                       plotOutput(outputId = "datasummary_output_plot", height = "500px")
                                                )
                                         )
                                     )
                              )
                            )
)
