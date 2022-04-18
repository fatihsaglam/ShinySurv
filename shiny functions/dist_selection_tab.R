dist_selection_tab <- tabPanel(title = h1("Distribution Selection", style = "margin: 0px; padding: 0px"),
                               fluidRow(
                                 column(12,
                                        box(width = 12, 
                                            title = tags$table(style = "border-collapse: collapse; width = '100%';",
                                                               tags$body(
                                                                 tags$tr(
                                                                   tags$td(style = "width: 50%; height: 10px",
                                                                           tags$span(style = "margin: 0px; padding: 0px", 
                                                                                     h2(tags$image(height = "48px", src = "https://img.icons8.com/external-flaticons-lineal-color-flat-icons/64/000000/external-result-marketing-agency-flaticons-lineal-color-flat-icons-2.png"), 
                                                                                        style = "margin: 0px; padding: 0px", "Results"))),
                                                                   tags$td(style = "width: 5%; text-align: right; height: 10px", 
                                                                           dist_selection_input_report)
                                                                 )
                                                               )), 
                                            status = "primary", 
                                            solidHeader = TRUE,
                                            background = "gray", 
                                            column(width = 12,
                                                   style = dist_selection_tab_box_upper_style, 
                                                   h3(tags$img(height = "32px", src = "https://img.icons8.com/external-becris-lineal-color-becris/64/000000/external-probability-data-science-becris-lineal-color-becris.png"),
                                                      style = "padding-left: 40px; margin-top: 3px; padding-top: 5px;", 
                                                      strong("Distribution results")),
                                                   
                                                   column(width = 6,
                                                          style = dist_selection_tab_box_lower_style,
                                                          column(12,
                                                                 br()),
                                                          column(6,
                                                                 verbatimTextOutput(outputId = "dist_selection_top3_dist", placeholder = TRUE)
                                                          ),
                                                          column(6,
                                                                 verbatimTextOutput(outputId = "dist_selection_top3_dist_pars", placeholder = TRUE)
                                                          )
                                                   ),
                                                   column(width = 6,
                                                          style = dist_selection_tab_box_lower_style,
                                                          uiOutput(outputId = "dist_selection_AIC"),
                                                          column(12,
                                                                 hr()),
                                                          uiOutput(outputId = "dist_selection_LL")
                                                   )
                                            )
                                        )
                                 )
                               ))
