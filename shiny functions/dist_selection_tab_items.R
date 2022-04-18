dist_selection_tab_box_upper_style <- 'border: 0px solid black; background: white; border-radius: 12px 12px 0px 0px; margin: 0px'
dist_selection_tab_box_lower_style <- "border-top: 1px solid black; background: white; border-radius: 0px 0px 12px 12px;"

dist_selection_input_report <- tags$button(class = "btn btn-default action-button", type = "button", id = "dist_selection_input_report",
                                           tags$span(h4(ph("notebook"), "Generate", 
                                                        style = "margin-top: 0px; padding: 3px 3px 5px 5px; color: black")),
                                           style = "margin-bottom: 0px; border-radius: 8px; padding-bottom: 0px; background: white")
                                      
