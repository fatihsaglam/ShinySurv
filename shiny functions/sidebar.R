sidebar <- sidebarMenu(
  minified = FALSE,
  tags$head(
    tags$style(
      HTML(
        "#div_id selectpicker-control.single selectpicker-input:after{
          content: none;
        }"
      )
    )
  ),
  
  div(fluidRow(
    column(width = 5,
             radioGroupButtons(inputId = "data_vars", 
                               label = "Variables", 
                               choices = "No Variable",
                               direction = "vertical", 
                               disabled = TRUE)
    ), 
    
    column(width = 7,
           lapply(1:length(surv_var_ids), function(m) {
             fluidRow(
               column(3,
                      actionButton(inputId = paste0(surv_var_ids[m],"_add"), 
                                   label = "", 
                                   icon = icon("arrow-right"), 
                                   width = "40px"),
               ),
               column(9,
                      div(id = "div_id", 
                          pickerInput(inputId = surv_var_ids[m], 
                                      label = surv_var_labels[m], 
                                      choices = "No Variable", 
                                      width = "150px",
                                      choicesOpt = list(content = "<span class = 'label label-danger'>No Variable</span>"),
                                      options = pickerOptions())), 
               ),
               tags$style(type = "text/css", 
                          paste0("#", surv_var_ids[m], "_add { margin: 36px 0px 0px}"))
             )
           }),
           checkboxInput(inputId = "interval",
                         label = "Interval Censor",
                         value = FALSE, 
                         width = "100%")
    )
  )),
  
  div(selectInput(inputId = "load_import_data",
              label = "Data",
              choices = c("Import data",
                          "Load data"),
              selected = "Import data"), style = "margin-bottom: 0px"),
  
  
  
  ##---------------------------------------------------------------
  ##                          Import data                         -
  ##---------------------------------------------------------------
  conditionalPanel(condition = "input.load_import_data == 'Import data'",
                   fileInput(inputId = "import_data_path",
                             label = "Import data",
                             accept = "csv"),
                   checkboxInput(inputId = "header",
                                 label = "Header",
                                 value = TRUE),
                   actionButton(inputId = "import_data",
                                label = "Import (csv file only)",
                                style = "border: 1px solid black"),
                   br(),
                   column(width = 12,
                          "Preview",
                          verbatimTextOutput(outputId = "import_data_preview_import", 
                                             placeholder = TRUE))
  ),
  ##---------------------------------------------------------------
  
  
  ##---------------------------------------------------------------
  ##                          Load data                         -
  ##---------------------------------------------------------------
  conditionalPanel(condition = "input.load_import_data == 'Load data'",
                   selectInput(inputId = "loaded_data",
                               label = "Load data",
                               choices = c("right censored",
                                           "left and right censored",
                                           "interval censored",
                                           "lung",
                                           "ovarian"),
                               selected = "right censored"),
                   actionButton(inputId = "load_data",
                                label = "Load",
                                style = "border: 1px solid black"),
                   br(),
                   column(width = 12,
                          "Preview",
                          verbatimTextOutput(outputId = "import_data_preview_load", 
                                             placeholder = TRUE))
  )
  ##---------------------------------------------------------------
  
)
