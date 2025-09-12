library(shiny)
library(tidyverse)
library(ggplot2)
library(reticulate)

# UI
ui <- fluidPage(
  titlePanel("Mixed Effects Analysis - Residuals by Study"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("study_id", 
                  "Select number of studies:",
                  choices = c(1,2,5,10),
                  selected = 1),
      
      actionButton("run_analysis", 
                   "Run Analysis", 
                   class = "btn-primary"),
      
      br(), br(),
      
      helpText("Select a study ID and click 'Run Analysis' to generate data, 
               run the mixed effects model in SLC, and display the residuals histogram.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", 
                 plotOutput("residuals_histogram", height = "500px")),
        tabPanel("Model Results", 
                 verbatimTextOutput("model_summary")),
        tabPanel("Data Summary", 
                 tableOutput("data_summary"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store results
  values <- reactiveValues(
    mixed_results = NULL,
    fixed_effects = NULL,
    variance_components = NULL,
    study_data = NULL
  )
  
  # Initialize SLC connection
  slc_setup <- reactive({
    req(input$run_analysis)
    
    withProgress(message = "Initializing SLC...", value = 0.1, {
      
      # Set up Python path to include SLC
      py_run_string("import sys; sys.path.insert(0, '/opt/altair/slc/2025/python')")
      
      slc <- import("slc.slc")
      slc_lib <- import("slc.library")
      
      slc_client <- slc$Slc()
      work_lib <- slc_client$get_library('WORK')
      
      list(client = slc_client, work_lib = work_lib)
    })
  })
  
  # Generate and analyze data when button is clicked
  observeEvent(input$run_analysis, {
    
    withProgress(message = "Running analysis...", {
      
      incProgress(0.2, detail = "Generating data...")
      
      # Set random seed for reproducibility
      set.seed(12345)
      
      # Generate data for selected study
      study_data <- expand_grid(
        study_id = 1:as.numeric(input$study_id),
        site_id = 1:2,
        subject_id = 1:20,
        visit = 1:4
      ) |>
        mutate(
          unique_subject_id = (study_id * 100) + (site_id * 20) + subject_id,
          subject_intercept = rep(rnorm(n_distinct(unique_subject_id), mean = 0, sd = 2), 
                                 each = 4)[row_number()],
          age = runif(n(), min = 18, max = 75),
          gender = rbinom(n(), size = 1, prob = 0.52),
          treatment = rbinom(n(), size = 1, prob = 0.5),
          baseline_score = rnorm(n(), mean = 50, sd = 10),
          study_effect = rnorm(n(), mean = 0, sd = 1.5),
          site_effect = rnorm(n(), mean = 0, sd = 1.0),
          time_trend = visit * 2.5,
          treatment_effect = treatment * 8.3,
          age_effect = (age - 45) * 0.2,
          gender_effect = gender * 3.1,
          error = rnorm(n(), mean = 0, sd = 3.2),
          outcome = 45 + time_trend + treatment_effect + age_effect + 
                   gender_effect + study_effect + site_effect + 
                   subject_intercept + error
        ) |>
        mutate(
          outcome = if_else(runif(n()) < 0.05, NA_real_, outcome),
          age = if_else(runif(n()) < 0.02, NA_real_, age)
        ) |>
        mutate(
          age_group = case_when(
            is.na(age) ~ "Unknown",
            age < 30 ~ "Young",
            age < 50 ~ "Middle",
            age >= 50 ~ "Older"
          ),
          treatment_group = if_else(treatment == 1, "Active", "Placebo"),
          gender_label = if_else(gender == 1, "Female", "Male")
        )
      
      values$study_data <- study_data
      
      incProgress(0.3, detail = "Setting up SLC connection...")
      
      # Get SLC setup
      slc_env <- slc_setup()
      
      incProgress(0.4, detail = "Uploading data to SLC...")
      
      # Upload data to SLC
      slc_env$work_lib$create_dataset_from_dataframe("study_data_sas", study_data)
      
      incProgress(0.6, detail = "Running mixed effects model...")
      
      # Define and run SAS code
      sas_code <- "
      proc mixed data=study_data_sas method=reml;
          class unique_subject_id study_id treatment_group visit;
          model outcome = visit treatment_group visit*treatment_group / 
                solution outp=mixed_results;
          random intercept / subject=unique_subject_id;
          random intercept / subject=study_id;
          repeated visit / subject=unique_subject_id type=ar(1);    
          ods output SolutionF=fixed_effects
                     CovParms=variance_components;
      run;
      "
      
      result <- slc_env$client$submit(sas_code)
      
      incProgress(0.8, detail = "Retrieving results...")
      
      # Get results
      values$fixed_effects <- slc_env$work_lib$open_dataset("fixed_effects", "r")$to_data_frame()
      values$mixed_results <- slc_env$work_lib$open_dataset("mixed_results", "r")$to_data_frame()
      values$variance_components <- slc_env$work_lib$open_dataset("variance_components", "r")$to_data_frame()
      
      incProgress(1.0, detail = "Complete!")
    })
  })
  
  # Histogram output
  output$residuals_histogram <- renderPlot({
    req(values$mixed_results)
    
    ggplot(values$mixed_results, aes(x = Resid)) +
      geom_histogram(binwidth = function(x) diff(range(x, na.rm = TRUE))/30, 
                     fill = "lightblue", color = "black", alpha = 0.7) +
      labs(
        title = paste("Histogram of Residuals - Study", input$study_id),
        x = "Residuals",
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  
  # Model summary output
  output$model_summary <- renderPrint({
    req(values$fixed_effects, values$variance_components)
    
    cat("Fixed Effects:\n")
    cat("==============\n")
    print(values$fixed_effects)
    
    cat("\n\nVariance Components:\n")
    cat("===================\n")
    print(values$variance_components)
    
    cat("\n\nResiduals Summary:\n")
    cat("=================\n")
    if (!is.null(values$mixed_results)) {
      print(summary(values$mixed_results$Resid))
    }
  })
  
  # Data summary output
  output$data_summary <- renderTable({
    req(values$study_data)
    
    values$study_data |>
      group_by(treatment_group, visit) |>
      summarise(
        n = n(),
        mean_outcome = round(mean(outcome, na.rm = TRUE), 2),
        sd_outcome = round(sd(outcome, na.rm = TRUE), 2),
        median_outcome = round(median(outcome, na.rm = TRUE), 2),
        .groups = "drop"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)