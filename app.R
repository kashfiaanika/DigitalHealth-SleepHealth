# app.R ----

library(shiny)
library(tidyverse)

# Load cleaned data
sleep <- read_csv("data/sleep_clean.csv") %>%
  mutate(
    gender = as.factor(gender),
    occupation = as.factor(occupation),
    bmi_category = factor(
      bmi_category,
      levels = c("Normal", "Overweight", "Obese")
    ),
    sleep_disorder = as.factor(sleep_disorder)
  )


ui <- fluidPage(
  titlePanel("Sleep Health & Lifestyle Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      sliderInput("age_range", "Age range:",
                  min = min(sleep$age),
                  max = max(sleep$age),
                  value = c(min(sleep$age), max(sleep$age))),
      selectInput("disorder", "Sleep disorder:",
                  choices = c("All", levels(sleep$sleep_disorder)),
                  selected = "All"),
      selectInput("bmi_cat", "BMI category:",
                  choices = c("All", levels(sleep$bmi_category)),
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview table",
                 h3("First rows of filtered data"),
                 tableOutput("table_filtered")),
        tabPanel("Sleep vs Stress",
                 h3("Sleep quality vs Stress level"),
                 plotOutput("plot_sleep_stress"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  # Reactive filtered dataset based on inputs
  filtered_data <- reactive({
    df <- sleep
    
    # Filter by age range
    df <- df %>%
      filter(age >= input$age_range[1],
             age <= input$age_range[2])
    
    # Filter by sleep disorder (if not "All")
    if (input$disorder != "All") {
      df <- df %>% filter(sleep_disorder == input$disorder)
    }
    
    # Filter by BMI category (if not "All")
    if (input$bmi_cat != "All") {
      df <- df %>% filter(bmi_category == input$bmi_cat)
    }
    
    df
  })
  
  # Show first rows of the filtered data
  output$table_filtered <- renderTable({
    head(filtered_data(), 10)
  })
  
  # Plot: Sleep quality vs Stress level
  output$plot_sleep_stress <- renderPlot({
    ggplot(filtered_data(),
           aes(x = stress_level, y = quality_of_sleep,
               color = sleep_disorder)) +
      geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
      labs(x = "Stress level (1–10)",
           y = "Quality of sleep (1–10)",
           color = "Sleep disorder") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)
