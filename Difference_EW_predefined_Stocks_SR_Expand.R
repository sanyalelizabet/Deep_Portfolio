ew_deltas_predef_sr_expand_df <-  as.data.frame(weights_predef_sr_expand)
colnames(ew_deltas_predef_sr_expand_df) <-colnames(returns_short)
dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
ew_deltas_predef_sr_expand_df$dates <- as.Date(dates_testing, format = "%Y-%m-%d")

ew_deltas_predef_sr_expand_long <- pivot_longer(ew_deltas_predef_sr_expand_df, cols = -dates, names_to = "Stock", values_to = "Delta")


library(shiny)
library(ggplot2)
library(zoo)
library(lubridate)

# Define the UI
ui <- fluidPage(
  titlePanel("Difference from Equal Weight"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "date_slider",
        "Select Date",
        min = min(as.Date(ew_deltas_predef_sr_expand_long$dates)),
        max = max(as.Date(ew_deltas_predef_sr_expand_long$dates)),
        value = min(as.Date(ew_deltas_predef_sr_expand_long$dates)),
        timeFormat = "%Y-%m-%d",
        step = 1
      ),
      actionButton("toggle_update", "Toggle Auto Update")
    ),
    mainPanel(
      plotOutput("difference_plot")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Create a reactive object for the selected date
  selected_date <- reactive({
    ceiling_date(input$date_slider, "month") - days(1)
  })
  
  # Initialize the update flag
  update_flag <- reactiveVal(TRUE)
  
  # Update the slider input with the last day of the selected month
  observeEvent(input$toggle_update, {
    update_flag(!update_flag())
  })
  
  observe({
    req(update_flag())
    Sys.sleep(1)  # Pause for 1 second
    next_date <- ceiling_date(input$date_slider, "month")
    if (next_date > max(as.Date(ew_deltas_predef_sr_expand_long$dates))) {
      next_date <- min(as.Date(ew_deltas_predef_sr_expand_long$dates))
    }
    updateSliderInput(session, "date_slider", value = next_date)
  })
  
  output$difference_plot <- renderPlot({
    filtered_data <- subset(
      ew_deltas_predef_sr_expand_long,
      dates == as.character(selected_date())
    )
    
    ggplot(filtered_data, aes(x = Stock, y = Delta, fill = Stock)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Stock", y = "Difference",
           title = paste("Difference from Equal Weight -", selected_date())) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      coord_cartesian(ylim = c(-0.2, 0.2), xlim = c(1, nrow(filtered_data)))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
