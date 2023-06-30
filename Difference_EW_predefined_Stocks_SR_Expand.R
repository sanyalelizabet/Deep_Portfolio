<<<<<<< HEAD
ew_deltas_predef_sr_expand_df <-  as.data.frame(weights_predef_sr_expand)
colnames(ew_deltas_predef_sr_expand_df) <-colnames(returns_short)
dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
ew_deltas_predef_sr_expand_df$dates <- as.Date(dates_testing, format = "%Y-%m-%d")

ew_deltas_predef_sr_expand_long <- pivot_longer(ew_deltas_predef_sr_expand_df, cols = -dates, names_to = "Stock", values_to = "Delta")
# Convert weights_predef_sr_tc_expand to a data frame
ew_deltas_predef_sr_tc_expand_df <- as.data.frame(weights_predef_sr_tc_expand)

# Set column names of ew_deltas_predef_sr_tc_expand_df to match returns_short
colnames(ew_deltas_predef_sr_tc_expand_df) <- colnames(returns_short)

# Get the testing dates
dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]

# Convert dates_testing to Date format
ew_deltas_predef_sr_tc_expand_df$dates <- as.Date(dates_testing, format = "%Y-%m-%d")

# Pivot the data frame from wide to long format
ew_deltas_predef_sr_tc_expand_long <- pivot_longer(ew_deltas_predef_sr_tc_expand_df, cols = -dates, names_to = "Stock", values_to = "Delta")
=======
ew_deltas_fixed_sr_expand_df <-  as.data.frame(weights_fixed_sr_expand)
colnames(ew_deltas_fixed_sr_expand_df) <-colnames(returns_short)
dates_testing <- sort(data_predef_stocks$dates)[first_test:end_test]
ew_deltas_fixed_sr_expand_df$dates <- as.Date(dates_testing, format = "%Y-%m-%d")

ew_deltas_fixed_sr_expand_long <- pivot_longer(ew_deltas_fixed_sr_expand_df, cols = -dates, names_to = "Stock", values_to = "Delta")
>>>>>>> 173778f5e15b879c8c22810bcd2d271c34830ac5


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
<<<<<<< HEAD
        min = min(as.Date(ew_deltas_predef_sr_expand_long$dates)),
        max = max(as.Date(ew_deltas_predef_sr_expand_long$dates)),
        value = min(as.Date(ew_deltas_predef_sr_expand_long$dates)),
=======
        min = min(as.Date(ew_deltas_fixed_sr_expand_long$dates)),
        max = max(as.Date(ew_deltas_fixed_sr_expand_long$dates)),
        value = min(as.Date(ew_deltas_fixed_sr_expand_long$dates)),
>>>>>>> 173778f5e15b879c8c22810bcd2d271c34830ac5
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
<<<<<<< HEAD
    if (next_date > max(as.Date(ew_deltas_predef_sr_expand_long$dates))) {
      next_date <- min(as.Date(ew_deltas_predef_sr_expand_long$dates))
=======
    if (next_date > max(as.Date(ew_deltas_fixed_sr_expand_long$dates))) {
      next_date <- min(as.Date(ew_deltas_fixed_sr_expand_long$dates))
>>>>>>> 173778f5e15b879c8c22810bcd2d271c34830ac5
    }
    updateSliderInput(session, "date_slider", value = next_date)
  })
  
  output$difference_plot <- renderPlot({
    filtered_data <- subset(
<<<<<<< HEAD
      ew_deltas_predef_sr_expand_long,
=======
      ew_deltas_fixed_sr_expand_long,
>>>>>>> 173778f5e15b879c8c22810bcd2d271c34830ac5
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
