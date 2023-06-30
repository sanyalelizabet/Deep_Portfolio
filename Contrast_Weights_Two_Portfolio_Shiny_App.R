# Define the UI
library(shiny)
library(lubridate)
ew_deltas_predef_sr_expand_long <- pf_weights_longer(backtest_predef_lstm_2feat)

ew_deltas_predef_sr_tc_expand_long <-  pf_weights_longer(backtest_res_predef_ret_sr)
# Define the UI
ui <- fluidPage(
  titlePanel("Difference"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "date_slider",
        "Select Date",
        min = min(as.Date(ew_deltas_predef_sr_expand_long$Dates)),
        max = max(as.Date(ew_deltas_predef_sr_expand_long$Dates)),
        value = min(as.Date(ew_deltas_predef_sr_expand_long$Dates)),
        timeFormat = "%Y-%m-%d",
        step = 1
      ),
      actionButton("toggle_update", "Toggle Auto Update")
    ),
    mainPanel(
      plotOutput("difference_plot1"),
      plotOutput("difference_plot2")
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
    if (next_date > max(as.Date(ew_deltas_predef_sr_expand_long$Dates))) {
      next_date <- min(as.Date(ew_deltas_predef_sr_expand_long$Dates))
    }
    updateSliderInput(session, "date_slider", value = next_date)
  })
  
  output$difference_plot1 <- renderPlot({
    filtered_data <- subset(
      ew_deltas_predef_sr_expand_long,
      Dates == as.character(selected_date())
    )
    
    ggplot(filtered_data, aes(x = Stock, y = Weights, fill = Stock)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Stock", y = "Weights",
           title = paste("Weights compared - SR Loss", selected_date())) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      coord_cartesian(ylim = c(-0.05, 0.05), xlim = c(1, nrow(filtered_data)))
  })
  
  output$difference_plot2 <- renderPlot({
    filtered_data2 <- subset(
      ew_deltas_predef_sr_tc_expand_long,
      Dates == as.character(selected_date())
    )
    
    ggplot(filtered_data2, aes(x = Stock, y = Weights, fill = Stock)) +
      geom_col(show.legend = FALSE) +
      labs(x = "Stock", y = "Weights",
           title = paste("Weights compared - SR Transaction Cost Adj. Loss", selected_date())) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      coord_cartesian(ylim = c(-0.05, 0.05), xlim = c(1, nrow(filtered_data2)))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
