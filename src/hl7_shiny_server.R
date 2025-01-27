server <- function(input, output, session) {
  # Render the selected kable table
  output$table_output <- renderText({
    selection <- viz_df$Name == input$selected_table
    if (input$display_format == "String") {
      HTML(viz_df$String[selection])
    } else if (input$display_format == "Table") {
      HTML(viz_df$Table[selection])
    }
  })
}