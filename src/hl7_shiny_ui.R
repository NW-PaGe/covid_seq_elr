ui <- fluidPage(
  titlePanel("HL7 Viewer"),
  fluidRow(
    column(
      width=6,
      radioButtons(
        inputId = "display_format",
        label = "View as:",
        choices = c("Table"="Table", "String"="String"),
        selected = "Table",
        inline = TRUE  # Display radio buttons horizontally
      )
    ),
    column(
      width=6,
      selectInput("selected_table", "Choose a Sumbitter/Case:", 
                  choices = viz_df$Name, selected = viz_df$Name[1])
    )
  ),
  tags$hr(style = "border-top: 2px solid #000000; margin: 5px 0;"),  # Adjust margin size before/after hr
  DT::DTOutput("hl7_table"),
  htmlOutput("hl7_string")
)
