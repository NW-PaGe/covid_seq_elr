ui <- fluidPage(
  # Custom CSS tags to adjust DT button colors
  tags$style(HTML("
    /* Change the background color and text color of the dt buttons */
    .dt-buttons .dt-button.buttons-collection {
      background-color: #78c2ad !important;  /* Mint color for the button */
      color: white  !important;  /* White text */
      border: none  !important;  /* No border */
    }
    
    /* On hover, change the background color */
    .dt-buttons .dt-button.buttons-collection:hover {
      background-color: #66a593 !important;  /* Darker mint when hovered */
      color: white  !important;  /* White text */
      border: none  !important;  /* No border */
    }
  ")),
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
