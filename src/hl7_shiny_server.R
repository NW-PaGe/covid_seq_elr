library(DT)

server <- function(input, output, session) {
  selection <- reactive({
    viz_df$Name == input$selected_table
  })
  output$hl7_string <- renderText({
    if (input$display_format == "String") {
      HTML(viz_df$String[selection()])
    } else {
      ""
    }
  })
  output$hl7_table <- DT::renderDT({
    if (input$display_format == "Table") {
      example_tbl <- parse_hl7(
        gsub('<br>', '\r\n', viz_df$String[selection()], fixed=T),
        remove_pii = F
        )
      example_tbl$Segment <- dplyr::if_else(
        is.na(example_tbl$Instance), 
        example_tbl$Segment, 
        paste0(example_tbl$Segment, '[', example_tbl$Instance, ']') 
      )
      example_tbl <- select(example_tbl, Order, Segment, Position, Name, Value)
      example_tbl$Segment <- as.factor(example_tbl$Segment)
      # Create searchCols list dynamically
      searchCols_val <- vector("list", ncol(example_tbl)) # add one because row names are included at front
      #searchCols_val[[which(names(example_tbl) == 'Value')]] <- list(search = 'strain|accession|lineage')
      
      DT::datatable(
        example_tbl,
        rownames = FALSE, # turn off row names
        extensions = c(
          "RowGroup", # allows for grouping of rows by col
          "FixedHeader", # allows for fixing headers during vertical scroll
          "Buttons" # allows for built-in copy/csv/print/pdf/colvis buttons
        ),  
        escape = FALSE, # Allow tool tips labels in html
        filter = "top", # Add filters to top of table
        selection = list(mode="single", target="row"), # make 
        options = list(
          search = list(regex = TRUE, smart = FALSE), # Allow for regex searches
          searchCols = searchCols_val, # set regex for "Value" field
          paging = FALSE,  # Disable pagination (show all rows)
          fixedHeader = TRUE, # fix header while scrolling
          rowGroup = list(dataSrc = which(names(example_tbl) == "Segment") - 1),  # Group by the "Segment" column (5th column is assumed to be Segment)
          columnDefs = list(
            list(targets = "_all", searchable = TRUE),  # Enable searching for all columns
            list(targets = which(names(example_tbl) == "Segment") - 1, visible = FALSE)  # Hide "Segment" column
          ),
          dom = "Bfrtip",  # Include Buttons in the table UI, for some reason Buttons is not working if only includes in `extensions`
          buttons = list(
            list(
              extend = 'collection',
              text = "View Sequencing Only",
              action = DT::JS(
              "function (e, dt, node, config) {",
              #    Define the regex pattern for filtering
              "   var pattern = /<span/i;",
              #    Clear any existing column filter for the column of interest
              "   dt.column(4).search('');",
              #    Apply the regex filter to the Value column
              "   dt.column(4).search(pattern.source, true, false).draw();",
              "}"
              )
            ),
            list(
              extend = 'collection',
              text = 'View ALL',
              action = DT::JS(
              "function (e, dt, node, config) {",
              # Clear any filters applied to the Value column
              "   dt.column(4).search('').draw();",
              "}"
              )
            )
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            # Automatically apply the sequencing only filter
            "   var table = this.api();",
            "   var pattern = /<span/i;",
            "   table.column(4).search(pattern.source, true, false).draw();",
            #  Change header color
            "   $(this.api().table().header()).css({'background-color': '#C3B1E1'});",
            "}"
          )
        )
      )
    } else {
      NULL
    }
  })
}
