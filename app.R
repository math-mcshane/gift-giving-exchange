library(shiny)
library(DT)

# ---- Default dataset --------------------------------------------------------
default_data <- data.frame(
  id    = 1:5,
  name  = c("Alpha", "Bravo", "Charlie", "Delta", "Echo"),
  value = c(10, 20, 30, 40, 50),
  stringsAsFactors = FALSE
)

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Editable dataset"),

  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Integer input", value = 1, step = 1),
      actionButton("add_row", "Add row"),
      actionButton("reset", "Reset to default"),
      helpText("Double-click a cell in the input table to edit it.")
    ),

    mainPanel(
      h4("Input data (editable)"),
      DTOutput("input_table"),
      hr(),
      h4("Output"),
      DTOutput("output_table")
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Holds the current (possibly edited) dataset
  data_rv <- reactiveVal(default_data)

  # Editable input table
  output$input_table <- renderDT({
    datatable(
      data_rv(),
      editable = TRUE,
      rownames = FALSE,
      options  = list(pageLength = 10, dom = "tip")
    )
  })

  # Apply cell edits back to the stored dataset
  observeEvent(input$input_table_cell_edit, {
    data_rv(editData(data_rv(), input$input_table_cell_edit, rownames = FALSE))
  })

  # Append a blank row
  observeEvent(input$add_row, {
    df <- data_rv()
    new_row <- df[NA_integer_, , drop = FALSE][1, , drop = FALSE]
    rownames(new_row) <- NULL
    data_rv(rbind(df, new_row))
  })

  # Restore the default dataset
  observeEvent(input$reset, {
    data_rv(default_data)
  })

  # Validated integer input
  n_int <- reactive({
    req(input$n)
    validate(need(input$n == round(input$n), "Please enter a whole number."))
    as.integer(input$n)
  })

  # Result of modifying the dataset with the integer
  result <- reactive({
    df <- data_rv()
    n  <- n_int()

    # ------------------------------------------------------------------------
    # TODO: Insert your code here to modify `df` using the integer `n`.
    # `df` is the current edited dataset (a data.frame).
    # Assign the final result back to `df`.
    #
    # Example:
    #   df$value <- df$value * n
    # ------------------------------------------------------------------------

    df
  })

  # Output table
  output$output_table <- renderDT({
    datatable(
      result(),
      rownames = FALSE,
      options  = list(pageLength = 10, dom = "tip")
    )
  })
}

shinyApp(ui, server)
