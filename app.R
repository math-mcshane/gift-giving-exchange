library(shiny)
library(DT)
library(tidyverse)

# ---- Default dataset --------------------------------------------------------
presents = tibble(
  Person = c("Amy", "Jess", "Carly", "Joe", "Christian", "Ryan", "Ted", "James", "Bree", "Hannah"),
  Partner = c("Ryan", "Ted", "James", "Bree", "Hannah", "Amy", "Jess", "Carly", "Joe", "Christian"),
  Recipient1 = sample(c("Amy", "Jess", "Carly", "Joe", "Christian", "Ryan", "Ted", "James", "Bree", "Hannah")),
  Recipient2 = sample(c("Amy", "Jess", "Carly", "Joe", "Christian", "Ryan", "Ted", "James", "Bree", "Hannah"))
)


# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Editable dataset"),

  sidebarLayout(
    sidebarPanel(
      width = 5,
      numericInput("n", "Enter an integer between 1000 and 1000000", value = 1, step = 1),
      actionButton("add_row", "Add row"),
      actionButton("reset", "Reset to default"),
      helpText("Double-click a cell in the input table to edit it."),
      hr(),
      h4("Input data (editable)"),
      DTOutput("input_table")
    ),

    mainPanel(
      width = 7,
      h4("Output"),
      DTOutput("output_table")
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Holds the current (possibly edited) dataset
  data_rv <- reactiveVal(presents)

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

  # # Append a blank row
  # observeEvent(input$add_row, {
  #   df <- data_rv()
  #   new_row <- df[NA_integer_, , drop = FALSE][1, , drop = FALSE]
  #   rownames(new_row) <- NULL
  #   data_rv(rbind(df, new_row))
  # })

  # Restore the default dataset
  observeEvent(input$reset, {
    data_rv(presents)
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
    set.seed(n)

    presents = presents |>
      rowwise() |>
      mutate(
        rec1_swap = Person == Recipient1 | Partner == Recipient1
      ) |>
      ungroup()

    i = 0
    while (any(presents$rec1_swap == TRUE)) {
      i = i + 1
      if (i >= 100) presents$Recipient1 = sample(c("Amy", "Jess", "Carly", "Joe", "Christian", "Ryan", "Ted", "James", "Bree", "Hannah"))
      swap_rows = which(presents$rec1_swap == TRUE)
      presents[swap_rows, "Recipient1"] = slice_sample(presents[swap_rows, "Recipient1"], n = length(swap_rows))
      presents = presents |>
        rowwise() |>
        mutate(
          rec1_swap = Person == Recipient1 | Partner == Recipient1
        ) |>
        ungroup()
    }

    presents = presents |>
      select(!rec1_swap) |>
      rowwise() |>
      left_join(
        y = presents |>
          select(Person, Partner) |>
          rename(rec2_partner = Partner),
        by = join_by(Recipient1 == Person)
      ) |>
      mutate(
        rec2_swap = Person == Recipient2 |
          Partner == Recipient2 |
          Recipient1 == Recipient2 |
          rec2_partner == Recipient2
      ) |>
      ungroup() |>
      select(!rec2_partner)

    i = 0
    while (any(presents$rec2_swap == TRUE)) {
      i = i + 1
      if (i >= 100) presents$Recipient2 = sample(c("Amy", "Jess", "Carly", "Joe", "Christian", "Ryan", "Ted", "James", "Bree", "Hannah"))
      swap_rows = which(presents$rec2_swap == TRUE)
      presents[swap_rows, "Recipient2"] = slice_sample(presents[swap_rows, "Recipient2"], n = length(swap_rows))
      presents = presents |>
        rowwise() |>
        left_join(
          y = presents |>
            select(Person, Partner) |>
            rename(rec2_partner = Partner),
          by = join_by(Recipient1 == Person)
        ) |>
        mutate(
          rec2_swap = Person == Recipient2 |
            Partner == Recipient2 |
            Recipient1 == Recipient2 |
            rec2_partner == Recipient2
        ) |>
        ungroup() |>
        select(!rec2_partner)
    }

    df = presents |>
      select(!c(rec2_swap, Partner))
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
