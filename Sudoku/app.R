library(shiny)
library(magrittr) # For %>% operator

# UI: Define the layout
ui <- fluidPage(
    titlePanel("Sudoku Input Template"),
    sidebarLayout(
        sidebarPanel(
            actionButton("check", "Check Sudoku"),
            actionButton("clear", "Clear All"),
            p("Enter numbers (1-9) or leave blank."),
            p("Blanks or invalid entries will default to 0.")
        ),
        mainPanel(
            uiOutput("sudokuTable") # Use `uiOutput` for dynamic HTML table rendering
        )
    )
)

# Server: Define the logic
server <- function(input, output, session) {
    # Create a reactive grid for Sudoku input
    grid <- reactiveValues(data = matrix("", nrow = 9, ncol = 9))
    
    # Render the input grid
    output$sudokuTable <- renderUI({
        # Generate a 9x9 table with input fields
        tags$table(
            lapply(1:9, function(row) {
                tags$tr(
                    lapply(1:9, function(col) {
                        tags$td(
                            textInput(paste0("cell_", row, "_", col), label = NULL, value = grid$data[row, col], width = "50px")
                        )
                    })
                )
            })
        )
    })
    
    # Observe and update the grid values based on user input
    observe({
        for (row in 1:9) {
            for (col in 1:9) {
                cell_id <- paste0("cell_", row, "_", col)
                value <- input[[cell_id]]
                grid$data[row, col] <- ifelse(value %in% as.character(1:9), value, "")
            }
        }
    })
    
    # Action: Clear the grid
    observeEvent(input$clear, {
        grid$data <- matrix("", nrow = 9, ncol = 9)
        for (row in 1:9) {
            for (col in 1:9) {
                updateTextInput(s
                                