library(shiny)
library(bslib)
library(dplyr)
library(DT)

# Load starwars data
data("starwars", package = "dplyr")

ui <- page_sidebar(
  title = "Data Types in R - Star Wars Edition",
  
  sidebar = sidebar(
    selectInput(
      "data_type",
      "Choose data type to create:",
      choices = list(
        "Matrix" = "matrix",
        "Array" = "array",
        "List" = "list",
        "Data Frame" = "dataframe",
        "Factor" = "factor",
        "Date" = "date"
      ),
      selected = "matrix"
    ),
    
    conditionalPanel(
      condition = "input.data_type == 'matrix'",
      checkboxGroupInput(
        "matrix_vars",
        "Select numeric variables:",
        choices = c("height", "mass", "birth_year"),
        selected = c("height", "mass")
      )
    ),
    
    conditionalPanel(
      condition = "input.data_type == 'array'",
      checkboxGroupInput(
        "array_vars", 
        "Select numeric variables:",
        choices = c("height", "mass", "birth_year"),
        selected = c("height", "mass", "birth_year")
      )
    ),
    
    conditionalPanel(
      condition = "input.data_type == 'list'",
      checkboxGroupInput(
        "list_vars",
        "Select variables for list:",
        choices = c("name", "height", "mass", "species", "films"),
        selected = c("name", "height", "species")
      )
    ),
    
    conditionalPanel(
      condition = "input.data_type == 'factor'",
      selectInput(
        "factor_var",
        "Select categorical variable:",
        choices = c("species", "gender", "hair_color", "eye_color"),
        selected = "species"
      )
    )
  ),
  
  # Main content area
  card(
    card_header("Created Data Structure"),
    verbatimTextOutput("data_display"),
    full_screen = TRUE
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Structure Summary"),
      verbatimTextOutput("structure_info")
    ),
    
    card(
      card_header("Properties & Dimensions"),
      verbatimTextOutput("dimension_info")
    )
  )
)

server <- function(input, output, session) {
  
  created_data <- reactive({
    # Get clean starwars data
    clean_data <- starwars %>%
      select(name, height, mass, birth_year, species, gender, hair_color, eye_color, films) %>%
      slice(1:10) %>%
      mutate(
        # Create some dates for demonstration
        fictional_date = as.Date("1977-05-25") + sample(0:365, 10),
        # Clean up species for factors
        species = ifelse(is.na(species), "Unknown", species)
      )
    
    switch(input$data_type,
      "matrix" = {
        if(length(input$matrix_vars) > 0) {
          selected_data <- clean_data[input$matrix_vars]
          matrix(as.matrix(selected_data), nrow = nrow(selected_data))
        } else {
          matrix(c(1, 2, 3, 4), nrow = 2)
        }
      },
      
      "array" = {
        if(length(input$array_vars) > 0) {
          selected_data <- clean_data[input$array_vars]
          array(as.matrix(selected_data), dim = c(5, 2, length(input$array_vars)))
        } else {
          array(1:24, dim = c(3, 4, 2))
        }
      },
      
      "list" = {
        if(length(input$list_vars) > 0) {
          result_list <- list()
          for(var in input$list_vars) {
            result_list[[var]] <- clean_data[[var]]
          }
          result_list
        } else {
          list(names = clean_data$name, heights = clean_data$height)
        }
      },
      
      "dataframe" = {
        clean_data %>% select(name, height, mass, species, gender)
      },
      
      "factor" = {
        factor(clean_data[[input$factor_var]])
      },
      
      "date" = {
        clean_data$fictional_date
      }
    )
  })
  
  output$data_display <- renderText({
    data <- created_data()
    
    # Format output based on data type
    if(input$data_type == "matrix") {
      paste("MATRIX:\n", paste(capture.output(print(data)), collapse = "\n"))
    } else if(input$data_type == "array") {
      paste("ARRAY:\n", paste(capture.output(print(data)), collapse = "\n"))
    } else if(input$data_type == "list") {
      paste("LIST:\n", paste(capture.output(str(data)), collapse = "\n"))
    } else if(input$data_type == "dataframe") {
      paste("DATA FRAME:\n", paste(capture.output(print(head(data))), collapse = "\n"))
    } else if(input$data_type == "factor") {
      paste("FACTOR:\n", paste(capture.output(print(data)), collapse = "\n"))
    } else if(input$data_type == "date") {
      paste("DATE:\n", paste(capture.output(print(data)), collapse = "\n"))
    }
  })
  
  output$structure_info <- renderText({
    data <- created_data()
    
    class_info <- class(data)
    type_info <- typeof(data)
    
    paste0(
      "Class: ", paste(class_info, collapse = ", "), "\n",
      "Type: ", type_info, "\n",
      "Mode: ", mode(data), "\n",
      "Storage: ", storage.mode(data)
    )
  })
  
  output$dimension_info <- renderText({
    data <- created_data()
    
    if(input$data_type == "matrix") {
      paste0(
        "Dimensions: ", paste(dim(data), collapse = " x "), "\n",
        "Rows: ", nrow(data), "\n",
        "Columns: ", ncol(data), "\n",
        "Total elements: ", length(data)
      )
    } else if(input$data_type == "array") {
      paste0(
        "Dimensions: ", paste(dim(data), collapse = " x "), "\n",
        "Total elements: ", length(data), "\n",
        "Array structure: 3D"
      )
    } else if(input$data_type == "list") {
      paste0(
        "Length: ", length(data), "\n",
        "Elements: ", paste(names(data), collapse = ", "), "\n",
        "Type: Heterogeneous collection"
      )
    } else if(input$data_type == "dataframe") {
      paste0(
        "Dimensions: ", paste(dim(data), collapse = " x "), "\n",
        "Rows: ", nrow(data), "\n",
        "Columns: ", ncol(data), "\n",
        "Variables: ", paste(names(data), collapse = ", ")
      )
    } else if(input$data_type == "factor") {
      paste0(
        "Length: ", length(data), "\n",
        "Levels: ", nlevels(data), "\n",
        "Categories: ", paste(levels(data)[1:min(3, nlevels(data))], collapse = ", "), 
        if(nlevels(data) > 3) "..." else ""
      )
    } else if(input$data_type == "date") {
      paste0(
        "Length: ", length(data), "\n",
        "Range: ", min(data, na.rm = TRUE), " to ", max(data, na.rm = TRUE), "\n",
        "Format: YYYY-MM-DD"
      )
    }
  })
}

shinyApp(ui = ui, server = server)
