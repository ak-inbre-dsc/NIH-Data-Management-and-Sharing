library(shiny)
library(dplyr)
library(DT)
library(stringr)

# Load CSV
# NOTE: This assumes 'Sample_DMS.csv' is in the same directory as this app.R file.
plans <- read.csv("Sample_DMS.csv", stringsAsFactors = FALSE)

# --- Clean and expand multi-value fields ---
# Remove quotes and trim whitespace
plans$Data.Type <- str_replace_all(plans$Data.Type, '"', "")
plans$Research.Area <- str_replace_all(plans$Research.Area, '"', "")

# Get unique choices for filters
data_type_choices <- plans$Data.Type %>%
  str_split(",") %>%
  unlist() %>%
  str_trim() %>%
  unique() %>%
  sort()

research_area_choices <- plans$Research.Area %>%
  str_split(",") %>%
  unlist() %>%
  str_trim() %>%
  unique() %>%
  sort()

# Get the last modified date of the CSV file
last_updated_date <- format(file.info("Sample_DMS.csv")$mtime, "%B %d, %Y")

# UI with custom CSS for a professional look
ui <- fluidPage(
  # Add a header for custom styling
  tags$head(
    # Link to Google Fonts for a professional font (Lato)
    tags$link(href = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap", rel = "stylesheet"),
    
    # Custom CSS for the blue color palette and overall professional look
    tags$style(HTML("
      /* Use Lato font for all text */
      body {
        font-family: 'Lato', sans-serif;
        background-color: #f0f4f8; /* Light gray-blue background */
      }

      /* Style the header container using flexbox for alignment */
      .header-container {
        display: flex;
        align-items: center;
        justify-content: space-between;
        width: 100%;
        margin-bottom: 20px;
      }
      
      /* Style the main title panel container */
      .title {
        background-color: transparent; /* Remove the blue bar background */
        text-align: center;
        flex-grow: 1; /* Allows the title to fill the space */
        margin: 0 20px; /* Add some margin between logos and title */
      }
      
      /* New style for the main app title h1 tag */
      .app-title {
        color: #003366; /* Set the text color */
        font-weight: 700;
        font-size: 35px;
      }

      /* Style the sidebar panel */
      .well {
        background-color: #e3f2fd; /* Lighter blue */
        border: none;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        padding: 20px;
      }
      
      /* Style all h4 headings */
      h4 {
        color: #004080; /* Corporate blue */
        border-bottom: 2px solid #b3d9ff; /* Light blue underline */
        padding-bottom: 5px;
        margin-top: 25px;
        font-weight: 700;
      }
      
      /* Style select input boxes */
      .form-control, .selectize-input {
        border-radius: 5px;
        border-color: #90caf9;
      }

      /* Style the Reset Filters button */
      .btn {
        background-color: #007bff; /* Standard blue for buttons */
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        font-weight: 700;
        transition: background-color 0.3s ease;
      }
      .btn:hover {
        background-color: #0056b3; /* Darker blue on hover */
      }
      
      /* Add some spacing and padding for a cleaner layout */
      .shiny-input-container {
        margin-bottom: 15px;
      }
      
      /* Style the DT table for better readability */
      .dataTables_wrapper {
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
      }
      table.dataTable thead th {
        background-color: #b3d9ff;
        color: #003366;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color: #f4f9ff;
      }
      
      /* Style the document viewer iframe */
      .iframe-container {
        border: 1px solid #ddd;
        border-radius: 8px;
        overflow: hidden;
      }
      
      /* Use a larger fixed height for the left logo */
      .logo-left img {
        height: 140px; 
      }
      
      /* Use a larger fixed height for the right logo */
      .logo-right img {
        height: 100px;
      }
      
      /* Style the container for the last updated text */
      .main-panel-container {
        position: relative;
      }

      /* Position the last updated text at the bottom right */
      .last-updated {
        text-align: right;
        font-size: 12px;
        color: #666;
        margin-top: 15px;
      }
    "))
  ),
  
  # A flexbox container to hold the logos and the title
  div(class = "header-container",
      div(class = "logo-left", 
          tags$img(src = "DSC_logo.png", alt = "DSC Logo")
      ),
      div(class = "title", tags$h1("Examples: Data Management & Sharing Plan", class = "app-title")),
      div(class = "logo-right", 
          tags$img(src = "INBRE_logo.png", alt = "INBRE Logo")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", "Subject Type:", 
                  choices = sort(unique(plans$Subject.Type)), multiple = TRUE),
      selectInput("institute", "Institute:", 
                  choices = sort(unique(plans$Institute)), multiple = TRUE),
      # Clinical placed directly after Institute as requested
      selectInput("clinical", "Clinical/Non-Clinical:", 
                  choices = sort(unique(plans$Clinical.Non.Clinical)), multiple = TRUE),
      selectInput("research", "Research Area:", 
                  choices = research_area_choices, multiple = TRUE),
      selectInput("psdata", "Primary/Secondary Data Used:", 
                  choices = sort(unique(plans$Primary.Secondary.Data.used)), multiple = TRUE),
      selectInput("datatype", "Data Type:", 
                  choices = data_type_choices, multiple = TRUE),
      selectInput("date", "Year Created:", 
                  choices = sort(unique(plans$Date)), multiple = TRUE),
      actionButton("reset_filters", "Reset Filters"),
      div(class = "last-updated", paste("Last Updated:", last_updated_date))
    ),
    mainPanel(
      h4("Matching Plans"),
      DTOutput("table"),
      br(),
      h4("Document Viewer"),
      div(class = "iframe-container", uiOutput("doc_viewer"))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filteredData <- reactive({
    df <- plans
    
    if (length(input$subject) > 0) {
      df <- df %>% filter(Subject.Type %in% input$subject)
    }
    if (length(input$institute) > 0) {
      df <- df %>% filter(Institute %in% input$institute)
    }
    if (length(input$clinical) > 0) df <- df %>% filter(Clinical.Non.Clinical %in% input$clinical)
    if (length(input$research) > 0) {
      df <- df %>% filter(sapply(Research.Area, function(x) {
        any(input$research %in% str_trim(unlist(str_split(x, ","))))
      }))
    }
    if (length(input$psdata) > 0) {
      df <- df %>% filter(Primary.Secondary.Data.used %in% input$psdata)
    }
    if (length(input$datatype) > 0) {
      df <- df %>% filter(sapply(Data.Type, function(x) {
        any(input$datatype %in% str_trim(unlist(str_split(x, ","))))
      }))
    }
    
    # Filter by date if a date is selected
    if (length(input$date) > 0) {
      df <- df %>% filter(Date %in% input$date)
    }
    
    df
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "subject", selected = character(0))
    updateSelectInput(session, "institute", selected = character(0))
    updateSelectInput(session, "clinical", selected = character(0))
    updateSelectInput(session, "research", selected = character(0))
    updateSelectInput(session, "psdata", selected = character(0))
    updateSelectInput(session, "datatype", selected = character(0))
    updateSelectInput(session, "date", selected = character(0))
  })
  
  # Render table (Clinical column now placed after Institute)
  output$table <- renderDT({
    filteredData() %>%
      select(Subject.Type, Institute, Clinical.Non.Clinical, Research.Area, 
             Primary.Secondary.Data.used, Data.Type, Link)
  }, selection = "single", options = list(pageLength = 5),
  colnames = c("Subject Type", "Institute", "Clinical/Non-Clinical", 
               "Research Area", "Primary/Secondary Data Used", 
               "Data Type", "Link"))
  
  # Render PDF viewer
  output$doc_viewer <- renderUI({
    df <- filteredData()
    if (nrow(df) == 0) return(NULL)
    
    selected <- input$table_rows_selected
    if (length(selected) == 0) return(NULL)
    
    file <- df$Link[selected]
    tags$iframe(src = file, width = "100%", height = "600px", style = "border:none;")
  })
}

shinyApp(ui, server)
