#Load important packages
library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(ggplot2)

#Define UI for the app
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("claims_data", label = "Upload Your Claims Data (.csv)", accept = ".csv"),
      numericInput("tail_factor", "Tail Factor", value = 1.1, step = 0.1),
      helpText("Note: Please ensure your csv file has the column names corresponding to", 
               "loss year, development year, and amounts of claims paid.", 
               "Enter the column names below according to your uploaded file."),
      splitLayout(
        textInput("loss_year", label = "Loss Year", value = "loss_year", width = "100%"),
        textInput("dvlp_year", label = "Development Year", value = "dvlp_year"),
        textInput("claims", label = "Claims Paid", value = "claims")
      ),
      checkboxInput("data_table", label = "View Your Data Table", value = TRUE)
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.data_table == true",
        dataTableOutput(outputId = "table")
      ),
      plotOutput(outputId = "Plot", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  cal_data <- reactive({
    if(is.null(input$claims_data))
      return(NULL)
    
    # Assign column names
    loss_year_col <- input$loss_year
    dvlp_year_col <- input$dvlp_year
    claims_col <- input$claims
    
    df <- read.csv(input$claims_data$datapath)
    
    loss_years <- sort(unique(df[[loss_year_col]]))
    dvlp_years <- sort(unique(df[[dvlp_year_col]]))
    N <- length(dvlp_years)
    
    df_cumulative <- data.frame(matrix(NA, nrow=length(loss_years), ncol=N+1), row.names=loss_years)
    colnames(df_cumulative) <- c(as.character(dvlp_years), "Tail")
    
    # Fill in the cumulative claims data
    for (j in 1:N) {
      for (i in 1:(length(loss_years) - j + 1)) {
        df_cumulative[i, j] <- sum(df[[claims_col]][df[[loss_year_col]] == loss_years[i] & df[[dvlp_year_col]] <= dvlp_years[j]])
      }
      
      if (N - j + 2 <= length(loss_years)) {
        for (i in (N - j + 2):length(loss_years)) {
          df_cumulative[i, j] <- sum(df_cumulative[1:(length(loss_years) - j + 1), j], na.rm = TRUE) / 
            sum(df_cumulative[1:(length(loss_years) - j + 1), j - 1], na.rm = TRUE) * df_cumulative[i, j - 1]
        }
      }
    }
    
    return(df_cumulative)
  })
  
  final_data <- reactive({
    TailFactor <- input$tail_factor
    df_cumulative <- cal_data()
    N <- ncol(df_cumulative) - 1
    
    if (is.null(df_cumulative))
      return(NULL)
    
    df_cumulative[, N + 1] <- df_cumulative[, N] * TailFactor
    
    return(round(df_cumulative))
  })
  
  # Render the data table
  output$table <- renderDataTable({
    final_data <- final_data()
    if(is.null(final_data))
      return(NULL)
    
    if(input$data_table) {
      DT::datatable(data=final_data, options=list(pageLength=10),
                    caption="Rows represent loss year; Columns represent development year")
    }
  })
  
  # Render the plot
  output$Plot <- renderPlot({
    final_data <- final_data()
    if(is.null(final_data))
      return(NULL)
    
    # Prepare the data for plotting
    plot_data <- final_data %>%
      rownames_to_column(var="loss_year") %>%
      pivot_longer(cols = -loss_year, names_to = "development_year", values_to = "cumulative_claims")
    plot_data$development_year <- as.numeric(plot_data$development_year)
    
    #Plot the data
    ggplot(plot_data, mapping=aes(x = development_year, y = cumulative_claims, color = loss_year)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Cumulative Paid Claims ($)",
           x = "Development Year",
           y = "Cumulative Claims ($)",
           color = "Loss Year") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)