# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("quantmod")) install.packages("quantmod")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("plotly")) install.packages("plotly")
if (!require("viridis")) install.packages("viridis")
if (!require("zoo")) install.packages("zoo")
if (!require("DT")) install.packages("DT")
if (!require("shinyjs")) install.packages("shinyjs")

# Load required libraries
library(shiny)
library(quantmod)
library(dplyr)
library(tidyr)
library(plotly)
library(viridis)
library(zoo)
library(DT)
library(shinyjs)  # Load shinyjs for JavaScript capabilities

# Password to access the app
correct_password <- "jac"

# List of available stock symbols
available_stocks <- c("AAPL", "MSFT", "GOOG", "AMZN", "TSLA", "NVDA", "META", "NFLX", "BA", "DIS", "BRK-B")

# Custom CSS for dark mode styling, thick borders around sections, and square frames around each stock
dark_mode_css <- "
  body, .shiny-input-container {
    background-color: #1e1e1e !important;
    color: #00BFFF !important;
  }
  .well {
    background-color: #333333 !important;
    color: #00BFFF !important;
  }
  .btn {
    background-color: #4CAF50 !important;
    color: #ffffff !important;
    border-color: #4CAF50 !important;
  }
  .btn:hover {
    background-color: #66bb6a !important;
    border-color: #66bb6a !important;
  }
  .form-control, .selectize-input, .selectize-control.single .selectize-input, .selectize-control.multi .selectize-input > div {
    background-color: #333333 !important;
    color: #00BFFF !important;
    border-color: #4a4a4a !important;
  }
  .dataTables_wrapper .dataTables_length, 
  .dataTables_wrapper .dataTables_filter,
  .dataTables_wrapper .dataTables_info, 
  .dataTables_wrapper .dataTables_paginate,
  table.dataTable tbody th, table.dataTable tbody td {
    color: #00BFFF !important;
  }
  .stock-frame {
    border: 4px solid #000000;
    border-radius: 5px;
    padding: 15px;
    margin-top: 15px;
    background-color: #333333;
  }
  .portfolio-frame, .plot-frame, .stocks-section-frame, .df-section-frame {
    border: 4px solid #000000;
    border-radius: 5px;
    padding: 15px;
    margin-top: 15px;
    background-color: #333333;
  }
"

# JavaScript to enable Enter key for password input
enter_key_js <- "
  $(document).on('keypress', function(e) {
    if(e.which == 13) {
      $('#login_button').click();
    }
  });
"

# Define UI for the Shiny App
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  tags$head(tags$style(HTML(dark_mode_css))),  # Apply the dark mode CSS
  tags$head(tags$script(HTML(enter_key_js))),  # JavaScript for Enter key
  
  # Centered title and subtitle
  titlePanel(
    div(
      h1("Portfolio Analysis App", style = "text-align: center;"),
      h4("Created by Jose Ahumada Castillo", style = "text-align: center; color: #00BFFF;")
    ),
    windowTitle = "Portfolio Analysis App"
  ),
  
  # Conditional panel for password entry
  uiOutput("login_ui"),
  
  # Main UI content for the app, only visible after successful login
  conditionalPanel(
    condition = "output.loggedIn == true",
    sidebarLayout(
      sidebarPanel(
        
        # Portfolio section with title and frame
        div(
          h3("Portfolio", style = "color: #00BFFF;"),
          textInput("portfolio_name", "Portfolio Name:", value = "My Portfolio"),
          dateInput("end_date", "Portfolio End Date:", value = Sys.Date()),  # Moved below Portfolio Name
          
          # Frame for the portfolio section
          class = "portfolio-frame"
        ),
        
        # Stocks section with title and outer frame
        div(
          h3("Stocks", style = "color: #00BFFF;"),
          
          selectInput("stocks", "Select Stock Symbols:", choices = available_stocks, selected = c("AAPL", "NVDA"), multiple = TRUE),
          
          # Input for individual stock transactions with inner frames around each stock
          uiOutput("transaction_inputs"),
          
          # Add the Save Portfolio button within a frame
          div(
            actionButton("save_portfolio", "Save Portfolio"),
            class = "stock-frame"  # Apply the same frame styling as for each stock
          ),
          
          # Frame for the entire stocks section
          class = "stocks-section-frame"
        ),
        
        # Plot section with title and frame
        div(
          h3("Plot", style = "color: #00BFFF;"),
          selectInput("portfolios_to_plot", "Select Portfolios to Plot:", choices = NULL, multiple = TRUE),
          selectInput("plot_type", "Choose Plot Type:", choices = c("Individual", "Portfolio", "Both")),
          
          # Frame for the plot section
          class = "plot-frame"
        ),
        
        # Place only the "Download and Plot" button below the "Plot" section
        actionButton("go", "Download and Plot"),
        
        # Data Frame section with Show Dataframe checkbox and Download button
        div(
          h3("Data Frame", style = "color: #00BFFF;"),
          checkboxInput("show_dataframe", "Show Dataframe", value = FALSE),
          downloadButton("download_dataframe", "Download Data Frame", style = "margin-top: 10px;", disabled = TRUE),
          class = "df-section-frame"
        )
      ),
      
      mainPanel(
        # Plot output wrapped in a div for framing
        div(plotlyOutput("stock_plot"), class = "plot-frame"),
        br(),
        conditionalPanel(
          condition = "input.show_dataframe == true",  # Show dataframe only if checkbox is checked
          DT::dataTableOutput("dataframe_display")
        )
      )
    )
  )
)

# Define server logic for the Shiny App
server <- function(input, output, session) {
  
  # Reactive value to check if the user is logged in
  logged_in <- reactiveVal(FALSE)
  
  # Reactive value to enable the download button after plot generation
  enable_download <- reactiveVal(FALSE)
  
  # UI for the login screen with caption
  output$login_ui <- renderUI({
    if (!logged_in()) {
      tagList(
        passwordInput("password", "Enter Password:", value = ""),
        actionButton("login_button", "Login"),
        p("Created by Jose Ahumada Castillo", style = "color: #00BFFF; text-align: center;")  # Sign-in page caption
      )
    }
  })
  
  # Check password on login button click
  observeEvent(input$login_button, {
    if (input$password == correct_password) {
      logged_in(TRUE)
    } else {
      showModal(modalDialog(
        title = "Incorrect Password",
        "The password you entered is incorrect. Please try again.",
        easyClose = TRUE
      ))
    }
  })
  
  # Expose the logged-in status to the UI
  output$loggedIn <- reactive({ logged_in() })
  outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)
  
  # Main app code, only accessible after login
  observe({
    req(logged_in())
    
    # Reactive values to store portfolio data and transaction details
    portfolios <- reactiveValues(data = list())
    stock_transactions <- reactiveValues()
    transaction_counter <- reactiveValues()
    
    # Generate dynamic transaction inputs for each stock with inner frames
    output$transaction_inputs <- renderUI({
      req(input$stocks)
      lapply(input$stocks, function(stock) {
        
        # Initialize transaction list and counter for each stock if not already present
        if (is.null(stock_transactions[[stock]])) {
          stock_transactions[[stock]] <- list(1)
          transaction_counter[[stock]] <- 1
        }
        
        # Create UI for each transaction in the stock with an inner frame
        tagList(
          div(
            h4(stock),
            actionButton(paste0("add_transaction_", stock), "Add Transaction"),
            lapply(stock_transactions[[stock]], function(i) {
              tagList(
                selectInput(paste0(stock, "_trans_type_", i), label = paste("Transaction Type", i), choices = c("Buy", "Sell")),
                dateInput(paste0(stock, "_trans_date_", i), label = paste("Transaction Date", i), value = Sys.Date() - 365),
                numericInput(paste0(stock, "_shares_", i), label = paste("Shares", i), value = 1, step = 0.01)  # Default set to 1
              )
            }),
            class = "stock-frame"  # Apply frame styling for each stock
          )
        )
      })
    })
    
    # Observer to handle adding new transactions
    lapply(input$stocks, function(stock) {
      observeEvent(input[[paste0("add_transaction_", stock)]], {
        transaction_counter[[stock]] <- transaction_counter[[stock]] + 1
        stock_transactions[[stock]] <- c(stock_transactions[[stock]], transaction_counter[[stock]])
        
        output$transaction_inputs <- renderUI({
          req(input$stocks)
          lapply(input$stocks, function(stock) {
            div(
              h4(stock),
              actionButton(paste0("add_transaction_", stock), "Add Transaction"),
              lapply(stock_transactions[[stock]], function(i) {
                tagList(
                  selectInput(paste0(stock, "_trans_type_", i), label = paste("Transaction Type", i), choices = c("Buy", "Sell")),
                  dateInput(paste0(stock, "_trans_date_", i), label = paste("Transaction Date", i), value = Sys.Date() - 365),
                  numericInput(paste0(stock, "_shares_", i), label = paste("Shares", i), value = 1, step = 0.01)  # Default set to 1
                )
              }),
              class = "stock-frame"  # Apply frame styling for each stock
            )
          })
        })
      })
    })
    
    # Save all transactions for each stock when 'Save Portfolio' is clicked
    observeEvent(input$save_portfolio, {
      portfolio_name <- input$portfolio_name
      req(input$stocks, portfolio_name)
      
      # Compile all transactions from each stock
      new_transactions <- bind_rows(lapply(input$stocks, function(stock) {
        do.call(rbind, lapply(stock_transactions[[stock]], function(i) {
          transaction_type <- input[[paste0(stock, "_trans_type_", i)]]
          shares <- ifelse(transaction_type == "Sell", -abs(input[[paste0(stock, "_shares_", i)]]), abs(input[[paste0(stock, "_shares_", i)]]))
          data.frame(
            Stock = stock,
            Type = transaction_type,
            Date = as.Date(input[[paste0(stock, "_trans_date_", i)]]),
            Shares = shares,
            stringsAsFactors = FALSE
          )
        }))
      }))
      
      # Save the portfolio with transactions to the portfolios list
      portfolios$data[[portfolio_name]] <- new_transactions
      updateSelectInput(session, "portfolios_to_plot", choices = names(portfolios$data))
    })
    
    # Render the dataframe only if the checkbox is checked
    dataframe_to_display <- reactiveVal(NULL)
    
    observe({
      req(input$show_dataframe, input$portfolios_to_plot)
      
      # Combine data from selected portfolios
      dataframe_to_display(bind_rows(lapply(input$portfolios_to_plot, function(portfolio_name) {
        portfolio_transactions <- portfolios$data[[portfolio_name]]
        
        stock_values <- lapply(unique(portfolio_transactions$Stock), function(stock) {
          # Load stock data
          stock_data <- tryCatch({
            getSymbols(stock, src = "yahoo", from = min(portfolio_transactions$Date), to = input$end_date, auto.assign = FALSE)
          }, error = function(e) {
            message(paste("Failed to load data for:", stock, "due to", e$message))
            return(NULL)
          })
          if (is.null(stock_data)) return(NULL)
          
          stock_prices <- Cl(stock_data)
          stock_data_df <- data.frame(Date = index(stock_prices), Price = as.numeric(stock_prices))
          
          # Calculate cumulative shares on each transaction date and propagate forward
          stock_transactions <- portfolio_transactions %>%
            filter(Stock == stock) %>%
            arrange(Date) %>%
            mutate(CumulativeShares = cumsum(Shares)) %>%
            complete(Date = seq(min(Date), max(stock_data_df$Date), by = "day")) %>%
            fill(CumulativeShares, .direction = "down") %>%
            mutate(CumulativeShares = replace_na(CumulativeShares, 0))
          
          # Merge stock prices with transactions and ensure consistent cumulative shares for all dates
          stock_data_df <- left_join(stock_data_df, stock_transactions, by = "Date") %>%
            arrange(Date) %>%
            mutate(Value = CumulativeShares * Price) %>%
            mutate(Portfolio = portfolio_name, Stock = stock) %>%
            select(Date, Stock, CumulativeShares, Price, Portfolio, Value)
          
          return(stock_data_df)
        }) %>% bind_rows()
        return(stock_values)
      })))
      
      output$dataframe_display <- DT::renderDataTable({
        dataframe_to_display()
      })
    })
    
    # Display the main plot and enable download button only when "Download and Plot" is clicked
    observeEvent(input$go, {
      req(input$portfolios_to_plot)
      
      # Enable download button
      enable_download(TRUE)
      
      # Combine data for all portfolios and calculate portfolio value
      plot_data <- bind_rows(lapply(input$portfolios_to_plot, function(portfolio_name) {
        portfolio_transactions <- portfolios$data[[portfolio_name]]
        
        stock_values <- lapply(unique(portfolio_transactions$Stock), function(stock) {
          
          # Load stock data
          stock_data <- tryCatch({
            getSymbols(stock, src = "yahoo", from = min(portfolio_transactions$Date), to = input$end_date, auto.assign = FALSE)
          }, error = function(e) {
            message(paste("Failed to load data for:", stock, "due to", e$message))
            return(NULL)
          })
          
          if (is.null(stock_data)) return(NULL)
          
          stock_prices <- Cl(stock_data)
          stock_data_df <- data.frame(Date = index(stock_prices), Price = as.numeric(stock_prices))
          
          # Merge stock prices with transactions and fill in the shares held
          stock_transactions <- portfolio_transactions %>%
            filter(Stock == stock) %>%
            arrange(Date) %>%
            mutate(CumulativeShares = cumsum(Shares)) %>%
            complete(Date = seq(min(Date), max(stock_data_df$Date), by = "day")) %>%
            fill(CumulativeShares, .direction = "down") %>%
            mutate(CumulativeShares = replace_na(CumulativeShares, 0))
          
          # Merge transactions with stock prices and calculate daily values for each stock position
          stock_data_df <- left_join(stock_data_df, stock_transactions, by = "Date") %>%
            arrange(Date) %>%
            mutate(Value = CumulativeShares * Price) %>%
            mutate(Portfolio = portfolio_name, Stock = stock) %>%
            select(Date, Stock, Value, Portfolio, CumulativeShares, Price)
          
          return(stock_data_df)
        }) %>% bind_rows()
        
        return(stock_values)
      }))
      
      # Summarize the data to calculate total portfolio value for each date
      portfolio_data <- plot_data %>%
        group_by(Date, Portfolio) %>%
        summarise(Total_Portfolio_Value = sum(Value, na.rm = TRUE), .groups = 'drop')
      
      # Define a color palette with distinct colors
      stock_colors <- RColorBrewer::brewer.pal(n = min(12, length(unique(plot_data$Stock))), "Set1")
      portfolio_colors <- RColorBrewer::brewer.pal(n = min(8, length(unique(portfolio_data$Portfolio))), "Dark2")
      
      # Plot based on the chosen plot type
      if (nrow(portfolio_data) > 0) {
        if (input$plot_type == "Portfolio") {
          fig <- plot_ly(data = portfolio_data, x = ~Date, y = ~Total_Portfolio_Value, color = ~Portfolio, type = 'scatter', mode = 'lines',
                         colors = portfolio_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Date:", Date, "<br>Total Value:", round(Total_Portfolio_Value, 2)),
                         hoverinfo = "text") %>%
            layout(
              paper_bgcolor = "#1e1e1e",
              plot_bgcolor = "#1e1e1e",
              font = list(color = "#00BFFF"),
              xaxis = list(title = "Date"),
              yaxis = list(title = "Portfolio Value (USD)")
            )
          
        } else if (input$plot_type == "Individual") {
          fig <- plot_ly(data = plot_data, x = ~Date, y = ~Value, color = ~Stock, type = 'scatter', mode = 'lines',
                         colors = stock_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Stock:", Stock, "<br>Date:", Date, "<br>Price:", round(Price, 2),
                                       "<br>Shares Held:", round(CumulativeShares, 2), "<br>Position Value:", round(Value, 2)),
                         hoverinfo = "text") %>%
            layout(
              paper_bgcolor = "#1e1e1e",
              plot_bgcolor = "#1e1e1e",
              font = list(color = "#00BFFF"),
              xaxis = list(title = "Date"),
              yaxis = list(title = "Stock Value (USD)")
            )
          
        } else {  # Both
          # Plot individual stocks with specific colors
          fig <- plot_ly(data = plot_data, x = ~Date, y = ~Value, color = ~Stock, type = 'scatter', mode = 'lines',
                         colors = stock_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Stock:", Stock, "<br>Date:", Date,
                                       "<br>Price:", round(Price, 2), "<br>Shares Held:", round(CumulativeShares, 2),
                                       "<br>Position Value:", round(Value, 2)),
                         hoverinfo = "text")
          
          # Add the total portfolio value for each portfolio with specific colors
          fig <- fig %>% add_trace(data = portfolio_data,
                                   x = ~Date, y = ~Total_Portfolio_Value, color = ~Portfolio, type = 'scatter', mode = 'lines',
                                   colors = portfolio_colors,
                                   name = ~Portfolio, text = ~paste("Portfolio:", Portfolio, "<br>Date:", Date,
                                                                    "<br>Total Value:", round(Total_Portfolio_Value, 2)),
                                   hoverinfo = "text") %>%
            layout(
              paper_bgcolor = "#1e1e1e",
              plot_bgcolor = "#1e1e1e",
              font = list(color = "#00BFFF"),
              xaxis = list(title = "Date"),
              yaxis = list(title = "Value (USD)")
            )
        }
        
        output$stock_plot <- renderPlotly({ fig })
      } else {
        output$stock_plot <- renderPlotly({
          plot_ly() %>%
            layout(
              title = "No Data to Display",
              xaxis = list(title = "Date"),
              yaxis = list(title = "Value (USD)")
            )
        })
      }
    })
    
    # Enable download button based on enable_download value
    observe({
      shinyjs::toggleState("download_dataframe", condition = enable_download())
    })
    
    # Dataframe download handler
    output$download_dataframe <- downloadHandler(
      filename = function() {
        paste("dataframe_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(dataframe_to_display(), file, row.names = FALSE)
      }
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

