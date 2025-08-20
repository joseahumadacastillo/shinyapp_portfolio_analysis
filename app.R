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
if (!require("pagedown")) install.packages("pagedown")
if (!require("htmltools")) install.packages("htmltools")
# Added (already used earlier): PDF ggplot + histogram
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("scales")) install.packages("scales")
if (!require("base64enc")) install.packages("base64enc")

# Load required libraries
library(shiny)
library(quantmod)
library(dplyr)
library(tidyr)
library(plotly)
library(viridis)
library(zoo)
library(DT)
library(shinyjs)   # Load shinyjs for JavaScript capabilities
library(pagedown)  # For HTML -> PDF
library(htmltools) # For building/saving HTML
# Added for PDF ggplot + histogram
library(ggplot2)
library(ggthemes)
library(scales)
library(base64enc)

# Password to access the app
correct_password <- "public"
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

  /* Password input dark style (incl. autofill) */
  input[type='password'] {
    background-color: #333333 !important;
    color: #ffffff !important;
    border: 1px solid #4a4a4a !important;
  }
  input[type='password']::placeholder {
    color: #9ecff5 !important;
    opacity: 0.7;
  }
  input[type='password']:-webkit-autofill,
  input[type='password']:-webkit-autofill:hover,
  input[type='password']:-webkit-autofill:focus,
  input[type='password']:-webkit-autofill:active {
    -webkit-text-fill-color: #ffffff !important;
    transition: background-color 5000s ease-in-out 0s;
    box-shadow: 0 0 0px 1000px #333333 inset !important;
    caret-color: #ffffff !important;
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
      h4("Created by José Ahumada Castillo", style = "text-align: center; color: #00BFFF;")
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
          textInput("portfolio_name", "Portfolio Name: (type the name of your portfolio or leave the default)", value = "My Portfolio"),
          fluidRow(
            column(6,
                   dateInput("start_date", "Portfolio Start Date: (click in the default date and select in the calendar the start date)", value = Sys.Date() - 365)
            ),
            column(6,
                   dateInput("end_date", "Portfolio End Date: (click in the default date and select in the calendar the end date)", value = Sys.Date())
            )
          ),
          class = "portfolio-frame"
        ),
        
        # Stocks section with title and outer frame
        div(
          h3("Stocks", style = "color: #00BFFF;"),
          
          selectInput("stocks", "Select Stock Symbols:( Click in the space and select within the given tickers, then select the start date and number of shares (can be fractional)) After that save the portfolio", choices = available_stocks, selected = NULL, multiple = TRUE),
          
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
          selectInput("plot_type", "Choose Plot Type:(Individual stocks, portfolio or both)", choices = c("Individual", "Portfolio", "Both")),
          
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
        # Daily returns histogram (Plotly) + RED density line
        div(plotlyOutput("returns_hist_plot"), class = "plot-frame"),
        br(),
        # Report below the graph with PDF button
        div(
          div(style = "text-align:right; margin-bottom:8px;",
              downloadButton("download_report_pdf", "Descargar reporte (PDF)")
          ),
          uiOutput("portfolio_report"),
          class = "portfolio-frame"
        ),
        br(),
        # Dataframe
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
  
  # For report: store last computed data
  rv <- reactiveValues(plot_data = NULL, portfolio_data = NULL, rets_data = NULL)
  
  # UI for the login screen with caption
  output$login_ui <- renderUI({
    if (!logged_in()) {
      tagList(
        passwordInput("password", "Enter Password: ( Public Password = public )", value = ""),
        actionButton("login_button", "Login"),
        p("Created by José Ahumada Castillo", style = "color: #00BFFF; text-align: center;")  # Sign-in page caption
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
    
    # Generate dynamic transaction inputs for each stock with inner frames
    output$transaction_inputs <- renderUI({
      req(input$stocks)
      lapply(input$stocks, function(stock) {
        
        # Initialize transaction list for each stock if not already present
        if (is.null(stock_transactions[[stock]])) {
          stock_transactions[[stock]] <- list(1)
        }
        
        # Per-stock inputs (shares only; dates are global)
        tagList(
          div(
            h4(stock),
            lapply(stock_transactions[[stock]], function(i) {
              tagList(
                numericInput(paste0(stock, "_shares_", i), label = paste("Shares (select the number of shares(can be fractional))", i), value = 1, min = 0, step = 0.01)
              )
            }),
            class = "stock-frame"
          )
        )
      })
    })
    
    # Save all transactions for each stock when 'Save Portfolio' is clicked
    observeEvent(input$save_portfolio, {
      portfolio_name <- input$portfolio_name
      req(input$stocks, portfolio_name)
      
      # Compile all transactions from each stock
      new_transactions <- bind_rows(lapply(input$stocks, function(stock) {
        do.call(rbind, lapply(stock_transactions[[stock]], function(i) {
          shares <- abs(input[[paste0(stock, "_shares_", i)]])  # always positive (default Buy)
          data.frame(
            Stock = stock,
            Type = "Buy",
            Date = as.Date(input$start_date),
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
            getSymbols(stock, src = "yahoo", from = input$start_date, to = input$end_date, auto.assign = FALSE)
          }, error = function(e) {
            message(paste("Failed to load data for:", stock, "due to", e$message))
            return(NULL)
          })
          if (is.null(stock_data)) return(NULL)
          
          stock_prices <- Cl(stock_data)
          stock_data_df <- data.frame(Date = index(stock_prices), Price = as.numeric(stock_prices))
          
          # Calculate cumulative shares on each transaction date and propagate forward
          stock_transactions_df <- portfolio_transactions %>%
            filter(Stock == stock) %>%
            arrange(Date) %>%
            mutate(CumulativeShares = cumsum(Shares)) %>%
            complete(Date = seq(as.Date(input$start_date), max(stock_data_df$Date), by = "day")) %>%
            fill(CumulativeShares, .direction = "down") %>%
            mutate(CumulativeShares = replace_na(CumulativeShares, 0))
          
          # Merge stock prices with transactions and ensure consistent cumulative shares for all dates
          stock_data_df <- left_join(stock_data_df, stock_transactions_df, by = "Date") %>%
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
            getSymbols(stock, src = "yahoo", from = input$start_date, to = input$end_date, auto.assign = FALSE)
          }, error = function(e) {
            message(paste("Failed to load data for:", stock, "due to", e$message))
            return(NULL)
          })
          
          if (is.null(stock_data)) return(NULL)
          
          stock_prices <- Cl(stock_data)
          stock_data_df <- data.frame(Date = index(stock_prices), Price = as.numeric(stock_prices))
          
          # Merge stock prices with transactions and fill in the shares held
          stock_transactions_df <- portfolio_transactions %>%
            filter(Stock == stock) %>%
            arrange(Date) %>%
            mutate(CumulativeShares = cumsum(Shares)) %>%
            complete(Date = seq(as.Date(input$start_date), max(stock_data_df$Date), by = "day")) %>%
            fill(CumulativeShares, .direction = "down") %>%
            mutate(CumulativeShares = replace_na(CumulativeShares, 0))
          
          # Merge transactions with stock prices and calculate daily values for each stock position
          stock_data_df <- left_join(stock_data_df, stock_transactions_df, by = "Date") %>%
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
      
      # Store for the report
      rv$plot_data <- plot_data
      rv$portfolio_data <- portfolio_data
      
      # ===== compute daily returns per portfolio for histogram =====
      rets_data <- portfolio_data %>%
        group_by(Portfolio) %>%
        arrange(Date, .by_group = TRUE) %>%
        mutate(Return = Total_Portfolio_Value / dplyr::lag(Total_Portfolio_Value) - 1) %>%
        filter(!is.na(Return)) %>%
        ungroup()
      rv$rets_data <- rets_data
      # ============================================================
      
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
          fig <- plot_ly(data = plot_data, x = ~Date, y = ~Value, color = ~Stock, type = 'scatter', mode = 'lines',
                         colors = stock_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Stock:", Stock, "<br>Date:", Date,
                                       "<br>Price:", round(Price, 2), "<br>Shares Held:", round(CumulativeShares, 2),
                                       "<br>Position Value:", round(Value, 2)),
                         hoverinfo = "text")
          
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
      
      # ===== Plotly histogram of daily returns with RED density line =====
      output$returns_hist_plot <- renderPlotly({
        req(rv$rets_data)
        # Use density scale so we can overlay density line without scaling issues
        p <- plot_ly(rv$rets_data, x = ~Return, color = ~Portfolio, type = "histogram",
                     nbinsx = 50, histnorm = "probability density") %>%
          layout(
            bargap = 0.05,
            paper_bgcolor = "#1e1e1e",
            plot_bgcolor = "#1e1e1e",
            font = list(color = "#00BFFF"),
            xaxis = list(title = "Daily Return", tickformat = ".2%"),
            yaxis = list(title = "Density"),
            legend = list(orientation = "h")
          )
        # Add pooled density line (red)
        dens <- density(rv$rets_data$Return, na.rm = TRUE)
        p <- add_lines(p, x = dens$x, y = dens$y, name = "Density",
                       line = list(color = "red", width = 2), inherit = FALSE, hoverinfo = "skip")
        p
      })
      # =========================================================
    })
    
    # ---- Portfolio Report renderer (with title, subtitle, date on top-right) ----
    output$portfolio_report <- renderUI({
      req(rv$portfolio_data, rv$plot_data, input$portfolios_to_plot)
      
      fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(sprintf("%.2f", 100*x), "%"))
      fmt_usd <- function(x) ifelse(is.na(x), "—", paste0("$", formatC(x, big.mark = ",", format = "f", digits = 2)))
      fmt_date <- function(x) ifelse(is.na(x), "—", format(as.Date(x), "%Y-%m-%d"))
      
      header <- tags$div(
        style = "display:flex; justify-content:space-between; align-items:flex-start; margin-bottom:8px;",
        tags$div(
          tags$h2("Reporte de Portafolio", style = "margin:0; color:#00BFFF;"),
          tags$h4("app https://joseahumada.shinyapps.io/portfolio/ creada por José Ahumada Castillo",
                  style = "margin:0; color:#9ecff5; font-weight:normal;")
        ),
        tags$div(
          style = "text-align:right; color:#00BFFF; font-weight:bold;",
          format(Sys.Date(), "%Y-%m-%d")
        )
      )
      
      panels <- lapply(input$portfolios_to_plot, function(pn) {
        pd <- rv$portfolio_data %>% filter(Portfolio == pn) %>% arrange(Date)
        req(nrow(pd) > 1)
        
        pd <- pd %>% mutate(ret = (Total_Portfolio_Value / dplyr::lag(Total_Portfolio_Value)) - 1)
        rets <- pd$ret[!is.na(pd$ret)]
        
        first_val <- dplyr::first(pd$Total_Portfolio_Value)
        last_val  <- dplyr::last(pd$Total_Portfolio_Value)
        max_val   <- max(pd$Total_Portfolio_Value, na.rm = TRUE)
        min_val   <- min(pd$Total_Portfolio_Value, na.rm = TRUE)
        
        period_ret <- ifelse(first_val > 0, last_val/first_val - 1, NA_real_)
        var_daily  <- ifelse(length(rets) > 1, var(rets, na.rm = TRUE), NA_real_)
        var_ann    <- ifelse(is.na(var_daily), NA_real_, var_daily * 252)
        ann_ret    <- ifelse(length(rets) > 0, (1 + period_ret)^(252/length(rets)) - 1, NA_real_)
        
        q05 <- ifelse(length(rets) > 0, as.numeric(quantile(rets, probs = 0.05, na.rm = TRUE)), NA_real_)
        var95_pct <- ifelse(is.na(q05), NA_real_, -q05)
        var95_abs <- ifelse(is.na(var95_pct), NA_real_, var95_pct * last_val)
        # NEW: Annualized VaR (sqrt(252) scaling)
        var95_pct_ann <- ifelse(is.na(var95_pct), NA_real_, var95_pct * sqrt(252))
        var95_abs_ann <- ifelse(is.na(var95_pct_ann), NA_real_, var95_pct_ann * last_val)
        
        stocks_in <- rv$plot_data %>%
          filter(Portfolio == pn) %>%
          distinct(Stock) %>%
          arrange(Stock) %>%
          pull(Stock)
        
        start_d <- input$start_date
        end_d   <- input$end_date
        
        tags$div(
          style = "margin-top:6px;",
          tags$h3(paste("Portafolio:", pn), style = "color:#00BFFF;"),
          tags$p(paste("Fechas:", fmt_date(start_d), "→", fmt_date(end_d))),
          tags$p(HTML(paste0("Stocks contenidos: <b>", paste(stocks_in, collapse = ", "), "</b>"))),
          tags$table(
            style = "width:100%; color:#00BFFF;",
            tags$tbody(
              tags$tr(tags$td("Rendimiento del periodo"), tags$td(style="text-align:right;", fmt_pct(period_ret))),
              tags$tr(tags$td("Varianza (diaria) del periodo"), tags$td(style="text-align:right;", ifelse(is.na(var_daily),"—",sprintf("%.6f", var_daily)))),
              tags$tr(tags$td("Rendimiento anualizado"), tags$td(style="text-align:right;", fmt_pct(ann_ret))),
              tags$tr(tags$td("Varianza anualizada"), tags$td(style="text-align:right;", ifelse(is.na(var_ann),"—",sprintf("%.6f", var_ann)))),
              tags$tr(tags$td("VaR 95% (diario, % de pérdida)"), tags$td(style="text-align:right;", fmt_pct(var95_pct))),
              tags$tr(tags$td("VaR 95% (diario, monto)"), tags$td(style="text-align:right;", fmt_usd(var95_abs))),
              # NEW rows:
              tags$tr(tags$td("VaR 95% anualizado (%, pérdida)"), tags$td(style="text-align:right;", fmt_pct(var95_pct_ann))),
              tags$tr(tags$td("VaR 95% anualizado (monto)"), tags$td(style="text-align:right;", fmt_usd(var95_abs_ann))),
              tags$tr(tags$td("Valor inicial"), tags$td(style="text-align:right;", fmt_usd(first_val))),
              tags$tr(tags$td("Valor máximo"), tags$td(style="text-align:right;", fmt_usd(max_val))),
              tags$tr(tags$td("Valor mínimo"), tags$td(style="text-align:right;", fmt_usd(min_val))),
              tags$tr(tags$td("Valor final"),  tags$td(style="text-align:right;", fmt_usd(last_val)))
            )
          )
        )
      })
      
      do.call(tagList, c(list(header), panels))
    })
    # ---- end report ----
    
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
    
    # ---- Download handler for report PDF (line plot BLUE, histogram with RED density; TEXT FIRST, THEN PLOTS) ----
    output$download_report_pdf <- downloadHandler(
      filename = function() {
        paste0("reporte_portafolio_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$portfolio_data, input$portfolios_to_plot)
        
        fmt_pct <- function(x) ifelse(is.na(x), "—", paste0(sprintf("%.2f", 100*x), "%"))
        fmt_usd <- function(x) ifelse(is.na(x), "—", paste0("$", formatC(x, big.mark = ",", format = "f", digits = 2)))
        fmt_date <- function(x) ifelse(is.na(x), "—", format(as.Date(x), "%Y-%m-%d"))
        
        # Header (same as UI)
        header <- tags$div(
          style = "display:flex; justify-content:space-between; align-items:flex-start; margin-bottom:8px;",
          tags$div(
            tags$h2("Reporte de Portafolio", style = "margin:0; color:#00BFFF;"),
            tags$h4("app https://joseahumada.shinyapps.io/portfolio/ creada por José Ahumada Castillo",
                    style = "margin:0; color:#9ecff5; font-weight:normal;")
          ),
          tags$div(
            style = "text-align:right; color:#00BFFF; font-weight:bold;",
            format(Sys.Date(), "%Y-%m-%d")
          )
        )
        
        # ---- ggplot line chart (Economist) in BLUE ----
        pd_all <- rv$portfolio_data %>%
          filter(Portfolio %in% input$portfolios_to_plot) %>%
          arrange(Date)
        gg_tag <- NULL
        if (nrow(pd_all) > 0) {
          g <- ggplot(pd_all, aes(x = Date, y = Total_Portfolio_Value)) +
            geom_line(linewidth = 1, color = "blue") +  # BLUE line
            scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
            labs(title = "Evolución del Valor del Portafolio",
                 x = "Fecha", y = "Valor del Portafolio (USD)") +
            ggthemes::theme_economist() +
            theme(plot.title = element_text(hjust = 0.5))
          tmp_png <- tempfile(fileext = ".png")
          ggsave(filename = tmp_png, plot = g, width = 10, height = 5, dpi = 150, bg = "white")
          img_b64 <- base64enc::base64encode(tmp_png)
          gg_tag <- tags$img(
            src = paste0("data:image/png;base64,", img_b64),
            style = "width:100%; max-width:100%; height:auto; margin: 8px 0 16px 0; border: 1px solid #333333; border-radius: 4px;"
          )
        }
        
        # ---- ggplot histogram of daily returns (Economist) + RED density line ----
        hist_tag <- NULL
        if (!is.null(rv$rets_data) && nrow(rv$rets_data) > 0) {
          g_hist <- ggplot(rv$rets_data, aes(x = Return, fill = Portfolio)) +
            geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.8,
                           position = "identity", color = "white", linewidth = 0.15) +
            geom_density(aes(x = Return), color = "red", linewidth = 1, inherit.aes = FALSE) +  # RED density
            scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
            labs(title = "Histograma de Rendimientos Diarios del Portafolio",
                 x = "Rendimiento Diario", y = "Densidad", fill = "Portafolio") +
            ggthemes::theme_economist() +
            ggthemes::scale_fill_economist() +
            theme(plot.title = element_text(hjust = 0.5))
          tmp_png_h <- tempfile(fileext = ".png")
          ggsave(filename = tmp_png_h, plot = g_hist, width = 10, height = 5, dpi = 150, bg = "white")
          img_b64_h <- base64enc::base64encode(tmp_png_h)
          hist_tag <- tags$img(
            src = paste0("data:image/png;base64,", img_b64_h),
            style = "width:100%; max-width:100%; height:auto; margin: 8px 0 16px 0; border: 1px solid #333333; border-radius: 4px;"
          )
        }
        
        # Panels per portfolio (metrics incl. new items)
        panels <- lapply(input$portfolios_to_plot, function(pn) {
          pd <- rv$portfolio_data %>% filter(Portfolio == pn) %>%
            arrange(Date)
          validate(need(nrow(pd) > 1, "No hay datos suficientes para el reporte."))
          
          pd <- pd %>% mutate(ret = (Total_Portfolio_Value / dplyr::lag(Total_Portfolio_Value)) - 1)
          rets <- pd$ret[!is.na(pd$ret)]
          
          first_val <- dplyr::first(pd$Total_Portfolio_Value)
          last_val  <- dplyr::last(pd$Total_Portfolio_Value)
          max_val   <- max(pd$Total_Portfolio_Value, na.rm = TRUE)
          min_val   <- min(pd$Total_Portfolio_Value, na.rm = TRUE)
          
          period_ret <- ifelse(first_val > 0, last_val/first_val - 1, NA_real_)
          var_daily  <- ifelse(length(rets) > 1, var(rets, na.rm = TRUE), NA_real_)
          var_ann    <- ifelse(is.na(var_daily), NA_real_, var_daily * 252)
          ann_ret    <- ifelse(length(rets) > 0, (1 + period_ret)^(252/length(rets)) - 1, NA_real_)
          
          q05 <- ifelse(length(rets) > 0, as.numeric(quantile(rets, probs = 0.05, na.rm = TRUE)), NA_real_)
          var95_pct <- ifelse(is.na(q05), NA_real_, -q05)
          var95_abs <- ifelse(is.na(var95_pct), NA_real_, var95_pct * last_val)
          # NEW: Annualized VaR (sqrt(252) scaling)
          var95_pct_ann <- ifelse(is.na(var95_pct), NA_real_, var95_pct * sqrt(252))
          var95_abs_ann <- ifelse(is.na(var95_pct_ann), NA_real_, var95_pct_ann * last_val)
          
          stocks_in <- rv$plot_data %>%
            filter(Portfolio == pn) %>%
            distinct(Stock) %>%
            arrange(Stock) %>%
            pull(Stock)
          
          start_d <- input$start_date
          end_d   <- input$end_date
          
          tags$div(
            style = "margin-top:6px;",
            tags$h3(paste("Portafolio:", pn), style = "color:#00BFFF;"),
            tags$p(paste("Fechas:", fmt_date(start_d), "→", fmt_date(end_d))),
            tags$p(HTML(paste0("Stocks contenidos: <b>", paste(stocks_in, collapse = ", "), "</b>"))),
            tags$table(
              style = "width:100%; color:#00BFFF;",
              tags$tbody(
                tags$tr(tags$td("Rendimiento del periodo"), tags$td(style="text-align:right;", fmt_pct(period_ret))),
                tags$tr(tags$td("Varianza (diaria) del periodo"), tags$td(style="text-align:right;", ifelse(is.na(var_daily),"—",sprintf("%.6f", var_daily)))),
                tags$tr(tags$td("Rendimiento anualizado"), tags$td(style="text-align:right;", fmt_pct(ann_ret))),
                tags$tr(tags$td("Varianza anualizada"), tags$td(style="text-align:right;", ifelse(is.na(var_ann),"—",sprintf("%.6f", var_ann)))),
                tags$tr(tags$td("VaR 95% (diario, % de pérdida)"), tags$td(style="text-align:right;", fmt_pct(var95_pct))),
                tags$tr(tags$td("VaR 95% (diario, monto)"), tags$td(style="text-align:right;", fmt_usd(var95_abs))),
                # NEW rows:
                tags$tr(tags$td("VaR 95% anualizado (%, pérdida)"), tags$td(style="text-align:right;", fmt_pct(var95_pct_ann))),
                tags$tr(tags$td("VaR 95% anualizado (monto)"), tags$td(style="text-align:right;", fmt_usd(var95_abs_ann))),
                tags$tr(tags$td("Valor inicial"), tags$td(style="text-align:right;", fmt_usd(first_val))),
                tags$tr(tags$td("Valor máximo"), tags$td(style="text-align:right;", fmt_usd(max_val))),
                tags$tr(tags$td("Valor mínimo"), tags$td(style="text-align:right;", fmt_usd(min_val))),
                tags$tr(tags$td("Valor final"),  tags$td(style="text-align:right;", fmt_usd(last_val)))
              )
            )
          )
        })
        
        # Assemble full HTML (TEXT FIRST, THEN PLOTS)
        html_doc <- tags$html(
          tags$head(
            tags$meta(charset = "UTF-8"),
            tags$title("Reporte de Portafolio"),
            tags$style(HTML("
              body { background: #1e1e1e; color:#00BFFF; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, 'Noto Sans', 'Liberation Sans', sans-serif; padding: 16px; }
              h2 { color:#00BFFF; margin:0; }
              h3 { color:#00BFFF; }
              h4 { color:#9ecff5; margin:0; }
              table { width:100%; border-collapse: collapse; }
              td { padding: 4px 0; }
            "))
          ),
          tags$body(
            header,
            do.call(tagList, panels),     # TEXT / TABLES FIRST
            if (!is.null(gg_tag)) gg_tag, # BLUE LINE PLOT
            if (!is.null(hist_tag)) hist_tag  # HISTOGRAM WITH RED DENSITY
          )
        )
        
        tmp_html <- tempfile(fileext = ".html")
        htmltools::save_html(html_doc, tmp_html)
        pagedown::chrome_print(input = tmp_html, output = file)
      }
    )
    # ---- end PDF download ----
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
