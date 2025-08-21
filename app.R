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
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("scales")) install.packages("scales")
if (!require("base64enc")) install.packages("base64enc")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
# Fallback PDF engines
if (!require("webshot2")) install.packages("webshot2")
if (!require("webshot"))  install.packages("webshot")

# Load required libraries
library(shiny)
library(quantmod)
library(dplyr)
library(tidyr)
library(plotly)
library(viridis)
library(zoo)
library(DT)
library(shinyjs)
library(pagedown)
library(htmltools)
library(ggplot2)
library(ggthemes)
library(scales)
library(base64enc)
library(RColorBrewer)
# (webshot2/webshot loaded by namespace in code paths)

# Password to access the app
correct_password <- "public"

# List of available stock symbols
available_stocks <- c("AAPL", "MSFT", "GOOG", "AMZN", "TSLA", "NVDA", "META", "NFLX", "BA", "DIS", "BRK-B")

# Custom CSS (dark mode and frames)
dark_mode_css <- "
  body, .shiny-input-container { background-color: #1e1e1e !important; color: #00BFFF !important; }
  .well { background-color: #333333 !important; color: #00BFFF !important; }
  .btn { background-color: #4CAF50 !important; color: #ffffff !important; border-color: #4CAF50 !important; }
  .btn:hover { background-color: #66bb6a !important; border-color: #66bb6a !important; }
  .form-control, .selectize-input, .selectize-control.single .selectize-input, .selectize-control.multi .selectize-input > div {
    background-color: #333333 !important; color: #00BFFF !important; border-color: #4a4a4a !important;
  }
  input[type='password'] { background-color: #333333 !important; color: #ffffff !important; border: 1px solid #4a4a4a !important; }
  input[type='password']::placeholder { color: #9ecff5 !important; opacity: 0.7; }
  input[type='password']:-webkit-autofill,
  input[type='password']:-webkit-autofill:hover,
  input[type='password']:-webkit-autofill:focus,
  input[type='password']:-webkit-autofill:active {
    -webkit-text-fill-color: #ffffff !important; transition: background-color 5000s ease-in-out 0s;
    box-shadow: 0 0 0px 1000px #333333 inset !important; caret-color: #ffffff !important;
  }
  .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate,
  table.dataTable tbody th, table.dataTable tbody td { color: #00BFFF !important; }
  .stock-frame, .portfolio-frame, .plot-frame, .stocks-section-frame, .df-section-frame {
    border: 4px solid #000000; border-radius: 5px; padding: 15px; margin-top: 15px; background-color: #333333;
  }
  /* Save-tip layout & color: inherits the same color as labels */
  .save-tip { display:flex; gap:10px; align-items:center; color: inherit; }
  .save-tip p { margin:0; font-size: 12px; line-height: 1.2; }
"

# JavaScript to enable Enter key for password input
enter_key_js <- "
  $(document).on('keypress', function(e) { if(e.which == 13) { $('#login_button').click(); } });
"

# =========================
#           UI
# =========================
ui <- fluidPage(
  useShinyjs(),
  # Load EB Garamond for in-app report rendering
  tags$head(tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=EB+Garamond:wght@400;700&display=swap")),
  tags$head(tags$style(HTML(dark_mode_css))),
  tags$head(tags$script(HTML(enter_key_js))),
  titlePanel(
    div(
      h1("Portfolio Analysis App", style = "text-align: center;"),
      h4("Created by José Ahumada Castillo", style = "text-align: center; color: #00BFFF;")
    ),
    windowTitle = "Portfolio Analysis App"
  ),
  uiOutput("login_ui"),
  conditionalPanel(
    condition = "output.loggedIn == true",
    sidebarLayout(
      sidebarPanel(
        # Portfolio block
        div(
          h3("Portfolio", style = "color: #00BFFF;"),
          textInput("portfolio_name", "Portfolio Name: (type the name of your portfolio or leave the default)", value = "My Portfolio"),
          fluidRow(
            column(6, dateInput("start_date", "Portfolio Start Date", value = Sys.Date() - 365)),
            column(6, dateInput("end_date", "Portfolio End Date", value = Sys.Date()))
          ),
          class = "portfolio-frame"
        ),
        
        # Stocks section
        div(
          h3("Stocks", style = "color: #00BFFF;"),
          # Label and help share same color via 'inherit'
          tags$label(
            style = "display:block; margin-bottom:6px; color: inherit;",
            "Select Stock Symbols (choose from list; fractions allowed in 'Shares'). Save portfolio after setting shares."
          ),
          selectInput(
            "stocks",
            NULL,  # label handled above to control color
            choices = available_stocks, selected = NULL, multiple = TRUE
          ),
          uiOutput("transaction_inputs"),
          
          # Save button + Tip (same color as label)
          div(
            class = "save-tip",
            actionButton("save_portfolio", "Save Portfolio"),
            p("Tip: If you’re analyzing only one portfolio, save and continue to the next section. 
               If you’re analyzing more than one, save and use Manage tickers to clear the required tickers, 
               then start the process for each new portfolio, beginning by creating its name. 
               Repeat until you’ve finished all additional portfolios (2 through n), then continue to the next section.")
          ),
          
          # Manage tickers
          div(
            h4("Manage tickers"),
            uiOutput("remove_ticker_ui"),
            actionButton("remove_ticker_btn", "Remove selected ticker"),
            actionButton("clear_tickers", "Clear all tickers"),
            style = "margin-top:10px;"
          ),
          
          class = "stocks-section-frame"
        ),
        
        # Plot options
        div(
          h3("Plot", style = "color: #00BFFF;"),
          checkboxGroupInput("portfolios_to_plot", "Available portfolios (check to plot):", choices = NULL, selected = NULL, inline = FALSE),
          selectInput("plot_type", "Choose Plot Type", choices = c("Individual", "Portfolio", "Both")),
          class = "plot-frame"
        ),
        
        actionButton("go", "Download and Plot"),
        
        # Data frame area
        div(
          h3("Data Frame", style = "color: #00BFFF;"),
          checkboxInput("show_dataframe", "Show Dataframe", value = FALSE),
          downloadButton("download_dataframe", "Download Data Frame", style = "margin-top: 10px;", disabled = TRUE),
          class = "df-section-frame"
        )
      ),
      
      mainPanel(
        # Main line plot
        div(plotlyOutput("stock_plot"), class = "plot-frame"),
        br(),
        # One histogram per portfolio (dynamic)
        div(uiOutput("returns_hist_container"), class = "plot-frame"),
        br(),
        # Report block with Garamond 12pt in-app
        div(
          div(style = "text-align:right; margin-bottom:8px; display:flex; gap:8px; justify-content:flex-end;",
              downloadButton("download_report_html", "Download report (HTML)"),
              downloadButton("download_report_pdf",  "Download report (PDF)")
          ),
          div(
            style = "font-family: 'EB Garamond','GaramondPremrPro','Adobe Garamond Pro','EB Garamond', Garamond, serif; font-size: 12pt;",
            uiOutput("portfolio_report")
          ),
          class = "portfolio-frame"
        ),
        br(),
        # Dataframe
        conditionalPanel(
          condition = "input.show_dataframe == true",
          DT::dataTableOutput("dataframe_display")
        )
      )
    )
  )
)

# =========================
#         SERVER
# =========================
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  enable_download <- reactiveVal(FALSE)
  
  rv <- reactiveValues(plot_data = NULL, portfolio_data = NULL, rets_data = NULL)
  
  # ---- Login UI ----
  output$login_ui <- renderUI({
    if (!logged_in()) {
      tagList(
        passwordInput("password", "Enter Password: ( Public Password = public )", value = ""),
        actionButton("login_button", "Login"),
        p("Created by José Ahumada Castillo", style = "color: #00BFFF; text-align: center;")
      )
    }
  })
  observeEvent(input$login_button, {
    if (input$password == correct_password) {
      logged_in(TRUE)
    } else {
      showModal(modalDialog(title = "Incorrect Password", "The password you entered is incorrect. Please try again.", easyClose = TRUE))
    }
  })
  output$loggedIn <- reactive({ logged_in() })
  outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)
  
  # ---- App logic after login ----
  observe({
    req(logged_in())
    
    portfolios <- reactiveValues(data = list())
    stock_transactions <- reactiveValues()
    
    # Manage tickers (remove one / clear all)
    output$remove_ticker_ui <- renderUI({
      choices_vec <- if (is.null(input$stocks)) character(0) else input$stocks
      selectInput("remove_ticker", "Ticker to remove:", choices = choices_vec, selected = NULL)
    })
    observeEvent(input$remove_ticker_btn, {
      req(input$remove_ticker)
      new_sel <- setdiff(input$stocks, input$remove_ticker)
      updateSelectInput(session, "stocks", selected = new_sel)
      stock_transactions[[input$remove_ticker]] <- NULL
    })
    observeEvent(input$clear_tickers, {
      updateSelectInput(session, "stocks", selected = character(0))
      for (nm in names(stock_transactions)) stock_transactions[[nm]] <- NULL
    })
    
    # Dynamic per-stock share inputs
    output$transaction_inputs <- renderUI({
      req(input$stocks)
      lapply(input$stocks, function(stock) {
        if (is.null(stock_transactions[[stock]])) stock_transactions[[stock]] <- list(1)
        div(
          h4(stock),
          lapply(stock_transactions[[stock]], function(i) {
            numericInput(paste0(stock, "_shares_", i), label = paste("Shares", i), value = 1, min = 0, step = 0.01)
          }),
          class = "stock-frame"
        )
      })
    })
    
    # Save portfolio
    observeEvent(input$save_portfolio, {
      portfolio_name <- input$portfolio_name
      req(portfolio_name, nzchar(portfolio_name), input$stocks)
      
      new_transactions <- bind_rows(lapply(input$stocks, function(stock) {
        do.call(rbind, lapply(stock_transactions[[stock]], function(i) {
          shares <- abs(input[[paste0(stock, "_shares_", i)]])
          data.frame(Stock = stock, Type = "Buy", Date = as.Date(input$start_date), Shares = shares, stringsAsFactors = FALSE)
        }))
      }))
      
      portfolios$data[[portfolio_name]] <- new_transactions
      updateCheckboxGroupInput(session, "portfolios_to_plot",
                               choices = names(portfolios$data),
                               selected = intersect(input$portfolios_to_plot, names(portfolios$data)))
    })
    
    # Data table
    dataframe_to_display <- reactiveVal(NULL)
    observe({
      req(input$show_dataframe, input$portfolios_to_plot)
      sel <- input$portfolios_to_plot
      dataframe_to_display(bind_rows(lapply(sel, function(portfolio_name) {
        portfolio_transactions <- portfolios$data[[portfolio_name]]
        
        stock_values <- lapply(unique(portfolio_transactions$Stock), function(stock) {
          stock_data <- tryCatch({
            getSymbols(stock, src = "yahoo", from = input$start_date, to = input$end_date, auto.assign = FALSE)
          }, error = function(e) {
            message(paste("Failed to load data for:", stock, "due to", e$message))
            return(NULL)
          })
          if (is.null(stock_data)) return(NULL)
          
          stock_prices <- Cl(stock_data)
          stock_df <- data.frame(Date = index(stock_prices), Price = as.numeric(stock_prices))
          
          tx_df <- portfolio_transactions %>%
            filter(Stock == stock) %>%
            arrange(Date) %>%
            mutate(CumulativeShares = cumsum(Shares)) %>%
            complete(Date = seq(as.Date(input$start_date), max(stock_df$Date), by = "day")) %>%
            fill(CumulativeShares, .direction = "down") %>%
            mutate(CumulativeShares = replace_na(CumulativeShares, 0))
          
          left_join(stock_df, tx_df, by = "Date") %>%
            arrange(Date) %>%
            mutate(Value = CumulativeShares * Price,
                   Portfolio = portfolio_name, Stock = stock) %>%
            select(Date, Stock, CumulativeShares, Price, Portfolio, Value)
        }) %>% bind_rows()
        stock_values
      })))
      output$dataframe_display <- DT::renderDataTable({ dataframe_to_display() })
    })
    
    # Plot + returns on GO
    observeEvent(input$go, {
      sel_portfolios <- input$portfolios_to_plot
      req(length(sel_portfolios) >= 1)
      
      enable_download(TRUE)
      
      plot_data <- bind_rows(lapply(sel_portfolios, function(portfolio_name) {
        portfolio_transactions <- portfolios$data[[portfolio_name]]
        
        stock_values <- lapply(unique(portfolio_transactions$Stock), function(stock) {
          stock_data <- tryCatch({
            getSymbols(stock, src = "yahoo", from = input$start_date, to = input$end_date, auto.assign = FALSE)
          }, error = function(e) {
            message(paste("Failed to load data for:", stock, "due to", e$message))
            return(NULL)
          })
          if (is.null(stock_data)) return(NULL)
          
          stock_prices <- Cl(stock_data)
          stock_df <- data.frame(Date = index(stock_prices), Price = as.numeric(stock_prices))
          
          tx_df <- portfolio_transactions %>%
            filter(Stock == stock) %>%
            arrange(Date) %>%
            mutate(CumulativeShares = cumsum(Shares)) %>%
            complete(Date = seq(as.Date(input$start_date), max(stock_df$Date), by = "day")) %>%
            fill(CumulativeShares, .direction = "down") %>%
            mutate(CumulativeShares = replace_na(CumulativeShares, 0))
          
          left_join(stock_df, tx_df, by = "Date") %>%
            arrange(Date) %>%
            mutate(Value = CumulativeShares * Price,
                   Portfolio = portfolio_name, Stock = stock) %>%
            select(Date, Stock, Value, Portfolio, CumulativeShares, Price)
        }) %>% bind_rows()
        stock_values
      }))
      
      portfolio_data <- plot_data %>%
        group_by(Date, Portfolio) %>%
        summarise(Total_Portfolio_Value = sum(Value, na.rm = TRUE), .groups = 'drop')
      
      rv$plot_data <- plot_data
      rv$portfolio_data <- portfolio_data
      
      rets_data <- portfolio_data %>%
        group_by(Portfolio) %>%
        arrange(Date, .by_group = TRUE) %>%
        mutate(Return = Total_Portfolio_Value / dplyr::lag(Total_Portfolio_Value) - 1) %>%
        filter(!is.na(Return)) %>%
        ungroup()
      rv$rets_data <- rets_data
      
      stock_colors <- RColorBrewer::brewer.pal(n = min(12, length(unique(plot_data$Stock))), "Set1")
      portfolio_colors <- RColorBrewer::brewer.pal(n = min(8, length(unique(portfolio_data$Portfolio))), "Dark2")
      
      if (nrow(portfolio_data) > 0) {
        if (input$plot_type == "Portfolio") {
          fig <- plot_ly(data = portfolio_data, x = ~Date, y = ~Total_Portfolio_Value, color = ~Portfolio,
                         type = 'scatter', mode = 'lines', colors = portfolio_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Date:", Date, "<br>Total Value:", round(Total_Portfolio_Value, 2)),
                         hoverinfo = "text") %>%
            layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e", font = list(color = "#00BFFF"),
                   xaxis = list(title = "Date"), yaxis = list(title = "Portfolio Value (USD)"))
        } else if (input$plot_type == "Individual") {
          fig <- plot_ly(data = plot_data, x = ~Date, y = ~Value, color = ~Stock,
                         type = 'scatter', mode = 'lines', colors = stock_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Stock:", Stock, "<br>Date:", Date,
                                       "<br>Price:", round(Price, 2), "<br>Shares:", round(CumulativeShares, 2),
                                       "<br>Position Value:", round(Value, 2)),
                         hoverinfo = "text") %>%
            layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e", font = list(color = "#00BFFF"),
                   xaxis = list(title = "Date"), yaxis = list(title = "Stock Value (USD)"))
        } else { # Both
          fig <- plot_ly(data = plot_data, x = ~Date, y = ~Value, color = ~Stock,
                         type = 'scatter', mode = 'lines', colors = stock_colors,
                         text = ~paste("Portfolio:", Portfolio, "<br>Stock:", Stock, "<br>Date:", Date,
                                       "<br>Price:", round(Price, 2), "<br>Shares:", round(CumulativeShares, 2),
                                       "<br>Position Value:", round(Value, 2)),
                         hoverinfo = "text") %>%
            add_trace(data = portfolio_data, x = ~Date, y = ~Total_Portfolio_Value, color = ~Portfolio,
                      type = 'scatter', mode = 'lines', colors = portfolio_colors,
                      name = ~Portfolio,
                      text = ~paste("Portfolio:", Portfolio, "<br>Date:", Date,
                                    "<br>Total Value:", round(Total_Portfolio_Value, 2)),
                      hoverinfo = "text") %>%
            layout(paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e", font = list(color = "#00BFFF"),
                   xaxis = list(title = "Date"), yaxis = list(title = "Value (USD)"))
        }
        output$stock_plot <- renderPlotly({ fig })
      } else {
        output$stock_plot <- renderPlotly({
          plot_ly() %>% layout(title = "No Data to Display",
                               xaxis = list(title = "Date"), yaxis = list(title = "Value (USD)"))
        })
      }
      
      # Dynamic per-portfolio histograms (Plotly)
      hist_ids <- paste0("hist_", make.names(sel_portfolios))
      output$returns_hist_container <- renderUI({
        tagList(lapply(seq_along(sel_portfolios), function(i) {
          div(
            h4(paste0("Daily Returns Histogram — ", sel_portfolios[i])),
            plotlyOutput(hist_ids[i], height = "320px"),
            style = "margin-bottom:18px;"
          )
        }))
      })
      lapply(seq_along(sel_portfolios), function(i) {
        local({
          pid <- sel_portfolios[i]
          out_id <- hist_ids[i]
          output[[out_id]] <- renderPlotly({
            req(rv$rets_data)
            dat <- rv$rets_data %>% filter(Portfolio == pid)
            p <- plot_ly(dat, x = ~Return, type = "histogram",
                         nbinsx = 50, histnorm = "probability density",
                         marker = list(line = list(width = 0))) %>%
              layout(
                title = paste("Histogram (", pid, ")"),
                bargap = 0.05,
                paper_bgcolor = "#1e1e1e",
                plot_bgcolor = "#1e1e1e",
                font = list(color = "#00BFFF"),
                xaxis = list(title = "Daily Return", tickformat = ".2%"),
                yaxis = list(title = "Density"),
                showlegend = FALSE
              )
            dens <- density(dat$Return, na.rm = TRUE)
            add_lines(p, x = dens$x, y = dens$y, name = "Density",
                      line = list(color = "red", width = 2), inherit = FALSE, hoverinfo = "skip")
          })
        })
      })
    })
    
    # On-screen report (lists included portfolios)
    output$portfolio_report <- renderUI({
      sel <- input$portfolios_to_plot
      req(rv$portfolio_data, rv$plot_data, length(sel) >= 1)
      
      fmt_pct  <- function(x) ifelse(is.na(x), "—", paste0(sprintf("%.2f", 100*x), "%"))
      fmt_usd  <- function(x) ifelse(is.na(x), "—", paste0("$", formatC(x, big.mark = ",", format = "f", digits = 2)))
      fmt_date <- function(x) ifelse(is.na(x), "—", format(as.Date(x), "%Y-%m-%d"))
      
      header <- tags$div(
        style = "display:flex; justify-content:space-between; align-items:flex-start; margin-bottom:8px;",
        tags$div(
          tags$h2("Reporte de Portafolio", style = "margin:0;"),
          tags$h4("app https://joseahumada.shinyapps.io/portfolio/ creada por José Ahumada Castillo",
                  style = "margin:0; color:#9ecff5; font-weight:normal;"),
          tags$p(HTML(paste0("<i>Portfolios incluidos: <b>", paste(sel, collapse = ", "), "</b></i>")))
        ),
        tags$div(style = "text-align:right; font-weight:bold;", format(Sys.Date(), "%Y-%m-%d"))
      )
      
      panels <- lapply(sel, function(pn) {
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
        var95_pct_ann <- ifelse(is.na(var95_pct), NA_real_, var95_pct * sqrt(252))
        var95_abs_ann <- ifelse(is.na(var95_pct_ann), NA_real_, var95_pct_ann * last_val)
        
        stocks_in <- rv$plot_data %>% filter(Portfolio == pn) %>% distinct(Stock) %>% arrange(Stock) %>% pull(Stock)
        
        tags$div(
          style = "margin-top:6px;",
          tags$h3(paste("Portafolio:", pn)),
          tags$p(paste("Fechas:", fmt_date(input$start_date), "→", fmt_date(input$end_date))),
          tags$p(HTML(paste0("Stocks contenidos: <b>", paste(stocks_in, collapse = ", "), "</b>"))),
          tags$table(
            style = "width:100%;",
            tags$tbody(
              tags$tr(tags$td("Rendimiento del periodo"), tags$td(style="text-align:right;", fmt_pct(period_ret))),
              tags$tr(tags$td("Varianza (diaria) del periodo"), tags$td(style="text-align:right;", ifelse(is.na(var_daily),"—",sprintf("%.6f", var_daily)))),
              tags$tr(tags$td("Rendimiento anualizado"), tags$td(style="text-align:right;", fmt_pct(ann_ret))),
              tags$tr(tags$td("Varianza anualizada"), tags$td(style="text-align:right;", ifelse(is.na(var_ann),"—",sprintf("%.6f", var_ann)))),
              tags$tr(tags$td("VaR 95% (diario, % de pérdida)"), tags$td(style="text-align:right;", fmt_pct(var95_pct))),
              tags$tr(tags$td("VaR 95% (diario, monto)"), tags$td(style="text-align:right;", fmt_usd(var95_abs))),
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
    
    # Enable dataframe download once plotted
    observe({ shinyjs::toggleState("download_dataframe", condition = enable_download()) })
    
    output$download_dataframe <- downloadHandler(
      filename = function() paste("dataframe_", Sys.Date(), ".csv", sep = ""),
      content  = function(file) write.csv(dataframe_to_display(), file, row.names = FALSE)
    )
    
    # ---------- Report builders & downloaders ----------
    # Helper to build the full light-mode (white bg) HTML document with Garamond 12pt
    build_report_html <- function(sel) {
      fmt_pct  <- function(x) ifelse(is.na(x), "—", paste0(sprintf("%.2f", 100*x), "%"))
      fmt_usd  <- function(x) ifelse(is.na(x), "—", paste0("$", formatC(x, big.mark = ",", format = "f", digits = 2)))
      fmt_date <- function(x) ifelse(is.na(x), "—", format(as.Date(x), "%Y-%m-%d"))
      
      header <- tags$div(
        style = "display:flex; justify-content:space-between; align-items:flex-start; margin-bottom:8px;",
        tags$div(
          tags$h2("Reporte de Portafolio", style = "margin:0;"),
          tags$h4("app https://joseahumada.shinyapps.io/portfolio/ creada por José Ahumada Castillo",
                  style = "margin:0; color:#555; font-weight:normal;"),
          tags$p(HTML(paste0("<i>Portfolios incluidos: <b>", paste(sel, collapse = ", "), "</b></i>")))
        ),
        tags$div(style = "text-align:right; font-weight:bold;", format(Sys.Date(), "%Y-%m-%d"))
      )
      
      # ----- Time series ggplot (different color per portfolio) -----
      pd_all <- rv$portfolio_data %>%
        filter(Portfolio %in% sel) %>%
        arrange(Date)
      
      gg_tag <- NULL
      if (nrow(pd_all) > 0) {
        base_pal <- c("blue", "red", "green", "purple", "gold", "orange",
                      "brown", "pink", "cyan", "magenta")
        portfolios_unique <- unique(pd_all$Portfolio)
        pal_use <- setNames(rep(base_pal, length.out = length(portfolios_unique)),
                            portfolios_unique)
        
        subtitle_text <- paste0(
          "Portafolios: ", paste(sel, collapse = ", "),
          " • Fechas: ", fmt_date(input$start_date), " a ", fmt_date(input$end_date)
        )
        caption_text <- "Fuente: Yahoo Finance vía quantmod · Reporte generado por la app Portfolio Analysis App creada por José Ahumada Castillo"
        
        g <- ggplot(pd_all,
                    aes(x = Date,
                        y = Total_Portfolio_Value,
                        color = Portfolio,
                        group = Portfolio)) +
          geom_line(linewidth = 1) +
          scale_color_manual(values = pal_use) +
          scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
          labs(
            title = "Serie de Tiempo del Valor de los Portafolios",
            subtitle = subtitle_text,
            x = "Fecha",
            y = "Valor del Portafolio (USD)",
            color = "Portafolio",
            caption = caption_text
          ) +
          ggthemes::theme_economist() +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(size = 8),
            plot.title.position = "plot",
            plot.caption.position = "plot"
          )
        
        tmp_png <- tempfile(fileext = ".png")
        ggsave(filename = tmp_png, plot = g, width = 10, height = 5, dpi = 150, bg = "white")
        gg_tag <- tags$img(
          src = paste0("data:image/png;base64,", base64enc::base64encode(tmp_png)),
          style = "width:100%; height:auto; margin: 8px 0 16px 0; border: 1px solid #ddd; border-radius: 4px;"
        )
      }
      
      # ----- Per-portfolio histograms (separate ggplots) -----
      hist_imgs <- list()
      if (!is.null(rv$rets_data) && nrow(rv$rets_data) > 0) {
        for (pn in sel) {
          dat <- rv$rets_data %>% filter(Portfolio == pn)
          if (nrow(dat) > 0) {
            subtitle_hist <- paste0(
              "Periodo: ", fmt_date(input$start_date), " a ", fmt_date(input$end_date),
              " • La línea roja muestra la densidad estimada"
            )
            caption_hist <- "Fuente: Yahoo Finance vía quantmod · Reporte generado por la app de José Ahumada Castillo"
            
            g_hist <- ggplot(dat, aes(x = Return)) +
              geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.85,
                             position = "identity", color = "white", linewidth = 0.15, fill = "#4575b4") +
              geom_density(aes(x = Return), color = "red", linewidth = 1, inherit.aes = FALSE) +
              scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
              labs(
                title = paste("Histograma de Rendimientos Diarios —", pn),
                subtitle = subtitle_hist,
                x = "Rendimiento Diario",
                y = "Densidad",
                caption = caption_hist
              ) +
              ggthemes::theme_economist() +
              theme(
                plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(size = 8),
                plot.title.position = "plot",
                plot.caption.position = "plot"
              )
            
            tmp_png_h <- tempfile(fileext = ".png")
            ggsave(filename = tmp_png_h, plot = g_hist, width = 10, height = 5, dpi = 150, bg = "white")
            hist_imgs[[pn]] <- tags$img(
              src = paste0("data:image/png;base64,", base64enc::base64encode(tmp_png_h)),
              style = "width:100%; height:auto; margin: 8px 0 16px 0; border: 1px solid #ddd; border-radius: 4px;"
            )
          }
        }
      }
      
      # ----- Metrics panels (same math as on-screen) -----
      panels <- lapply(sel, function(pn) {
        pd <- rv$portfolio_data %>% filter(Portfolio == pn) %>% arrange(Date)
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
        var95_pct_ann <- ifelse(is.na(var95_pct), NA_real_, var95_pct * sqrt(252))
        var95_abs_ann <- ifelse(is.na(var95_pct_ann), NA_real_, var95_pct_ann * last_val)
        
        stocks_in <- rv$plot_data %>% filter(Portfolio == pn) %>% distinct(Stock) %>% arrange(Stock) %>% pull(Stock)
        
        tags$div(
          style = "margin-top:6px;",
          tags$h3(paste("Portafolio:", pn)),
          tags$p(paste("Fechas:", fmt_date(input$start_date), "→", fmt_date(input$end_date))),
          tags$p(HTML(paste0("Stocks contenidos: <b>", paste(stocks_in, collapse = ", "), "</b>"))),
          tags$table(
            style = "width:100%;",
            tags$tbody(
              tags$tr(tags$td("Rendimiento del periodo"), tags$td(style="text-align:right;", fmt_pct(period_ret))),
              tags$tr(tags$td("Varianza (diaria) del periodo"), tags$td(style="text-align:right;", ifelse(is.na(var_daily),"—",sprintf("%.6f", var_daily)))),
              tags$tr(tags$td("Rendimiento anualizado"), tags$td(style="text-align:right;", fmt_pct(ann_ret))),
              tags$tr(tags$td("Varianza anualizada"), tags$td(style="text-align:right;", ifelse(is.na(var_ann),"—",sprintf("%.6f", var_ann)))),
              tags$tr(tags$td("VaR 95% (diario, % de pérdida)"), tags$td(style="text-align:right;", fmt_pct(var95_pct))),
              tags$tr(tags$td("VaR 95% (diario, monto)"), tags$td(style="text-align:right;", fmt_usd(var95_abs))),
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
      
      # ----- Assemble LIGHT-MODE HTML (white background, black text) -----
      tags$html(
        tags$head(
          tags$meta(charset = "UTF-8"),
          tags$title("Reporte de Portafolio"),
          # Load EB Garamond for the document
          tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=EB+Garamond:wght@400;700&display=swap"),
          tags$style(HTML("
            :root { --fg: #111111; --fg-subtle:#555555; --bg:#ffffff; }
            body { background: var(--bg); color: var(--fg); 
                   font-family: 'EB Garamond','GaramondPremrPro','Adobe Garamond Pro','EB Garamond', Garamond, serif; 
                   font-size: 12pt; padding: 16px; }
            h2 { margin:0; }
            h3 { margin-top: 12px; }
            h4 { margin:0; color: var(--fg-subtle); }
            table { width:100%; border-collapse: collapse; }
            td { padding: 4px 0; }
          "))
        ),
        tags$body(
          header,
          do.call(tagList, panels),
          if (!is.null(gg_tag)) gg_tag,
          do.call(tagList, hist_imgs)
        )
      )
    }
    
    # Helper that tries multiple PDF backends
    make_pdf <- function(html_path, pdf_path) {
      ok <- FALSE
      
      # Try pagedown + Chrome/Chromium
      if (requireNamespace("pagedown", quietly = TRUE)) {
        chrome_ok <- FALSE
        try({
          ch <- pagedown::find_chrome()
          chrome_ok <- length(ch) > 0 && !is.na(ch)
        }, silent = TRUE)
        if (chrome_ok) {
          try({
            pagedown::chrome_print(input = html_path, output = pdf_path)
            ok <- file.exists(pdf_path)
          }, silent = TRUE)
        }
      }
      
      # Try webshot2 (needs Chrome as well, but worth trying)
      if (!ok && requireNamespace("webshot2", quietly = TRUE)) {
        try({
          webshot2::webshot(url = html_path, file = pdf_path, vwidth = 1200, vheight = 2600, delay = 0.5)
          ok <- file.exists(pdf_path)
        }, silent = TRUE)
      }
      
      # Try webshot + PhantomJS (we can install PhantomJS at runtime)
      if (!ok && requireNamespace("webshot", quietly = TRUE)) {
        try({
          if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()
          webshot::webshot(url = html_path, file = pdf_path, vwidth = 1200, vheight = 2600)
          ok <- file.exists(pdf_path)
        }, silent = TRUE)
      }
      
      ok
    }
    
    # ---- HTML download (always works) ----
    output$download_report_html <- downloadHandler(
      filename = function() paste0("reporte_portafolio_", Sys.Date(), ".html"),
      content = function(file) {
        sel <- input$portfolios_to_plot
        req(rv$portfolio_data, length(sel) >= 1)
        html_doc <- build_report_html(sel)
        tmp_html <- tempfile(fileext = ".html")
        htmltools::save_html(html_doc, tmp_html)
        file.copy(tmp_html, file, overwrite = TRUE)
      }
    )
    
    # ---- PDF download with robust fallbacks ----
    output$download_report_pdf <- downloadHandler(
      filename = function() paste0("reporte_portafolio_", Sys.Date(), ".pdf"),
      content = function(file) {
        sel <- input$portfolios_to_plot
        req(rv$portfolio_data, length(sel) >= 1)
        
        html_doc <- build_report_html(sel)
        tmp_html <- tempfile(fileext = ".html")
        htmltools::save_html(html_doc, tmp_html)
        
        ok <- make_pdf(tmp_html, file)
        if (!ok) {
          showModal(modalDialog(
            title = "PDF export is not available on this server",
            "I couldn't start a headless browser (Chrome/Chromium or PhantomJS).
             Please use 'Download report (HTML)' and then print to PDF from your browser.",
            easyClose = TRUE
          ))
          stop("PDF export unavailable: no headless renderer found.")
        }
      }
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

