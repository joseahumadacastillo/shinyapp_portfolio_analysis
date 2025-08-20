Readme/

The goal of this app is to provide basic stock-portfolio analysis. It retrieves price data for the selected tickers and date range from the Yahoo Finance API, then lets you perform core portfolio analytics. It also generates a downloadable report with key performance metrics, a time-series plot of portfolio value, and a histogram of daily returns.

Instructions:

In your preferred browser, on your computer or phone, go to: https://joseahumada.shinyapps.io/portfolio/

Enter the password to access the app. The password for the public version is “public.” Press Enter or click the Login button.

Give your portfolio a name. By default it’s “Mi Portafolio” (My Portfolio), but you can choose a different name that helps you remember key characteristics (e.g., “Tech” or “August 2024”).

Set the end date up to which you want to compare. Often this will be today, which reflects results up to the close of the most recent market day. You can also use fixed windows: 1 month, 1 quarter (3 months), 6 months, or 1 year.

Select the stock tickers that make up your portfolio in “Stocks.” (You can choose several.)

For each ticker, enter the number of shares in “Shares” (fractions are allowed, e.g., 0.5).

Click “Save Portfolio” to save that combination under the name you chose.

Go to the “Plot” section and, in “Select Portfolios to Plot,” choose the name of your portfolio.

In “Choose Plot Type,” select the chart type: o Individual: value of each stock separately. o Portfolio: total portfolio value. o Both: both views overlaid.

Press “Download and Plot.” The app will download prices from Yahoo Finance and draw the series. (The chart is interactive—hover to see details.)

Review the histogram of daily returns that appears below the main chart. You’ll see the bars by portfolio and a red density line summarizing the distribution.

Scroll to “Reporte de Portafolio” (below the charts). There you’ll find: • Portfolio name, start and end dates, and list of stocks. • Period return. • Daily variance and annualized variance. • Annualized return. • 95% VaR (daily, % and amount) and 95% VaR (annualized, % and amount). • Initial, maximum, minimum, and final portfolio value.

To download the report as a PDF, click “Descargar reporte (PDF).” The document includes first the text/table with metrics and then two ggplot + ggthemes (Economist) charts: • Portfolio value series (blue line). • Returns histogram with a red density line. Note: PDF export uses pagedown (requires Chrome/Chromium installed on the system).

If you want to view the data table, check “Show Dataframe.” To download it as CSV, use “Download Data Frame.” Instrucciones en español:

En tu navegador de preferencia, desde tu computadora o celular, entra a https://joseahumada.shinyapps.io/portfolio/

Escribe el password para acceder a la app. El password para la versión pública de la app es “public”. Presiona enter o el botón de login.

Ponle nombre al portafolio. Por default el portafolio se llamará “Mi Portafolio” pero puedes ponerle un nombre distinto, que te recuerde características importantes del portafolio. (Por ejemplo, tecnológicas o agosto 2024).

Establece la fecha final hasta la cual quieres comparar. Frecuentemente el día de hoy, que reflejará los resultados hasta el momento del cierre del día de apertura de mercado inmediata anterior. Otra posibilidad es hacer fechas “fijas” 1 mes, 1 cuarto (3 meses), 6 meses o 1 año.

Selecciona los tickers de las acciones que componen tu portafolio en “Stocks”. (Puedes elegir varios.)

Para cada ticker, captura el número de acciones en “Shares” (se permiten fracciones, p. ej. 0.5).

Da clic en “Save Portfolio” para guardar esa combinación con el nombre que elegiste.

Ve a la sección “Plot” y, en “Select Portfolios to Plot” selecciona el nombre de tu portafolio

En “Choose Plot Type”, selecciona el tipo de gráfica: o Individual: valor de cada acción por separado. o Portfolio: valor total del portafolio. o Both: ambas vistas superpuestas.

Presiona “Download and Plot”. La app descargará precios de Yahoo Finance y dibujará la(s) serie(s). (La gráfica es interactiva: puedes pasar el mouse para obtener información.)

Revisa el histograma de rendimientos diarios que aparece debajo de la gráfica principal. Verás las barras por portafolio y una línea de densidad roja que resume la distribución.

Desplázate a “Reporte de Portafolio” (debajo de las gráficas). Ahí encontrarás: o Nombre del portafolio, fechas de inicio y fin y lista de acciones. o Rendimiento del periodo. o Varianza diaria y varianza anualizada. o Rendimiento anualizado. o VaR 95% diario (en % y en monto) y VaR 95% anualizado (en % y monto). o Valor inicial, máximo, mínimo y final del portafolio.

Para descargar el reporte en PDF, haz clic en “Descargar reporte (PDF)”. El documento incluye primero el texto/tabla con métricas y después dos gráficas estilo ggplot + ggthemes (Economist): o Serie de valor del portafolio (línea azul). o Histograma de rendimientos con línea de densidad roja. Nota: La exportación a PDF usa pagedown (requiere Chrome/Chromium instalado en el sistema).

Si quieres ver la tabla de datos, marca “Show Dataframe”. Para bajarla en CSV, usa “Download Data Frame”. translate to english

✨ Features • Password gate (default password: public) • Portfolio builder o Global start & end dates (apply to all selected stocks) o Pick tickers from a curated list o Enter shares per stock (fractional supported) o Save multiple portfolios and compare them • Interactive charts (Plotly) o Portfolio value over time (individual stocks / portfolio / both) o Histogram of daily returns with a red density line • Report (in-app & PDF) o Title: Reporte de Portafolio o Subtitle: app https://joseahumada.shinyapps.io/portfolio/ creada por José Ahumada Castillo o Date shown top-right o Metrics per portfolio:  Portfolio name, start & end dates, constituents  Period return  Daily variance, annualized variance  Annualized return  VaR 95% (daily, % and amount)  VaR 95% (annualized, % and amount)  Initial, max, min, final portfolio value o PDF export includes:

Text tables first
Economist-themed ggplot line chart (portfolio line in blue)
Economist-themed ggplot histogram with red density line • Data table view + CSV download of the computed time series
🧮 How calculations are done • Prices: Yahoo Finance via quantmod::getSymbols() (close prices). • Position value per stock: shares * close_price. • Portfolio value: sum of stock position values per date. • Daily returns: V_t / V_{t-1} - 1. • Period return: V_end / V_start - 1. • Variance (daily): sample variance of daily returns. • Variance (annualized): daily variance × 252. • Annualized return: (1 + period_return)^(252 / n_days_with_returns) - 1. • VaR 95% (daily, %) o Empirical: -quantile(daily_returns, 0.05). • VaR 95% (daily, amount): daily VaR % × last portfolio value. • VaR 95% (annualized, %) o Scaled as daily VaR % × sqrt(252) (simplifying assumption). • VaR 95% (annualized, amount): annualized VaR % × last portfolio value. ⚠️ VaR scaling uses a normal-like assumption (√252). For heavy tails, consider a more robust approach.

🛠️ Requirements • R ≥ 4.1 (recommended 4.2+) • R packages (installed automatically the first time): o shiny, quantmod, dplyr, tidyr, plotly, viridis, zoo, DT, shinyjs, pagedown, htmltools, ggplot2, ggthemes, scales, base64enc o Also recommended: RColorBrewer (used for Plotly color palettes). If you see a palette error, run: o install.packages("RColorBrewer") • Google Chrome or Chromium (required by pagedown::chrome_print() for PDF export) If pagedown cannot find Chrome, set the path: macOS Sys.setenv(CHROMOTE_CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome") Windows (example path; adjust if needed) Sys.setenv(CHROMOTE_CHROME="C:/Program Files/Google/Chrome/Application/chrome.exe") Linux Sys.setenv(CHROMOTE_CHROME="/usr/bin/google-chrome")

or: /usr/bin/chromium, /snap/bin/chromium, etc.
🧭 More about Using the app

Portfolio o Set Portfolio Name o Pick Start Date and End Date
Stocks o Select tickers (AAPL, MSFT, GOOG, AMZN, TSLA, NVDA, META, NFLX, BA, DIS, BRK-B) o For each selected stock, enter Shares (can be fractional) o Click Save Portfolio
Plot o Choose one or more saved portfolios to plot o Select Plot Type: Individual, Portfolio, or Both o Click Download and Plot
Daily returns histogram o Displays the distribution of daily returns per selected portfolio (Plotly) o Red line = kernel density estimate
Report (below the plots) o Shows portfolio metrics o Click Descargar reporte (PDF) to export a PDF with the report and the two Economist-style ggplots (blue line & red-density histogram)
Data Frame o Check Show Dataframe to view the computed series o Use Download Data Frame to export CSV
🧩 Notes & caveats • Ticker symbols: Use Yahoo symbols. BRK-B works with a dash in this app. • Date ranges: Ensure start date < end date and there’s enough market data in the period. • Data source: Yahoo can throttle or temporarily block queries; retry if loading fails. • Shares: Always treated as buys (positive amounts). Shorting / sells aren’t modeled in this version. • Multiple portfolios: You can save many (with different names) and compare them in the plots & histogram. • PDF export in hosted environments: On some hosting (e.g., shinyapps.io), headless Chrome availability may vary. If PDF export fails, verify Chrome is present and CHROMOTE_CHROME is set. On RStudio Connect, ask your admin to enable a Chromium binary for headless printing.

🧪 Troubleshooting • “Can’t find Chrome / Chromium” during PDF Set CHROMOTE_CHROME as shown above and restart the R session/app. • PDF downloads as HTML Ensure you are using the app’s “Descargar reporte (PDF)” button (it calls pagedown::chrome_print() directly). If the browser still tries to save HTML: o Update pagedown (install.packages("pagedown")) o Ensure Chrome is reachable o Try another browser • Palette / color errors Install RColorBrewer: • install.packages("RColorreadme.docx Brewer") • Yahoo data fails for a ticker Try again later; confirm the symbol exists on Yahoo; reduce the date range.

🧰 Customization • Add/remove tickers: edit available_stocks in app.R. • Default styling: adjust the dark_mode_css string. • Risk model: swap the VaR computation for a parametric or historical-window approach if desired. • Plots: Change ggplot theme or colors inside the PDF builder section.

📄 License & Credits • Created by José Ahumada Castillo. • Uses open-source R packages under their respective licenses.

