## load configuration info
config_url <- './data/nonquotes/config.csv'
config <- read.csv(config_url, stringsAsFactors=FALSE)
signal_tab_content_file <- config[config$param == 'signal_tab_content',]$value
source_tab_content_file <- config[config$param == 'source_tab_content',]$value
config_dir <- config[config$param == 'config_dir',]$value
signal_tab_content_file <- paste0(config_dir, signal_tab_content_file)
source_tab_content_file <- paste0(config_dir, source_tab_content_file)
signal_tab_content <- readChar(signal_tab_content_file,
                               file.info(signal_tab_content_file)$size)
source_tab_content <- readChar(source_tab_content_file,
                               file.info(source_tab_content_file)$size)

makeOptList <- function(labels, values) {
    names(values) <- labels
    return(as.list(values))
}

## Returns the date that's 10 years ago today in the format:
## yyyy-mm-dd.
tenYearsAgoToday <- function() {
    today <- as.POSIXlt(Sys.Date())
    today$year <- today$year - 10
    return(as.character(today))
}

# Default dates to use with live data
end_date_default <- as.character(Sys.Date() - 1)
start_date_default <- as.character(Sys.Date() - 365)
earliest_start_date = tenYearsAgoToday()
latest_end_date = end_date_default

# Get moving average options for radio buttons
moving_avg_url <- "./data/nonquotes/moving_avgs.csv"
ma_options <- read.csv(moving_avg_url, stringsAsFactors = FALSE)
ma_options_list <- makeOptList(ma_options$MA_Type, ma_options$option)

# Get postion mgmt strategies to populate Position Management select list
pos_mgmt_strats_url <- "./data/nonquotes/positionStrats.csv"
pm_options <- read.csv(pos_mgmt_strats_url, stringsAsFactors = FALSE)
# only first position mgmt option is implemented
pm_options_list <- makeOptList(pm_options$PSS_Type[1], pm_options$option[1])

fluidPage(
    headerPanel("MACO Analyzer"),
    sidebarPanel(
        # Let user know app is computing:
        # stackoverflow.com/questions/17325521#22475216
        tags$head(tags$style(type="text/css", "
                             #loadmessage {
                             position: fixed;
                             top: 0px;
                             left: 0px;
                             width: 100%;
                             padding: 5px 0px 5px 0px;
                             text-align: center;
                             font-weight: bold;
                             font-size: 100%;
                             color: #000000;
                             background-color: #CCFF66;
                             z-index: 105;
                             }
                             ")),
        
        textInput('inTicker', label=h4("Ticker:")),
        dateRangeInput('inQueryDateRange', label = h4("Quotes Date Range:"),
                       start=start_date_default, end=end_date_default,
                       min=earliest_start_date, max=latest_end_date),
        actionButton('inQueryQuotes', 'Get Quote Data'),
        radioButtons('inMovAvg', label=h4("Moving Average (MA):"),
                     choices=ma_options_list, selected=1, inline = TRUE),
        sliderInput('inFastSlowMavg', h4("Fast & Slow MA Days"),
                    min = 2, max = 200, value = c(9,18)),
        numericInput('inAccBalance', 'Starting Account Balance:',
                     10000, min = 5000, max = 1000000, step = 500),
        selectInput('inPosMgmt', label=h4("Position Management:"),
                    choices=pm_options_list, selected=1),
        actionButton('inRunSimButton', 'Run Simulation'),
        
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Working...",id="loadmessage"))

    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Analyzer",
                h4('Quote Data Status:'),
                verbatimTextOutput("outQuoteDataStatus"),
                h4('Simulation Parameters:'),
                verbatimTextOutput("outSimParams"),
                h4(paste0("Trades using MA signal and position management:")),
                h6("(ProfitLoss calculation assumes $10 commission for each buy or sell)"),
                div(style='height:240px; overflow-y: scroll',
                    tableOutput("outTrades")
                ),
                h4('Net Trading Profit/Loss:'),
                verbatimTextOutput("outTradesNet")
            ),
            tabPanel('Visualize', h4("Trades Identified Using This Signal:"),
                     h5(paste0("In the chart below, green triangles point to ",
                               "the BUY signals and are shifted down below ",
                               "the signals.  The red triangles point to the ",
                               "SELL signals and are shifted up above the ",
                               "signals to make them easier to see:")),
                     plotOutput("oidTradeSignalsPlot"),
                     h4("Breakdown of Simulated Trade Results:"),
                     plotOutput("oidTradesResultsHist")
            ),
            tabPanel('User Guide', HTML(signal_tab_content)
            ),
            tabPanel('Source Code', HTML(source_tab_content)
            )
        )
    )
)