
configUrl <- './data/nonquotes/config.csv'
config <- read.csv(configUrl)
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
simEndDate <- as.character(Sys.Date())
simStartDate <- as.character(Sys.Date() - 365)
simStartDateMin = tenYearsAgoToday(); simEndDateMax = simEndDate

# Get company drop down values
companyDataUrl <- "./data"
demoQuoteDataFiles <- dir('./data', '*.csv')
demoTickersList <- strsplit(demoQuoteDataFiles, '.csv')

# Get moving average options for radio buttons
movingAvgUrl <- "./data/nonquotes/moving_avgs.csv"
maOptions <- read.csv(movingAvgUrl, stringsAsFactors = FALSE)
maOptionsList <- makeOptList(maOptions$MA_Type, maOptions$option)

# Get postion mgmt strategies to populate Position Management select list
posMgmtStratsUrl <- "./data/nonquotes/positionStrats.csv"
pmOptions <- read.csv(posMgmtStratsUrl, stringsAsFactors = FALSE)
# pmOptionsList <- as.list((pmOptions$Position_Sizing_Strategy))
# only first position mgmt option is implemented
pmOptionsList <- makeOptList(pmOptions$PSS_Type[1], pmOptions$option[1])

fluidPage(
    headerPanel("MACO Analyzer"),
    sidebarPanel(
        # http://shiny.rstudio.com/gallery/selectize-examples.html example #6
        textInput('inTicker', label=h4("Company:")),
        dateRangeInput('inQueryDateRange', label = h4("Quotes Date Range:"),
                       start=simStartDate, end=simEndDate,
                       min=simStartDateMin, max=simEndDateMax),
        radioButtons('inMovAvg', label=h4("Moving Average (MA):"),
                     choices=maOptionsList, selected=1, inline = TRUE),
        # actionButton('inQueryQuotes', 'Get Quote Data'),
        sliderInput('inFastSlowMavg', h4("Fast & Slow MA Days"),
                    min = 2, max = 200, value = c(9,18)),
        numericInput('inAccBalance', 'Starting Account Balance:',
                     10000, min = 5000, max = 1000000, step = 500),
        selectInput('inPosMgmt', label=h4("Position Management:"),
                    choices=pmOptionsList, selected=1),
        actionButton('inRunSimButton', 'Run Simulation')

    ),
    mainPanel(
        tabsetPanel(
            tabPanel('Analyze',
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
            tabPanel("User Guide", HTML(signal_tab_content)
            ),
            tabPanel("Source Code", HTML(source_tab_content)
            ),
            selected = 'Analyze'
        )
    )
)