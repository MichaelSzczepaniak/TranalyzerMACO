source("DataManager.R")
source("TradingEvaluator.R")
source("StrategySimulator.R")

shinyServer(
    function(input, output) {
        
        ## Returns the string 'SMA' or 'EMA' depending on the radio button
        ## setting
        getMaToken <- function() {
            maToken <- 'SMA'
            # cat('input$inMovAvg =', input$inMovAvg, '\n')
            if(input$inMovAvg == 2) {
                maToken <- 'EMA'
            } else if(input$inMovAvg == 3) {
                maToken <- 'WMA'  # TODO
            }
            maToken
        }
        
        ## Returns string that summarizes the selected configuratio parameters
        ## for a simulation
        simConfig <- function() {
            maToken <- getMaToken()
            maToken <- sprintf('%s%s%s%s%s%s', maToken, '(', input$inFastSlowMavg[1],
                               ',', input$inFastSlowMavg[2], ')')
            # sab = starting account balance
            sabToken <- sprintf('%s%d', 'Start Bal=$', input$inAccBalance)
            if(input$inAccBalance != 10000) {
                sabToken <- sprintf('%s%d', 'Start Bal=$', input$inAccBalance)
            }
            # TODO need better way handle many position management strat's
            pmToken <- 'AIAO-OPAAT-OL'
            if(input$inPosMgmt == 2) {
                pmToken <- 'AIAO-OPAAT-LAS'
            } else if(input$inPosMgmt == 3) {
                pmToken <- '???'  #TODO
            }
            pmToken <- sprintf('%s%s', 'PM=', pmToken)

            params <- sprintf('%s%s%s%s%s', maToken, ' | ',
                                            sabToken, ' | ', pmToken)
            params
        }

        ## Sends config param string to ui
        output$outSimParams <- renderText({
            simConfig()
        })
        
        getQuotes <- function() {
            startDateStr <- as.character(input$inQueryDateRange[1])
            endDateStr <- as.character(input$inQueryDateRange[2])
            pdat <- getStockQuotes(input$inTicker, startDateStr, endDateStr)
            pdat
        }
        
        ## Gets quote data, builds a quote status message, puts both of these
        ## into a list which is returned to the caller.
        getQuotesObj <- function() {
            quoteStatusMsg <- paste0('Enter Company ticker to acquire quote data.')
            pdat <- NULL
            if(input$inTicker != '') {
                startDateStr <- as.character(input$inQueryDateRange[1])
                endDateStr <- as.character(input$inQueryDateRange[2])
                pdat <- getDemoQuotes(input$inTicker, startDateStr, endDateStr)
                # pdat <- getQuotes()
                quoteDateRange <- sprintf('%s%s%s%s', 'from ',
                                          startDateStr, ' to ', endDateStr)
                if(nrow(pdat) > 0) {
                    quoteStatusMsg <- sprintf('%s%s%s%s', input$inTicker,
                                              ' quotes ', quoteDateRange,
                                              ' acquired.')
                } else {
                    quoteStatusMsg <- sprintf('%s%s%s%s', input$inTicker,
                                              ' quotes ', quoteDateRange,
                                              ' NOT acquired.')
                }
                
            }
            
            quoteData <- list(quoteStatusMsg, pdat)
            
            quoteData
        }
        
        ## Sends the quote status back to the user.
        output$outQuoteDataStatus <- renderText({
            quoteStatus <- getQuotesObj()[[1]]
            quoteStatus
        })
        
        ## Runs the simulation when the Run Simulation button is clicked
        runSim <- eventReactive(input$inRunSimButton, {
            if(input$inFastSlowMavg[2] > input$inFastSlowMavg[1]) {
                sim <- doSimulation(ticker=input$ticker,
                                    priceData=getQuotesObj()[[2]],
                                    startDate=as.character(input$inQueryDateRange[1]),
                                    endDate=as.character(input$inQueryDateRange[2]),
                                    signalParms=c(fastDays=input$inFastSlowMavg[1],
                                                  slowDays=input$inFastSlowMavg[2]),
                                    maType = getMaToken(),
                                    signalGen='SignalGenMacoLongOnlyOpaat.R',
                                    startBalance=input$inAccBalance)
            } else {
                data.frame(
                    Error_Message=c("Slow SMA days must be larger than Fast SMA days.",
                                    "Fast SMA is the left circle slider and Slow SMA is the right.",
                                    "It looks like you put these circles on top of each other.",
                                    "Please make Slow SMA days larger than Fast SMA days and try again.")
                )
            }
            
            sim
        })
        
        output$outTrades <- renderTable({
            runSim()
        })
        
        getNetPL <- function() {
            simResult <- runSim()
            netPL <- netStrategyPL(simResult)
            netPL
        }
        
        output$outTradesNet <- renderPrint(getNetPL())
        
        ## Creates the upper trade signals plot in Graphics tab
        ## Note: makeTradeSignalsPlot impl'd in TradingEvaluator.R
        tradeSignalPlot <- eventReactive(input$inRunSimButton, {
            makeTradeSignalsPlot(input$inTicker, getQuotesObj()[[2]],
                                 getMaToken(),
                                 as.character(input$queryDateRange[1]),
                                 as.character(input$queryDateRange[2]),
                                 signalParms=c(fastDays=input$inFastSlowMavg[1],
                                               slowDays=input$inFastSlowMavg[2]),
                                 signalGen="SignalGenMacoLongOnlyOpaat.R",
                                 startBalance=input$inAccBalance,
                                 shift=0.04)
        })
        
        resultsHist <- eventReactive(input$inRunSimButton, {
            makeTradesResultsHist(input$ticker,
                                  as.character(input$queryDateRange[1]),
                                  as.character(input$queryDateRange[2]),
                                  runSim(),
                                  signalParms=c(fastDays=input$inFastSlowMavg[1],
                                                slowDays=input$inFastSlowMavg[2]),
                                  signalGen="SignalGenMacoLongOnlyOpaat.R",
                                  startBalance=input$inAccBalance)
        })
        
        output$oidTradeSignalsPlot <- renderPlot({
            tradeSignalPlot()
        })
        
        output$oidTradesResultsHist <- renderPlot({
            resultsHist()
        })
        
    }
)