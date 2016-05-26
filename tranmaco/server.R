source("DataManager.R")
source("TradingEvaluator.R")
source("StrategySimulator.R")

shinyServer(
    function(input, output) {
        
        simConfig <- function() {
            maToken <- 'SMA'
            # cat('input$inMovAvg =', input$inMovAvg, '\n')
            if(input$inMovAvg == 2) {
                maToken <- 'EMA'
            } else if(input$inMovAvg == 3) {
                maToken <- 'WMA'  # TODO
            }
            maToken <- sprintf('%s%s%s%s%s%s', maToken, '(', input$inFastSlowMavg[1],
                               ',', input$inFastSlowMavg[2], ')')
            # sab = starting account balance
            sabToken <- sprintf('%s%d', 'Start Bal=$', input$inAccBalance)
            if(input$inAccBalance != 10000) {
                sabToken <- sprintf('%s%d', 'Start Bal=$', input$inAccBalance)
            }
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

        output$outSimParams <- renderText({
            simConfig()
        })
        
        getQuotes <- function() {
            # cat(paste0('init ticker: [', input$inTicker, '], is.null=',
            #            is.null(input$inTicker), ', length=',
            #            length(input$inTicker), ', empty sting? ',
            #            (input$inTicker == ''), '\n'
            #            )
            #     )
            startDateStr <- as.character(input$inQueryDateRange[1])
            endDateStr <- as.character(input$inQueryDateRange[2])
            pdat <- getDemoQuotes(input$inTicker, startDateStr, endDateStr)
            pdat
        }
        
        getQuotesObj <- function() {
            quoteStatusMsg <- paste0('Select Company ticker to acquire quote data.')
            pdat <- NULL
            if(input$inTicker != '') {
                startDateStr <- as.character(input$inQueryDateRange[1])
                endDateStr <- as.character(input$inQueryDateRange[2])
                # pdat <- getDemoQuotes(input$inTicker, startDateStr, endDateStr)
                pdat <- getQuotes()
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
        
        output$outQuoteDataStatus <- renderText({
            quoteStatus <- getQuotesObj()[[1]]
            quoteStatus
        })
        
        runSim <- eventReactive(input$inRunSimButton, {
            if(input$inFastSlowMavg[2] > input$inFastSlowMavg[1]) {
                sim <- doSimulation(input$ticker,
                                    priceData=getQuotesObj()[[2]],
                                    as.character(input$inQueryDateRange[1]),
                                    as.character(input$inQueryDateRange[2]),
                                    signalParms=c(fastDays=input$inFastSlowMavg[1],
                                                  slowDays=input$inFastSlowMavg[2]),
                                    maType = input$inMovAvg,
                                    signalGen = 'SignalGenMacoLongOnlyOpaat.R',
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
        
    }
)