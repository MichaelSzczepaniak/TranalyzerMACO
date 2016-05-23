source("DataManager.R")
source("TradingEvaluator.R")
source("StrategySimulator.R")

shinyServer(
    function(input, output) {
        
        simConfig <- function() {
            maToken <- 'SMA'
            if(input$inMovAvg == 2) {
                maToken <- 'EMA'
            } else if(input$inMovAvg == 3) {
                maToken <- 'WMA'  # TODO
            }
            maToken <- sprintf('%s%s%s%s%s%s', maToken, '(', input$inFastSlowMavg[1],
                               ',', input$inFastSlowMavg[2], ')')


            sabToken <- sprintf('%s%d', 'Start Bal=$', 10000)
            if(input$inAccBalance != 10000) {
                sabToken <- sprintf('%s%d', 'Start Bal=$', input$inAccBalance)
            }
            pmToken <- sprintf('%s%s', 'PM=', input$inPosMgmt)

            params <- sprintf('%s%s%s%s%s', maToken, ' | ',
                                            sabToken, ' | ', pmToken)
            params
        }

        output$outSimParams <- renderText({
            simConfig()
        })
        
        
        
        output$outQuoteDataStatus <- eventReactive(input$inQueryQuotes, {
            # http://stackoverflow.com/questions/33662033/shiny-how-to-make-reactive-value-initialize-with-default-value
            if(input$inQueryQuotes > 0) {
                startDateStr <- as.character(input$inQueryDateRange[1])
                endDateStr <- as.character(input$inQueryDateRange[2])
                pdat <- getStockQuotes(input$ticker, startDateStr, endDateStr)
                quoteDateRange <- sprintf('%s%s%s%s', 'from ',
                                          startDateStr, ' to ', endDateStr)
                if(nrow(pdat) > 0) {
                    sprintf('%s%s%s%s', input$ticker, ' quotes ', quoteDateRange,
                            ' acquired.')
                } else {
                    sprintf('%s%s%s%s', input$ticker, ' quotes ', quoteDateRange,
                            ' NOT acquired.')
                }
                
                
            } else {
                sprintf('%s', "NO QUOTE DATA: 'Get Quote Data' before 'Run Simulation'.")
            }
            
        }, ignoreNULL = FALSE)
        
        runSim <- eventReactive(input$inRunSimButton, {
            if(input$inFastSlowMavg[2] > input$inFastSlowMavg[1]) {
                sim <- doSimulation(input$ticker,
                                    priceData=NULL,
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
        
        output$outTradesNet <- renderPrint(
                netStrategyPL(runSim())
            )
        
    }
)