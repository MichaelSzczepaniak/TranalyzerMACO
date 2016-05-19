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
            
            if(input$inQueryQuotes > 0) {
                pdat <- getStockQuotes(input$ticker,
                                       input$inQueryDateRange[1],
                                       input$inQueryDateRange[2])
                quoteDateRange <- sprintf('%s%s%s%s', 'from ',
                                          input$inQueryDateRange[1], ' to ',
                                          input$inQueryDateRange[2])
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
        
        
        
    }
)