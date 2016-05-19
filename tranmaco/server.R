source("DataManager.R")
source("TradingEvaluator.R")
source("StrategySimulator.R")

shinyServer(
    function(input, output) {
        
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
                sprintf('%s', 'NO QUOTE DATA LOADED: Get Quote data before beginning.')
            }
            
        }, ignoreNULL = FALSE)
        
        
    }
)