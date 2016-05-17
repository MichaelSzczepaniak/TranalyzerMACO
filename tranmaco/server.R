source("TradingEvaluator.R")
source("StrategySimulator.R")

shinyServer(
    function(input, output) {
        output$outQuoteDataStatus <- eventReactive(input$inQueryQuotes, {
            quoteDateRange <- sprintf('%s%s%s%s', 'from ',
                                      input$inQueryDateRange[1], ' to ',
                                      input$inQueryDateRange[2])
            sprintf('%s%s%s%s', input$ticker, ' quotes ', quoteDateRange,
                    ' acquired.')
        })
        
        
    }
)