library( shiny )
library( ggplot2 )

feralIndex <- read.csv( "data/feral_index.csv" )
  
shinyServer( 
  
  

  function( input, output, session ){

#     catModel <- reactive( { data.frame( Year = catFacts$Year, 
#                                         Feral_Index = catFacts$feralIndex, 
#                                         estTotal = catFacts$feralIndex / input$q, 
#                                         N = ) } )
#     catModel <- reactiveValues( { Year = catFacts$Year
#     feralIndex = catFacts$feralIndex
#     estTotal = catFacts$feralIndex / input$q
#     N = estTotal
#     impComp = N * input$q
#     for( i in 2:length( N ) ){
#       N[ i ] = N[ i - 1 ] * exp( input$b0 - ( input$a * input$q * N[ i - 1 ] ) - 0.22 ) - impComp[ i - 1 ]
#     }
#     } )
    catFacts <- reactiveValues()
    data <- reactive( { catFacts$Year = min( feralIndex$Year ):( max( feralIndex$Year ) + 200 )
                        catFacts$feralIndex = c( feralIndex$Feral_Index, 
                                                 rep( NA, times = 200 ) )
                        catFacts$estTotal = catFacts$feralIndex / input$q
                        catFacts$N = catFacts$estTotal
                        catFacts$impComp = catFacts$N * input$q
                        for( i in 2:length( catFacts$N ) ){
                          catFacts$N[ i ] = catFacts$N[ i - 1 ] * exp( input$b0 - ( input$a * input$q * catFacts$N[ i - 1 ] ) - 0.22 ) - catFacts$impComp[ i - 1 ]
                          catFacts$impComp[ i ] = catFacts$N[ i ] * input$q
                        } } )
    
    output$linePlot <- renderPlot( {
      ggplot( data = data(), 
              aes( x = catFacts$Year, 
                   y = catFacts$N ) ) + 
        geom_line() + 
        theme_classic()
    } )
    
  }
)
