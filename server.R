library( shiny )
library( dplyr )
library( tidyr )
library( ggplot2 )

feralIndex <- read.csv( "data/feral_index.csv" )
  
shinyServer( 
  function( input, output, session ){
    catFacts <- reactive( {
      
      toShow <- list( dn = FALSE, 
                      sq = FALSE, 
                      dr = FALSE, 
                      tnr = FALSE )
      
      for( k in names( toShow ) ){
        if( k %in% input$toShow ){
          toShow[ which( input$toShow == k ) ] <- TRUE
        }
      }
      
      parameters <- list( a = input$a, 
                          b0 = input$b0, 
                          q = input$q, 
                          sdb = input$a * input$q, 
                          c = 0, 
                          d0 = 0.22, 
                          TNRq = ifelse( toShow$tnr, input$TNRq, 0 ), 
                          rq = ifelse( toShow$dr, input$rq, 0 ) )
      
      theData <- data.frame( Year = min( feralIndex$Year ):( max( feralIndex$Year ) + 50 ), 
                             fIndex = c( feralIndex$Feral_Index, 
                                         rep( x = NA, times = 50 ) ) )
      theData$estTotal <- theData$fIndex / parameters$q

      # "Do Nothing" scenario
      if( toShow$dn ){
        theData$dn_N <- theData$estTotal
        theData$dn_Impounded <- theData$dn_N * parameters$q
        for( h in 2:17 ){
          theData$dn_N[ h ] <- theData$dn_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$dn_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$dn_N[ h - 1 ] + theData$dn_Impounded[ h - 1 ] ) ) ) - theData$dn_Impounded[ h - 1 ]
          theData$dn_Impounded[ h ] <- theData$dn_N[ h ] * parameters$q
        }
        for( i in 18:length( theData$Year ) ){
          theData$dn_N[ i ] <- theData$dn_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$dn_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$dn_N[ i - 1 ] + theData$dn_Impounded[ i - 1 ] ) ) )
          theData$dn_Impounded[ i ] <- theData$dn_N[ i ] * parameters$q
        }
      }
      
      # "Status Quo" scenario
      if( toShow$sq ){
        theData$sq_N <- theData$estTotal
        theData$sq_Impounded <- theData$sq_N * parameters$q
        for( h in 2:17 ){
          theData$sq_N[ h ] <- theData$sq_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$sq_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$sq_N[ h - 1 ] + theData$sq_Impounded[ h - 1 ] ) ) ) - theData$sq_Impounded[ h - 1 ]
          theData$sq_Impounded[ h ] <- theData$sq_N[ h ] * parameters$q
        }
        for( i in 18:length( theData$Year ) ){
          theData$sq_N[ i ] <- theData$sq_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$sq_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * theData$sq_N[ i - 1 ] ) ) - theData$sq_Impounded[ i - 1 ] + 100
          theData$sq_Impounded[ i ] <- theData$sq_N[ i ] * parameters$q
        }
      }

      # "Destructive Removal" scenario
      if( toShow$dr ){
        theData$dr_N <- theData$estTotal
        theData$dr_Impounded <- theData$dr_N * parameters$q
        theData$dr_removed <- c( rep( x = 0, times = 17 ), rep( x = parameters$rq, times = ( length( theData$Year ) - 17 ) ) )
        for( h in 2:17 ){          
          theData$dr_N[ h ] <- theData$dr_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$dr_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * theData$dr_N[ h - 1 ] ) ) - theData$dr_Impounded[ h - 1 ]
          theData$dr_Impounded[ h ] <- theData$dr_N[ h ] * parameters$q
        }
        for( i in 18:length( theData$Year ) ){
          theData$dr_N[ i ] <- theData$dr_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$dr_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$dr_N[ i - 1 ] + theData$dr_Impounded[ i - 1 ] ) ) ) - theData$dr_Impounded[ i - 1 ] - theData$dr_removed[ i - 1 ] + 100
          theData$dr_Impounded[ i ] <- theData$dr_N[ i ] * parameters$q
          if( theData$dr_removed[ i ] >= theData$dr_N[ i ] * 0.3 ){
            theData$dr_removed[ i ] <- theData$dr_N[ i ] * 0.3
          }
        }
        theData$dn_removed <- 0
        theData$sq_removed <- 0
      }
      
      # "Trap-Neuter-Release" scenario
      if( toShow$tnr ){
        theData$tnr_N <- theData$estTotal
        theData$tnr_Impounded <- theData$tnr_N * parameters$q
        theData$tnr_TNRed <- c( rep( x = 0, times = 17 ), rep( x = parameters$TNRq, times = ( length( theData$Year ) - 17 ) ) )
        theData$tnr_altN <- 0
        for( h in 2:17 ){          
          theData$tnr_N[ h ] <- theData$tnr_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$tnr_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * theData$tnr_N[ h - 1 ] ) ) - theData$tnr_Impounded[ h - 1 ]
          theData$tnr_Impounded[ h ] <- theData$tnr_N[ h ] * parameters$q
          theData$tnr_altN[ h ] <- theData$tnr_altN[ h - 1 ] * exp( -parameters$d0 - ( parameters$c * ( theData$tnr_N[ h - 1 ] + theData$tnr_altN[ h - 1 ] ) ) ) + theData$tnr_TNRed[ h - 1 ]
        }
        for( i in 18:length( theData$Year ) ){
          theData$tnr_N[ i ] <- theData$tnr_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$tnr_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$tnr_N[ i - 1 ] + theData$tnr_Impounded[ i - 1 ] ) ) ) - theData$tnr_Impounded[ i - 1 ] - theData$tnr_TNRed[ i - 1 ] + 100
          theData$tnr_Impounded[ i ] <- theData$tnr_N[ i ] * parameters$q
          if( theData$tnr_TNRed[ i ] >= theData$tnr_N[ i ] * 0.3 ){
            theData$tnr_TNRed[ i ] <- theData$tnr_N[ i ] * 0.3
          }
          theData$tnr_altN[ i ] <- theData$tnr_altN[ i - 1 ] * exp( -parameters$d0 - ( parameters$c * ( theData$tnr_N[ i - 1 ] + theData$tnr_altN[ i - 1 ] ) ) ) + theData$tnr_TNRed[ i - 1 ]
        }
        theData$dn_TNRed <- 0
        theData$dn_altN <- 0
        theData$sq_TNRed <- 0
        theData$sq_altN <- 0
      }
      
      if( toShow$tnr & toShow$dr ){
        theData$dr_TNRed <- 0
        theData$dr_altN <- 0
        theData$tnr_removed <- 0
      }
      
      theData <- theData %>% gather( key = Scenario_Variable, value = Value, -c( Year, fIndex, estTotal ) ) %>% 
        separate( col = Scenario_Variable, into = c( "Scenario", "Variable" ), sep = "_" ) %>% 
        spread( key = Variable, value = Value ) %>% 
        arrange( Scenario, Year )

      theData$Scenario <- as.factor( theData$Scenario )
      levels( theData$Scenario )[ levels( theData$Scenario ) == "dn" ] <- "Do nothing"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "sq" ] <- "Status quo"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "dr" ] <- "Destructive removal"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "tnr" ] <- "TNR"
      
      theData[ 1:10, names( theData ) %in% c( "altN", "N", "removed", "Impounded" ) ] <- floor( theData[ 1:10, names( theData ) %in% c( "altN", "N", "removed", "Impounded" ) ] )
      
      theData
   } )

    output$popPlot <- renderPlot( {
      ggplot( data = catFacts(), 
              aes( x = Year, 
                   y = N, 
                   colour = Scenario ) ) + 
        geom_line( size = 1.5, lineend = "round" ) + 
        theme_classic() + 
        theme( axis.title = element_text( size = 16 ), 
               axis.text = element_text( size = 14 ), 
               legend.title = element_text( size = 16 ), 
               legend.text = element_text( size = 14 ), 
               legend.title.align = 0.5 ) +
        scale_x_continuous( name = "Year", 
                            breaks = pretty( x = catFacts()$Year, n = 20 ) ) + 
        scale_y_continuous( name = "Population size", 
                            breaks = pretty( x = catFacts()$N, n = 6 ), 
                            labels = scales::comma ) + 
        guides( size = FALSE )
    } )
    
  }
)
