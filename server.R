library( shiny )
library( dplyr )
library( tidyr )
library( ggplot2 )

feralIndex <- read.csv( "data/feral_index.csv" )
  
shinyServer( 
  function( input, output, session ){
    catFacts <- reactive( {
      
      toShow <- list( dn = FALSE, 
                      rf = FALSE, 
                      dr = FALSE, 
                      tnr = FALSE, 
                      ce = FALSE )
      
      for( k in names( toShow ) ){
        if( k %in% input$toShow ){
          toShow[ which( names( toShow ) == k ) ] <- TRUE
        }
      }
      
      parameters <- list( a = input$a, 
                          b0 = input$b0, 
                          q = input$q, 
                          sdb = input$a * input$q, 
                          sdbBase = 0.00015 * input$q, 
                          c = 0, 
                          d0 = 0.22, 
                          TNRq = input$TNRq, 
                          rq = input$rq )
      
      theData <- data.frame( Year = min( feralIndex$Year ):( max( feralIndex$Year ) + 50 ), 
                             fIndex = c( feralIndex$Feral_Index, 
                                         rep( x = NA, times = 50 ) ) )
      theData$estTotal <- theData$fIndex / parameters$q

      # "Do Nothing" scenario
      if( toShow$dn ){
        theData$dn_N <- theData$estTotal
        theData$dn_Impounded <- theData$dn_N * parameters$q
        for( h in 2:17 ){
          theData$dn_N[ h ] <- theData$dn_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$dn_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$dn_N[ h - 1 ] + theData$dn_Impounded[ h - 1 ] ) ) ) - theData$dn_Impounded[ h - 1 ]
          theData$dn_Impounded[ h ] <- theData$dn_N[ h ] * parameters$q
        }
        for( i in 18:length( theData$Year ) ){
          theData$dn_N[ i ] <- theData$dn_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$dn_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * ( theData$dn_N[ i - 1 ] + theData$dn_Impounded[ i - 1 ] ) ) )
          theData$dn_Impounded[ i ] <- theData$dn_N[ i ] * parameters$q
        }
      }
      
      # "Reduced Feeding" scenario
      if( toShow$rf ){
        theData$rf_N <- theData$estTotal
        theData$rf_Impounded <- theData$rf_N * parameters$q
        for( h in 2:17 ){
          theData$rf_N[ h ] <- theData$rf_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$rf_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * theData$rf_N[ h - 1 ] ) ) - theData$rf_Impounded[ h - 1 ]
          theData$rf_Impounded[ h ] <- theData$rf_N[ h ] * parameters$q
        }
        for( i in 18:length( theData$Year ) ){
          theData$rf_N[ i ] <- theData$rf_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdb * theData$rf_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * theData$rf_N[ i - 1 ] ) ) - theData$rf_Impounded[ i - 1 ]
          theData$rf_Impounded[ i ] <- theData$rf_N[ i ] * parameters$q
        }
      }

      # "Destructive Removal" scenario
      if( toShow$dr ){
        theData$dr_N <- theData$estTotal
        theData$dr_Impounded <- theData$dr_N * parameters$q
        theData$dr_removed <- c( rep( x = 0, times = 17 ), rep( x = parameters$rq, times = ( length( theData$Year ) - 17 ) ) )
        for( h in 2:17 ){          
          theData$dr_N[ h ] <- theData$dr_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$dr_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * theData$dr_N[ h - 1 ] ) ) - theData$dr_Impounded[ h - 1 ] - theData$dr_removed[ h - 1 ]
          theData$dr_Impounded[ h ] <- theData$dr_N[ h ] * parameters$q
        }
        for( i in 18:length( theData$Year ) ){ 
          theData$dr_N[ i ] <- theData$dr_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$dr_N[ i - 1 ] ) - parameters$d0 - ( parameters$c * theData$dr_N[ i - 1 ] ) ) - theData$dr_Impounded[ i - 1 ] - theData$dr_removed[ i - 1 ]
          theData$dr_Impounded[ i ] <- theData$dr_N[ i ] * parameters$q
          if( theData$dr_removed[ i ] >= theData$dr_N[ i ] * 0.3 ){
            theData$dr_removed[ i ] <- theData$dr_N[ i ] * 0.3
          }
        }
      }
      
      # "Trap-Neuter-Release" scenario
      if( toShow$tnr ){
        theData$tnr_N <- theData$estTotal
        theData$tnr_Impounded <- theData$tnr_N * parameters$q
        theData$tnr_TNRed <- c( rep( x = 0, times = 17 ), rep( x = parameters$TNRq, times = ( length( theData$Year ) - 17 ) ) )
        altN <- vector( mode = "numeric", length = length( theData$ Year ) )
        for( h in 2:17 ){          
          theData$tnr_N[ h ] <- theData$tnr_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$tnr_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * theData$tnr_N[ h - 1 ] ) ) - theData$tnr_Impounded[ h - 1 ]
          theData$tnr_Impounded[ h ] <- theData$tnr_N[ h ] * parameters$q
          altN[ h ] <- altN[ h - 1 ] * exp( -parameters$d0 - ( parameters$c * ( theData$tnr_N[ h - 1 ] + altN[ h - 1 ] ) ) ) + theData$tnr_TNRed[ h - 1 ]
        }
        for( i in 18:length( theData$Year ) ){
          theData$tnr_N[ i ] <- theData$tnr_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * ( theData$tnr_N[ i - 1 ] + altN[ i - 1 ] ) ) - parameters$d0 - ( parameters$c * ( theData$tnr_N[ i - 1 ] + altN[ i - 1 ] ) ) ) - theData$tnr_TNRed[ i - 1 ]
          theData$tnr_Impounded[ i ] <- theData$tnr_N[ i ] * parameters$q
          if( theData$tnr_TNRed[ i ] >= theData$tnr_N[ i ] * 0.3 ){
            theData$tnr_TNRed[ i ] <- theData$tnr_N[ i ] * 0.3
          }
          altN[ i ] <- altN[ i - 1 ] * exp( -parameters$d0 - ( parameters$c * ( theData$tnr_N[ i - 1 ] + altN[ i - 1 ] ) ) ) + theData$tnr_TNRed[ i - 1 ]
        }
        theData$tnr_N <- theData$tnr_N + altN
      }

      # "Combined Effects" scenario
      if( toShow$ce ){
        theData$ce_N <- theData$estTotal
        theData$ce_Impounded <- theData$ce_N * parameters$q
        theData$ce_removed <- c( rep( x = 0, times = 17 ), rep( x = parameters$rq, times = ( length( theData$Year ) - 17 ) ) )
        theData$ce_TNRed <- c( rep( x = 0, times = 17 ), rep( x = parameters$TNRq, times = ( length( theData$Year ) - 17 ) ) )
        altN <- vector( mode = "numeric", length = length( theData$Year ) )
        for( h in 2:17 ){ 
          theData$ce_N[ h ] <- theData$ce_N[ h - 1 ] * exp( parameters$b0 - ( parameters$sdbBase * theData$ce_N[ h - 1 ] ) - parameters$d0 - ( parameters$c * theData$ce_N[ h - 1 ] ) ) - theData$ce_Impounded[ h - 1 ]
          theData$ce_Impounded[ h ] <- theData$ce_N[ h ] * parameters$q
          altN[ h ] <- altN[ h - 1 ] * exp( -parameters$d0 - ( parameters$c * ( theData$ce_N[ h - 1 ] + altN[ h - 1 ] ) ) ) + theData$ce_TNRed[ h - 1 ]
        }
        for( i in 18:length( theData$Year ) ){
          theData$ce_N[ i ] <- theData$ce_N[ i - 1 ] * exp( parameters$b0 - ( parameters$sdb * ( theData$ce_N[ i - 1 ] + altN[ i - 1 ] ) ) - parameters$d0 - ( parameters$c * ( theData$ce_N[ i - 1 ] + altN[ i - 1 ] ) ) ) - theData$ce_TNRed[ i - 1 ] - theData$ce_removed[ i - 1 ]
          theData$ce_Impounded[ i ] <- theData$ce_N[ i ] * parameters$q
          if( theData$ce_TNRed[ i ] >= theData$ce_N[ i ] * 0.3 ){
            theData$ce_TNRed[ i ] <- theData$ce_N[ i ] * 0.3
          }
          if( theData$ce_removed[ i ] >= theData$ce_N[ i ] * 0.3 ){
            theData$ce_removed[ i ] <- theData$ce_N[ i ] * 0.3
          }
          altN[ i ] <- altN[ i - 1 ] * exp( -parameters$d0 - ( parameters$c * ( theData$ce_N[ i - 1 ] + altN[ i - 1 ] ) ) ) + theData$ce_TNRed[ i - 1 ]
        }
        theData$ce_N <- theData$ce_N + altN
      }
      
            
      theData <- theData %>% gather( key = Scenario_Variable, value = Value, -c( Year, fIndex, estTotal ) ) %>% 
        separate( col = Scenario_Variable, into = c( "Scenario", "Variable" ), sep = "_" ) %>% 
        spread( key = Variable, value = Value ) %>% 
        arrange( Scenario, Year )

      theData$Scenario <- as.factor( theData$Scenario )
      levels( theData$Scenario )[ levels( theData$Scenario ) == "dn" ] <- "Do nothing"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "rf" ] <- "Reduced feeding"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "dr" ] <- "Destructive removal"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "tnr" ] <- "TNR"
      levels( theData$Scenario )[ levels( theData$Scenario ) == "ce" ] <- "Combined effects"
      
      theData[ 1:10, names( theData ) %in% c( "N", "removed", "Impounded" ) ] <- floor( theData[ 1:10, names( theData ) %in% c( "N", "removed", "Impounded" ) ] )
      
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
    
    output$ratePlot <- renderPlot( { 
      
      parameters <- data.frame( a = input$a, 
                                b0 = input$b0, 
                                q = input$q, 
                                sdb = input$a * input$q, 
                                sdbBase = 0.00015 * input$q, 
                                c = 0, 
                                d0 = 0.22, 
                                TNRq = input$TNRq, 
                                rq = input$rq )
      
      ddf <- data.frame( N = 0:(1.2 * max( catFacts()$N ) ), 
                         TNR = parameters$TNRq, 
                         R <- parameters$rq, 
                         d = parameters$d0 )

## My attempt to make a rate for effective death rate (removal from breeding population)
#       for( i in 1:length( ddf$N ) ){
#         if( ddf$TNR[ i ] >= ddf$N[ i ] * 0.3 ){
#           ddf$TNR[ i ] <- ddf$N[ i ] * 0.3
#         }
#         if( ddf$R[ i ] >= ddf$N[ i ] * 0.3 ){
#           ddf$R[ i ] <- ddf$N[ i ] * 0.3
#         }
#         ddf$d[ i ] <- parameters$d0 + ( ( ddf$TNR[ i ] + ddf$R[ i ] ) / ddf$N[ i ] )
#       }
      
      ggplot( data = catFacts(), 
              aes( x = N ) ) + 
        theme_classic() + 
        theme( axis.title = element_text( size = 16 ), 
               axis.text = element_text( size = 14 ) ) + 
        scale_x_continuous( name = "Population size", 
                            breaks = pretty( seq( 0, 1.2 * max( catFacts()$N ), 5000 ), n = 12 ), 
                            limits = c( 0, 1.2 * max( catFacts()$N ) ), 
                            labels = scales::comma, 
                            expand = c( 0, 0 ) ) + 
        scale_y_continuous( name = "Per-capita birth and death rates", 
                            breaks = pretty( seq( 0, 1.2 * parameters$b0, 0.01 ), n = 5 ), 
                            limits = c( 0, 1.2 * parameters$b0 ), 
                            expand = c( 0, 0 ) ) + 
        geom_abline( slope = -0.00015 * parameters$q, 
                     intercept = parameters$b0, 
                     size = 1, 
                     colour = "dodgerblue" ) + 
        geom_abline( slope = -parameters$a * parameters$q, 
                     intercept = parameters$b0, 
                     size = 1, 
                     colour = "mediumseagreen" ) + 
        geom_abline( slope = parameters$c, 
                     intercept = parameters$d0, 
                     size = 1, 
                     colour = "tomato2" ) + 
        geom_segment( x = ( parameters$d0 - parameters$b0 ) / ( parameters$c - parameters$sdb ), 
                      y = ( ( parameters$sdb * ( parameters$b0 - parameters$d0 ) ) / ( parameters$c - parameters$sdb ) ) + parameters$b0, 
                      xend = ( parameters$d0 - parameters$b0 ) / ( parameters$c - parameters$sdb ), 
                      yend = 0, 
                      linetype = "dashed" )# + 
#         geom_line( data = ddf, 
#                    aes( x = N, 
#                         y = d ), 
#                    colour = "orange" )
    } )
    
  }
)
