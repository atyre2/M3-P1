## Create tabpanel objects here
## they should be named tP<i> where <i> is the
## index of the order they are created.

tP1 <- fluidPage( # start "Model" fluidpage
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput( inputId = "toShow", 
                          label = NULL, 
                          choices = c( "Do nothing" = "dn", 
                                       "Reduced feeding" = "rf", 
                                       "Destructive removal" = "dr", 
                                       "TNR" = "tnr", 
                                       "Combined effects" = "ce" ), 
                          selected = c( "dn" ), 
                          inline = TRUE ), 
      sliderInput( inputId = "a", 
                   label = "Per-capita decrease in birth rate:", 
                   min = 0, 
                   max = 0.002, 
                   value = 0.00015, 
                   step = 0.00001, 
                   ticks = FALSE, 
                   sep = "" ), 
      sliderInput( inputId = "b0", 
                   label = "Per-capita birth rate at N = 0:", 
                   min = 0, 
                   max = 2, 
                   value = 0.5, 
                   step = 0.01, 
                   ticks = TRUE ), 
      sliderInput( inputId = "q", 
                   label = "Proportion of actual population trapped in index:", 
                   min = 0, 
                   max = 1, 
                   value = 0.07, 
                   step = 0.01, 
                   ticks = TRUE, 
                   sep = "" ), 
      sliderInput( inputId = "TNRq", 
                   label = "Annual quota for trap-neuter-release:", 
                   min = 0, 
                   max = 2000, 
                   value = 200, 
                   step = 1, 
                   ticks = TRUE, 
                   sep = "" ), 
      sliderInput( inputId = "rq", 
                   label = "Annual quota for destructive removal:", 
                   min = 0, 
                   max = 2000, 
                   step = 1, 
                   value = 0, 
                   ticks = TRUE, 
                   sep = "" )
    ),
    mainPanel( 
      plotOutput( outputId = "popPlot" ), 
      plotOutput( outputId = "ratePlot" )
    )
  )
) # end "Model" fluidpage
