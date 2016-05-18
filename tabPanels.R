## Create tabpanel objects here
## they should be named tP<i> where <i> is the
## index of the order they are created.

tP1 <- fluidPage( # start "Do Nothing" fluidpage
  
  sliderInput( inputId = "a", 
               label = "Per-capita decrease in birth rate:", 
               min = 0, 
               max = 0.002, 
               value = 0.00015, 
               ticks = FALSE, 
               sep = "" ), 
  
  sliderInput( inputId = "b0", 
               label = "Per-capita birth rate at N = 0:", 
               min = 0, 
               max = 10, 
               value = 0.5, 
               ticks = TRUE ), 
  
  sliderInput( inputId = "q", 
               label = "Proportion of actual population trapped in index:", 
               min = 0, 
               max = 0.5, 
               value = 0.07, 
               ticks = TRUE, 
               sep = "" ), 
  
  plotOutput( outputId = "linePlot" )
  
) # end "Do Nothing" fluidpage


tP2 <- fluidPage( # start "Status Quo" fluidpage
  
) # end "Status Quo" fluidpage


tP3 <- fluidPage( # start "Base TNR" fluidpage
  
) # end "Base TNR" fluidpage


tP4 <- fluidPage( # start "TNR Alone" fluidpage
  
) # end "TNR Alone" fluidpage


tP5 <- fluidPage( # start "Destructive Removal" fluidpage
  
) # end "Destructive Removal" fluidpage


tP6 <- fluidPage( # start "Reduced Feeding" fluidpage
  
) # end "Reduced Feeding" fluidpage


tP7 <- fluidPage( # start "Freestyle" fluidpage
  
) # end "Freestyle" fluidpage

