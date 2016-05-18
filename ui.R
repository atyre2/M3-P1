library( shiny )
library( markdown )

source( "tabPanels.R", local=TRUE )

shinyUI( 
    navbarPage( "Feral Cat Problem Case", 
    tabPanel( "Introduction", 
             withMathJax(), 
             includeMarkdown( "Introduction.md" ), 
             value = "tP0" ), 
    tabPanel( "Do Nothing", tP1, value = "tP1" ), # end tabPanel "tP1"
    tabPanel( "Status Quo", tP2, value = "tP2" ), # end tabPanel "tP2"
    tabPanel( "Base TNR", tP3, value = "tP3" ), # end tabPanel "tP3"
    tabPanel( "TNR Alone", tP4, value = "tP4" ), # end tabPanel "tP4"
    tabPanel( "Destructive Removal", tP5, value = "tP5" ), # end tabPanel "tP5"
    tabPanel( "Reduced Feeding", tP6, value = "tP6" ), # end tabPanel "tP6"
    tabPanel( "Freestyle", tP7, value = "tP7" ), # end tabPanel "tP7"
    id = "panels", 
    footer = div( br(), 
                 img( src="R-UN_L4c_tag_4c.png" ), 
                 tags$a( href = "http://snr.unl.edu/", "Brought to you by the School of Natural Resources" ) 
                 ) 
  ) # end navbarPage
) # end shinyUI

