# tlc_RBM_classification
#

library(shiny)
library(DLC)
source('Supervised_systematic_propolis.R')

ui <- fluidPage(
  tags$head(tags$style(HTML(".box {height: 90vh; overflow-y: auto;}"))),
  tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}")),
  tags$head(tags$style(type="text/css", ".btn {border-radius: 20px; font-size: 30px;}")),
  Supervised_systematic_propolisUI('Supervised_systematic_propolis')
)

server <- function(input,output,session){
  Supervised_systematic_propolis <- callModule(Supervised_systematic_propolisServer,id='Supervised_systematic_propolis')
}

shinyApp(ui,server)
