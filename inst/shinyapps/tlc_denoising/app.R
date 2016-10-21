# tlc_denoising
#

library(shiny)
library(DLC)
source('tlc_denoising.R')
options(shiny.maxRequestSize=1000*1024^2)

ui <- fluidPage(
  tags$head(tags$style(HTML(".box {height: 90vh; overflow-y: auto;}"))),
  tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}")),
  tags$head(tags$style(type="text/css", ".btn {border-radius: 20px; font-size: 30px;}")),
  tlc_denoisingUI('tlc_denoising')
)

server <- function(input,output,session){
  tlc_denoising <- callModule(tlc_denoisingServer,id='tlc_denoising')
}


shinyApp(ui,server)
