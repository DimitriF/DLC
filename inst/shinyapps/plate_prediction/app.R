# plate_prediction
#
options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(DLC)
source('plate_prediction.R')

ui <- fluidPage(
  # tags$head(tags$style(HTML(".box {height: 90vh; overflow-y: auto;}"))),
  tags$head(tags$style(HTML(".col-sm-6 {border-style: solid;border-color: black; border-width:1px;}"))), #background: Lavender;
  tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}")),
  tags$head(tags$style(type="text/css", ".btn {border-radius: 20px; font-size: 30px;}")),
  plate_predictionUI('plate_prediction')
)

server <- function(input,output,session){
  plate_prediction <- callModule(plate_predictionServer,id='plate_prediction')
}

shinyApp(ui,server)
