##' Function to launch the rtlc app
##'
##' @param port if not NULL will deploy on the local network on this port, just look for the IP of the host and go for example on 192.168.1.20:port to access the app from anywhere in the network
##' @author Dimitri Fichou
##' @examples
##' run.rtlc()
##' @export
##'

run.rtlc <- function(port=NULL) {
  appDir <- system.file("shinyapps", "rtlc", package = "DLC")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  if(is.null(port)){
    shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
  }else{
    shiny::runApp(appDir, display.mode = "normal",
                  launch.browser = T,host = '0.0.0.0',port=port)
  }
}
