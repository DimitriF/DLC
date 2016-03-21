##' Function to shift a 3D array
##'
##' @param data 3D array
##' @param margin direction to shift
##' @param shift number to shift
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F)
##' data <- data %>% redim.array(256)
##' data %>% raster()
##' @author Dimitri Fichou
##' @export

shift.array <- function(data,margin,shift){
  if(shift == 0){return(data)}
  if(shift < 0){
    if(margin == 1){
      return(data[c(rep(1,abs(shift)),1:(dim(data)[1]-abs(shift))),,])
    }
    if(margin == 2){
      return(data[,c(rep(1,abs(shift)),1:(dim(data)[2]-abs(shift))),])
    }
    if(margin == 3){
      return(data[,,c(rep(1,abs(shift)),1:(dim(data)[3]-abs(shift)))])
    }
  }else{
    if(margin == 1){
      return(data[c((abs(shift)+1):dim(data)[1],rep(dim(data)[1],abs(shift))),,])
    }
    if(margin == 2){
      return(data[,c((abs(shift)+1):dim(data)[2],rep(dim(data)[2],abs(shift))),])
    }
    if(margin == 3){
      return(data[,,c((abs(shift)+1):dim(data)[3],rep(dim(data)[3],abs(shift)))])
    }
  }
}
