##' Deprecated. Function to read multiple image file
##'
##' @param source the path of the file
##' @param native should the data be return natively or as a 3D array
##' @param height redimension the file
##' @param ls.format use to return a list instead of an array
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F,height=256)
##' data %>% raster()
##' @author Dimitri Fichou
##' @export
f.read.image.multiple<-function(source,native=F,height,Normalize=F,ls.format=F){
  ls <- list()
  for(i in source){
    try(data<-readTIFF(i,native=native))
    try(data<-readJPEG(source=i,native=native))
    try(data<-readPNG(source=i,native=native))
    data <- redim.array(data,height)
    if(Normalize == T){data <- data %>% normalize}
    ls[[i]]<- data
  }
  if(ls.format == F){
    data <- abind(ls,along=2)
  }else{
    data <- ls
  }
  return(data)
}
