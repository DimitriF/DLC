##' Function to normalize array or matrix
##'
##' @param data a 3D array or a matrix
##' @param hidden option to normalize in other dimension than the dimension 3
##' @author Dimitri Fichou
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F) %>% redim.array(256)
##' data %>% normalize %>% raster
##' @export
##'

normalize <- function(data,hidden=F){
  if(hidden == F){
    if(length(dim(data)) ==2){
      data <- data - min(data);data <- data/max(data)
    }else{
      for(i in seq(dim(data)[3])){
        data[,,i] <- data[,,i] - min(data[,,i]);data[,,i] <- data[,,i]/max(data[,,i])
      }
    }
  }else{
    if(hidden == 'row'){
      for(i in seq(dim(data)[1])){
        data[i,] <- data[i,] - min(data[i,]);data[i,] <- data[i,]/max(data[i,])
      }
    }else{
      for(i in seq(dim(data)[2])){
        data[,i] <- data[,i] - min(data[,i]);data[,i] <- data[,i]/max(data[,i])
      }
    }

  }
  # print(dim(data))
  return(data)
}

