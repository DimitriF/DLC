##' Function to normalize array or matrix
##'
##' @param data a 3D array or a matrix
##' @param hidden option to normalize in other dimension than the dimension 3
##' @param ref optionnal picture to keep the max and min values
##' @author Dimitri Fichou
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F) %>% redim.array(256)
##' data %>% normalize %>% raster
##' @export
##'

normalize <- function(data,hidden=F,ref=NULL){
  if(hidden == F){
    if(length(dim(data)) ==2){
      if(!is.null(ref)){
        data <- data - min(data);data <- data/(max(data)/(max(ref)-min(ref)))+min(ref)
      }else{
        data <- data - min(data);data <- data/max(data)
      }
    }else{
      for(i in seq(dim(data)[3])){
        if(!is.null(ref)){
          data[,,i] <- data[,,i] - min(data[,,i]);data[,,i] <- data[,,i]/(max(data[,,i])/(max(ref[,,i])-min(ref[,,i])))+min(ref[,,i])
        }else{
          data[,,i] <- data[,,i] - min(data[,,i]);data[,,i] <- data[,,i]/max(data[,,i])
        }
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

