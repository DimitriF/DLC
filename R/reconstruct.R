##' function to reconstruct an array from a matrix,
##' @param data a matrix
##' @param margin the margin used for deconstruction
##' @param transform was transform used for deconstruction
##' @param dimension dimension of the original array
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',native=F,format = 'jpeg',height=128)
##' decon <- data %>% deconstruct.convol(margin =3 ,transform = F, conv_width =2 )
##' recon <- decon %>% reconstruct(margin =3 ,transform = F, dim(data))
##' @author Dimitri Fichou
##' @export

reconstruct <- function(data,margin,transform,dimension){
  if(transform == T){
    if(margin == 1){ # Working
      data <- aaply(data,1,function(x){dim(x)<-c(dimension[2],dimension[3]);return(x)},.expand=T)
    }
    if(margin == 2){ # Working
      data <- aaply(data,1,function(x){dim(x) <- c(dimension[1],dimension[3]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,1,3))
    }
    if(margin == 3){ # Working
      data <- aaply(data,1,function(x){dim(x) <- c(dimension[1],dimension[2]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,3,1))
    }
  }else{
    if(margin == 1){ ## Working
      data <- aaply(data,2,function(x){dim(x)<-c(dimension[2],dimension[3]);return(x)},.expand=T)
    }
    if(margin == 2){ # Working
      data <- aaply(data,2,function(x){dim(x)<-c(dimension[1],dimension[3]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,1,3))
    }
    if(margin == 3){ # Working
      data <- aaply(data,2,function(x){dim(x)<-c(dimension[1],dimension[2]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,3,1))
    }
  }
  return(data)
}
