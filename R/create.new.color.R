##' Function to calculate the euclidean distance of each pixel to one selected pixel
##'
##' @param data a 3D array, basically create by the function f.read.image
##' @param x the x coordonate of the picture (x is the second dimension)
##' @param y the y coordonate of the picture (y is the first dimension and start from the top, dont ask why)
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F) %>% redim.array(256)
##' par(mfrow=c(2,1),oma=c(0,0,0,0),mar=c(0,0,1,0),xaxt='n',yaxt='n')
##' x=180;y=70;
##' data %>% raster(main='original')
##' text(x=x,y=y,labels='X',col='red')
##' data %>% create.new.color(x,y) %>% normalize %>% raster(main='New color')
##' text(x=x,y=y,labels='X',col='red')
##' @author Dimitri Fichou
##' @export


create.new.color <- function(data,x,y,func){
  # c('Euclidean distance','1 - Euclidean distance','Sigmoid Euclidean distance')
  pixel <- data[dim(data)[1]-y,x,]
  if(func == 'Euclidean distance'){
    data <- apply(data,c(1,2),function(x){sqrt(sum((c(x[1]-pixel[1],x[2]-pixel[2],x[3]-pixel[3]))^2))})
  }
  if(func == '1 - Euclidean distance'){
    data <- 1 - apply(data,c(1,2),function(x){sqrt(sum((c(x[1]-pixel[1],x[2]-pixel[2],x[3]-pixel[3]))^2))})
  }
  if(func == 'Sigmoid Euclidean distance'){
    data <- apply(data,c(1,2),function(x){sqrt(sum((c(x[1]-pixel[1],x[2]-pixel[2],x[3]-pixel[3]))^2))}) %>% sigm
  }
  return(data)
}



