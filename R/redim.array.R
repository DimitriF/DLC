##' Function to redimension a 3D array
##'
##' @param data 3D array
##' @param height.out number of pixel in height (first dimension of the 3D array)
##' @param width.out number of pixel in width, optionnal, if not specified will be infered from the height and array dimension (second dimension of the 3D array)
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F)
##' data <- data %>% redim.array(256)
##' data %>% raster()
##' @author Dimitri Fichou
##' @export

redim.array = function (data, height.out, width.out = NULL)
{
  height.in = dim(data)[1]
  width.in = dim(data)[2]
  if(is.null(width.out)){
    width.out = ceiling(height.out/height.in * width.in)
  }
  if (length(dim(data)) == 3) {
    data <- data[floor(seq(1, height.in, length.out = height.out)), , ]
    data <- data[,floor(seq(1, width.in, length.out = width.out)),]
  }
  else {
    data <- data[floor(seq(1, height.in, length.out = height.out)),
                 floor(seq(1, width.in, length.out = width.out))]
  }
  return(data)
}
