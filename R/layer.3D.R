##' function to plot a matrix
##' @param data a matrix
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
##' layer.3D(data[,,1])
##' @author Dimitri Fichou
##' @export

layer.3D <- function(data){
  persp3d(y = seq(ncol(data)),
          x = seq(nrow(data)),
          z=data,xlab="X",ylab="Y",zlab="intensity",
          col='grey',back="lines",aspect=T)
}

