##' function to plot several figures, for publication
##' @param data list of image that will be redimension in height and bind in the second dimension
##' @author Dimitri Fichou
##' @export

raster_figure <- function(data,lab = NULL,...){
  height = min(unlist(lapply(data,function(x){dim(x)[1]})))
  data = lapply(data,redim.array,height)
  widths = unlist(lapply(data,function(x){dim(x)[2]}))
  data.bind = abind(data,along=2)

  plot(c(0,dim(data.bind)[2]),c(0,dim(data.bind)[1]), type='n',ylab="",xlab="",...)
  rasterImage(data.bind,0,0,dim(data.bind)[2],height)

  if(!is.null(lab)){
    if(length(lab) != length(data)){
      print("Not the good number of labels")
      return(NULL)
    }else{
      text(y=rep(height*0.95,length(lab),
                 x=c(0,widths[1:(length(lab)-1)]),
                 labels=lab,pos=4
                 ))
    }
  }

}
