##' function to deconstruct an array,
##' @param data a data frame or a matrix
##' @param margin the margin used for deconstruction
##' @param transform was transform used for deconstruction
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',native=F,format = 'jpeg',height=128)
##' decon <- data %>% deconstruct(margin =3 ,transform = F)
##' @author Dimitri Fichou
##' @export

deconstruct <-function(data,margin,transform,conv_width=1){
  if(conv_width==1){
    data <- apply(data,margin,c)
    if(transform == T){
      data <- t(data)
    }
    return(data)
  }else{
    data <- apply(data,margin,c)
    if(transform == T){
      data <- t(data)
    }
    return(data)
  }
}
