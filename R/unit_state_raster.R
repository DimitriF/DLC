##' function to plot the image of a given hidden unit state for pixel studies
##'
##' @param data a vector of length the number of hidden unit in the network
##' @param model the model used
##' @param conv conv used for deconstruction
##' @param normalize should the windows be normalized
##' @param main should the hidden state be plotted
##' @author Dimitri Fichou
##' @export

unit_state_raster <- function(data,model,conv,main=F,normalize=F){
  #here data is a hidden layer state
  data.save<-data
  data <- rbm.down(model,matrix(data,ncol=length(data),nrow=1))
  vec1 <- c(0);for(i in seq(conv)){vec1 <- c(vec1,-i,i)};for(i in seq(length(vec1))){vec1 <- c(vec1,rep(vec1[i],conv*2))}
  vec2 <- rep(0,conv*2+1);for(i in rep(seq(conv),conv*2+1)){vec2 <- c(vec2,-i,i)};
  mat.shift<-cbind(vec1,vec2)
  pixel <- array(NA,c(conv*2+1,conv*2+1,3))
  for(j in seq(nrow(mat.shift))){
    pixel[mat.shift[j,1]+conv+1,mat.shift[j,2]+conv+1,] <- data[1:3]
    data <- data[-c(1:3)]
  }
  if(normalize==T){
    pixel <- pixel %>% normalize
  }
  if(main == T){
    pixel %>% raster(main=paste0(data.save,collapse="-"))
  }else{
    pixel %>% raster
  }
}

##' function to plot the image of a given hidden unit state for pixel studies
##'
##' @param vec a vector of length the number of hidden unit in the network
##' @param conv conv used for deconstruction
##' @param normalize should the windows be normalized
##' @author Dimitri Fichou
##' @export

vec2patch <- function(vec,conv,normalize=F){
  vec1 <- c(0);for(i in seq(conv)){vec1 <- c(vec1,-i,i)};for(i in seq(length(vec1))){vec1 <- c(vec1,rep(vec1[i],conv*2))}
  vec2 <- rep(0,conv*2+1);for(i in rep(seq(conv),conv*2+1)){vec2 <- c(vec2,-i,i)};
  mat.shift<-cbind(vec1,vec2)
  pixel <- array(NA,c(conv*2+1,conv*2+1,3))
  for(j in seq(nrow(mat.shift))){
    pixel[mat.shift[j,1]+conv+1,mat.shift[j,2]+conv+1,] <- vec[1:3]
    vec <- vec[-c(1:3)]
  }
  if(normalize==T){
    pixel <- pixel %>% normalize
  }
  return(pixel)
}
