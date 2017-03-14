##' function to count the number of successif occurence in a vector
##'
##' @param vec a vector
##' @author Dimitri Fichou
##' @export

f.count.successif <- function(vec){
  start <- 1
  count <- 0
  store <- rep(1,length(vec))
  for(i in seq(length(vec))){
    if(vec[i] == vec[start]){
      count <- count+1
      store[start:i] <- count
    }else{
      start <- i
      count <- 1
    }
  }
  return(store)
}

#
# ## In progress
# f.do.original.mean <- function(vec,data){
#   start <- 1
#   count <- 0
# #   store <- rep(1,length(vec))
#   for(i in seq(length(vec))){
#     if(vec[i] == vec[start]){
#       count <- count+1
#       # store[start:i] <- count
#     }else{
#       data[,start:(i-1),] <- apply(data[,start:(i-1),],c(1),function(x){colMeans(x)})
#       start <- i
#       count <- 1
#     }
#   }
#   data[,start:dim(data)[2],] <- apply(data[,start:dim(data)[2],],c(2),mean)
#   return(data)
# }
