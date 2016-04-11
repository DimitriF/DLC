##' function to extract chromato, corret baseline, integrate peak
##' @param data an array
##' @param dim.table a matrix with a row per peak to extract/integrate, as column respectively xmin, xmax, ymin, ymax
##' @param correct.baseline use or not the rolling ball algorithm to correct the baseline
##' @param negatif apply a transformation to put the data in negatif, needed for white light pictures and for some extracted featuress from the network
##' @param ... Arguments to be passed to methods
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
##' dim.table <- rbind(c(1,2,50,60,100,110),c(1,2,70,80,100,110))
##' f.integrate(data,dim.table,F,T)
##' @author Dimitri Fichou
##' @export

# data <- f.read.image("inst/extdata/rTLC_demopicture.JPG",256)
# dim.table <- rbind(c(1,2,50,60,100,110),c(1,2,70,80,100,110))
f.integrate <- function(data,dim.table,negatif=F,correct.baseline=F){
  if(negatif==T){data <- 1-data}
  stock <- array(NA,dim=c(nrow(dim.table),256,dim(data)[3]))
  for(i in seq(nrow(dim.table))){
    for(j in seq(dim(data)[3])){
      stock[i,,j] <- apply(data[,dim.table[i,1]:dim.table[i,2],j],1,sum)
    }
  }
  if(correct.baseline==T){
    for(i in seq(dim(data)[3])){
      stock[,,i] <- stock[,,i]%>% baseline(spectra = .,method = "rollingBall",wm=50,ws=5) %>% getCorrected()
    }
  }
  final <- array(NA,dim=c(nrow(dim.table),dim(data)[3]))
  for(i in seq(nrow(dim.table))){
    for(j in seq(dim(data)[3])){
      final[i,j] <- sum(stock[i,dim.table[i,3]:dim.table[i,4],j])
    }
  }
  return(final)
}



