##' Function for data augmentation, go inside a list and append the new pictures
##'
##' @param data list of pictures return by f.read.image with options ls.format = T
##' @param shift vector of shift values. c(0) means no shifting.
##' @param gamma vector of gamma values, c(1) means no attenuation. normalization will be applied after in order to stay in the 0-1 range (note that normalization should be applied anyway for efficienty)
##' @author Dimitri Fichou
##' @examples
##' data <- f.read.image(paste0('www/propolis-silicate-',seq(6),'.jpg'),height = 126,Normalize = T,ls.format = T)
##' augmented <- augmentation(data,shift=c(-5,5),gamma=c(0.5,2))
##' abind(augmented,along=2) %>% raster()
##' @export
##'
##'

augmentation <- function(data,shift=c(),gamma=c()){
  store <- data
  if(length(shift) != 0){
    for(i in as.numeric(shift)){
      for(j in seq(length(store))){
        data[[length(data)+1]] <- shift.array(store[[j]],1,i)
      }
    }
  }
  store <- data
  if(length(gamma) != 0){
    for(i in as.numeric(gamma)){
      for(j in seq(length(store))){
        data[[length(data)+1]] <- store[[j]]^i %>% normalize
      }
    }
  }
  return(data)
}

# data <- f.read.image(paste0('www/propolis-silicate-',seq(1),'.jpg'),height = 126,Normalize = T,ls.format = T)
# # augmented <- augmentation(data,gamma=c(0.8,2,3,4))
# model <- abind(augmented,along=2) %>%deconstruct(2,T) %>% rbm.train(hidden=16,numepochs=100,learningrate = 0.1,verbose = T)
# model.2 <- abind(augmented,along=2) %>%deconstruct(2,T) %>% rbm.up(model,.) %>% rbm.train(hidden=1,numepochs=100,learningrate = 0.1,verbose = T)
# up <- abind(data,along=2) %>%deconstruct(2,T) %>% rbm.up(model,.)
# up.2 <- rbm.up(model.2,up)
# par(mfrow=c(4,1),mar=c(0,0,0,0),oma=c(0,0,0,0))
# abind(data,along=2) %>% raster()
# up %>% t %>% raster
# up %>% rbm.down(model,.) %>% reconstruct(2,T,dim(abind(data,along=2))) %>% raster
# up.2 %>%t%>% raster


