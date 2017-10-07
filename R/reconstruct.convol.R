##' function to reconstruct an array from a matrix,
##' @param data a matrix
##' @param margin the margin used for deconstruction
##' @param transform was transform used for deconstruction
##' @param dimension dimension of the original array
##' @param conv_width number of pixels to take around, 0 mean take only 1
##' @param take_center in case of convolution, should the principal pixel only be taken for reconstruction or should each array be shifted and the total mean computed
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
##' decon <- data %>% deconstruct.convol(margin =3 ,transform = F, conv_width =2 )
##' recon <- decon %>% reconstruct.convol(margin =3 ,transform = F,dimension=dim(data), conv_width =2)
##' @author Dimitri Fichou
##' @export

reconstruct.convol <- function(data,margin,transform,dimension, conv_width = 0, take_center = F,return.ls=F){
  if(conv_width == 0 | take_center == T){ ## call reconstruct normal
    return(reconstruct(data[,1:3],margin,transform,dimension))
  }else{
    ## resplit the df into list according to conv_width,
    split_number <- conv_width*2+1
    if(transform == F){
      split_number <- split_number*(conv_width*2+1)
    }
    ### create a matrix defining each shift, will depend of defolding options
    if(transform == T){
      vec <- c(0);for(i in seq(conv_width)){vec <- c(vec,-i,i)}
      mat.shift <- cbind(rep(margin,split_number),vec)
    }else{
      vec <- c(0);for(i in seq(conv_width)){vec <- c(vec,-i,i)};for(i in seq(length(vec))){vec <- c(vec,rep(vec[i],conv_width*2))}
      if(margin == 1){mar<-2};if(margin==2){mar<-3};if(margin==3){mar<-1}
      mat.shift <- cbind(rep(mar,length(vec)),vec)
    }

    data <- lapply(seq(split_number), function(col){data[,((col-1)*dimension[3]+1):((col-1)*dimension[3]+dimension[3])]})
    ## lapply reconstruct
    data <- lapply(data,reconstruct,margin,transform,dimension)
    ### apply the shift for the first dimension (and last if transform ==T)
    for(i in seq(nrow(mat.shift))){
      data[[i]] <- shift.array(data[[i]],mat.shift[i,1],mat.shift[i,2])
    }
    if(transform == F){
      vec <- rep(0,conv_width*2+1);for(i in rep(seq(conv_width),conv_width*2+1)){vec <- c(vec,-i,i)}
      if(margin == 1){mar<-3};if(margin==2){mar<-1};if(margin==3){mar<-2}
      mat.shift <- cbind(rep(mar,length(vec)),vec)
      for(i in seq(nrow(mat.shift))){
        data[[i]] <- shift.array(data[[i]],mat.shift[i,1],mat.shift[i,2])
      }
    }
    ## mean each array of the list to get the final
    if(return.ls == F){
      lapply(data,c,recursive=T) %>% do.call('rbind',.) %>% colMeans %>% array(dimension) %>% return
    }else{
      data %>% return
    }
  }
}
