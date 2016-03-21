##' function to reconstruct an array from a df by evolving one of the dimension
##'
##' @param data a data frame or a matrix
##' @param margin the margin used for deconstruction
##' @param transform was transform used for deconstruction
##' @param dimension the dimension of the original array
##' @author Dimitri Fichou
##' @export

## function to reconstruct an aray from a df,
## giving the df, margin, transform option and original dimensions
## The only interest found here yet is to evolve the number of layer of the array
## Also, only work with transform == F,
## because the only way to reconstruct the array is to take one line at a time and not binding them,
## which is what transform doing
evolve <- function(data,margin,transform,dimension){
  if(transform == T){
    if(margin == 1){ # Working but give unreconstructable df, so data.recon = data.df.after
      data <- data
    }
    if(margin == 2){ # Working but give unreconstructable df, so data.recon = data.df.after
      data <- data
    }
    if(margin == 3){ # Working but give unreconstructable df, so data.recon = data.df.after
      data <- data
    }
  }else{
    if(margin == 1){ ## Working
      data <- aaply(data,2,function(x){dim(x)<-c(dimension[2],dimension[3]);return(x)},.expand=T)
    }
    if(margin == 2){ # Working
      data <- aaply(data,2,function(x){dim(x)<-c(dimension[1],dimension[3]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,1,3))
    }
    if(margin == 3){ # Working
      data <- aaply(data,2,function(x){dim(x)<-c(dimension[1],dimension[2]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,3,1))
    }
  }
  return(data)
}

