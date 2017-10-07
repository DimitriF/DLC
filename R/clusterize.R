##' function to clusterize the code of the model
##'
##' @param data a data frame or a matrix
##' @param margin the margin used for deconstruction
##' @param transform was transform used for deconstruction
##' @param dimension the dimension of the original array
##' @author Dimitri Fichou
##' @export

clusterize <- function(data,margin,transform,dimension){
  if(transform == T){
    if(margin == 1){ # Working but give unreconstructable df, so data.recon = data.df.after
      data <- aaply(data,1,function(x){dim(x) <-c(dimension[2],dimension[3]);return(x)},.expand=T)
    }
    if(margin == 2){ # Working but give unreconstructable df, so data.recon = data.df.after
      data <- aaply(data,1,function(x){dim(x) <-c(dimension[1],dimension[3]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,1,3))
    }
    if(margin == 3){ # Working but give unreconstructable df, so data.recon = data.df.after
      data <- aaply(data,1,function(x){dim(x) <-c(dimension[1],dimension[2]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,3,1))
    }
  }else{
    if(margin == 1){ # Working
      data <- aaply(data,2,function(x){dim(x) <-c(dimension[2],dimension[3]);return(x)},.expand=T)
    }
    if(margin == 2){ #
      data <- aaply(data,2,function(x){dim(x) <-c(dimension[1],dimension[3]);return(x)},.expand=T)
      data <- data %>% aperm(c(2,1,3))
    }
    if(margin == 3){ # Problem here, nothing to see
      data <- data
    }
  }
}
