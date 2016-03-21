##' function to deconstruct an array,
##' @param data a data frame or a matrix
##' @param margin the margin used for deconstruction
##' @param transform was transform used for deconstruction
##' @param conv_width number of pixels to take around, 0 mean take only 1
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',native=F,format = 'jpeg',height=128)
##' print(dim(data))
##' decon <- data %>% deconstruct.convol(margin =3 ,transform = F, conv_width =2 )
##' print(dim(decon))
##' @author Dimitri Fichou
##' @export

deconstruct.convol <- function(data,margin,transform,conv_width=0){
  ls <- list(data)
  if(conv_width!=0){
    if(transform == T){
      ## must convolve in 1 dim only, this is the margin
      for(i in seq(conv_width)){ # convolv in the first dim, i.e. margin
        if(margin==1){
          ls[[length(ls)+1]] <- ls[[1]][c((i+1):dim(ls[[1]])[1],rep(dim(ls[[1]])[1],i)),,]
          ls[[length(ls)+1]] <- ls[[1]][c(rep(1,i),1:(dim(ls[[1]])[1]-i)),,]
        }
        if(margin==2){
          ls[[length(ls)+1]] <- ls[[1]][,c((i+1):dim(ls[[1]])[2],rep(dim(ls[[1]])[2],i)),]
          ls[[length(ls)+1]] <- ls[[1]][,c(rep(1,i),1:(dim(ls[[1]])[2]-i)),]
        }
        if(margin==3){
          ls[[length(ls)+1]] <- ls[[1]][,,c((i+1):dim(ls[[1]])[3],rep(dim(ls[[1]])[3],i))]
          ls[[length(ls)+1]] <- ls[[1]][,,c(rep(1,i),1:(dim(ls[[1]])[3]-i))]
        }
      }
    }else{
      ## must convolve around and not in 1 dim only so if margin = 1, first conv in 2, then conv in 3,
      ##useless anyway, may be an option to just convolv in 2 which could make sens here
      for(i in seq(conv_width)){ # convolv in the first dim, i.e. margin+1
        if(margin==1){
          ls[[length(ls)+1]] <- ls[[1]][,c((i+1):dim(ls[[1]])[2],rep(dim(ls[[1]])[2],i)),]
          ls[[length(ls)+1]] <- ls[[1]][,c(rep(1,i),1:(dim(ls[[1]])[2]-i)),]
        }
        if(margin==2){
          ls[[length(ls)+1]] <- ls[[1]][,,c((i+1):dim(ls[[1]])[3],rep(dim(ls[[1]])[3],i))]
          ls[[length(ls)+1]] <- ls[[1]][,,c(rep(1,i),1:(dim(ls[[1]])[3]-i))]
        }
        if(margin==3){
          ls[[length(ls)+1]] <- ls[[1]][c((i+1):dim(ls[[1]])[1],rep(dim(ls[[1]])[1],i)),,]
          ls[[length(ls)+1]] <- ls[[1]][c(rep(1,i),1:(dim(ls[[1]])[1]-i)),,]
        }
      }
      for(j in seq(length(ls))){
        for(i in seq(conv_width)){ # convolv in the second dim, i.e. margin-1
          if(margin==1){
            ls[[length(ls)+1]] <- ls[[j]][,,c((i+1):dim(ls[[j]])[3],rep(dim(ls[[j]])[3],i))]
            ls[[length(ls)+1]] <- ls[[j]][,,c(rep(1,i),1:(dim(ls[[j]])[3]-i))]
          }
          if(margin==2){
            ls[[length(ls)+1]] <- ls[[j]][c((i+1):dim(ls[[j]])[1],rep(dim(ls[[j]])[1],i)),,]
            ls[[length(ls)+1]] <- ls[[j]][c(rep(1,i),1:(dim(ls[[j]])[1]-i)),,]
          }
          if(margin==3){
            ls[[length(ls)+1]] <- ls[[j]][,c((i+1):dim(ls[[j]])[2],rep(dim(ls[[j]])[2],i)),]
            ls[[length(ls)+1]] <- ls[[j]][,c(rep(1,i),1:(dim(ls[[j]])[2]-i)),]
          }
        }
      }
    }
  }
  decon <- lapply(ls,deconstruct,margin,transform) %>% base:::do.call('cbind',.)
  return(decon)
}
## Apply deconstruction
# margin <- 3
# transform = F
# decon <- data %>% deconstruct.convol(margin,transform,0)


## script to plot a 3*3 images if conv_width=3, need to choose the center
# par(mfrow=c(3,3),oma=c(0,0,4,0),mar=c(0,0,4,0),xaxt='n',yaxt='n')
# center <- 12120
# for(i in c(center-256-2,center-2,center+256-2,center-256,center,center+256,center-256+2,center+2,center+256+2)){
#   machin <- truc[i,]
#   bidule <- array(data=rep(NA,3*7*7),dim=c(7,7,3))
#   bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[4,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[4,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[4,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[4,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[4,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[4,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[5,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[3,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[6,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[2,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[7,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   # bidule[4,4,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,5,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,3,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,6,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,2,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,7,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule[1,1,] <- machin[1:3];machin<-machin[-(1:3)]
#   bidule %>% SOM.cluster.plot.picture()
# }
# title(main='Convolution around pixel\nmargin = 3, transform = F, conv_width=3',outer = T)
## Victory



