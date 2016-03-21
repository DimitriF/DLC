##' Function to plot an object return by the SOM.cluster function
##'
##' @param data list return by the SOM.cluster function
##' @param xcoord xcoord of the point to look at
##' @param ycoord ycoord of te point to look at
##' @param index.original index of the original picture in the list
##' @param index.recon index of the reconstruct picture in the list
##' @author Dimitri Fichou
##' @export
##'
SOM.cluster.plot <- function(data,xcoord=NULL,ycoord=NULL,index.original=1,index.recon=1){
  print('Som cluster plot')
  data.recon <- data$data.recon[[index.recon]]
  data.original <- data$data.original[[index.original]]

  main <- ''
  for(index.process in seq(index.recon)){
    margin=data$margin[[index.process]]
    grid.x=data$grid.x[[index.process]]
    grid.y=data$grid.y[[index.process]]
    topo=data$topo[[index.process]]
    action=data$action[[index.process]]
    transform = data$transform[[index.process]]
    dim.df.before=data$dim.df.before[[index.process]]
    dim.df.after=data$dim.df.after[[index.process]]
    dim.recon=data$dim.recon[[index.process]]
    dim.original=data$dim.original[[index.process]]
    main <- paste0(
      main,paste0('Process: ',index.process,
                  '; margin: ',margin,' ; Transform: ',transform,' ; Grid: ',paste(c(grid.x,grid.y,topo),collapse='-'),' ; Action: ',action,
                  '\n; Dim original: ',paste(dim.original,collapse='-'),' ; Dim before: ',paste(dim.df.before,collapse='-'),
                  '; ; Dim after: ',paste(dim.df.after,collapse='-'),'; Dim reconstruction: ',paste(dim.recon,collapse='-'),'\n')
      )
  }

#   if(ycoord > dim(data.original)[1]){
#     stop('ycoord too big, not in the array')
#   }
  print('Som cluster plot Init finish')
  par(mfrow=c(1,1),mar = c(0, 0, 0, 0),oma=c(0,0,2+index.recon*2,0))
  plot(c(-4,1.2),c(-2.3,2.1), type='n',ylab="",xlab="",bty='n',
       main='')
  data.original %>% rasterImage(-4 , 1.1, 0, 2.1,interpolate = F)
  data.recon %>% rasterImage(-4 , 0, 0, 1,interpolate = F)
  print('raster finish')
  ## original xcoord ##
  print('## original xcoord ##')
  if(!is.null(xcoord)){
    simple.array.data.original.x <- array(data.original[,xcoord,c(1,2,3)],dim=c(dim(data.original)[1],1,3))
    simple.array.data.original.x %>% rasterImage(1.1 , 1.1, 1.2, 2.1,interpolate = F)
    par(new=T)
    plot(simple.array.data.original.x[,,1],y=seq(2.1,1.1,length.out=dim(data.original)[1]),type='l',col='red',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(simple.array.data.original.x[,,2],y=seq(2.1,1.1,length.out=dim(data.original)[1]),type='l',col='green',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(simple.array.data.original.x[,,3],y=seq(2.1,1.1,length.out=dim(data.original)[1]),type='l',col='blue',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
  }
  ## recon xcoord ##
  print(' ## recon xcoord ##')
  if(!is.null(xcoord)){
    simple.array.data.recon.x <- array(data.recon[dim(data.recon)[1]:1,xcoord,c(1,2,3)],dim=c(dim(data.recon)[1],1,3)) ## MAC.INVERSE
    array(data.recon[,xcoord,c(1,2,3)],dim=c(dim(data.recon)[1],1,3)) %>% rasterImage(1.1 , 0, 1.2, 1,interpolate = F)
    par(new=T)
    plot(simple.array.data.recon.x[,,1],y=seq(0,1,length.out=dim(data.recon)[1]),type='l',col='red',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(simple.array.data.recon.x[,,2],y=seq(0,1,length.out=dim(data.recon)[1]),type='l',col='green',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(simple.array.data.recon.x[,,3],y=seq(0,1,length.out=dim(data.recon)[1]),type='l',col='blue',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    ## Abline x ##
    abline(v=seq(-4,0,length.out = dim(data.original)[2])[xcoord])
  }
  ## original ycoord ##
  print('## original ycoord ##')
  if(!is.null(ycoord)){
    simple.array.data.original.y <- array(data.original[(dim(data.original)[1]-ycoord),,c(1,2,3)],dim=c(1, dim(data.original)[2],3)) ## MAC INVERSE
    simple.array.data.original.y %>%  rasterImage(-4 , -0.05, 0, -0.1,interpolate = F)
    simple.array.data.original.y <- simple.array.data.original.y -1.2
    par(new=T)
    plot(y=simple.array.data.original.y[,,1],x=seq(-4,0,length.out=dim(data.original)[2]),type='l',col='red',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(y=simple.array.data.original.y[,,2],x=seq(-4,0,length.out=dim(data.original)[2]),type='l',col='green',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(y=simple.array.data.original.y[,,3],x=seq(-4,0,length.out=dim(data.original)[2]),type='l',col='blue',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    ## abline ##
    abline(h=round(seq(1.1,2.1,length.out=dim(data.original)[1])[ycoord],2))
  }
  ## recon ycoord ##
  print('## recon ycoord ##')
  if(!is.null(ycoord)){
    simple.array.data.recon.y <- array(data.recon[(dim(data.recon)[1]-ycoord),,c(1,2,3)],dim=c(1,dim(data.recon)[2],3)) ## MAC INVERSE
    simple.array.data.recon.y %>% rasterImage(-4 , -0.15, 0, -0.2,interpolate = F)
    simple.array.data.recon.y <- simple.array.data.recon.y-2.3
    par(new=T)
    plot(y=simple.array.data.recon.y[,,1],x=seq(-4,0,length.out=dim(data.recon)[2]),type='l',col='red',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(y=simple.array.data.recon.y[,,2],x=seq(-4,0,length.out=dim(data.recon)[2]),type='l',col='green',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    par(new=T)
    plot(y=simple.array.data.recon.y[,,3],x=seq(-4,0,length.out=dim(data.recon)[2]),type='l',col='blue',xlim=c(-4,1.2),ylim=c(-2.3,2.1),ylab="",xlab="")
    ## Abline y ##
    abline(h=round(seq(0,1,length.out=dim(data.recon)[1])[ycoord],2))
  }
  mtext(main, outer = TRUE,cex.main=1)
}
##' Function to plot the evolution of a picture
##'
##' @param data one list return by SOM.sluster
##' @param Normalize if TRUE, the picture will be normalize
##' @param grid vector of length 2 containing the dimension of the grid to plot the pictures
##' @param main title of the plot
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F)
##' data <- data %>% redim.array(256)
##' data %>% raster()
##' @author Dimitri Fichou
##' @export
##'
SOM.cluster.plot.evolve <- function(data,Normalize=F,grid=c(3,3),main=''){
  if(Normalize == T){data$data.recon <- normalize(data$data.recon)}
  par(mfrow=grid,mar=c(0,0,0,0))
  for(i in seq(grid[1]*grid[2])){
    plot(c(0,1),c(0,1), type='n',ylab="",xlab="",axes=F,bty='n',main='')
    rasterImage(as.matrix(data$data.recon[,,i]),0,0,1,1)
  }
  mtext(main, outer = TRUE)
}

##' Function to plot a picture
##'
##' @param data a 3D array
##' @param main title of the picture
##' @author Dimitri Fichou
##' @export
##'
SOM.cluster.plot.picture <- function(data,main=''){ # a 3D array
  # par(mar=c(0,0,2,0),oma=c(0,0,2,0))
  # print('SOM.cluster.plot.picture')
  # print(dim(data))
  plot(c(0,dim(data)[2]),c(0,dim(data)[1]), type='n',ylab="",xlab="",main=paste0('Dimensions: ',paste(dim(data),collapse='-'),'\n',main))
  rasterImage(data,0,0,dim(data)[2],dim(data)[1],interpolate=F)
}
