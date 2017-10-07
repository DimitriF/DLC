##' Training a RBM(restricted Boltzmann Machine)
##'
##' Training a RBM(restricted Boltzmann Machine)
##' @param x matrix of x values for examples
##' @param hidden number of hidden units
##' @param visible_type activation function of input unit.Only support "sigm" now
##' @param hidden_type activation function of hidden unit.Only support "sigm" now
##' @param learningrate learning rate for gradient descent. Default is 0.8.
##' @param momentum momentum for gradient descent. Default is 0.5 .
##' @param learningrate_scale  learning rate will be mutiplied by this scale after every iteration. Default is 1 .
##' @param numepochs number of iteration for samples  Default is 3.
##' @param batchsize size of mini-batch. Default is 100.
##' @param cd number of iteration for Gibbs sample of CD algorithm.
##' @param verbose print a string for each epoch done
##' @param keep.data keep the weight for each epoch, usefull for DPE
##' @examples
##' Var1 <- c(rep(1,50),rep(0,50))
##' Var2 <- c(rep(0,50),rep(1,50))
##' x3 <- matrix(c(Var1,Var2),nrow=100,ncol=2)
##' r1 <- rbm.train(x3,10,numepochs=20,cd=10)
##' @author Xiao Rong
##' @export
rbm.train <- function(x,hidden,
                      numepochs=3,batchsize=100,
                      learningrate=0.8,learningrate_scale=1,momentum=0.5,
                      visible_type="bin",hidden_type="bin",cd=1,
                      verbose=F,keep.data=F,model=NULL){

  if (!is.matrix(x))
    stop("x must be a matrix!")
  input_dim <- ncol(x)

  if(is.null(model)){
    rbm <- list(
      size = c(input_dim, hidden),
      W = matrix(runif(hidden*input_dim,min=-0.1,max=0.1), c(hidden,input_dim)),
      vW = matrix(rep(0,hidden*input_dim), c(hidden,input_dim)),
      B = runif(input_dim,min=-0.1,max=0.1),
      vB = rep(0,input_dim),
      C = runif(hidden,min=-0.1,max=0.1),
      vC = rep(0,hidden),
      learningrate = learningrate,
      learningrate_scale = learningrate_scale,
      momentum = momentum,
      hidden_type = hidden_type, visible_type = visible_type,
      cd=cd,
      iter_e=c(),
      euclidean=c(),
      iter_euclidean=c()
    )
  }else{
    rbm = model
  }

  if(keep.data == T){
    keep <- list()
  }
  m <- nrow(x);
  numbatches <- m / batchsize;
  s <- 0
  for(i in 1:numepochs){
    randperm <- sample(1:m,m)
    if(numbatches >= 1){
      for(l in 1 : numbatches){
        s <- s + 1
        batch_x <- x[randperm[((l-1)*batchsize+1):(l*batchsize)], ]
        rbm <- do.rbm.train(rbm,batch_x,s)
      }
    }
    #last fraction of sample
    if(numbatches > as.integer(numbatches)){
      batch_x <- x[randperm[(as.integer(numbatches)*batchsize):m], ]
      s <- s + 1
      rbm <- do.rbm.train(rbm,batch_x,s)
    }
    rbm$learningrate <- rbm$learningrate * rbm$learningrate_scale;
    if(keep.data == T){
      keep[[i]] <- rbm
    }
    if(verbose == T){
      message(sprintf("training epoch %d rbm ...",i))
    }
  }
  if(keep.data == T){
    rbm[['keep']] <- keep
  }
  rbm
}


##' do the rbm training, mean to be use inside rbm.train
##' @param rbm an rbm object
##' @param batch_x batch_x
##' @param s s
##' @author Xiao Rong
##' @export

do.rbm.train <- function(rbm,batch_x,s){
  m <- nrow(batch_x)
  v1 <- batch_x
  h1 <- binary.state(rbm.up(rbm, v1))

  vn <- v1
  hn <- h1
  for(i in 1:rbm$cd){
    vn <- rbm.down(rbm, hn)
    hn <- rbm.up(rbm, vn)
    #only last hidden state use probability real value
    if(i < rbm$cd){
      hn <- binary.state(hn);
    }
  }


  dW <- (t(h1) %*% v1 - t(hn)  %*% vn) / m
  dW <- rbm$learningrate * dW
  rbm$vW <- rbm$vW * rbm$momentum + dW
  dw <- rbm$vW
  rbm$W <- rbm$W + dW

  dB <- colMeans(v1 - vn)
  dB <- rbm$learningrate * dB
  rbm$vB <- rbm$vB * rbm$momentum + dB
  dB <- rbm$vB
  rbm$B <- rbm$B + dB

  dC <- colMeans(h1 - hn)
  dC <- rbm$learningrate * dC
  rbm$vC <- rbm$vC * rbm$momentum + dC
  dC <- rbm$vC
  rbm$C <- rbm$C + dC

  rbm$e[s] <- sum((v1 - vn)^2)/m
  rbm$iter_e <- c(rbm$iter_e,rbm$e[s]) # for online learning
  rbm$euclidean[s] <- sqrt(rbm$e[s]*m)
  rbm$iter_euclidean <- c(rbm$iter_euclidean,rbm$euclidean[s]) # for online learning
  rbm
}


##' Infer hidden units state by visible units
##'
##' Infer hidden units states by visible units
##' @param rbm an rbm object trained by function train.rbm
##' @param v visible units states
##' @return hidden units states
##' @examples
##' Var1 <- c(rep(1,50),rep(0,50))
##' Var2 <- c(rep(0,50),rep(1,50))
##' x3 <- matrix(c(Var1,Var2),nrow=100,ncol=2)
##' r1 <- rbm.train(x3,3,numepochs=20,cd=10)
##' v <- c(0.2,0.8)
##' h <- rbm.up(r1,v)
##' @author Xiao Rong
##' @export
rbm.up <- function(rbm,v){
  m <- nrow(v)
  if(rbm$hidden_type == "bin"){
    sum <- t( t(v %*% t(rbm$W)) + rbm$C )
    h <- sigm( sum )
  }else{
    stop("only support binary state for rbm hidden layer!")
  }
  h
}

##' Generate visible vector by hidden units states
##'
##' Generate visible vector by hidden units states
##' @param rbm an rbm object trained by function train.rbm
##' @param h hidden units states
##' @return generated visible vector
##' @examples
##' Var1 <- c(rep(1,50),rep(0,50))
##' Var2 <- c(rep(0,50),rep(1,50))
##' x3 <- matrix(c(Var1,Var2),nrow=100,ncol=2)
##' r1 <- rbm.train(x3,3,numepochs=20,cd=10)
##' h <- c(0.2,0.8,0.1)
##' v <- rbm.down(r1,h)
##' @author Xiao Rong
##' @export
rbm.down <- function(rbm,h){
  m <- nrow(h)
  if(rbm$visible_type == "bin"){
    sum <- t( t(h %*% rbm$W) + rbm$B )
    v <- sigm( sum )
  }else{
    stop("only support binary state for rbm hidden layer!")
  }
  v
}


##' Calculate probability real values
##' @param h hidden units states
##' @author Xiao Rong
##' @export
binary.state <- function(h){
  p <- matrix( runif(length(h),min=0,max=1)
               ,nrow=nrow(h),ncol=ncol(h))
  h[h>p] <- 1
  h[h<=p] <- 0
  h
}

##' function for sigm unit
##' @param x unit value
##' @author Xiao Rong
##' @examples
##' data <- f.read.image(source='www/rTLC_demopicture.JPG',height=128)
##' data %>% sigm %>% raster
##' @export
sigm <- function(x){
  1/(1+exp(-x))
}
