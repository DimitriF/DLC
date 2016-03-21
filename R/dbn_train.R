##' Training a Deep Belief Net
##'
##' Training a Deep Belief Net
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
##' @author Xiao Rong
##' @export
dbn.train <- function(x,hidden=c(10,10),
                      numepochs=3,batchsize=100,
                      learningrate=0.8,learningrate_scale=1,momentum=0.5,
                      visible_type="bin",hidden_type="bin",cd=1){
  if (!is.matrix(x))
    stop("x must be a matrix!")
  input_dim <- ncol(x)
  dbn <- list(
    size = c(input_dim, hidden)
  )
  train_x <- x
  message("training layer 1 rbm ...")
  dbn$rbm[[1]] <- rbm.train(train_x,hidden[1],
                            numepochs=numepochs,batchsize=batchsize,
                            learningrate=learningrate,learningrate_scale=learningrate_scale,
                            momentum=momentum,
                            visible_type=visible_type,hidden_type=hidden_type,cd=cd)

  if(length(dbn$size) > 2){
    for(i in 2:(length(dbn$size) - 1)){
      train_x <- rbm.up(dbn$rbm[[i-1]], train_x)
      message(sprintf("training layer %d rbm ...",i))
      dbn$rbm[[i]] <- rbm.train(train_x,hidden[i],
                                numepochs=numepochs,batchsize=batchsize,
                                learningrate=learningrate,learningrate_scale=learningrate_scale,
                                momentum=momentum,
                                visible_type=visible_type,hidden_type=hidden_type,cd=cd)
    }
  }
  dbn
}

##' Predict visible unit from top hidden layer
##'
##' Predict visible unit from top hidden layer
##' @param dbn model
##' @param h state of hidden unit
##' @param round number of time to cross the network at each layer
##' @author Xiao Rong
##' @export

dbn.down <- function(dbn,h,round=10){
  hi <- h
  i <- length(dbn$size) - 1 #top rbm
  for(j in 1:round){
    vi <- rbm.down(dbn$rbm[[i]],hi)
    hi <- rbm.up(dbn$rbm[[i]],vi)
  }
  if(length(dbn$size) > 2){
    hi <- vi
    for(i in (length(dbn$size) - 2):1){
      vi <- rbm.down(dbn$rbm[[i]],hi)
      hi <- vi
    }
  }
  vi
}

##' Predict top layer hidden unit from visible units
##'
##' Predict top layer hidden unit from visible units
##' @param dbn model
##' @param v matrix of x values for examples
##' @param round number of time to cross the network at each layer
##' @author Xiao Rong
##' @export

dbn.up <- function(dbn,v,round=10){
  vi <- v
  i <- 1
  for(j in 1:round){
    hi <- rbm.up(dbn$rbm[[i]],vi)
    vi <- rbm.down(dbn$rbm[[i]],hi)
  }
  if(length(dbn$size) > 2){
    vi <- hi
    for(i in (2:length(dbn$size) - 1)){
      for(j in 1:round){
        hi <- rbm.up(dbn$rbm[[i]],vi)
        vi <- rbm.down(dbn$rbm[[i]],hi)
      }
      vi <- hi
    }
  }
  hi
}
