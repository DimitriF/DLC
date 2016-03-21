##' Training a Stacked Autoencoder
##'
##' Training a Stacked Autoencoder
##' @param x matrix of x values for examples
##' @param hidden vector for number of units of hidden layers.Default is c(10).
##' @param activationfun activation function of hidden unit.Can be "sigm","linear" or "tanh".Default is "sigm" for logistic function
##' @param learningrate learning rate for gradient descent. Default is 0.8.
##' @param momentum momentum for gradient descent. Default is 0.5 .
##' @param learningrate_scale  learning rate will be mutiplied by this scale after every iteration. Default is 1 .
##' @param numepochs number of iteration for samples  Default is 3.
##' @param batchsize size of mini-batch. Default is 100.
##' @param output function of output unit, can be "sigm","linear" or "softmax". Default is "sigm".
##' @param hidden_dropout drop out fraction for hidden layer. Default is 0.
##' @param visible_dropout drop out fraction for input layer Default is 0.
##' @author Xiao Rong
##' @export
sae.train <- function(x,hidden=c(10),
                      activationfun="sigm",
                      learningrate=0.8,
                      momentum=0.5,
                      learningrate_scale=1,
                      output="sigm",
                      numepochs=3,batchsize=100,
                      hidden_dropout=0,visible_dropout=0.2
                      ){
  if (!is.matrix(x))
    stop("x must be a matrix!")
  input_dim <- ncol(x)
  size <- c(input_dim, hidden)
  sae <- list(
    input_dim = input_dim,
    hidden = hidden,
    size = size
  )
  train_x <- x
  message(sprintf("training layer 1 autoencoder ..."))
  sae$encoder[[1]] <-  nn.train(train_x,train_x,hidden=c(hidden[1]),
                                      activationfun=activationfun,
                                      learningrate=learningrate,
                                      momentum=momentum,
                                      learningrate_scale=learningrate_scale,
                                      output=output,
                                      numepochs=numepochs,batchsize=batchsize,
                                      hidden_dropout=hidden_dropout,visible_dropout=visible_dropout)

  if(length(sae$size) > 2){
    for(i in 2:(length(sae$size) - 1)){
      pre <- t( sae$encoder[[i-1]]$W[[1]] %*% t(train_x) + sae$encoder[[i-1]]$B[[i-1]] )
      if(sae$encoder[[i-1]]$activationfun == "sigm"){
        post <- sigm( pre )
      }else if(sae$encoder[[i-1]]$activationfun == "tanh"){
        post <- tanh(pre)
      }else{
        stop("unsupport activation function 'nn$activationfun'");
      }
      train_x <- post
      message(sprintf("training layer %d autoencoder ...",i))
      sae$encoder[[i]] <- nn.train(train_x,train_x,hidden=c(hidden[i]),
                                   activationfun=activationfun,
                                   learningrate=learningrate,
                                   momentum=momentum,
                                   learningrate_scale=learningrate_scale,
                                   output=output,
                                   numepochs=numepochs,batchsize=batchsize,
                                   hidden_dropout=hidden_dropout,visible_dropout=visible_dropout)
    }
  }
  sae
}


