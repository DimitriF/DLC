
##' Modification of the map function of the kohonen package in order to return the distance
##'
##' @param x x param
##' @author Dimitri Fichou
##' @export
##'

"map" <- function(x, ...)
{
  UseMethod("map")
}


##' Modification of the map.kohonen function of the kohonen package in order to return the distance
##'
##' @param x x param
##' @param newdata newdata param
##' @param whatmap whatmap param
##' @param weights weights param
##' @param scale.distances scale distance param
##' @author Dimitri Fichou
##' @export
##'

"map.kohonen" <- function(x, newdata, whatmap=NULL, weights,
                          scale.distances = (nmaps > 1), ...)
{
  codes <- x$codes
  if (is.matrix(codes)) codes <- list(codes)

  if (missing(newdata) & !is.null(x$data))
    newdata <- x$data
  if (is.matrix(newdata)) newdata <- list(newdata)

  if (is.null(whatmap) && !is.null(x$whatmap)) {
    whatmap <- x$whatmap
  } else {
    whatmap <- 1
  }

  nmaps <- length(whatmap)

  nd <- nrow(newdata[[1]])
  ncodes <- nrow(codes[[ whatmap[1] ]])

  distances <- matrix(0, nd*ncodes, nmaps)
  ## first calculate all distance matrices: every column contains the
  ## distances of the objects to all unit in that particular layer.
  for (i in seq(along = whatmap)) {
    np <- ncol(codes[[ whatmap[i] ]])
    dt <- newdata[[ whatmap[i] ]]
    if (is.factor(dt))
      dt <- classvec2classmat(dt)
    cd <- codes[[ whatmap[i] ]]

    distances[,i] <- .C("mapKohonen",
                        as.double(dt),
                        as.double(cd),
                        as.integer(ncodes),
                        as.integer(nd),
                        as.integer(np),
                        dists = double(nd * ncodes),
                        NAOK = TRUE,
                        PACKAGE = "kohonen")$dists
  }

  if (scale.distances)
    distances <- sweep(distances, 2,
                       apply(distances, 2, max, na.rm=TRUE), FUN="/")

  ## next determine overall closest
  if (nmaps > 1) {
    if (missing(weights)) {
      weights <- x$weights[whatmap] / sum(x$weights[whatmap])
    } else {
      if (abs(sum(weights)) < .Machine$double.eps) {
        warning("sum of weights equals zero! Unscaled weights are used...")
      } else {
        weights <- weights / sum(weights)
      }
    }
    overall.distances <- matrix(rowMeans(sweep(distances, 2, weights,
                                               FUN="*"),
                                         na.rm=TRUE),
                                nd, ncodes, byrow=TRUE)

    ## The next bit seems simple but stumbles on NAs
    ##    overall.distances <-
    ##      matrix(distances %*% weights, nd, ncodes, byrow=TRUE)
  } else {
    weights <- 1
    overall.distances <- matrix(distances, nd, ncodes, byrow=TRUE)
  }

  NArows <- which(apply(overall.distances, 1, function(x) all(is.na(x))))
  if (length(NArows) > 0) {
    classif <- mindists <- rep(NA, nrow(overall.distances))
    mindists[-NArows] <- apply(overall.distances[-NArows,], 1, min, na.rm=TRUE)
    classif[-NArows] <- apply(overall.distances[-NArows,],
                              1,
                              function(x)
                                which(x == min(x, na.rm=TRUE))[1])
  } else {
    mindists <- apply(overall.distances, 1, min, na.rm=TRUE)
    classif <- apply(overall.distances,
                     1,
                     function(x)
                       which(x == min(x, na.rm=TRUE))[1])
  }

  list(unit.classif = classif,
       distances = mindists,
       whatmap = whatmap,
       weights = weights,
       scale.distances = scale.distances,
       overall.distances = overall.distances)
}
