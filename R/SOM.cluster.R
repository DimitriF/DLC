##' Function to apply a pipeline of deconstruction, learning, reconstruction on a picture
##'
##' @param data a 3D array, create by the function f.read.image
##' @param margin the margin to apply the deconstruction
##' @param transform should the array be transformed after deconstruction (interesting value are: margin=3-transform=F and margin=2-transform=T)
##' @param grid.x x width of the kohonen map
##' @param grid.y y width of the kohonen map
##' @param topo topo of the kohonen map
##' @param toroidal should the map be toroidal
##' @param rlen number of iteration
##' @param alpha.1 learning rate begining
##' @param alpha.2 learning rate end
##' @param radius radius of the neighbourhood
##' @param action what the process should return for the next step, choices are: reconstruct, evolve, clusterize, original, original_noirci.
##' @param count.successif.1 for original_noirci what is the minimum of successive sample to be consider as sample and note noise or interband
##' @param count.successif.2 for original_noirci what is the maximum of successive sample to be consider as sample and note noise or interband
##' @param use use to skip a process
##' @examples
##' data <- f.read.image('www/rTLC_demopicture.JPG',format='jpeg',native=F) %>% redim.array(256)
##' model <- SOM.cluster(data,margin=c(3),transform=F,action='evolve')
##' str(model)
##' model$data.recon[[1]][,,1] %>% raster
##' @author Dimitri Fichou
##' @export

SOM.cluster <- function(data,
                        margin,
                        transform = c(F),
                        grid.x = c(3),
                        grid.y = c(3),
                        topo = c('hexagonal'),
                        toroidal = c(F),
                        rlen=c(100),
                        alpha.1 = c(0.05),
                        alpha.2 = c(0.01),
                        radius = c('Default'),
                        action = c('reconstruct'),
                        count.successif.1 = rep(0,length(margin)),
                        count.successif.2 = rep(100,length(margin)),
                        use=length(margin)){

  ## Store the 5 steps datasets + 6 pour stocker evolve, reconstruct et clusterize df.after and data.recon.blablabla
  print('Store the 5 steps datasets')
  data.original <- list()
  data.df.before <- list()
  model <- list()
  data.df.after <- list()
  data.df.after.recon <- list()
  data.df.after.evolve <- list()
  data.df.after.clusterize <- list()
  data.recon <- list()
  data.recon.recon <- list()
  data.recon.evolve <- list()
  data.recon.clusterize <- list()
  ## Store dimensions
  print('Store dimensions')
  dim.df.before <- list()
  dim.df.after <- list()
  dim.df.after.recon <- list()
  dim.df.after.evolve <- list()
  dim.df.after.clusterize <- list()
  dim.original <- list()
  dim.recon <- list()
  ## Initialise
    data.original[[1]] <- data
  for(index in seq(use)){
    if(index !=1){
      data.original[[index]] <- data.recon[[index-1]]
    }
      # print(seq(length(margin)))
      ## Deconstruction
      print(paste0('Deconstruction ',index))
      dim.original[[index]] <- dim(data.original[[index]]) # dim
      print(dim(data.original[[index]]))
      data.df.before[[index]] <- deconstruct(data.original[[index]],margin[index],transform[index])
      dim.df.before[[index]] <- dim(data.df.before[[index]])
      print(dim(data.df.before[[index]]))

      ## Model
      print(paste0('Model ',index))
      grid <- somgrid(as.numeric(grid.x[index]),as.numeric(grid.y[index]),topo[index])
      if(topo[index] == 'hexagonal'){grid$n.hood <- 'circular'}else{grid$n.hood <- 'square'}
      if(radius[index] == 'Default'){
        rad = quantile(unit.distances(grid,toroidal = toroidal[index]), 0.67) * c(1, -1)
      }else{
          rad=as.numeric(radius[index])
          }
      model[[index]] <- kohonen::som(data.df.before[[index]],
                            somgrid(as.numeric(grid.x[index]),as.numeric(grid.y[index]),topo[index]),
                            toroidal = toroidal[index],
                            rlen = rlen[index],
                            alpha = c(alpha.1[index],alpha.2[index]),
                            radius = rad
                            )

      ## data.df.after
      print(paste0('data.df.after ',index))
      data.df.after.recon[[index]] <- model[[index]]$codes[model[[index]]$unit.classif,]
      mapping <- map.kohonen(list(codes = model[[index]]$codes), newdata = data.df.before[[index]])
      data.df.after.evolve[[index]] <- mapping$overall.distances %>% apply(MARGIN=1,function(x){x/sum(x)}) %>% t
      data.df.after.clusterize[[index]] <- model[[index]]$codes
      print("print(dim(data.df.after.evolve[[index]]))")
      dim.df.after.recon[[index]] <- dim(data.df.after.recon[[index]]) # dim
      print(dim(data.df.after.recon[[index]]))
      dim.df.after.evolve[[index]] <- dim(data.df.after.evolve[[index]]) # dim
      print('print(dim(data.df.after.evolve[[index]]))')
      print(dim(data.df.after.evolve[[index]]))
      dim.df.after.clusterize[[index]] <- dim(data.df.after.clusterize[[index]]) # dim
      print('print(dim(data.df.after.clusterize[[index]]))')
      print(dim(data.df.after.clusterize[[index]]))
      ## Reconstruction
      print(paste0('Reconstruction ',index))
      data.recon.recon[[index]] <- reconstruct(data.df.after.recon[[index]],margin[index],transform[index],dim.original[[index]])
      print('data.recon.recon done')

      print(paste0('Evolution ',index))
      data.recon.evolve[[index]] <- evolve(data.df.after.evolve[[index]],margin[index],transform[index],dim.original[[index]])
      print('data.recon.evolve done')

      print(paste0('Clusterize ',index))
      data.recon.clusterize[[index]] <- clusterize(data.df.after.clusterize[[index]],margin[index],transform[index],dim.original[[index]])
      print('data.recon.clusterize done')

      if(action[[index]] == 'reconstruct'){
        data.recon[[index]] <- data.recon.recon[[index]]
        data.df.after[[index]] <- data.df.after.recon[[index]]
      }
      if(action[[index]] == 'evolve'){
        data.recon[[index]]<- data.recon.evolve[[index]]
        data.df.after[[index]] <- data.df.after.evolve[[index]]
      }
      if(action[[index]] == 'clusterize'){
        data.recon[[index]]<- data.recon.clusterize[[index]]
        data.df.after[[index]] <- data.df.after.clusterize[[index]]
      }
      if(action[[index]] == 'original'){
        data.recon[[index]]<- data.original[[index]]
        data.df.after[[index]] <- data.df.before[[index]]
      }
      if(action[[index]] == 'original_subset'){
        truc <- f.count.successif(model[[index]]$unit.classif)
        truc <- truc > count.successif.1[index] & truc < count.successif.2[index]
        data.recon[[index]]<- data.original[[index]][,truc,]
        data.df.after[[index]] <- data.df.before[[index]]
      }
      if(action[[index]] == 'original_noirci'){
        truc <- f.count.successif(model[[index]]$unit.classif)
        truc <- truc > count.successif.1[index] & truc < count.successif.2[index]
        data.recon[[index]]<- data.original[[index]]
        data.recon[[index]][,!truc,] <- 0
        data.df.after[[index]] <- data.df.before[[index]]
      }
      if(action[[index]] == 'original_mean'){
        truc <- f.count.successif(model[[index]]$unit.classif)
        truc <- truc[truc > count.successif.1[index] & truc < count.successif.2[index]]
        data.recon[[index]]<- data.original[[index]]
        data.recon[[index]][,!truc,] <- 0
        data.df.after[[index]] <- data.df.before[[index]]
      }
      dim.df.after[[index]] <- dim(data.df.after[[index]]) # dim
      print(dim(data.df.after[[index]]))
      dim.recon[[index]] <- dim(data.recon[[index]]) # dim
      print(dim(data.recon[[index]]))
      print('data.recon done')
    }

  return(list(data.original = data.original,
              data.recon = data.recon,
              data.original = data.original,
              data.df.before = data.df.before,
              data.recon.clusterize = data.recon.clusterize,
              data.recon.evolve = data.recon.evolve,
              data.recon.recon = data.recon.recon,
              model = model,
              data.df.after =data.df.after,
              margin=margin,
              grid.x=grid.x,
              grid.y=grid.y,
              topo=topo,
              action=action,
              transform = transform,
              dim.df.before=dim.df.before,
              dim.df.after.recon=dim.df.after.recon,
              dim.df.after.evolve=dim.df.after.evolve,
              dim.df.after.clusterize=dim.df.after.clusterize,
              dim.df.after=dim.df.after,
              dim.recon=dim.recon,
              dim.original=dim.original))
}



