##' Function to prepare TLC data
##'
##' @param source a vector, path of the images files
##' @param dimension a dataframe, dimension table with colnames: "Plate_width","First_appli_position", "Band_Length", "Distance_between_tracks", "Tolerance"
##' @param Dep a dataframe, dependante variables
##' @param remove_interband boolean, T by default, should the interband be removed
##' @param height, number of vertical pixels to resize in
##' @author Dimitri Fichou
##' @example
##' dimension = read.xlsx("/home/clau/Dropbox/DLC/test/Catechine/TableDimensionCatechine in 1 µl.xls",1)
##' load("~/Dropbox/DLC/test/Catechine/rTLC Catechine.Rdata")
##' Dep = data$batch[,8:12]
##' source = dir("/home/clau/Dropbox/DLC/test/Catechine/",pattern = "254nm",full.names = T)
##' data = TLC_prep(source= Ind,dimension = dimension,Dep = Dep,height=256)
##' @export
TLC_prep<-function(source,dimension,Dep, remove_interband = T,height= 256,conv="ATS"){
  if(conv != "linomat"){ # this put everybody back to linomat convention
    dimension[,2]<-dimension[,2]-dimension[,3]/2
    dimension[,4]<-dimension[,4]-dimension[,3]
  }
  source = f.read.image(source,height = height,ls.format = T)
  Dep.ls = list()
  id_sample = 1
  for(i in seq(nrow(dimension))){
    nbr.band<-round((dimension[i,1]-2*dimension[i,2])/(dimension[i,3]+dimension[i,4]))
    Dep.ls[[i]] = matrix(0,nrow=dim(source[[i]])[2],ncol=ncol(Dep)+3) # 3 for id line, id sample, plate name
    Dep.ls[[i]][,3] = names(source)[i] # plate name
    for(j in 0:(nbr.band-1)){
      indexes = round(c((dim(source[[i]])[2]/dimension[i,1]*((dimension[i,2]+dimension[i,5])+j*(dimension[i,3]+dimension[i,4]))),
                  (dim(source[[i]])[2]/dimension[i,1]*((dimension[i,2]+dimension[i,3]-dimension[i,5])+j*(dimension[i,3]+dimension[i,4])))))
      indexes = indexes[1]:indexes[2]
      for(k in indexes){
        Dep.ls[[i]][k,4:(ncol(Dep)+3)] = unlist(Dep[id_sample,])
        Dep.ls[[i]][k,2] = id_sample
      }
      id_sample = id_sample+1
    }
  }
  source = lapply(source,aperm,c(2,1,3))
  source = abind(source,along=1)
  Dep.ls = abind(Dep.ls,along=1)
  if(remove_interband){
    source = source[Dep.ls[,2] != 0,,]
    Dep.ls = Dep.ls[Dep.ls[,2] != 0,]
    Dep.ls[,1] = seq(nrow(Dep.ls))
  }
  return(list(source,Dep.ls))
}



