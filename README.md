title: "DLC: Deep Layer Chromatography"

===========

Artificial neural network for planar chromatographic image evaluation for denoising and feature extraction

To see it running:

http://shinyapps.ernaehrung.uni-giessen.de/tlc_denoising

To install locally:

Install R
https://www.r-project.org/

In the console, install the package with those commands
```r
install.packages('devtools')
devtools::install_github('DimitriF/DLC')
```

Then, run this command to launch the application
```r
DLC::run.tlc_denoising()
```

To use it from the console:
```r
library(DLC)
## get the path of the image, change it with your own
appDir <- system.file("shinyapps", "tlc_denoising", package = "DLC")
pict_path = paste0(appDir,"/www/bioassay-1.jpg")
## read the file
data = f.read.image(pict_path,  height=256)
## check the image
str(data) ## should be an array with 3 dimensions
## plot it
par(mar = c(0,0,2,0),mfrow = c(3,2),xaxt="n",yaxt="n",xaxs="i",yaxs="i",oma=c(0,0,2,0)) ## graphic parameter, use ?par for help
raster(data,main="Original chromatograms") 

## deconstruct the data ## use ?deconstruct.convol for help
decon = deconstruct.convol(data,margin = 3,transform = F,conv_width = 2) 

## train the model ## use ?rbm.train for help
model <- rbm.train(decon,hidden = 4,numepochs = 10,batchsize = 1000,learningrate = 0.1,momentum = 0.5,cd = 2,verbose = T)

## cross the mode both way
up = rbm.up(model, decon)
down = rbm.down(model,up)

## reconstruct ## use ?reconstruct.convol for help
recon = reconstruct.convol(down,margin=3,transform = F,dimension = dim(data),conv_width = 2)
str(recon)
raster(recon,main="Reconstruct chromatograms")

## extract features ## use ?reconstruct for help
features = reconstruct(up,margin=3,transform = F,dimension = dim(data))
str(features)
for(i in seq(4)){
  raster(features[,,i],main=paste0("Extracted chromatograms ",i))
}

## add the title
mtext("Denoising and feature extraction of chromatograms",outer=T)
```

Note that the package contain other shiny applications and functions, they must be considered as experimental though.
