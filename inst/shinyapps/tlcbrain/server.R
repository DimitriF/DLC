#### Preparation ####
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


# require("jpeg");require("png");require('tiff')
# library(abind)
#
# library(shiny)
# library(magrittr)
# library(kohonen)
# library(plyr)
# # library(deepnet)
# library(markdown)
# # library(openssl)
# # library(mongolite)
# # library(jsonlite)
# # library(EBImage)
# library(tsne)
require(DLC)


if(is.vector(Sys.info())){
  if(Sys.info()[4] == "MacBook-Air-de-dimitri.local" |Sys.info()[4] == "lenovo"){
    readonly = F
    height=256
    send_feedback =F
  }else{
    readonly = F
    height=256
    send_feedback =T
  }
}else{
  readonly = F
  height=256
  send_feedback =T
}



options(shiny.maxRequestSize=1000*1024^2)
shinyServer(function(input, output,session) {
  session$onSessionEnded(function() {
    Kohonen.1.Process.2.obs.dblclick$suspend()
    Kohonen.1.Process.2.obs.click$suspend()
    click.New.Col.raster_0.obs$suspend()
    dblclick.RBM.2.network_1.obs$suspend()
    click.RBM.2.network_1.obs$suspend()
    dblclick.RBM.3.network_1.obs$suspend()
    click.RBM.3.network_1.obs$suspend()
    click.InterExtract.raster.1.obs$suspend()
    dblclick.InterExtract.raster.1.obs$suspend()
  })
#### InterExtract ####
  click.InterExtract.raster.1 <- reactiveValues(value=c())
  click.InterExtract.raster.1.obs <- observeEvent(input$click.InterExtract.raster.1,{
    click <- input$click.InterExtract.raster.1
    x <- ceiling(as.numeric(click[1]))
    click.InterExtract.raster.1$value <- c(click.InterExtract.raster.1$value,x)
  })
  dblclick.InterExtract.raster.1.obs <- observeEvent(input$dblclick.InterExtract.raster.1,{
    click.InterExtract.raster.1$value <- c()
  })
  output$InterExtract.raster.1 <- renderPlot({
    data.raw() %>% raster
    abline(v=click.InterExtract.raster.1$value,col='red')
  })
  output$InterExtract.raster.2 <- renderPlot({
    data <- data.raw()
    main = 'CLick on the beginning of the first band'
    if(length(click.InterExtract.raster.1$value) > 0){
      largeur = dim(data)[2]
      dist.gauche = click.InterExtract.raster.1$value[1]
      data[,1:dist.gauche,] <- 0
      main = 'Click on the end of the first band'
      if(length(click.InterExtract.raster.1$value) > 1){
        band = click.InterExtract.raster.1$value[2] - click.InterExtract.raster.1$value[1]
        data[,(dist.gauche+band):largeur,] <- 0
        main = 'CLick on the beginning of the second band'
      }
      if(length(click.InterExtract.raster.1$value) > 2){
        ecart <- 0
        click <- click.InterExtract.raster.1$value[-2]
        print(click)
        click <- click - dist.gauche # only band beginning left
        for(i in (length(click)-1)){
          truc <- click[i+1] - click[i] - band
          ecart <- c(ecart,truc)
          print(click)
        }
        ecart <- ecart %>% mean
        data <- data.raw() # need to start again
        data[,c(1:dist.gauche),] <- 0
        nbr.band<-round((largeur-2*dist.gauche-band)/(band+ecart))
        print(nbr.band)
        print(ecart)
        print(dist.gauche)
        print(band)
        print(largeur)
        for(i in seq(nbr.band-1)){
          data[,(dist.gauche+i*band+(i-1)*ecart):(dist.gauche+i*band+i*ecart),] <-0
        }
        data[,largeur:(largeur-dist.gauche),] <- 0
        main = 'CLick on the beginning of the next band and so on to optimize'
      }
    }
    data %>% raster(main = paste0(main,'\nDouble click to reset'))
  })
#### Algo_table ####

  output$Algo_table <- renderTable({
    read.csv("www/Algo_table.csv",sep="\t",quote="\"")
  })

#### data.raw ####
  data.raw <- reactive({
    withProgress(message = "Work in progress", value=0, {
      if(is.null(input$FilePicture)){
        data <- f.read.image('www/rTLC_demopicture.JPG',height = input$height,Normalize = input$data.raw.normalize)
      }else{
        validate(
          need(input$FilePicture != "", "Please upload a picture")
        )
        data <- f.read.image(input$FilePicture$datapath,height = input$height,Normalize = input$data.raw.normalize)
        if(send_feedback == T){saveData(data)} # inser here to create user collection
      }
    })
    return(data)
  })
  output$Kohonen.1.raster.Process_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(data.raw(),main='click on the picture to see the chromatograms')
  })
  output$Kohonen.1.chromato.Process_0 <- renderPlot({
    validate(
      need(input$click.Kohonen.1.raster.Process_0 != "", "Click on the picture to see the chromatograms")
    )
    data <- data.raw()
    click <- input$click.Kohonen.1.raster.Process_0
    x <- ceiling(as.numeric(click[1]))
    data <- array(data[dim(data)[1]:1,x,c(1,2,3)],dim=c(dim(data)[1],1,3))
    par(mar=c(0,0,0,0))
    plot(c(0,dim(data)[1]),c(0,1.2), type='n',ylab="",xlab="",bty='n', ##input$Kohonen.1.height = 128
         main='')
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,dim(data)[1] ,1.2) ##input$Kohonen.1.height = 128
    color <- c('red','green','blue')
    for(i in seq(3)){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i])
    }
  })
#### Kohonen.1 ####
  output$Kohonen.1.option <- renderTable({
    data <- data.frame(Grid.X = c(5,5),
                       Grid.Y = c(2,2)
    )
    for(i in seq(ncol(data))){
      if(readonly==T){
        data[,i] <- paste0("<input id='Kohonen.1.",colnames(data)[i],".",seq(nrow(data)),"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,i],"'>")
      }else{
        data[,i] <- paste0("<input id='Kohonen.1.",colnames(data)[i],".",seq(nrow(data)),"' class='shiny-bound-input' type='number' value='",data[,i],"'>")
      } #
    }
    rownames(data) <- paste0('Process ',seq(nrow(data)))
    # print(data)
    data
  }, sanitize.text.function = function(y) y)

  Kohonen.1.data.process <- eventReactive(input$Kohonen.1.go,{
    withProgress(message = "Work in progress", value=0, {
      SOM.cluster(data.raw(),
                  margin=c(3,2),
                  transform=c(F,T),
                  grid.x = c(input$Kohonen.1.Grid.X.1,input$Kohonen.1.Grid.X.2),
                  grid.y = c(input$Kohonen.1.Grid.Y.1,input$Kohonen.1.Grid.Y.2),
                  topo =c('hexagonal','hexagonal'),
                  toroidal = c(F,F),
                  rlen=c(100,100),
                  alpha.1 = c(0.05,0.05),
                  alpha.2 = c(0.01,0.01),
                  radius = c('Default','Default'),
                  action = c('evolve','reconstruct'),
                  count.successif.1 = c(0,0),
                  count.successif.2 = c(100,100),
                  use=2)
    })
  })
  output$Kohonen.1.mapping.Process_1.RGB.explosion <- renderPlot({
    validate(
      need(input$Kohonen.1.go, "Click on the Analyze button")
    )
    data <- Kohonen.1.data.process()
    par(mar=c(0,0,4,0))
    plot(data$model[[1]],type='mapping',main='Click on a unit to see the resulting new channel',
         bgcol=apply(data$model[[1]]$codes,1,function(x){rgb(x[1],x[2],x[3])})
         # pch='.'
    )
    #     plot(data$model[[1]],type='mapping',main='',
    #          # bgcol=apply(data$model[[1]]$codes,1,function(x){rgb(x[1],x[2],x[3])}),
    #          pch='.'
    #     )
    #     plot(data$model[[1]],type='mapping',main='')
    #     text(data$model[[1]]$grid$pts,labels=seq(32),cex=2)
  })
  output$Kohonen.1.raster.Process_1.RGB.explosion <- renderPlot({
    index <- which.click.kohonen(grid.x=input$Kohonen.1.Grid.X.1,grid.y=input$Kohonen.1.Grid.Y.1,topo='hexagonal',input$click.Kohonen.1.plot.mapping.Process_1.RGB.explosion$x,input$click.Kohonen.1.plot.mapping.Process_1.RGB.explosion$y)
    data <- Kohonen.1.data.process()$data.recon[[1]][,,index]
    par(mar=c(0,0,4,0))
    plot(c(0,dim(data)[2]),c(0,dim(data)[1]), type='n',ylab="",xlab="",bty='n',
         main='Click on the plot to see the chromatograms')
    data %>% normalize %>% rasterImage(0,0,dim(data)[2],dim(data)[1])
  })
  output$Kohonen.1.chromato.Process_1.RGB.explosion <- renderPlot({
    validate(
      need(input$click.Kohonen.1.raster.Process_1.RGB.explosion != "", "Click on the picture to see the chromatograms")
    )
    index <- which.click.kohonen(grid.x=input$Kohonen.1.Grid.X.1,grid.y=input$Kohonen.1.Grid.Y.1,topo='hexagonal',input$click.Kohonen.1.plot.mapping.Process_1.RGB.explosion$x,input$click.Kohonen.1.plot.mapping.Process_1.RGB.explosion$y)
    par(mfrow=c(2,1),mar=c(0,0,0,0))
    click <- input$click.Kohonen.1.raster.Process_1.RGB.explosion
    x <- ceiling(as.numeric(click[1]))
    data <- Kohonen.1.data.process()$data.recon[[1]][,x,index]
    plot(rev(data),type='l',axes=F)
    data <- data.raw()
    data <- array(data[dim(data)[1]:1,x,c(1,2,3)],dim=c(dim(data)[1],1,3))
    plot(c(0,dim(data)[1]),c(0,1.2), type='n',ylab="",xlab="",bty='n', ## input$Kohonen.1.height = 128
         main='')
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,dim(data)[1],1.2) ##input$Kohonen.1.height=128
    color <- c('red','green','blue')
    for(i in seq(3)){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i])
    }
  })
  output$Kohonen.1.mapping.Process_2.chromato.cluster <- renderPlot({
    data <- Kohonen.1.data.process()
    # par(mfrow=c(1,3))
    plot(data$model[[2]],type='mapping',main='Click on a unit to view the corresponding observations on the pictures bellow\nDouble Click to reset',
         # bgcol=apply(data$model[[1]]$codes,1,function(x){rgb(x[1],x[2],x[3])})
         pch='.'
    )
    add.cluster.boundaries(data$model[[2]],cutree(hclust(dist(data$model[[2]]$codes)), 3),col='red')
    # somgrid(grid.x,grid.y,topo)$pts[Kohonen.1.Process.2.obs.index$number,]
    if(length(Kohonen.1.Process.2.obs.index$number) != 0){
      text(x=data$model[[2]]$grid$pts[Kohonen.1.Process.2.obs.index$number,1],y=data$model[[2]]$grid$pts[Kohonen.1.Process.2.obs.index$number,2],labels='X',col='red',cex=2)
    }
  })
  Kohonen.1.Process.2.obs.click <- observeEvent(input$click.Kohonen.1.mapping.Process_2.chromato.cluster,{
    index <- which.click.kohonen(grid.x=input$Kohonen.1.Grid.X.2,grid.y=input$Kohonen.1.Grid.Y.2,topo='hexagonal',
                                 input$click.Kohonen.1.mapping.Process_2.chromato.cluster$x,
                                 input$click.Kohonen.1.mapping.Process_2.chromato.cluster$y)
    Kohonen.1.Process.2.obs.index$number <- c(Kohonen.1.Process.2.obs.index$number,index)
  })
  Kohonen.1.Process.2.obs.dblclick <- observeEvent(input$dblclick.Kohonen.1.mapping.Process_2.chromato.cluster,{
    Kohonen.1.Process.2.obs.index$number <- c()
  })
  Kohonen.1.Process.2.obs.index <- reactiveValues(number= c())
  output$Kohonen.1.raster.Process_1.original.subset <- renderPlot({
    index <- Kohonen.1.Process.2.obs.index$number
    print(index)
    data <- data.raw()
    data[,! Kohonen.1.data.process()$model[[2]]$unit.classif %in% index,] <- 0
    SOM.cluster.plot.picture(data,main='Subset of the selected cluster')
  })
  output$Kohonen.1.changes.Process_1 <- renderPlot({
    plot(Kohonen.1.data.process()$model[[1]],type='changes')
  })
  output$Kohonen.1.changes.Process_2 <- renderPlot({
    plot(Kohonen.1.data.process()$model[[2]],type='changes')
  })
#### Kohonen.2 ####
  output$Kohonen.2.option <- renderTable({
    data <- data.frame(Grid.X = c(5),
                       Grid.Y = c(2)
    )
    for(i in seq(ncol(data))){
      if(readonly==T){
        data[,i] <- paste0("<input id='Kohonen.2.",colnames(data)[i],".",seq(nrow(data)),"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,i],"'>")
      }else{
        data[,i] <- paste0("<input id='Kohonen.2.",colnames(data)[i],".",seq(nrow(data)),"' class='shiny-bound-input' type='number' value='",data[,i],"'>")
      } #
    }
    rownames(data) <- paste0('Process ',seq(nrow(data)))
    # print(data)
    data
  }, sanitize.text.function = function(y) y)
  output$Kohonen.2.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(data.raw(),main='Original data')
  })
  Kohonen.2.data.process <- eventReactive(input$Kohonen.2.go,{
    withProgress(message = "Work in progress", value=0, {
      SOM.cluster(data.raw(),
                  margin=c(3),
                  transform=c(F),
                  grid.x = c(input$Kohonen.2.Grid.X.1),
                  grid.y = c(input$Kohonen.2.Grid.Y.1),
                  topo =c('hexagonal'),
                  toroidal = c(F),
                  rlen=c(100),
                  alpha.1 = c(0.05),
                  alpha.2 = c(0.01),
                  radius = c('Default'),
                  action = c('reconstruct'),
                  count.successif.1 = c(0),
                  count.successif.2 = c(100),
                  use=1)
    })
  })
  output$Kohonen.2.mapping.Process_1.RGB.explosion <- renderPlot({
    validate(
      need(input$Kohonen.2.go, "Click on the Analyze button")
    )
    data <- Kohonen.2.data.process()
    par(mar=c(0,0,4,0))
    plot(data$model[[1]],type='mapping',
         bgcol=apply(data$model[[1]]$codes,1,function(x){rgb(x[1],x[2],x[3])})
    )
  })
  output$Kohonen.2.raster_1 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(Kohonen.2.data.process()$data.recon[[1]],main='Reconstruct data')
  })
  output$Kohonen.2.chromato_1 <- renderPlot({
    validate(
      need(input$click.Kohonen.2.raster_1 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.Kohonen.2.raster_1
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    ## Original
    chrom.pict(data.raw(),x)
    ## Reconstruct
    chrom.pict(Kohonen.2.data.process()$data.recon[[1]],x)
  })
#### New.Col ####
  New.Col.data.raw <- reactive({
    data.raw()
  })
  output$New.Col.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(New.Col.data.raw(),main='Click on the picture to extract the new color')
    if(!is.null(click.New.Col.raster_0.value$value)){
      text(x=click.New.Col.raster_0.value$value[1],y=click.New.Col.raster_0.value$value[2],col='red',labels = 'X')
    }
  })
  click.New.Col.raster_0.value <- reactiveValues(value=NULL)
  click.New.Col.raster_0.obs <- observeEvent(input$click.New.Col.raster_0,{
    click.New.Col.raster_0.value$value <- input$click.New.Col.raster_0
  })
  New.Col.data.process <- reactive({
    click <- click.New.Col.raster_0.value$value
    x <- ceiling(as.numeric(click[1]))
    y <- ceiling(as.numeric(click[2]))
    truc <- New.Col.data.raw() %>% create.new.color(x=x,y=y,func=input$New.Col.dist.function)
    return(truc)
  })
  output$New.Col.raster_1 <- renderPlot({
    validate(
      need(!is.null(click.New.Col.raster_0.value$value), "Click on the picture to calculate the new color")
    )
    par(mar=c(0,0,4,0))
    click <- click.New.Col.raster_0.value$value
    x <- ceiling(as.numeric(click[1]))
    y <- ceiling(as.numeric(click[2]))
    New.Col.data.process() %>% normalize %>% raster(main='click on the picture to see the chromatograms')
    # SOM.cluster.plot.picture(normalize(New.Col.data.process()),main=paste0('click on the picture to see the chromatograms\nRGB values: ',paste0(round(New.Col.data.raw()[dim(New.Col.data.raw())[1]-y,x],3),collapse=';')))
  })
  output$New.Col.chromato_0 <- renderPlot({
    validate(
      need(input$click.New.Col.raster_1 != "", "Click on the picture to see the chromatograms")
    )
    par(mfrow=c(2,1),mar=c(0,0,0,0))
    click <- input$click.New.Col.raster_1
    x <- ceiling(as.numeric(click[1]))
    data <- New.Col.data.process()[,x]
    plot(rev(data),type='l',bty='n',axes = F)
    data <- New.Col.data.raw()
    data <- array(data[dim(data)[1]:1,x,c(1,2,3)],dim=c(dim(data)[1],1,3))
    plot(c(0,dim(data)[1]),c(0,1.2), type='n',ylab="",xlab="",bty='n',
         main='')
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,dim(data)[1],1.2)
    color <- c('red','green','blue')
    for(i in seq(3)){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i])
    }
  })
  output$New.Col.raster_0.red <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(New.Col.data.raw()[,,1],main='Red Channel alone')
  })
  output$New.Col.raster_0.green <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(New.Col.data.raw()[,,2],main='Green Channel alone')
  })
  output$New.Col.raster_0.blue <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(New.Col.data.raw()[,,3],main='Blue Channel alone')
  })
  output$New.Col.raster_0.gray <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(apply(New.Col.data.raw(),c(1,2),mean),main='Gray Channel alone')
  })
#### RBM.1 ####
  output$RBM.1.option <- renderTable({
    data <- data.frame(Name = c('hidden','numepochs','batchsize','momentum','learningrate'),
                       Value = c(16,100,10,0.5,0.8)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RBM.1.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RBM.1.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") # readonly='readonly'
    }

    data
  }, sanitize.text.function = function(y) y)
  RBM.1.data.raw <- reactive({
    data.raw()
  })
  output$RBM.1.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(RBM.1.data.raw(),main='Original data')
  })
  RBM.1.model.tot <- eventReactive(input$RBM.1.go,{
    withProgress(message = "Work in progress", value=0, {
      data <- RBM.1.data.raw() %>% deconstruct(margin=2,transform = T)
      rbm.train(data[sample(nrow(data)),],  ## really important to randomise the data
                hidden=input$RBM.1.hidden,
                numepochs = input$RBM.1.numepochs,
                batchsize = input$RBM.1.batchsize,
                momentum = input$RBM.1.momentum,
                learningrate = input$RBM.1.learningrate,keep.data=T,verbose=T)
    })
  })
  output$RBM.1.slider.epochs <- renderUI({
    sliderInput('RBM.1.slider.epochs','Number of epoch to stop',min=1,max=input$RBM.1.numepochs,value=input$RBM.1.numepochs)
  })
  RBM.1.model <- reactive({
    RBM.1.model.tot()$keep[[input$RBM.1.slider.epochs]]
  })
  RBM.1.data.up <- reactive({
    model <- RBM.1.model()
    RBM.1.data.raw() %>%
      deconstruct(margin=2,transform = T) %>% rbm.up(model,.)
  })
  RBM.1.data.process <- reactive({
    model <- RBM.1.model()
    RBM.1.data.up() %>% rbm.down(model,.) %>%
      reconstruct(margin=2,transform = T,dimension = dim(RBM.1.data.raw()))
  })
  output$RBM.1.raster_1 <- renderPlot({
    validate(
      need(input$RBM.1.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(RBM.1.data.process(),main='Reconstructed data')
  })
  output$RBM.1.raster_2 <- renderPlot({
    validate(
      need(input$RBM.1.go, "Click on the Analyze button")
    )
    model <- RBM.1.model()
    par(mar=c(0,0,4,0))
    RBM.1.data.up() %>% apply(2,function(x){x <- x - min(x);x <- x/max(x)}) %>% t %>% ## Carefull here as the raster need a t of the matrix,
      SOM.cluster.plot.picture(main='Hidden unit, with normalisation step')
    for(i in c(0:dim(RBM.1.data.up())[1])){
      abline(h=i,col='red')
    }
  })

  output$RBM.1.chromato.1 <- renderPlot({
    validate(
      need(input$click.RBM.1.raster_2 != "", "Click on the picture to see the chromatograms")
    )
    par(mfrow=c(3,1),mar=c(0,0,0,0))
    click <- input$click.RBM.1.raster_2
    x <- ceiling(as.numeric(click[1]))
    y <- ceiling(as.numeric(click[2]))
    data <- RBM.1.model()$W %>% reconstruct(margin=2,transform=T,dimension=dim(data.raw())) %>% normalize
    data %>% chrom.pict(y)
    data.raw() %>% chrom.pict(x)
    RBM.1.data.process() %>% chrom.pict(x)
  })

  RBM.1.model.PCA <- reactive({
    PCA(RBM.1.data.up())
  })
  RBM.1.data.up.2 <- reactive({
    ChemometricsWithR::reconstruct(object=RBM.1.model.PCA(),npc=input$RBM.1.slider.comp)[[1]] %>% normalize ## Carefull with the reconstruction of PCA, list of 2
  })
  output$RBM.1.raster_3 <- renderPlot({
    par(mar=c(0,0,4,0),xaxt='n',yaxt='n')
    str(RBM.1.data.up.2())
    RBM.1.data.up.2() %>% t %>% raster(main='After PCA') ## Carefull here as the raster need a t of the matrix,
  })
  output$RBM.1.scores_1 <- renderPlot({
    RBM.1.model.PCA()$scores[,c(1,2)] %>% plot(type='p')
  })
  output$RBM.1.raster_4 <- renderPlot({
    # print(str(input$brush.PCA.1.scores_1))
    # brush <- lapply(input$brush.PCA.1.scores_1,as.numeric)
    brush <- input$brush.RBM.1.scores_1
    score <- RBM.1.model.PCA()$scores[,c(1,2)]
    truc <- which(!(score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
    data <- data.raw()
    data[,truc,] <- 0
    data %>% raster(main='Seleted points')
  })
#### RBM.2 ####
  output$RBM.2.option <- renderTable({
    data <- data.frame(Name = c('hidden','numepochs','batchsize','momentum','learningrate'),
                       Value = c(8,10,100,0.5,0.8)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RBM.2.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RBM.2.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") #
    }

    data
  }, sanitize.text.function = function(y) y)
  RBM.2.data.raw <- reactive({
    data.raw()
  })
  output$RBM.2.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(RBM.2.data.raw(),main='Original data')
  })
  RBM.2.model <- eventReactive(input$RBM.2.go,{
    withProgress(message = "Work in progress", value=0, {
      data <- RBM.2.data.raw() %>% deconstruct(margin=3,transform = F)
      model <- rbm.train(data[sample(nrow(data)),],  ## really important to randomise the data
                         hidden=input$RBM.2.hidden,
                         numepochs = input$RBM.2.numepochs,
                         batchsize = input$RBM.2.batchsize,
                         momentum = input$RBM.2.momentum,
                         learningrate = input$RBM.2.learningrate)
      # data.recon <- data %>% rbm.up(model,.) %>% rbm.down(model,.)
      return(model)
    })
  })
  RBM.2.data.up <- reactive({
    data <- RBM.2.data.raw() %>% deconstruct(margin=3,transform = F)
    model <- RBM.2.model()
    data %>% rbm.up(model,.) %>% reconstruct(margin=3,transform = F,dimension = dim(RBM.2.data.raw()))
  })
  RBM.2.data.process <- reactive({
    data <- RBM.2.data.up() %>% deconstruct(margin=3,transform = F)
    model <- RBM.2.model()
    data  %>% rbm.down(model,.) %>% reconstruct(margin=3,transform = F,dimension = dim(RBM.2.data.raw()))
  })
  output$RBM.2.raster_1 <- renderPlot({
    validate(
      need(input$RBM.2.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    RBM.2.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data')
  })
  output$RBM.2.raster_2 <- renderPlot({
    validate(
      need(input$RBM.2.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    RBM.2.data.process() %>% normalize %>% SOM.cluster.plot.picture(main='Reconstructed data, normalized for sensibility\nClick on the picture to see the chromatograms')
  })
  output$RBM.2.chromato_1 <- renderPlot({
    validate(
      need(input$click.RBM.2.raster_2 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.RBM.2.raster_2
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(5,1),mar=c(0,0,2,0))
    ## Original
    data <- RBM.2.data.raw()
    plot(c(0,height),c(0,1.2), type='n',ylab="",xlab="",bty='n',axes=F, main='original')
    data <- array(data[dim(data)[1]:1,x,seq(dim(data)[3])],dim=c(dim(data)[1],1,dim(data)[3]))
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,height,1.2)
    color <- c('red','green','blue')
    for(i in seq(3)){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i])
    }
    ## Hidden
    data <- RBM.2.data.up()
    plot(c(0,height),c(0,1.2), type='n',ylab="",xlab="",bty='n',axes=F, main='hidden')
    data <- array(data[dim(data)[1]:1,x,seq(dim(data)[3])],dim=c(dim(data)[1],1,dim(data)[3]))
    for(i in seq(RBM.2.model()$size[2])){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=i)
    }
    ## Hidden normalize
    data <- RBM.2.data.up() %>% normalize
    plot(c(0,height),c(0,1.2), type='n',ylab="",xlab="",bty='n',axes=F, main='hidden normalize')
    data <- array(data[dim(data)[1]:1,x,seq(dim(data)[3])],dim=c(dim(data)[1],1,dim(data)[3]))
    for(i in seq(RBM.2.model()$size[2])){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=i)
    }
    ## Reconstruct
    data <- RBM.2.data.process()
    plot(c(0,height),c(0,1.2), type='n',ylab="",xlab="",bty='n',axes=F, main='reconstruct')
    data <- array(data[dim(data)[1]:1,x,seq(dim(data)[3])],dim=c(dim(data)[1],1,dim(data)[3]))
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,height,1.2)
    color <- c('red','green','blue')
    for(i in seq(3)){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i])
    }
    ## Reconstruct normalize
    data <- RBM.2.data.process()  %>% normalize
    plot(c(0,height),c(0,1.2), type='n',ylab="",xlab="",bty='n',axes=F, main='reconstruct normalize')
    data <- array(data[dim(data)[1]:1,x,seq(dim(data)[3])],dim=c(dim(data)[1],1,dim(data)[3]))
    data %>% aperm(c(2,1,3)) %>% rasterImage(0,1.1,height,1.2)
    color <- c('red','green','blue')
    for(i in seq(3)){
      par(new=T)
      plot(y=data[,,i],x=seq(0,1,length.out=dim(data)[1]),type='l',ylab="",xlab="",ylim=c(0,1.2),col=color[i])
    }
  })
  output$RBM.2.network_1 <- renderPlot({
    model <- RBM.2.model()
    par(mfrow=c(1,1),mar=c(0,0,2,0))
    plot(c(-0.5,1+1/model$size[1]),c(0,5.5), type='n',ylab="",xlab="",bty='n',axes=F,main='RBM network') #
    size.circle <- 1/max(model$size)/2.5
    x <- c(0.5,
           seq(1/model$size[1],1,length.out = model$size[1]),
           seq(1/model$size[2],1,length.out = model$size[2]),
           seq(1/model$size[1],1,length.out = model$size[1]),
           0.5)
    y <- c(1,rep(2,model$size[1]),rep(3,model$size[2]),rep(4,model$size[1]),5)
    symbols(x=x,y=y,circles =rep(size.circle,2+2*model$size[1]+model$size[2]),inches=F,add=T)
    text(x=rep(0,5),y=seq(5),labels=c('Input\npicture','Input\nchannel','hidden\nunits','output\nchannel','output\npicture'))
    text(x=click.RBM.2.network_1$value[1],y=click.RBM.2.network_1$value[2],labels='click',col='red')
    text(x=dblclick.RBM.2.network_1$value[1],y=dblclick.RBM.2.network_1$value[2],labels='dblclick',col='red')
  })
  click.RBM.2.network_1 <- reactiveValues(value=c(0.5,1))
  click.RBM.2.network_1.obs <- observeEvent(input$click.RBM.2.network_1,{
    click.RBM.2.network_1$value <- input$click.RBM.2.network_1
  })
  dblclick.RBM.2.network_1 <- reactiveValues(value=c(0.5,5))
  dblclick.RBM.2.network_1.obs <- observeEvent(input$dblclick.RBM.2.network_1,{
    dblclick.RBM.2.network_1$value <- input$dblclick.RBM.2.network_1
  })
  output$RBM.2.raster_3_click <- renderPlot({
    par(mar=c(0,0,4,0))
    click <- click.RBM.2.network_1$value
    model <- RBM.2.model()
    y <- round(as.numeric(click[2]))
    if(y== 1){
      RBM.2.data.raw() %>% SOM.cluster.plot.picture(main='Original data\n clicked cell')
    }
    if(y == 2){
      RBM.2.data.raw()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Original data, single channel\n clicked cell')
    }
    if(y == 4){
      RBM.2.data.process()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Reconstructed data, single channel\n clicked cell')
    }
    if(y == 5){
      RBM.2.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data\n clicked cell')
    }
    if(y == 3){
      data <- RBM.2.data.up()
      data[,,round(as.numeric(click[1])*model$size[2])] %>%SOM.cluster.plot.picture(main='hidden units\nclicked cell')
    }
  })
  output$RBM.2.raster_4_dblclick <- renderPlot({
    par(mar=c(0,0,4,0))
    click <- dblclick.RBM.2.network_1$value
    model <- RBM.2.model()
    y <- round(as.numeric(click[2]))
    if(y== 1){
      RBM.2.data.raw() %>% SOM.cluster.plot.picture(main='Original data\n dblclicked cell')
    }
    if(y == 2){
      RBM.2.data.raw()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Original data, single channel\n dblclicked cell')
    }
    if(y == 4){
      RBM.2.data.process()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Reconstructed data, single channel\n dblclicked cell')
    }
    if(y == 5){
      RBM.2.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data\n dblclicked cell')
    }
    if(y == 3){
      data <- RBM.2.data.up()
      data[,,round(as.numeric(click[1])*model$size[2])] %>%SOM.cluster.plot.picture(main='hidden units\ndblclicked cell')
    }
  })
#### RBM.3 ####
  output$RBM.3.option <- renderTable({
    data <- data.frame(Name = c('conv_width','hidden','numepochs','batchsize','momentum','learningrate'),
                       Value = c(2,4,10,1000,0.5,0.8)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RBM.3.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RBM.3.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") #
    }

    data
  }, sanitize.text.function = function(y) y)
  RBM.3.data.raw <- reactive({
    data.raw()
  })
  RBM.3.data.raw.decon <- reactive({
    data <- RBM.3.data.raw() %>% deconstruct.convol(margin=3,transform = F,conv_width = input$RBM.3.conv_width)
    print(dim(data))
    data
  })
  output$RBM.3.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(RBM.3.data.raw(),main='Original data')
  })
  RBM.3.model <- eventReactive(input$RBM.3.go,{
    withProgress(message = "Work in progress", value=0, {
      data <- RBM.3.data.raw.decon()
      model <- rbm.train(data[sample(nrow(data)),],  ## really important to randomise the data
                         hidden=input$RBM.3.hidden,
                         numepochs = input$RBM.3.numepochs,
                         batchsize = input$RBM.3.batchsize,
                         momentum = input$RBM.3.momentum,
                         learningrate = input$RBM.3.learningrate,verbose = T)
      # data.recon <- data %>% rbm.up(model,.) %>% rbm.down(model,.)
      return(model)
    })
  })
  RBM.3.data.up <- reactive({
    model <- RBM.3.model()
    RBM.3.data.raw.decon() %>% rbm.up(model,.) #%>% reconstruct(margin=3,transform = F,dimension = dim(RBM.2.data.raw()))
  })
  RBM.3.data.up.recon <- reactive({
    RBM.3.data.up() %>% reconstruct(margin=3,transform = F,dimension = dim(data.raw()))
  })
  RBM.3.data.process <- reactive({
    data <- RBM.3.data.up() #%>% deconstruct.convol(margin=3,transform = F,conv_width = input$RBM.3.conv_width)
    model <- RBM.3.model()
    data  %>% rbm.down(model,.) %>% reconstruct.convol(margin=3,transform = F,dimension = dim(RBM.3.data.raw()),conv_width = input$RBM.3.conv_width,take_center = F)
  })
  output$RBM.3.raster_1 <- renderPlot({
    validate(
      need(input$RBM.3.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    RBM.3.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data')
  })
  output$RBM.3.chromato_1 <- renderPlot({
    validate(
      need(input$click.RBM.3.raster_1 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.RBM.3.raster_1
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    ## Original
    chrom.pict(RBM.3.data.raw(),x)
    ## Reconstruct
    chrom.pict(RBM.3.data.process(),x)
  })
  output$RBM.3.network_1 <- renderPlot({
    model <- RBM.3.model()
    par(mfrow=c(1,1),mar=c(0,0,2,0))
    plot(c(-0.5,1+1/model$size[1]),c(0,5.5), type='n',ylab="",xlab="",bty='n',axes=F,main='RBM network') #
    size.circle <- 1/max(model$size)/2.5
    x <- c(0.5,
           seq(1/model$size[1],1,length.out = model$size[1]),
           seq(1/model$size[2],1,length.out = model$size[2]),
           seq(1/model$size[1],1,length.out = model$size[1]),
           0.5)
    y <- c(1,rep(2,model$size[1]),rep(3,model$size[2]),rep(4,model$size[1]),5)
    symbols(x=x,y=y,circles =rep(size.circle,2+2*model$size[1]+model$size[2]),inches=F,add=T)
    text(x=rep(0,5),y=seq(5),labels=c('Input\npicture','Input\nchannel','hidden\nunits','output\nchannel','output\npicture'))
    text(x=click.RBM.3.network_1$value[1],y=click.RBM.3.network_1$value[2],labels='click',col='red')
    text(x=dblclick.RBM.3.network_1$value[1],y=dblclick.RBM.3.network_1$value[2],labels='dblclick',col='red')
  })
  click.RBM.3.network_1 <- reactiveValues(value=c(0.5,1))
  click.RBM.3.network_1.obs <- observeEvent(input$click.RBM.3.network_1,{
    click.RBM.3.network_1$value <- input$click.RBM.3.network_1
  })
  dblclick.RBM.3.network_1 <- reactiveValues(value=c(0.5,5))
  dblclick.RBM.3.network_1.obs <- observeEvent(input$dblclick.RBM.3.network_1,{
    dblclick.RBM.3.network_1$value <- input$dblclick.RBM.3.network_1
  })
  output$RBM.3.raster_3_click <- renderPlot({
    par(mar=c(0,0,4,0))
    click <- click.RBM.3.network_1$value
    model <- RBM.3.model()
    y <- round(as.numeric(click[2]))
    if(y== 1){
      RBM.3.data.raw() %>% SOM.cluster.plot.picture(main='Original data\n clicked cell')
    }
    if(y == 2){
      RBM.3.data.raw()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Original data, single channel\n clicked cell')
    }
    if(y == 4){
      RBM.3.data.process()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Reconstructed data, single channel\n clicked cell')
    }
    if(y == 5){
      RBM.3.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data\n clicked cell')
    }
    if(y == 3){
      data <- RBM.3.data.up.recon()
      data[,,round(as.numeric(click[1])*model$size[2])] %>%SOM.cluster.plot.picture(main='hidden units\nclicked cell')
    }
  })
  output$RBM.3.raster_4_dblclick <- renderPlot({
    par(mar=c(0,0,4,0))
    click <- dblclick.RBM.3.network_1$value
    model <- RBM.3.model()
    y <- round(as.numeric(click[2]))
    if(y== 1){
      RBM.3.data.raw() %>% SOM.cluster.plot.picture(main='Original data\n dblclicked cell')
    }
    if(y == 2){
      RBM.3.data.raw()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Original data, single channel\n dblclicked cell')
    }
    if(y == 4){
      RBM.3.data.process()[,,round(as.numeric(click[1])*model$size[1])] %>% SOM.cluster.plot.picture(main='Reconstructed data, single channel\n dblclicked cell')
    }
    if(y == 5){
      RBM.3.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data\n dblclicked cell')
    }
    if(y == 3){
      data <- RBM.3.data.up.recon()
      data[,,round(as.numeric(click[1])*model$size[2])] %>%SOM.cluster.plot.picture(main='hidden units\ndblclicked cell')
    }
  })
  output$RBM.3.chromato_2 <- renderPlot({
    validate(
      need(input$click.RBM.3.raster_3 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.RBM.3.raster_3
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    ## Original
    chrom.pict(RBM.3.data.raw(),x)
    ## Reconstruct
    click <- click.RBM.3.network_1$value
    model <- RBM.3.model()
#     data <- RBM.3.data.up() %>% reconstruct(margin=3,transform = F,dimension = dim(data.raw()))
    data <- RBM.3.data.up.recon()
    data[nrow(data):1,x,round(as.numeric(click[1])*model$size[2])] %>% plot(type='l')
  })
  output$RBM.3.Process.2.option <- renderTable({
    data <- data.frame(Name = c('hidden','layer','numepochs','batchsize','momentum','learningrate'),
                       Value = c(16,1,100,10,0.5,0.8)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RBM.3.Process.2.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RBM.3.Process.2.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") # readonly='readonly'
    }

    data
  }, sanitize.text.function = function(y) y)
  output$RBM.3.raster_5 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(data.raw(),main='Original data')
  })
  RBM.3.Process.2.model <- eventReactive(input$RBM.3.Process.2.go,{
    withProgress(message = "Work in progress", value=0, {
      data <- RBM.3.data.up.recon()
      data %>% dim %>% print
      data <- data %>% deconstruct(margin=2,transform = T)
      data %>% dim %>% print
      rbm.train(data[sample(nrow(data)),],  ## really important to randomise the data
                hidden=input$RBM.3.Process.2.hidden,
                numepochs = input$RBM.3.Process.2.numepochs,
                batchsize = input$RBM.3.Process.2.batchsize,
                momentum = input$RBM.3.Process.2.momentum,
                learningrate = input$RBM.3.Process.2.learningrate,
                verbose = T)
    })
  })
  RBM.3.Process.2.data.up <- reactive({
    model <- RBM.3.Process.2.model()
    RBM.3.data.up.recon() %>% deconstruct(margin=2,transform = T) %>% rbm.up(model,.)
  })

  output$RBM.3.raster_6 <- renderPlot({
    validate(
      need(input$RBM.3.Process.2.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    RBM.3.Process.2.data.up() %>% apply(2,function(x){x <- x - min(x);x <- x/max(x)}) %>% t %>% ## Carefull here as the raster need a t of the matrix,
      SOM.cluster.plot.picture(main='Hidden unit, with normalisation step')
  })
#### RBM.4 ####
  output$RBM.4.option <- renderTable({
    data <- data.frame(Name = c('hidden_1','hidden_2','hidden_3','numepochs','batchsize','momentum','learningrate'),
                       Value = c(32,8,1,100,10,0.5,0.8)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RBM.4.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RBM.4.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") # readonly='readonly'
    }

    data
  }, sanitize.text.function = function(y) y)
  output$RBM.4.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    data.raw() %>% raster(main='Original',Title.dim = T)
  })
  RBM.4.model.1 <- eventReactive(input$RBM.4.go,{
    withProgress(message = "Work in progress: First layer", value=0, {
      data <- data.raw() %>% deconstruct(margin=2,transform = T)
      rbm.train(data,
                hidden=input$RBM.4.hidden_1,
                numepochs = input$RBM.4.numepochs,
                batchsize = input$RBM.4.batchsize,
                momentum = input$RBM.4.momentum,
                learningrate = input$RBM.4.learningrate,keep.data=F,verbose=T)
    })
  })
  RBM.4.data.up.layer.1 <- reactive({
    data.raw() %>% deconstruct(margin=2,transform = T) %>% rbm.up(RBM.4.model.1(),.)
  })
  output$RBM.4.raster_1 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.up.layer.1() %>% t %>% raster(main='RBM.4.data.up.layer.1',Title.dim = T)
  })
  RBM.4.data.down.original.from.layer.1 <- reactive({
    rbm.down(RBM.4.model.1(),RBM.4.data.up.layer.1())
  })
  output$RBM.4.raster_2 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.down.original.from.layer.1() %>% reconstruct(2,T,dim(data.raw())) %>% raster(main='RBM.4.data.down.original.from.layer.1',Title.dim = T)
  })

  RBM.4.model.2 <- eventReactive(input$RBM.4.go,{
    withProgress(message = "Work in progress: Second layer", value=0, {
      data <- RBM.4.data.up.layer.1()
      rbm.train(data,
                hidden=input$RBM.4.hidden_2,
                numepochs = input$RBM.4.numepochs,
                batchsize = input$RBM.4.batchsize,
                momentum = input$RBM.4.momentum,
                learningrate = input$RBM.4.learningrate,keep.data=F,verbose=T)
    })
  })
  RBM.4.data.up.layer.2 <- reactive({
    rbm.up(RBM.4.model.2(),RBM.4.data.up.layer.1())
  })
  output$RBM.4.raster_3 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.up.layer.2() %>% t %>% raster(main='RBM.4.data.up.layer.2',Title.dim = T)
  })
  RBM.4.data.down.layer.1.from.layer.2 <- reactive({
    rbm.down(RBM.4.model.2(),RBM.4.data.up.layer.2())
  })
  output$RBM.4.raster_4 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.down.layer.1.from.layer.2() %>% t %>% raster(main='RBM.4.data.down.layer.1.from.layer.2',Title.dim = T)
  })
  RBM.4.data.down.original.from.layer.2 <- reactive({
    rbm.down(RBM.4.model.1(),RBM.4.data.down.layer.1.from.layer.2())
  })
  output$RBM.4.raster_5 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.down.original.from.layer.2() %>% reconstruct(2,T,dim(data.raw())) %>% raster(main='RBM.4.data.down.original.from.layer.2',Title.dim = T)
  })

  RBM.4.model.3 <- eventReactive(input$RBM.4.go,{
    withProgress(message = "Work in progress: Third layer", value=0, {
      data <- RBM.4.data.up.layer.2()
      rbm.train(data,
                hidden=input$RBM.4.hidden_3,
                numepochs = input$RBM.4.numepochs,
                batchsize = input$RBM.4.batchsize,
                momentum = input$RBM.4.momentum,
                learningrate = input$RBM.4.learningrate,keep.data=F,verbose=T)
    })
  })
  RBM.4.data.up.layer.3 <- reactive({
    rbm.up(RBM.4.model.3(),RBM.4.data.up.layer.2())
  })
  output$RBM.4.raster_6 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.up.layer.3() %>% t %>% raster(main='RBM.4.data.up.layer.3',Title.dim = T)
  })
  RBM.4.data.down.layer.2.from.layer.3 <- reactive({
    rbm.down(RBM.4.model.3(),RBM.4.data.up.layer.3())
  })
  output$RBM.4.raster_7 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.down.layer.2.from.layer.3() %>% t %>% raster(main='RBM.4.data.down.layer.2.from.layer.3',Title.dim = T)
  })
  RBM.4.data.down.layer.1.from.layer.3 <- reactive({
    rbm.down(RBM.4.model.2(),RBM.4.data.down.layer.2.from.layer.3())
  })
  output$RBM.4.raster_8 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.down.layer.1.from.layer.3() %>% t %>% raster(main='RBM.4.data.down.layer.1.from.layer.3',Title.dim = T)
  })
  RBM.4.data.down.original.from.layer.3 <- reactive({
    rbm.down(RBM.4.model.1(),RBM.4.data.down.layer.1.from.layer.3())
  })
  output$RBM.4.raster_9 <- renderPlot({
    par(mar=c(0,0,4,0))
    RBM.4.data.down.original.from.layer.3() %>% reconstruct(2,T,dim(data.raw())) %>% raster(main='RBM.4.data.down.original.from.layer.3',Title.dim = T)
  })
#### PCA.1 ####
  output$PCA.1.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(data.raw(),main='Original data')
  })
  PCA.1.model <- reactive({
    decon <- deconstruct(data = data.raw(),margin = 2,transform = T)
    PCA(decon)
  })
  PCA.1.data.process <- reactive({
    pred <- ChemometricsWithR::reconstruct(object=PCA.1.model(),npc=input$PCA.1.slider.comp)
    print(dim(data.raw()))
    DLC::reconstruct(data = pred[[1]],margin = 2,transform = T,dimension = dim(data.raw())) %>% normalize
  })
  output$PCA.1.raster_1 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(PCA.1.data.process(),main='Reconstruct data')
  })
  output$PCA.1.chromato_1 <- renderPlot({
    validate(
      need(input$click.PCA.1.raster_1 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.PCA.1.raster_1
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    ## Original
    chrom.pict(data.raw(),x)
    ## Reconstruct
    chrom.pict(PCA.1.data.process(),x)
  })
  output$PCA.1.scores_1 <- renderPlot({
    PCA.1.model()$scores[,c(1,2)] %>% plot(type='p')
  })
  output$PCA.1.raster_2 <- renderPlot({
    # print(str(input$brush.PCA.1.scores_1))
    # brush <- lapply(input$brush.PCA.1.scores_1,as.numeric)
    brush <- input$brush.PCA.1.scores_1
    score <- PCA.1.model()$scores[,c(1,2)]
    truc <- which(!(score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
    data <- data.raw()
    data[,truc,] <- 0
    par(xaxt='n',yaxt='n')
    data %>% raster(main='Seleted points')
  })
  output$PCA.1.raster_3 <- renderPlot({
    par(xaxt='n',yaxt='n')
    PCA.1.model()$scores[,c(1:10)] %>% t %>% normalize(hidden = 'row') %>% raster(main = 'PCA component, first on top')
  })
#### HCA.1 ####
  output$HCA.1.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(data.raw(),main='Original data')
  })
  HCA.1.model <- reactive({
    data <- data.raw()
    decon <- deconstruct(data = data,margin = 2,transform = T)
    d <- dist(decon) # distance matrix
    hclust(d)
  })
  output$HCA.1.cluster_1 <- renderPlot({
    plot(HCA.1.model(),main="Cluster Dentogram",xlab="",sub='') # display dendogram
    # groups <- cutree(HCA.1.model(), k=input$HCA.1.cluster.nbr.1)
    vec <- rep('red',input$HCA.1.cluster.nbr.1)
    vec[input$HCA.1.select.1] <- 'green'
    rect.hclust(HCA.1.model(), k=input$HCA.1.cluster.nbr.1, border=vec)
  })
  output$HCA.1.raster_1 <- renderPlot({
    print(input$click.HCA.1.cluster_1$x)
    groups <- cutree(HCA.1.model(), k=input$HCA.1.cluster.nbr.1)
    truc <- groups != input$HCA.1.select.1
    data <- data.raw()
    data[,truc,] <- 0
    data %>% raster(main='Seleted points')
  })
#### RNN.1 ####
  output$RNN.1.option <- renderTable({
    data <- data.frame(Name = c('hidden_layer_1','hidden_layer_2','maxit'),
                       Value = c(4,4,10)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RNN.1.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RNN.1.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") #
    }

    data
  }, sanitize.text.function = function(y) y)
  RNN.1.data.raw.decon <- reactive({
    if(input$RNN.1.channel != 0){
      data <- data.raw()[,,as.numeric(input$RNN.1.channel)] %>% t
    }else{
      data <- data.raw() %>% deconstruct(margin=2,transform = T)
    }
    data
  })
  output$RNN.1.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    if(input$RNN.1.channel != 0){
      data <- data.raw()[,,as.numeric(input$RNN.1.channel)]# %>% t
    }else{
      data <- data.raw() #%>% deconstruct(margin=2,transform = T)
    }
    data %>% raster(main='Original data')
  })
  RNN.1.model <- eventReactive(input$RNN.1.go,{
    withProgress(message = "Learning", value=0, {
      data <- RNN.1.data.raw.decon()
      model <- elman(data,data,size=c(input$RNN.1.hidden_layer_1,input$RNN.1.hidden_layer_2),maxit=input$RNN.1.maxit,shufflePatterns = T)
      return(model)
    })
  })
  RNN.1.data.process <- reactive({
    withProgress(message = "Prediction and Reconstruction: train set", value=0, {
      data <- RNN.1.data.raw.decon()
      model <- RNN.1.model()
      data <- data  %>% predict(model,.) %>% normalize#reconstruct(margin=1,transform = F,dimension = dim(data.raw()))
      if(input$RNN.1.channel != 0){
        data <- data %>% t
      }else{
        data <- data %>% reconstruct(margin=2,transform = T,dimension = dim(data.raw()))
      }
    })
  })
  output$RNN.1.raster_1 <- renderPlot({
    validate(
      need(input$RNN.1.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    RNN.1.data.process() %>% SOM.cluster.plot.picture(main='Reconstructed data')
  })
  output$RNN.1.chromato_1 <- renderPlot({
    validate(
      need(input$click.RNN.1.raster_1 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.RNN.1.raster_1
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    if(input$RNN.1.channel != 0){
      ## Original
      chrom.pict(data.raw()[,,as.numeric(input$RNN.1.channel)],x)
      ## Reconstruct
      chrom.pict(RNN.1.data.process(),x)
    }else{
      ## Original
      chrom.pict(data.raw(),x)
      ## Reconstruct
      chrom.pict(RNN.1.data.process(),x)
    }

  })
  RNN.1.data.test.raw <- reactive({
    data <- f.read.image(input$RNN.1.upload.test$datapath,height=input$height,Normalize = input$data.raw.normalize)
    validate(
      need(dim(data)[1] >= input$height, 'One at least of the picture is smaller than the redimension height')
    )
    if(input$RNN.1.channel != 0){
      data <- data[,,as.numeric(input$RNN.1.channel)]
    }
    return(data)
  })
  RNN.1.data.test.raw.decon <- reactive({
    data <- RNN.1.data.test.raw()
    if(input$RNN.1.channel != 0){
      data <- data %>% t
    }else{
      data <- data %>% deconstruct(margin=2,transform = T)
    }
    data
  })
  output$RNN.1.raster_2 <- renderPlot({
    par(mar=c(0,0,4,0))
    RNN.1.data.test.raw() %>% raster(main='Test Original')
  })
  RNN.1.data.test.process <- reactive({
    withProgress(message = "Prediction and Reconstruction : test set", value=0, {
      data <- RNN.1.data.test.raw.decon()
      model <- RNN.1.model()
      pred <- data  %>% predict(model,.) #reconstruct(margin=1,transform = F,dimension = dim(data.raw()))
      if(input$RNN.1.channel != 0){
        pred <- pred %>% t
      }else{
        pred <- pred %>% reconstruct(margin=2,transform = T,dimension = dim(RNN.1.data.test.raw()))
      }
      pred %>% normalize
    })
  })
  output$RNN.1.raster_3 <- renderPlot({
    validate(
      need(input$RNN.1.go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    RNN.1.data.test.process() %>% SOM.cluster.plot.picture(main='Test Reconstructed')
  })
  output$RNN.1.chromato_2 <- renderPlot({
    validate(
      need(input$click.RNN.1.raster_3 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.RNN.1.raster_3
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    ## Original
    chrom.pict(RNN.1.data.test.raw(),x)
    ## Reconstruct
    chrom.pict(RNN.1.data.test.process(),x)
  })

  output$RNN.1.option.RBM <- renderTable({
    data <- data.frame(Name = c('hidden_1','hidden_2','numepochs','batchsize','momentum','learningrate'),
                       Value = c(16,4,100,10,0.5,0.8)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='RNN.1.RBM.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='RNN.1.RBM.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") # readonly='readonly'
    }

    data
  }, sanitize.text.function = function(y) y)
  RNN.1.RBM.model.1 <- eventReactive(input$RNN.1.RBM.go,{
    if(input$RNN.1.channel != 0){
      data <- RNN.1.data.test.process() %>% t
    }else{
      data <- RNN.1.data.test.process() %>% deconstruct(margin=2,transform = T)
    }
    withProgress(message = "Work in progress: First layer", value=0, {
     data  %>% rbm.train(hidden=input$RNN.1.RBM.hidden_1,
                numepochs = input$RNN.1.RBM.numepochs,
                batchsize = input$RNN.1.RBM.batchsize,
                momentum = input$RNN.1.RBM.momentum,
                learningrate = input$RNN.1.RBM.learningrate,keep.data=F,verbose=T)
    })
  })
  RNN.1.RBM.data.up.layer.1 <- reactive({
    if(input$RNN.1.channel != 0){
      data <- RNN.1.data.test.process() %>% t
    }else{
      data <- RNN.1.data.test.process() %>% deconstruct(margin=2,transform = T)
    }
    data %>% rbm.up(RNN.1.RBM.model.1(),.)
  })
  RNN.1.RBM.model.2 <- eventReactive(input$RNN.1.RBM.go,{
    withProgress(message = "Work in progress: Second layer", value=0, {
      RNN.1.RBM.data.up.layer.1() %>%
        rbm.train(hidden=input$RNN.1.RBM.hidden_2,
                numepochs = input$RNN.1.RBM.numepochs,
                batchsize = input$RNN.1.RBM.batchsize,
                momentum = input$RNN.1.RBM.momentum,
                learningrate = input$RNN.1.RBM.learningrate,keep.data=F,verbose=T)
    })
  })
  RNN.1.RBM.data.up.layer.2 <- reactive({
    rbm.up(RNN.1.RBM.model.2(),RNN.1.RBM.data.up.layer.1())
  })
  output$RNN.1.raster_4 <- renderPlot({
    par(mar=c(0,0,4,0))
    RNN.1.data.test.raw() %>% raster(main='Original multi channel')
  })
  output$RNN.1.raster_5 <- renderPlot({
    par(mar=c(0,0,4,0))
    RNN.1.RBM.data.up.layer.1() %>% t %>% normalize %>% raster(main='hidden unit after alignment - first layer')
  })
  output$RNN.1.raster_6 <- renderPlot({
    par(mar=c(0,0,4,0))
    RNN.1.RBM.data.up.layer.2() %>% t %>% normalize %>%raster(main='hidden unit after alignment - second layer')
  })
#### TSNE.1 ####
  output$TSNE.1.option <- renderTable({
    data <- data.frame(Name = c('initial_dims','perplexity','max_iter'),
                       Value = c(30,30,300)
    )
    if(readonly == F){
      data[,2] <- paste0("<input id='TSNE.1.",data[,1],"' class='shiny-bound-input' type='number' value='",data[,2],"'>") # readonly='readonly'
    }else{
      data[,2] <- paste0("<input id='TSNE.1.",data[,1],"' class='shiny-bound-input' type='number' readonly='readonly' value='",data[,2],"'>") #
    }

    data
  }, sanitize.text.function = function(y) y)
  output$TSNE.1.raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    SOM.cluster.plot.picture(data.raw(),main='Original data')
  })
  TSNE.1.model <- eventReactive(input$TSNE.1.go,{
    withProgress(message = "Learning", value=0, {
      decon <- deconstruct(data = data.raw(),margin = 2,transform = T)
      tsne(decon,initial_dims = input$TSNE.1.initial_dims, perplexity = input$TSNE.1.perplexity, max_iter = input$TSNE.1.max_iter)
    })
  })
  output$TSNE.1.scores_1 <- renderPlot({
    plot(TSNE.1.model()[,1],TSNE.1.model()[,2],xlab='Dimension 1',ylab='Dimension 2',main='Brush to subset the above picture')
  })
  output$TSNE.1.raster_1 <- renderPlot({
    # print(str(input$brush.PCA.1.scores_1))
    # brush <- lapply(input$brush.PCA.1.scores_1,as.numeric)
    brush <- input$brush.TSNE.1.scores_1
    score <- TSNE.1.model()
    truc <- which(!(score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
    data <- data.raw()
    data[,truc,] <- 0
    data %>% raster(main='Seleted points')
  })
#### END ####
})
