# tlc_denoising module

## this file contain several deprecated object in teh server part.

library(DLC)

tlc_denoisingUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
                 tabsetPanel(
                   tabPanel('input',
                            fileInput(ns('FilePicture'),'Select a chromatogram image',multiple=T),
                            numericInput(ns('height'),'Pixel height',256),
                            checkboxInput(ns('negative'),'Use negative peak inversion',F),
                            checkboxInput(ns('normalize'),'Normalize the input picture',T),
                            checkboxInput(ns('normalize.output'),'Normalize the output picture',F),
                            actionButton(ns('go'),'Analyze'),
                            # uiOutput(ns('Options'))
                            numericInput(ns('conv_width'),'Pixel windows for the patch',3),
                            numericInput(ns('hidden'),'Number of hidden unit in the layer',4),
                            numericInput(ns('numepochs'),'Number of epoch',5),
                            numericInput(ns('batchsize'),'Batchsize',1000),
                            numericInput(ns('momentum'),'Momentum',0.5),
                            numericInput(ns('learningrate'),'Learningrate',0.1),
                            numericInput(ns('cd'),'Contrastive divergence',2)

                   ),
                   tabPanel('Links',
                            br(),
                            HTML('<a href="mailto:dimitrifichou@gmail.com?Subject=tlc_denoising" target="_top">Contact</a>'),br(),br(),
                            HTML('<a href="http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf" target="_blank">Paper on RBM</a>'),br(),br(),
                            tags$a(href='https://en.wikipedia.org/wiki/Restricted_Boltzmann_machine',target="_blank", "Wikipedia"),br(),br(),
                            tags$a(href='https://github.com/DimitriF/DLC',target="_blank", "GitHub")
                   ),
                   tabPanel('Download',
                            downloadButton(ns("downloadPicture"),label = "Download pictures")
                   )
                 )
    ),
    mainPanel(width = 9,
              plotOutput(ns('raster_0'),brush = brushOpts(id=ns("brush.raster_0"),resetOnNew = F),dblclick = ns("dblclick.raster_0")),br(),
              plotOutput(ns('raster_1'),click=ns('click.raster_1')),br(),
              # selectizeInput(ns("raster_hidden_select"),label="Hidden unit to see",choices=seq(10),selected=1),
              # selectizeInput(ns("raster_hidden_select"),label="Hidden unit to see",choices=seq(input$hidden),selected=1),
              uiOutput(ns("raster_hidden_select_UI")),
              plotOutput(ns('raster_hidden'),click=ns('click.raster_hidden')),br(),
              plotOutput(ns("model.plot.1")),br()
    )
  )
}

tlc_denoisingServer <- function(input,output,session){
  output$raster_hidden_select_UI <- renderUI({
    ns <- session$ns
    selectizeInput(ns("raster_hidden_select"),label="Hidden unit to see",choices=seq(input$hidden),selected=1)
    # selectizeInput("raster_hidden_select",label="Hidden unit to see",choices=seq(input$hidden),selected=1)
  })
  session$onSessionEnded(function() {
    click.raster_2.reset.obs$suspend()
    click.raster_2.update.obs$suspend()
    # brush.raster_0.update.obs$suspend()
    # dblclick.raster_0.reset.obs$suspend()
  })
  click.network_0 <- reactiveValues(value=NULL,color=NULL,last=c(1))
  click.network_0.obs.1 <- observeEvent(input$go,{
    click.network_0$value <- rep(F,input$hidden)
    click.network_0$color <- rep("white",input$hidden)
  })
  click.network_0.obs.2 <- observeEvent(input$click.network_0,{
    x <- round(as.numeric(input$click.network_0$x)*input$hidden)
    y <- as.numeric(input$click.network_0$y)
    if(round(y) == 1){
      if(click.network_0$value[x] ==F){
        click.network_0$value[x] <-T
        click.network_0$color[x] <-"black"
      }else{
        click.network_0$value[x] <-F
        click.network_0$color[x] <-"white"
      }
      click.network_0$last <-c(x)
    }
  })
  output$network_0 <- renderPlot({
    model <- model()
    # load("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/www/model-store.Rdata")
    par(mfrow=c(1,1),mar=c(0,0,2,0))
    plot(c(-0.5,1+1/model$size[1]),c(-0.5,1.5), type='n',ylab="",xlab="",bty='n',axes=F,main='RBM network\nClick on a unit to activate or deactivate and synthetize a patch') #
    size.circle <- 1/max(model$size)/2.5
    text(x=rep(0,2),y=c(1,2),labels=c('Input\nunits','Hidden\nunits'))
    x_vis <- seq(1/model$size[1],1,length.out = model$size[1])
    x_hid <- seq(1/model$size[2],1,length.out = model$size[2])
    y_vis <- rep(0,model$size[1])
    y_hid <- rep(1,model$size[2])
    bg <- click.network_0$color
    if(is.null(bg)){bg <- rep("white",model$size[2])}
    symbols(x=x_vis,y=y_vis ,circles =rep(size.circle,model$size[1]),inches=F,add=T)
    symbols(x=x_hid,y=y_hid ,circles =rep(size.circle,model$size[2]),inches=F,add=T,bg=bg)
  })
  output$raster_network_0 <- renderPlot({
    unit_state_raster(as.numeric(click.network_0$value),model=model(),conv=(input$conv_width-1)/2,normalize=F,main=T)
  })
  brush.raster_network_1 <- reactiveValues(xmin=NULL,xmax=NULL,ymin=NULL,ymax=NULL)
  dblclick.raster_network_1.reset.obs <- observeEvent(input$dblclick.raster_network_1, {
    brush <- input$brush.raster_network_1
    if (!is.null(brush)) {
      brush.raster_network_1$xmin <- as.numeric(brush$xmin)
      brush.raster_network_1$ymin <- as.numeric(brush$ymin)
      brush.raster_network_1$xmax <- as.numeric(brush$xmax)
      brush.raster_network_1$ymax <- as.numeric(brush$ymax)
    } else {
      brush.raster_network_1$xmin <- NULL
      brush.raster_network_1$ymin <- NULL
      brush.raster_network_1$xmax <- NULL
      brush.raster_network_1$ymax <- NULL
    }
  })
  click.raster_network_1 <- reactiveValues(x=c(1))
  click.raster_network_1.obs <- observeEvent(input$click.raster_network_1,{
    click <- input$click.raster_network_1
    x <- ceiling(as.numeric(click[1]))
    click.raster_network_1$x <- c(x)
  })
  dblclick.raster_network_1.obs.reset <- observeEvent(input$dblclick.raster_network_1,{
    click.raster_network_1$x <- c(1)
  })

  output$raster_network_1 <- renderPlot({
    par(mar=c(0,0,0,0))
    channel <- click.network_0$last
    if(is.null(brush.raster_network_1$xmin)){
      data.up.recon()[,,channel] %>% normalize %>% chrom.raster(click.raster_network_1$x)
    }else{
      data.up.recon()[input$height-brush.raster_network_1$ymax:brush.raster_network_1$ymin,brush.raster_network_1$xmin:brush.raster_network_1$xmax,channel] %>%
        normalize %>% chrom.raster(click.raster_network_1$x)
    }
  })
  output$raster_network_1_original <- renderPlot({
    par(mar=c(0,0,0,0))
    if(is.null(brush.raster_network_1$xmin)){
      data.raw() %>% chrom.raster(click.raster_network_1$x)
    }else{
      data.raw()[input$height-brush.raster_network_1$ymax:brush.raster_network_1$ymin,brush.raster_network_1$xmin:brush.raster_network_1$xmax,] %>%
        normalize %>% chrom.raster(click.raster_network_1$x)
    }
  })

  output$raster_2 <- renderPlot({
    validate(
      need(input$go, "Click on the analyse button")
    )
    par(mar=c(0,0,4,0))
    data.process() %>% normalize %>% raster(main='Click on the center of a zone to add this point to the table\nNote that the plot is normalized for sensibility')
    if(length(click.raster_2$value) != 0){
      coord <- click.raster_2$value
      truc <- matrix(coord,length(coord)/2,2,byrow = T)
      text(x=truc[,2],y=truc[,1],label=seq(nrow(truc)),col="red")
      symbols(x=truc[,2],y=truc[,1],col="red",inches = F,add = T,rectangles = cbind(rep(input$integration_width,nrow(truc)),rep(input$integration_height,nrow(truc))))
    }
  })

  output$raster_2_zoom <- renderPlot({
    validate(
      need(input$go, "Click on the analyse button")
    )
    validate(
      need(input$hover.raster_2, "Hover the above picture to get a zoom")
    )
    hover <- input$hover.raster_2
    x <- ceiling(as.numeric(hover[1]))
    y <- dim(data.process())[1] - ceiling(as.numeric(hover[2]))
    par(mfrow=c(2,1),mar=c(0,0,0,0))
    data.process()[(y-input$integration_height):(y+input$integration_height),(x-input$integration_width):(x+input$integration_width),] %>%
      normalize %>% chrom.raster(x=input$integration_height*2)
    abline(v=c(input$integration_width/2,input$integration_width*3/2),col="red")
    abline(h=c(input$integration_height/2,input$integration_height*3/2),col="red")
    data.up.recon()[(y-input$integration_height):(y+input$integration_height),(x-input$integration_width):(x+input$integration_width),input$raster_hidden_select] %>%
      normalize %>% chrom.raster(x=input$integration_height*2)
    abline(v=c(input$integration_width/2,input$integration_width*3/2),col="red")
    abline(h=c(input$integration_height/2,input$integration_height*3/2),col="red")
  })
  click.raster_2 <- reactiveValues(value=c())
  click.raster_2.update.obs <- observeEvent(input$click.raster_2,{
    click <- input$click.raster_2
    x <- ceiling(as.numeric(click[1]))
    y <- ceiling(as.numeric(click[2]))
    click.raster_2$value <- c(click.raster_2$value,y,x)
  })
  click.raster_2.reset.obs <- observeEvent(input$reset_table,{
    click.raster_2$value <- c()
  })
  integration_prep <- reactive({
    validate(
      need(length(click.raster_2$value) != 0, "Click on the plot to select zones of interest")
    )
    coord <- click.raster_2$value
    truc <- matrix(coord,length(coord)/2,2,byrow = T) %>% as.data.frame
    colnames(truc) <- c("y","x")
    # truc$y <- dim(data.raw())[1] - truc$y
    # truc$x <- dim(data.raw())[2] - truc$x
    truc$xmin <- truc$x - input$integration_width/2
    truc$xmax <- truc$x + input$integration_width/2
    truc$ymin <- truc$y - input$integration_height/2
    truc$ymax <- truc$y + input$integration_height/2
    truc
  })
  output$table_1 <- renderTable({
    # This table only take the volume under the curve, little correction: we minus the mean of the four angles
    truc <- integration_prep()
    truc$ymin <- dim(data.raw())[1] - truc$ymin
    truc$ymax <- dim(data.raw())[1] - truc$ymax
    truc$Red_sum_corrected <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],1] - mean(data.process()[c(x[3],x[4]),c(x[1],x[2]),1]))})
    truc$Green_sum_corrected <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],2] - mean(data.process()[c(x[3],x[4]),c(x[1],x[2]),2]))})
    truc$Blue_sum_corrected <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],3] - mean(data.process()[c(x[3],x[4]),c(x[1],x[2]),3]))})
    for(i in seq(input$hidden)){
      truc[,paste0("hidden_",i)] <- apply(truc[,3:6],1,function(x){sum(data.up.recon()[x[3]:x[4],x[1]:x[2],i] - mean(data.up.recon()[c(x[3],x[4]),c(x[1],x[2]),i]))})
    }
    truc[,7:ncol(truc)]
  })

  output$table_2 <- renderTable({
    # This table do a more complex job. We extract each chromatogram, remove the baseline with the rolling ball algorithm, and integrate the area under the curve.
    truc <- integration_prep()
    truc$ymin <- dim(data.raw())[1] - truc$ymin
    truc$ymax <- dim(data.raw())[1] - truc$ymax
    dim.table <-truc[,3:6]
    data <- abind(data.process(),data.up.recon(),along=3)
    final <- f.integrate(data,dim.table,negatif = F,correct.baseline = T)
    colnames(final) <- colnames(final,do.NULL = F)
    colnames(final)[1:3] <- c("red","green","blue")
    colnames(final)[4:ncol(final)] <- paste0("hidden_",seq(dim(data)[3]-3))
    final
  },digits=5)

  output$table_3 <- renderTable({
    # This table do a more complex job. we go in negatif, We extract each chromatogram, remove the baseline with the rolling ball algorithm, and integrate the area under the curve.
    truc <- integration_prep()
    truc$ymin <- dim(data.raw())[1] - truc$ymin
    truc$ymax <- dim(data.raw())[1] - truc$ymax
    dim.table <-truc[,3:6]
    data <- abind(data.process(),data.up.recon(),along=3)
    final <- f.integrate(data,dim.table,negatif = T,correct.baseline = T)
    colnames(final) <- colnames(final,do.NULL = F)
    colnames(final)[1:3] <- c("red","green","blue")
    colnames(final)[4:ncol(final)] <- paste0("hidden_",seq(dim(data)[3]-3))
    final
  },digits=5)

  brush.raster_0 <- reactiveValues(xmin=NULL,xmax=NULL,ymin=NULL,ymax=NULL)
  dblclick.raster_0.reset.obs <- observeEvent(input$dblclick.raster_0, {
    brush <- input$brush.raster_0
    if (!is.null(brush)) {
      brush.raster_0$xmin <- as.numeric(brush$xmin)
      brush.raster_0$ymin <- as.numeric(brush$ymin)
      brush.raster_0$xmax <- as.numeric(brush$xmax)
      brush.raster_0$ymax <- as.numeric(brush$ymax)
    } else {
      brush.raster_0$xmin <- NULL
      brush.raster_0$ymin <- NULL
      brush.raster_0$xmax <- NULL
      brush.raster_0$ymax <- NULL
    }
  })
  data.raw <- reactive({
    withProgress(message = "Reading image", value=0, {
      if(is.null(input$FilePicture)){
        data <- f.read.image('www/bioassay-1.jpg',height = input$height)
      }else{
        validate(
          need(input$FilePicture != "", "Please upload a picture")
        )
        data <- f.read.image(input$FilePicture$datapath,height = input$height,Normalize = input$normalize)
      }
    })
    if(input$normalize){data=normalize(data)}
    if(!is.null(brush.raster_0$xmin)){
      # truc <- brush.raster_0
      # print(truc)
      # truc[3:4] <- dim(data)[1] - truc[3:4]
      data <- data[dim(data)[1]-brush.raster_0$ymax:brush.raster_0$ymin,brush.raster_0$xmin:brush.raster_0$xmax,]
    }
    return(data)
  })
  data.raw.decon <- reactive({
    validate(
      need(round(input$conv_width/2) != input$conv_width/2, "Please choose a odd value for the pixel window")
    )
    data <- data.raw() %>% deconstruct.convol(margin=3,transform = F,conv_width = (input$conv_width-1)/2)
    # data <- data.raw() %>% deconstruct.convol(margin=3,transform = F,conv_width = 2)
    print(dim(data))
    data
  })
  click.raster_0_1_2 <- reactiveValues(value=1)
  click.raster_0_1_2.obs_0 <- observeEvent(input$click.raster_0,{
    click.raster_0_1_2$value <- ceiling(as.numeric(input$click.raster_0[1]))
  })
  click.raster_0_1_2.obs_1 <- observeEvent(input$click.raster_1,{
    click.raster_0_1_2$value <- ceiling(as.numeric(input$click.raster_1[1]))
  })
  click.raster_0_1_2.obs_hidden <- observeEvent(input$click.raster_hidden,{
    click.raster_0_1_2$value <- ceiling(as.numeric(input$click.raster_hidden[1]))
  })
  output$raster_0 <- renderPlot({
    par(mar=c(0,0,2,0))
    chrom.raster(data.raw(),main='Original data',x = click.raster_0_1_2$value)
  })
  model <- eventReactive(input$go,{
    withProgress(message = "Training network", value=0, {
      data <- data.raw.decon()
      if(input$negative){data <- 1- data}
      set.seed(1)
      model <- rbm.train(data,
                         hidden=input$hidden,
                         numepochs = input$numepochs,
                         batchsize = input$batchsize,
                         momentum = input$momentum,
                         learningrate = input$learningrate,cd=input$cd,verbose = T)
      return(model)
    })
  })
  data.up.recon <- reactive({
    model <- model()
    data <- data.raw.decon() %>% rbm.up(model,.) %>% reconstruct(3,F,dimension = dim(data.raw())) %>% normalize
    data
  })
  data.process <- reactive({
    withProgress(message = "Reconstructing picture", value=0, {
      model <- model()
      data <- data.raw.decon() %>% rbm.up(model,.)  %>% rbm.down(model,.) %>% reconstruct.convol(margin=3,transform = F,dimension = dim(data.raw()),conv_width = (input$conv_width-1)/2,take_center = F)
      if(input$negative){data <- 1- data}
      if(input$normalize.output == T){
        data %>% normalize
      }else{
        data
      }
      # data.raw.decon() %>% rbm.up(model,.)  %>% rbm.down(model,.) %>% reconstruct.convol(margin=3,transform = F,dimension = dim(data.raw()),conv_width = 2,take_center = F)
    })
  })
  output$raster_1 <- renderPlot({
    validate(
      need(input$go, "Click on the Analyze button")
    )
    par(mar=c(0,0,2,0))
    data.process() %>% chrom.raster(main='Reconstructed data',x = click.raster_0_1_2$value)
  })
  output$raster_hidden <- renderPlot({
    validate(
      need(input$go, "Click on the Analyze button")
    )
    par(mar=c(0,0,2,0))
    data.up.recon()[,,input$raster_hidden_select] %>% chrom.raster(main=paste0('hidden unit ',input$raster_hidden_select),x = click.raster_0_1_2$value)
  })

  output$plot3D_raw <- renderPlot({
    df <- data.raw()[,,as.numeric(input$channel_3D)]
    fields::drape.plot(1:nrow(df), 1:ncol(df), df, border=NA, theta = as.numeric(input$theta_3D), phi=as.numeric(input$phi_3D))
  },height=800)

  output$plot3D_process <- renderPlot({
    df <- data.process()[,,as.numeric(input$channel_3D)]
    fields::drape.plot(1:nrow(df), 1:ncol(df), df, border=NA, theta=as.numeric(input$theta_3D), phi=as.numeric(input$phi_3D))
  },height=800)

  output$downloadPicture <- downloadHandler(
    filename = 'tlc_denoising.zip',
    content = function(file) {
      fs <- c()
      if(!is.null(input$FilePicture)){
        path <- paste0(input$FilePicture$name)
        fs <- c(fs,path)
        writeJPEG(data.raw(),target=path)
      }

      path <- paste0('recon.jpeg')
      fs <- c(fs,path)
      writeJPEG(data.process(),target=path)
      #data.up.recon()[,,input$raster_hidden_select]
      for(i in seq(dim(data.up.recon())[3])){
        path <- paste0("hidden_",i,'.jpeg')
        fs <- c(fs,path)
        writeJPEG(data.up.recon()[,,i] ,target=path)
      }

      tempFile <- tempfile(fileext = ".zip")
      zip(zipfile=tempFile, files=fs)
      file.rename(tempFile, file)
    },
    contentType = "application/zip"
  )
  output$model.plot.1 <- renderPlot({
    par(mfrow=c(1,2))
    truc <- PCA(scale(model()$W))$scores[,1:2]
    plot(x=truc[,1],y=truc[,2],type="n",main="Principal component analysis of the weight matrix",
         xlab="PC1",ylab="PC2")
    text(x=truc[,1],y=truc[,2],labels=seq(input$hidden),cex=1)
    plot(model()$iter_euclidean,type="l",main="Evolution of the euclidean distance",
         xlab="minibatch iteration",ylab="distance")
  })
}
