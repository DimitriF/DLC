# tlc_denoising module
# rsconnect::deployApp("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/",account="dimitrif",appName="tlc_denoising")
# shiny::runApp("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/",launch.browser = T)
library(shiny)
library(shinydashboard)

tlc_denoisingUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
      tabsetPanel(
                  tabPanel('input',
                           fileInput(ns('FilePicture'),'Upload your own picture(s) or let empty to use the demo file',multiple=T),
                           numericInput(ns('height'),'Height to redimension',256),
                           checkboxInput(ns('normalize'),'Normalize the picture(s)',T),
                           actionButton(ns('go'),'Analyze'),
                           # uiOutput(ns('Options'))
                           numericInput(ns('conv_width'),'Pixel windows for the patch',3),
                           numericInput(ns('hidden'),'Number of hidden unit in the layer',4),
                           numericInput(ns('numepochs'),'Number of epoch',5),
                           numericInput(ns('batchsize'),'batchsize',1000),
                           numericInput(ns('momentum'),'momentum',0.5),
                           numericInput(ns('learningrate'),'learningrate',0.1)
                           ),
                  tabPanel('Info',
                           h4('Intro'),
                           p('This tool use neural network to reproduce the way the brain see a noisy HPTLC picture in order to remove it'),
                           p('The process contain 5 phases:'),
                           p('read the picture'),
                           p('deconstruct the array'),
                           p('train the network'),
                           p('cross the network with the data to denoise them'),
                           p('reconstruct the array'),
                           h4('Algorithm:'),
                           p('RBM: an algorithm witch encode the input into a given amount of hidden unit and try to reproduce the input from it'),
                           h4('Plots:'),
                           p('image: original'),
                           p('image: reconstruct from hidden layer'),
                           p('chromatograms: input and output data'),
                           h4('Options:'),
                           p('Pixel windows for the patch: 5*5'),
                           p('hidden:	number of hidden units'),
                           p('numepochs:	number of iteration for samples'),
                           p('batchsize: size of mini-batch'),
                           p('learningrate:	learning rate for gradient descent.'),
                           p('momentum:	momentum for gradient descent.'),
                           h4('Integration tab'),
                           p("The integration tab allows to integrate the volume under the curve,
                              click on the normalised picture to select a zone of interest,
                             choose the dimension of the zone and observe the table"),
                           p("For now, copy paste the table, later, we'll implement in tool calibration and plotting")
                           ),
                  tabPanel('Links',
                           br(),
                           HTML('<a href="mailto:dimitrifichou@gmail.com?Subject=tlc_denoising" target="_top">contact</a>'),br(),br(),
                           HTML('<a href="http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf" target="_blank">Paper</a>'),br(),br(),
                           tags$a(href='https://en.wikipedia.org/wiki/Restricted_Boltzmann_machine',target="_blank", "Wikipedia"),br(),br(),
                           tags$a(href='https://cran.r-project.org/web/packages/deepnet/deepnet.pdf', "R package")
                           ),
                  tabPanel('Download',
                           p('download button incoming')
                           )
                  )
    ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel(title = "Picture",
                         plotOutput(ns('raster_0'),brush = brushOpts(id=ns("brush.raster_0"),resetOnNew = T),dblclick = ns("dblclick.raster_0")),br(),
                         plotOutput(ns('raster_1'),click=ns('click.raster_1')),br(),
                         plotOutput(ns('chromato_1'),height='600px'),br()
                ),
                tabPanel(title = "Integration",
                         plotOutput(ns("raster_2"),click=ns("click.raster_2"),hover = ns("hover.raster_2")),
                         plotOutput(ns("raster_2_zoom")),
                         column(width=4,
                                actionButton(ns('reset_table'),"Reset table"),
                                numericInput(ns("integration_width"),"Number of pixels to take in width for the integration",30),
                                numericInput(ns("integration_height"),"Number of pixels to take in height for the integration",15)
                         ),
                         column(width=8,
                                tableOutput(ns("table_1"))
                         )
                ),
                tabPanel("Network exploration",
                         h2("synthetic patches"),
                         plotOutput(ns("network_0"),click=ns("click.network_0")),
                         plotOutput(ns("raster_network_0")),
                         h2("specific patch state"),
                         plotOutput(ns("raster_network_1"),brush = brushOpts(id=ns("brush.raster_network_1"),resetOnNew = T),
                                    dblclick = ns("dblclick.raster_network_1")),
                         plotOutput(ns("raster_network_1_original"),click = ns("click.raster_network_1"))
                         ),
                tabPanel(title="3D",
                         sliderInput(ns("theta_3D"),"theta",min=0,max=360,value = 45,step=1),
                         sliderInput(ns("phi_3D"),"phi",min=0,max=360,value = 45,step=1),
                         selectInput(ns("channel_3D"),"Channel to use",choices=seq(3)),
                         column(width=6,
                                plotOutput(ns("plot3D_raw"))
                                ),
                         column(width=6,
                                plotOutput(ns("plot3D_process"))
                                )
                         )
              )
    )
  )
}

tlc_denoisingServer <- function(input,output,session){
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
    y <- ceiling(as.numeric(hover[2]))
    par(mar=c(0,0,0,0))
    data.process() %>% normalize %>%
      raster(xlim=c(x-input$integration_width,x+input$integration_width),
             ylim=c(y-input$integration_height,y+input$integration_height))
    abline(v=c(x-input$integration_width/2,x+input$integration_width/2),col="red")
    abline(h=c(y-input$integration_height/2,y+input$integration_height/2),col="red")
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
    truc <- integration_prep()
    truc$ymin <- dim(data.raw())[1] - truc$ymin
    truc$ymax <- dim(data.raw())[1] - truc$ymax

    truc$Red_sum <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],1])})
    truc$Red_sum_corrected <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],1] - mean(data.process()[c(x[3],x[4]),c(x[1],x[2]),1]))})
    truc$Red_max <- apply(truc[,3:6],1,function(x){max(data.process()[x[3]:x[4],x[1]:x[2],1])})
    truc$Green_sum <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],2])})
    truc$Green_sum_corrected <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],2] - mean(data.process()[c(x[3],x[4]),c(x[1],x[2]),2]))})
    truc$Green_max <- apply(truc[,3:6],1,function(x){max(data.process()[x[3]:x[4],x[1]:x[2],2])})
    truc$Blue_sum <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],3])})
    truc$Blue_sum_corrected <- apply(truc[,3:6],1,function(x){sum(data.process()[x[3]:x[4],x[1]:x[2],3] - mean(data.process()[c(x[3],x[4]),c(x[1],x[2]),3]))})
    truc$Blue_max <- apply(truc[,3:6],1,function(x){max(data.process()[x[3]:x[4],x[1]:x[2],3])})
    truc[,7:ncol(truc)]
  })

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
        data <- f.read.image('www/bioassay-1.jpg',height = input$height,Normalize = input$normalize)
      }else{
        validate(
          need(input$FilePicture != "", "Please upload a picture")
        )
        data <- f.read.image(input$FilePicture$datapath,height = input$height,Normalize = input$normalize)
      }
    })
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
      need(input$conv_width != 0, "Please choose a odd value for the pixel window")
    )
    data <- data.raw() %>% deconstruct.convol(margin=3,transform = F,conv_width = (input$conv_width-1)/2)
    # data <- data.raw() %>% deconstruct.convol(margin=3,transform = F,conv_width = 2)
    print(dim(data))
    data
  })
  output$raster_0 <- renderPlot({
    par(mar=c(0,0,4,0))
    raster(data.raw(),main='Original data')
  })
  model <- eventReactive(input$go,{
    withProgress(message = "Training network", value=0, {
      data <- data.raw.decon()
      model <- rbm.train(data,
                         hidden=input$hidden,
                         numepochs = input$numepochs,
                         batchsize = input$batchsize,
                         momentum = input$momentum,
                         learningrate = input$learningrate,verbose = T)
      return(model)
    })
  })
  data.up.recon <- reactive({
    model <- model()
    data.raw.decon() %>% rbm.up(model,.) %>% reconstruct(3,F,dimension = dim(data.raw()))
  })
  data.process <- reactive({
    withProgress(message = "Reconstructing picture", value=0, {
      model <- model()
      data.raw.decon() %>% rbm.up(model,.)  %>% rbm.down(model,.) %>% reconstruct.convol(margin=3,transform = F,dimension = dim(data.raw()),conv_width = (input$conv_width-1)/2,take_center = F)
      # data.raw.decon() %>% rbm.up(model,.)  %>% rbm.down(model,.) %>% reconstruct.convol(margin=3,transform = F,dimension = dim(data.raw()),conv_width = 2,take_center = F)
    })
  })
  output$raster_1 <- renderPlot({
    validate(
      need(input$go, "Click on the Analyze button")
    )
    par(mar=c(0,0,4,0))
    data.process() %>% raster(main='Reconstructed data')
  })
  output$chromato_1 <- renderPlot({
    validate(
      need(input$click.raster_1 != "", "Click on the picture to see the chromatograms")
    )
    click <- input$click.raster_1
    x <- ceiling(as.numeric(click[1]))
    par(mfrow=c(2,1),mar=c(0,0,0,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    ## Original
    chrom.pict(data.raw(),x)
    ## Reconstruct
    chrom.pict(data.process(),x)
  })


  output$plot3D_raw <- renderPlot({
    df <- data.raw()[,,as.numeric(input$channel_3D)]
    fields::drape.plot(1:nrow(df), 1:ncol(df), df, border=NA, theta = as.numeric(input$theta_3D), phi=as.numeric(input$phi_3D))
  },height=800)
  output$plot3D_process <- renderPlot({
    df <- data.process()[,,as.numeric(input$channel_3D)]
    fields::drape.plot(1:nrow(df), 1:ncol(df), df, border=NA, theta=as.numeric(input$theta_3D), phi=as.numeric(input$phi_3D))
  },height=800)
}
