# tlc_RBM_classification module
library(shiny)
library(shinydashboard)
library(DLC)

tlc_RBM_classificationUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
      tabsetPanel(
                  tabPanel('Input',
                           fileInput(ns('FilePicture'),'Train Files',multiple=T),
                           fileInput(ns('FilePicture_test'),'Test Files',multiple=T),
                           # selectizeInput(ns('format'),'Picture(s) format',choices=c('jpeg','tiff','png'),selected='jpeg'),
                           numericInput(ns('height'),'Height to redimension',126),
                           checkboxInput(ns('normalize'),'Normalize the picture(s)',T),
                           checkboxInput(ns('online'),'Train picture after picture, so called Online learning',T),
                           actionButton(ns('go'),'Analyze'),
                           numericInput(ns('layers'),'Number of hidden layers',3),
                           uiOutput(ns('hidden')),
                           numericInput(ns('numepochs'),'Number of epoch',50),
                           numericInput(ns('batchsize'),'batchsize',10),
                           numericInput(ns('momentum'),'momentum',0.5),
                           numericInput(ns('learningrate'),'learningrate',0.8),
                           numericInput(ns('learningrate_scale'),'learningrate scale',0.97),
                           numericInput(ns('cd'),'Constrastive Divergence',1)
                           ),
                  tabPanel('Info',
                           h4('Intro'),
                           p('This pipeline use restricted boltzmann machine stacked together (Deep believe net), to learn the intrinsics features of chromatograms in HPTLC plates'),
                           p('It is possible to teach the network one plate at a time, so called, online learning, in order to help the system generalize and avoid overfitting'),
                           p('There is the possibility to add a test set plate to evaluate the model for different options'),
                           p('Once again, the only preprocecing available is the normalization'),
                           p('In the futur, we will investigate the effect of data modification to increase the size of the dataset, like shifting, attenuate, skewding')
                           ),
                  tabPanel('Links',
                           br(),
                           HTML('<a href="mailto:dimitrifichou@gmail.com?Subject=tlc_denoising" target="_top">contact</a>'),br(),br(),
                           HTML('<a href="http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf" target="_blank">Paper</a>'),br(),br(),
                           tags$a(href='https://en.wikipedia.org/wiki/Restricted_Boltzmann_machine',target="_blank", "Wikipedia"),br(),br(),
                           tags$a(href='https://cran.r-project.org/web/packages/deepnet/deepnet.pdf', "R package"),
                           HTML('<a href="http://www.cs.toronto.edu/~hinton/absps/fastnc.pdf" target="_blank">DBN Paper</a>'),br(),br(),
                           tags$a(href='https://en.wikipedia.org/wiki/Deep_belief_network',target="_blank", "DBN Wikipedia"),br(),br()
                           ),
                  tabPanel('Download',
                           p('download button incoming')
                           )
                  )
    ),
    mainPanel(width = 9,
      plotOutput(ns('raster_0'),width='auto',height='650px'),br(),
      plotOutput(ns('network_0'),click=ns('click.network_0'),dblclick=ns('dblclick.network_0'),height = "200px")
    )
  )
}

tlc_RBM_classificationServer <- function(input,output,session){
  output$hidden <- renderUI({
    ns <- session$ns
    default <- c(16,4,1,1,1,1,1,1)
    x <- list()
    for(i in seq(input$layers)){
        x[[i]] <- numericInput(inputId = ns(paste0('hidden',i)),label = paste0('Number of unit in the layer number ',i),value = default[i])
    }
    return(tagList(x))
  })
  data_train_raw <- reactive({
    # here the 6 pictures
    withProgress(message = "Reading image", value=0, {
      if(is.null(input$FilePicture)){
        path <- paste0('www/propolis-silicate-',seq(6),'.jpg')
      }else{
        path <- input$FilePicture$datapath
      }
      data <- f.read.image(path,height=input$height,Normalize = input$normalize,ls.format=T)
    })
    return(data)
  })


  data_test_raw <- reactive({
    # here the 7 picture
    withProgress(message = "Reading image", value=0, {
      if(is.null(input$FilePicture)){
        path <- paste0('www/propolis-silicate-',7,'.jpg')
      }else{
        path <- input$FilePicture_test$datapath
      }
      if(is.null(path)){
        return(c()) # take the case where no test file is upload, need to verify this
      }else{
        data <- f.read.image(path,height=input$height,Normalize = input$normalize)
      }

    })
    return(data)
  })
  data_test_tot_raw <- reactive({
    if(is.null(input$FilePicture)){
      data <- abind(data_train_raw(),along=2)
      data <- abind(data,data_test_raw(),along=2)
    }
    if(!is.null(input$FilePicture) & is.null(input$FilePicture_test)){
      data <- abind(data_train_raw(),along=2)
    }
    if(!is.null(input$FilePicture) & !is.null(input$FilePicture_test)){
      data <- abind(data_train_raw(),along=2)
      data <- abind(data,data_test_raw(),along=2)
    }
    data
  })

  models <- eventReactive(input$go,{
    set.seed(1)
    withProgress(message = "Model training", value=0, {
      models <- list()
      verbose=F
      if(input$online == F){
        incProgress(0,message='Pictures deconstruction')
        vis <- data_train_raw() %>% abind(along=2) %>% deconstruct(2,T)
        for(i in seq(input$layers)){
          incProgress(1/input$layers,message = paste0('layer ',i,' training'))
          models[[i]] <- rbm.train(vis,input[[paste0('hidden',i)]],verbose=verbose,
                                   numepochs = input$numepochs,learningrate = input$learningrate,
                                   learningrate_scale = input$learningrate_scale,batchsize = input$batchsize,
                                   momentum = input$momentum,cd = input$cd)
          vis <- rbm.up(models[[i]],vis)
        }
      }else{
        for(j in seq(length(data_train_raw()))){
          incProgress(1/(input$layers*length(data_train_raw())+length(data_train_raw())),message='Picture deconstruction')
          vis <- data_train_raw()[[j]] %>% deconstruct(2,T)
          for(i in seq(input$layers)){
            incProgress(1/(input$layers*length(data_train_raw())+length(data_train_raw())),message = paste0('layer ',i,' - picture ',j,' training'))
            if(j == 1){ # First picture
              models[[i]] <- rbm.train(x = vis,hidden = input[[paste0('hidden',i)]],verbose=verbose,
                                       numepochs = input$numepochs,learningrate = input$learningrate,
                                       learningrate_scale = input$learningrate_scale,batchsize = input$batchsize,
                                       momentum = input$momentum,cd = input$cd)
            }else{ # other pictures, we take the model created from the first training
              models[[i]] <- rbm.train(x = vis,hidden = input[[paste0('hidden',i)]],model = models[[i]],verbose=verbose,
                                       numepochs = input$numepochs,batchsize = input$batchsize) # need other options, important to feed with the last model for the online aspect
            }
            vis <- rbm.up(models[[i]],vis)
          }
        }
      }
    })
    return(models)
  })

  output$test_print <- renderPrint({
    print(str(models()))
  })

  border <- reactive({
    dim(abind(data_train_raw(),along=2))[2]
  })

  output$raster_0 <- renderPlot({
    par(mfrow=c(3,1),mar=c(0,0,0,0),oma=c(0,0,2,0),xaxt='n',yaxt='n')
    raster(data_test_tot_raw())
    abline(v=border(),col='red')
    if(is.null(input$click.network_0)){
      y_up <- input$layers+1
    }else{
      click <- input$click.network_0
      y_up <- round(as.numeric(click[2]))
    }
    data <- data_test_uping()[[y_up]] %>% t
    raster(data)
    abline(v=border(),col='red')
    if(is.null(input$click.network_0)){
      y_up <- input$layers+1
    }else{
      click <- input$click.network_0
      y_up <- round(as.numeric(click[2]))
    }
    if(is.null(input$dblclick.network_0)){
      y_down <- 1
    }else{
      dblclick <- input$dblclick.network_0
      y_down <- round(as.numeric(dblclick[2]))
    }
    data <- data_test_downing()[[y_up]][[y_up-y_down+1]]
    if(y_down == 1){
      data <- data %>% reconstruct(2,T,dim(data_test_tot_raw()))
    }else{
      data <- data %>% t
    }
    raster(data)
    abline(v=border(),col='red')
    title(main='Top: Original data - Middle: Acending data - Bottom: Descending data',outer = T)
  })
  output$network_0 <- renderPlot({
    unit <- c(input$height*3)
    for(i in seq(input$layers)){
      unit <- c(unit,input[[paste0('hidden',i)]])
    }
    par(mfrow=c(1,1),mar=c(0,0,4,0),oma=c(0,0,0,0),xaxt='n',yaxt='n')
    layers <- 1 + input$layers
    plot(x=c(0,1),y=c(0,layers+1),type='n',main='Network representation: \n Click to see the picture ascending to a layer\n dblclick to see the picture descending from the clicked layer to the dblclicked layer')
    symbols(rep(0.5,layers),y=seq(layers),rectangles=matrix(c(rep(0.5,layers),rep(0.5,layers)),nrow=layers,ncol=2,byrow = F),inches=F,add=T)
    text(rep(0.5,layers),y=seq(layers),labels = paste0('Layer ',seq(0,layers-1,length.out = layers),' - ',unit,' units'))
  })


  data_test_tot_decon <- reactive({
    data_test_tot_raw() %>% deconstruct(2,T)
  })

  data_test_uping <- reactive({
    ls <- list(data_test_tot_decon())
    for(i in seq(input$layers)){
      ls[[i+1]] <- ls[[i]] %>% rbm.up(models()[[i]],.)
    }
    return(ls)
  })
  data_test_downing <- reactive({
    "we want to stock each possibility in a nested list,
    first nest will be the ascending layer, default: size = 3, starting with the layer 0
    second nest will be the descending layers, default: size = 1,2,3, starting with the upper layer which is data_test_uping"
    ls <- list()
    for(i in seq(input$layers+1)){
      ls[[i]] <- list()
      for(j in seq(i)){
        if(j == 1){
          ls[[i]][[j]] <- data_test_uping()[[i]]
        }else{
          ls[[i]][[j]] <- ls[[i]][[j-1]] %>% rbm.down(models()[[i-j+1]],.)
        }
      }
    }
    return(ls)
  })
}
