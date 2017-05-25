# tlc_denoising module
# rsconnect::deployApp("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/",account="dimitrif",appName="tlc_denoising")
# shiny::runApp("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/",launch.browser = T)
library(shiny)
library(shinydashboard)
library(DLC)

plate_predictionUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
       fileInput(ns('FilePicture'),'Upload your own picture(s) or let empty to use the demo file',multiple=T),
       numericInput(ns('height'),'Height to redimension',1024),
       # checkboxInput(ns('normalize'),'Normalize the input picture',T),
       numericInput(ns("track_width"),"track windows in pixels",value = 50),
       radioButtons(ns("class_to_label"),"class to label",choices = c("class 1","class 2","class 3"),selected = "class 1"),
       actionButton(ns('reset'),'Reset'),
       actionButton(ns('analyse'),'Analyze'),br(),br(),
       HTML('<a href="mailto:dimitrifichou@gmail.com?Subject=tlc_var_selector" target="_top">contact</a>')#,br(),br(),
       # HTML('<a href="http://link.springer.com/article/10.1023%2FA%3A1010933404324" target="_blank">Paper</a>'),br(),br(),
       # tags$a(href='https://en.wikipedia.org/wiki/Random_forest',target="_blank", "Wikipedia"),br(),br(),
       # tags$a(href='https://cran.r-project.org/web/packages/randomForest/index.html', "R package")
    ),
    mainPanel(width = 9,
              plotOutput(ns("raster_original"),height = 400,click=ns('click.raster_original')),
              plotOutput(ns("raster_prediction"),height=200),
              plotOutput(ns("varImp"),height=500)
    )
  )
}

plate_predictionServer <- function(input,output,session){
  data.raw <- reactive({
    withProgress(message = "Reading image", value=0, {
      if(is.null(input$FilePicture)){
        # data <- f.read.image('www/rTLC_demopicture_2_02',height = input$height,Normalize = input$normalize)
        data <- f.read.image('www/rTLC_demopicture_2_02',height = input$height,Normalize = F)
      }else{
        validate(
          need(input$FilePicture != "", "Please upload a picture")
        )
        # data <- f.read.image(input$FilePicture$datapath,height = input$height,Normalize = input$normalize)
        data <- f.read.image(input$FilePicture$datapath,height = input$height,Normalize = F)
      }
    })
    return(data)
  })

  output$raster_original <- renderPlot({
    par(mar=c(0,2,2,0))
    data.raw() %>% raster(yaxt="s")
    if(!is.null(train_set$class_1)){
      symbols(x=train_set$class_1,y=rep(input$height/2,length(train_set$class_1)),
              rectangles = matrix(rep(c(input$track_width,input$height),length(train_set$class_1)),nrow=length(train_set$class_1),ncol=2,byrow = T),
              inches = F,add = T,fg="red"
      )
    }
    if(!is.null(train_set$class_2)){
      symbols(x=train_set$class_2,y=rep(input$height/2,length(train_set$class_2)),
              rectangles = matrix(rep(c(input$track_width,input$height),length(train_set$class_2)),nrow=length(train_set$class_2),ncol=2,byrow = T),
              inches = F,add = T,fg="green"
      )
    }
    if(!is.null(train_set$class_3)){
      symbols(x=train_set$class_3,y=rep(input$height/2,length(train_set$class_3)),
              rectangles = matrix(rep(c(input$track_width,input$height),length(train_set$class_3)),nrow=length(train_set$class_3),ncol=2,byrow = T),
              inches = F,add = T,fg="blue"
      )
    }
  })

  decon <- reactive({
    decon <- data.raw() %>% deconstruct(2,T)
    colnames(decon) <- c(paste0("red_",c(nrow(data.raw()):1)),
                         paste0("green_",c(nrow(data.raw()):1)),
                         paste0("blue_",c(nrow(data.raw()):1)))
    decon
  })

  train_set <- reactiveValues(class_1 = NULL,class_2 = NULL,class_3=NULL)
  train_set.click <- observeEvent(input$click.raster_original,{
    click <- input$click.raster_original
    x <- ceiling(as.numeric(click[1]))
    if(input$class_to_label == "class 1"){
      train_set$class_1 <- c(train_set$class_1,x)
    }
    if(input$class_to_label == "class 2"){
      train_set$class_2 <- c(train_set$class_2,x)
    }
    if(input$class_to_label == "class 3"){
      train_set$class_3 <- c(train_set$class_3,x)
    }
  })
  train_set.reset <- observeEvent(input$reset,{
    train_set$class_1 <- NULL
    train_set$class_2 <- NULL
    train_set$class_3 <- NULL
  })

  model <- eventReactive(input$analyse,{
    Dep <- rep(F,nrow(decon()))
    for(i in train_set$class_1){
      Dep[(i-round(input$track_width/2)):(i+round(input$track_width/2))] <- 1
    }
    for(i in train_set$class_2){
      Dep[(i-round(input$track_width/2)):(i+round(input$track_width/2))] <- 2
    }
    for(i in train_set$class_3){
      Dep[(i-round(input$track_width/2)):(i+round(input$track_width/2))] <- 3
    }
    length(as.logical(Dep))
    decon <- decon()[as.logical(Dep),]
    Dep <- Dep[as.logical(Dep)]
    randomForest::randomForest(x=decon,y=as.factor(Dep))
  })

  pred <- reactive({
    predict(model(),decon(),type="prob")
  })
  output$raster_prediction <- renderPlot({
    validate(
      need(input$analyse,"Label at least two classes by clicking on the above picture and selecting the class to label in the left panel.\nThen click on the analyse button to train the model and predict the samples")
    )
    prediction <- pred() %>% classmat2classvec()
    pred <- array(0,dim=c(2,dim(data.raw())[2],3))
    pred[,prediction == 1,1] <- 1
    pred[,prediction == 2,2] <- 1
    pred[,prediction == 3,3] <- 1
    par(mar=c(0,2,0,0),xaxt="n",yaxt="n")
    plot(x=c(0,dim(data.raw())[2]),y=c(0,2),type="n",ylim=c(1,2)) # remove ylim for line plot
    pred %>% rasterImage(0,1,dim(data.raw())[2],2,interpolate=F)
    # for(i in seq(length(unique(prediction)))){
    #   par(new=T)
    #   couleur <- c("red","green","blue")[as.numeric(unique(prediction))[i]]
    #   plot(pred()[,i],ylim=c(0,2),type="l",col=couleur)
    # }
  })
  output$varImp <- renderPlot({
    randomForest::varImpPlot(model(),main = "")
  })
}
