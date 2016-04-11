# tlc_denoising module
# rsconnect::deployApp("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/",account="dimitrif",appName="tlc_denoising")
# shiny::runApp("/home/clau/Dropbox/DLC/inst/shinyapps/tlc_denoising/",launch.browser = T)
library(shiny)
library(shinydashboard)
library(DLC)

tlc_var_selectorUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
      tabsetPanel(
                  tabPanel('input',
                           fileInput(ns('FilePicture'),'Upload your own picture(s) or let empty to use the demo file',multiple=T),
                           numericInput(ns('height'),'Height to redimension',256),
                           checkboxInput(ns('normalize'),'Normalize the input picture',T),
                           checkboxInput(ns('scale_var_selector'),'Scale the data for the variable selection step',T),
                           checkboxInput(ns('scale_sample_cluster'),'Scale the data for the sample clusterisation step',T),
                           # actionButton(ns('go'),'Analyze'),
                           plotOutput(ns('raster_original')),br()
                           ),
                  tabPanel('Links',
                           br(),
                           HTML('<a href="mailto:dimitrifichou@gmail.com?Subject=tlc_var_selector" target="_top">contact</a>'),br(),br(),
                           HTML('<a href="http://www.cs.toronto.edu/~hinton/absps/guideTR.pdf" target="_blank">Paper</a>'),br(),br(),
                           tags$a(href='https://en.wikipedia.org/wiki/Restricted_Boltzmann_machine',target="_blank", "Wikipedia"),br(),br(),
                           tags$a(href='https://cran.r-project.org/web/packages/deepnet/deepnet.pdf', "R package")
                           ),
                  tabPanel('Download',
                           p("not yet")
                           # downloadButton(ns("downloadPicture"))
                           )
                  )
    ),
    mainPanel(width = 9,
              column(6,
                     h3("Variable selection"),
                     radioButtons(ns("radio_scatterplot_var_selector_1"),label = "PC for X axis",choices = seq(10),selected = 1,inline = T),
                     radioButtons(ns("radio_scatterplot_var_selector_2"),label = "PC for Y axis",choices = seq(10),selected = 2,inline = T),
                     plotOutput(ns('scatterplot_var_selector'),brush = brushOpts(id=ns("brush.scatterplot_var_selector"),resetOnNew = F),dblclick = ns("dblclick.scatterplot_var_selector")),br(),
                     plotOutput(ns('raster_var_selector')),br(),
                     plotOutput(ns("lineplot_var_selector"))
              ),
              column(6,
                     h3("Sample analysis"),
                     radioButtons(ns("radio_scatterplot_sample_cluster_1"),label = "PC for X axis",choices = seq(10),selected = 1,inline = T),
                     radioButtons(ns("radio_scatterplot_sample_cluster_2"),label = "PC for Y axis",choices = seq(10),selected = 2,inline = T),
                     plotOutput(ns('scatterplot_sample_cluster'),brush = brushOpts(id=ns("brush.scatterplot_sample_cluster"),resetOnNew = F)),br(),
                     plotOutput(ns('raster_sample_cluster')),br()
              )
    )
  )
}

tlc_var_selectorServer <- function(input,output,session){
  data.raw <- reactive({
    withProgress(message = "Reading image", value=0, {
      if(is.null(input$FilePicture)){
        data <- f.read.image('www/rTLC_demopicture.JPG',height = input$height,Normalize = input$normalize)
      }else{
        validate(
          need(input$FilePicture != "", "Please upload a picture")
        )
        data <- f.read.image(input$FilePicture$datapath,height = input$height,Normalize = input$normalize)
      }
    })
    return(data)
  })

  output$raster_original <- renderPlot({
    par(mar=c(2,2,2,0))
    data.raw() %>% raster(main = "original")
  })

  decon_var_selector <- reactive({
    data.raw() %>% deconstruct(1,T)
  })
  decon_sample_cluster <- reactive({
    if(is.null(var_selected$values) | sum(var_selected$values) == 0){
      data.raw() %>% deconstruct(2,T)
    }else{
      data.raw()[var_selected$values,,] %>% deconstruct(2,T)
    }

  })

  model_var_selector <- reactive({
    data <- decon_var_selector()
    if(input$scale_var_selector == T){data <- scale(data)}
    PCA(data)
  })

  output$scatterplot_var_selector <- renderPlot({
    par(mgp=c(2,1,0),mar=c(3,3,3,0))
    model <- model_var_selector()
    score <- model$scores[,c(as.numeric(input$radio_scatterplot_var_selector_1),as.numeric(input$radio_scatterplot_var_selector_2))]
    score %>% plot(type="n",main="PCA with Rf, i.e. horizontale lines\nbrush to turn red")
    col.selected <- rep("black",input$height)
    if(!is.null(var_selected$values)){
      col.selected[var_selected$values] <- "red"
    }
    text(x=score[,1],y=score[,2],labels=c(input$height:1),col=col.selected)
  })

  output$raster_var_selector <- renderPlot({
    par(mar=c(0,0,2,0))
    # model <- model_var_selector()
    # brush <- input$brush.scatterplot_var_selector
    # score <- model$scores[,c(as.numeric(input$radio_scatterplot_var_selector_1),as.numeric(input$radio_scatterplot_var_selector_2))]
    # truc <- which(!(score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
    data <- data.raw()
    data[!var_selected$values,,] <- 0
    par(xaxt='n',yaxt='n')
    data %>% raster(main='Seleted Rf')
  })

  output$lineplot_var_selector <- renderPlot({
    par(mfrow=c(2,1),xaxt="n",mar=c(0,0,0,0))
    model <- model_var_selector()
    loading <- model$loadings[,c(as.numeric(input$radio_scatterplot_var_selector_1),as.numeric(input$radio_scatterplot_var_selector_2))]
    matplot(matrix(loading[,1],nrow = dim(data.raw())[2],ncol=3),type="l",col=c("red","green","blue"))
    matplot(matrix(loading[,2],nrow = dim(data.raw())[2],ncol=3),type="l",col=c("red","green","blue"))
  })

  var_selected <- reactiveValues(values=NULL)
  var_selected.height.obs <- observeEvent(input$height, {
    var_selected$values <- rep(F,input$height)
  })
  var_selected.dblclick.obs <- observeEvent(input$dblclick.scatterplot_var_selector, {
    var_selected$values <- rep(F,input$height)
  })
  var_selected.brusht.obs <- observeEvent(input$brush.scatterplot_var_selector, {
    model <- model_var_selector()
    brush <- input$brush.scatterplot_var_selector
    score <- model$scores[,c(as.numeric(input$radio_scatterplot_var_selector_1),as.numeric(input$radio_scatterplot_var_selector_2))]
    truc <- which((score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
    var_selected$values[truc] <- T
  })

  model_sample_cluster <- reactive({
    data <- decon_sample_cluster()
    if(input$scale_sample_cluster == T){data <- scale(data)}
    PCA(data)
  })

  output$scatterplot_sample_cluster <- renderPlot({
    par(mgp=c(2,1,0),mar=c(3,3,3,0))
    model <- model_sample_cluster()
    score <- model$scores[,c(as.numeric(input$radio_scatterplot_sample_cluster_1),as.numeric(input$radio_scatterplot_sample_cluster_2))]
    score %>% plot(main="PCA with samples, i.e. verticale lines\nonly the red Rf in the other PCA are used",pch="+")
  })

  output$raster_sample_cluster <- renderPlot({
    par(mar=c(0,0,2,0))
    model <- model_sample_cluster()
    brush <- input$brush.scatterplot_sample_cluster
    score <- model$scores[,c(as.numeric(input$radio_scatterplot_sample_cluster_1),as.numeric(input$radio_scatterplot_sample_cluster_2))]
    truc <- which(!(score[,1] > brush$xmin & score[,1] < brush$xmax & score[,2] > brush$ymin & score[,2] < brush$ymax))
    data <- data.raw()
    data[,truc,] <- 0
    par(xaxt='n',yaxt='n')
    data %>% raster(main='Seleted points')
  })
}
