# Supervised_systematic_propolis
library(shiny)
library(shinydashboard)
library(ggplot2)
load("www/20160422_RNN_supervised_systematic.RData")
unique_grid <- c()
for(i in seq(8)){unique_grid<-c(unique_grid,as.character(unique(grid[,i])))}
# unique_grid <- as.character(unique_grid)

Supervised_systematic_propolisUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
                 tabsetPanel(
                   tabPanel("plot variables",
                            radioButtons(ns("color"),"color",choices=colnames(grid)[1:8]),
                            radioButtons(ns("x"),"x",choices=colnames(grid)[1:8])
                            ),
                   tabPanel("subset",
                            p("dirty and confusing but it will have to do"),
                            checkboxGroupInput(ns("subset"),"check to remove",choices=c(unique_grid))
                            ),
                   tabPanel("plot_option",
                            radioButtons(ns("geom"),"geom",choices=c("violin","boxplot","jitter"))
                            )
                 )

    ),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("plot",
                         plotOutput(ns('plot'),width='auto',height='650px')
                         ),
                tabPanel("table",
                         dataTableOutput(ns("table"))
                         )
              )

    )
  )
}

Supervised_systematic_propolisServer <- function(input,output,session){
  output$plot <-renderPlot({
    data <- grid
    for(i in seq(8)){
      data <- data[!data[,i] %in% input$subset,]
    }
    colnames(data)[colnames(data) == input$x] <- "X"
    if(input$x != input$color){
      colnames(data)[colnames(data) == input$color] <- "color"
      p <- ggplot(data,aes(x=X,col=color,y=accuracy))+#geom_violin()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    }else{
      p <-ggplot(data,aes(x=X,y=accuracy))+#geom_violin()+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    }
    if(input$geom == "jitter"){p<-p+geom_jitter()}
    if(input$geom == "boxplot"){p<-p+geom_boxplot()}
    if(input$geom == "violin"){p<-p+geom_violin()}
    p
  })
  output$table <- renderDataTable({
    data <- grid[,1:9]
    for(i in seq(8)){
      data <- data[!data[,i] %in% input$subset,]
    }
    data
  })
}
