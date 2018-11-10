## app.R ##
library(shinydashboard)
library(shiny)
library(corrgram)
library(datasets)
library(xlsx)
library(randomForest)
library(readr)
library(dplyr)
library(ggvis)
library(tree)
library(BH)
library(tigerstats)

ui <- dashboardPage(skin = "blue",
                    # skin = "green",
                    dashboardHeader(title = "Data Analysis"),
                    dashboardSidebar( 
                      sidebarMenu(
                        ##Tab Item One
                        menuItem("File Upload",tabName = "file",icon = icon("file-excel-o")),
                        ##Tab Item Two
                        menuItem("Plot", tabName = "plot1", icon = icon("line-chart")),
                        ##Tab Item Three
                        menuItem("Table", tabName = "table", icon = icon("table")),
                        ##Tab Item Four
                        menuItem("Cross Tabulation",tabName = "crosstab", icon = icon("table")),
                        ##Tab Item Five
                        menuItem("Summary",tabName = "summary",icon = icon("list-alt")),
                        ##Tab Item Six
                        menuItem("Correlation",tabName = "correlation",icon = icon("gears")),
                        ##Tab Item Seven
                        menuItem("Correlation Matrix",tabName = "correlationm",icon = icon("th")),
                        ##Tab Item Eight
                        menuItem("Correlogram",tabName = "correlogram",icon = icon("picture-o")),
                        ##Tab Item Nine
                        menuItem("Simple Linear Regression",tabName = "linearregression",icon = icon("line-chart")),
                        ##Tab Item Ten
                        menuItem("Multiple Linear Regression",tabName = "linearregressionm",icon = icon("line-chart")),
                        ##Tab Item Eleven
                        menuItem("Histogram", tabName = "histogram",icon = icon("bar-chart")),
                        ##Tab Item Twelve
                        menuItem("Interactive Plot",tabName = "ggvis",icon = icon("bar-chart")),
                        ##Tab Item Thirteen
                        menuItem("Random Forest:Under Construction",tabName = "randomforest",icon = icon("line-chart"))
                        
                        
                        
                        
                      )),
                    dashboardBody(
                      tags$head(includeScript("www/google-analytics.js")),
                      tabItems(
                        ##Tab Item One
                        tabItem(tabName = "file",
                                fileInput("file", label = h3("File input: CSV/Text",multiple = FALSE,accept = NULL,width=NULL),
                                          accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.xlsx')),
                                radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
                                radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"),'"'),
                                # radioButtons(inputId = 'fileext',label = 'File Extension',choices = c('read.csv','read.xlsx')),
                                checkboxInput("header", "Header", TRUE),
                                downloadButton('downloadData', 'Download')),
                        ##Tab Item Two
                        tabItem(
                          tabName = "plot1",solidHeader = TRUE,
                          fluidRow(box(title = "Plot Controls",
                                       selectInput(inputId = "download1",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                                       selectInput("xcol", "X Variable",choices = names(df)),
                                       selectInput("ycol", "Y Variable", choices = names(df)),
                                       sliderInput("point","Point Type",min=0, max=25,value = 19),
                                       numericInput("size","Point size",3,min=0.5,max = 10),
                                       selectInput(inputId = "line",label = "Line Type",choices = list("Line"="l","Points"="p","Stairs"="s"),selected = "p"),
                                       selectInput(inputId = 'color',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                   selected = "Black"),collapsible = TRUE),
                                   downloadButton('download', 'Download Plot')),
                          # downloadButton('download', 'Download Plot'),
                          # box(title = "Download",radioButtons(inputId = "download1",label = "Choose Format",choices = list("png","pdf")),
                          # downloadButton('download', 'Download Plot'),collapsible = TRUE,collapsed = TRUE),
                          box(title = "Data Plot",
                              plotOutput("plot1"),width = "auto",height = "auto",collapsible = TRUE,collapsed = TRUE)
                        ),
                        ##Tab Item Three
                        tabItem(tabName = "table",         
                                fluidRow(
                                  tableOutput("table"))), 
                        
                        ##Tab Item Four
                        tabItem(tabName = "summary",
                                fluidRow(
                                  box(title = "Data Summary",solidHeader = TRUE,background = "light-blue",
                                      verbatimTextOutput("summary"),width = "auto",height = "auto")),
                                # radioButtons(inputId = "download2",label = "Choose Format",choices = list("txt","csv")),
                                downloadButton('downloadtwo', 'Download Summary')),
                        ##Tab Item Five
                        tabItem(tabName = "correlation",
                                box(
                                  fluidRow(box(title = "Correlation: Select Variables Below",
                                               selectInput("xcol7", "X Variable",choices = names(df)),
                                               selectInput("ycol7", "Y Variable", choices = names(df)))),
                                  box(title = "Correlation: Pearson",
                                      verbatimTextOutput("correlation")),width = "auto",height = "auto")),
                        ##Tab Item Six
                        tabItem(tabName = "correlationm",
                                box(title = "Correlation Matrix",solidHeader = TRUE,background = "light-blue",
                                    verbatimTextOutput("correlationm"),width = "auto",height = "auto")),
                        ##Tab Item Seven
                        tabItem(tabName = "correlogram",
                                selectInput(inputId = "panel",label = "Correlogram Type",choices = list("Bar"="panel.bar","Conf"="panel.conf","panel.corr","Density"="panel.density","Ellipse"="panel.ellipse","MinMax"="panel.minmax","Pie"="panel.pie","Points"="panel.pts","Shade"="panel.shade","Text"="panel.text"),selected = "panel.shade"),
                                box(title = "Correlogram",solidHeader = TRUE,
                                    plotOutput("corr"),width = 400,height = 600,collapsible = TRUE,collapsed = TRUE)),
                        ##Tab Item Eight
                        tabItem(tabName = "linearregression",
                                selectInput("xcol1", "X Variable",choices = names(df)),
                                selectInput("ycol2", "Y Variable", choices = names(df)),
                                box(title = "Simple Linear Regression Output:",solidHeader = TRUE,background = "light-blue",
                                    verbatimTextOutput("summarylm"),width = 300)),
                        ##Tab Item Nine
                        tabItem(tabName = "linearregressionm",
                                selectInput("xcol3", "Predictor One",choices = names(df)),
                                selectInput("xcol4", "Predictor Two",choices = names(df)),
                                selectInput("xcol5", "Predictor Three",choices = names(df)),
                                selectInput("xcol6", "Predictor Four",choices = names(df)),
                                selectInput("ycol3", "Dependent", choices = names(df)),
                                box(title = "Multiple Linear Regression Output:",solidHeader = TRUE,background = "light-blue",
                                    verbatimTextOutput("summarylmmulti"),width = 300)),
                        ##Tab Item Ten
                        tabItem(tabName = "histogram",
                                fluidPage(
                                  box(solidHeader = TRUE,background = "light-blue",
                                      selectInput("xcol8", "Histogram Variable",choices = names(df)),
                                      selectInput(inputId = 'colortwo',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                                  selected = "lightblue"),
                                      numericInput("bins","Number of Bins",10,min=1,max = 50),
                                      selectInput(inputId = "download3",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                                      downloadButton('downloadthree', 'Download Histogram')), 
                                  box(title = "Histogram",solidHeader = TRUE,background = "light-blue",
                                      plotOutput("hist"),collapsible = TRUE)
                                )),
                        ##Tab Item Eleven
                        tabItem(tabName = "ggvis",
                                fluidPage(
                                  selectInput('x', 'x:' ,'x'),
                                  selectInput('y', 'y:', 'y'),
                                  sliderInput("size1","Point size",100,min=100,max = 400),
                                  selectInput(inputId = 'color1',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                              selected = "lightblue"),
                                  uiOutput("plot_ui"),
                                  box(
                                    ggvisOutput("plot"),width = 400))),
                        ##Tab Item Twelve
                        tabItem(tabName = "randomforest",
                                fluidPage(
                                  selectInput("randomcol", "Random Forest Predictor: Under Construction...Please Check Back Soon",choices = names(df)),
                                  box(plotOutput("randomforest"))
                                )),
                        ##Tab Item Thirteen
                        tabItem(tabName = "crosstab",
                                fluidPage(
                                  selectInput(inputId = "tab1",label = "Columns",choices = names(df)),
                                  selectInput(inputId = "tab2",label = "Rows",choices = names(df)),
                                  box(title = "Cross Tabulation",verbatimTextOutput("crosstab"),
                                      downloadButton('downloadcrosstab', 'Download Cross Tabulation')),
                                  box(title = "Column Percentages:",verbatimTextOutput("crosstabcolperc"),
                                      collapsible = TRUE,collapsed = TRUE),
                                  box(title = "Row Percentages:",verbatimTextOutput("crosstabrowperc"),
                                      collapsible = TRUE,collapsed = TRUE)
                                ))
                        
                      )))







server <- function(input, output,session) {
  options(shiny.maxRequestSize = 25*1024^2)
  ## Code to read file in and load into variable selections
  data <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df))
    
    
    
    
    return(df)
    
  })
  
  ##Second Reactive for Simple Linear Regression ####
  dataset <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol1', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol2', label = 'Y Variable',
                      choices = names(df))
    
    
    return(df)
    
  }) 
  
  ## Third Reactive for Multiple Linear Regression ####
  datamultiple <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol3', label = 'Predictor One',
                      choices = names(df))
    updateSelectInput(session, inputId = 'xcol4', label = 'Predictor Two',
                      choices = names(df))
    updateSelectInput(session, inputId = 'xcol5', label = 'Predictor Three',
                      choices = names(df))
    updateSelectInput(session, inputId = 'xcol6', label = 'Predictor Four',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol3', label = 'Dependent',
                      choices = names(df))
    
    
    return(df)
    
  })
  ## Fourth Reactive for Correlation
  
  datasetcor <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol7', label = 'X Variable',
                      choices = names(df))
    updateSelectInput(session, inputId = 'ycol7', label = 'Y Variable',
                      choices = names(df))
    
    
    return(df)
    
  })
  
  ##Histogram Reactive Code 
  datasethist <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    updateSelectInput(session, inputId = 'xcol8', label = 'Histogram Variable',
                      choices = names(df))
    
    
    
    return(df)
    
  })
  
  # datavis <- reactive({ 
  #   req(input$file) ## ?req #  require that the input is available
  #   
  #   inFile <- input$file 
  #   
  #   
  #   df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
  # 
  #   
  #   
  #   updateSelectInput(session, inputId = 'x', label = 'X Variable',
  #                     choices = names(df))
  #   updateSelectInput(session, inputId = 'y', label = 'Y Variable',
  #                     choices = names(df))
  #   # updateSelectInput(session, inputId = 'zcol11', label = 'Z Variable',
  #   #                   choices = names(df), selected = names(df))
  #   # updateSelectInput(session, inputId = 'zzcol', label = 'Q Variable',
  #   #                   choices = names(df), selected = names(df))
  #   
  
  
  
  #   return(df)
  #   
  # })
  
  
  
  ##Random Forest File Reader ##############
  datarandom <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    
    
    
    updateSelectInput(session, inputId = 'randomcol', label = 'Random Forest Predictor',
                      choices = names(df))
    
    
    
    
    
    return(df)
    
  })
  
  
  summarydata <- reactive({ 
    req(input$file) ## ?req #  require that the input is available
    
    inFile <- input$file 
    
    
    df <- read.csv(inFile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    summary(df)
    
    
    return(df)
    
  })
  
  
  ## Plot output code 
  output$plot1 <- renderPlot({
    
    par(mar = c(5.1, 4.1, 0, 1))
    
    x <- data()[, c(input$xcol, input$ycol)]
    plot(x,col=input$color,pch = input$point,type = input$line,cex=input$size)
  })
  
  ## Table Data Code
  output$table <- renderTable({
    
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    # datasetInput()
    read.csv(inFile$datapath, header = input$header)
    
  })
  
  ##File Summary Code
  output$summary <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    yowsa<- read.csv(inFile$datapath, header = input$header)
    summary(yowsa)
  })
  
  ##Observe function used for Cross Tabulation
  observe({
    updateSelectInput(session,inputId = "tab1",label =  "Columns", choices = sort(as.character(colnames(data()))))
    updateSelectInput(session, inputId = "tab2",label = "Rows", choices = sort(as.character(colnames(data()))))
    
  })
  
  # observe({
  #   updateSelectInput(session, inputId = "tab2",label = "Rows", choices = sort(as.character(colnames(data()))))
  # })
  
  
  
  output$crosstab <- renderPrint({
    validate(need(input$tab2,''),
             need(input$tab1,''))
    with(data(), table(get(input$tab2),get(input$tab1)))
    # xtabs(as.formula(paste0("~",input$tab2,"+",input$tab1)), data())
  })
  
  
  output$crosstabcolperc <- renderPrint({
    validate(need(input$tab2,''),
             need(input$tab1,''))
    colPerc(with(data(), table(get(input$tab2),get(input$tab1))))
    # colPerc(xtabs(as.formula(paste0("~",input$tab2,"+",input$tab1)), data()))
  })
  
  output$crosstabrowperc <- renderPrint({
    validate(need(input$tab2,''),
             need(input$tab1,''))
    rowPerc(with(data(), table(get(input$tab2),get(input$tab1))))
  })
  
  summarytwo <- reactive({
    
    inFile <- input$file
    yowsa<- read.csv(inFile$datapath, header = input$header)
    as.data.frame.matrix(summary(yowsa))
  })
  
  crossout <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    # ## Needed to hold table until data is updated in it
    # validate(need(input$tab2,''),
    #          need(input$tab1,''))
    xtabs(as.formula(paste0("~",input$tab2,"+",input$tab1)), data())
    # with(data(), table(get(input$tab2),get(input$tab1)))
    
  })
  
  ## Download Buttons ################################################################################################  
  
  ##Download Button Code for downloading csv file
  
  output$download <- downloadHandler(
    filename = function() { paste("Plot ",input$xcol," by ",input$ycol,input$download1,sep = ".") },
    content = function(file) {
      if(input$download1=="png")
        png(file)
      else if (input$download1=="jpeg")
        jpeg(file)
      else if (input$download1=="bmp")
        bmp(file)
      else if (input$download1=="pdf")
        pdf(file)
      plot(data()[, c(input$xcol, input$ycol)],col=input$color,pch = input$type,type = input$line,cex=input$size,main = paste(input$xcol," by ",input$ycol))
      dev.off()
    })
  
  
  output$downloadthree <- downloadHandler(
    filename = function() { paste("Histogram",input$xcol8,input$download3,sep = ".") },
    content = function(file) {
      if(input$download3=="png")
        png(file)
      else if (input$download3=="jpeg")
        jpeg(file)
      else if (input$download3=="bmp")
        bmp(file)
      else if (input$download3=="pdf")
        pdf(file)
      hist(as.numeric(datasethist()[,input$xcol8]), col=input$colortwo,breaks = input$bins,xlab = input$xcol,main = input$xcol,border = input$bordercolor,
           freq = TRUE)
      dev.off()
    })
  
  output$down <- downloadHandler(
    filename = function() { paste(input$file, sep='') },
    content = function(file) {
      write.csv(data(), file)
      
    })
  
  
  
  output$downloadtwo <- downloadHandler(
    filename = function() { paste("Summary",input$file, sep='.') },
    content = function(file) {
      write.csv(summarytwo(), file)
    })
  
  output$downloadcrosstab <- downloadHandler(
    filename = function() { paste("Crosstab",input$tab1,"BY",input$tab2,input$file, sep='.') },
    content = function(file) {
      # x <- (table((input$tab2),(input$tab1),data()))
      write.csv(crossout(),file,col.names = TRUE,row.names = TRUE)
      
    })
  
  ## Download Buttons #####  
  
  
  ## Statistics Code ######################################################
  ##Correlation Code
  
  output$correlation <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    correlation<- datasetcor()[,c(input$xcol7,input$ycol7)]
    cor(correlation)
  })
  
  ##Correlation Code
  output$correlationm <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    correlation<- data()
    cor(correlation)
  })
  
  ##Simple Linear Regression Code
  output$summarylm <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    linearmodel<- read.csv(inFile$datapath, header = input$header)
    
    x<-dataset()[,input$xcol1]
    y<-dataset()[,input$ycol2]
    lmone<-lm(as.formula(paste0(input$ycol2,"~",input$xcol1)),data = linearmodel)
    print(paste0("Formula:",input$ycol2," ~ ",input$xcol1," data: "))
    # # print(input$xcol1)
    # print(paste0("Y =",input$ycol2))
    # # print(input$ycol2)
    summary(lmone)
    # summary(lm(as.formula(paste0(input$ycol2,"~",input$xcol1)),data=linearmodel))
    # summary(lm(as.formula(paste0("~",input$ycol2,input$xcol1)),data=linearmodel))
    
  })
  
  ### Correlogram Code
  output$corr <- renderPlot({
    par(mar = c(20, 20, 0, 1))
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    corrfile<- read.csv(inFile$datapath, header = input$header)
    corrgram(corrfile,panel = input$panel,order = TRUE)
    
  })
  
  ##Multiple Linear Regression Code
  output$summarylmmulti <- renderPrint({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    linearmodelmulti<- read.csv(inFile$datapath, header = input$header)
    
    p1<-datamultiple()[,input$xcol3]
    p2<-datamultiple()[,input$xcol4]
    p3<-datamultiple()[,input$xcol5]
    p4<-datamultiple()[,input$xcol6]
    y<-datamultiple()[,input$ycol3]
    # lmonemulti<-lm(y~p1+p2+p3+p4,data = linearmodelmulti)
    # print("Predictor One =")
    # print(input$xcol3)
    # print("Predictor Two =")
    # print(input$xcol4)
    # print("Predictor Three =")
    # print(input$xcol5)
    # print("Predictor Four =")
    # print(input$xcol6)
    # print("Dependent =")
    # print(input$ycol3)
    # summary(lm(y~p1+p2+p3+p4,data = linearmodelmulti))
    summary(lm(as.formula(paste0(input$ycol3,"~",input$xcol3,"+",input$xcol4,"+",input$xcol5,"+",input$xcol6)),data=linearmodelmulti))
    
  })
  
  
  ##Histogram Code 
  output$hist <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))
      return("Please Upload File")
    
    x<-as.numeric(datasethist()[,input$xcol8])
    hist(x, col=input$colortwo,breaks = input$bins,xlab = input$xcol8,main = input$xcol8,border = input$bordercolor,
         freq = TRUE)
    
    
  })
  
  #load the data when the user inputs a file
  theData <- reactive({
    infile <- input$file        
    if(is.null(infile))
      return(NULL)        
    d <- read.csv(infile$datapath, header = input$header,sep = input$sep,quote = input$quote)
    d        
  })
  
  
  
  # dynamic variable names
  observe({
    data<-theData()
    updateSelectInput(session, 'x', label = 'x:',choices = names(data))
    updateSelectInput(session, 'y', label = 'y:',choices = names(data))
    
  }) # end observe
  
  #gets the y variable name, will be used to change the plot legends
  yVarName<-reactive({
    input$y
  })
  
  #gets the x variable name, will be used to change the plot legends
  xVarName<-reactive({
    input$x
  })
  
  #make the filteredData frame
  
  filteredData<-reactive({
    data<-isolate(theData())
    #if there is no input, make a dummy dataframe
    if(input$x=="x" && input$y=="y"){
      if(is.null(data)){
        data<-data.frame(x=0,y=0)
      }
    }else{
      data<-theData()[,c(input$x,input$y)]
      names(data)<-c("x","y")
    }
    data
  })
  
  input_size <- reactive(input$size1)
  input_color <- reactive(input$color1)
  
  #plot the ggvis plot in a reactive block so that it changes with filteredData
  vis<-reactive({
    plotData<-filteredData()
    # z <- paste0(filteredData()[,input$y])
    plotData %>%
      ggvis(~x, ~y,fill=~y,size := input_size,fill := input_color) %>%
      layer_points() %>%
      add_axis("y", title = yVarName()) %>%
      add_axis("x", title = xVarName()) 
    # add_tooltip(function(df) data()[,input$y[]])
  })
  vis%>%bind_shiny("plot", "plot_ui")
  
  
  
  # # A reactive expression with the ggvis plot
  # vis <- reactive({
  # 
  #   inFile <- input$file
  #   # if (is.null(inFile))
  #   #   return("Please Upload File")
  #   # datanew<- read.csv(inFile$datapath, header = input$header)
  # 
  #   xvar <- prop("x", as.symbol(input$xcol12))
  #   yvar <- prop("y", as.symbol(input$ycol12))
  # 
  # 
  #   # xvar <- datavis()[,input$xcol12]
  #   # yvar <- datavis()[,input$ycol12]
  # 
  #   data %>%
  #     ggvis(x = xvar,y = yvar) %>%
  #     layer_points()
  #     # set_options(width = 500, height = 500)
  # })
  # #
  # vis %>% bind_shiny("plot2")
  # input_size <- reactive(input$size)
  
  # mtcars %>% 
  #   ggvis(~disp, ~mpg, size := input_size) %>%
  #   layer_points() %>%
  #   bind_shiny("ggvis", "ggvis_ui")
  
  ## Random Forest Approach ############################################
  # ## Student Performance Random Forest #############################################
  # 
  # 
  # output$randomforest <- renderPlot({
  # 
  # library(tree)
  # datarandom <- datarandom()
  # names(datarandom)
  # library(randomForest)
  # # attach(datarandom())
  # 
  # G3 <- paste0(input$randomcol)
  # lstMSEs=numeric()
  # set.seed(1)
  # maxnumpreds=ncol(datarandom)-1
  # maxnumtrees=10
  # 
  # for(numpreds in 1:maxnumpreds){
  #   for(numtrees in 1:maxnumtrees){
  # 
  #     nrow(datarandom)
  #     train=sample(1:nrow(datarandom),nrow(datarandom)/2)
  # 
  # 
  #     model.bagged=randomForest(G3~.,data = datarandom,subset = train,mtry=numpreds,ntree=numtrees,importance=TRUE)
  # 
  #     subtract <- paste0(datarandom$G3)
  # 
  #     pred.vals.bagged=predict(model.bagged,newdata = datarandom[-train])
  #     testvals=datarandom[-train]
  #     mse=mean((pred.vals.bagged - testvals)^2)
  #     lstMSEs=rbind(lstMSEs,mse)
  #     print(paste("     Processed Trees:",numtrees))
  #   }
  #   print(paste("     Processed Predictors:",numpreds))
  # }
  # 
  # matMSEs=matrix(lstMSEs,nrow = maxnumpreds,ncol=maxnumtrees)
  # 
  # 
  # print(paste("The optimal configuration is",loc[1],"predictors and",loc[2], "trees"))
  # length(lstMSEs)
  # list(lstMSEs)
  # 
  # min(lstMSEs)
  # min(matMSEs)
  # lstMSEs
  # 
  # loc=which(matMSEs==min(matMSEs),arr.ind=TRUE)
  # print(paste("The optimal configuration is",loc[1],"predictors and",loc[2], "trees"))
  # length(lstMSEs)
  # print(paste("        Processed Trees:", numtrees))
  # print(paste("        Processed Predictors:",numpreds))
  # matMSEs[loc[1],loc[2]]
  # 
  # 
  # 
  # which(matMSEs==min(matMSEs),arr.ind = TRUE)
  # importance(model.bagged)
  # tree.student1=tree(G3~.,data = datarandom)
  # plot(model.bagged)
  # plot(tree.student1)
  # text(tree.student1,pretty = 0)
  # varImpPlot(model.bagged)
  # model.bagged
  # min(lstMSEs)
  # })
}

shinyApp(ui, server)

# ?validColors
