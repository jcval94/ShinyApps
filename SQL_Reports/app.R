
text_eval<-function(a){
    if(class(a)!="character"){return()}
    eval(parse(text = a))
}

dtsts<-ls("package:datasets")
classes<-purrr::map(dtsts,~class(text_eval(.x)))

library(DT)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(sqldf)
library(V8)
library(psych)
library(ggplot2)
library(markdown)
library(assertthat)
library(FitUltD)
library(quantmod)
library(caret)
library(knitr)
library(randomForest)
library(shinythemes)
library(shinyAce)
library(RColorBrewer)
library(qualV)
library(colourpicker)
library(ggcorrplot)
library(tibble)
library(TSA)
# library(forecast)

ui <- fluidPage(
    
    useShinyjs(),
    
    theme = shinytheme("lumen"),
    includeCSS("www/custom.css"),
    titlePanel(""),
    #helpText(strong("Data Visualization Dashboard")),
    br(),
    column(12, offset=0,
           h1(strong("José Carlos Del Valle's Demo"),style="
				background-image: url(my_image.png);
				opacity: 0.7;
				background-color: white;
				width: 100%;
				text-align:center;
     margin-top: 0px;
     ")),
    br(""),
    mainPanel(
        # within tabPanels(), define the pages for sidebar menu items
        
        tabsetPanel(
            tabPanel("SQL & R", 
                     br(),
                     column(12, offset=0, align="center" ,
                            
                            style="
				background-image: url(my_image.png);
				opacity: 0.8;
				background-color: white;
				margin-top: -20px;
				width: 150%;
				",
                            br(),
                            helpText((p("It is possible to compile R and SQL code online and download Excel files"
                                        , style="color:black ; font-family: 'times'; font-size:18pt;text-align:left"))) ,
                            h4("You can write any code accepted by R and SQL by pushing the buttons"),
                            
                            textInput("query__01",label =  h3("SQL Query"), 
                                      value = "SELECT * FROM DF WHERE Species = \"virginica\" ",
                                      width = "700px"),
                            textInput("query__02",label =  h3("R code for Machine Learning analysis"), 
                                      value = "round(importance(randomForest(Species ~ ., data=DF, mtry=3,importance=TRUE, na.action=na.omit)),3)",
                                      width = "700px"),
                            actionButton("Con_D","SQL query"),
                            actionButton("Con_R","R code"),
                            downloadButton("desc_qr","Download (csv)","csv"),
                            
                            hr(),
                            dataTableOutput("DLS"),
                            br(),br(),br(),br(),br()
                     )
            ),
            tabPanel("Visualization", 
                     br(),
                     column(12, offset=0, align="center" ,
                            
                            style="
				background-image: url(my_image.png);
				opacity: 0.8;
				background-color: white;
				margin-top: -20px;
				width: 150%;
				",
                            br(),
                            helpText((p("Principle features of a database: Correlation, plots and Distribution"
                                        , style="color:black ; font-family: 'times'; font-size:18pt;text-align:left"))) ,
                            
                            div(
                                style = "padding: 50px",
                                
                                uiOutput("datasets2"),
                                actionButton("QVW","View (Push Me)"),downloadButton("report", "Generate report"),
                                h3("Correlogram"),
                                plotOutput("Corrrr"),
                                h3("Variables"),
                                plotOutput("Vars"),
                                h3("Distribution"),
                                plotOutput("Distplt")
                            )
                     )
            )
        )
        
    )
)


server <- function(input, output,session) {
    
    output$query_r<-renderPrint({"Try kmeans(DF[,-5],3)[[2]]"})
    
    output$DLS<-renderDataTable({
        data(iris)
        iris
    },options = list(scrollX = TRUE))
    
    
    
    
    observeEvent(input$Con_D,{
        if(exists("Data_server_")){
            DF<-Data_server_
        }else{
            data(iris)
            DF<-iris
        }
        
        consulta__01<-input$query__01
        
        qqry<<-try(sqldf::sqldf(consulta__01))
        if(assertthat::is.error(qqry)){
            qqry<<-data.frame(Error=as.character(qqry))
        }
        
        output$DLS<-renderDataTable({
            qqry
        },options = list(scrollX = TRUE))
        
        
    })
    observeEvent(input$Con_R,{
        if(exists("Data_server_")){
            DF<-Data_server_
        }else{
            data(iris)
            DF<-iris
        }
        
        consulta__02<-input$query__02
        consulta__02<-paste0("DF<-iris;",consulta__02)
        #consulta__01<-input$query__01
        qqry<<-try(text_eval(consulta__02))
        #qqry<-try(sqldf::sqldf(consulta__01))
        
        if(assertthat::is.error(qqry)){
            qqry<<-data.frame(Error=as.character(qqry))
        }
        if(!"data.frame" %in% class(qqry)){
            qqry<<-as.data.frame(qqry)
        }
        output$DLS<-renderDataTable({
            qqry
        },options = list(scrollX = TRUE))
        
    })
    
    
    output$desc_qr <- downloadHandler(
        filename = function() {
            paste("qqry",".csv", sep = "")
        },
        content = function(file) {
            if(!exists("qqry")){
                write.csv(iris
                          , file, row.names = FALSE)
            }else{
                write.csv(qqry, file, row.names = FALSE)
            }
        },
        contentType="text/csv"
    )
    
    output$Sim_sum<-renderDataTable({data.frame()})
    
    filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        read.csv(infile$datapath)
    })
    output$datasets <- renderUI(
        selectInput("Data", label = "Select Dataset", 
                    choices = dtsts[classes %in% "data.frame"], 
                    selected = "",multiple = F)
    )
    output$datasets2 <- renderUI(
        selectInput("Data2", label = "Select Dataset", 
                    choices = dtsts[classes %in% "data.frame"], 
                    selected = "iris",multiple = F)
    )
    
    output$report <- downloadHandler(
        
        filename = paste0("report_",Sys.Date(),".docx"),
        content = function(file) {
            
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(n = input$Data2)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,encoding = "UTF-8",
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    observeEvent(input$QVW,{
        Data2<-tibble::as_tibble(get(input$Data2))
        if(ncol(Data2)<2){
            Data2<-iris
        }
        
        DataN<-(Data2[,purrr::map_lgl(Data2,is.numeric)])
        
        corr <- round(cor(na.omit(DataN)), 1)
        
        pl1<-try(ggcorrplot::ggcorrplot(corr, hc.order = TRUE, 
                                        type = "lower", 
                                        lab = TRUE, 
                                        lab_size = 3, 
                                        method="circle", 
                                        colors = c("tomato2", "white", "springgreen3"), 
                                        title=paste0("Correlogram of ",input$Data2), 
                                        ggtheme=theme_bw))
        
        if(assertthat::is.error(pl1)){
            pl1<-plot(DataN)
        }
        
        output$Corrrr<-renderPlot({
            pl1
        })
        
        output$Vars<-renderPlot({
            plot(DataN[,1:min(5,ncol(DataN))],col=rainbow(ncol(DataN)))
        })
        
        all_nms<-names(Data2)
        all_N<-names(DataN)
        clasif<-all_nms[!all_nms %in% all_N]
        if(length(clasif)!=0){
            ntcls<-names(Data2) %in% clasif
            
            names(Data2)[ntcls]<-paste0("Cl",1:sum(ntcls))
            names(Data2)[!ntcls]<-paste0("Nm",1:sum(!ntcls))
            
            p <- ggplot(Data2, aes(factor(Cl1),Nm1))
            
            output$Distplt<-renderPlot({
                p + geom_violin() + geom_jitter(height = 0, width = 0.05) +  theme_minimal() +
                    ylab((all_N)[1]) +
                    xlab((clasif)[1])
            })
        }else{
            names(Data2)<-paste0("Nm",1:ncol(Data2))
            Data2<-scale(Data2)
            DF2<-data.frame(Y=do.call(c,purrr::map(Data2,c)),
                            X=rep(all_nms,each=nrow(Data2)))
            
            p <- ggplot(DF2, aes(factor(X),Y))
            
            output$Distplt<-renderPlot({
                p + geom_violin() + geom_jitter(height = 0, width = 0.05) +  theme_minimal() +
                    ylab((all_N)[1])+xlab("Variables")
            })
        }
        
    })
    
    observeEvent(input$Data,{
        Data<-get(input$Data)
        ND<-names(Data)
        
        output$Data_Form<-renderUI({
            textInput("Form_ML","Write the independant(s) and dependent variables",
                      paste0(ND[1]," ~ ",paste0(ND[2:length(ND)],collapse = " + "))
            )
        })
        
    })
    
    if(FALSE){
        library(rsconnect)
        deployApp()
    }
}

shinyApp(ui = ui, server = server)



# rsconnect::setAccountInfo(name='valc941226', token='31D85682FA046802FF9FF61D3B45910F', secret='HmIPh8+uJCIZPxykIm/UFxZ+T+mf5PALB4pwUw1L')
