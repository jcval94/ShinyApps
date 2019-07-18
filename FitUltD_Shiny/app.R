
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
library(psych)
library(ggplot2)
library(markdown)
library(assertthat)
library(FitUltD)
library(shinythemes)

ui <- fluidPage(
    
    useShinyjs(),
    
    theme = shinytheme("lumen"),
    includeCSS("www/custom.css"),
    
    titlePanel(""),
    br(),
    column(12, offset=0,
           h1(strong("Welcome to this Demo"),style="
				background-image: url(my_image.png);
				opacity: 0.7;
				background-color: white;
				width: 100%;
				text-align:center;
     margin-top: 0px;
     ")),
    br(""),
    mainPanel("", 
                     br(),
                     column(12, offset=0, align="center" ,
                            
                            style="
				background-image: url(my_image.png);
				opacity: 0.8;
				background-color: white;
				margin-top: -20px;
				width: 150%;
				",br(),
                            helpText((p("During July of 2018 I developed a model which used Machine Learning and Goodness of Fit called FitUltD, this is the culmination of that model, which I used to explain illneses distribution in a population and measure medicine prices through a probability function. How it works? fitting a distribution using machine learning clustering methodS."
                                        , style="color:black ; font-family: 'times'; font-size:18pt;text-align:left"))) ,
                            
                            br(""),
                            helpText((p("Instructions: push Simulate button to generate a random variable, then push Fit and scroll down to fit a distribution (from 3 hours to 7 segs)"
                                        , style="color:black ; font-family: 'times'; font-size:18pt;text-align:left"))) ,
                            
                            
                            textInput("SIM_Dist",label =  h3("Choose a distribution function."), 
                                      value = "c(rnorm(700,0,1),rgamma(700,9,2))",
                                      width = "600px"),
                            actionButton("SIM_FT","Simulate (1)"),
                            actionButton("FIT_FT","Fit (2)"),
                            h4("A random variable represents numbers founded by counting or measuring natural behaviours, prices or time gaps. For example: number of marbles in a jar, petal length of a flower, number of heads when tossing two coins, the volume of a tree, etc."),
                            h5("iris$Petal.Length"),
                            h5("trees$Volume"),
                            fileInput("datafile", "Choose File",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            actionButton("FIT_FTU","Fit CSV"),
                            plotOutput("Sim_plot"),
                            withSpinner(dataTableOutput("Sim_sum"),type = 6),
                            withSpinner(plotOutput("Fit_plot"),type = 6),
                            dataTableOutput("Fit_sum"),
                            br(),
                            plotOutput("Fit_plot2"),
                            br(),br(),br(),br(),br(),br(),
                            
                            helpText((p("Applications: DNA methylation in human  genome | Growth rates among the individuals | Ruin theory for insurance companies | Insurance policies distribution | Fraud detection "
                                        , style="color:black ; font-family: 'times'; font-size:16pt;text-align:left"))) ,
                            
                            h5("Contact: valc941226@gmail.com"),
                            br(),br(),br(),br(),br(),br()
                            
                     )
    )
)

#######################################################
#######################################################
#######################################################

server <- function(input, output,session) {
    
    
    output$Fit_plot<-renderPlot({})
    output$Sim_sum<-renderDataTable({data.frame()})
    
    
    filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        read.csv(infile$datapath)
    })
    
    observeEvent(input$SIM_FT,{
        sim<-text_eval(input$SIM_Dist)
        
        df <- data.frame(x <- sim,
                         group <- sample(LETTERS[1:5], size = length(sim), replace = T))
        
        p <- ggplot(df, aes(x)) + 
            geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
            geom_density(fill = "#ff4d4d", alpha = 0.35) + 
            theme(panel.background = element_rect(fill = '#ffffff')) + 
            ggtitle("Density with Histogram")
        
        output$Sim_plot <- renderPlot({
            p
        })
        output$Sim_sum<-renderDataTable({
            data.frame(Min=round(min(sim,na.rm = T),4),
                       Median=round(median(sim,na.rm = T),4),
                       Mean=round(mean(sim,na.rm = T),4),
                       Max=round(max(sim,na.rm = T),4),
                       SD=round(sd(sim,na.rm = T),4))
        },options = list(scrollX = TRUE))
        
        
        observeEvent(input$FIT_FT,{
            
            sim<-na.omit(as.numeric(text_eval(input$SIM_Dist)))
            
            A<-FitUltD::FDistUlt(sim,plot = T,subplot = T,crt = 2)
            
            output$Fit_plot <- renderPlot({
                A[[4]][[1]]
            })
            AA<-A[[3]][,1:4]
            if(nrow(AA)>4){AA[,5]<-"Overfit"}
            output$Fit_sum<-renderDataTable({
                AA
            },options = list(scrollX = TRUE))
            
            output$Fit_plot2 <- renderPlot({
                if(nrow(AA)>1){A[[4]][[2]]}
            })
            
        })
        
    })
    
    observeEvent(input$FIT_FTU,{
        fl<-filedata()
        if(is.null(fl)){fl<-rnorm(1000)}
        if(ncol(fl)>1){
            output$Sim_sum<-renderDataTable({
                data.frame("File with more than 1 columns")
            },options = list(scrollX = TRUE))
        }else{
            sim<-na.omit(as.numeric(fl[[1]]))
            
            df <- data.frame(x <- sim,
                             group <- sample(LETTERS[1:5], size = length(sim), replace = T))
            
            p <- ggplot(df, aes(x)) + 
                geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
                geom_density(fill = "#ff4d4d", alpha = 0.35) + 
                theme(panel.background = element_rect(fill = '#ffffff')) + 
                ggtitle("Density with Histogram")
            
            output$Sim_plot <- renderPlot({
                p
            })
            output$Sim_sum<-renderDataTable({
                data.frame(Min=round(min(sim,na.rm = T),4),
                           Median=round(median(sim,na.rm = T),4),
                           Mean=round(mean(sim,na.rm = T),4),
                           Max=round(max(sim,na.rm = T),4),
                           SD=round(sd(sim,na.rm = T),4))
            },options = list(scrollX = TRUE))
            
            
            A<-FitUltD::FDistUlt(sim,plot = T,subplot = T,crt = 2)
            
            output$Fit_plot <- renderPlot({
                A[[4]][[1]]
            })
            AA<-A[[3]][,1:4]
            if(nrow(AA)>4){AA[,5]<-"Overfit"}
            output$Fit_sum<-renderDataTable({
                AA
            },options = list(scrollX = TRUE))
            
            output$Fit_plot2 <- renderPlot({
                if(nrow(AA)>1){A[[4]][[2]]}
            })
        }
    })
    
    output$Fit_sum<-renderDataTable({data.frame()})
    
    if(FALSE){
        library(rsconnect)
        deployApp()
    }
}
shinyApp(ui = ui, server = server)
