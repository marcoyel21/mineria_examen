


source("03-environment.R",   encoding = 'UTF-8')
mydata <- parcial_data

library(shiny)

mydata <- parcial_data
 ui <- navbarPage("EDA", 
                  tabPanel("Univariado",fluidPage(titlePanel("Univariado"),
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     uiOutput(outputId = "aa")
                                                 ),
                                                 mainPanel(textOutput("a"),
                                                           verbatimTextOutput("summary"),
                                                           plotOutput("plot", click = "plot_click")
                                                 )
                                             )
                  )),
                  tabPanel("Bivariado",fluidPage(titlePanel("Bivariado"),
                                                   sidebarLayout(
                                                       sidebarPanel(
                                                           uiOutput(outputId = "bb"),
                                                           uiOutput(outputId = "cc"),
                                                           multiple = 
                                                               TRUE),
                           
                                                       mainPanel(textOutput("b"),
                                                                 verbatimTextOutput("summary2"),
                                                                 verbatimTextOutput("summary3"),
                                                                 plotOutput("plot2", click = "plot_click")
                                                       )
                                                   )
                  )),
                  tabPanel("Multivariado",fluidPage(titlePanel("Multivariado"),
                                                   sidebarLayout(
                                                       sidebarPanel(
                                                           uiOutput(outputId = "dd"),
                                                           uiOutput(outputId = "ee"),
                                                           uiOutput(outputId = "ff"),
                                                           multiple = 
                                                               TRUE),
                                                       
                                                       mainPanel(textOutput("c"),
                                                                 verbatimTextOutput("summary4"),
                                                                 verbatimTextOutput("summary5"),
                                                                 verbatimTextOutput("summary6"),
                                                                 plotOutput("plot3", click = "plot_click")
                                                       )
                                                   )
                  ))
 )
 


server <- function(input,output) {
    
    output$aa <- renderUI({
        selectInput(inputId = "aa2", 
                    label="Selecciona una variable para analizar:",
                    choices = colnames(mydata))
    })
    
    
    mysubsetdata <- eventReactive(input$aa2,{
        mydata[[input$aa2]]
    })
    
    output$summary <- renderPrint({
        summary(mysubsetdata())
    })
    
    output$plot <- renderPlot({
        general(mydata,mysubsetdata())
    })
    
    
    #### pestaña 2·######
    
    output$bb <- renderUI({
        selectInput(inputId = "bb2", 
                    label="Select una variable para analizar",
                    choices = colnames(mydata))
    })
    
    
    mysubsetdata2 <- eventReactive(input$bb2,{
        mydata[[input$bb2]]
    })
    
    output$cc <- renderUI({
        selectInput(inputId = "cc2", 
                    label="Seleccciona otra variable para analizar",
                    choices = colnames(mydata))
    })
    
    mysubsetdata3 <- eventReactive(input$cc2,{
        mydata[[input$cc2]]
    })  
    output$summary2 <- renderPrint({
        summary(mysubsetdata2())
    })
    
    output$summary3 <- renderPrint({
        summary(mysubsetdata3())
    })
    
    output$plot2 <- renderPlot({
        bigeneral(mydata,input$bb2,input$cc2)
    })
    
    
    ### parte 3
    
    output$dd <- renderUI({
        selectInput(inputId = "dd2", 
                    label="Select una parametro de color (b)",
                    choices = colnames(mydata %>% select_if(is.character)))
    })
    
    
    mysubsetdata4 <- eventReactive(input$dd2,{
        mydata[[input$dd2]]
    })
    
    output$ee <- renderUI({
        selectInput(inputId = "ee2", 
                    label="Seleccciona una variable para analizar",
                    choices = colnames(mydata))
    })
    
    mysubsetdata5 <- eventReactive(input$ee2,{
        mydata[[input$ee2]]
    })  
    
    output$ff <- renderUI({
        selectInput(inputId = "ff2", 
                    label="Seleccciona otra variable para analizar",
                    choices = colnames(mydata))
    })
    
    mysubsetdata6 <- eventReactive(input$ff2,{
        mydata[[input$ff2]]
    })  
    
    output$summary4 <- renderPrint({
        summary(mysubsetdata4())
    })
    
    output$summary5 <- renderPrint({
        summary(mysubsetdata5())
    })
    
    output$summary6 <- renderPrint({
        summary(mysubsetdata6())
    })
    
    output$plot3 <- renderPlot({
        multigeneral(mydata,mysubsetdata4(),input$ee2,input$ff2)
    })
    
}

shinyApp(ui=ui, server=server)
