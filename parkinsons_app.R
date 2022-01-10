library(shiny)
library(tidyverse)
library(boot)
library(caret)
library(MASS)
library(ggplot2)
library(leaps)

PD.data0 <-read.csv("parkinsons.txt",
                   sep=',',
                   header=TRUE)
# get rid of name info so can group
PD.data0$name <- sapply(strsplit(as.character(PD.data0$name), split='_', fixed=TRUE), function(x) (x[3]))
# reorder columns so status is last column
PD.data0 <- PD.data0 %>% 
  dplyr::select(name:HNR, RPDE:PPE, status)

# subset only predictors and create so can use in list 
PD.pred <- PD.data0 %>% dplyr::select(- status)
predChoices <- 1:ncol(PD.pred)
names(predChoices) <- names(PD.pred)

ui <- fluidPage(
  headerPanel("Parkinsons App"),
  headerPanel("Exploratory Data Analysis"),
  
  sidebarPanel(
    h4("Transform Data"),
    radioButtons(inputId="choice", label="Choose How To Group Data for Analysis By Patient", 
                 choices=c("Average","Minimum", "Maximum", "Sum", "None")),
    h4("Correlation Plot"),
    checkboxInput(inputId = "twoVars", label = "Two Predictors", value = FALSE),
    selectInput(inputId='class1Var1', label='Variable 1', choices=names(PD.pred),
                selected=names(PD.pred)[[1]]),
    conditionalPanel(
      condition = "input.twoVars == true",
      selectInput(inputId='class1Var2', label='Variable 2', choices=names(PD.pred), selected=names(PD.pred)[[2]]))
  ),
  
  mainPanel(
    plotOutput('class1')
  ), 
  
  headerPanel("Classification Technique"),
  h4("Note: The following models are created using the data transformation chosen above."),
  h4("The models are 10-fold cross validated"),

  fluidRow(
    column(3, 
           radioButtons(inputId="method", label="Choose What Method To Use For Classification", 
                        choices=c("Logistic Regression", "LDA", "QDA", "KNN"))),
     column(4, offset =1,
           checkboxGroupInput(inputId = "Preds", label = "Choose Predictors for Model", choices = predChoices, inline = T),
           #   numericInput(inputId='pred1Value', label="Variable 1 Test Value =", min=0, max=600, value=5, step=0.0001)
           actionButton("runModel", "Predict!")
     ),
    column(4,
           h3("Model Accuracy"),
           tableOutput("AccuracyTable"))
  ),

  mainPanel(
         h3("The Best Subset of Variables"),
         plotOutput('bestPlot')
 )
)


server <- function(input, output, session) {
  values <- reactiveValues()
  values$error.table <- data.frame(Method = character(),
                                   Factors = character(),
                                   Transformed = character(),
                                   RSquared = numeric())

#######################################################
# Transform data according to input
#######################################################  
  transformData  <- function(input, output, session, transformation){
    PD.data <-read.csv("parkinsons.txt",
                       sep=',',
                       header=TRUE)
    PD.data$name <- sapply(strsplit(as.character(PD.data$name), split='_', fixed=TRUE), function(x) (x[3]))
    PD.data <- PD.data %>% 
      dplyr::select(name:HNR, RPDE:PPE, status)
    PD.data <- PD.data[complete.cases(PD.data),]
    
    if(transformation != "None"){
      PD.data <- PD.data %>% group_by(name)
      
      if(transformation == "Average") { PD.data <- PD.data %>% summarize_all(mean)}
      if(transformation == "Minimum") { PD.data <- PD.data %>% summarize_all(min)}
      if(transformation == "Maximum") { PD.data <- PD.data %>% summarize_all(max)}
      if(transformation == "Sum") { PD.data <- PD.data %>% summarize_all(sum) %>% mutate(status = ifelse(status<3, 0,1))}
    }
    PD.data <- as.data.frame(PD.data)
    
    return(PD.data)
  }
    
#######################################################
# RenderPlot for 1 or 2 Predictor classification
#######################################################
  # need to change lengend titles for these plots
  output$class1 <- renderPlot({

    orig.PD.data <- transformData(input, output, session, input$choice)
    
    if(input$twoVars){
      ggplot() +
        geom_point(aes(x=orig.PD.data[,input$class1Var1], y=orig.PD.data[,input$class1Var2], color=as.factor(orig.PD.data[,"status"])), size=3) +
        labs(x=input$class1Var1, y="Status", title="Parkinsons Data")
    }else{
      ggplot() +
        geom_point(aes(x=orig.PD.data[,input$class1Var1], y=orig.PD.data[,"status"], color=as.factor(orig.PD.data[,"status"])), size=3) +
        labs(x=input$class1Var1, y="Status", title="Parkinsons Data")
    }
  })

######################################################
# Observes Predict Button: Classification Technques
#######################################################  
  observeEvent( input$runModel, {

    control <- trainControl(method="cv", number=10)
    PD.data <- transformData(input, output, session, input$choice)
    
    fcols <- as.numeric(input$Preds)
    cols <- c(fcols, ncol(PD.data))
    
    if(length(cols) > 1){
      modelData <- data.frame(PD.data[,cols])
      names(modelData) <- names(PD.data)[cols]
    } 
    
    if(input$method == "Logistic Regression"){
      Model <- train(as.factor(status) ~ . , data=modelData, method="glm", trControl=control)
    }else if(input$method == "LDA"){
      Model <- train(as.factor(status) ~ . , data=modelData, method="lda", trControl=control)
    }else if(input$method == "QDA"){
      Model <- train(as.factor(status) ~ . , data=modelData, method="qda", trControl=control)
    }else if(input$method == "KNN"){
      Model <- train(as.factor(status) ~ . , data=modelData, method="knn", trControl=control)
    }
    
    Method <- input$method
    Factors <- paste0(names(PD.data)[c(fcols)], collapse = ", ")
    Transformed <- input$choice
    RSquared <- max(Model$results[,3])
    this.row <- as.data.frame(cbind(Method, Factors, Transformed, RSquared))
    isolate(values$error.table <- rbind(values$error.table, this.row))
  })
  
#######################################################
# Instantiate AccuracyTable
#######################################################  
  output$AccuracyTable <- renderTable({values$error.table})

#######################################################
# Find and Plot Best Predictors 
#######################################################   
  output$bestPlot <- renderPlot( {
    
    control <- trainControl(method="cv", number=10)
    PD.data <- transformData(input, output, session, input$choice)
    
    # Model <- glm(as.factor(status) ~ . , data=PD.data, family = binomial)
    
    regsubsets.out <-
      regsubsets(as.factor(status) ~ . ,
                 data = PD.data[,-(1)],
                 nbest = 1,
                 method = "forward")
    
     plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
  })
  
}

shinyApp(ui = ui, server = server)
