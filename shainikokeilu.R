library(shiny)
library(lme4)
library(dplyr)
library(DT)
library(caTools)
library(readr)
library(caret)
library(rpart)

ui <- fluidPage(
  fileInput("upload","Lataa tiedosto"),
  tableOutput("head"),
  selectInput("var1", "Select Variable", choices = NULL),
  selectInput("var2", "Select Variable", choices = NULL),
  selectInput("var3", "Select Variable", choices = NULL),
  selectInput("var4", "Select Variable", choices = NULL),
  selectInput("var5", "Select Variable", choices = NULL),
  selectInput("var1_ml", "Select ML Variable", choices = NULL),
  selectInput("var2_ml", "Select ML Variable", choices = NULL),
  selectInput("var3_ml", "Select ML Variable", choices = NULL),
  actionButton("go","Run model"),
  actionButton("go_ml","Run ML model"),
  downloadButton("download", "Download .tsv"),
  verbatimTextOutput("glmSummary"),
  verbatimTextOutput("mlSummary")
)

server <- function(input, output, session) { 
  
  
  
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
    
  })
  
  
  
  observeEvent(input$upload, {
    req(data())
    updateSelectInput(session, "var1", choices = colnames(data()))
    updateSelectInput(session, "var2", choices = colnames(data()))
    updateSelectInput(session, "var3", choices = colnames(data()))
    updateSelectInput(session, "var4", choices = colnames(data()))
    updateSelectInput(session, "var5", choices = colnames(data()))
    updateSelectInput(session, "var1_ml", choices = colnames(data()))
    updateSelectInput(session, "var2_ml", choices = colnames(data()))
    updateSelectInput(session, "var3_ml", choices = colnames(data()))
  })
  
  glmModel <- eventReactive(input$go, {
    req(data(),input$var1,input$var2,input$var3,input$var4,input$var5)
    x <- as.factor(data()[[as.name(input$var1)]])
    y <- as.factor(data()[[as.name(input$var2)]])
    z <- as.factor(data()[[as.name(input$var3)]])
    x2 <- as.factor(data()[[as.name(input$var4)]])
    y2 <- as.factor(data()[[as.name(input$var5)]])
    if (length(x) == length(y)){
      model <- glmer(x ~ y + z + (1|x2) + (1|y2), data = data(),family = binomial, na.action=na.exclude)
    }else model <- NULL
    return(model)
  })
  
  
  
  mlModel <- eventReactive(input$go_ml, {
    req(data(),input$var1_ml,input$var2_ml,input$var3_ml)
    x3 <- as.factor(data()[[as.name(input$var1_ml)]])
    y3 <- as.factor(data()[[as.name(input$var2_ml)]])
    z3 <- as.factor(data()[[as.name(input$var3_ml)]])
    df <- data.frame(x3,y3,z3)
    set.seed(123)
    split = sample.split(df$x3, SplitRatio = 0.75)
    training_set = subset(df, split == TRUE)
    test_set = subset(df, split == FALSE)
    classifier = rpart(formula = x3 ~ y3 + z3, data = training_set)
    y_pred = predict(classifier, newdata = test_set[-1], type = 'class')
    sum(diag(acc))/sum(acc)*100
    
    
  })
  
  
  output$glmSummary <- renderPrint({
    req(glmModel())
    glmModel()
  })
  
  output$mlSummary <- renderPrint({
    req(mlModel())
    mlModel()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(mlModel(), ".csv")
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
  
  output$head <- renderTable({
    head(data())
  })
} 


shinyApp(ui = ui, server = server) 

