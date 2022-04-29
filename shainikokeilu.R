library(shiny)
library(lme4)
library(dplyr)
ui <- fluidPage(
  fileInput("upload","Lataa tiedosto"),
  tableOutput("head"),
  selectInput("var1", "Select Variable", choices = NULL),
  selectInput("var2", "Select Variable", choices = NULL),
  selectInput("var3", "Select Variable", choices = NULL),
  selectInput("var4", "Select Variable", choices = NULL),
  selectInput("var5", "Select Variable", choices = NULL),
  actionButton("go","Run model"),
  verbatimTextOutput("glmSummary")
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
  

  
  output$regressio <- renderPrint({
    glmer(f(), data = data(), family = binomial)
  })
  
  output$glmSummary <- renderPrint({
    req(glmModel())
    summary(glmModel())
  })
  



  output$head <- renderTable({
    head(data())
  })
  } 


shinyApp(ui = ui, server = server) 

