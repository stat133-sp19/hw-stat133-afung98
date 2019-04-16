#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(ggplot2)
library(reshape2)
source('functions.R')

# Define UI for application that produces output
ui <- fluidPage(
   
   # Application title
   titlePanel("Saving-Investing Modalities"),
   #creating input widgets
   fluidRow(column(4, wellPanel(
         sliderInput(inputId = "initial",
                     label = "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000, pre = "$", step = 500))),
         column(4, wellPanel(sliderInput(inputId = "rate", 
                                         label = "Return Rate",
                                         min = 0,
                                         max = 20,
                                         value = 5,
                                         step = 0.1,
                                         post = "%"))),
         column(4, wellPanel(sliderInput(inputId = "years", 
                                         label = "Years",
                                         min = 0,
                                         max = 50,
                                         value = 20,
                                         step = 1))),
        column(4, wellPanel(sliderInput(inputId = "contrib", 
                     label = "Annual Contribution",
                     min = 0,
                     max = 50000,
                     value = 2000,
                     step = 500,
                     pre = "$"))),
     
         column(4, wellPanel(sliderInput(inputId = "growth", 
                     label = "Growth Rate",
                     min = 0,
                     max = 20,
                     value = 2,
                     step = 0.1,
                     post = "%"))),
       
        column(4, wellPanel(selectInput(inputId = "facet",
                     label = "Facet?",
                     choices = c("Yes", "No"))))
       ),
    
      
      # Show a modalities plot and table
      mainPanel(
         h4("Timeline"),
        #timeline plot
         plotOutput("modPlot"),
         h4("Balances"),
         #balances table
         verbatimTextOutput("balances")
      )
   )

# Define server logic required to produce output
server <- function(input, output) {

  modalities <- reactive({

    no_contrib <- c(input$initial, rep(0, input$years))
    for (i in 0:input$years) {
      no_contrib[i+1] <- future_value(amount = input$initial, rate = input$rate/100, i)
    }
    fixed_contrib <- c(input$initial, rep(0, input$years))
    for (i in 0: input$years) {
      fixed_contrib[i+1] <- (future_value(amount = input$initial, rate = input$rate/100, i)) + annuity(contrib = input$contrib, rate = input$rate/100, i)
    }
    growing_contrib <- c(input$initial, rep(0, input$years))
    for (i in 0:input$years) {
      growing_contrib[i+1] <- (future_value(amount = input$initial, rate = input$rate/100, i)) + growing_annuity(contrib = input$contrib, rate = input$rate/100, growth = input$growth/100, i)
    }
    modalities <- data.frame(
      year = 0:input$years,
      no_contrib,
      fixed_contrib,
      growing_contrib
    )
    return(modalities)
  })
 #reshaping data for facet wrap later
  dat <- reactive({
    melt(modalities(), id.vars = 'year', value.name = 'balances')
  })
  
  #creating the interactive plot and including facet
  output$modPlot <- renderPlot({
    if (input$facet == 'No') {
    facet_no <- ggplot(data = modalities()) +geom_line(aes(year, no_contrib, color = "no contribution")) +
       geom_line(aes(year, fixed_contrib, color = "fixed contribution")) + 
      geom_line(aes(year, growing_contrib, color = "growing contribution")) + 
      labs(x = "Years", y = "Savings", title = "Savings Modalities Timeline Graph", color = "Modalities")
    facet_no}
    else {
     facet_yes <- ggplot(data = dat(), aes(x = year, y = balances)) + geom_area(aes(fill = variable)) +
       geom_point(aes(color = variable)) + geom_line(aes(color = variable)) + facet_wrap(~variable)
     facet_yes  
    }
    
  })
  
  #printing interactive balances table
  output$balances <- renderPrint({
    head(modalities(), 10)
  })
  

}
# Run the application 
shinyApp(ui = ui, server = server)

