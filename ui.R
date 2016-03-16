library(shiny)
shinyUI(fluidPage(
  titlePanel(title=h3("Personal Loan Underwriting Tool (pluto)",align="center")),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "channel", "Channel", choices = c("Affiliates", "DirectMail","Email","Homesite","PaidSearch")),
      selectInput(inputId = "FICO", "FICO", choices = c("660-700","700-740","740-850")),
      sliderInput(inputId ="loanAmt", label = "Loan Amount", min=0, max = 40000, step=5000,value=10000),
      sliderInput(inputId ="tenor", label = "Tenor", min=12, max = 72, step=12,value=36),
      sliderInput(inputId ="APR", label = "APR", min=4.99, max = 28.99, step=1,value=9.99),
      actionButton("go", "Run"),
      downloadButton('downloadData', 'Download Monthly Cash Flows')
    ),
    mainPanel(
      tabsetPanel(type="tab", 
                  tabPanel("Monthly Cash Flow", dataTableOutput("df11")),
                  tabPanel("Annual Summary",dataTableOutput("df22")),
                  tabPanel("Overall Summary",dataTableOutput("df33")),
                  tabPanel("Plot", plotOutput("plot1"))
      )
    )
  ))
)