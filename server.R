library(shiny)

shinyServer(function(input, output){
  
  df1<-reactive({cashFlowAllDrivers(APR=(as.numeric(unlist(input$APR)/100)), 
                                    tenor=as.numeric(unlist(input$tenor)), 
                                    loanAmt=as.numeric(unlist(input$loanAmt)), 
                                    FICO=input$FICO, 
                                    channel=input$channel)}) 
  df2<-reactive({annualSummary(APR=(as.numeric(unlist(input$APR)/100)), 
                               tenor=as.numeric(unlist(input$tenor)), 
                               loanAmt=as.numeric(unlist(input$loanAmt)), 
                               FICO=input$FICO, 
                               channel=input$channel)}) 
  df3<-reactive({overallSummary(APR=(as.numeric(unlist(input$APR)/100)), 
                                tenor=as.numeric(unlist(input$tenor)), 
                                loanAmt=as.numeric(unlist(input$loanAmt)), 
                                FICO=input$FICO, 
                                channel=input$channel)}) 
  
  output$df11 <- renderDataTable({
    input$go
    isolate({df1()})
  })
  output$df22 <- renderDataTable({
    input$go
    isolate({df2()})
  })
  output$df33 <- renderDataTable({
    input$go
    isolate({df3()})
  })
  
  output$plot1 <- renderPlot({
    data4plot<-df1()
    plot<-plot(endBal~month,data=data4plot,ylab="Ending Balance",col="green",main="Ending Balance Trend")
    return(plot) })
  
  output$downloadData <- downloadHandler(
    file = c('monthlyCashFlow.csv'),
    content = function(file) {
      write.csv(df1(), file)
    }
  )
}
)
