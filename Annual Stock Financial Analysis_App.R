  
  #Packages Required for the Application's Components to work
  library("tidyverse")
  library("dplyr")
  library("plotly")
  library("shiny")
  
  # Use a fluid Bootstrap layout
  ui = 
    fluidPage(
      titlePanel("Annual Financial Stock Analysis"), #Title of the page/application
      
      sidebarLayout(   #Generates a row with SideBar input   
        sidebarPanel( # Define the sidebar with one input
          
          selectInput("stock", #InputID
                      "Choose the Stock:", #Input Title
                      choices= c( #Input Value
                        "Apple"=1,
                        "Bristol-Myers Squibb"=2,
                        "Costco Inc"=3,
                        "Microsoft"=4,
                        "TripAdvisor"=5,
                        "Visa Inc."=6,
                        "Yahoo Inc."=7
                        )
                      ),
                    
        #RadioButtons to choose the Financial Parameter
        radioButtons("param", #InputID
                    "Choose the Financial Parameter:", #Input Title
                    choices= c( #Input Value
                      "Cash and Cash Equivalents"=1,
                      "Total Current Assets"=2,
                      "Total Current Liabilities"=3,
                      "Short Term Investments Due"=4,
                      "Current Ratio"=5,
                      "Operation Margin"=6,
                      "Quick Ratio"=7
                      
                      )
                    )
      
        ),
        
        mainPanel( #Create a spot for the Plot
          plotlyOutput("stockPlot")  
                 )
                  
                )
              )
  
  # Define a server for the Shiny app
  server<-function(input, output) { #Define a server for the Shiny app
  
   
    #Creating a dataframe consisting of all the financial parameters of stocks
    Annual_Analysis<-Annual_Balance_Info%>% select("Financial_Year","Ticker_Symbol","Cash_And_Cash_Equivalents","Total_Current_Assets","Total_Current_Liabilities","Short_Term_Investments","Operation_Margin","Quick_Ratio","Cash_Ratio")
    Annual_Analysis<-na.omit(Annual_Analysis)
    Annual_Analysis$Current_Ratio<-Annual_Analysis$Total_Current_Assets/Annual_Analysis$Total_Current_Liabilities
    
    #Creating reactive components which will assign dynamic values
    Stock<-reactive({
      switch(input$stock,
             "1"="AAPL",
             "2"="BMY",
             "3"="COST",
             "4"="MSFT",
             "5"="TRIP",
             "6"="V",
             "7"="YHOO"
             )
    })
    
    Year<-c("2013","2014","2015","2016") #Data Frame consisting of the Financial Years to be displayed on X-Axes
    
    output$stockPlot <- renderPlotly({ #Fill in the spot we created for a plot
      Choose_Stock<-Annual_Analysis%>%
        filter(Ticker_Symbol==Stock()) #Filter based on the Stock Name chosen
  
      Fin_Param<-reactive({
        switch(input$param,
               "1"=Choose_Stock$Cash_And_Cash_Equivalents,
               "2"=Choose_Stock$Total_Current_Assets,
               "3"=Choose_Stock$Total_Current_Liabilities,
               "4"=Choose_Stock$Short_Term_Investments,
               "5"=Choose_Stock$Current_Ratio,
               "6"=Choose_Stock$Operation_Margin,
               "7"=Choose_Stock$Quick_Ratio
               
        )
      })
      p<-plot_ly(x = ~Year, y = Fin_Param(), type = 'bar', color='Red') #Render the Plot
      layout(p, xaxis = list(title = "Financial Year"),title='Annual Financial Stock Analysis') #Name the axes of Plot
    })
  }
  
  #Connecting UI and Server
  shinyApp(ui=ui, server=server)

