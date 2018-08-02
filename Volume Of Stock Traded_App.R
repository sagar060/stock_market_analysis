
#Packages Required for the Application's Components to work
library("tidyverse")
library("dplyr")
library("plotly")
library("shiny")

# Use a fluid Bootstrap layout
ui = 
  fluidPage(
    titlePanel("Volume of Stock Traded from 2012-2016"), #Title of the page/application
    
    sidebarLayout(   #Generates a row with SideBar input   
      sidebarPanel( # Define the sidebar with one input
        
        selectInput("duration", #InputID
                    "Duration:", #Input Title
                    choices= c( #Input Value
                      "Yearly"=1,
                      "Monthly"=2,
                      "Weekly"=3,
                      "Daily"=4 )
                  )
      ),
      
      mainPanel( #Create a spot for the Plot
        plotlyOutput("stockPlot")  
               )
                
              )
            )

# Define a server for the Shiny app
server<-function(input, output) { #Define a server for the Shiny app

  names(Volume_Year)<-c("Duration","Total")
  names(Volume_Month)<-c("Duration","Total")
  names(Volume_Weekday)<-c("Duration","Total")
  names(Volume_Day)<-c("Duration","Total")
  
  #Creating a dataframe consisting of total Volume of stocks as per different duration
  Volume_Stocks<-rbind(Volume_Year,Volume_Month,Volume_Weekday,Volume_Day) 
  
  #Creating reactive components which will assign dynamic values
  Duration<-reactive({
    switch(input$duration,
           "1"=Volume_Stocks$Duration[1:5], 
           "2"=Volume_Stocks$Duration[6:17],
           "3"=Volume_Stocks$Duration[18:22], 
           "4"=Volume_Stocks$Duration[23:53]
           )
  })
  
  Total<-reactive({
    switch(input$duration,
           "1"=Volume_Stocks$Total[1:5], 
           "2"=Volume_Stocks$Total[6:17],
           "3"=Volume_Stocks$Total[18:22], 
           "4"=Volume_Stocks$Total[23:53]
    )
  })
  
  output$stockPlot <- renderPlotly({ #Fill in the spot we created for a plot
    p<-plot_ly(x = Duration(), y = Total(), type = 'bar', name = 'Duration') #Render the Plot
    layout(p, xaxis = list(title = "Duration"),yaxis = list(title = 'Volume of stocks traded'),title='Volume of stocks traded') #Name the axes of Plot
  })
}

#Connecting UI and Server
shinyApp(ui=ui, server=server)

