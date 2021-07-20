- 👋 Hi, I’m @shekharmondal
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

dataset <- as.data.frame(read.csv("Final.csv"))
df1 <- dataset[dataset$Party == "BJP",]
df2 <- dataset[dataset$Party == "AITC",]
df3 <- dataset[dataset$Party == "BSP",]
df4 <- dataset[dataset$Party == "CPI(M)",]


header <- dashboardHeader(title = "Candidate Summary")  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary", tabName = "dashboard", icon = icon("dashboard")),
    
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "http://jantakamood.com/")
  )
)
frow1 <- fluidRow(
   valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
  ,valueBoxOutput("value4")
  ,valueBoxOutput("value5")
)

frow2 <- fluidRow(
  
  box(
    title = "BJP Age Distribution"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("BJPa", height = "150px")
  )
  
  ,box(
    title = "AITC Age Distribution"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("AITCa", height = "150px")
  ) 
  
  ,box(
    title = "BSP Age Distribution"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("BSPa", height = "150px")
  ) 
  
  ,box(
    title = "CPI(M) Age Distribution"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("CPIa", height = "150px")
  )
)

body<- dashboardBody(frow1,frow2)

ui<-dashboardPage(title = 'Assembly Elections 2011-2016-2021',header, sidebar, body, skin='red')

server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  total.candidates <- nrow(dataset)
  total.males <- nrow(dataset[dataset$Gender=="M",])
  total.females <- nrow(dataset[dataset$Gender == "F",])
  mean.assets <- mean(dataset$Assets)
  median.age <- median(dataset$Age)
 
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.candidates, format="d", big.mark=',')
      ,paste('Total Candidates:',total.candidates)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(total.males, format="d", big.mark=',')
      ,paste('Total Males:',total.males)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
    
    
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(total.females, format="d", big.mark=',')
      ,paste('Total Females:',total.females)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "yellow")
    
    
  })
  
  output$value4 <- renderValueBox({
    valueBox(
      formatC(mean.assets, format="d", big.mark=',')
      ,paste('Mean Assets:',mean.assets)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
    
  })
  output$value5 <- renderValueBox({
    valueBox(
      formatC(median.age, format="d", big.mark=',')
      ,paste('Median Age:',median.age)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "orange")
    
    
  })
  
  output$BJPa <- renderPlot({
    hist(df1$Age, main="Age distribution for BJP")
  })
  output$AITCa <- renderPlot({
    hist(df2$Age, main="Age distribution for AITC")
  })
  output$BSPa <- renderPlot({
    hist(df3$Age, main="Age distribution for BSP")
  })
  output$BJPa <- renderPlot({
    hist(df4$Age, main="Age distribution for CPI(M)")
  })
}
shinyApp(ui, server)
  
