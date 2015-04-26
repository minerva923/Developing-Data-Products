# setwd('Developing Data Products/shiny')

library(shiny)

col <- c("Campaign","AdSet","Ad","Placement","Date","Impressions","Clicks",
         "Spending","PageLikes","Leads","VideoViews","MobileAppInstalls","CTR",
         "CPC","CPL","CPV")

shinyUI(fluidPage(
  titlePanel("Facebook Advertising Report"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Social campaigns are important to digital marketers. 
               Aiming to save lots of times from day by day pivot table, 
               this app help us to quickly identify common metrics on Facebook 
               advertising campaigns. Please noticed that all data have been 
               preprocessed to protect client privacy."),
    
      fileInput("file", "Choose CSV File", accept = '.csv'),
    
      dateRangeInput("dates", 
        "Date range",
        start = as.character(Sys.Date()-7), 
        end = as.character(Sys.Date())),
      
      checkboxGroupInput('level', 'Group by:',
                         col[c(1:5)], selected = col[c(2,3,5)]),    
      
      checkboxGroupInput('var', 'Columns to show:',
                         col[c(6:16)], selected = col[c(6:9, 13:14)])
    ),
    
    mainPanel(
        tabsetPanel(
            id = 'dataset',
            tabPanel('Performance', dataTableOutput('performance')),
            tabPanel('ROI', dataTableOutput('ROI'))
        )
        )
  )
))