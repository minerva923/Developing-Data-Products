# server.R
library(shiny)
library(dplyr)

shinyServer(function(input, output) {
    
    file <- reactive({
        inFile <- input$file
        if (is.null(inFile)) return(NULL)
        data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    })
    
    dataInput <- reactive({
        
        if (is.null(file())) {data <- read.csv("./data/Daily.csv", stringsAsFactors = FALSE)}
        else {data <- file()}
        ## Assign colnames
        col <- c("Date", "Campaign", "AdSet", "Ad", "Spending", "PageLikes", 
                 "Leads", "VideoViews", "MobileAppInstalls")
        colnames(data)[c(1, 3:5, 9:13)] <- col
        data$Date <- as.POSIXct(strptime(data$Date, "%Y-%m-%d"))
        ## Create Type
        Type <- vector("character", length(data[,1]))
        Type[grep('pagepost', data$Ad)] <- "PagePost"
        Type[grep('pagelike', data$Ad)] <- "PageLike"
        Type[Type==""] <- "Others"
        data <- mutate(data, CTR=Clicks/Impressions, CPC=Spending/Clicks, 
                       CPL=Spending/Leads, CPV=Spending/VideoViews)
        
        data <- cbind(data[3:6], data[1], data[7:17], Type)
        ## Recode Placement
        data$Placement[grep('[Rr]ight', data$Placement)] <- "RHS"
        data$Placement[grep('[Mm]obile', data$Placement)] <- "Mobile"
        data$Placement[grep('[Dd]esktop', data$Placement)] <- "Desktop"
        return(data)
    })

    dots <- reactive({
        dots <- lapply(input$level, as.symbol)
    })
    
    f <- reactive({
        lapply(input$var, function(x) paste("sum(", as.name(x), ")", sep = ""))
        #substitute(sum(var, na.rm = TRUE), list(var = as.name(input$var)))
    })
   
    finalInput <- reactive({
        
        tbl_df(dataInput()) %>% 
            group_by_(.dots=dots()) %>%
            summarise_(.dots=f()) 
    })
    
    ROInput <- reactive({
        tbl_df(dataInput()) %>% 
            group_by(Campaign, Type) %>%
            summarise(NetClicks=sum(Clicks), Spending=sum(Spending)) %>%
            mutate(EndDate=as.POSIXct('2015-04-30'), LeftDays=EndDate-as.POSIXct(Sys.Date()), 
                   TargetClicks=sample(5000:20000, length(Campaign), T), 
                   LeftClicks=TargetClicks-NetClicks, CPC=Spending/NetClicks,
                   Budget=sample(300:6500, length(Campaign), T), 
                   ROI=(Budget-Spending)/Budget) %>%
            select(Campaign:Type, EndDate:TargetClicks, NetClicks, LeftClicks, CPC, 
                   Budget, Spending, ROI)
    })

    output$performance <- renderDataTable({
        if ("Date" %in% names(finalInput())) {
        idate <- as.POSIXct(strptime(input$dates, "%Y-%m-%d"))
        filter(finalInput(), Date >= idate[1] & 
                   Date <= idate[2])
        }
        else {finalInput()}
        }, options = list(orderClasses = TRUE))  

    output$ROI <- renderDataTable({
        ROInput()
    }, options = list(orderClasses = TRUE))


})