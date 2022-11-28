#################################################################################################
# Project: ALCM
# Version: 1.0
# Date: 10/21/2021
# Description: Shiny R dashboard to priotize the testing methods
# Author: Saranya Murugan
# Change: New smartsheet added as per comments and Rshiny code updated accordingly
#################################################################################################


library(rsmartsheet)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(timevis)
library(ggrepel)
library(dplyr)
library(shinyWidgets)
library(DT)
library(lubridate)


#------ Mandatory environment variables ------#
source("template.R")

ui <- dashboardPage(
    dashboardHeader(title = "ALCM dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("ALCM_Data", tabName = "file", icon = icon("home")),
            menuItem("Four Quadrant Chart", tabName = "Chart1", icon = icon("bar-chart-o")),
            menuItem("Gantt Chart", tabName = "Chart2", icon = icon("calendar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "file",
                    fluidRow(
                        box(
                            dropdownButton(circle = FALSE,label = "Filter site",
                                           checkboxGroupInput(inputId = "sel_site", 
                                                              label = "Filter site", 
                                                              "Names",
                                                              selected = "runtime"),
                                           actionLink('site', "Select All"))),
                        box(
                            
                            dropdownButton(circle = FALSE,label = "Filter content",
                                           checkboxGroupInput(inputId = "sel_content", 
                                                              label = "Filter content", 
                                                              choices = NULL,
                                                              selected = "runtime")
                            )),
                        box(
                            title = "Data Frame", width = 10, height = "600px",
                            DT::dataTableOutput('contents')
                        )
                    )
            ),
            tabItem(tabName = "Chart1",
                    h2("Four Quadrant Chart"),
                    fluidRow(
                        box(width = 10, height = "600px",
                            plotOutput("MyPlot")),
                        box(width = 10, height = "600px",
                            DT::dataTableOutput("quad"))
                        )
                    ),
            tabItem(tabName = "Chart2",
                    h2("Gantt Chart"),
                    box(width = 15, height = "600px",timevisOutput("MyPlot2"))
            )
        )
    )
)
server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
    
    set_smartsheet_api_key("FrM3ZkOfvrLLL7hTCDO7T3llexDJMS7ObVBN8")
    data = (get_sheet_as_csv("ALCM"))
    df <- read.table(textConnection(data), header = T, sep = ",", stringsAsFactors = T)
    df<- df[c('G4','G3','Reach.Score','Impact.Score','Confidence.Score','Effort.Score','RICE.Score')]
    df<-df %>% rename( site =G4, content=G3)
    
    
    data <- reactive({
        df
        if (is.null(input$sel_site) && is.null(input$sel_content)){
            df <- df
            df$X = round(((df$Reach.Score*df$Impact.Score*(df$Confidence.Score))^(1/3))/((max(df$Reach.Score)*max(df$Impact.Score)*max(df$Confidence.Score))^(1/3))*100,2)
            df$Y = round((df$Effort.Score/max(df$Effort.Score))*100,2)
            df$RICE.Score = round(df$RICE.Score,2)
        }
        else if ((!is.null(input$sel_content)) &&(is.null(input$sel_site))) {
            df <- df %>% filter(content %in% input$sel_content)
            df$X = round(((df$Reach.Score*df$Impact.Score*(df$Confidence.Score))^(1/3))/((max(df$Reach.Score)*max(df$Impact.Score)*max(df$Confidence.Score))^(1/3))*100,2)
            df$Y = round((df$Effort.Score/max(df$Effort.Score))*100,2)
            df$RICE.Score = round(df$RICE.Score,2)
        }
        
        else{
            #target = c(input$sel_site)
            df <- df %>% filter(site %in% (input$sel_site)) %>% filter(content %in% (input$sel_content))
            df$X = round(((df$Reach.Score*df$Impact.Score*(df$Confidence.Score))^(1/3))/((max(df$Reach.Score)*max(df$Impact.Score)*max(df$Confidence.Score))^(1/3))*100,2)
            df$Y = round((df$Effort.Score/max(df$Effort.Score))*100,2)
            df$RICE.Score = round(df$RICE.Score,2)
        }
        return(df %>% select(1,2,8,9,7))
    })
    
    #output$contents <- renderTable({
        #data()
   # })

    output$contents <- DT::renderDataTable(
        data(),
        options = list(scrollX = TRUE)
    )
    
    #update select input dynamically
    
    observe({
        if (input$site%%2 == 0)
        {
            updateCheckboxGroupInput(session,"sel_site","Choose site(s):",choices=unique(df$site))
        }
        else
        {
            updateCheckboxGroupInput(session,"sel_site","Choose site(s):",choices=unique(df$site),selected=unique(df$site))
        }
        
    })
    
    observe({
        if (is.null(input$sel_site))
        {
            updateCheckboxGroupInput(session,"sel_content","Choose content(s):",choices=unique(df$content))
        }
        
        else
        {
            x <- df %>% filter(df$site %in% input$sel_site)
            updateCheckboxGroupInput(session,"sel_content","Choose content(s):",choices=(x$content),selected=(x$content))
        }
        
    })
    
    
    output$MyPlot <- renderPlot({
        ggplot(data(), aes(x = data()$X, y = data()$Y, label = data()$content)) +
            coord_fixed() +
            empty_theme +
            # create the quadrants
            geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100)) +
            geom_segment(aes(x = 0, y = 0, xend = 0, yend = 100)) +
            geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0)) +
            geom_segment(aes(x = 0, y = 50, xend = 100, yend = 50)) +
            geom_segment(aes(x = 50, y = 0, xend = 50, yend = 100)) +
            geom_segment(aes(x = 0, y = 100, xend = 100, yend = 100)) +
            # quadrant labels
            annotate("text", x = 12, y = 5, alpha = 35, label = "Low Effort Low Value") +
            annotate("text", x = 12, y = 95, alpha = 35, label = "High Effort Low Value") +
            annotate("text", x = 88, y = 5, alpha = 35, label = "Low Effort High Value") +
            annotate("text", x = 88, y = 95, alpha = 35, label = "High Effort High Value") +
            labs(x = "Value",
                 y = "Effort") +
            geom_point(colour = "black", size = 2) +
            geom_label_repel(size = 5,
                             fill = "deepskyblue",
                             colour = "black",
                             min.segment.length = unit(0, "lines"))
        
    },height = 550, width = 600)
    
    
    output$quad <- DT::renderDataTable({
        df$RICE.Score = round(df$RICE.Score,2)
        df[c('site','content','RICE.Score')]
    })
    
    data_new = (get_sheet_as_csv("ALCM Time Sheet"))
    df_new <- read.table(textConnection(data_new), header = T, sep = ",", stringsAsFactors = T)
    df_new <- df_new[c('Method','Start.Date','End.Date')]
    df_new <-df_new %>% rename(start =Start.Date, content=Method, end=End.Date)
    df_new <- df_new[!apply(is.na(df_new) | df_new == "", 1, all),]
    df_new$start <- mdy(df_new$start)
    df_new$end <- mdy(df_new$end)
    df_new <- df_new[order(as.Date(df_new$start)),]
    df_new <- df_new %>% mutate(id= 1:n())
    df_new['new'] <- ""
    newdata <- reactive({
        if (is.null(input$sel_content))
        {
            df_new
        }
        else
        {
            df_new <-df_new %>% filter(content %in% input$sel_content)  
        }
            
    })
    
    
    output$MyPlot2 <- renderTimevis({
        timevis(data = data.frame(
            start = newdata()$start,
            end = newdata()$end,
            content = newdata()$content,
            new = newdata()$new,
            group = newdata()$id),
            groups = data.frame(id = 1:nrow(newdata()), content = newdata()$new))
    })
})

# Run the application 
shinyApp(ui = ui, server = server)