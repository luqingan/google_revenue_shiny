
library(shiny)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(markdown)
library(plotly)
library(crosstalk)
library(gridExtra)
library(shinycssloaders)
library(grDevices)
library(devtools)
library(lubridate)
library(usmap)
library(tidyverse)
library(reshape)
library(knitr)
library(kableExtra)


ui <-  shinyUI(navbarPage("Google Analytics Customer Revenue Prediction",
                          tabPanel("Descriptions", fluidPage(
                            uiOutput('descriptions')
                          )),
                          tabPanel("EDA", fluidPage(
                            tags$head(
                              tags$style(HTML(" @import url('https://fonts.googleapis.com/css?family=Roboto:400,700');
                                              h1{
                                              font-family: 'Roboto', cursive;
                                              font-weight: 500;
                                              line-height: 1.5;
                                              color: #2A9FD6;
                                              }
                                              h2{
                                              font-family: 'Roboto', cursive;
                                              font-weight: 500;
                                              line-height: 1.5;
                                              color: #2A9FD6;
                                              }
                                              h3{
                                              font-family: 'Roboto', cursive;
                                              font-weight: 500;
                                              line-height: 1.5;
                                              color: #2A9FD6;
                                              }
                                              figure {
                                              margin: 0 0 1rem;
                                              }
                                              
                                              # img {
                                              # vertical-align: middle;
                                              # border-style: none;
                                              # }
                                              "))
                              ),
                            headerPanel("Google Analytics Customer Revenue Prediction"),
                            sidebarLayout(
                              sidebarPanel(
     
                                helpText("Explore feature distribution"),
                                uiOutput('feature'),
                                uiOutput('fea_top'),

                                helpText("Explore feature correlations"),
                                uiOutput('feacor'),
                                
                                helpText("Maps"),
                                uiOutput('maps'),
                                
                                helpText("Explore time series"),
                                uiOutput('time'),
                                
                                helpText("Prediction"),
                                uiOutput('pred')
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                 
                                            
                                           # verbatimTextOutput('curve_text'),
                                           # verbatimTextOutput('exon_curve_text'),
                                           # verbatimTextOutput('intron_curve_text'),
                                           h2("Data Exploration"),
                                           
                                           h4('Feature distribution'),

                                           plotlyOutput('plot1',height = 600)%>% withSpinner(),
                                           br(),br(),
                                           
                                           h4('Feature correlation'),

                                           plotOutput('plot2',height = 600)%>% withSpinner(),
                                           br(),br(),

                                           h4('Map'),

                                           plotlyOutput('plot3',height = 600)%>% withSpinner(),
                                           br(),br(),
                                           h4('Time series'),
                                           plotlyOutput('plot4',height = 600)%>% withSpinner()
                                  )
                                
                              
                            )#end of main panel 
                              )
                              ),
                          tabPanel("Prediction", fluidPage(
                              tags$head(
                                tags$style(HTML(" @import url('https://fonts.googleapis.com/css?family=Roboto:400,700');
                                                h1{
                                                font-family: 'Roboto', cursive;
                                                font-weight: 500;
                                                line-height: 1.5;
                                                color: #2A9FD6;
                                                }
                                                h2{
                                                font-family: 'Roboto', cursive;
                                                font-weight: 500;
                                                line-height: 1.5;
                                                color: #2A9FD6;
                                                }
                                                h3{
                                                font-family: 'Roboto', cursive;
                                                font-weight: 500;
                                                line-height: 1.5;
                                                color: #2A9FD6;
                                                }
                                                figure {
                                                margin: 0 0 1rem;
                                                }
                                                
                                                # img {
                                                # vertical-align: middle;
                                                # border-style: none;
                                                # }
                                                "))
                                ),
                              headerPanel("Google Analytics Customer Revenue Prediction"),
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  helpText("Make Prediction for new data"),
                                  h3('Data processing'),
                                  fileInput("file1", "Upload DNase CSV File"),
                                
                                  checkboxInput("new", label = 'Show new data'),
                                  
                                  actionButton('go','Make prediction!')
                                ),
                                
                                # Show a plot of the generated distribution
                                mainPanel(
                                  h2("New data"),
                                  conditionalPanel(
                                       condition = "input.new == true",
                                       DT::dataTableOutput("mytable")
                                     ),
                                  DT::dataTableOutput("result")
                                )
                                
                              )
                          )),
                          tabPanel("External Data", fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                
                                
                              )
                            
                          ))
                          ))
)


load('nomiss.rda')
original<- read_csv("train_US_1year_nojson.csv")



#load('original.rda')
# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=30000*1024^2)
  
  ###input
  output$feature <- renderUI({
    selectizeInput("feature",
                   label = "Feature of Interest",
                   choices = colnames(dat)[7:15],
                   multiple = F
    )
  })

  output$fea_top <- renderUI({
    sliderInput("fea_top", "See Top Number:",
              min = 1, max = nrow(cg()), value = 10,step = 1)
  })
  
  output$feacor <- renderUI({
    selectizeInput("feacor",
                   label = "Features of Interest",
                   choices = colnames(dat)[c(7:10,14,15)],
                   multiple = T,
                   selected = c('operatingSystem','deviceCategory'),
                   options = list(maxItems = 2,placeholder = 'Select two features')
    )
  })

  output$maps <- renderUI({
    selectizeInput("maps",
                   label = "Count or Revenue?",
                   choices = c('Visit count', 'Transcation revenue', 'Visit count w/ no-zero transaction'),
                   multiple = F
    )
  })

  output$time <- renderUI({
    selectizeInput("time",
                   label = "Time series",
                   choices = c('Date', 'Weekdays', 'Month','Year'),
                   multiple = F
    )
  })
  percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  }
  
    
    sel_feature <- reactive ({
      match(input$feature,colnames(dat))
    })
    cg = reactive({
      dd = data.frame(table(dat[,sel_feature()]))
      dd %>%
        mutate(percent = percent(Freq/sum(Freq)))
      dd[order(-dd$Freq),]
      
    })
    
    cg_top  = reactive(
      if (nrow(cg()) > input$fea_top){
        cg()[1:input$fea_top,]
      }else{
        cg()
      }
    )
    
    
    # 
    # 
    # output$curve_text = renderPrint({
    #  dim(cg_top())
    # })
    # 
    # output$exon_curve_text = renderPrint({
    #   input$fea_top
    # })
    # output$intron_curve_text = renderPrint({
    #   cg()[1:input$fea_top,]
    # })

    output$plot1 <- renderPlotly({
      plot_ly(cg_top(),x=~reorder(Var1,-Freq),y=~Freq,type = 'bar',text = cg()$percent)%>%
        layout(title = input$feature,
               xaxis = list(title = input$feature),
               yaxis = list(title = "Number"))
    })
    
    sel_feacor <- reactive ({
      match(input$feacor,colnames(dat))
    })

    ob = reactive({
      ob = (table(dat[,(sel_feacor()[1])],(dat[,(sel_feacor()[2])])))
      ob = ob[,which(colSums(ob)!=0)]
      ob = ob[which(rowSums(ob)!=0),]
      melt(ob)
    })
    

    output$plot2 <- renderPlot({
      ob = ob()
      ggplot(ob, aes(Var.1,Var.2)) + geom_tile(aes(fill = value), colour = "white")+
        scale_fill_gradient(low = "white",high = "steelblue")+
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1, size=13))+
        geom_text(aes(label = round(value, 1))) +
        labs(title = paste0(input$feacor[1] , " vs. ", input$feacor[2]), x = input$feacor[1] , y = input$feacor[2])
    })
    
    
    ####### maps 
    l <- list(color = toRGB("white"), width = 2)
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    map.visit <- dat %>%
      select(region,transactionRevenue) %>%
      group_by(region) %>%
      summarise(n = round(log(n()),3),rev = round(log(sum(as.numeric(na.omit(transactionRevenue)))+1),3))
      #rename(full = region)
    
    colnames(statepop)[3] = 'region'

    state.google <- statepop %>%
      select(fips, abbr, region) %>%
      left_join(map.visit, by = "region")

    state.google$n[is.na(state.google$n)] = 0
    state.google$hover <- with(state.google, paste(region))

    
    
    
    map.visit2 <- dat %>%
      select(region,transactionRevenue) %>%
      filter(transactionRevenue>0)%>%
      group_by(region) %>%
      summarise(n = round(log(n()),3),rev = round(log(sum(as.numeric(na.omit(transactionRevenue)))+1),3))
    
    state.google2 <- statepop %>%
      select(fips, abbr, region) %>%
      left_join(map.visit2, by = "region")

    state.google2$n[is.na(state.google2$n)] = 0
    #
    state.google2$hover <- with(state.google2, paste(region))

    output$plot3 <- renderPlotly({
      if (input$maps == 'Visit count'){
            plot_geo(state.google, locationmode = 'USA-states') %>%
              add_trace(
                z = ~n, text = ~hover, locations = ~abbr,
                color = ~n, colors = 'Greens'
              ) %>%
              colorbar(title = "log(Number of visit)") %>%
              layout(
                title = 'Visit per State (log scale)',
                geo = g
              )
      }else if (input$maps == 'Transcation revenue'){
        plot_geo(state.google, locationmode = 'USA-states') %>%
          add_trace(
            z = ~rev, text = ~hover, locations = ~abbr,
            color = ~rev, colors = 'Blues'
          ) %>%
          colorbar(title = "log(TransactionRevenue) USD") %>%
          layout(
            title = 'Google transaction revenue by State (log scale)',
            geo = g
          )
      }else{
        plot_geo(state.google2, locationmode = 'USA-states') %>%
          add_trace(
            z = ~n, text = ~hover, locations = ~abbr,
            color = ~n, colors = 'Reds'
          ) %>%
          colorbar(title = "log(Number of visit) ") %>%
          layout(
            title = 'Visit per State in non-zero transactions (log scale)',
            geo = g
          )
      }

    })
########## time series 
    dat = dat%>%
      mutate(date= ymd(date))
    
    date = dat%>%
      select(date,transactionRevenue) %>%
      group_by((date)) %>%
      summarise(n = round(log(n()),3),rev = round(log(sum(as.numeric(na.omit(transactionRevenue)))+1),3))
    colnames(date)[1] = 'Date'
    
    date$hover <- with(date, paste('Date: ',Date, '<br>', "Visits: ", n,'<br>','Revenue: ', rev))
    
    year = dat%>%
      select(date,transactionRevenue) %>%
      group_by(year(date)) %>%
      summarise(n = round(log(n()),3),rev = round(log(sum(as.numeric(na.omit(transactionRevenue)))+1),3))
    
    colnames(year)[1] = 'Year'
    year$Year = as.character(year$Year)

    mm <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
            'August', 'September', 'October', 'November', 'December')
    month = dat%>%
      select(date,transactionRevenue) %>%
      group_by(month(date)) %>%
      summarise(n = round(log(n()),3),rev = round(log(sum(as.numeric(na.omit(transactionRevenue)))+1),3))
    month = data.frame(Month = mm,month)
    
    month$Month <- factor(month$Month, levels = month[['Month']])
    
    month$hover <- with(month, paste('Month: ',Month, '<br>', "Visits: ", n,'<br>','Revenue: ', rev))
    
    week = dat%>%
      select(date,transactionRevenue) %>%
      group_by(weekdays(date)) %>%
      summarise(n = round(log(n()),3),rev = round(log(sum(as.numeric(na.omit(transactionRevenue)))+1),3))
    
    colnames(week)[1] = 'Week'
    

    week$Week <- factor(week$Week, levels = c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday","Sunday"))
    
    week$hover <- with(week, paste('Week: ',Week, '<br>', "Visits: ", n,'<br>','Revenue: ', rev))
    
    order = c(2,6,7,5,1,3,4)
    week = week[order,]
  
    
    
    
    output$plot4 <- renderPlotly({
      if (input$time == 'Date'){
        plot_ly(x = ~Date, y = ~rev, data = date, mode = 'lines', hoverinfo = 'text', text = date$hover)%>%
          layout(title = "Revenues per day (log scale)",
                 xaxis = list(title = "Day"),
                 yaxis = list(title = "Revenue"))
      }else if (input$time == 'Month'){
        plot_ly(x = ~Month, y = ~rev, data = month,type = 'scatter', mode = 'lines', hoverinfo = 'text', text = month$hover)%>%
          layout(title = "Revenues per month (log scale)",
                 xaxis = list(title = "Month"),
                 yaxis = list(title = "Revenue"))
      }else if (input$time == 'Weekdays'){
        plot_ly(x = ~Week, y = ~rev, data = week,type = 'scatter', mode = 'lines', hoverinfo = 'text', text = week$hover)%>%
          layout(title = "Revenues per weekday (log scale)",
                 xaxis = list(title = "Weekday"),
                 yaxis = list(title = "Revenue"))
      }else{
        ggplot(year,aes(x = Year , y = rev))+
          geom_bar(stat="identity",fill="darkblue")+
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 45, hjust = 1, size=13))+
          geom_text(aes(label=rev ), vjust=0) +
          labs(title = "Revenue per year (log scale)", x = 'Year', y = "Number")
 
      }
    })

       
########################### prediction 

    file <- reactive({
      infile <- input$file1
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      file = read.csv(infile$datapath)
      file[,-1]
    })
    ######## show data 

    output$mytable = DT::renderDataTable({
      file()
    })
  
    # ######################## NN model
# 
#     original <- original %>% dplyr::select(- transactions, -totalTransactionRevenue, -adwordsClickInfo.isVideoAd, -visitStartTime, -referralPath)
# 
#     original.mat <- original %>%
#       mutate(date = ymd(date)) %>%
#       mutate(year = year(date),
#              month = month(date),
#              day = day(date),
#              isMobile = ifelse(isMobile, 1L, 0L),
#              isTrueDirect = ifelse(isMobile, 1L, 0L)) %>%
#       mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
#       dplyr::select(-date, -fullVisitorId, -visitId) %>%
#       mutate_if(is.character, factor) %>%
#       mutate_if(is.factor, fct_lump, prop = 0.01)
# 
#     train.index <- sample(1 : nrow(original.mat), 0.7 * nrow(original.mat), replace = FALSE)
#     original.train <- original.mat[train.index, ]
#     original.test <- original.mat[-train.index, ]
# 
#     
#     original.keras.x <- original.mat %>%
#       dplyr::select(-transactionRevenue) %>%
#       mutate_if(is.factor, fct_explicit_na) %>%
#       mutate_if(is.numeric, funs(ifelse(is.na(.), 0L, .))) %>%
#       mutate_if(is.factor, fct_lump, prop = 0.05) %>%
#       model.matrix(~.-1, .)
#     original.keras.y <- original.mat %>% dplyr::select(transactionRevenue)
#     ## dataset for training model
#     original.keras.train.x <- original.keras.x[train.index, ]
#     original.keras.train.y <- pull(original.keras.y[train.index, ])
#     ## dataset for testing the model
#     original.keras.test.x <- original.keras.x[-train.index, ]
#     original.keras.test.y <- pull(original.keras.y[-train.index, ])
#     ## Train the neural network model
#      model_nn <- keras_model_sequential()
#     model_nn %>%
#       layer_dense(units = 32, activation = "relu", input_shape = ncol(original.keras.x)) %>%
#       layer_dropout(rate = 0.1) %>%
#       layer_dense(units = 16, activation = "relu") %>%
#       layer_dropout(rate = 0.1) %>%
#       layer_dense(units = 1, activation = "linear")
#     model_nn %>% compile(loss = "mean_squared_error",
#                          optimizer = optimizer_rmsprop())
#     history <- model_nn %>%
#       fit(original.keras.train.x, log(original.keras.train.y + 1),
#           epochs = 100,
#           batch_size = 4096,
#           verbose = 1,
#           validation_split = 0.2)
#  
#     pred.res <- reactive({
#       if (is.null(input$file1)) {
#                 predict(model_nn, file())
#         }
#       })
     
    pred_new = reactive({
      if (is.null(input$file1)) {
        nn = cbind(pred.res,file)
        colnames(nn)[1] = 'Predicted Revenue'
      }
    })
    
    output$result = DT::renderDataTable({
      pred_new()
    })
    
    output$descriptions <- renderUI({
      withMathJax(includeMarkdown('info.md'))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

