#setwd("C:/r/b00705231Project/")
# getwd()

# str(myData)
# attach(myData)
# is.data.frame(dataPlot)

library(forecast)
library(DT)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(tidyr)
library(shinyjs)

library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(prophet)
library(dygraphs)
library(shinycssloaders)
library(rsconnect)
library(httr)
library(jsonlite)
library(RMySQL)
library(dbConnect)
library(RSQLite)
library(RODBC)
library(gridExtra)



##connecting to shinyapps.io

rsconnect::setAccountInfo(name='b00705231',
                          token='78295776A93813A19B1C851A271E8285',
                          secret='Urq9xIvMIpLouUPw2XLuanDtshVgqOOAagKvW+9o')


##initialise CSV Files

genderData <-read.csv("gender.csv")
ageData <- read.csv("ages.csv")
ethnicData <- read.csv("ethnicity.csv")
disabilityData <- read.csv("disability.csv")
subjectData <- read.csv("subjects.csv")

foundationDegreeData <-read.csv("fDegreeQualifiedData.csv")

englandData <- read.csv("englandData.csv")
englandDataFT <- read.csv("englandDataFT.csv")
englandDataPT <- read.csv("englandDataPT.csv")

walesData <- read.csv("walesData.csv")
walesDataFT <- read.csv("walesDataFT.csv")
walesDataPT <- read.csv("walesDataPT.csv")

scotlandData <- read.csv("scotlandData.csv")
scotlandDataFT <- read.csv("scotlandDataFT.csv")
scotlandDataPT <- read.csv("scotlandDataPT.csv")

niData <- read.csv("northernIrelandData.csv")
niDataFT <- read.csv("northernIrelandDataFT.csv")
niDataPT <- read.csv("northernIrelandDataPT.csv")

ukSubjectFT <- read.csv("ukSubjectAreasFT.csv")
ukSubjectPT <- read.csv("ukSubjectAreasPT.csv")

jobData <- read.csv("jobVacancies.csv")

openUniData <- read.csv("openUniversity.csv")




##initialise Drop Down list for Past Data Tab

datasets <- list(
  
  `Gender Information` = genderData,
  `Age Information` = ageData,
  `Ethnicity Information` = ethnicData,
  `Disability Information` = disabilityData,
  `Subject Information` = subjectData
  
)


##SERVER CODE
server = function(input, output, session){
  options(scipen=999)
  
  
  ##initialise DB
  #con = odbcConnect(MySQL(), user='root', password="tv0@Oqmh", dbname='predicttrenddb', host='localhost')
  #con <- odbcConnect("predicttrenddb", uid="root", pwd="tv0@Oqmh", case="postgresql")
  
  
  
  
  
  ##past data table in past data tab
  # output$table <- renderDataTable({ 
  #   req(input$data)
  #   datasets[[input$data]]
  # })
  
  
  
  
  
  # lapply(dbListConnections(MySQL()), dbDisconnect)
  
  observe({
    
    r<- GET("https://www.reed.co.uk/api/1.0/search", query = list(keywords = input$keyword), authenticate("a32b16cc-f522-494b-8a70-8513a1fd48c2", ""))
    
    parsed_content <- content(r, "parsed")
    parsed_content
    
    output$jobTitle1 <- renderText({
      parsed_content$results[[1]]$jobTitle
    })
    output$employer1 <- renderText({
      parsed_content$results[[1]]$employerName
    })
    output$location1 <- renderText({
      parsed_content$results[[1]]$locationName
    })
    
    output$jobTitle2 <- renderText({
      parsed_content$results[[2]]$jobTitle
    })
    output$employer2 <- renderText({
      parsed_content$results[[2]]$employerName
    })
    output$location2 <- renderText({
      parsed_content$results[[2]]$locationName
    })
    
    output$jobTitle3 <- renderText({
      parsed_content$results[[3]]$jobTitle
    })
    output$employer3 <- renderText({
      parsed_content$results[[3]]$employerName
    })
    output$location3 <- renderText({
      parsed_content$results[[3]]$locationName
    })
    
    output$jobTitle4 <- renderText({
      parsed_content$results[[4]]$jobTitle
    })
    output$employer4 <- renderText({
      parsed_content$results[[4]]$employerName
    })
    output$location4 <- renderText({
      parsed_content$results[[4]]$locationName
    })
    
    output$tResults <- renderText({
      paste("Total Number of ", input$keyword , " Jobs available: ", parsed_content$totalResults)
    })
    
    
    if(input$keyword != ""){
      if (parsed_content$totalResults == 0){
        output$numberResults <- renderUI ({
          fluidRow(
            column(2),
            column(8,
                   align = "center",
                   
                   
                   div(class="panel-body",  width = "1000px",
                       align = "center",
                       div(
                         
                         h4(
                           
                           textOutput("tResults")
                         ),
                         
                       )
                   )
                   
            ),
            column(2)
          )
          
        })
        
      } else {
        output$oneJob <- renderUI({
          
          fluidRow(align = "center",
                   
                   
                   column(3,
                          
                          div(class="panel-body",  width = "1000px",
                              align = "center",
                              div(
                                tags$img(src = "one.png", 
                                         width = "50px", height = "50px")
                              ),
                              div(
                                
                                h4(strong(
                                  
                                  textOutput("jobTitle1")
                                )),
                                h5(
                                  textOutput("employer1")
                                ),
                                h6(
                                  textOutput("location1")
                                )
                              )
                          )
                          
                   ),
                   column(3,
                          
                          div(class="panel-body",  width = "1000px", 
                              align = "center",
                              div(
                                tags$img(src = "two.png", 
                                         width = "50px", height = "50px")
                              ),
                              div(
                                h4(strong(
                                  
                                  textOutput("jobTitle2")
                                )),
                                h5(
                                  textOutput("employer2")
                                ),
                                h6(
                                  textOutput("location2")
                                )
                              )
                          )
                          
                   ),
                   column(3,
                          
                          div(class="panel-body",  width = "1000px", 
                              align = "center",
                              div(
                                tags$img(src = "three.png", 
                                         width = "50px", height = "50px")
                              ),
                              div(
                                h4(strong(
                                  
                                  textOutput("jobTitle3")
                                )),
                                h5(
                                  textOutput("employer3")
                                ),
                                h6(
                                  textOutput("location3")
                                )
                              )
                          )
                          
                   ),
                   column(3,
                          
                          div(class="panel-body",  width = "1000px", 
                              align = "center",
                              div(
                                tags$img(src = "four.png", 
                                         width = "50px", height = "50px")
                              ),
                              div(
                                h4(strong(
                                  
                                  textOutput("jobTitle4")
                                )),
                                h5(
                                  textOutput("employer4")
                                ),
                                h6(
                                  textOutput("location4")
                                )
                              )
                          )
                          
                   )
                   
          )
          
          
        })
        output$numberResults <- renderUI ({
          fluidRow(
            column(2),
            column(8,
                   align = "center",
                   
                   
                   div(class="panel-body",  width = "1000px",
                       align = "center",
                       div(
                         
                         h4(
                           
                           textOutput("tResults")
                         ),
                         
                       )
                   )
                   
            ),
            column(2)
          )
          
        })
      }
    }
    
    
  }) ##Reed Job Search
  
  
  
  ##SubjectAreaTab
  
  observe({
    if(input$country == "All" && input$mode == "Full-Time"){
      subAreaTS = ts(ukSubjectFT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(ukSubjectFT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
        
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
      
      
    }
    else if (input$country == "All" && input$mode == "Part-Time") {
      subAreaTS = ts(ukSubjectPT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(ukSubjectPT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
        
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
        
      } else{
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
    }else if (input$country == "All" && input$mode == "Both Full-Time and Part-Time"){
      subAreaTS = ts(subjectData[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(subjectData$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
      
    }else if (input$country == "England" && input$mode == "Full-Time"){
      subAreaTS = ts(englandDataFT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(englandDataFT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if (input$country == "England" && input$mode == "Part-Time"){
      subAreaTS = ts(englandDataPT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(englandDataPT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      # output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if(input$country == "England" && input$mode == "Both Full-Time and Part-Time"){
      subAreaTS = ts(englandData[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(englandData$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    } else if (input$country == "Wales" && input$mode == "Full-Time"){
      subAreaTS = ts(walesDataFT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(walesDataFT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if (input$country == "Wales" && input$mode == "Part-Time"){
      subAreaTS = ts(walesDataPT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(walesDataPT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      # output$subTable <- DT::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
    }else if (input$country == "Wales" && input$mode == "Both Full-Time and Part-Time"){
      subAreaTS = ts(walesData[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(walesData$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if (input$country == "Scotland" && input$mode == "Full-Time"){
      subAreaTS = ts(scotlandDataFT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(scotlandDataFT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      # output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if (input$country == "Scotland" && input$mode == "Part-Time"){
      subAreaTS = ts(scotlandDataPT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(scotlandDataPT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
    }else if (input$country == "Scotland" && input$mode == "Both Full-Time and Part-Time"){
      subAreaTS = ts(scotlandData[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(scotlandData$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if (input$country == "Northern Ireland" && input$mode == "Full-Time"){
      subAreaTS = ts(niDataFT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(niDataFT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      # output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
    }else if (input$country == "Northern Ireland" && input$mode == "Part-Time"){
      subAreaTS = ts(niDataPT[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(niDataPT$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      } else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
      
    }else if (input$country == "Northern Ireland" && input$mode == "Both Full-Time and Part-Time"){
      subAreaTS = ts(niData[(as.numeric(input$subArea))], start=2000)
      subAreaTSTrain = window(subAreaTS, start = 2000, end = 2014)
      subAreaTSTest = window(subAreaTS, start = 2015)
      
      trainArima = auto.arima(subAreaTSTrain, stepwise = T, approximation = F, trace = T)
      testArima = auto.arima(subAreaTSTest, stepwise = T, approximation = F, trace = T)
      trainETS = ets(subAreaTSTrain)
      testETS = ets(subAreaTSTest)
      trainHolt = holt(subAreaTSTrain, h = 4)
      testHolt = holt(subAreaTSTest, h = 1)
      trainHoltDamped = holt(subAreaTSTrain, h = 4, damped = T)
      testHoltDamped = holt(subAreaTSTest, h = 1, damped = T)
      
      subAreaArima = auto.arima(subAreaTS, stepwise = T, approximation = F, trace = T)
      subAreaETS = ets(subAreaTS)
      subAreaHolt = holt(subAreaTS, h = input$subYearsPredicted)
      subAreaHoltDamped = holt(subAreaTS, h = input$subYearsPredicted, damped = T)
      #subAreaHW = hw(subAreaTS)
      
      #rmse <- function(error) {sqrt(mean(error^2))}
      
      aiccArima <- trainArima$aicc
      aiccETS <- trainETS$aicc
      aiccHolt <- trainHolt$model$aicc
      aiccHoltDamped <- trainHoltDamped$model$aicc
      
      rmseArima <- as.data.frame(accuracy(trainArima))[2]
      rmseETS <- as.data.frame(accuracy(trainETS))[2]
      rmseHolt <- as.data.frame(accuracy(trainHolt))[2]
      rmseHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[2]
      
      mapeArima <- as.data.frame(accuracy(trainArima))[5]
      mapeETS <- as.data.frame(accuracy(trainETS))[5]
      mapeHolt <- as.data.frame(accuracy(trainHolt))[5]
      mapeHoltDamped <- as.data.frame(accuracy(trainHoltDamped))[5]
      
      
      if(rmseArima < rmseETS && rmseArima < rmseHolt && rmseArima < rmseHoltDamped){
        chosenSubAreaModel <- subAreaArima
        modelChoice <- "ARIMA"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainArima, h=4)
        
      } else if (rmseETS < rmseArima && rmseETS < rmseHolt && rmseETS < rmseHoltDamped){
        chosenSubAreaModel <- subAreaETS
        modelChoice <- "ETS"
        plotSubArea = forecast(chosenSubAreaModel, h=input$subYearsPredicted)
        #addedTestModel = forecast(trainETS, h=4)
        
      } else if(rmseHolt < rmseArima && rmseHolt < rmseETS && rmseHolt < rmseHoltDamped){
        chosenSubAreaModel <- subAreaHolt
        modelChoice <- "Holt"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHolt, h=4)
        
      } else if(rmseHoltDamped < rmseArima && rmseHoltDamped < rmseETS && rmseHoltDamped < rmseHolt){
        chosenSubAreaModel <- subAreaHoltDamped
        modelChoice <- "Holt Damped"
        plotSubArea = chosenSubAreaModel
        #addedTestModel = forecast(trainHoltDamped, h=4)
      }
      
      output$subModel <- renderValueBox(
        valueBox("Forecast Model:", modelChoice)
      )
      output$textOut <-renderText(
        paste("ARIMA RMSE: ", round(rmseArima, 3), "\nETS RMSE: ", round(rmseETS, 3), "\nHolt RMSE: ", round(rmseHolt, 3), "\nHolt Damped RMSE: ", round(rmseHoltDamped, 3),
              "\n\nARIMA MAPE: ", round(mapeArima, 3), "\nETS MAPE: ", round(mapeETS, 3), "\nHolt MAPE: ", round(mapeHolt, 3), "\nHolt Damped MAPE: ", round(mapeHoltDamped, 3),
              "\n\nARIMA AICc: ", round(aiccArima, 3), "\nETS AICc: ", round(aiccETS, 3), "\nHolt AICc: ", round(aiccHolt, 3), "\nHolt Damped AICc: ", round(aiccHoltDamped, 3))
      )
      
      roundedSub <- round(plotSubArea$mean, 0)
      
      predictedSubValues <- as.data.frame(c(subAreaTS, roundedSub))
      subYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
      predicSub <- append(niData$year, subYear[1:input$subYearsPredicted])
      subAreaTable <- cbind(predicSub, predictedSubValues)
      names(subAreaTable)[1] <-"Academic Year Beginning"
      names(subAreaTable)[2] <-"No. Of Students"
      pSubValues <- c(subAreaTS, roundedSub)
      
      differenceV <- pSubValues[19+input$subYearsPredicted]-pSubValues[19]
      percentageV <- round((differenceV/pSubValues[19])*100, 2)
      
      #output$subTable <- shiny::renderDataTable(datatable(subAreaTable, list(paging=F), rownames = F))
      output$subTable <- renderTable(
        
        subAreaTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
      )
      
      output$subGraph <- renderPlot({
        autoplot(plotSubArea)+
          labs(caption = "Source: Higher Education Statistics Agency")+
          xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
      })
      
      if(pSubValues[19+input$subYearsPredicted] > pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Increase of ", percentageV , "%")
        )
      }else if (pSubValues[19+input$subYearsPredicted] < pSubValues[19]){
        
        output$subPercent <- renderText(
          paste("Forecasted Decrease of ", percentageV , "%")
        )
      }
      else {
        output$subPercent <- renderText(
          paste("Forecasted Change of 0%")
        )
      }
    }
    
    
    
    
  }) ##SubjectAreaGraphCode
  
  output$scienceSubAreaGraph <- renderPlot({
    #subMedDf <- sqlQuery(con, "select medDentist from subjectdata;")
    subTS = ts(subjectData$medDentist, start=2000)
    autoplot(subTS)+
      labs(title = "Number of Students Studying Science Subjects", 
           subtitle = "Total Number of Students Studying Both Part-Time and Full-Time in the UK", 
           caption = "Source: Higher Education Statistics Agency")+
      autolayer(subTS, series="Medicine & Dentistry", lwd = 1.25) +
      autolayer(ts(subjectData$subAlliedToMed, start=2000), lwd = 1.25, series="Subjects Allied to Medicine")+
      autolayer(ts(subjectData$bioScience, start=2000), lwd = 1.25, series="Biological Sciences")+
      autolayer(ts(subjectData$vetScience, start=2000), lwd = 1.25, series="Veterinary Sciences")+
      autolayer(ts(subjectData$agri, start=2000), lwd = 1.25, series="Agriculture")+
      autolayer(ts(subjectData$phyScience, start=2000), lwd = 1.25, series="Physical Sciences")+
      autolayer(ts(subjectData$mathScience, start=2000), lwd = 1.25, series="Mathematical Sciences")+
      autolayer(ts(subjectData$computerSciences, start=2000), lwd = 1.25, series="Computer Sciences")+
      autolayer(ts(subjectData$engTech, start=2000), lwd = 1.25, series="Engineering & Technology")+
      autolayer(ts(subjectData$archBuildPlan, start=2000), lwd = 1.25, series="Architecture, Building & Planning")+
      xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
  }) ##ScienceSubjectsGraph
  
  output$nonScienceSubAreaGraph <- renderPlot({
    nonSubTS = ts(subjectData$socialStudies, start=2000)
    autoplot(nonSubTS)+
      labs(title = ("Number of Students Studying Non-Science Subjects"), 
           subtitle = "Total Number of Students Studying Both Part-Time and Full-Time in the UK", 
           caption = "Source: Higher Education Statistics Agency")+ 
      
      autolayer(nonSubTS, lwd = 1.25, series = "Social Studies") +
      autolayer(ts(subjectData$law, start=2000), lwd = 1.25, series="Law")+
      autolayer(ts(subjectData$BusAdmin, start=2000), lwd = 1.25, series="Business and Administration Studies")+
      autolayer(ts(subjectData$MassCommDoc, start=2000), lwd = 1.25, series="Mass Communication & Documentation")+
      autolayer(ts(subjectData$languages, start=2000), lwd = 1.25, series="Languages")+
      autolayer(ts(subjectData$histPhilosophical, start=2000), lwd = 1.25, series="Historical & Philosophical Studies")+
      autolayer(ts(subjectData$artDesign, start=2000), lwd = 1.25, series="Creative Arts & Design")+
      autolayer(ts(subjectData$education, start=2000), lwd = 1.25, series="Education")+
      autolayer(ts(subjectData$combined, start=2000), lwd = 1.25, series="Combined Courses")+
      xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
  }) ##NonScienceSubjectsGraph
  
  output$openUniGraph <- renderPlot({
    subTS = ts(openUniData$subAlliedToMed, start=2005)
    autoplot(subTS)+
      labs(title = "Number of Students Studying Science Subjects", 
           subtitle = "Total Number of Students Studying Both Part-Time and Full-Time in the UK", 
           caption = "Source: Higher Education Statistics Agency")+
      autolayer(subTS, series="Subjects Allied to Medicine", lwd = 1.25) +
      autolayer(ts(openUniData$bioScience, start=2005), lwd = 1.25, series="Biological Sciences")+
      autolayer(ts(openUniData$agri, start=2005), lwd = 1.25, series="Agriculture")+
      autolayer(ts(openUniData$phyScience, start=2005), lwd = 1.25, series="Physical Sciences")+
      autolayer(ts(openUniData$mathScience, start=2005), lwd = 1.25, series="Mathematical Sciences")+
      autolayer(ts(openUniData$computerSciences, start=2005), lwd = 1.25, series="Computer Sciences")+
      autolayer(ts(openUniData$engTech, start=2005), lwd = 1.25, series="Engineering & Technology")+
      autolayer(ts(openUniData$archBuildPlan, start=2005), lwd = 1.25, series="Architecture, Building & Planning")+
      xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
  }) ##OpenUniversityDegreeGraph
  
  output$foundationGraph <- renderPlot({
    subTS = ts(foundationDegreeData$subAlliedToMed, start=2003)
    autoplot(subTS)+
      labs(title = "Number of Students Studying Science Subjects", 
           subtitle = "Total Number of Students Studying Both Part-Time and Full-Time in the UK", 
           caption = "Source: Higher Education Statistics Agency")+
      autolayer(subTS, series="Subjects Allied to Medicine", lwd = 1.25) +
      autolayer(ts(foundationDegreeData$bioScience, start=2003), lwd = 1.25, series="Biological Sciences")+
      autolayer(ts(foundationDegreeData$agri, start=2003), lwd = 1.25, series="Agriculture")+
      autolayer(ts(foundationDegreeData$phyScience, start=2003), lwd = 1.25, series="Physical Sciences")+
      autolayer(ts(foundationDegreeData$mathScience, start=2003), lwd = 1.25, series="Mathematical Sciences")+
      autolayer(ts(foundationDegreeData$computerSciences, start=2003), lwd = 1.25, series="Computer Sciences")+
      autolayer(ts(foundationDegreeData$engTech, start=2003), lwd = 1.25, series="Engineering & Technology")+
      autolayer(ts(foundationDegreeData$archBuildPlan, start=2003), lwd = 1.25, series="Architecture, Building & Planning")+
      xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
  }) ##FoundationDegreeGraph
  
  #OpenUni Degree Forecast
  observe({
    
    openTS = ts(openUniData[(as.numeric(input$openDataSelect))], start = 2005)
    openTrainTS = window(openTS, start = 2005, end = 2015)
    openTestTS = window(openTS, start = 2016)
    
    openTrainArima = auto.arima(openTrainTS, stepwise = T, approximation = F, trace = T)
    openTestArima = auto.arima(openTestTS, stepwise = T, approximation = F, trace = T)
    openTrainETS = ets(openTrainTS)
    openTestETS = ets(openTestTS)
    openTrainHolt = holt(openTrainTS, h = input$openYearsPredicted)
    openTestHolt = holt(openTestTS, h = input$openYearsPredicted)
    openTrainHoltDamped = holt(openTrainTS, h = input$openYearsPredicted, damped = T)
    openTestHoltDamped = holt(openTestTS, h = input$openYearsPredicted, damped = T)
    
    openArima = auto.arima(openTS, stepwise = T, approximation = F, trace = T)
    openETS = ets(openTS)
    openHolt = holt(openTS, h = input$openYearsPredicted)
    openHoltDamped = holt(openTS, h = input$openYearsPredicted, damped = T)
    
    rmseOpenArima <- as.data.frame(accuracy(openTrainArima))[2]
    rmseOpenETS <- as.data.frame(accuracy(openTrainETS))[2]
    rmseOpenHolt <- as.data.frame(accuracy(openTrainHolt))[2]
    rmseOpenHoltDamped <- as.data.frame(accuracy(openTrainHoltDamped))[2]
    
    
    if(rmseOpenArima <= rmseOpenETS && rmseOpenArima <= rmseOpenHolt && rmseOpenArima <= rmseOpenHoltDamped){
      chosenOpenModel <- openArima
      modelChoice = "ARIMA"
      plotOpen = forecast(chosenOpenModel, h = input$openYearsPredicted)
      
    }else if(rmseOpenETS < rmseOpenArima && rmseOpenETS <= rmseOpenHolt && rmseOpenETS <= rmseOpenHoltDamped){
      chosenOpenModel <- openETS
      modelChoice = "ETS"
      plotOpen = forecast(chosenOpenModel, h = input$openYearsPredicted)
      
    } else if(rmseOpenHolt < rmseOpenETS && rmseOpenHolt < rmseOpenArima && rmseOpenHolt <= rmseOpenHoltDamped){
      chosenOpenModel <- openHolt
      modelChoice = "Holt"
      plotOpen = forecast(chosenOpenModel)
      
    } else if(rmseOpenHoltDamped < rmseOpenETS && rmseOpenHoltDamped < rmseOpenHolt && rmseOpenHoltDamped < rmseOpenArima){
      chosenOpenModel <- openHoltDamped
      modelChoice = "Holt Damped"
      plotOpen = forecast(chosenOpenModel)
      
    }
    
    
    output$openModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    
    output$openTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseOpenArima, 3), "\nETS RMSE: ", round(rmseOpenETS, 3),"\nHolt RMSE: ", round(rmseOpenHolt, 3),"\nHolt Damped RMSE: ", round(rmseOpenHoltDamped, 3))
    )
    
    roundedOpen <- round(plotOpen$mean, 0)
    predictedOpenValues <- as.data.frame(c(openTS, roundedOpen))
    openYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
    predicOpen <- append(openUniData$year, openYear[1:input$openYearsPredicted])
    
    openTable <- cbind(predicOpen, predictedOpenValues)
    names(openTable)[1] <- "Academic Year Beginning"
    names(openTable)[2] <- "No. Of Students"
    pOpenValues <- c(openTS, roundedOpen)
    
    differenceOpenV <- pOpenValues[14+input$openYearsPredicted]- pOpenValues[14]
    percentageOpenV <- round((differenceOpenV/pOpenValues[14])*100, 2)
    
    # output$openUniTable <- shiny::renderDataTable(datatable(openTable, 
    #                                                      list(paging = F),
    #                                                      rownames = F) 
    #                                                      
    #   
    # )
    output$openUniTable <- renderTable(
      
      openTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    output$openPlot <- renderPlot ({
      autoplot(plotOpen) +
        labs(title = "Number of Students Studying through Open University", 
             caption = "Source: Higher Education Statistics Agency")+
        xlab("Academic Year Beginning") + ylab("No. Of Students") + theme(text = element_text(size=15))
    })
    
    if(pOpenValues[14+input$openYearsPredicted] > pOpenValues[14]){
      output$openPercent <- renderText(
        paste("Forecasted Increase of ", percentageOpenV , "%")
      )
    } else if (pOpenValues[14+input$openYearsPredicted] < pOpenValues[14]){
      output$openPercent <- renderText(
        paste("Forecasted Decrease of ", percentageOpenV , "%")
      )
    }else{
      output$openPercent <- renderText(
        paste("Forecasted Change of 0%")
      )
    }
    
  })
  #End of Open Uni Degree Tab
  
  #Foundation Degree Forecast
  observe({
    
    foundationTS = ts(foundationDegreeData[(as.numeric(input$foundationDataSelect))], start = 2003)
    foundationTrainTS = window(foundationTS, start = 2003, end = 2016)
    foundationTestTS = window(foundationTS, start = 2017)
    
    foundationTrainArima = auto.arima(foundationTrainTS, stepwise = T, approximation = F, trace = T)
    foundationTestArima = auto.arima(foundationTestTS, stepwise = T, approximation = F, trace = T)
    foundationTrainETS = ets(foundationTrainTS)
    foundationTestETS = ets(foundationTestTS)
    foundationTrainHolt = holt(foundationTrainTS, h = input$foundationYearsPredicted)
    foundationTestHolt = holt(foundationTestTS, h = input$foundationYearsPredicted)
    foundationTrainHoltDamped = holt(foundationTrainTS, h = input$foundationYearsPredicted, damped = T)
    foundationTestHoltDamped = holt(foundationTestTS, h = input$foundationYearsPredicted, damped = T)
    
    foundationArima = auto.arima(foundationTS, stepwise = T, approximation = F, trace = T)
    foundationETS = ets(foundationTS)
    foundationHolt = holt(foundationTS, h = input$foundationYearsPredicted)
    foundationHoltDamped = holt(foundationTS, h = input$foundationYearsPredicted, damped = T)
    
    rmseFoundArima <- as.data.frame(accuracy(foundationTrainArima))[2]
    rmseFoundETS <- as.data.frame(accuracy(foundationTrainETS))[2]
    rmseFoundHolt <- as.data.frame(accuracy(foundationTrainHolt))[2]
    rmseFoundHoltDamped <- as.data.frame(accuracy(foundationTrainHoltDamped))[2]
    
    
    if(rmseFoundArima <= rmseFoundETS && rmseFoundArima <= rmseFoundHolt && rmseFoundArima <= rmseFoundHoltDamped){
      chosenFoundModel <- foundationArima
      modelChoice = "ARIMA"
      plotFound = forecast(chosenFoundModel, h = input$foundationYearsPredicted)
      
    }else if(rmseFoundETS < rmseFoundArima && rmseFoundETS <= rmseFoundHolt && rmseFoundETS <= rmseFoundHoltDamped){
      chosenFoundModel <- foundationETS
      modelChoice = "ETS"
      plotFound = forecast(chosenFoundModel, h = input$foundationYearsPredicted)
      
    } else if(rmseFoundHolt < rmseFoundETS && rmseFoundHolt < rmseFoundArima && rmseFoundHolt <= rmseFoundHoltDamped){
      chosenFoundModel <- foundationHolt
      modelChoice = "Holt"
      plotFound = forecast(chosenFoundModel)
      
    } else if(rmseFoundHoltDamped < rmseFoundETS && rmseFoundHoltDamped < rmseFoundHolt && rmseFoundHoltDamped < rmseFoundArima){
      chosenFoundModel <- foundationHoltDamped
      modelChoice = "Holt Damped"
      plotFound = forecast(chosenFoundModel)
      
    }
    
    
    output$foundationModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    
    output$foundationTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseFoundArima, 3), "\nETS RMSE: ", round(rmseFoundETS, 3),"\nHolt RMSE: ", round(rmseFoundHolt, 3),"\nHolt Damped RMSE: ", round(rmseFoundHoltDamped, 3))
    )
    
    roundedFound <- round(plotFound$mean, 0)
    predictedFoundValues <- as.data.frame(c(foundationTS, roundedFound))
    foundYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
    predicFound <- append(foundationDegreeData$year, foundYear[1:input$foundationYearsPredicted])
    
    foundTable <- cbind(predicFound, predictedFoundValues)
    names(foundTable)[1] <- "Academic Year Beginning"
    names(foundTable)[2] <- "No. Of Students"
    pFoundValues <- c(foundationTS, roundedFound)
    
    differenceFoundV <- pFoundValues[16+input$foundationYearsPredicted]- pFoundValues[16]
    percentageFoundV <- round((differenceFoundV/pFoundValues[16])*100, 2)
    
    #output$foundationTable <- shiny::renderDataTable(datatable(foundTable, list(paging=F), rownames = F))
    output$foundationTable <- renderTable(
      
      foundTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    output$foundationPlot <- renderPlot ({
      autoplot(plotFound) + 
        labs(title = "Number of Students Studying Foundation Degrees", 
             caption = "Source: Higher Education Statistics Agency")+
        xlab("Academic Year Beginning") + ylab("No. Of Students") + theme(text = element_text(size=15))
    })
    
    if(pFoundValues[16+input$foundationYearsPredicted] > pFoundValues[16]){
      output$foundationPercent <- renderText(
        paste("Forecasted Increase of ", percentageFoundV , "%")
      )
    } else if (pFoundValues[16+input$foundationYearsPredicted] < pFoundValues[16]){
      output$foundationPercent <- renderText(
        paste("Forecasted Decrease of ", percentageFoundV , "%")
      )
    }else{
      output$foundationPercent <- renderText(
        paste("Forecasted Change of 0%")
      )
    }
    
  })
  #End of Foundation Tab
  ##End of SubjectAreaTab
  
  ##CharacteristicsTab
  
  #Gender Information Tab
  observe({
    
    genderTS = ts(genderData[(as.numeric(input$genderDataSelect))], start = 2000)
    genderTrainTS = window(genderTS, start = 2000, end = 2014)
    genderTestTS = window(genderTS, start = 2015)
    
    genderTrainArima = auto.arima(genderTrainTS, stepwise = T, approximation = F, trace = T)
    genderTestArima = auto.arima(genderTestTS, stepwise = T, approximation = F, trace = T)
    genderTrainETS = ets(genderTrainTS)
    genderTestETS = ets(genderTestTS)
    genderTrainHolt = holt(genderTrainTS, h = input$genderYearsPredicted)
    genderTestHolt = holt(genderTestTS, h = input$genderYearsPredicted)
    genderTrainHoltDamped = holt(genderTrainTS, h = input$genderYearsPredicted, damped = T)
    genderTestHoltDamped = holt(genderTestTS, h = input$genderYearsPredicted, damped = T)
    
    genderArima = auto.arima(genderTS, stepwise = T, approximation = F, trace = T)
    genderETS = ets(genderTS)
    genderHolt = holt(genderTS, h = input$genderYearsPredicted)
    genderHoltDamped = holt(genderTS, h = input$genderYearsPredicted, damped = T)
    
    otherGenderTS = window(genderTS, start = 2014)
    otherGenderArima = auto.arima(otherGenderTS, stepwise = T, approximation = F, trace = T)
    otherGenderETS = ets(otherGenderTS)
    otherGenderHolt = holt(otherGenderTS, h = input$genderYearsPredicted)
    otherGenderHoltDamped = holt(otherGenderTS, h = input$genderYearsPredicted, damped = T)
    
    if(input$genderDataSelect == 4){
      rmseGenArima <- as.data.frame(accuracy(otherGenderArima))[2]
      rmseGenETS <- as.data.frame(accuracy(otherGenderETS))[2]
      rmseGenHolt <- as.data.frame(accuracy(otherGenderHolt))[2]
      rmseGenHoltDamped <- as.data.frame(accuracy(otherGenderHoltDamped))[2]
    }
    else{
      rmseGenArima <- as.data.frame(accuracy(genderTrainArima))[2]
      rmseGenETS <- as.data.frame(accuracy(genderTrainETS))[2]
      rmseGenHolt <- as.data.frame(accuracy(genderTrainHolt))[2]
      rmseGenHoltDamped <- as.data.frame(accuracy(genderTrainHoltDamped))[2]
    }
    
    #  plotGen = forecast(genderArima, h= input$genderYearsPredicted)
    
    
    if(rmseGenArima < rmseGenETS && rmseGenArima < rmseGenHolt && rmseGenArima < rmseGenHoltDamped){
      if(input$genderDataSelect == 4){
        chosenGenModel <- otherGenderArima
      } else{chosenGenModel <- genderArima}
      
      modelChoice = "ARIMA"
      plotGen = forecast(chosenGenModel, h = input$genderYearsPredicted)
      
    }else if(rmseGenETS < rmseGenArima && rmseGenETS < rmseGenHolt && rmseGenETS < rmseGenHoltDamped){
      if(input$genderDataSelect == 4){
        chosenGenModel <- otherGenderETS
      } else{chosenGenModel <- genderETS}
      
      modelChoice = "ETS"
      plotGen = forecast(chosenGenModel, h = input$genderYearsPredicted)
      
    } else if(rmseGenHolt < rmseGenETS && rmseGenHolt < rmseGenArima && rmseGenHolt <= rmseGenHoltDamped){
      if(input$genderDataSelect == 4){
        chosenGenModel <- otherGenderHolt
      } else{chosenGenModel <- genderHolt}
      
      modelChoice = "Holt"
      plotGen = forecast(chosenGenModel)
      
    } else if(rmseGenHoltDamped < rmseGenETS && rmseGenHoltDamped < rmseGenHolt && rmseGenHoltDamped < rmseGenArima){
      if(input$genderDataSelect == 4){
        chosenGenModel <- otherGenderHoltDamped
      } else{chosenGenModel <- genderHoltDamped}
      
      modelChoice = "Holt Damped"
      plotGen = forecast(chosenGenModel)
      
    }
    
    
    output$genderModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    
    output$genderTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseGenArima, 3), "\nETS RMSE: ", round(rmseGenETS, 3),"\nHolt RMSE: ", round(rmseGenHolt, 3),"\nHolt Damped RMSE: ", round(rmseGenHoltDamped, 3))
    )
    
    roundedGen <- round(plotGen$mean, 0)
    
    predictedGenValues <- as.data.frame(c(genderTS, roundedGen))
    genYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
    predicGen <- append(genderData$Year, genYear[1:input$genderYearsPredicted])
    
    genTable <- cbind(predicGen, predictedGenValues)
    names(genTable)[1] <- "Academic Year Beginning"
    names(genTable)[2] <- "No. Of Students"
    pGenValues <- c(genderTS, roundedGen)
    
    differenceGenV <- pGenValues[19+input$genderYearsPredicted]- pGenValues[19]
    percentageGenV <- round((differenceGenV/pGenValues[19])*100, 2)
    
    # output$genderTable <- shiny::renderDataTable(datatable(genTable, list(paging=F), rownames = F))
    output$genderTable <- renderTable(
      
      genTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    
    output$genderPlot <- renderPlot ({
      autoplot(plotGen) +
        labs(title = "Number of Students of each Gender", 
             caption = "Source: Higher Education Statistics Agency")+
        xlab("Academic Year Beginning") + ylab("No. Of Students") + theme(text = element_text(size=15))
    })
    
    if(pGenValues[19+input$genderYearsPredicted] > pGenValues[19]){
      output$genderPercent <- renderText(
        paste("Forecasted Increase of ", percentageGenV , "%")
      )
      
    } else if (pGenValues[19+input$genderYearsPredicted] < pGenValues[19]){
      output$genderPercent <- renderText(
        paste("Forecasted Decrease of ", percentageGenV , "%")
      )
    }else{
      output$genderPercent <- renderText(
        paste("Forecasted Change of 0%")
      )
    }
    
    # output$export = downloadHandler(
    #   filename = function(){"gender.png"},
    #   content = function(file) {
    #     png(file)
    #     genderTable
    #     #grid.arrange(output$genderPlot,output$genderTable)
    #     dev.off()
    #   }
    # )
    
    
  })
  #End of Gender Tab
  
  #Age Information Tab
  observe({
    
    ageTS = ts(ageData[(as.numeric(input$ageDataSelect))], start=2009)
    ageTSTrain = window(ageTS, start = 2009, end = 2016)
    ageTSTest = window(ageTS, start = 2017)
    
    ageTrainArima = auto.arima(ageTSTrain, stepwise = T, approximation = F, trace = T)
    ageTestArima = auto.arima(ageTSTest, stepwise = T, approximation = F, trace = T)
    ageTrainETS = ets(ageTSTrain)
    ageTestETS = ets(ageTSTest)
    ageTrainHolt = holt(ageTSTrain, h = input$ageYearsPredicted)
    ageTestHolt = holt(ageTSTest, h = input$ageYearsPredicted)
    ageTrainHoltDamped = holt(ageTSTrain, h = input$ageYearsPredicted, damped = T)
    ageTestHoltDamped = holt(ageTSTest, h = input$ageYearsPredicted, damped = T)
    
    agePlotArima = auto.arima(ageTS, stepwise = T, approximation = F, trace = T)
    ageETS = ets(ageTS)
    ageHolt = holt(ageTS, h = input$ageYearsPredicted)
    ageHoltDamped = holt(ageTS, h = input$ageYearsPredicted, damped = T)
    
    #rmse <- function(error) {sqrt(mean(error^2))}
    
    rmseAgeArima <- as.data.frame(accuracy(ageTrainArima))[2]
    rmseAgeETS <- as.data.frame(accuracy(ageTrainETS))[2]
    rmseAgeHolt <- as.data.frame(accuracy(ageTrainHolt))[2]
    rmseAgeHoltDamped <- as.data.frame(accuracy(ageTrainHoltDamped))[2]
    
    
    if(rmseAgeArima < rmseAgeETS && rmseAgeArima < rmseAgeHolt && rmseAgeArima < rmseAgeHoltDamped){
      chosenModel <- agePlotArima
      modelChoice <- "ARIMA"
      plotAge = forecast(chosenModel, h=input$ageYearsPredicted)
      
      
    } else if (rmseAgeETS < rmseAgeArima && rmseAgeETS < rmseAgeHolt && rmseAgeETS < rmseAgeHoltDamped){
      chosenModel <- ageETS
      modelChoice <- "ETS"
      plotAge = forecast(chosenModel, h=input$ageYearsPredicted)
      
      
    } else if(rmseAgeHolt < rmseAgeArima && rmseAgeHolt < rmseAgeETS && rmseAgeHolt <= rmseAgeHoltDamped){
      chosenModel <- ageHolt
      modelChoice <- "Holt"
      plotAge = forecast(chosenModel)
      
      
    } else if(rmseAgeHoltDamped < rmseAgeArima && rmseAgeHoltDamped < rmseAgeETS && rmseAgeHoltDamped < rmseAgeHolt){
      chosenModel <- ageHoltDamped
      modelChoice <- "Holt Damped"
      plotAge = forecast(chosenModel)
      
    }
    
    output$ageModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    output$ageTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseAgeArima, 3), "\nETS RMSE: ", round(rmseAgeETS, 3), "\nHolt RMSE: ", round(rmseAgeHolt, 3), "\nHolt Damped RMSE: ", round(rmseAgeHoltDamped, 3))
    )
    
    roundedAgeY <- round(plotAge$mean, 0)
    
    predictAgeValues <- as.data.frame(c(ageTS, roundedAgeY))
    ageYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
    predicAge <- append(ageData$Year, ageYear[1:input$ageYearsPredicted])
    ageTable <- cbind(predicAge, predictAgeValues)
    names(ageTable)[1] <-"Academic Year Beginning"
    names(ageTable)[2] <-"No. Of Students"
    pAgeValues <- c(ageTS, roundedAgeY)
    
    differenceAgeV <- pAgeValues[10+input$ageYearsPredicted]-pAgeValues[10]
    percentageAgeV <- round((differenceAgeV/pAgeValues[10])*100, 2)
    
    
    # output$ageModelTable <- shiny::renderDataTable(datatable(ageTable, list(paging=F), rownames = F))
    output$ageModelTable <- renderTable(
      
      ageTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    output$ageModelGraph <- renderPlot(
      autoplot(plotAge)+
        labs(title = "Number of Students in each Age Category", 
             caption = "Source: Higher Education Statistics Agency")+
        xlab("Academic Year Beginning")+ ylab("No. of Students") + theme(text = element_text(size=15))
    )
    
    if(pAgeValues[10+input$ageYearsPredicted] > pAgeValues[10]){
      
      output$agePercent <- renderText(
        paste("Forecasted Increase of ", percentageAgeV , "%")
      )
    }else if (pAgeValues[10+input$ageYearsPredicted] < pAgeValues[10]){
      
      output$agePercent <- renderText(
        paste("Forecasted Decrease of ", percentageAgeV , "%")
      )
    } else {
      output$agePercent <- renderText(
        paste("Forecasted Change of 0%")
      )
      
    }
    
    
    
  })
  #End of Age Tab
  
  #Ethnicity Information Tab
  observe({
    
    ethnicTS = ts(ethnicData[(as.numeric(input$ethnicDataSelect))], start = 2005)
    ethnicTrainTS = window(ethnicTS, start = 2005, end = 2015)
    ethnicTestTS = window(ethnicTS, start = 2016)
    
    ethnicTrainArima = auto.arima(ethnicTrainTS, stepwise = T, approximation = F, trace = T)
    ethnicTestArima = auto.arima(ethnicTestTS, stepwise = T, approximation = F, trace = T)
    ethnicTrainETS = ets(ethnicTrainTS)
    ethnicTestETS = ets(ethnicTestTS)
    ethnicTrainHolt = holt(ethnicTrainTS, h = input$ethYearsPredicted)
    ethnicTestHolt = holt(ethnicTestTS, h = input$ethYearsPredicted)
    ethnicTrainHoltDamped = holt(ethnicTrainTS, h = input$ethYearsPredicted, damped = T)
    ethnicTestHoltDamped = holt(ethnicTestTS, h = input$ethYearsPredicted, damped = T)
    
    ethnicArima = auto.arima(ethnicTS, stepwise = T, approximation = F, trace = T)
    ethnicETS = ets(ethnicTS)
    ethnicHolt = holt(ethnicTS, h = input$ethYearsPredicted)
    ethnicHoltDamped = holt(ethnicTS, h = input$ethYearsPredicted, damped = T)
    
    rmseEthArima <- as.data.frame(accuracy(ethnicTrainArima))[2]
    rmseEthETS <- as.data.frame(accuracy(ethnicTrainETS))[2]
    rmseEthHolt <- as.data.frame(accuracy(ethnicTrainHolt))[2]
    rmseEthHoltDamped <- as.data.frame(accuracy(ethnicTrainHoltDamped))[2]
    
    
    if(rmseEthArima < rmseEthETS && rmseEthArima < rmseEthHolt && rmseEthArima < rmseEthHoltDamped){
      chosenEthModel <- ethnicArima
      modelChoice = "ARIMA"
      plotEth = forecast(chosenEthModel, h = input$ethYearsPredicted)
      
    }else if(rmseEthETS < rmseEthArima && rmseEthETS < rmseEthHolt && rmseEthETS < rmseEthHoltDamped){
      chosenEthModel <- ethnicETS
      modelChoice = "ETS"
      plotEth = forecast(chosenEthModel, h = input$ethYearsPredicted)
      
    } else if(rmseEthHolt < rmseEthETS && rmseEthHolt < rmseEthArima && rmseEthHolt < rmseEthHoltDamped){
      chosenEthModel <- ethnicHolt
      modelChoice = "Holt"
      plotEth = forecast(chosenEthModel)
      
    } else if(rmseEthHoltDamped < rmseEthETS && rmseEthHoltDamped < rmseEthHolt && rmseEthHoltDamped < rmseEthArima){
      chosenEthModel <- ethnicHoltDamped
      modelChoice = "Holt Damped"
      plotEth = forecast(chosenEthModel)
      
    }
    
    
    plotEthnicArima = forecast(ethnicTrainArima, h=input$ethYearsPredicted)
    
    output$ethnicModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    
    output$ethnicTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseEthArima, 3), "\nETS RMSE: ", round(rmseEthETS, 3),"\nHolt RMSE: ", round(rmseEthHolt, 3),"\nHolt Damped RMSE: ", round(rmseEthHoltDamped, 3))
    )
    
    roundedEth <- round(plotEth$mean, 0)
    predictedEthValues <- as.data.frame(c(ethnicTS, roundedEth))
    ethYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
    predicEth <- append(ethnicData$year, ethYear[1:input$ethYearsPredicted])
    
    ethTable <- cbind(predicEth, predictedEthValues)
    names(ethTable)[1] <- "Academic Year Beginning"
    names(ethTable)[2] <- "No. Of Students"
    pEthValues <- c(ethnicTS, roundedEth)
    
    differenceEthV <- pEthValues[14+input$ethYearsPredicted]- pEthValues[14]
    percentageEthV <- round((differenceEthV/pEthValues[14])*100, 2)
    
    output$ethnicTable <- renderTable(
      ethTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    #output$ethnicTable <- shiny::renderDataTable(datatable(ethTable, list(paging=F), rownames = F))
    
    
    
    output$ethnicPlot <- renderPlot ({
      autoplot(plotEth) +
        labs(title = "Number of Students of each Ethnicity", 
             caption = "Source: Higher Education Statistics Agency")+
        xlab("Academic Year Beginning") + ylab("No. Of Students") + theme(text = element_text(size=15))
    })
    
    if(pEthValues[14+input$ethYearsPredicted] > pEthValues[14]){
      output$ethnicPercent <- renderText(
        paste("Forecasted Increase of ", percentageEthV , "%")
      )
    } else if (pEthValues[14+input$ethYearsPredicted] < pEthValues[14]){
      output$ethnicPercent <- renderText(
        paste("Forecasted Decrease of ", percentageEthV , "%")
      )
    }else{
      output$ethnicPercent <- renderText(
        paste("Forecasted Change of 0%")
      )
    }
    
  })
  # End of Ethnicity Tab
  
  #Disability Information Tab
  observe({
    
    disTS = ts(disabilityData[(as.numeric(input$disDataSelect))], start = 2005)
    disTrainTS = window(disTS, start = 2005, end = 2015)
    disTestTS = window(disTS, start = 2016)
    
    disTrainArima = auto.arima(disTrainTS, stepwise = T, approximation = F, trace = T)
    disTestArima = auto.arima(disTestTS, stepwise = T, approximation = F, trace = T)
    disTrainETS = ets(disTrainTS)
    disTestETS = ets(disTestTS)
    disTrainHolt = holt(disTrainTS, h = input$disYearsPredicted)
    disTestHolt = holt(disTestTS, h = input$disYearsPredicted)
    disTrainHoltDamped = holt(disTrainTS, h = input$disYearsPredicted, damped = T)
    disTestHoltDamped = holt(disTestTS, h = input$disYearsPredicted, damped = T)
    
    disArima = auto.arima(disTS, stepwise = T, approximation = F, trace = T)
    disETS = ets(disTS)
    disHolt = holt(disTS, h = input$disYearsPredicted)
    disHoltDamped = holt(disTS, h = input$disYearsPredicted, damped = T)
    
    rmseDisArima <- as.data.frame(accuracy(disTrainArima))[2]
    rmseDisETS <- as.data.frame(accuracy(disTrainETS))[2]
    rmseDisHolt <- as.data.frame(accuracy(disTrainHolt))[2]
    rmseDisHoltDamped <- as.data.frame(accuracy(disTrainHoltDamped))[2]
    
    if(rmseDisArima < rmseDisETS && rmseDisArima < rmseDisHolt && rmseDisArima < rmseDisHoltDamped){
      chosenDisModel <- disArima
      modelChoice = "ARIMA"
      plotDis = forecast(chosenDisModel, h = input$disYearsPredicted)
      
    }else if(rmseDisETS < rmseDisArima && rmseDisETS < rmseDisHolt && rmseDisETS < rmseDisHoltDamped){
      chosenDisModel <- disETS
      modelChoice = "ETS"
      plotDis = forecast(chosenDisModel, h = input$disYearsPredicted)
      
    } else if(rmseDisHolt < rmseDisETS && rmseDisHolt < rmseDisArima && rmseDisHolt < rmseDisHoltDamped){
      chosenDisModel <- disHolt
      modelChoice = "Holt"
      plotDis = forecast(chosenDisModel)
      
    } else if(rmseDisHoltDamped < rmseDisETS && rmseDisHoltDamped < rmseDisHolt && rmseDisHoltDamped < rmseDisArima){
      chosenDisModel <- disHoltDamped
      modelChoice = "Holt Damped"
      plotDis = forecast(chosenDisModel)
      
    }
    
    # modelChoice = "ARIMA"
    # plotDisArima = forecast(disArima, h=input$disYearsPredicted)
    
    output$disModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    
    output$disTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseDisArima, 3), "\nETS RMSE: ", round(rmseDisETS, 3),"\nHolt RMSE: ", round(rmseDisHolt, 3),"\nHolt Damped RMSE: ", round(rmseDisHoltDamped, 3))
    )
    
    roundedDis <- round(plotDis$mean, 0)
    predictedDisValues <- as.data.frame(c(disTS, roundedDis))
    disYear <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
    predicDis <- append(disabilityData$year, disYear[1:input$disYearsPredicted])
    
    disTable <- cbind(predicDis, predictedDisValues)
    names(disTable)[1] <- "Academic Year Beginning"
    names(disTable)[2] <- "No. Of Students"
    pDisValues <- c(disTS, roundedDis)
    
    differenceDisV <- pDisValues[14+input$disYearsPredicted]- pDisValues[14]
    percentageDisV <- round((differenceDisV/pDisValues[14])*100, 2)
    
    output$disabilityTable <- renderTable(
      disTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    #output$disabilityTable <- shiny::renderDataTable(datatable(disTable, list(paging=F), rownames = F))
    
    output$disPlot <- renderPlot ({
      autoplot(plotDis) +
        labs(title = "Number of Students with or without a disability", 
             caption = "Source: Higher Education Statistics Agency")+
        xlab("Academic Year Beginning") + ylab("No. Of Students") + theme(text = element_text(size=15))
    })
    
    if(pDisValues[14+input$disYearsPredicted] > pDisValues[14]){
      output$disPercent <- renderText(
        paste("Forecasted Increase of ", percentageDisV , "%")
      )
    } else if (pDisValues[14+input$disYearsPredicted] < pDisValues[14]){
      output$disPercent <- renderText(
        paste("Forecasted Decrease of ", percentageDisV , "%")
      )
    }else{
      output$disPercent <- renderText(
        paste("Forecasted Decrease of 0%")
      )
    }
    
  })
  # End of Disability Tab
  
  ##End of CharacteristicsTab
  
  ##Job Vacancies Tab
  observe({
    jobTS = ts(jobData[(as.numeric(input$jobDataSelect))], start = 2001)
    jobTrainTS = window(jobTS, start = 2001, end = 2015)
    jobTestTS = window(jobTS, start = 2016)
    
    jobTrainArima = auto.arima(jobTrainTS, stepwise = T, approximation = F, trace = T)
    jobTestArima = auto.arima(jobTestTS, stepwise = T, approximation = F, trace = T)
    jobTrainETS = ets(jobTrainTS)
    jobTestETS = ets(jobTestTS)
    jobTrainHolt = holt(jobTrainTS, h = input$jobYearsPredicted)
    jobTestHolt = holt(jobTestTS, h = input$jobYearsPredicted)
    jobTrainHoltDamped = holt(jobTrainTS, h = input$jobYearsPredicted, damped = T)
    jobTestHoltDamped = holt(jobTestTS, h = input$jobYearsPredicted, damped = T)
    
    jobArima = auto.arima(jobTS, stepwise = T, approximation = F, trace = T)
    jobETS = ets(jobTS)
    jobHolt = holt(jobTS, h = input$jobYearsPredicted)
    jobHoltDamped = holt(jobTS, h = input$jobYearsPredicted, damped = T)
    
    rmseJobArima <- as.data.frame(accuracy(jobTrainArima))[2]
    rmseJobETS <- as.data.frame(accuracy(jobTrainETS))[2]
    rmseJobHolt <- as.data.frame(accuracy(jobTrainHolt))[2]
    rmseJobHoltDamped <- as.data.frame(accuracy(jobTrainHoltDamped))[2]
    
    
    if(rmseJobArima < rmseJobETS && rmseJobArima < rmseJobHolt && rmseJobArima < rmseJobHoltDamped){
      chosenJobModel <- jobArima
      modelChoice = "ARIMA"
      plotJob = forecast(chosenJobModel, h = input$jobYearsPredicted)
      
    }else if(rmseJobETS < rmseJobArima && rmseJobETS < rmseJobHolt && rmseJobETS < rmseJobHoltDamped){
      chosenJobModel <- jobETS
      modelChoice = "ETS"
      plotJob = forecast(chosenJobModel, h = input$jobYearsPredicted)
      
    } else if(rmseJobHolt < rmseJobETS && rmseJobHolt < rmseJobArima && rmseJobHolt < rmseJobHoltDamped){
      chosenJobModel <- jobHolt
      modelChoice = "Holt"
      plotJob = forecast(chosenJobModel)
      
    } else if(rmseJobHoltDamped < rmseJobETS && rmseJobHoltDamped < rmseJobHolt && rmseJobHoltDamped < rmseJobArima){
      chosenJobModel <- jobHoltDamped
      modelChoice = "Holt Damped"
      plotJob = forecast(chosenJobModel)
      
    }
    
    output$jobModel <- renderValueBox(
      valueBox("Forecast Model:", modelChoice)
    )
    
    output$jobTextOut <-renderText(
      paste("ARIMA RMSE: ", round(rmseJobArima, 3), "\nETS RMSE: ", round(rmseJobETS, 3),"\nHolt RMSE: ", round(rmseJobHolt, 3),"\nHolt Damped RMSE: ", round(rmseJobHoltDamped, 3))
    )
    
    roundedJob <- round(plotJob$mean, 0)
    predictedJobValues <- as.data.frame(c(jobTS, roundedJob))
    jobYear <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033)
    predicJob <- append(jobData$year, jobYear[1:input$jobYearsPredicted])
    
    jobTable <- cbind(predicJob, predictedJobValues)
    names(jobTable)[1] <- "Year"
    names(jobTable)[2] <- "No. Of Students (Thousands)"
    pJobValues <- c(jobTS, roundedJob)
    
    differenceJobV <- pJobValues[19+input$jobYearsPredicted]- pJobValues[19]
    percentageJobV <- round((differenceJobV/pJobValues[19])*100, 2)
    
    output$jobVacTable <- renderTable(
      jobTable, striped = TRUE, bordered = TRUE, hover = TRUE, responsive = TRUE
    )
    
    # output$jobVacTable <- shiny::renderDataTable(datatable(jobTable, list(paging=F), rownames = F))
    
    output$jobPlot <- renderPlot ({
      autoplot(plotJob) +
        labs(title = "Number of Jobs Available per Job Category relevant to University Qualifications", 
             caption = "Source: Office for National Statistics - Vacancy Survey")+
        xlab("Year") + ylab("No. Of Students (Thousands)") + theme(text = element_text(size=15))
    })
    
    
    if(pJobValues[19+input$jobYearsPredicted] > pJobValues[19]){
      output$jobPercent <- renderText(
        paste("Forecasted Increase of ", percentageJobV , "%")
      )
    } else if (pJobValues[19+input$ethYearsPredicted] < pJobValues[19]){
      output$jobPercent <- renderText(
        paste("Forecasted Decrease of ", percentageJobV , "%")
      )
    }else{
      output$jobPercent <- renderText(
        paste("Forecasted Change of 0%")
      )
    }
    
  })
  ##End of Job Tab
  
  # observeEvent(input$studButton, {
  #   updateTabsetPanel(session, "navbarId", selected = "tabStud")
  #  # updateTabsetPanel(session, "tabHome", selected = "genderInfo")
  # })
  
  
  
  # session$onSessionEnded (function(){
  #   odbcClose(con)
  # })
}

##UI CODE
ui = navbarPage(theme = shinytheme("cerulean"), title = ("Predicting Education Trends"), footer = includeHTML("footer.html"), id="navbarId",
                header = tags$style(
                  ".navbar-right {
                       float: right !important;
                       }",
                ),
                
                tabPanel("Home", id="tabHome",
                         fluidRow(
                           div(
                             tags$img(src="banner2.jpg", class="img-responsive"),
                             div(
                               h1("Predicting Education Trends", style="text-align:center")
                               
                             ) ),
                           
                           style = "height:responsive"),
                         
                         
                         # PAGE BREAK
                         tags$hr(),
                         
                         fluidRow(
                           column(2),
                           column(8,
                                  div(
                                    
                                    shiny::HTML("<br><br><center><h2>How will Education be affected in the Future?</h2></center><br>"),
                                    
                                    div(align = "center",
                                        tags$img(src="education.jpg", height="200px")
                                    ),
                                    shiny::HTML("<br><h4 style=color:black><center>Will there be an change in the numbers of students studying via traditional methods
                                     of University Degrees?<br>
                                     Will there be a change in the mode of study when studying for a Degree?<br>
                                     Will there be any trend changes in which Subject Areas are more favourable to Students?
                                     <br><br><p style=font-size:14px>See the predictions in the Subject Data Tab</p><br></center></h4>")
                                    
                                    # div(align = "center",
                                    #     tags$button(href="Gender Information", "Gender")
                                    #     )
                                  )
                           ),
                           column(2)
                         ),
                         
                         fluidRow(
                           
                           style = "height:50px;"),
                         
                         # PAGE BREAK
                         tags$hr(),
                         
                         # HOW
                         fluidRow(
                           column(3),
                           column(6,
                                  shiny::HTML("<br><br><center><h1>How will Students Differ in the Future?</h1></center><br>"),
                                  div(align = "center",
                                      tags$img(src="students.jpg", height="200px")
                                  ),
                                  shiny::HTML("<br><h4 style=color:black><center>Will there be a change in characteristics of Students studying University Degrees?
                                     <br><br><p style=font-size:14px>See the predictions in the Student Characteristics Tab</p><br></center></h4>"),
                                  
                           ),
                           column(3)
                         ),
                         
                         fluidRow(
                           
                           style = "height:50px;"),
                         
                         # PAGE BREAK
                         tags$hr(),
                         
                         # WHERE
                         fluidRow(
                           column(3),
                           column(6,
                                  shiny::HTML("<br><br><center><h1>Will there be any changes in Job Vacancy Trends?</h1></center><br>"),
                                  div(align = "center",
                                      tags$img(src="jobs.jpg", height="200px")
                                  ),
                                  shiny::HTML("<br><h4 style=color:black><center>Will there be a change in the numbers of Jobs available in the future?<br>
                         Will there be any change in Subject Areas with the most Jobs?
                                     <br><br><p style=font-size:14px>See the predictions in the Job Vacancies Tab</p></center></h4>"),
                                  
                                  shiny::HTML("<br><center> <h2>Current Job Search</h2></center>
                                     <center><h5>Source:</h5><img src= reed.png height=50px width = 100px/></center><br>"),
                                  div(align = "center",
                                      searchInput(
                                        inputId = "keyword",
                                        placeholder = "Search Jobs",
                                        btnSearch = icon("search"),
                                        btnReset = icon("remove"),
                                        width = "450px"
                                      )
                                  )
                                  
                           ),
                           column(3)
                         ),
                         conditionalPanel(condition="parse_content.totalResults>0",
                                          uiOutput("oneJob")
                         ),
                         uiOutput("numberResults"),
                         
                         fluidRow(
                           
                           style = "height:50px;"),
                         
                         
                ),
                
                tabPanel("Student Characteristics", id= "tabStud",
                         navbarPage(theme = shinytheme("cerulean"), title = ("Student Characteristics"), id="navbarstud",
                                    tabPanel("Gender Information", id = "genderInfo",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          h1("Forecasting Gender Information"),
                                                          
                                                          selectInput("genderDataSelect", "Select Gender", c("No. of Female Students" = 2, "No. of Male Students" = 3, "No. of Other Gender Students" = 4, "Total No. of Students" = 5)),
                                                          
                                                          sliderInput(inputId="genderYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=2, min=1, max=10),
                                                          
                                                          
                                                          plotOutput("genderPlot"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("genderPercent")
                                                          ),
                                                          tags$br(),
                                                          #downloadButton(outputId = "export", "Export Graph to PDF"),
                                                          tableOutput("genderTable")
                                                          
                                                          #valueBoxOutput("genderModel"),
                                                          #verbatimTextOutput("genderTextOut")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    ),
                                    
                                    tabPanel("Age Information", id = "tabAge",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,   
                                                          h1("Forecasting Age Information"),
                                                          selectInput("ageDataSelect", "Select Age Range", c("20 Years and Under" = 2, "21 - 24 Years" = 3, "25 - 29 Years" = 4, "30+ Years" = 5, "Unknown Age" = 6)),
                                                          
                                                          
                                                          sliderInput(inputId="ageYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=2, min=1, max=8),
                                                          
                                                          plotOutput("ageModelGraph"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("agePercent")
                                                          ),
                                                          tags$br(),
                                                          tableOutput("ageModelTable")
                                                          #valueBoxOutput("agePercent"),
                                                          #valueBoxOutput("ageModel"),
                                                          #verbatimTextOutput("ageTextOut")
                                                          
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                             
                                    ),
                                    tabPanel("Ethnicity Information", 
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          h1("Forecasting Ethnicity Information"),
                                                          selectInput("ethnicDataSelect", "Select Ethnicity", c("White" = 2, "Black" = 3, "Asian" = 4, "Other Race Inc. Mixed Race" = 5, "Unknown Ethnicity" = 6, "Total UK Domicile" = 7, "Total Non-UK Domicile" = 8)),
                                                          sliderInput(inputId="ethYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=2, min=1, max=8),
                                                          plotOutput("ethnicPlot"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("ethnicPercent")
                                                          ),
                                                          tags$br(),
                                                          tableOutput("ethnicTable")
                                                          #valueBoxOutput("ethnicPercent"),
                                                          #valueBoxOutput("ethnicModel"),
                                                          #verbatimTextOutput("ethnicTextOut")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    ),
                                    tabPanel("Disability Information",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          h1("Forecasting Disability Information"),
                                                          selectInput("disDataSelect", "Select Ability Category", c("Disabled" = 2, "Not Disabled" = 3)),
                                                          sliderInput(inputId="disYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=2, min=1, max=8),
                                                          plotOutput("disPlot"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("disPercent")
                                                          ),
                                                          tags$br(),
                                                          tableOutput("disabilityTable")
                                                          # valueBoxOutput("disPercent"),
                                                          #valueBoxOutput("disModel"),
                                                          #verbatimTextOutput("disTextOut")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    )
                                    
                         )
                ),
                
                tabPanel("Subject Data", id = "tabSub",
                         navbarPage(theme = shinytheme("cerulean"), title = ("Subject Information"), id="navbarsub",
                                    
                                    tabPanel("Overview",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          h1("Subject Statistics"),
                                                          h2("Science Degree Subjects"),
                                                          plotOutput("scienceSubAreaGraph"),
                                                          tags$hr(),
                                                          h2("Non-Science Degree Subjects"),
                                                          plotOutput("nonScienceSubAreaGraph"),
                                                          tags$hr(),
                                                          h1("Foundation Degree Subjects"),
                                                          plotOutput("foundationGraph"),
                                                          tags$hr(),
                                                          h1("Open University Subjects"),
                                                          plotOutput("openUniGraph")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    ),
                                    
                                    tabPanel("Degrees",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          h1("Forecasting Subject Area Information"),
                                                          helpText("Select which Student Data you would like to View:"),
                                                          selectInput("country", "Select UK Country", c("All", "England", "Wales", "Scotland", "Northern Ireland")),
                                                          selectInput("subArea", "Select Subject Area", c("Medicine  & Dentistry" = 3, "Subjects Allied to Medicine" = 4, "Biological Sciences" = 5, "Veterinary Sciences" = 6, "Agriculture" = 7, "Physical Sciences" = 8, "Mathematical Sciences" = 9, "Computer Sciences" = 10, "Engineering & Technology" = 11, "Architecture, Building and Planning" = 12, "Total Science Subjects" = 13, "Social Studies" = 14, "Law" = 15, "Business & Administrative Studies" = 16, "Mass Communications & Documentation" = 17, "Languages" = 18, "Historical & Philosophical Studies" = 19, "Creative Arts & Design" = 20, "Education" = 21, "Combined" = 22, "Total Non-Science Subjects" = 23)),
                                                          selectInput("mode", "Select Mode of Study", c("Full-Time", "Part-Time", "Both Full-Time and Part-Time")),
                                                          sliderInput(inputId="subYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=3, min=1, max=10),
                                                          plotOutput("subGraph"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("subPercent")
                                                          ),
                                                          tags$br(),
                                                          #valueBoxOutput("subPercent"),
                                                          # valueBoxOutput("subModel"),
                                                          #verbatimTextOutput("textOut"),
                                                          tableOutput("subTable")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    ),
                                    
                                    tabPanel("Open University Degrees",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          selectInput("openDataSelect", "Select Subject Area", c("Subjects Allied to Medicine" = 4, "Biological Sciences" = 5, "Physical Sciences" = 8, "Mathematical Sciences" = 9, "Computer Sciences" = 10, "Engineering & Technology" = 11, "Architecture, Building and Planning" = 12, "Total Science Subjects" = 13, "Social Studies" = 14, "Law" = 15, "Business & Administrative Studies" = 16, "Languages" = 18, "Historical & Philosophical Studies" = 19, "Creative Arts & Design" = 20, "Education" = 21, "Combined" = 22, "Total Non-Science Subjects" = 23)),
                                                          
                                                          sliderInput(inputId="openYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=2, min=1, max=8),
                                                          plotOutput("openPlot"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("openPercent")
                                                          ),
                                                          tags$br(),
                                                          #valueBoxOutput("openPercent"),
                                                          #valueBoxOutput("openModel"),
                                                          # verbatimTextOutput("openTextOut"),
                                                          tableOutput("openUniTable")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    ),
                                    
                                    tabPanel("Foundation Degrees",
                                             div(align="center",
                                                 fluidRow(
                                                   column(2),
                                                   
                                                   column(8,
                                                          selectInput("foundationDataSelect", "Select Subject Area", c("Subjects Allied to Medicine" = 4, "Biological Sciences" = 5, "Agriculture" = 7, "Physical Sciences" = 8, "Computer Sciences" = 10, "Engineering & Technology" = 11, "Architecture, Building and Planning" = 12, "Total Science Subjects" = 13, "Social Studies" = 14, "Law" = 15, "Business & Administrative Studies" = 16, "Mass Communications & Documentation" = 17, "Languages" = 18, "Historical & Philosophical Studies" = 19, "Creative Arts & Design" = 20, "Education" = 21, "Combined" = 22, "Total Non-Science Subjects" = 23)),
                                                          
                                                          sliderInput(inputId="foundationYearsPredicted",
                                                                      label="Number of Years Predicted",
                                                                      value=2, min=1, max=8),
                                                          plotOutput("foundationPlot"),
                                                          tags$br(),
                                                          h4(
                                                            textOutput("foundationPercent")
                                                          ),
                                                          tags$br(),
                                                          #valueBoxOutput("foundationPercent"),
                                                          #valueBoxOutput("foundationModel"),
                                                          #verbatimTextOutput("foundationTextOut"),
                                                          tableOutput("foundationTable")
                                                   ),
                                                   column(2)
                                                 )
                                             )
                                    )
                                    
                                    
                         )
                ),
                tabPanel("Job Vacancies", id = "tabJob",
                         div(align="center",
                             fluidRow(
                               column(2),
                               
                               column(8,
                                      h1("Forecasting Job Vacancy Information"),
                                      selectInput("jobDataSelect", "Select Job Category", c("Education" = 2, "Information and Communication" = 3, "Arts, Entertainment & Recreation" = 4, "Human Health & Social Work" = 5, "Administrative & Supportive Services" = 6, "Finance & Insurance" = 7, "Professional Scientific & Technical Activities" = 8)),
                                      sliderInput(inputId="jobYearsPredicted",
                                                  label="Number of Years Predicted",
                                                  value=2, min=1, max=8),
                                      plotOutput("jobPlot"),
                                      tags$br(),
                                      h4(
                                        textOutput("jobPercent")
                                      ),
                                      tags$br(),
                                      tableOutput("jobVacTable")
                                      # valueBoxOutput("jobPercent"),
                                      # valueBoxOutput("jobModel"),
                                      #verbatimTextOutput("jobTextOut")
                               ),
                               column(2)
                             )
                         )
                )
                
                
                # tabPanel("Past Data",
                #          helpText("Select which Student Data you would like to View:"),
                #          selectInput("data", "Select", choices = names(datasets)),
                #          dataTableOutput("table")
                # 
                #  )
                
                
                
)
shinyApp(ui = ui, server=server)



