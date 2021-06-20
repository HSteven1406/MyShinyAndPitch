#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applicat ions with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(utils)
library(ggplot2)
library(plotly)
library(dplyr)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    SumData <- read.csv2("~/MyShinyAndPitch/RoadAccident/Data/DataShort.csv")
    React <- eventReactive(input$go,{
        if (input$groupby=='Provinces'){
            if ('All Flemish Provinces' %in% input$Selection){
                sel <- c("Antwerp","East Flanders","Flemish Brabant","Limburg","West Flanders")
                Data <- SumData[SumData$Prov %in% sel,]
            }
            else if ("All Walloon Provinces" %in% input$Selection){
                sel <- c("Hainaut","Liege","Luxembourg","Namur","Walloon Brabant")
                Data <- SumData[SumData$Prov %in% sel,]
            }
            else {
                Data <- SumData[SumData$Prov %in% input$Selection,]
            }
            if (input$ycol=="ACCT"){
                Data <- Data %>% group_by(Prov,Region) %>% dplyr::summarise(ACCT=sum(ACCT))
                MyPlot <- ggplot(Data, aes(x=Prov, y= ACCT, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Province',y='Number of accidents')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="DEAD"){
                Data <- Data %>% group_by(Prov,Region) %>% dplyr::summarise(DEAD=sum(DEAD))
                MyPlot <- ggplot(Data, aes(x=Prov, y= DEAD, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with deaths')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="DEAD_30_DAYS"){
                Data <- Data %>% group_by(Prov,Region) %>% dplyr::summarise(DEAD_30_DAYS=sum(DEAD_30_DAYS))
                MyPlot <- ggplot(Data, aes(x=Prov, y= DEAD_30_DAYS, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with deaths within 30 days')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="MORTALLY_INJ"){
                Data <- Data %>% group_by(Prov,Region) %>% dplyr::summarise(MORTALLY_INJ=sum(MORTALLY_INJ))
                MyPlot <- ggplot(Data, aes(x=Prov, y= MORTALLY_INJ, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with mortally injured')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="SERLY_INJ"){
                Data <- Data %>% group_by(Prov,Region) %>% dplyr::summarise(SERLY_INJ=sum(SERLY_INJ))
                MyPlot <- ggplot(Data, aes(x=Prov, y= SERLY_INJ, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with seriously injured')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else {
                Data <- Data %>% group_by(Prov,Region) %>% dplyr::summarise(SLY_INJ=sum(SLY_INJ))
                MyPlot <- ggplot(Data, aes(x=Prov, y= SLY_INJ, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with slightly injured')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
        }
        
        
        else {
            Data <- SumData[SumData$Region %in% input$SelectionReg,]
            if (input$ycol=="ACCT"){
                Data <- Data %>% group_by(Region) %>% dplyr::summarise(ACCT=sum(ACCT))
                MyPlot <- ggplot(Data, aes(x=Region, y= ACCT, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Region',y='Number of accidents')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="DEAD"){
                Data <- Data %>% group_by(Region) %>% dplyr::summarise(DEAD=sum(DEAD))
                MyPlot <- ggplot(Data, aes(x=Region, y= DEAD, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Region',y='Number of accidents with deaths')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="DEAD_30_DAYS"){
                Data <- Data %>% group_by(Region) %>% dplyr::summarise(DEAD_30_DAYS=sum(DEAD_30_DAYS))
                MyPlot <- ggplot(Data, aes(x=Region, y= DEAD_30_DAYS, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Region',y='Number of accidents with deaths within 30 days')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="MORTALLY_INJ"){
                Data <- Data %>% group_by(Region) %>% dplyr::summarise(MORTALLY_INJ=sum(MORTALLY_INJ))
                MyPlot <- ggplot(Data, aes(x=Region, y= MORTALLY_INJ, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Region',y='Number of accidents with mortally injured')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else if (input$ycol=="SERLY_INJ"){
                Data <- Data %>% group_by(Region) %>% dplyr::summarise(SERLY_INJ=sum(SERLY_INJ))
                MyPlot <- ggplot(Data, aes(x=Region, y= SERLY_INJ, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Region',y='Number of accidents with seriously injured')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
            else {
                Data <- Data %>% group_by(Region) %>% dplyr::summarise(SLY_INJ=sum(SLY_INJ))
                MyPlot <- ggplot(Data, aes(x=Region, y= SLY_INJ, fill=Region)) + 
                    geom_bar(stat='identity') + labs(x='Region',y='Number of accidents with slightly injured')+
                    scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                                      values=c('yellow3','indianred2','steelblue'))
            }
        }
        MyPlot
    })
    output$distPlot <- renderPlotly({ggplotly(React())})
})
