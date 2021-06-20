library(shiny)
library(plotly)


SumData <- read.csv2("~/MyShinyAndPitch/RoadAccident/Data/DataShort.csv")

shinyUI(pageWithSidebar(
    headerPanel("Road accidents in 2020 in Belgium"),

    sidebarPanel(
            selectInput('groupby',"Select if you want to show data for provinces or regions.",
                        choices = c('Provinces','Regions'), multiple=F),
            conditionalPanel(
                condition = "input.groupby == 'Provinces'",
                selectInput('Selection','Select your province(s)', 
                            choices =c('Brussels Capital', 'Antwerp','West Flanders','East Flanders',
                                       'Hainaut','Liege','Limburg','Luxembourg','Namur',
                                       'Flemish Brabant','Walloon Brabant', 'All Flemish Provinces','All Walloon Provinces'), multiple=T)),
            conditionalPanel(
                condition = "input.groupby =='Regions'",
                selectInput('SelectionReg','Select your region(s)',
                            choices =unique(SumData$Region), multiple=T)
            ),
            selectInput("ycol", "Select which variables to show",
                        choices = c("ACCT","DEAD", "DEAD_30_DAYS","MORTALLY_INJ","SERLY_INJ","SLY_INJ")),
            actionButton("go","Go")
            ),

    # Show a plot of the generated distribution
    mainPanel(
            plotlyOutput("distPlot"),
        )
    )
)
