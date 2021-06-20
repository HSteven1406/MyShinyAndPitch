options(java.parameters = "-Xmx8000m")
library(xlsx)
library(dplyr, warn.conflicts = F)
library(ggplot2)
library(plotly, warn.conflicts = F)

RData <- read.xlsx2('~/MyShinyAndPitch/Data/Data.xlsx',1, header=T)

Data <- RData[,!grepl('NL|FR',names(RData))]


cols = c("CD_DSTR_REFNIS" , "CD_MUNTY_REFNIS" ,"CD_LIGHT_COND" , "CD_COLL_TYPE" ,"CD_DAY_OF_WEEK",'DT_HOUR', 'DT_DAY')

KeepData <- Data[,!names(Data)%in%cols]
names(KeepData) <- c('AreaType','RoadType','Prov','Region','ACCT','DEAD','DEAD_30_DAYS','MORTALLY_INJ','SERLY_INJ','SLY_INJ')
KeepData$ACCT <- as.integer(KeepData$ACCT)
KeepData$DEAD <- as.integer(KeepData$DEAD)
KeepData$DEAD_30_DAYS <- as.integer(KeepData$DEAD_30_DAYS)
KeepData$MORTALLY_INJ <- as.integer(KeepData$MORTALLY_INJ)
KeepData$SERLY_INJ <- as.integer(KeepData$SERLY_INJ)
KeepData$SLY_INJ <- as.integer(KeepData$SLY_INJ)


KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '10000','Antwerp'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '30000','West Flanders'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '40000','East Flanders'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '50000','Hainaut'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '60000','Liege'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '70000','Limburg'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '80000','Luxembourg'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '90000','Namur'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '20001','Flemish Brabant'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '20002','Walloon Brabant'))
KeepData <- KeepData %>% mutate(Prov=replace(Prov, Prov == '','Brussels Capital'))
KeepData <- KeepData %>% mutate(Region=replace(Region, Region == '02000','Flemish Region'))
KeepData <- KeepData %>% mutate(Region=replace(Region, Region == '03000','Walloon Region'))
KeepData <- KeepData %>% mutate(Region=replace(Region, Region == '04000','Brussels Capital Region'))



SumData <- KeepData %>% group_by(Region,Prov) %>% summarise(ACCT=sum(ACCT), DEAD=sum(DEAD),
                                                     DEAD_30_DAYS=sum(DEAD_30_DAYS), MORTALLY_INJ=sum(MORTALLY_INJ),
                                                     SERLY_INJ=sum(SERLY_INJ),SLY_INJ=sum(SLY_INJ))



