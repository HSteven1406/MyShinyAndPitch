MyFunc <- function(groupy,selection, ycol){

if (groupby=='Provinces'){
  Data <- SumData[SumData$Prov %in% selection,]
  if (ycol=="ACCT"){
    Data <- Data %>% group_by(Prov,Region) %>% summarise(ACCT=sum(ACCT))
    MyPlot <- ggplot(Data, aes(x=Prov, y= ACCT, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="DEAD"){
    Data <- Data %>% group_by(Prov,Region) %>% summarise(DEAD=sum(DEAD))
    MyPlot <- ggplot(Data, aes(x=Prov, y= DEAD, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with deaths')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="DEAD_30_DAYS"){
    Data <- Data %>% group_by(Prov,Region) %>% summarise(DEAD_30_DAYS=sum(DEAD_30_DAYS))
    MyPlot <- ggplot(Data, aes(x=Prov, y= DEAD_30_DAYS, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with deaths within 30 days')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="MORTALLY_INJ"){
    Data <- Data %>% group_by(Prov,Region,RegionRegion) %>% summarise(MORTALLY_INJ=sum(MORTALLY_INJ))
    MyPlot <- ggplot(Data, aes(x=Prov, y= MORTALLY_INJ, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with mortally injured')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="SERLY_INJ"){
    Data <- Data %>% group_by(Prov,Region) %>% summarise(SERLY_INJ=sum(SERLY_INJ))
    MyPlot <- ggplot(Data, aes(x=Prov, y= SERLY_INJ, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with seriously injured')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else {
    Data <- Data %>% group_by(Prov,Region) %>% summarise(SLY_INJ=sum(SLY_INJ))
    MyPlot <- ggplot(Data, aes(x=Prov, y= SLY_INJ, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with slightly injured')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }


}
else {
  Data <- SumData[SumData$Region %in% selection,]
  if (ycol=="ACCT"){
    Data <- Data %>% group_by(Region) %>% summarise(ACCT=sum(ACCT))
    MyPlot <- ggplot(Data, aes(x=Region, y= ACCT, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="DEAD"){
    Data <- Data %>% group_by(Region) %>% summarise(DEAD=sum(DEAD))
    MyPlot <- ggplot(Data, aes(x=Region, y= DEAD, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with deaths')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="DEAD_30_DAYS"){
    Data <- Data %>% group_by(Region) %>% summarise(DEAD_30_DAYS=sum(DEAD_30_DAYS))
    MyPlot <- ggplot(Data, aes(x=Region, y= DEAD_30_DAYS, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with deaths within 30 days')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="MORTALLY_INJ"){
    Data <- Data %>% group_by(Region) %>% summarise(MORTALLY_INJ=sum(MORTALLY_INJ))
    MyPlot <- ggplot(Data, aes(x=Region, y= MORTALLY_INJ, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with mortally injured')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else if (ycol=="SERLY_INJ"){
    Data <- Data %>% group_by(Region) %>% summarise(SERLY_INJ=sum(SERLY_INJ))
    MyPlot <- ggplot(Data, aes(x=Region, y= SERLY_INJ, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with seriously injured')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
  else {
    Data <- Data %>% group_by(Region) %>% summarise(SLY_INJ=sum(SLY_INJ))
    MyPlot <- ggplot(Data, aes(x=Region, y= SLY_INJ, fill=Region)) + 
      geom_bar(stat='identity') + labs(x='Province',y='Number of accidents with slightly injured')+
      scale_fill_manual(breaks=c('Flemish Region','Walloon Region','Brussels Capital Region'),
                        values=c('yellow3','indianred2','steelblue'))
  }
}
MyPlot
}
