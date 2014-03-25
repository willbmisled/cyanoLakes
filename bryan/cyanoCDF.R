v<-'cyanoCDF.r'

####################Load the NLA Data and Biovolume Data
#Data Definitions:  
browseURL('https://github.com/jhollist/cyanoLakes/blob/master/bryan/cyanoBioVolData.md')
#Get the Data
load(url('https://raw.github.com/jhollist/cyanoLakes/master/bryan/cyanoBioVolData.rda'))

a<-bioV  #copy df to play with it
a$mcPlus.01<-a$Microcystin_ugl+.01  #most of the Microcystin values are zero.  Add 0.01 to value for log plot

#compare the BioVolume Categories to the trophic state categories
  with(a,table(bvCat,TS_CHLA))

#subset the data by bvCat
  LOW<-subset(a,bioV$bvCat=="LOW")  
  MED<-subset(a,bioV$bvCat=="MED") 
  HIGH<-subset(a,bioV$bvCat=="HIGH") 


fPlotCDF<-function(x){  #x is the variable name as character e.g. 'CHLA', 'NTL', 'PTL'
  Low<-LOW[,x]
  Med<-MED[,x]
  High<-HIGH[,x]
  plot(ecdf(Low),col=Color[1],xlim=c(min(a[,x],na.rm=T),max(a[,x],na.rm=T)),log='x',
       xlab=x,ylab='CDF',
       main='Cummulative Distribution Function by Cyano Biovolume Category')
  abline(h=.5,col='blue')
  plot(ecdf(Med),col=Color[2],add=T)
  plot(ecdf(High),col=Color[3],add=T)
  legend('bottomright',c('High','Med','Low'),col=rev(Color),pch=19)
  mtext(paste(v,'.r ',Sys.Date(),sep='') ,1,3,adj=1,cex=.7)
}

par(mfrow=c(1,1))
Color<-c('green','orange','red')
fPlotCDF('CHLA')
fPlotCDF('NTL')
fPlotCDF('PTL')
fPlotCDF('mcPlus.01')

############eof