fPlotCDF<-function(x,v="cyanoBiovol_Analysis"){  #x is the variable name as character e.g. 'CHLA', 'NTL', 'PTL'
  Low<-LOW[,x]
  Med<-MED[,x]
  High<-HIGH[,x]
  plot(ecdf(Low),col=Color[1],xlim=c(min(Lakes[,x],na.rm=T),max(Lakes[,x],na.rm=T)),log='x',
       xlab=x,ylab='CDF',
       main='Cummulative Distribution Function by Cyano Biovolume Category')
  abline(h=.5,col='blue')
  plot(ecdf(Med),col=Color[2],add=T)
  plot(ecdf(High),col=Color[3],add=T)
  legend('topleft',c('High','Med','Low'),col=rev(Color),pch=19)
  mtext(paste(v,'.r ',Sys.Date(),sep='') ,1,3,adj=1,cex=.7)
}

Color<-c('green','orange','red')
fPlotCDF('CHLA')
fPlotCDF('NTL')
fPlotCDF('PTL')