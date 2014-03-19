setwd("D:/DATA/SSWRNexusCyano/cyanoLakes/jeff")
require(condprob2)
source("getBioVolCatData.R")
cutoff<-quantile(bioV$sumLbioV, na.rm=T)[4]
binCutoff<-as.numeric(bioV$sumLbioV>=cutoff)
binCutoff<-binCutoff[!is.na(binCutoff)]
bioVcpa<-condprob(bioV$CHLA,bioV$sumLbioV,cutoff,"gt","gte", T,R=1000)
plot.condprob<-function(condprobObj,minss,...){
  maxX<-max(condprobObj[,1][1:(length(condprobObj[,1])-minss)])
  plot(condprobObj[,1],condprobObj[,2],ylim=c(0,1),xlim=c(0,maxX)
       ,...)
  lines(condprobObj[,1],condprobObj[,4])
  lines(condprobObj[,1],condprobObj[,5])
}
plot.condprob(bioVcpa,100)
