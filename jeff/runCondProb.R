setwd("D:/DATA/SSWRNexusCyano/cyanoLakes/jeff")
require(condprob2)
source("getBioVolCatData.R")
cutoff<-quantile(bioV$sumLbioV, na.rm=T)[4]
binCutoff<-as.numeric(bioV$sumLbioV>=cutoff)
minss<-cpaMinSamp(binCutoff,10,20,0.01)
bioVcpa<-condprob(bioV$CHLA,bioV$sumLbioV,cutoff,"gt","gte", T,R=1000,
                  minSamp=minss)
plot.condprob<-function(condprobObj,...){
  plot(condprobObj[,1],condprobObj[,2],...)
  lines(condprobObj[,1],condprobObj[,4])
  lines(condprobObj[,1],condprobObj[,5])
}