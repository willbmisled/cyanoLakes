cutoff<-quantile(bioV$sumLbioV, na.rm=T)[4]
bioVcpa<-condprob(bioV$CHLA,bioV$sumLbioV,cutoff,"gt","gte", T)
