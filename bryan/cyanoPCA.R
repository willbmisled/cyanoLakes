v<-'cyanoPCA.r'

####################Load the NLA Data and Biovolume Data
#Data Definitions:  
  #
browseURL('https://github.com/jhollist/cyanoLakes/blob/master/bryan/cyanoBioVolData.md')
#Get the Data
  load(url('https://raw.github.com/jhollist/cyanoLakes/master/bryan/cyanoBioVolData.rda'))


#Assign Colors for plots
  Colors<-data.frame(N=rep(NA,nrow(bioV)))
  #Color by Trophic State
    Color<-c("blue","cyan","orange","red")
      Colors$N<-bioV$TS_NTL
        levels(Colors$N)<-(Color)
          table(bioV$TS_NTL,Colors$N)
      Colors$P<-bioV$TS_PTL
        levels(Colors$P)<-(Color)
          table(bioV$TS_PTL,Colors$P)
      Colors$C<-bioV$TS_CHLA
        levels(Colors$C)<-(Color)
          table(bioV$TS_CHLA,Colors$C)
  #Color by cyano biovolume category
    Color<-c("blue","orange","red")
      Colors$BV<-bioV$bvCat
        levels(Colors$BV)<-(Color)
          table(bioV$bvCat,Colors$BV)

#select variables for the PCA

#start with a list of all Vars in TbioV
  allVars<-c("NLA_ID","AlbersX","AlbersY","DO2_2M","PH_FIELD",            
           "TminW","TmaxW","TmeanW","ANC","ANDEF2",              
           "NO3_NO2","NO3","BALANCE2","Ln_LakeArea","Ln_LakePerim",        
           "Ln_BASINAREA","Ln_DEPTHMAX","Ln_CHLA","Ln_COND","Ln_TURB",            
           "Ln_TOC","Ln_DOC","Ln_NH4","Ln_NTL","Ln_PTL",              
           "Ln_CL","Ln_SO4","Ln_CA","Ln_MG","Ln_Na",               
           "Ln_K","Ln_SIO2","Ln_CATSUM","Ln_ANSUM2","Ln_SOBC",             
           "Ln_ORGION","Ln_CONCAL2","Ln_CONDHO2","Ln_SECMEAN","Ln_DDs40",            
           "Ln_DDs45","Ln_DDs50","Ln_DDs55","Ln_MeanWidth","Ln_FetchN",           
           "Ln_FetchNE","Ln_FetchE","Ln_FetchSE","Ln_ShoreDevel","Ln_MaxLength",        
           "Ln_MaxWidth","Ln_MaxDepthCorrect","Ln1p_sumBioV","Ln1p_ELEV_PT","Ln1p_COLOR",          
           "Ln1p_H","Ln1p_OH","Ln1p_NH4ION","Ln1p_VolumeCorrect","Ln1p_Microcystin_ugl")

#Prune the list
  Vars<-c(               #"NLA_ID",
            "AlbersX","AlbersY","DO2_2M","PH_FIELD",            
                         #"TminW","TmaxW",
           "TmeanW","ANC","ANDEF2",              
           "NO3_NO2","NO3","BALANCE2","Ln_LakeArea","Ln_LakePerim",        
           "Ln_BASINAREA","Ln_DEPTHMAX","Ln_CHLA","Ln_COND","Ln_TURB",            
           "Ln_TOC","Ln_DOC","Ln_NH4","Ln_NTL","Ln_PTL",              
           "Ln_CL","Ln_SO4","Ln_CA","Ln_MG","Ln_Na",               
           "Ln_K","Ln_SIO2","Ln_CATSUM","Ln_ANSUM2","Ln_SOBC",             
           "Ln_ORGION","Ln_CONCAL2","Ln_CONDHO2","Ln_SECMEAN",
                        #"Ln_DDs40","Ln_DDs45",
           "Ln_DDs50",
                        #"Ln_DDs55",
           "Ln_MeanWidth","Ln_FetchN",           
           "Ln_FetchNE","Ln_FetchE","Ln_FetchSE","Ln_ShoreDevel","Ln_MaxLength",        
           "Ln_MaxWidth","Ln_MaxDepthCorrect","Ln1p_sumBioV","Ln1p_ELEV_PT","Ln1p_COLOR",          
           "Ln1p_H","Ln1p_OH","Ln1p_NH4ION","Ln1p_VolumeCorrect","Ln1p_Microcystin_ugl")

#Data for the PCA
  Keep<-complete.cases(TbioV[,Vars])  #identify rows without missing values
  pcaVars<-TbioV[Keep,Vars]         #new df with the complete cases and Vars specified above
  pcaColors<-Colors[Keep,]    #select the colors for the PCA plots


#PCA
  #You tend to use the covariance matrix when the variable scales are similar and the correlation matrix 
    #when variables are on different scales. Using the correlation matrix standardises the data.

  #correlations
    round(cor(pcaVars),2)   
  #covariance
    round(cor(pcaVars),2)   

#Run PCA both ways-probably should use the Correlation Matrix even though it explains less of the variance
pcaCor<-princomp(pcaVars,cor=T)
  summary(pcaCor)     #pca1=30%  pca1-3=60%
  loadings(pcaCor)
  pcaCor$loadings[abs(order(pcaCor$loadings[,1])),1]

pcaCov<-princomp(pcaVars,cor=F)
  summary(pcaCov)     #pca1=80%  pca1-3=99.9%
  loadings(pcaCov)
  pcaCov$loadings[abs(order(pcaCov$loadings[,1])),1]

###################

PCA1<-pcaCov

Col<-as.character(pcaColors$BV)
#Leg<-'bottomleft'
#Leg<-'topleft'
Leg<-'bottomright'

x<-1
y<-3
w<-2  #scale for the arrows
X <- PCA1$loadings*w  # Extract & scale loadings
plot(PCA1$scores[,x],PCA1$scores[,y],col=Col,pch=16, xlab=paste('PCA',x,sep=''),ylab=paste('PCA',y,sep=''),
     main=paste('PCA= ','; Cor=T','; Color= Cyano BioVol',sep=""))

legend(Leg,c('Hyper-','Eu-','Meso-','Oligo-'),col=Color,pch=16)


plotPCA<-function(pca,x,y,Color,title){
  plot(pca$scores[,x],pca$scores[,y],col=Color,pch=16, xlab=paste('PCA',x,sep=''),ylab=paste('PCA',y,sep=''),
       main=title)
}

par(mfrow = c(2, 2))
plotPCA(pcaCor,1,2,as.character(pcaColors$BV),'Corr. Matrix; Colors=Biovol. Cat.')
plotPCA(pcaCor,1,3,as.character(pcaColors$BV),'Corr. Matrix; Colors=Biovol. Cat.')
plotPCA(pcaCor,1,2,as.character(pcaColors$C),'Corr. Matrix; Colors=TS_CHLA Cat.')
plotPCA(pcaCor,1,3,as.character(pcaColors$C),'Corr. Matrix; Colors=TS_CHLA Cat.')

plotPCA(pcaCov,1,2,as.character(pcaColors$BV),'Cov. Matrix; Colors=Biovol. Cat.')
plotPCA(pcaCov,1,3,as.character(pcaColors$BV),'Cov. Matrix; Colors=Biovol. Cat.')
plotPCA(pcaCov,1,2,as.character(pcaColors$C),'Cov. Matrix; Colors=TS_CHLA Cat.')
plotPCA(pcaCov,1,3,as.character(pcaColors$C),'Cov. Matrix; Colors=TS_CHLA Cat.')



###############################
PCA1<-pcaCov

Col<-as.character(pcaColors$BV)
#Leg<-'bottomleft'
#Leg<-'topleft'
Leg<-'bottomright'

x<-1
y<-3
w<-2  #scale for the arrows
X <- PCA1$loadings*w  # Extract & scale loadings
plot(PCA1$scores[,x],PCA1$scores[,y],col=Col,pch=16, xlab=paste('PCA',x,sep=''),ylab=paste('PCA',y,sep=''),
     main=paste('PCA= ','; Cor=T','; Color= Cyano BioVol',sep=""))
arrows(0, 0, X[,x], X[,y], len=0.1, col="black",lwd=2)  # Plot arrows
text(1.1*X[,x],1.1*X[,y], rownames(X), col="black", xpd=T)  # Label the arrows
legend(Leg,c('Hyper-','Eu-','Meso-','Oligo-'),col=Color,pch=16)

############eof