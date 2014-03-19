v<-'cyanoBioVolData'
options(stringsAsFactors=FALSE)

###################
#get Lesters Volume data  Note: these data received from Lester Yuan as an R data set.
  load('L:/Public/Milstead_Lakes/Rdata/LesterCyanoCellVolume20140221.rda')

# vol: the Beaulieu cell volume
# cell.vol.new”: Lester's aggregated cell volume.  The aggregated cell volume starts with the NLA cell volumes, 
        #and replaces cyano taxa volumes with the Beaulieu numbers with the exception of Phormidium.  
        #Lester kept the NLA Phormidium numbers because it appears that these were actually measured in the lab 
        #and so they vary from sample to sample. 
#cell.volume: NLA cell volume

#decide which field to keep
  keep<-c("site.id","taxaname","otu","division","order","family","genus","species","variety",
          "taxatype","vol","abund","cell.volume","cell.vol.new")
  Lester<-subset(phyt.cnt.new[,keep],  
               phyt.cnt.new$visit.no==1 & phyt.cnt.new$lake.samp=='Target_Sampled' & unclass(phyt.cnt.new$division)!=1)
        #Note: unclass(phyt.cnt.new$division)!=1 removes OTUs that are not assigned to a division

#rename to match NLA
  names(Lester)[c(1:4)]<-c("SITE_ID","TAXANAME","OTU","DIVISION")

#convert SITE_ID factors to character (to match the NLA format)
  Lester$SITE_ID<-as.character(Lester$SITE_ID)

#Make df of Lester's cyano data & rename volume fields; remove the generic OTU='Cyanophyta' entries
  LC<-Lester[Lester$DIVISION=='Cyanophyta' & Lester$OTU!='Cyanophyta',
             c("SITE_ID","TAXANAME","OTU","abund","cell.volume","vol", "cell.vol.new")]
  names(LC)<-c("SITE_ID","TAXANAME","OTU","abund","Nvol",       "Bvol","Lvol")

#Calculate Biovolumes
  LC$BbioV<-with(LC,abund*Bvol)  #biovolume based on Beaulieu's aggregated Volume
  LC$LbioV<-with(LC,abund*Lvol)  #biovolume based on Lester's aggregated Volume

#compare the Beaulieu and Lester Biovolume
  with(LC,plot(BbioV,LbioV,pch=19,log='xy'))

#How many missing values in df? 
  test<-function(x) table(is.na(x))
  apply(LC,2,test)
  table(LC$TAXANAME[is.na(LC$Lvol)]) 
  table(LC$TAXANAME[is.na(LC$Bvol)])

#The following OTU/TAXANAME are missing volume estimates (number of missing values in parentheses): 
  #aulosira(1), cylindrospermum(40), glaucospira(42), gloeotrichia(5) 

#How many lakes have missing values?
  table(is.na(aggregate(LC$LbioV,by=list(LC$SITE_ID),sum)[2]))  #84 lakes

#make df of site_ID and whether they have cyanos present and merge to LC
  test<-Lester$DIVISION=='Cyanophyta'
  table(test)
  Lakes<-aggregate(test,by=list(Lester$SITE_ID),sum)
  names(Lakes)<-c('SITE_ID','hasCyano')
  Lakes$hasCyano[Lakes$hasCyano>0]<-1
  table(Lakes$hasCyano)
  LC<-merge(Lakes,LC,by='SITE_ID',all.x=T)
  LC[LC$hasCyano==0,5:10]<-0  #change biovolume and abundance to zeros for lakes without cyanos

#Use the Lester Biovolumes to calculate Cyano biomass categories: 
  bioV<-aggregate(LC$LbioV,by=list(LC$SITE_ID),sum)
  names(bioV)<-c('NLA_ID','sumBioV')
  table(bioV$sumBioV==0,useNA='ifany')  
    #1031 Lakes with Cyano biomass; 33 no cyanos; 84 with missing values
  bioV$bvCat<-'MED'
  bioV$bvCat[bioV$sumBioV<=quantile(bioV$sumBioV,.25,na.rm=T)]<-'LOW'
  bioV$bvCat[bioV$sumBioV>=quantile(bioV$sumBioV,.75,na.rm=T)]<-'HIGH'
  bioV$bvCat[is.na(bioV$sumBioV)]<-NA
    table(bioV$bvCat,useNA='ifany')
  bioV$bvCat<-factor(bioV$bvCat,levels=c('LOW','MED','HIGH'),ordered=TRUE)
  table(bioV$bvCat,useNA='ifany')


#Data definition bioV  (bioVolume based on Lester's aggregated Cyano volumes)
  #'data.frame':  1148 obs. of  3 variables:
  # SITE_ID : chr  "NLA06608-0001" "NLA06608-0002" : NLA ID
  # sumLbioV: num  172 260935 6959269 63955 8220 ...: sum of the Cyanobacteria Biovolumes (Vol*Abund)
  # bvCat   : Ord.factor w/ 3 levels "LOW"<"MED"<"HIGH": Low<=Q1  High>=Q3
      #NOTE:  84 lakes with one or more missing Volume estimates sumLbioV and bcCat==NA
      #       33 lakes with no cyano (abund==0) sumLbioV==0 and bcCat=='LOW'


#Get the NLA microcystin data
require(RODBC)   
con <- odbcConnectAccess("c:/bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
Micro <- sqlQuery(con, "
                  SELECT tblNLA_Microcystin.SITE_ID, tblNLA_Microcystin.Microcystin_ugl, tblNLA_Microcystin.WHO_Category
                  FROM tblNLA_Microcystin;
                  ")
close(con)
str(Micro)

#Data definitions Micro (NLA Microcystin data)
  #'data.frame':  1161 obs. of  3 variables:
  # SITE_ID        : chr  "NLA06608-AK14" "NLA06608-AK38" "NLA06608-AK41" "NLA06608-AK42" ...
  # Microcystin_ugl: num  0 0 0 0.49 0 0 0 0.33 0 0 ...
  # WHO_Category   : num  1 1 1 1 1 1 1 1 1 1 ...

names(Micro)[1]<-'NLA_ID'

#get the NLA wq data
load("L:/Public/Milstead_Lakes/RData/NLA_Chla_Data_20140116.rda")
    #Source = C:/Bryan/PortableApps/R/scripts/Chla/NLA_Chla_Data_20140116.r
#Data Definitions Lakes n=1151 (with some missing values)  
#NLA_ID:  NLA ID assigned to each site
#AlbersX: (m) ESRI USA Contiguous Albers Equal Area Conic X coordinate in from National_LakePoly.shp
#AlbersY: (m) ESRI USA Contiguous Albers Equal Area Conic Y coordinate in from National_LakePoly.shp
#LakeArea: (km2) Lake Area from attribute table of from National_LakePoly.shp
#LakePerim: (km) Lake Perimeter from attribute table of from National_LakePoly.shp
#ShoreDevel: Shoreline development index from attribute table of from National_LakePoly.shp
#DATE_COL:  Date of site visit
#WSA_ECO9 : Wadeable Streams Assessment Aggregated Ecoregion
#CPL  Coastal Plains
#NAP  Northern Appalachians
#NPL	Northern Plains
#SAP	Southern Appalachians
#SPL	Southern Plains
#TPL	Temporate Plains
#UMW	Upper Midwest
#WMT	Western Mountains
#XER	Xeric
#BASINAREA: (km2) Area of lake basin (upstream area) from attribute table of from National_LakePoly.shp
#DEPTHMAX: (m) Maximum Observed Lake Depth 
#ELEV_PT: (m) Site elevation from the National Elevation Dataset
#CHLA: (?g/L) Chlorophyll a concentration. 
#DO2_2M: (mg/L) MEAN DO2 CONC IN UPPER 2m (or UPPER 50% IF DEPTH < 4m) 
#PH_FIELD:  Field pH from Profile DO data (pH measured at first non-zero depth unless only depth was zero)
#COND:  Conductivity (uS/cm @ 25 C)
#ANC:  Gran ANC (ueq/L)
#TURB:  Turbidity (NTU)
#TOC:  Total Organic Carbon (mg/L)
#DOC:  Dissolved Organic Carbon (mg/L)
#NH4:  Ammonium (ueq/L)
#NO3_NO2:  Nitrate + Nitrite by Flow Injection Analysis (mg N/L)
#NTL:  Total Nitrogen (ug/L)
#PTL:  Total Phosphorus (ug/L)
#CL:  Chloride (ueq/L)
#NO3:  Nitrate (ueq/L)
#SO4:  Sulfate (ueq/L)
#CA:  Calcium (ueq/L)
#MG:  Magnesium (ueq/L)
#Na:  Sodium (ueq/L)
#K:  Potassium (ueq/L)
#COLOR:  Color (PCU)
#SIO2:  Silica (mg/L SiO2)
#H:  H+ from PH_LAB (ueq/L)
#OH:  Hydroxide from PH_LAB (ueq/L)
#NH4ION:  Calculated NH4+ protolyte (ueq/L)
#CATSUM:  Sum of Cations (ueq/L)
#ANSUM2:  Sum of Anions using ANC (ueq/L)
#ANDEF2:  Anion Deficit using ANC [C-A] (ueq/L)
#SOBC:  Sum of Base Cations (ueq/L)
#BALANCE2:  Ion Balance using ANC (%)
#ORGION:  Est. Organic Anion (ueq/L)
#CONCAL2:  Calculated Conductivity w/ANC (uS/cm)
#CONDHO2:  D-H-O Calc. Cond. w/ANC (uS/cm)
#SECMEAN:  Secchi transparency (m)(=avg. of disk disappearance and reappearance depths)
#TminW: (degrees C) minimum water temperature observed for depths <=1m  (8 missing values)
#TmaxW: (degrees C) maximum water temperature observed for depths <=1m  (8 missing values)
#TmeanW: (degrees C) mean water temperature for depths <=1m  (8 missing values)
#DDs40	Single Sine Method used to Calculate Degree Days with a lower threshold of 40 degrees F
#DDs45	Single Sine Method used to Calculate Degree Days with a lower threshold of 45 degrees F
#DDs50	Single Sine Method used to Calculate Degree Days with a lower threshold of 50 degrees F
#DDs55	Single Sine Method used to Calculate Degree Days with a lower threshold of 55 degrees F
# MaxLength       : num  (m) the maximum distance on the lake surface between any two points on the shore line.
# MaxWidth        : num  (m) The maximum distance between the shores perpendicular to the line of maximum length.
# MeanWidth       : num  (m) the surface area divided by the maximum length.
# FetchN          : num  (m) max N to S length of lake surface area without land interruption that wind can act on.
# FetchNE         : num  (m) max NE to SW length of lake surface area without land interruption that wind can act on.
# FetchE          : num  (m) max E to W length of lake surface area without land interruption that wind can act on.
# FetchSE         : num  (m) max SE to NW length of lake surface area without land interruption that wind can act on.
# MaxDepthCorrect : num  (m) Max estimated depth-See Hollister et al 2011
# VolumeCorrect   : num  (m3) Estimated Volume
# MeanDepthCorrect: num  (m) VolumeCorrect/SurfaceArea; based on corrected maximum depth
# TS_NTL          : chr  Trophic State Based on NTL;Oligo <=350; Meso >350 & <=750;Eu >750 & <=1400;Hyper >1400
# TS_PTL          : chr  Trophic State Based on PTL;Oligo <=10; Meso >10 & <=25;Eu >25 & <=50;Hyper >50
# TS_CHLA         : chr  Trophic State Based on CHLA;Oligo <=2; Meso >2 & <=7;Eu >7 & <=30;Hyper >30

#Change TS_ to factor
  Lakes$TS_NTL<-factor(Lakes$TS_NTL,levels=c('Oligo','Meso','Eu','Hyper'),ordered=T)
  Lakes$TS_PTL<-factor(Lakes$TS_PTL,levels=c('Oligo','Meso','Eu','Hyper'),ordered=T)
  Lakes$TS_CHLA<-factor(Lakes$TS_CHLA,levels=c('Oligo','Meso','Eu','Hyper'),ordered=T)

#Merge Biovolume, NLA, and Microcystin data
bioV<-merge(bioV,Lakes,by='NLA_ID',all=F)
bioV<-merge(bioV,Micro,by='NLA_ID',all=F)

################Transform data for analysis
#function to test for normality and skewness
testNorm<-function(x,Var){ 
  #install/load packages
  if (!"moments" %in% installed.packages()) install.packages("moments")
  require(moments)
  if (!"nortest" %in% installed.packages()) install.packages("nortest")
  require(nortest)
  #eliminate missing values
  x<-subset(x,!is.na(x) & x!=Inf & x!=-Inf)
  #sample size
  N<-length(x)  
  #Pearson Test of Normality
  P<-tryCatch(pearson.test(x),error = function(e) NA)  
  P<-round(P$p.value,3)
  P[P==0]<-.001
  #Angostino Test of Skewness
  S<-tryCatch(agostino.test(x),error = function(e) NA)
  S<-round(S$p.value,3)
  S[S==0]<-.001
  #Plots with test of Normality and Skewness
  hist(x,main=Var)
  qqnorm(x,main=paste("Obs =",N,"Normality =",P,"Skewness =",S))  
  #results  
  return(data.frame(N=N,Norm=P,Skew=S))
}

#function to try various transformation and test for normality
plotNorm<-function(Variable){
  par(mfrow=c(3,2)) 
  testNorm(bioV[,Variable],Variable) 
  testNorm(log(bioV[,Variable]),paste('log',Variable,sep='')) 
  testNorm(log1p(bioV[,Variable]),paste('log1p',Variable,sep='')) 
  #testNorm(log(log(bioV[,Variable])),paste('loglog',Variable,sep='')) 
  #testNorm(sqrt(bioV[,Variable]),paste('sqrt',Variable,sep='')) 
  #testNorm(asin(sqrt(bioV[,Variable])),paste('asin',Variable,sep='')) 
}


#select numeric, explanatory variables to test
Vars<-c("sumBioV","AlbersX","AlbersY","LakeArea","LakePerim","ShoreDevel",    
        "BASINAREA","DEPTHMAX","ELEV_PT","CHLA","DO2_2M","PH_FIELD",       
        "COND","ANC","TURB","TOC","DOC","NH4",            
        "NO3_NO2","NTL","PTL","CL","NO3","SO4",            
        "CA","MG","Na","K","COLOR","SIO2",           
        "H","OH","NH4ION","CATSUM","ANSUM2","ANDEF2",         
        "SOBC","BALANCE2","ORGION","CONCAL2","CONDHO2","SECMEAN",        
        "TminW","TmaxW","TmeanW","DDs40","DDs45","DDs50",          
        "DDs55","MaxLength","MaxWidth","MeanWidth","FetchN","FetchNE",        
        "FetchE","FetchSE","MaxDepthCorrect","VolumeCorrect","Microcystin_ugl")

#manually loop through the Vars and visually interpret best transformation
i<-1

plotNorm(Vars[i]) 
i<-i+1


#Untransformed Variables
  Raw<-c('AlbersX','AlbersY','DO2_2M','PH_FIELD','TminW','TmaxW','TmeanW',
         'ANC','ANDEF2','NO3_NO2','NO3','BALANCE2')

#Log transformed variables
  Log<-c('LakeArea','LakePerim','BASINAREA','DEPTHMAX','CHLA','COND',
    'TURB','TOC','DOC','NH4','NTL','PTL','CL','SO4','CA','MG','Na','K','SIO2',
    'CATSUM','ANSUM2','SOBC','ORGION','CONCAL2','CONDHO2','SECMEAN',
    'DDs40','DDs45','DDs50','DDs55','MeanWidth','FetchN','FetchNE','FetchE','FetchSE',
    'ShoreDevel','MaxLength','MaxWidth','MaxDepthCorrect')

#Log1p transformed variables  (log(x+1))
  Log1p<-c('sumBioV','ELEV_PT','COLOR','H','OH','NH4ION','VolumeCorrect','Microcystin_ugl')

#Create df of the transformed variables
TbioV<-data.frame(bioV$NLA_ID,bioV[,Raw],log(bioV[,Log]),log1p(bioV[,Log1p]))

#Rename Log transformed Variables
  names(TbioV)<-c('NLA_ID',Raw,paste('Ln_',Log,sep=''),paste('Ln1p_',Log1p,sep=''))



#check for NaN and -Inf
  testNaN<-function(x) table(is.nan(x))
    apply(TbioV[,-1],2,testNaN)
  
  testInf<-function(x) table(is.infinite(x))
    apply(TbioV[,-1],2,testInf)

##################save the data
  bioV_BuildDate<-Sys.Date()  #date the data were built
  save(bioV_BuildDate,bioV,TbioV,
       file='C:/Bryan/PortableApps/R/scripts/cyanoLakes/bryan/cyanoBioVolData.rda')  


####################Load the NLA Data and Biovolume Data
  #Data Definitions:  
    browseURL('https://github.com/jhollist/cyanoLakes/blob/master/bryan/cyanoBioVolData.md')
  #Get the Data
    load('https://github.com/jhollist/cyanoLakes/blob/master/bryan/cyanoBioVolData.rda')


