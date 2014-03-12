#get the Biovolume data
load('L:/Public/Milstead_Lakes/RData/cyanoBioVolume2014-03-11.rda')

#Data definition LbioV  (bioVolume based on Lester's aggregated Cyano volumes)
#'data.frame':  1148 obs. of  3 variables:
# SITE_ID : chr  "NLA06608-0001" "NLA06608-0002" : NLA ID
# sumLbioV: num  172 260935 6959269 63955 8220 ...: sum of the Cyanobacteria Biovolumes (Vol*Abund)
# bvCat   : Ord.factor w/ 3 levels "LOW"<"MED"<"HIGH": Low<=Q1  High>=Q3
#NOTE:  84 lakes with one or more missing Volume estimates sumLbioV and bcCat==NA
#       33 lakes with no cyano (abund==0) sumLbioV==0 and bcCat=='LOW'

names(LbioV)[1]<-'NLA_ID'


#get the NLA wq data
load("L:/Public/Milstead_Lakes/RData/NLA_Chla_Data_20140116.rda")
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
#NAP    Northern Appalachians
#NPL     Northern Plains
#SAP     Southern Appalachians
#SPL      Southern Plains
#TPL      Temporate Plains
#UMW Upper Midwest
#WMT  Western Mountains
#XER      Xeric
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
#DDs40 Single Sine Method used to Calculate Degree Days with a lower threshold of 40 degrees F
#DDs45 Single Sine Method used to Calculate Degree Days with a lower threshold of 45 degrees F
#DDs50 Single Sine Method used to Calculate Degree Days with a lower threshold of 50 degrees F
#DDs55 Single Sine Method used to Calculate Degree Days with a lower threshold of 55 degrees F
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

Lakes$TS_NTL<-factor(Lakes$TS_NTL,levels=c('Oligo','Meso','Eu','Hyper'),ordered=T)
Lakes$TS_PTL<-factor(Lakes$TS_PTL,levels=c('Oligo','Meso','Eu','Hyper'),ordered=T)
Lakes$TS_CHLA<-factor(Lakes$TS_CHLA,levels=c('Oligo','Meso','Eu','Hyper'),ordered=T)

bioV<-merge(LbioV,Lakes,by='NLA_ID',all=F)


str(bioV)

with(bioV,table(bvCat,TS_CHLA))


LOW<-subset(bioV,bioV$bvCat=="LOW")  
MED<-subset(bioV,bioV$bvCat=="MED") 
HIGH<-subset(bioV,bioV$bvCat=="HIGH") 







