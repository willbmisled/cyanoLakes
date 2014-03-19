cyanoBioVolData.RDA Data Defintions
========================================================
**Last Update=March 19, 2014**
***************
**This data set contains 3 objects**:
- bioV_BuildDate: is the date of the last data build
- bioV: a dataframe with the NLA data and the Biovolumes (see definitions below)
- TbioV: the continuous numerical data from bioV that was transformed to reduce left skew (see below)
************

**bioV**: NLA water quality data, NLA microcystin data, and biovolume data from Lester Yuan.

'data.frame':  1148 obs. of  68 variables: 

**Field**  | **Definition**
------------- | ------------- 
NLA_ID|ID assigned to each site
sumBioV  |(um^3 cell/mL)sum of cyanobacteria abundance * taxa specific Cell volume 
bvCat|Cyanobiovolume category; Ordered Factor; Low=1st Q, Med=Q2 & Q3, High=4th Q
AlbersX|(m) ESRI USA Contiguous Albers Equal Area Conic X coordinate in from National_LakePoly.shp
AlbersY|(m) ESRI USA Contiguous Albers Equal Area Conic Y coordinate in from National_LakePoly.shp
LakeArea|(km2) Lake Area from attribute table of from National_LakePoly.shp
LakePerim|(km) Lake Perimeter from attribute table of from National_LakePoly.shp
ShoreDevel|Shoreline development index from attribute table of from National_LakePoly.shp
DATE_COL|Date of site visit
WSA_ECO9|Wadeable Streams Assessment Aggregated Ecoregion; see definitions below
BASINAREA|(km2) Area of lake basin (upstream area) from attribute table of from National_LakePoly.shp
DEPTHMAX|(m) Maximum Observed Lake Depth 
ELEV_PT|(m) Site elevation from the National Elevation Dataset
CHLA|Chlorophyll a concentration (µg/L).  
DO2_2M|MEAN DO2 CONC (mg/L) IN UPPER 2m (or UPPER 50% IF DEPTH < 4m)
PH_FIELD|Field pH from Profile DO data (pH measured at first non-zero depth unless only depth was zero)
COND|Conductivity (uS/cm @ 25 C)
ANC|Gran ANC (ueq/L)
TURB|Turbidity (NTU)
TOC|Total Organic Carbon (mg/L)
DOC|Dissolved Organic Carbon (mg/L)
NH4|Ammonium (ueq/L)
NO3_NO2|Nitrate + Nitrite by Flow Injection Analysis (mg N/L)
NTL|Total Nitrogen (ug/L)
PTL|Total Phosphorus (ug/L)
CL|Chloride (ueq/L)
NO3|Nitrate (ueq/L)
SO4|Sulfate (ueq/L)
CA|Calcium (ueq/L)
MG|Magnesium (ueq/L)
Na|Sodium (ueq/L)
K|Potassium (ueq/L)
COLOR|Color (PCU)
SIO2|Silica (mg/L SiO2)
H|H+ from PH_LAB (ueq/L)
OH|Hydroxide from PH_LAB (ueq/L)
NH4ION|Calculated NH4+ protolyte (ueq/L)
CATSUM|Sum of Cations (ueq/L)
ANSUM2|Sum of Anions using ANC (ueq/L)
ANDEF2|Anion Deficit using ANC (C - A) (ueq/L)
SOBC|Sum of Base Cations (ueq/L)
BALANCE2|Ion Balance using ANC (%)
ORGION|Est. Organic Anion (ueq/L)
CONCAL2|Calculated Conductivity w/ANC (uS/cm)
CONDHO2|D-H-O Calc. Cond. w/ANC (uS/cm)
SECMEAN|Secchi transparency (m)(=avg. of disk disappearance and reappearance depths)
TminW|(degrees C) minimum water temperature observed for depths <=1m  (8 missing values)
TmaxW|(degrees C) maximum water temperature observed for depths <=1m  (8 missing values)
TmeanW|(degrees C) mean water temperature for depths <=1m  (8 missing values)
DDs40|Single Sine Method used to Calculate Degree Days with a lower threshold of 40 degrees F
DDs45|Single Sine Method used to Calculate Degree Days with a lower threshold of 45 degrees F
DDs50|Single Sine Method used to Calculate Degree Days with a lower threshold of 50 degrees F
DDs55|Single Sine Method used to Calculate Degree Days with a lower threshold of 55 degrees F
MaxLength|(m) the maximum distance on the lake surface between any two points on the shore line.
MaxWidth|(m) The maximum distance between the shores perpendicular to the line of maximum length.
MeanWidth|(m) the surface area divided by the maximum length.
FetchN| (m) max N to S length of lake surface area without land interruption that wind can act on.
FetchNE|(m) max NE to SW length of lake surface area without land interruption that wind can act on.
FetchE|(m) max E to W length of lake surface area without land interruption that wind can act on.
FetchSE|(m) max SE to NW length of lake surface area without land interruption that wind can act on.
MaxDepthCorrect|(m) Max estimated depth-See Hollister et al 2011
VolumeCorrect|(m3) Estimated Volume
MeanDepthCorrect|(m) VolumeCorrect/SurfaceArea; based on corrected maximum depth
TS_NTL|Trophic State Based on NTL; Ordered Factor; Oligo <=350; Meso >350 & <=750;Eu >750 & <=1400;Hyper >1400
TS_PTL|Trophic State Based on PTL; Ordered Factor; Oligo <=10; Meso >10 & <=25;Eu >25 & <=50;Hyper >50
TS_CHLA|Trophic State Based on CHLA; Ordered Factor; Oligo <=2; Meso >2 & <=7;Eu >7 & <=30;Hyper >30
Microcystin_ugl| (µg/L) Total Microcystin Conc.  Values listed as "<0.1" set to Zero. 
WHO_Category |Microcystin WHO Category
********
**WSA_ECO9 Ecoregions**

**WSA_ECO9**      | **Ecoregions**
------------- | ------------- 
CPL | Coastal Plains
NAP|	Northern Appalachians
NPL|	Northern Plains
SAP|	Southern Appalachians
SPL|	Southern Plains
TPL|	Temporate Plains
UMW|	Upper Midwest
WMT|	Western Mountains
XER|	Xeric


********
**Transformations**-bioV variables tested for normality (they aren't) and skew (they are).  Data viewed graphically and assigned to a log transformation or used raw depending on the distribution.  Data with zeros were transformed as log(x+1)

**Untransformed Variables**
  Raw<-c('AlbersX','AlbersY','DO2_2M','PH_FIELD','TminW','TmaxW','TmeanW',
         'ANC','ANDEF2','NO3_NO2','NO3','BALANCE2')

**Log transformed variables**
  Log<-c('LakeArea','LakePerim','BASINAREA','DEPTHMAX','CHLA','COND',
    'TURB','TOC','DOC','NH4','NTL','PTL','CL','SO4','CA','MG','Na','K','SIO2',
    'CATSUM','ANSUM2','SOBC','ORGION','CONCAL2','CONDHO2','SECMEAN',
    'DDs40','DDs45','DDs50','DDs55','MeanWidth','FetchN','FetchNE','FetchE','FetchSE',
    'ShoreDevel','MaxLength','MaxWidth','MaxDepthCorrect')

**Log1p transformed variables (log(x+1))**
  Log1p<-c('sumBioV','ELEV_PT','COLOR','H','OH','NH4ION','VolumeCorrect','Microcystin_ugl')


