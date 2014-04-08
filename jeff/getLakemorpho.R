#' Pull NLA LakeMorpho Objects
#' 
#' @param homeDir character vector of the home directory in which to start looking for lakemorpho objects
#' @param comid a vector of comids
#' @param outDir an existing directory, will be created if it doesn't exist
#' 
#' @examples
#' library(dplyr)
#' lmorphoAll_src<-src_sqlite("jeff/lakemorpho.sqlite3",create=FALSE)
#' lmorphoAll_sqlite<-tbl(lmorphoAll_src,"lakemorpho")
#' lmorphoNLA<-filter(lmorphoAll_sqlite,!is.na(nlaSITE_ID))%.%
#'                select(COMID,V2COMID,nlaSITE_ID)%.%
#'                data.frame()
#' getLakemorpho("L:/Priv/LakeMorphometry",lmorphoNLA$COMID,"nlalm")                
getLakemorpho<-function(homeDir, comid, outDir){
  if(!file.exists(outDir)){dir.create(outDir)}
  folders<-list.dirs(homeDir,full.names=TRUE,recursive=FALSE)
  folders<-folders[grep("[0-9LNSWU]$", folders)]
  folders<-paste(folders,"lakemorphodata",sep="/")
  for(i in 1:length(folders)){
    lmlist<-list.files(folders[i],"*.RData",full.names=T)
    comidList<-as.numeric(gsub(".RData","",gsub("^.*/lakemorpho","",lmlist)))
    nlalmlist<-lmlist[comidList%in%comid]
    file.copy(nlalmlist,outDir,overwrite=TRUE)
  }
}
