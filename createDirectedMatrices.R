## this function is called by the main one ReadMoveBankDataFormat
## it loops on all time groups
## for each one it updates the numbers of co-occuring vultures
createDirectedMatrices<-function(Dataset, DistThreshold){
  
  
ColumToSelect=c("ID","coords.x2","coords.x1","Easting","Northing","timegroup","group")
for (timgrpind in 1: max(Dataset$timegroup)){#loop on all time groups ## timegroup comes from analysis
  ## extract current time group 
  timegroupDF=subset(Dataset, timegroup==timgrpind,select=ColumToSelect)
  #print(timegroupDF[order(timegroupDF$group),])#,'timegroupDF$ID'#plot it
  
  ## working within this time group: dyads and distances: now dont need timegroup but 'group' (spatiotemporal) 
  timegroupDF=subset(timegroupDF, timegroup==timgrpind,select=c("ID","coords.x2","coords.x1","group")) #subset columns of interest further
 
##these vultures were observed around the same time on the same day
  
  DT <- expand.grid.df(timegroupDF,timegroupDF);  
  #Each vulture with all others so that their locations can be compared to see who was close to each other
  
  names(DT)[5:7] <- c('ID2',"lat_secondInd","long_secondInd") 
  ##making a template: ID lat long  group (spatial)   ID2 lat2 long2  group2 (spatial)
  setDT(DT)[ , dist_km := distGeo(matrix(c(coords.x1, coords.x2), ncol = 2), 
                                  matrix(c(long_secondInd, lat_secondInd), ncol = 2))/1000];
  
  #distance between all dyads
  #distGeo  = Vector of distances in meters (which is why divided by 1000)
  #setDT = instead of as.data.table because big lists/dataframes make copies first and then perform a function
  #         and can thus take a long time and memory
  #dist_km find the shortest distance between two points i.e. latlong of ID and ID2 so those with 
  # self would obviously give dist_km=0 
  # distGeo = Highly accurate estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid). The shortest path between two points on an ellipsoid is called the geodesic.
  ## create all possible dyads of vultures in a long format:
  PresentVultures=subset(DT, dist_km==0&as.character(ID)==as.character(ID2),select=c(1,5))
  ##Identify all self-association dyads with zero inter-location distance and same IDs
  #these are the vultures present in this time group - selecting only ID and ID2 i.e. columns 1 & 5
  PresentVultures=as.data.frame(unique(PresentVultures$ID))#PresentVultures=unique(PresentVultures$ID);
  #all self-associating IDs
  PresentVultures=expand.grid.df(PresentVultures,PresentVultures,unique=F)
  #transform back to rows of ID1=ID2
    #these are the dyads that has concurrent time point
  names(PresentVultures)=c('ID','ID2') #these are all the vultures whose location was recorded around the same time including self
   ## loop on current dyads (including self) to update co-occurances:
  for (dyadcnt in 1: dim(PresentVultures)[1]){
    Dyadind=which(SimlDataPntCnt$ID==PresentVultures$ID[dyadcnt]&SimlDataPntCnt$ID2==PresentVultures$ID2[dyadcnt])
    #In above line, just identifying which row in the empty SimlDataPntCnt ID1 and ID2 dyads are the same as PresentVulture for one timgroup at a time 
    #identified all rows of self AND non-self association in same timegroup and 0 distance in SimlDataPntCnt
    ##gives which row number in SimlDataPntCnt has the same dyad
    SimlDataPntCnt$counter[Dyadind]=SimlDataPntCnt$counter[Dyadind]+1; 
    #Dyadind is the row number with that dyad
    #adding one to the frequency such that this dyad could've hung out close to each other because they were around at the same time
    #add another (1) tallymark to counter in front of the dyad every time the pair co-occurs in time 
  }#for loop on current dyads 
  ## since self dyads appear only once
  SelfDyad=which(PresentVultures$ID==PresentVultures$ID2);#since self dyads appear only once, another loop on them, so diag will be counted twice like the rest
  
  for (dyadcnt in (SelfDyad)){#count dyad twice? A-B and B-A
    Dyadind=which(SimlDataPntCnt$ID==PresentVultures$ID[dyadcnt]&SimlDataPntCnt$ID2==PresentVultures$ID2[dyadcnt])
    }
  ## now setting interacting dyads
  InteractingSelf=subset(DT, dist_km==0 & (as.character(ID)==as.character(ID2))) 
  InteractingSelf=InteractingSelf[!duplicated(InteractingSelf$ID),]
  InteractingDyads=subset(DT,(dist_km<=DistThreshold/1000 & (as.character(ID)!=as.character(ID2)))) 
  #in the Interacting dyads we check if a dyad was spatially proximate
  
  #subset data table such that non self-overlapping IDs as well as within a certain distance from each other
  InteractingDyads=InteractingDyads[!duplicated(InteractingDyads[,c("ID","ID2")])];
  
  if(dim(PresentVultures)[1]<dim(InteractingDyads)[1])      {
    break
  }#for debugging
  

  InteractingDyads=rbind(InteractingDyads,InteractingSelf)
  #rbind non-self overlapping rows with self-overlapping rows
  
  ## a loop on interacting dyads, non-self, for updating the CoocurCountr storge
  for (dyadcnt in 1: dim(InteractingDyads)[1]){#
    Dyadind=which(CoocurCountr$ID==InteractingDyads$ID[dyadcnt]&CoocurCountr$ID2==InteractingDyads$ID2[dyadcnt])
    #Identifying in the empty dataframe where SPATIO-temporal proximity is recorded, which dyads are in which rows
    #Spatial proximity between dyads was calculated after SimlDataPntCnt (temporal overlap) in the step where (< distthreshold)
    #was calculated as InteractingDyads
    CoocurCountr$counter[Dyadind]=CoocurCountr$counter[Dyadind]+1;#
  }#loop 
  
  # }#if making sure Ids match
  
  rm(list=c("DT","InteractingDyads","InteractingSelf","SelfDyad","Dyadind","dyadcnt","timegroupDF"))
  
}#loop on time groups
return(list(SimlDataPntCnt,CoocurCountr))
}
