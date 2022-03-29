#initialize working space
rm(list=ls()) #clean workspace
graphics.off() #close all plots

########################    CoRoost 2021   ##################

#### loading packages #######
library(move)#for downloading data
library(mapproj);library(ggmap) #these packages are necessary to work with google maps
library(spatsoc);library("asnipe");library("igraph"); # for working with the SN parts
library(reshape);library(data.table) #for the manual section where i build the SN myself
library(adehabitatLT);
library(moveVis)
library(ggplot2)  # sample plots
library(dplyr)    # bind_rows(), etc
library(sp)       # spatial data

source('C:/XXXXX/MyCodes/createDirectedMatrices.R')

#### key paramterer values ######
MaxSpeedPermited=120 #in movebank units (m/s) anything above this will be filtered

VulturesToPlotMap=10:15 
DistThreshold=1000 #---at what distance two indi are considered interacting
TimeThreshold='10 minutes' # timegroups - temporally overlapping
MinCoocurForValue=2; #minimal number of coocurences for considering a viable pair- 

############# other variables specified##########

MaxSpeedPermited=120 #in movebank units (m/s??) anything above this will be filtered
roostbuffer = 50 # buffer around roosting polygons (in metres)
feedBuff = 100 # buffer around feeding stations (in metres)

################### reading data from movebank ###################

Password = "XXXXXX"
MB.LoginObject=movebankLogin(username='NitikaSharma',password=Password);rm(Password)

## Extracting Dec 2020 onwards data from the study="Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel" in MoveBank

MoveStackDataset=getMovebankData(study=1252551761, login=MB.LoginObject,
                                 includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,removeDuplicatedTimestamps=TRUE,
                                 timestamp_start="20200828120100000", ##Starting on December 1 2018 to include in breeding commencemnet
                                 timestamp_end="20210814120100000")#animalName



#### just loooking on raw downloaded data ####
show(MoveStackDataset)
summary(MoveStackDataset)

unstacked=split(MoveStackDataset) #splitting MoveBank data into individuals 
#sanity checks
length(unstacked)
class(unstacked)
unstacked[[3]]@data
class(unstacked[[8]])

citations(MoveStackDataset)
equalProj(MoveStackDataset)
n.locs(MoveStackDataset)
timeLag(MoveStackDataset)
#too heavy but works: 
#plot(MoveStackDatasetOhad,  lwd=2, xlab="location_long", ylab="location_lat",col='blue',pch=5)

############# very basic filtering. stage 1  ##############
## creating an empty metadata storage df
TagsMetaData=setNames(data.frame(matrix(ncol =8, nrow = length(unstacked))), 
                      c('name',"InitialPoints", "PercentThrown", "N_locs","TrackDurationDays","DaysBetweenStartEnd",'FirstDay','LastDay'))

#### Extracting Breeding season data: December 2020 to June 2021 ##########

library(lubridate)

BreedingCoRoost_2021 <- lapply(unstacked, subset,ground_speed<=5 & 
                                month(timestamp)==1 | month(timestamp)==2| month(timestamp)==3|
                                month(timestamp)==4| month(timestamp)==5| 
                                month(timestamp)==6|(month(timestamp)==12 & year(timestamp)==2020))##Also included Dec 2020 in breeding season 

BreedingCoRoost_2021 <- BreedingCoRoost_2021[sapply(BreedingCoRoost_2021, function(x) dim(x)[1]) > 0] ##only keep non-empty dataframes
length(BreedingCoRoost_2021)

hist(unique(date(BreedingCoRoost_2021[[8]]@data$timestamp)), breaks = 13)

min(date(BreedingCoRoost_2021[[1]]@data$timestamp))
max(date(BreedingCoRoost_2021[[1]]@data$timestamp))

######################
## converting back to a movestack
Breeding_MS<-moveStack(BreedingCoRoost_2021)
tz(MoveStackDatasetOhad_2021)
idData(Breeding_MS)
Breeding_DF <- methods::as(Breeding_MS, "data.frame")
Breeding_DF$DateOnly<-as.Date(as.character(Breeding_DF$timestamp));
head(Breeding_DF)

#########Removing Out of Israel Breeding locations before removing vultures recorded for inadequate duration #########
####################Pruning data to only select points falling inside Israel polygon  ################

FileName='CutOffRegion.kml'
LayerName='Regional_polygon'
Outline = readOGR(dsn="C:/XXXXX/KML_Files/CutOffRegion.kml", 
                  layer="CutOffRegion.kml")

plot(Outline)

class(Outline)
head(allLastLocs) ## WGS84
Outline@polygons  ##  WGS84

####  convert all Last Locations to a spatial points object

xy <- Breeding_DF[,c("coords.x1","coords.x2")]

Sp_InIsrael_Dataset <- SpatialPointsDataFrame(coords = xy, data = Breeding_DF,
                                              proj4string = CRS(projection(Breeding_MS)))

#pts_in<-Sp_allLastLoc_AllID[!is.na(over(Sp_allLastLoc_AllID,Outline)),]
Breed_INIsrael<-Sp_InIsrael_Dataset[complete.cases(over(Sp_InIsrael_Dataset, Outline)), ]
plot(Outline)
plot(Breed_INIsrael, col="red", add=TRUE)

##now only keeping the rows with data from within Israel
head(Breed_INIsrael)

#MoveStackDataset2021[[-which(namesIndiv(MoveStackDataset2020)%in%all_INIsrael$ID)]]
Dataset_BreedInIsrael<-as.data.frame(Breed_INIsrael)##renaming only the InIsrael data of 2021 that falls within Israel as the dataset that will be used henceforth 

####Removing vultures that have fewer than 1/3rd of their duration recorded INSIDE ISRAEL cut-off region ######
library(plyr)
IdByDateLocs_Br<-plyr::ddply(Dataset_BreedInIsrael, .(Dataset_BreedInIsrael$trackId, Dataset_BreedInIsrael$DateOnly), nrow)
colnames(IdByDateLocs_Br)<-c("trackId","DateOnly","no_Relocs")
DaysRecorded_Br<-as.data.frame(table(IdByDateLocs_Br$trackId))
colnames(DaysRecorded_Br)<-c("trackId", "DaysRec")
hist(DaysRecorded_Br$DaysRec)
max(DaysRecorded_Br$DaysRec)*(1/3) #1/3 of 213 max days

plot(DaysRec~trackId, data= DaysRecorded_Br, las=3, cex.axis=0.5)

Vultures_min71Days_Br<-subset(DaysRecorded_Br, DaysRec>max(DaysRecorded_Br$DaysRec)*(1/3) &
                                trackId != "J53w" & trackId !="T59w" & trackId != "T61w")
Vultures_min71Days_Br$trackId<-factor(Vultures_min71Days_Br$trackId)


########## Subsetting Dataset to only include vultures that were recorded long enough ######
nrow(Dataset_BreedInIsrael)
length(unique(Dataset_BreedInIsrael$trackId))
length(unique(Dataset_BreedInIsrael$trackId))

Dataset_BrCoRoost_MinDays<-Dataset_BreedInIsrael[Dataset_BreedInIsrael$trackId %in% Vultures_min71Days_Br$trackId,]
Dataset_BrCoRoost_MinDays$trackId<-factor(Dataset_BrCoRoost_MinDays$trackId)
length(unique(Dataset_BrCoRoost_MinDays$trackId))

table(Dataset_BrCoRoost_MinDays$trackId)

#######################################################################
# use df2move to convert the data.frame into a moveStack
projection(MoveStackDatasetOhad_2021)
Movestacked_BrCoRoost<-df2move(Dataset_BrCoRoost_MinDays,
                              proj = projection(MoveStackDatasetOhad_2021), 
                              x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId",
                              data = Dataset_BrCoRoost_MinDays)


extent(Movestacked_BrCoRoost)

class(Movestacked_BrCoRoost)
str(Movestacked_BrCoRoost)
summary(Movestacked_BrCoRoost)
colnames(Dataset_BrCoRoost_MinDays)
unstacked_BrCoRoost<-split(Movestacked_BrCoRoost)

length(unstacked_BrCoRoost)

## a loop on tags. ###### #
for (indv in 1:length(unstacked_BrCoRoost) ){## loop on individuals, now in separate Move objects
  TagsMetaData$InitialPoints[indv]=  dim(unstacked_BrCoRoost[[indv]]@data)[1]  ##number of fixes for an individual
  
  TagsMetaData$name[indv]=          names(unstacked_BrCoRoost)[indv]
  plot(unstacked_BrCoRoost[[indv]],col='blue', type='b',main=paste("Indiv=",indv, ', name=',TagsMetaData$name[indv],sep=' '))#just simple plot of this individual's points
  #dim(unstacked_BrCoRoost[[indv]]@coords)
  
  ## removing unneeded columns
  VarsToRemove <- names(unstacked_BrCoRoost[[indv]]@data) %in% c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                                                                "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                                                                "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                                                                "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                                                                "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                                                                "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                                                                "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                                                                "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                                                                "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                                                                "argos_gdop","argos_error_radius","argos_calcul_freq","location_lat.1","location_long.1","timestamps","height_raw",
                                                                "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                                                                "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                                                                'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate");  
  
  
  unstacked_BrCoRoost[[indv]]@data=unstacked_BrCoRoost[[indv]]@data[!VarsToRemove]  ##all columns except above listed ones
  dim(unstacked_BrCoRoost[[indv]]@data)#checking if colunms removed
  
  ## filtering: choosing indices to keep for this individual
  
  indx=1:dim(unstacked_BrCoRoost[[indv]]@data)[1] #starting with all points a
  if(sum(unstacked_BrCoRoost[[indv]]@data$heading <= 360,na.rm=T)){#do i have heading data or this one?
    
    indx=intersect(indx,which(unstacked_BrCoRoost[[indv]]@data$heading <= 360))} #if yes, now index include only points with realistic heading 
  
  if(sum(unstacked_BrCoRoost[[indv]]@data$ground_speed<=MaxSpeedPermited,na.rm=T)){#below threshhold speed?
    indx=intersect(indx,which(unstacked_BrCoRoost[[indv]]@data$ground_speed<=120))}
  
  if(sum(unstacked_BrCoRoost[[indv]]@data$gps_satellite_count>=3,na.rm=T)){#enough satellite numbers?
    indx=intersect(indx,which(unstacked_BrCoRoost[[indv]]@data$gps_satellite_count>=3))}
  
  
  ## subsetting the different slots of this move object
  print(paste("indiv",indv,"name",TagsMetaData$name[indv],'. I throw out', TagsMetaData$InitialPoints[indv]-length(indx), 'points, out of',TagsMetaData$InitialPoints[indv]))
  TagsMetaData$PercentThrown[indv]=(TagsMetaData$InitialPoints[indv]-length(indx))/TagsMetaData$InitialPoints[indv]
  
  unstacked_BrCoRoost[[indv]]@timestamps=unstacked_BrCoRoost[[indv]]@timestamps[indx]
  unstacked_BrCoRoost[[indv]]@sensor=unstacked_BrCoRoost[[indv]]@sensor[indx]
  unstacked_BrCoRoost[[indv]]@data=unstacked_BrCoRoost[[indv]]@data[indx,]
  if(dim(unstacked_BrCoRoost[[indv]]@data)[1]>1){ ##Nitika = to avoid stalling the loop if not enough data locations
    unstacked_BrCoRoost[[indv]]@coords=unstacked_BrCoRoost[[indv]]@coords[indx,]
    unstacked_BrCoRoost[[indv]]@bbox[1,]=range(unstacked_BrCoRoost[[indv]]@coords[,1]);unstacked_BrCoRoost[[indv]]@bbox[2,]=range(unstacked_BrCoRoost[[indv]]@coords[,2])
    
    
    ## collecting metadata and plotting fitered track: 
    TagsMetaData$N_locs[indv]=  dim(unstacked_BrCoRoost[[indv]]@data)[1]
    TagsMetaData$TrackDurationDays[indv]=  length(unique(as.Date(as.character(unstacked_BrCoRoost[[indv]]@data$timestamp))))
    TagsMetaData$FirstDay[indv]=as.character(min(as.Date(as.character(unstacked_BrCoRoost[[indv]]@data$timestamp))));
    TagsMetaData$LastDay[indv]= as.character(max(as.Date(as.character(unstacked_BrCoRoost[[indv]]@data$timestamp))));
    TagsMetaData$DaysBetweenStartEnd[indv]=as.Date(TagsMetaData$LastDay[indv])-as.Date(TagsMetaData$FirstDay[indv]);
    lines(unstacked_BrCoRoost[[indv]],col='red')
    #plot(unstacked_BrCoRoost[[indv]], type="o", col=3, lwd=2, pch=20, xlab="location_long", ylab="location_lat")
    
    ##logging metadata
    
    head(timeLag(unstacked_BrCoRoost[[indv]], units="mins"))
    head(timestamps(unstacked_BrCoRoost[[indv]]))
  }
}#loop on individuals



#####################Import roost polygons  ####################
setwd("C:/XXXXX/KML_Files")
FileName='AllRoostPolygons.kml'
LayerName='Roosting'
Roosts = readOGR(dsn=FileName, layer=LayerName)
head(Roosts)
summary(Roosts)

plot(Roosts)

allRoostCoods = data.frame()
for (i in 1:length(Roosts)){
  roost1 = as.data.frame(cbind((as.character(Roosts@data[i,1])),Roosts@polygons[[i]]@Polygons[[1]]@coords))
  allRoostCoods = rbind(allRoostCoods, roost1)
}
colnames(allRoostCoods)<-c("Roost","x","y") 
length(Roosts)
RoostAreas = data.frame() ##Calculating roost polygon areas
for (i in 1:length(Roosts)){
  RoostAreas = rbind(RoostAreas,cbind(as.character(Roosts@data$Name[i]),Roosts@polygons[[i]]@Polygons[[1]]@area))
}

colnames(RoostAreas) = c("Roost","area")
library(dplyr)
RoostAreas$area=as.numeric(as.character(RoostAreas$area))*1000000
mean(RoostAreas$area)
sd(RoostAreas$area)
NewRoosts = as.data.frame(RoostAreas%>%arrange(desc(as.numeric(area))))
hist(NewRoosts$area)
nrow(allRoostCoods)
nrow(NewRoosts)

allPolysArea = merge(allRoostCoods,NewRoosts, by = "Roost")
head(allPolysArea)
unique(allPolysArea$Roost)
length(unique(allPolysArea$Roost))
length(unique(RoostAreas$Roost))
head(allPolysArea) ##it contains polygon boundary coordinates for each roost as rbind


############   Making coordinate systems of polygon consistent ###########
#################checking projections:##########

###for polygon coordinates' projection transformation
allPolysList<-allPolysArea[,c(1:3)]
allPolysList$x<-as.numeric(as.character(allPolysList$x))
allPolysList$y<-as.numeric(as.character(allPolysList$y))


BackStacked2021=moveStack(unstacked_BrCoRoost)
#preparing a dataframe for mapping:
Dataset2021=as.data.frame(BackStacked2021) #converting to a dataset with all indis and their rows

##for last locations of vultures on each night:
projection(BackStacked2021)#this was the original data projection from movebank
allPolysListF_wgs=allPolysList;
coordinates(allPolysListF_wgs)<-~x+y
proj4string(allPolysListF_wgs)<-CRS(projection(BackStacked2021))
#allPolysList last location projection is the same as BackStacked2021 i.e. WGS84

##converting to UTM

utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south
utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north

## converting to UTM36North
allPolysListF_utmN_Poly <- spTransform(allPolysListF_wgs, CRS(utmN))
head(coordinates(allPolysListF_utmN_Poly))#now the lat long are in metric
# just plotting the new dataset to see the locations look fine: plot(Dataset2021_utmN,col='blue')

## appending the new coordinates in easting northing- for calculating distance UTM locally
allPolysList$Easting_poly=coordinates(allPolysListF_utmN_Poly)[,1]
allPolysList$Northing_poly=coordinates(allPolysListF_utmN_Poly)[,2]

##################################  CREATING A BUFFER OF HALF A KM AROUND ROOST POLYGONS IN UTM ###########

roostsUTM=allPolysList[,c(1,4,5)]
head(roostsUTM)
summary(roostsUTM)
##pivot wider to wrangle data from long format to wider format 
#so each row is a roost ID and column in front of it are all polygon coordinates
library(dplyr)
library(tidyr)

roostsUTM_wide = roostsUTM %>% group_by(Roost) %>%
  dplyr::mutate(time=row_number()) %>%
  pivot_wider(names_from=time, values_from = c(Easting_poly, Northing_poly), names_prefix = "coord") %>%
  dplyr::select(Roost, names(.)[-1][order(readr::parse_number(names(.)[-1]))]) ##for keeping the easting northing columns alternating in order
roostsUTM_wide
head(as.matrix(roostsUTM_wide))
levels(roostsUTM_wide$Roost)
class(roostsUTM_wide)
WGS84 = projection(BackStacked2021)
roostsUTM_wide= as.data.frame(roostsUTM_wide)
head(roostsUTM_wide)
roost_coords=as.matrix(roostsUTM_wide[,-1])
ID =(as.character(roostsUTM_wide[,1]))
as.character(ID)
nrow(roost_coords)
##Projection in UTM:
utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south, but most points are in the north
utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(roost_coords, row(roost_coords)), ID),proj4string=CRS(utmN))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

plot(polys.df, col=rainbow(50, alpha=0.5))

###plotting two flight paths

date(Dataset_BrCoRoost_MinDays$timestamp)
TwoVultures<-subset(Dataset_BrCoRoost_MinDays, 
                    #(trackId =="J17w"|trackId=="J34w") & 
                    (#trackId =="A00w"|
                      trackId=="A15w"|trackId=="J12w"| #CoFlying
                        trackId == "T17w"| trackId =="J17w") & #CoRoosting
                    #date(Dataset_BrCoRoost_MinDays$timestamp)== "2021-01-29")
                    #trackId=="A00w"|
                    #trackId =="Y27b" & 
                    date(Dataset_BrCoRoost_MinDays$timestamp)== "2021-04-26")
unique(subset(Dataset_BrCoRoost_MinDays, date(Dataset_BrCoRoost_MinDays$timestamp)== "2021-02-27")$trackId)
unique(subset(Dataset_BrCoRoost_MinDays, date(Dataset_BrCoRoost_MinDays$timestamp)== "2021-01-26")$timestamp)

nrow(TwoVultures)
mapObj <- get_map(bbox(extent(min(Dataset_BrCoRoost_MinDays$coords.x1)-0.05, 
                               max(Dataset_BrCoRoost_MinDays$coords.x1)-0.5, 
                               min(Dataset_BrCoRoost_MinDays$coords.x2)-3.75, 
                               max(Dataset_BrCoRoost_MinDays$coords.x2)-0.075 )*1.1), source="google", zoom=11,
                   style = 'element:labels|visibility:off',color= "bw")


mapObj2 <- get_map(bbox(extent(min(TwoVultures$coords.x1)-0.035, 
                               max(TwoVultures$coords.x1)+0.035, 
                               min(TwoVultures$coords.x2)-0.035, 
                               max(TwoVultures$coords.x2)+0.0035 )*1.1), source="stamen", zoom=10,
                   #style = 'feature:administrative.country|element:labels|visibility:off',
                   color= "bw",#style = c('element:labels|visibility:off', 'feature:road.local|visibility:off'),
                   #color = "bw",
                   #style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off", 
                   maptype = "toner-background",
                   style=feature:all|element:labels|visibility:off
                   )
                   #style = 'element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.country%7Celement:geometry.stroke%7Ccolor:0x000000%7Cvisibility:on&style=feature:administrative.land_parcel%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Cvisibility:off&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=480x360',color= "bw")





unique(TwoVultures$trackId)
#unique(TwoVultures$timestamp)
as.data.frame(table(date(Dataset_BrCoRoost_MinDays$timestamp)))

as.data.frame(table(date(Dataset_BrCoRoost_MinDays$timestamp)))
nrow(TwoVultures)
length(TwoVultures$coords.x1)
TwoVultures$trackId<-factor(TwoVultures$trackId)

class(allRoostCoods$y)
allRoostCoods$x<-as.numeric(as.character(allRoostCoods$x))
allRoostCoods$y<-as.numeric(as.character(allRoostCoods$y))

meanRoosts<-plyr::ddply(allRoostCoods, .(Roost), summarize,  meanX=mean(x), meanY=mean(y))
AllRoostsWCentres<-left_join(allRoostCoods,meanRoosts, by = "Roost")
OneRoost<-subset(AllRoostsWCentres, Roost == "HemarWa")
OneMeanRoost<-subset(meanRoosts, Roost == "HemarWa")

TwoRoost<-subset(AllRoostsWCentres, Roost == "SmMachtech")
TwoMeanRoost<-subset(meanRoosts, Roost == "SmMachtech")

BothRoosts<-as.data.frame(rbind(OneRoost, TwoRoost))
BothMeanRoosts<-as.data.frame(rbind(OneMeanRoost,TwoMeanRoost))

tail(TwoVultures)
class(TwoVultures)

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/FiguresForVultureManuscript1/Trajectories")
write.csv(TwoVultures, "CoFlighCoRoostingTracks.csv")

Map=ggmap(mapObj2#,base_layer = base_map
          ) +
#Map2=ggplot(Israel, aes(x=x, y=y)
geom_path(data=TwoVultures,
            aes(x=coords.x1, y=coords.x2,
                color = trackId),
            size=1.25,alpha = 0.85, show.legend = TRUE) +
  scale_color_manual(values = c("A15w" = "dodgerblue",
                                "J12w" ="deepskyblue4",
                                "T17w" = "mediumseagreen",
                                "J17w" = "chartreuse3"
                                )) +
  geom_polygon(data = OneRoost, aes(x=x,y=y, group = Roost), color = "forestgreen", fill = "NA",size=2
               )+
  theme_void()+
  geom_text(aes(x = meanX, y = meanY, label = Roost),
            data= OneMeanRoost,
            alpha = 1,
            color = "black", size=3)+
  #geom_text(aes(label = Roost, x = meanX, y = meanY)) + #add labels at centroids
  ggtitle('CoFlight Between Roosts 2021 on Apr 26, 2021');print(Map)

plot(Map)


#dev.off()
Roosts@polygons[[2]]@Polygons[[1]]@coords
#This is the correct roost shapefile
#writeOGR(polys.df, dsn="C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/201001_RoostAffiliation/RoostPolygons.kml", layer="id", driver="KML")


library(rgeos)
##add buffer to spatialPolygonDataFrame in UTM projection:
plot(gBuffer(polys.df, width= roostbuffer, byid = TRUE), border='blue', add= T)

RoostUTM_buff=gBuffer(polys.df, width= roostbuffer, byid = TRUE)
class(RoostUTM_buff)
class(polys.df)
RoostUTM_buff@polygons
polys.df@polygons[[59]]@Polygons[[1]]@area
RoostUTM_buff@polygons@Polygons[[1]]@coords ##increased in the buffered polygon

class(RoostUTM_buff)

##convert spatialpoygonsdataframe to dataframe:
library(sp)
library(FRK)
Orig_RoostUTM_df = SpatialPolygonsDataFrame_to_df(polys.df, vars = names(RoostUTM_buff))

RoostUTM_df = SpatialPolygonsDataFrame_to_df(RoostUTM_buff, vars = names(RoostUTM_buff))



###########################################################################################################
#         plot all last locations for each day for all vultures to see where they slept - ROOST
###########################################################################################################

allLastLoc_1ID<-list()

for (indv in 1:length(unstacked_BrCoRoost)){## loop on individuals, now for plotting
  
  dt=as.POSIXct(unstacked_BrCoRoost[[indv]]@data$timestamp, format="%m/%d/%Y %H:%M") 
  lastTimestamp=lapply( split( dt, 
                               format(dt,"%m-%d") ), function(d) as.POSIXct(d[which.max(d)] ) )
 
  ##list of dataframes containing each vulture's last location for each day throughout the year 2021
  LastLocs=unstacked_BrCoRoost[[indv]]@data[,c("location_lat","location_long","timestamp")][unstacked_BrCoRoost[[indv]]@data$timestamp %in% lastTimestamp,]
  LastLocs$ID=rep(names(unstacked_BrCoRoost)[indv], nrow(LastLocs))
  LastLocs$date=date(LastLocs$timestamp)
  
  allLastLoc_1ID=c(allLastLoc_1ID, list(LastLocs))
  
}
#converting all list to dataframe
allLastLocs<-do.call(rbind.data.frame, allLastLoc_1ID)
nrow(allLastLocs)

##############    Remove last locations that fall outside Israel cut-off boundary##########

setwd("C:/XXXXX/KML_Files")
FileName='CutOffRegion.kml'
LayerName='Regional_polygon'
Outline = readOGR(dsn="C:/XXXXX/KML_Files/CutOffRegion.kml", 
                  layer="CutOffRegion.kml")
plot(Outline)
class(Outline)
head(allLastLocs) ## WGS84
Outline@polygons  ##  WGS84

####  convert all Last Locations to a spatial points object
xy <- allLastLocs[,c(2,1)]
Sp_allLastLoc_AllID <- SpatialPointsDataFrame(coords = xy, data = allLastLocs,
                                              proj4string = CRS(projection(BackStacked2021)))
allLastLoc_IN<-Sp_allLastLoc_AllID[complete.cases(over(Sp_allLastLoc_AllID, Outline)), ]
plot(Outline)
plot(allLastLoc_IN, col="red", add=TRUE)
allLastLoc_AllID = as.data.frame(allLastLoc_IN)[,c(1:5)]

#####################FIRST LOCATION IN THE MORNINGS FOR EACH VULTURE#################
allFirstLoc_1ID<-list()

for (indv in 1:length(unstacked_BrCoRoost)){## loop on individuals, now for plotting
  
  dt=as.POSIXct(unstacked_BrCoRoost[[indv]]@data$timestamp, format="%m/%d/%Y %H:%M") 
  FirstTimestamp=lapply( split( dt, 
                                format(dt,"%m-%d") ), function(d) as.POSIXct(d[which.min(d)] ) )
  
 ##list of dataframes containing each vulture's First location for each day throughout the year 2021
  FirstLocs=unstacked_BrCoRoost[[indv]]@data[,c("location_lat","location_long","timestamp")][unstacked_BrCoRoost[[indv]]@data$timestamp %in% FirstTimestamp,]
  FirstLocs$ID=rep(names(unstacked_BrCoRoost)[indv], nrow(FirstLocs))
  FirstLocs$date=date(FirstLocs$timestamp)
  allFirstLoc_1ID=c(allFirstLoc_1ID, list(FirstLocs))
  }
#converting list to dataframe
allFirstLocs<-do.call(rbind.data.frame, allFirstLoc_1ID)
####  convert all First Locations to a spatial points object
xy1 <- allFirstLocs[,c(2,1)]
Sp_allFirstLoc_AllID <- SpatialPointsDataFrame(coords = xy1, data = allFirstLocs,
                                               proj4string = CRS(projection(BackStacked2021)))
allFirstLoc_IN<-Sp_allFirstLoc_AllID[complete.cases(over(Sp_allFirstLoc_AllID, Outline)), ]
plot(allFirstLoc_IN, col="green", add=TRUE)
allFirstLoc_AllID = as.data.frame(allFirstLoc_IN)[,c(1:5)]

############   Making coordinate systems of polygon consistent ###########
#################checking projections:##########

###for polygon coordinates' projection transformation
utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south, but most points are in the north
utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north

##for last locations of vultures on each night:
#this was the original data projection from movebank
RoostUTM_df_utm=RoostUTM_df;
coordinates(RoostUTM_df_utm)<-~x+y
proj4string(RoostUTM_df_utm)<-CRS(projection(utmN))
#RoostUTM_df last location projection is the same as BackStacked2021 i.e. WGS84

#################### plot all roosting sites ##############################

## in the WGS84 projection:
colnames(RoostUTM_df)<-c("x","y","Roost")
Rplots = ggplot(data = RoostUTM_df, aes(x = x, y = y)) + geom_polygon(aes(fill = Roost, group = Roost), color=NA) + theme_classic()+
  theme(legend.title = element_blank())+ theme(legend.position = "none") #remove legends
Rplots


## in UTM projection

UTMPolys = ggplot(data = RoostUTM_df, aes(x = x, y = y)) + geom_polygon(aes(fill = Roost, group = Roost), color=NA) + theme_classic()+
  theme(legend.title = element_blank())+ theme(legend.position = "none") #remove legends
##for last locations of vultures on each night:
#this was the original data projection from movebank
Orig_RoostUTM_df_utm=Orig_RoostUTM_df;
coordinates(Orig_RoostUTM_df_utm)<-~X1+X2
proj4string(Orig_RoostUTM_df_utm)<-CRS(projection(utmN))
#Orig_RoostUTM_df last location projection is the same as BackStacked2021 i.e. WGS84

#################### plot all roosting sites ##############################

## in the WGS84 projection:
colnames(Orig_RoostUTM_df)<-c("x","y","Roost")
Rplots = ggplot(data = Orig_RoostUTM_df, aes(x = x, y = y)) + geom_polygon(aes(fill = Roost, group = Roost), color=NA) + theme_classic()+
  theme(legend.title = element_blank())+ theme(legend.position = "none") #remove legends
Rplots

## in UTM projection
UTMPolys = ggplot(data = Orig_RoostUTM_df, aes(x = x, y = y)) + geom_polygon(aes(fill = Roost, group = Roost), color=NA) + theme_classic()+
  theme(legend.title = element_blank())+ theme(legend.position = "none") #remove legends

UTMPolys
##########    Selecting each date and assigning vulture locations to a roost  ################
head(allLastLoc_AllID) ##all last locations of all vultures recorded on each date
length(unique(allLastLoc_AllID$date))  
table(allLastLoc_AllID$ID)
VulturesInOrigRoost  = list()
for (D in 1:length(unique(allLastLoc_AllID$date))){
  EachDate=subset(allLastLoc_AllID, date == unique(allLastLoc_AllID$date)[D]) ##all vultures recorded on the same night
  ###### keeping vultures' last location points in consistent projections #####
  ##for last locations of vultures on each night:
  EachDateF_wgs=EachDate;
  coordinates(EachDateF_wgs)<-~location_long+location_lat
  proj4string(EachDateF_wgs)<-CRS(projection(BackStacked2021))
  #EachDate last location projection is the same as BackStacked2021 i.e. WGS84
  utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south, but most points are in the north
  utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north
  
  ## converting to UTM36North, but note that not all points are within, just the majority
  EachDateF_utmN_LastPts <- spTransform(EachDateF_wgs, CRS(utmN))
  head(coordinates(EachDateF_utmN_LastPts))#now the lat long are in metric
  # just plotting the new dataset to see the locations look fine: plot(Dataset2021_utmN,col='blue')
  
  ## appending the new coordinates in easting northing- for calculating distance UTM locally
  EachDate$Easting_pts=coordinates(EachDateF_utmN_LastPts)[,1]
  EachDate$Northing_pts=coordinates(EachDateF_utmN_LastPts)[,2]
  ### checking if the last locs of different vultures were located in various roost polygons
  
  ##For point in polygon:
   #  polygons
  localpolydf <- data.frame(
    xR = Orig_RoostUTM_df$x, #WGS84
    yR = Orig_RoostUTM_df$y,
    Roost = Orig_RoostUTM_df$Roost)
  #  points
  offsetdf_nyt <- data.frame(
    xP = EachDate$Easting_pts,#UTM
    yP = EachDate$Northing_pts,
    ID = EachDate$ID)
  # Sample plot
  ggplot() + 
    geom_polygon(aes(xR, yR, group = Roost), 
                 localpolydf, fill = NA, colour = "black") +
    geom_point(aes(xP, yP,colour = ID), offsetdf_nyt)  + 
    ylim(27,35)+ 
    theme(legend.title = element_blank())+ theme(legend.position = "none") 
  ptm <- proc.time() # Start timer
  #  create lists
  offsetlist <- split(offsetdf_nyt, offsetdf_nyt$ID)
  polygonlist <- split(localpolydf, localpolydf$Roost)
  # lapply over each pt in offsetlist
  pts <- lapply(offsetlist, function(pt) {
    # lapply over each polygon in polygonlist
    ptpoly <- lapply(polygonlist, function(poly) {
      data.frame(
        Roost = poly$Roost[1],
        ptin = point.in.polygon(pt[1,1], pt[1,2], poly$xR, poly$yR))
    })
    ptpoly <- bind_rows(ptpoly) %>% filter(ptin != 0)
    if (nrow(ptpoly) == 0) return(data.frame(x = pt$x, y = pt$y, Roost = NA, ptin = NA))
    ptpoly$x = pt$x
    ptpoly$y = pt$y
    return(ptpoly[c("x", "y", "Roost", "ptin")])
  })
  pts_apply <- bind_rows(pts)
  VultureInRoost = left_join(pts_apply, offsetdf_nyt, by = c("x" = "xP", "y" = "yP"))
  proc.time() - ptm # end timer
  
  VulturesInOrigRoost = c(VulturesInOrigRoost, list(cbind(VultureInRoost, date = unique(allLastLoc_AllID$date)[D])))
}

length(unique(allLastLoc_AllID$date))
length(unique(allLastLoc_AllID$ID))
PerNightVulturesInOrigRoost = do.call(rbind.data.frame, VulturesInOrigRoost)

### so check for same point being assigned to multiple polygons through viewing duplicates
### find duplicated rows based on few columns
dupe = PerNightVulturesInOrigRoost[,c('x','y','ID','date')] # select columns to check duplicates
PerNightVulturesInOrigRoost[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]
head(allLastLoc_AllID)
dupe2 = allLastLoc_AllID[,c('location_long','location_lat','ID','date')] # select columns to check duplicates
allLastLoc_AllID[duplicated(dupe2) | duplicated(dupe2, fromLast=TRUE),]

#####Instead of randomly picking a polygon for a point that falls in more than one polygon, choose the one that is closer 
#but first remove the rows that are just EXACT duplicates instead of different roosts
if(nrow(PerNightVulturesInOrigRoost)>nrow(allLastLoc_AllID)){
  PerNightVulturesInOrigRoost = PerNightVulturesInOrigRoost[!duplicated(PerNightVulturesInOrigRoost),]
}
dupe3 = PerNightVulturesInOrigRoost[,c('x','y','ID','date')] # select columns to check duplicates
PerNightVulturesInOrigRoost[duplicated(dupe3) | duplicated(dupe3, fromLast=TRUE),]
nonDupe = anti_join(PerNightVulturesInOrigRoost, 
                    as.data.frame(PerNightVulturesInOrigRoost[duplicated(dupe3) | duplicated(dupe3, fromLast=TRUE),]))
nrow(PerNightVulturesInOrigRoost)
## In case of a tie: select the polygon that is closer to the point
library(rgeos)
reps =PerNightVulturesInOrigRoost[duplicated(dupe3) | duplicated(dupe3, fromLast=TRUE),]
pts =reps;
coordinates(pts)<-~x+y
proj4string(pts)<-CRS(projection(utmN))
p = Orig_RoostUTM_df_utm ## spatialPolygonsDataframe in UTM
##  First project data into a planar coordinate system (here UTM zone 32)
crs=utmN
pUTM <- spTransform(p, crs)
ptsUTM <- spTransform(pts, crs)
## Set up containers for results
n <- length(ptsUTM)
nearestCanton <- character(n)
distToNearestCanton <- numeric(n)
sortedPts_df<-data.frame()
## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearestCanton)) {
  gDists <- gDistance(ptsUTM[i,], pUTM, byid=TRUE)
  nearestCanton[i] <- pUTM$id[which.min(gDists)]
  distToNearestCanton[i] <- min(gDists)
  
  sortedPts_df = as.data.frame(rbind(sortedPts_df, 
                                     cbind(ptsCoords = ptsUTM[i,]@coords, nearestRoost = nearestCanton[i], 
                                           dist =distToNearestCanton[i])))
  reps[i,]$Roost <- nearestCanton[i] ### replacing Roost assigned to the one that is actually closest to the spatial point
  
}

if(nrow(reps)>1){
  reps = reps[!duplicated(reps),]
}
PerNightVulturesInOrigRoostPolygon = as.data.frame(rbind(nonDupe, reps))
PerNightVulturesInOrigRoostPolygon_Assigned = subset(PerNightVulturesInOrigRoostPolygon, ptin =="1")
nrow(PerNightVulturesInOrigRoostPolygon_Assigned)
nrow(allLastLoc_AllID)
pratishat = (nrow(PerNightVulturesInOrigRoostPolygon_Assigned)/nrow(allLastLoc_AllID))*100
pratishat
##############################################
######         BUFFERED ROOSTS   #####
#############################################
##########    Selecting each date and assigning vulture locations to a roost  ################
head(allLastLoc_AllID) ##all last locations of all vultures recorded on each date
length(unique(allLastLoc_AllID$date))  
VulturesInRoost  = list()

for (D in 1:length(unique(allLastLoc_AllID$date))){
  EachDate=subset(allLastLoc_AllID, date == unique(allLastLoc_AllID$date)[D]) ##all vultures recorded on the same night
  ###### keeping vultures' last location points in consistent projections #####
  ##for last locations of vultures on each night:
  EachDateF_wgs=EachDate;
  coordinates(EachDateF_wgs)<-~location_long+location_lat
  proj4string(EachDateF_wgs)<-CRS(projection(BackStacked2021))
  #EachDate last location projection is the same as BackStacked2021 i.e. WGS84
  utmS <- '+proj=utm +zone=36 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs' #south, but most points are in the north
  utmN <- '+proj=utm +zone=36        +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  #north
  EachDateF_utmN_LastPts <- spTransform(EachDateF_wgs, CRS(utmN))
  head(coordinates(EachDateF_utmN_LastPts))#now the lat long are in metric
  # just plotting the new dataset to see the locations look fine: plot(Dataset2021_utmN,col='blue')
  
  ## appending the new coordinates in easting northing- for calculating distance UTM locally
  EachDate$Easting_pts=coordinates(EachDateF_utmN_LastPts)[,1]
  EachDate$Northing_pts=coordinates(EachDateF_utmN_LastPts)[,2]
  
    ### checking if the last locs of different vultures were located in various roost polygons
  
  ##For point in polygon:------ #-------------------------------------------
  #  polygons
  localpolydf <- data.frame(
    xR = RoostUTM_df$x, #WGS84
    yR = RoostUTM_df$y,
    Roost = RoostUTM_df$Roost)
  #  points
  offsetdf_nyt <- data.frame(
    xP = EachDate$Easting_pts,#UTM
    yP = EachDate$Northing_pts,
    ID = EachDate$ID)
  # Sample plot
  ggplot() + 
    geom_polygon(aes(xR, yR, group = Roost), 
                 localpolydf, fill = NA, colour = "black") +
    geom_point(aes(xP, yP,colour = ID), offsetdf_nyt)  + 
    ylim(27,35)+ 
    theme(legend.title = element_blank())+ theme(legend.position = "none") 
  
  ptm <- proc.time() # Start timer
  #  create lists
  offsetlist <- split(offsetdf_nyt, offsetdf_nyt$ID)
  polygonlist <- split(localpolydf, localpolydf$Roost)
  # lapply over each pt in offsetlist
  pts <- lapply(offsetlist, function(pt) {
    # lapply over each polygon in polygonlist
    ptpoly <- lapply(polygonlist, function(poly) {
      data.frame(
        Roost = poly$Roost[1],
        ptin = point.in.polygon(pt[1,1], pt[1,2], poly$xR, poly$yR))
    })
    ptpoly <- bind_rows(ptpoly) %>% filter(ptin != 0)
    if (nrow(ptpoly) == 0) return(data.frame(x = pt$x, y = pt$y, Roost = NA, ptin = NA))
    ptpoly$x = pt$x
    ptpoly$y = pt$y
    return(ptpoly[c("x", "y", "Roost", "ptin")])
  })
  pts_apply <- bind_rows(pts)
  VultureInRoost = left_join(pts_apply, offsetdf_nyt, by = c("x" = "xP", "y" = "yP"))
  proc.time() - ptm # end timer
  
  VulturesInRoost = c(VulturesInRoost, list(cbind(VultureInRoost, date = unique(allLastLoc_AllID$date)[D])))
}



UTMPolys +
  geom_point(aes(xP, yP,colour = ID), offsetdf)  + 
  ylim(27,35)+ 
  theme(legend.title = element_blank())+ theme(legend.position = "none")

length(unique(allLastLoc_AllID$date))
length(unique(allLastLoc_AllID$ID))
subset(RoostUTM_buff, id=="GolhanWad")
PerNightVulturesInRoost = do.call(rbind.data.frame, VulturesInRoost)

### check for same point being assigned to multiple polygons through viewing duplicates
### find duplicated rows based on few columns
dupe = PerNightVulturesInRoost[,c('x','y','ID','date')] # select columns to check duplicates
PerNightVulturesInRoost[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]

dupe2 = allLastLoc_AllID[,c('location_long','location_lat','ID','date')] # select columns to check duplicates
allLastLoc_AllID[duplicated(dupe2) | duplicated(dupe2, fromLast=TRUE),]
#####Instead of randomly picking a polygon for a point that falls in more than one polygon, choose the one that is closer 
#but first remove the rows that are just EXACT duplicates instead of different roosts
if(nrow(PerNightVulturesInRoost)>nrow(allLastLoc_AllID)){
  PerNightVulturesInRoost = PerNightVulturesInRoost[!duplicated(PerNightVulturesInRoost),]
}
nrow(PerNightVulturesInRoost)
nrow(allLastLoc_AllID)
dupe3 = PerNightVulturesInRoost[,c('x','y','ID','date')] # select columns to check duplicates
PerNightVulturesInRoost[duplicated(dupe3) | duplicated(dupe3, fromLast=TRUE),]
nonDupe = anti_join(PerNightVulturesInRoost, 
                    as.data.frame(PerNightVulturesInRoost[duplicated(dupe3) | duplicated(dupe3, fromLast=TRUE),]))
nrow(PerNightVulturesInRoost)
## In case of a tie: select the polygon that is closer to the point
library(rgeos)
reps =PerNightVulturesInRoost[duplicated(dupe3) | duplicated(dupe3, fromLast=TRUE),]
pts =reps;
coordinates(pts)<-~x+y
proj4string(pts)<-CRS(projection(utmN))
p = RoostUTM_df_utm ## spatialPolygonsDataframe in UTM
##  First project data into a planar coordinate system (here UTM zone 32)
crs=utmN
pUTM <- spTransform(p, crs)
ptsUTM <- spTransform(pts, crs)
## Set up containers for results
n <- length(ptsUTM)
nearestCanton <- character(n)
distToNearestCanton <- numeric(n)
sortedPts_df<-data.frame()
## For each point, find name of nearest polygon (in this case, cantons)
for (i in seq_along(nearestCanton)) {
  gDists <- gDistance(ptsUTM[i,], pUTM, byid=TRUE)
  nearestCanton[i] <- pUTM$id[which.min(gDists)]
  distToNearestCanton[i] <- min(gDists)
  sortedPts_df = as.data.frame(rbind(sortedPts_df, 
                                     cbind(ptsCoords = ptsUTM[i,]@coords, nearestRoost = nearestCanton[i], 
                                           dist =distToNearestCanton[i])))
  reps[i,]$Roost <- nearestCanton[i] ### replacing Roost assigned to the one that is actually closest to the spatial point
}
if(nrow(reps)>1){
  reps = reps[!duplicated(reps),]
}
PerNightVulturesInRoostPolygon = as.data.frame(rbind(nonDupe, reps))
nrow(subset(PerNightVulturesInOrigRoost, ptin == "1")) ## night points assigned in actual roosts 
nrow(subset(PerNightVulturesInRoostPolygon, ptin == "1")) ## night points assigned in buffered roosts 
pratishat
Buff_pratishat = nrow(subset(PerNightVulturesInRoostPolygon, ptin == "1"))/nrow(allLastLoc_AllID)
Buff_pratishat*100
############################    CREATING FIRST LOCATION IN THE MORNINGS FOR EACH VULTURE    #################
#only selecting the points that were unassigned from last night's last location
NotAssigned <-  PerNightVulturesInRoostPolygon[is.na(PerNightVulturesInRoostPolygon$ptin),]
nrow(NotAssigned)
table(NotAssigned$ID)
#######Plot non-assigned points after night assignments with and without buffer #####
plot(NotAssigned$x,NotAssigned$y, col="red", add=TRUE)
UTMPolys +
  geom_point(aes(NotAssigned$x, NotAssigned$y),colour = "black", NotAssigned)  + 
  #ylim(27,35)+ 
  theme(legend.title = element_blank())+ theme(legend.position = "none")
NotAssigned$MornDate = NotAssigned$date + 1
head(allFirstLoc_AllID)
colnames(allFirstLoc_AllID)[5] = "MornDate"
## We will match vulture A to the morning after the night it wasn't located to sleep in the roost
head(merge(NotAssigned, allFirstLoc_AllID, by=c("ID","MornDate")))
LastNightFirstMornData = merge(NotAssigned, allFirstLoc_AllID, by=c("ID","MornDate"))[,-(c(5,6))]
head(LastNightFirstMornData)
colnames(LastNightFirstMornData)<-c("ID", "MornDate","Long_night","Lat_night","night_date",
                                    "WGS_Lat_morn","WGS_Long_morn","timestamp")
head(LastNightFirstMornData)
####converting morning WGS lat long to UTM lat long:
WGS_LastNightFirstMornData=LastNightFirstMornData;
coordinates(WGS_LastNightFirstMornData)<-~WGS_Long_morn+WGS_Lat_morn
proj4string(WGS_LastNightFirstMornData)<-CRS(projection(BackStacked2021))
## converting to UTM36North, but note that not all points are within, just the majority
UTM_LastNightFirstMornData <- spTransform(WGS_LastNightFirstMornData, CRS(utmN))
head(coordinates(UTM_LastNightFirstMornData))#now the lat long are in metric
# just plotting the new dataset to see the locations look fine: plot(Dataset2021_utmN,col='blue')

## appending the new coordinates in easting northing- for calculating distance UTM locally
LastNightFirstMornData$Long_morn=coordinates(UTM_LastNightFirstMornData)[,1]
LastNightFirstMornData$Lat_morn=coordinates(UTM_LastNightFirstMornData)[,2]
nrow(LastNightFirstMornData)

#################  assigning non assigned vultures based on their morning after#############################################################################
FirstInRoost  = list()

for (M in 1:length(unique(LastNightFirstMornData$MornDate))){
  
  EachMorn=subset(LastNightFirstMornData, MornDate == unique(LastNightFirstMornData$MornDate)[M]) ##all vultures recorded on the same night
  
  ###### keeping vultures' last location points in consistent projections #####
  ##for last locations of vultures on each night:
  ## appending the new coordinates in easting northing- for calculating distance UTM locally
  EachMorn$Easting_pts=EachMorn$Long_morn
  EachMorn$Northing_pts=EachMorn$Lat_morn
  ### checking if the last locs of different vultures were located in various roost polygons
  ##checking multiple points in multiple polygons
  ##For point in polygon:
  #  polygons
  localpolydf <- data.frame(
    xR = RoostUTM_df$Easting_poly, #UTM
    yR = RoostUTM_df$Northing_poly,
    Roost = RoostUTM_df$Roost)
  #  points
  offsetdf <- data.frame(
    xP = EachMorn$Easting_pts,#UTM
    yP = EachMorn$Northing_pts,
    ID = EachMorn$ID)
  # Sample plot
  ggplot() + 
    geom_polygon(aes(xR, yR, group = Roost), 
                 localpolydf, fill = NA, colour = "black") +
    geom_point(aes(xP, yP,colour = ID), offsetdf)  + 
    ylim(27,35)+ 
    theme(legend.title = element_blank())+ theme(legend.position = "none") 
  ptm <- proc.time() # Start timer
  #  create lists
  offsetdf$ID=factor(offsetdf$ID)
  offsetlist <- split(offsetdf, offsetdf$ID)
  polygonlist <- split(localpolydf, localpolydf$Roost)
  # lapply over each pt in offsetlist
  pts <- lapply(offsetlist, function(pt) {
    # lapply over each polygon in polygonlist
    ptpoly <- lapply(polygonlist, function(poly) {
      data.frame(
        Roost = poly$Roost[1],
        ptin = point.in.polygon(pt[1,1], pt[1,2], poly$xR, poly$yR))
    })
    ptpoly <- bind_rows(ptpoly) %>% filter(ptin != 0)
    if (nrow(ptpoly) == 0) return(data.frame(x = pt$x, y = pt$y, Roost = NA, ptin = NA))
    ptpoly$x = pt$x
    ptpoly$y = pt$y
    return(ptpoly[c("x", "y", "Roost", "ptin")])
  })
  pts_apply <- bind_rows(pts)
  VultureInMornRoost = left_join(pts_apply, offsetdf, by = c("x" = "xP", "y" = "yP"))
  proc.time() - ptm # end timer
  FirstInRoost = c(FirstInRoost, list(cbind(VultureInMornRoost, MornDate = unique(LastNightFirstMornData$MornDate)[M])))
}
PerMornFirstInRoostPolygon = do.call(rbind.data.frame, FirstInRoost)

##checking again for multiple assignments of same point or duplicated rows for same day
if(nrow(PerMornFirstInRoostPolygon)> nrow(LastNightFirstMornData)){
  
  PerMornFirstInRoostPolygon = PerMornFirstInRoostPolygon[!duplicated(PerMornFirstInRoostPolygon),] ## all columns are exactly same
  
}

############### if multiple polygons claim the same point:  ##################

if(nrow(PerMornFirstInRoostPolygon)> nrow(LastNightFirstMornData)){
  dupe_morn = PerMornFirstInRoostPolygon[,c('x','y','ID','MornDate')] # select columns to check duplicates
  PerMornFirstInRoostPolygon[duplicated(dupe_morn) | duplicated(dupe_morn, fromLast=TRUE),]
  
  nonDupe = anti_join(PerMornFirstInRoostPolygon, 
                      as.data.frame(PerMornFirstInRoostPolygon[duplicated(dupe_morn) | duplicated(dupe_morn, fromLast=TRUE),]))
  
  nrow(PerMornFirstInRoostPolygon)
  ## In case of a tie: select the polygon that is closer to the point
  
  library(rgeos)
  reps_morn = PerMornFirstInRoostPolygon[duplicated(dupe_morn) | duplicated(dupe_morn, fromLast=TRUE),]
  
  pts = reps_morn;
  
  coordinates(pts)<-~x+y
  proj4string(pts)<-CRS(projection(utmN))
  p = RoostUTM_df_utm ## spatialPolygonsDataframe in UTM
  crs=utmN
  pUTM <- spTransform(p, crs)
  ptsUTM <- spTransform(pts, crs)
  
  ## Set up containers for results
  n <- length(ptsUTM)
  nearestCanton <- character(n)
  distToNearestCanton <- numeric(n)
  sortedPts_df<-data.frame()
  
  ## For each point, find name of nearest polygon (in this case, Belgian cantons)
  for (i in seq_along(nearestCanton)) {
    gDists <- gDistance(ptsUTM[i,], pUTM, byid=TRUE)
    nearestCanton[i] <- pUTM$id[which.min(gDists)]
    distToNearestCanton[i] <- min(gDists)
    
    sortedPts_df = as.data.frame(rbind(sortedPts_df, 
                                       cbind(ptsCoords = ptsUTM[i,]@coords, nearestRoost = nearestCanton[i], 
                                             dist =distToNearestCanton[i])))
    reps_morn[i,]$Roost <- nearestCanton[i] ### replacing Roost assigned to the one that is actually closest to the spatial point
    
  }
  
  if(nrow(reps_morn)>1){
    reps_morn = reps_morn[!duplicated(reps_morn),]
  }
  
  PerMornFirstInRoostPolygon = as.data.frame(rbind(nonDupe, reps_morn))
  
}

nrow(PerMornFirstInRoostPolygon)
nrow(LastNightFirstMornData)
nrow(allLastLoc_AllID)


## subset the morning locations that were successfully assigned to a roost
MornsAssigned = subset(PerMornFirstInRoostPolygon, ptin == "1")
nrow(MornsAssigned)

##############  done checking for multiple assignments and duplicates for Morning data #########
##attach to the earlier successfully assigned list of vultures from last location data
NightsAssigned = subset(PerNightVulturesInRoostPolygon, ptin == "1")
NightsAssigned$cat = rep("night", nrow(NightsAssigned))
##successful assignments
MornsAssigned$cat =rep("morn", nrow(MornsAssigned))
colnames(MornsAssigned)[6]="date"
MornNyt_success = rbind(NightsAssigned,MornsAssigned)
head(PerMornFirstInRoostPolygon)
colnames(PerMornFirstInRoostPolygon)<-c("Long_morn","Lat_morn","Roost","ptin","ID","MornDate")
###merging to keep timestamp
head(merge(LastNightFirstMornData, PerMornFirstInRoostPolygon, by=c("ID","MornDate","Long_morn","Lat_morn")))
LastNightFirstMornData_2 = merge(LastNightFirstMornData, PerMornFirstInRoostPolygon, by=c("ID","MornDate","Long_morn","Lat_morn"))[,-(c(5,6))]
head(LastNightFirstMornData_2)

##################################  If neither night nor morning, average location btwn night and morning ##############################################
## further subsetting the ones that were still not successfully assigned in night or morning to
##  take an average location 
NotAssigned2 <-  PerMornFirstInRoostPolygon[is.na(PerMornFirstInRoostPolygon$ptin),]
head(NotAssigned2)
head(LastNightFirstMornData)
colnames(NotAssigned2)<-c("Long_morn","Lat_morn","Roost","ptin","ID","MornDate")
## We will match vulture A to the morning after the night it wasn't located to sleep in the roost
head(merge(NotAssigned2, LastNightFirstMornData, by=c("ID","MornDate","Long_morn","Lat_morn")))
UnassignedB4Avg = merge(NotAssigned2, LastNightFirstMornData, by=c("ID","MornDate","Long_morn","Lat_morn"))
head(UnassignedB4Avg)
UnassignedB4Avg$AvgLong= 0.5*(UnassignedB4Avg$Long_night+UnassignedB4Avg$Long_morn)
UnassignedB4Avg$AvgLat= 0.5*(UnassignedB4Avg$Lat_night+UnassignedB4Avg$Lat_morn)
head(UnassignedB4Avg)
#################  assigning not assigned vultures based on their morning after#############################################################################
AvgInRoost  = list()
for (M in 1:length(unique(UnassignedB4Avg$MornDate))){
  EachMorn=subset(UnassignedB4Avg, MornDate == unique(UnassignedB4Avg$MornDate)[M]) ##all vultures recorded on the same night
  ###### keeping vultures' last location points in consistent projections #####
  ## appending the new coordinates in easting northing- for calculating distance UTM locally
  EachMorn$Easting_pts=EachMorn$AvgLong
  EachMorn$Northing_pts=EachMorn$AvgLat
  #  polygons
  localpolydf <- data.frame(
    xR = RoostUTM_df$x, #WGS84
    yR = RoostUTM_df$y,
    Roost = RoostUTM_df$Roost)
  #  points
  offsetdf <- data.frame(
    xP = EachMorn$Easting_pts,#UTM
    yP = EachMorn$Northing_pts,
    ID = EachMorn$ID)
  # Sample plot
  ggplot() + 
    geom_polygon(aes(xR, yR, group = Roost), 
                 localpolydf, fill = NA, colour = "black") +
    geom_point(aes(xP, yP,colour = ID), offsetdf)  + 
    ylim(27,35)+ 
    theme(legend.title = element_blank())+ theme(legend.position = "none") 
  
  ptm <- proc.time() # Start timer
  #  create lists
  offsetdf$ID=factor(offsetdf$ID)
  offsetlist <- split(offsetdf, offsetdf$ID)
  polygonlist <- split(localpolydf, localpolydf$Roost)
  # lapply over each pt in offsetlist
  pts <- lapply(offsetlist, function(pt) {
    # lapply over each polygon in polygonlist
    ptpoly <- lapply(polygonlist, function(poly) {
      data.frame(
        Roost = poly$Roost[1],
        ptin = point.in.polygon(pt[1,1], pt[1,2], poly$xR, poly$yR))
    })
    ptpoly <- bind_rows(ptpoly) %>% filter(ptin != 0)
    if (nrow(ptpoly) == 0) return(data.frame(x = pt$x, y = pt$y, Roost = NA, ptin = NA))
    ptpoly$x = pt$x
    ptpoly$y = pt$y
    return(ptpoly[c("x", "y", "Roost", "ptin")])
  })
  pts_apply <- bind_rows(pts)
  VultureInMornRoost = left_join(pts_apply, offsetdf, by = c("x" = "xP", "y" = "yP"))
  proc.time() - ptm # end timer
  
  AvgInRoost = c(AvgInRoost, list(cbind(VultureInMornRoost, MornDate = unique(UnassignedB4Avg$MornDate)[M])))
}


PerNMAvgInRoostPolygon = do.call(rbind.data.frame, AvgInRoost)
NotAssigned3 <-  PerNMAvgInRoostPolygon[is.na(PerNMAvgInRoostPolygon$ptin),]
UnAssignedVultures = as.data.frame(sort(table(NotAssigned3$ID), decreasing = TRUE))

###  binding successful assignments from night, day & avg using polygon buffers of 0.5km#########

##successful assignments
AvgAssigned = subset(PerNMAvgInRoostPolygon, ptin =="1")
AvgAssigned$cat =rep("avg", nrow(AvgAssigned))
colnames(MornNyt_success)
colnames(AvgAssigned)[6]<-"date"
MornNytAvg_success = rbind(MornNyt_success,AvgAssigned)
levels(MornNytAvg_success$Roost)
#########################   calculate distances of all remaining unassigned pts to all polygons #################################################
pts_non3 = NotAssigned3;
NotAssigned3_copy = NotAssigned3
coordinates(pts_non3)<-~x+y
proj4string(pts_non3)<-CRS(projection(utmN))
p = RoostUTM_df_utm ## spatialPolygonsDataframe in UTM
crs=utmN
pUTM <- spTransform(p, crs)
pts_non3UTM <- spTransform(pts_non3, crs)
## Set up containers for results
n <- length(pts_non3UTM)
nearestCanton <- character(n)
distToNearestCanton <- numeric(n)
AllsortedPts_df<-data.frame()
## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearestCanton)) {
  gDists <- sort(gDistance(pts_non3UTM[i,], pUTM, byid=TRUE), decreasing = FALSE)
  if (gDists[1]<=(gDists[2]/2)){
    nearestCanton[i] <- pUTM$id[which.min(gDists)]
    distToNearestCanton[i] <- min(gDists)
    
    AllsortedPts_df = as.data.frame(rbind(AllsortedPts_df, 
                                          cbind(pts_non3Coords = pts_non3UTM[i,]@coords, nearestRoost = nearestCanton[i], 
                                                dist =distToNearestCanton[i])))
    NotAssigned3_copy[i,]$Roost <- nearestCanton[i] ### replacing Roost assigned to the one that is actually closest to the spatial point
  }
}
FinalAssignment = NotAssigned3_copy[!is.na(NotAssigned3_copy$ptin),]
FinalUnassigned = NotAssigned3_copy[is.na(NotAssigned3_copy$ptin),]
AllSuccessfulAssignments = rbind(MornNytAvg_success, FinalAssignment)
############################################################################
#####Look for co-occurence for same night as well as following morning####
###merge so as to have timestamps alongside
head(allLastLoc_AllID)
head(NightsAssigned)
allNightsAssigned = merge(NightsAssigned, allLastLoc_AllID,by = c("ID","date"))
colnames(allNightsAssigned)[2]="night_date"
head(allNightsAssigned)
Nights = allNightsAssigned[,c("ID","Roost","timestamp","x","y","location_long","location_lat","cat","night_date")]
head(allFirstLoc_AllID)
head(MornsAssigned)
colnames(MornsAssigned)[6]<-"MornDate"
allMornsAssigned = merge(MornsAssigned, allFirstLoc_AllID,by = c("ID","MornDate") )
allMornsAssigned$night_date=allMornsAssigned$MornDate-1
head(allMornsAssigned)
Morns = allMornsAssigned[,c("ID","Roost","timestamp","x","y","location_long","location_lat","cat","night_date")]
head(UnassignedB4Avg)
table(UnassignedB4Avg$ID)
nrow(UnassignedB4Avg)
head(AvgAssigned)
colnames(AvgAssigned)[6]<-"MornDate"
allAvgAssigned = merge(AvgAssigned, UnassignedB4Avg, by = c("ID","MornDate"))
head(allAvgAssigned)
colnames(allAvgAssigned)
Avg = allAvgAssigned[,c("ID","Roost.x","timestamp","x","y","WGS_Long_morn","WGS_Lat_morn","cat","night_date")]
colnames(Avg)=c("ID","Roost","timestamp","x","y","location_long","location_lat","cat","night_date")
colnames(NotAssigned3_copy)[1:2]<-c("Long_morn","Lat_morn")
UnAssignedB4PolyComparison = merge(NotAssigned3_copy,UnassignedB4Avg,
                                   by=c("ID","MornDate","Long_morn","Lat_morn"), all.x=TRUE)
colnames(FinalAssignment)[1:2]<-c("x","y")
colnames(FinalAssignment)[6]<-"MornDate"
allFinAssigned = merge(FinalAssignment, UnAssignedB4PolyComparison, by = c("ID","MornDate"))
allFinAssigned$cat = rep("PolyComps", nrow(allFinAssigned))
table(UnAssignedB4PolyComparison$ID)
colnames(allFinAssigned)
Final = allFinAssigned[,c("ID","Roost.x","timestamp","x","y","WGS_Long_morn","WGS_Lat_morn","cat","night_date")]
colnames(Final)=c("ID","Roost","timestamp","x","y","location_long","location_lat","cat","night_date")

########All successfully assigned datapoints #######
df_allAssigned = as.data.frame(rbind(Nights,Morns,Avg, Final))
##now we want to see which vultures could've co-roosted because they were recorded by us on the same night
AllPresent = expand.grid.df(df_allAssigned,df_allAssigned)
head(AllPresent)
colnames(AllPresent)=c("ID1","Roost1","timestamp1","x1","y1","location_long1","location_lat1","cat1","night_date1",
                       "ID2","Roost2","timestamp2","x2","y2","location_long2","location_lat2","cat2","night_date2")

AllPossiblePairs = subset(AllPresent, ID1 != ID2 & night_date1 == night_date2) ##all non-self pairs that were recorded on the same night
sorted_AllPossiblePairs = get.data.frame(graph.data.frame(AllPossiblePairs[,c("ID1","ID2")], directed=FALSE),"edges") ##order of ID1 & ID2 shouldn't matter
colnames(sorted_AllPossiblePairs)<-c("ID1","ID2")
AllPossiblePairs$ID1 = sorted_AllPossiblePairs[,1]
AllPossiblePairs$ID2 = sorted_AllPossiblePairs[,2]
AllPossibleEdgelist = plyr::ddply(sorted_AllPossiblePairs,.(ID1, ID2),nrow)
colnames(AllPossibleEdgelist)[3]<-"wt"
##these are all possible opportunitites for vulture pairs to have co-roosted because they were recorded on the same nights by us
### could see for each roost which vultures were there on the same night
CoRoostingPairs = subset(AllPresent, ID1 != ID2 & night_date1 == night_date2 & Roost1 == Roost2)
RoostingPairs = CoRoostingPairs[,c("ID1","ID2")]
library(igraph)
Sorted_Pairs = get.data.frame(graph.data.frame(RoostingPairs, directed=FALSE),"edges") ##order of ID1 & ID2 shouldn't matter
library(plyr)
CoRoostEdgelist = ddply(Sorted_Pairs,.(from,to),nrow)
colnames(CoRoostEdgelist)=c("ID1","ID2","wt")
merge_CoOccurAll = left_join(AllPossibleEdgelist,CoRoostEdgelist, by = c("ID1","ID2"))
colnames(merge_CoOccurAll)[3:4]<-c("wt_all","wt_CoRoost")

##replacing NA's in cooccurence column by 0 but not in allpossible weights column
merge_CoOccurAll[["wt_CoRoost"]][is.na(merge_CoOccurAll[["wt_CoRoost"]])] <- 0
merge_CoOccurAll$SRI = round(merge_CoOccurAll$wt_CoRoost/merge_CoOccurAll$wt_all,3)
SRI_CoRoostEdgelist = merge_CoOccurAll[,c(1,2,5)]
