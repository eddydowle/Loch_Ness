#ejd loch ness data
library(data.table)
library(tidyverse)
library(plyr)
setwd('~/Documents/CoAuthorMS/loch_ness/')

latlongs<-read.table('gps_coordinates.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
otu_counts<-read.table('OTU_counts.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
out_map_species<-read.table('OTU_speces_map.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
class(otu_counts$GGW_1_A)
head(latlongs)
otu_counts.t<-as.data.frame(t(otu_counts),stringsAsFactors = F)
setnames(otu_counts.t, as.character(otu_counts.t[1,]))
otu_counts.t <- otu_counts.t[-1,]
head(otu_counts.t$location_id)


#just going to merge across sample replicates hear until GJ gives more instructions on how he is calling a positive sample
class(otu_counts.t$ZOTU1)
otu_counts.t[, -1] <- sapply(otu_counts.t[, -1], as.numeric)
otu_counts.t.sumbyloc<-otu_counts.t %>% group_by(., location_id) %>% summarise_each(funs(sum))
otu_counts.t.sumbyloc$location_id
#weird shit with names
#GGW.1
#LN1.10
#LNT8 I assume this is LNT08
#LNT7 I assume this is LNT07
#LNS_11_d I assume this is LNS_11_2?
#fucks sake - made spreadsheet of weird shit for gj to deal with

#going to filter down for now until GJ clarifies what the issue is:
otu_counts.t.sumbyloc.comp<-otu_counts.t.sumbyloc %>% filter(location_id %in% latlongs$location_id)

#mapping figures
library(leaflet)
library(htmltools)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m

class(latlongs$lat)
class(latlongs$long)
mymap <- leaflet() %>% addTiles() 
#latlongs$location_id
mymap %>% addCircles(data=latlongs,lat= ~as.numeric(lat), lng = ~long, radius = 50, color = '#ff0000')

mymap %>% addPopups(data=latlongs,lat= ~as.numeric(lat), lng = ~long,latlongs$location_id)
?addPopups

latlongs$location_id
leaflet(latlongs) %>% addTiles() %>% 
addMarkers(lat= ~as.numeric(lat), lng = ~long, popup=~htmlEscape(location_id))
#okay works
#maybe we need a pop up that indicates the type of sampling at that spot e.g. sampled at depth 0,100,150,200 metres  or something.

#new codes
latlongs<-read.table('gsp_coordinates_newcodes.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
head(latlongs)
latlongs_flat<-latlongs %>%  group_by(.,new_code) %>% dplyr::summarize(location_id_merge = paste(unique(location_id), collapse = ","),depth_merge = paste(unique(Depthm), collapse = ","),lat = paste(unique(lat), collapse = ","),long = paste(unique(long), collapse = ","),descrip = paste(unique(Description), collapse = ","))
setDT(latlongs_flat)
#?setDT
latlongs_flat$long

leaflet(latlongs_flat) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip))


?addMarkers
?addAwesomeMarkers
leaflet(latlongs_flat) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip))

#recolouring based on description
getColor <- function(latlongs_flat) {
  sapply(latlongs_flat$descrip, function(descrip) {
    if(descrip == "Lake edge") {
      "deepskyblue"
    } else {
      "darkblue"
    } })
}


icons <- awesomeIcons(
  icon = 'ios',
  iconColor = 'black',
  library = 'ion',
  markerColor = unname(getColor(latlongs_flat))
)

leaflet(latlongs_flat) %>% addTiles() %>% 
  addAwesomeMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long),icon=icons,popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip))

#?awesomeIcons
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = unname(getColor(latlongs_flat))
)

leaflet(latlongs_flat) %>% addTiles() %>% 
  addAwesomeMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long),icon=icons,popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip))


#?addAwesomeMarkers
#?addCircleMarkers

leaflet(latlongs_flat) %>% addTiles() %>%
  addCircleMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long),color=unname(getColor(latlongs_flat)),popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:",descrip), stroke = FALSE,fillOpacity = 0.8)
    

#okay setting colours if we were going to select by depth
depthselect<-'100m'

getColor <- function(latlongs_flat) {
  sapply(latlongs_flat$depth_merge, function(depth) {
    if(grepl(depthselect,depth) == TRUE) {
      "red"
    } else {
      "yellow"
    } })
}

getColor(latlongs_flat)

leaflet(latlongs_flat) %>% addTiles() %>%
  addCircleMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long),color=unname(getColor(latlongs_flat)),popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:",descrip), stroke = FALSE,fillOpacity = 0.8)


#make own icon of nessy?
?makeIcon
nesiredIcon <- makeIcon(
  iconUrl = "icons/nessy_red.png",
  iconWidth = 38, iconHeight = 38,
  iconAnchorX = 0, iconAnchorY = 0
)

leaflet(latlongs_flat) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=nesiredIcon)

##################################################
############this code for shiny maps!#############
##################################################

#two nessys


nessysIcons <- iconList(
  red = makeIcon("icons/nessy_red.png", iconWidth = 38, iconHeight = 38,iconAnchorX = 0, iconAnchorY = 0),
  grey = makeIcon("icons/nessy_grey.png", iconWidth = 38, iconHeight = 38,iconAnchorX = 0, iconAnchorY = 0)
)


#selection based on position
subset_choice='Lake center'

latlongs_flat_choice<-latlongs_flat %>% mutate(icon_choice= ifelse(descrip==subset_choice, 'red','grey'))

leaflet(latlongs_flat_choice) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=~nessysIcons[icon_choice])

#selection based on depth
depth_choice='100m'

latlongs_flat_choice<-latlongs_flat %>% mutate(icon_choice= ifelse(grepl(depth_choice,depth_merge) == TRUE,'red','grey'))

leaflet(latlongs_flat_choice) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=~nessysIcons[icon_choice])

#####################
#okay now the tables#
#####################
latlongs<-read.table('gsp_coordinates_newcodes.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
otu_counts<-read.table('OTU_counts.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
out_map_species<-read.table('OTU_speces_map.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)

class(otu_counts$GGW_1_A)
head(latlongs)
otu_counts.t<-as.data.frame(t(otu_counts),stringsAsFactors = F)
setnames(otu_counts.t, as.character(otu_counts.t[1,]))
otu_counts.t <- otu_counts.t[-1,]
head(otu_counts.t$location_id)


#just going to merge across sample replicates hear until GJ gives more instructions on how he is calling a positive sample
class(otu_counts.t$ZOTU1)
otu_counts.t[, -1] <- sapply(otu_counts.t[, -1], as.numeric)
otu_counts.t.sumbyloc<-otu_counts.t %>% group_by(., location_id) %>% summarise_each(funs(sum))
otu_counts.t.sumbyloc$location_id
#weird shit with names
#GGW.1
#LN1.10
#LNT8 I assume this is LNT08
#LNT7 I assume this is LNT07
#LNS_11_d I assume this is LNS_11_2?
#fucks sake - made spreadsheet of weird shit for gj to deal with

#going to filter down for now until GJ clarifies what the issue is:
otu_counts.t.sumbyloc.comp<-otu_counts.t.sumbyloc %>% filter(location_id %in% latlongs$location_id)


#so we would first filter out by samples but to start lets just do across all the samples
#then we would filter out by colsums to remove blank columns
#
test<-otu_counts.t.sumbyloc.comp[, -which(numcolwise(sum)(otu_counts.t.sumbyloc.comp) == 0)]

#then we just need the column sums and the OTU name

#test_sum<-as.data.frame(colSums(test[,-1]))
#colnames(test_sum) <- "sum_reads"
#test_sum <- tibble::rownames_to_column(test_sum, "OTU")
#head(test_sum)
#out_map_species[grepl('ZOTU1449$|ZOTU1449,',out_map_species$OTUs),]

#test_sum %>% mutate(OTU_)

#okay rethinking this going to go from the outmapspecies and merge with larger set and back down
library(splitstackshape)
out_map_species_long <- cSplit(out_map_species, "OTUs", ",", "long",drop=FALSE)
out_map_species_long <- data.frame(lapply(out_map_species_long, as.character), stringsAsFactors=FALSE)

#will have to push otu_counts bac other way
#otu_counts.t.sumbyloc.comp.t<-t(otu_counts.t.sumbyloc.comp)
otu_counts.t.sumbyloc.comp.t<-as.data.frame(t(otu_counts.t.sumbyloc.comp),stringsAsFactors = F)
setnames(otu_counts.t.sumbyloc.comp.t, as.character(otu_counts.t.sumbyloc.comp.t[1,]))
otu_counts.t.sumbyloc.comp.t <- otu_counts.t.sumbyloc.comp.t[-1,]
head(otu_counts.t.sumbyloc.comp.t)

otu_counts.t.sumbyloc.comp.t <- tibble::rownames_to_column(otu_counts.t.sumbyloc.comp.t, "OTUs")
head(otu_counts.t.sumbyloc.comp.t)
otu_counts.t.sumbyloc.comp.t[, -1] <- sapply(otu_counts.t.sumbyloc.comp.t[, -1], as.numeric)

#must be inner join here as there is some OTU's that occur in the species map but not count table which cause NA issues later on 
test<-inner_join(out_map_species_long,otu_counts.t.sumbyloc.comp.t,by = ("OTUs" = "OTUs"))
head(test)

#so I guess you would take this table and do a subset on the columns you wanted if you were selecting down
#then sum the OTU counts by species and do a table on everything with >1 read?

output_table<-test  %>% mutate(sample_sums=rowSums(.[5:ncol(.)])) %>% select(.,Scientific_Name,Common_Name,sample_sums) %>% group_by(Common_Name) %>% dplyr::summarise(Scientific_Name = paste(unique(Scientific_Name), collapse = ","),Read_Counts=sum(sample_sums,na.rm = TRUE))

head(output_table)

