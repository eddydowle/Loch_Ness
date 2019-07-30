library(data.table)
library(plyr)
library(tidyverse)
library(splitstackshape)
library(leaflet)
library(htmltools)

setwd('~/Documents/CoAuthorMS/loch_ness/')


#code for maps#
#new codes
latlongs<-read.table('gsp_coordinates_newcodes.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
head(latlongs)
latlongs_flat<-latlongs %>%  group_by(.,new_code) %>% dplyr::summarize(location_id_merge = paste(unique(location_id), collapse = ","),depth_merge = paste(unique(Depthm), collapse = ","),lat = paste(unique(lat), collapse = ","),long = paste(unique(long), collapse = ","),descrip = paste(unique(Description), collapse = ","))

#nessy icons
setDT(latlongs_flat)
nessysIcons <- iconList(
  red = makeIcon("icons/nessy_red.png", iconWidth = 38, iconHeight = 38,iconAnchorX = 0, iconAnchorY = 0),
  grey = makeIcon("icons/nessy_grey.png", iconWidth = 38, iconHeight = 38,iconAnchorX = 0, iconAnchorY = 0)
)


#selection based on position
subset_choice='Lake center'

#just bolding to be consistent across maps
latlongs_flat_choice<-latlongs_flat %>% mutate(icon_choice= ifelse(descrip==subset_choice, 'red','grey')) %>% mutate(depth_merge=ifelse(icon_choice=='red',paste('<b>',depth_merge,'</b>',sep=''),depth_merge))
class(latlongs_flat_choice$new_code)
leaflet(latlongs_flat_choice) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=~nessysIcons[icon_choice])

#selection based on depth
depth_choice='100m'

#will bold 100m on the output too to make it more clear as to which samples are being selected
latlongs_flat_choice<-latlongs_flat %>% mutate(icon_choice= ifelse(grepl(depth_choice,depth_merge) == TRUE,'red','grey'))%>% mutate(depth_merge= str_replace(depth_merge, depth_choice, paste('<b>',depth_choice,'</b>',sep='')))
                                                                                                                                     
leaflet(latlongs_flat_choice) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=~nessysIcons[icon_choice])

#code for tables#

latlongs<-read.table('gsp_coordinates_newcodes.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
otu_counts<-read.table('OTU_counts.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
out_map_species<-read.table('OTU_speces_map.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)

#just getting counts into sane format
otu_counts.t<-as.data.frame(t(otu_counts),stringsAsFactors = F)
setnames(otu_counts.t, as.character(otu_counts.t[1,]))
otu_counts.t <- otu_counts.t[-1,]

#just going to merge across sample replicates hear until GJ gives more instructions on how he is calling a positive sample
otu_counts.t[, -1] <- sapply(otu_counts.t[, -1], as.numeric)
otu_counts.t.sumbyloc<-otu_counts.t %>% group_by(., location_id) %>% summarise_each(funs(sum))

#weird shit with names
#going to filter down for now until GJ clarifies what the issue is:
otu_counts.t.sumbyloc.comp<-otu_counts.t.sumbyloc %>% filter(location_id %in% latlongs$location_id)

#making species map unique by OTU
out_map_species_long <- cSplit(out_map_species, "OTUs", ",", "long",drop=FALSE)
out_map_species_long <- data.frame(lapply(out_map_species_long, as.character), stringsAsFactors=FALSE)

#will have to push otu_counts bac other way
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

write.table(test,'OTU_counts_merged_species.txt',row.names=F,quote=F,sep='\t')

#this is the table that I would take into the shiny graph and then use for subseting based of selections for either species or location
#so I guess you would take this table and do a subset on the columns you wanted if you were selecting down
#then sum the OTU counts by species and do a table on everything with >1 read?

output_table<-test  %>% mutate(sample_sums=rowSums(.[5:ncol(.)])) %>% select(.,Scientific_Name,Common_Name,sample_sums) %>% group_by(Common_Name) %>% dplyr::summarise(Scientific_Name = paste(unique(Scientific_Name), collapse = ","),Read_Counts=sum(sample_sums,na.rm = TRUE))

head(output_table)
#say you wanted to select one geographic site

#lets pretend you selected all samples from Ness 1 LNT01 LNM01 LND01
#you would first have to convert Ness1 to a string of those three
a<-c('LNT01', 'LNM01', 'LND01')

test  %>% select(.,Common_Name,Scientific_Name,one_of(a)) %>% mutate(sample_sums=rowSums(.[5:ncol(.)])) %>% select(.,Scientific_Name,Common_Name,sample_sums) %>% group_by(Common_Name) %>% dplyr::summarise(Scientific_Name = paste(unique(Scientific_Name), collapse = ","),Read_Counts=sum(sample_sums,na.rm = TRUE)) %>% filter(Read_Counts >0)

#yup that works

#how do we get to 'a'

head(latlongs)

#lets say they clicked Ness 1
x='Ness 1'
a<-latlongs %>% filter(new_code==x) %>% .$location_id

#lets say they clicked 50m
y='50m'
a<-latlongs %>% filter(Depthm==y) %>% .$location_id
a

#lets say Ness 1 and 100m
x='Ness 1'
y='100m'
a<-latlongs %>% filter(Depthm==y&new_code==x) %>% .$location_id
a

#the otherthing we want to be able to do is select by species and then plot that out on the map
z<-'European badger'

#I guess you could have a table of the counts for the species and below the map
species_counts<-test  %>% filter(.,Common_Name==z )  %>% select(-Scientific_Name,-Common_Name,-Most_Likely_Species,-OTUs) %>%  select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% colSums()

z<-'Wild boar'
species_counts<-test  %>% filter(.,Common_Name==z )  %>% select(-Scientific_Name,-Common_Name,-Most_Likely_Species,-OTUs) %>%  select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% colSums(.)
head(species_counts)

species_counts<-as.data.frame(species_counts) %>%  tibble::rownames_to_column(., "location_id")

output_table<-inner_join(latlongs, species_counts,by = ('location_id'='location_id')) %>% select(.,new_code,location_id,Description,Depthm,species_counts)


#make into red/grey nesi map



#selection based on position
subset_choice=unique(output_table$new_code)
subset_choice
latlongs_flat_choice<-latlongs_flat %>% mutate(icon_choice= ifelse(new_code %in% subset_choice, 'red','grey'))

leaflet(latlongs_flat_choice) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=~nessysIcons[icon_choice])

#what if we bold the depths that the sample is from
#to bold <b> </b>
subset_choice=unique(output_table$new_code)

output_table<-output_table %>% mutate(bold_depth=paste('<b>',Depthm,'</b>',sep='')) %>% mutate()
latlongs_flat_species<-output_table %>% select(-Description, -Depthm ,-species_counts,-new_code) %>% left_join(latlongs,.,by=('location_id')) %>% mutate(bold_depth = ifelse(is.na(bold_depth), Depthm, bold_depth)) %>%  group_by(.,new_code) %>% dplyr::summarize(location_id_merge = paste(unique(location_id), collapse = ","),depth_merge = paste(unique(bold_depth), collapse = ","),lat = paste(unique(lat), collapse = ","),long = paste(unique(long), collapse = ","),descrip = paste(unique(Description), collapse = ","))%>% mutate(icon_choice= ifelse(new_code %in% subset_choice, 'red','grey'))

leaflet(latlongs_flat_species) %>% addTiles() %>% 
  addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", descrip),icon=~nessysIcons[icon_choice])


