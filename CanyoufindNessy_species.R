##############################################################################
# Libraries
##############################################################################
##############################################################################

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(plotly)
library(data.table)
library(plyr)
library(tidyverse)
library(splitstackshape)
library(htmltools)
library(shinyWidgets)

##############################################################################
# Data
##############################################################################

OTUtable.sum.t.descrip<-read.table('OTU_counts_merged_species.forapp.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
OTUtable.sum.t.descrip.sum<-OTUtable.sum.t.descrip %>% group_by(new_code)%>% mutate(lat=as.factor(lat),long=as.factor(long)) %>% dplyr::summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else paste(unique(.), collapse=", ")))


summary_line<-OTUtable.sum.t.descrip.sum %>%  mutate(lat=as.factor(lat),long=as.factor(long)) %>% summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else paste(unique(.), collapse=", "))) %>% mutate(new_code = "Summary Ness", location_id="All locations",lat='All lats', long='All longs',Description='All areas',Depthm='All depths') #%>% summary_line
display_table<-OTUtable.sum.t.descrip.sum %>%   mutate(lat=as.numeric(lat),long=as.numeric(long)) %>% mutate(id=as.numeric(str_replace_all(new_code, "Ness ", "")))%>%  arrange(.,id)  #rbind(.,summary_line) %>%
display_table

#this is because the two sheets dont line up so there is missing ness 14 etc codes
display_table$id <- seq.int(nrow(display_table))

class(display_table$lat)

OTUtable.sum.t.descrip_depthreg<-OTUtable.sum.t.descrip%>%   mutate(lat=as.numeric(lat),long=as.numeric(long)) %>% mutate(id=as.numeric(str_replace_all(new_code, "Ness ", "")))%>%  arrange(.,id)
OTUtable.sum.t.descrip_depthreg$id <- seq.int(nrow(OTUtable.sum.t.descrip_depthreg))

latlongs<-read.table('gsp_coordinates_newcodes.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
#head(latlongs)
latlongs_flat<-latlongs %>%  group_by(.,new_code) %>% dplyr::summarize(location_id_merge = paste(unique(location_id), collapse = ","),depth_merge = paste(unique(Depthm), collapse = ","),lat = paste(unique(lat), collapse = ","),long = paste(unique(long), collapse = ","),descrip = paste(unique(Description), collapse = ","))
setDT(latlongs_flat)
nessysIcons <- iconList(
  red = makeIcon("icons/nessy_red.png", iconWidth = 38, iconHeight = 38,iconAnchorX = 0, iconAnchorY = 0),
  grey = makeIcon("icons/nessy_grey.png", iconWidth = 38, iconHeight = 38,iconAnchorX = 0, iconAnchorY = 0)
)

nesiredIcon <- makeIcon(
  iconUrl = "icons/nessy_red.png",
  iconWidth = 38, iconHeight = 38,
  iconAnchorX = 0, iconAnchorY = 0
)


col_var<-randomColor(count = 39, hue = "random", luminosity = "bright")
col_var<-c("#b7c7e3", "#77e156", "#cf50d5", "#4aa630", "#9665ee", "#e0d63f", "#5c7af1", "#abd64f", "#cc70e3", "#6fe084", "#e150b9", "#5fe1ab", "#e5562e", "#6ae2d3", "#e35677", "#63a04d", "#a27bdc", "#a59a2e", "#7189de", "#df9734", "#5697d7", "#d4ce78", "#c274bd", "#c0e3a0", "#e26a5c", "#6dcde5", "#c4804d", "#4c99ba", "#e3be97", "#978dc2", "#5ea378", "#e0ace3", "#909056", "#88859d", "#c4dac4", "#bb8175", "#4f9f99", "#c1919e", "#809286")
?randomColor
head(OTUtable.sum.t.descrip)

species_list<-OTUtable.sum.t.descrip %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm) %>% colnames()

col_species<-as.data.frame(cbind(species_list,col_var),stringsAsFactors = F) %>% mutate(Species =str_replace_all(species_list, "\\.", " ")) %>% select(-species_list)


##############################################################################
# UI Side
##############################################################################
ui <- fluidPage(
  titlePanel("Can you find Nessie?"),
  
  # side panel
  sidebarPanel(
    h3('Species Present'),
    
    pickerInput(
      inputId = "myPicker", 
      label = "Select species", 
      choices = col_species$Species, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    ),
    
    selectInput("Region_select", label = h3("Select region"), 
                choices = c("All regions", "Lake edge" ,  "Lake center" ), 
                selected = "All regions"),
    selectInput("Depth_select", label = h3("Select depth (m)"), 
                choices = c("All depths", "0.5m",  "20m", "100m","150m","200m"), 
                selected = "All depths"),
    plotlyOutput('hist01')
  ),
  
  # main panel
  mainPanel(
    leafletOutput('map01')
    
  )
  
)
##############################################################################
# Server Side
##############################################################################
server <- function(input,output){
  

  table_subset<- reactive({
    dep<-input$Depth_select
    reg<-input$Region_select
    if(dep=="All depths"){
      tab_set<-OTUtable.sum.t.descrip_depthreg
    } else if (dep=="20m"){
      tab_set<-OTUtable.sum.t.descrip_depthreg %>% filter(Depthm =='20m'| Depthm=='24m')
    } else(
      tab_set<-OTUtable.sum.t.descrip_depthreg %>% filter(Depthm==dep)
    )
    tab_set<-tab_set
    if (reg!="All regions"){
      tab_set<-tab_set %>% filter(Description==reg)
    }
    as.data.frame(tab_set)
  })
  
  
  output$hist01 <- renderPlotly({
    if(nrow(table_subset()) == 0)
      return(NULL)
    if(is.null(input$myPicker)){
    data<-table_subset() %>% dplyr::select(-location_id,-new_code,-lat,-long,-Description,-Depthm,-id)  %>% summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE)))
    data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0) %>% mutate(text_pos=ifelse(V1/sum(as.numeric(V1)) > 0.01, 'auto','none'))
    data<-left_join(data,col_species,by='Species')
    p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~text_pos %>% unlist(.),marker=list(colors= ~col_var)) %>%
      layout(title = "Read counts per species",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    else{
    chosenSpec<-input$myPicker
    data<-table_subset() %>% dplyr::select(-location_id,-lat,-long,-Description,-Depthm,-id) 
    data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") #%>% filter(Species %in% chosenSpec) 
    x<-c(chosenSpec,"new_code")
    data<-data %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(Species %in% x) %>% t(.) 
    data<-as.data.frame(data,stringsAsFactors = F) 
    samp2 <- as.character(data[1,])
    colnames(data) <- samp2
    data<- data[-1, ]
    data<-data %>%mutate_at(vars(-new_code), as.numeric)  %>%  group_by(new_code) %>%  summarise_each(funs(sum)) 
    data<-data[rowSums(data[,-1])> 0,]
    data<-data %>% mutate(id=as.numeric(str_replace_all(new_code, "Ness ", "")))%>%  arrange(.,id) %>% select(-id)
    colNames <- names(data)[-1] 
    print(data)
    if (nrow(data)==0){
      return(NULL)
    }
    else{
    col1<-col_species %>% filter(Species %in% x) 
    p <- plot_ly(data,x =as.formula(paste0("~`", colNames[1], "`")),y =~new_code,  type = 'bar',orientation = 'h',name = col1$Species[1], color = I(col1$col_var[1]))
  for(trace in colNames[-1]){
    col2<-col_species %>% filter(Species %in% trace)
    p <- p %>% plotly::add_trace(x = as.formula(paste0("~`", trace, "`")), name = col2$Species[1] ,color=I(col2$col_var[1]))
  }
    p <-p %>% layout(title = "",
           xaxis = list(title = "Read counts"),
           yaxis = list(title = "",categoryarray = ~new_code, categoryorder = "array"))
    }
}
    p
  })
  
  output$map01 <- renderLeaflet({
    if(is.null(input$myPicker)){
          dep<-input$Depth_select
    sam_subset<-table_subset()$new_code
    map_table<-display_table %>% mutate(icon_choice=ifelse( new_code %in% sam_subset ,'red','grey')) %>% mutate(depth_merge= str_replace(Depthm, dep, paste('<b>',dep,'</b>',sep='')))
    qMap<- leaflet(map_table) %>% addTiles() %>% 
      addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", Description),icon=~nessysIcons[icon_choice])    
    }
    else{
      dep<-input$Depth_select
      chosenSpec<-input$myPicker
      data<-table_subset() %>% dplyr::select(-location_id,-lat,-long,-Description,-Depthm,-id) 
      data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") #%>% filter(Species %in% chosenSpec) 
      x<-c(chosenSpec,"new_code")
      data<-data %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(Species %in% x) %>% t(.) 
      data<-as.data.frame(data,stringsAsFactors = F) 
      samp2 <- as.character(data[1,])
      colnames(data) <- samp2
      data<- data[-1, ]
      data<-data %>%mutate_at(vars(-new_code), as.numeric)  %>%  group_by(new_code) %>%  summarise_each(funs(sum)) 
      data<-data[rowSums(data[,-1])> 0,]
   #   print(data)
      sam_subset<-data$new_code
      map_table<-display_table %>% mutate(icon_choice=ifelse( new_code %in% sam_subset ,'red','grey')) %>% mutate(depth_merge= str_replace(Depthm, dep, paste('<b>',dep,'</b>',sep='')))
      qMap<- leaflet(map_table) %>% addTiles() %>% 
        addMarkers(lat= ~as.numeric(lat), lng = ~as.numeric(long), popup=~paste("Code:",new_code, "<br>","Samples:",depth_merge,"<br>","Description:", Description),icon=~nessysIcons[icon_choice])    
      
    }
    qMap
  })
  
}

##############################################################################
shinyApp(ui = ui, server = server)
##############################################################################

