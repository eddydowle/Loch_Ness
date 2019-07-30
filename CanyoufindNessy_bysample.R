##############################################################################
# Libraries
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

##############################################################################
# Data
##############################################################################
setwd('~/Documents/CoAuthorMS/loch_ness/')
OTUtable.sum.t.descrip<-read.table('OTU_counts_merged_species.forapp.txt',header=T,row.names=NULL,sep='\t',stringsAsFactors = F)
OTUtable.sum.t.descrip.sum<-OTUtable.sum.t.descrip %>% group_by(new_code)%>% mutate(lat=as.factor(lat),long=as.factor(long)) %>% summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else paste(unique(.), collapse=", ")))


summary_line<-OTUtable.sum.t.descrip.sum %>%  mutate(lat=as.factor(lat),long=as.factor(long)) %>% summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else paste(unique(.), collapse=", "))) %>% mutate(new_code = "Summary Ness", location_id="All locations",lat='All lats', long='All longs',Description='All areas',Depthm='All depths') #%>% summary_line
display_table<-OTUtable.sum.t.descrip.sum %>%   mutate(lat=as.numeric(lat),long=as.numeric(long)) %>% mutate(id=as.numeric(str_replace_all(new_code, "Ness ", "")))%>%  arrange(.,id)  #rbind(.,summary_line) %>%
display_table

#this is because the two sheets dont line up so there is missing ness 14 etc codes
display_table$id <- seq.int(nrow(display_table))

class(display_table$lat)


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

calculateTextpositions<-function(values){
  # Do not display percentages < 5%
  if (values/total_count < 0.01) {
    return ('none')
  }
  else if  (values/total_count > 0.01){
    return('auto')
  }
}



#subset_choice='Lake center'
#latlongs_flat_choice<-latlongs_flat %>% mutate(icon_choice= ifelse(descrip==subset_choice, 'red','grey')) %>% mutate(depth_merge=ifelse(icon_choice=='red',paste('<b>',depth_merge,'</b>',sep=''),depth_merge))

#qDat <- quakes
#qDat$id <- seq.int(nrow(qDat))
#str(qDat)
##############################################################################
# UI Side
##############################################################################
ui <- fluidPage(
  titlePanel("Can you find Nessie?"),
  
  # side panel
  sidebarPanel(
    h3('Species Present'),
    
    radioButtons('pie_graph_subset', label="Pie graph display",
                 choices=c('All samples','By geographic location'),selected='All samples'),
    
    plotlyOutput('hist01')
  ),
  
  # main panel
  mainPanel(
    leafletOutput('map01'),
    dataTableOutput('table01')
  )
  
)
##############################################################################
# Server Side
##############################################################################
server <- function(input,output){
 # qSub <-  reactive({
    
  #  subset <- subset(qDat, qDat$mag>=input$sld01_Mag[1] &
   #                    qDat$mag<=input$sld01_Mag[2]) %>% head(25)
#  })
  
  # histogram
  output$hist01 <- renderPlotly({
    if(input$pie_graph_subset=="All samples") {
      data<-summary_line %>% filter(., grepl("Summary", new_code, fixed = TRUE)) %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm)
      data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0)
   #   print(data)
      total_count<-sum(as.numeric(data$V1))
      p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~V1 %>% map(calculateTextpositions) %>% unlist(.)) %>%
        layout(title = 'Read counts per species (all sites)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      p
    }
    })
  
  observeEvent(input$table01_rows_selected, {
    row_selected = display_table[input$table01_rows_selected,]
  
  output$hist01 <- renderPlotly({
    if(input$pie_graph_subset=="All samples") {
      data<-summary_line %>% filter(., grepl("Summary", new_code, fixed = TRUE)) %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm)
      data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0)
    #  print(data)
      total_count<-sum(as.numeric(data$V1))
      p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~V1 %>% map(calculateTextpositions) %>% unlist(.)) %>%
        layout(title = 'Read counts per species (all sites)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
    }
    
    if(input$pie_graph_subset=="By geographic location") {
      title_plot<-paste('Read counts per species:',row_selected$new_code,sep=' ')
       data<-row_selected %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm)
     # print (data)
     data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0)
   # print(data)
    total_count<-sum(as.numeric(data$V1))
    p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~V1 %>% map(calculateTextpositions) %>% unlist(.)) %>%
      layout(title = title_plot,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    }
    p
  })
  })
  
  # table
  output$table01 <- renderDataTable({
    
    DT::datatable(display_table, selection = "single",options=list(stateSave = TRUE))
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  observeEvent(input$table01_rows_selected, {
    row_selected = display_table[input$table01_rows_selected,]
    proxy <- leafletProxy('map01')
#    print(row_selected)
    proxy %>%
      #addAwesomeMarkers(popup=as.character(row_selected$new_code),
      #                  layerId = as.character(row_selected$id),
     #                   lng=as.numeric(row_selected$long), 
    #                    lat=as.numeric(row_selected$lat),
    #                    icon = nesiredIcon)
    addMarkers(popup=paste("Code:",row_selected$new_code, "<br>","Samples:",row_selected$Depthm,"<br>","Description:", row_selected$Description),
                      layerId = as.character(row_selected$id),
                      lng=as.numeric(row_selected$long), 
                      lat=as.numeric(row_selected$lat),
                      icon = nessysIcons$red)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addMarkers(popup=as.character(prev_row()$new_code), 
                   layerId = as.character(prev_row()$id),
                   lng=as.numeric(prev_row()$long), 
                   lat=as.numeric(prev_row()$lat),icon = nessysIcons$grey)
    }
    # set new value to reactiveVal 
    prev_row(row_selected)
  })
  
  # map
  output$map01 <- renderLeaflet({
  #  pal <- colorNumeric("YlOrRd", domain=c(min(quakes$mag), max(quakes$mag)))
    qMap <- leaflet(data = display_table) %>% 
      addTiles() %>%
      addMarkers(popup=~as.character(new_code), layerId = as.character(display_table$id),icon = nessysIcons$grey)#%>%
   #   addLegend("bottomright", colors='blue',  values = ~new_code,
    #            title = "Earthquake Magnitude",
    #            opacity = 1)
    qMap
  })
  
  observeEvent(input$map01_marker_click, {
    clickId <- input$map01_marker_click$id
    if(as.numeric(clickId)%%10 == 0){
      dataTableProxy("table01") %>%
        selectRows(which(display_table$id == clickId)) %>%
        selectPage( which(input$table01_rows_all == clickId) %/% input$table01_state$length )
    }
    else{
      dataTableProxy("table01") %>%
        selectRows(which(display_table$id == clickId)) %>%
        selectPage(which(input$table01_rows_all == clickId) %/% input$table01_state$length +1)
    }
  })
}

##############################################################################
shinyApp(ui = ui, server = server)
##############################################################################


#by species
#make colours consistent by species
#id column showing up

