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

col_var<-c("#b7c7e3", "#77e156", "#cf50d5", "#4aa630", "#9665ee", "#e0d63f", "#5c7af1", "#abd64f", "#cc70e3", "#6fe084", "#e150b9", "#5fe1ab", "#e5562e", "#6ae2d3", "#e35677", "#63a04d", "#a27bdc", "#a59a2e", "#7189de", "#df9734", "#5697d7", "#d4ce78", "#c274bd", "#c0e3a0", "#e26a5c", "#6dcde5", "#c4804d", "#4c99ba", "#e3be97", "#978dc2", "#5ea378", "#e0ace3", "#909056", "#88859d", "#c4dac4", "#bb8175", "#4f9f99", "#c1919e", "#809286")
?randomColor
head(OTUtable.sum.t.descrip)

species_list<-OTUtable.sum.t.descrip %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm) %>% colnames()

col_species<-as.data.frame(cbind(species_list,col_var)) %>% mutate(Species =str_replace_all(species_list, "\\.", " ")) %>% select(-species_list)




##############################################################################
# UI Side
##############################################################################
ui <- fluidPage(
  titlePanel("Can you find Nessie?"),
  
  # side panel
  sidebarPanel(
    h3('Species Present'),
    tags$style(".well {background-color:  #ebf5fb;}"),
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

  
    total_count <- reactiveVal(0)
  output$hist01 <- renderPlotly({
    if(input$pie_graph_subset=="All samples") {
      data<-summary_line %>% filter(., grepl("Summary", new_code, fixed = TRUE)) %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm)
      data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0) %>% mutate(text_pos=ifelse(V1/sum(as.numeric(V1)) > 0.01, 'auto','none'))
      data<-left_join(data,col_species,by='Species')
      

      p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~text_pos %>% unlist(.),marker=list(colors= ~col_var)) %>%
        layout(title = 'Read counts per species (all sites)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }
      if (input$pie_graph_subset=="By geographic location") {
        return(NULL)
        
      }
      p
    
    })
  
  observeEvent(input$table01_rows_selected, {
    row_selected = display_table[input$table01_rows_selected,]
  
  output$hist01 <- renderPlotly({
    if(input$pie_graph_subset=="All samples") {
      data<-summary_line %>% filter(., grepl("Summary", new_code, fixed = TRUE)) %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm)
      data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0)%>% mutate(text_pos=ifelse(V1/sum(as.numeric(V1)) > 0.01, 'auto','none'))
      data<-left_join(data,col_species,by='Species')
      p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~text_pos %>% unlist(.),marker=list(colors= ~col_var)) %>%
        layout(title = 'Read counts per species (all sites)',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
    }
    
    if(input$pie_graph_subset=="By geographic location") {
      title_plot<-paste('Read counts per species:',row_selected$new_code,sep=' ')
      if (nrow(row_selected)==0){
        return(NULL)
      }
      else{
        data<-row_selected %>% select(-location_id,-new_code,-lat,-long,-Description,-Depthm,-id)
     # print (data)
          data<-as.data.frame(t(data)) %>%  tibble::rownames_to_column(., "Species") %>% mutate(Species =str_replace_all(Species, "\\.", " ")) %>% filter(V1 >0)%>% mutate(text_pos=ifelse(V1/sum(as.numeric(V1)) > 0.01, 'auto','none'))
     data<-left_join(data,col_species,by='Species')
    p <- plot_ly(data, labels = ~Species, values = ~V1, type = 'pie',textposition=~text_pos %>% unlist(.),marker=list(colors= ~col_var)) %>%
      layout(title = title_plot,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
     }
    }
    p
  })
  })
  
  # table
  output$table01 <- renderDataTable({
    out_tab<-display_table %>% select(-lat,-long,-location_id)
    DT::datatable(out_tab, selection = "single",options=list(stateSave = TRUE))
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  observeEvent(input$table01_rows_selected, {
    row_selected = display_table[input$table01_rows_selected,]
    proxy <- leafletProxy('map01')
    proxy %>%
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
    qMap <- leaflet(data = display_table) %>% 
      addTiles() %>%
      addMarkers(popup=~as.character(new_code), layerId = as.character(display_table$id),icon = nessysIcons$grey,lng=display_table$long,lat=display_table$lat)#%>%
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

