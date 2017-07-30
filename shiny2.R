library(leaflet)
library(shiny)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(geojsonio)
library(ggplot2)
library(readr)

####obróbka bazy danych
setwd("...")

bezrobocie<- read_delim("RYNE_2361_CTAB_20170424140402.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)  #plik z danymi


lata<-c("nazwa", 1995:2016)

bezrobocie%>%select(2:24)->bezrobocie
colnames(bezrobocie)<-lata     

bezrobocie$id_1<-c(0,1,9,5,13,7,14,11,2,8,3,16,6,10,4,12,15)   #id z pliku geojson




granice <- geojsonio::geojson_read("pol_adm1_1.geojson",
                                   what = "sp")   #plik z mapą



###stworzenie bazy z bezrobociem dla całej Polski (do wykresu z boku)
bezrobocie[1,]%>%gather->polska              
polska$key<-as.numeric(polska$key)
polska$value<-as.numeric(polska$value)
polska<-polska[2:22,]



id<-as.data.frame(granice@data$id_1)
colnames(id)<-"id_1"

ost<-left_join(id, bezrobocie, by="id_1")       #teraz wartości są z oddzielnej bazy danych (ost)








pal <- colorNumeric("viridis", NULL)     #kolory




ui<-fluidPage(
  # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  titlePanel("Mapa pokazująca wartość stopy bezrobocia dla Polski w latach 1995-2016"),
  
  fluidRow(
    column(6,
           sliderInput("rok", "Wybierz rok:", 1995, 2015,
                       value = 2015, step = 1, width = 400),
           textOutput("plrok"),
           textOutput("meanrok"),
           textOutput("rrpl"),
          plotOutput("cala")
          
          
           
           
           
    ),
    column(6,
           leafletOutput("map")
          
    ))
  
  
  
)










server<- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet(granice, height = 1, width = 2) %>%addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons()%>%setView(lng=19, lat=52, zoom=5.75)
    
  })
  
  
  
  observe({
    colfunc <- colorRampPalette(c("#00cccc","#ffff00" ))    #tutaj pobawić się
    kolory<-colfunc(50)
    rok<-input$rok-1992
    leafletProxy("map", data = granice)%>%
      clearMarkers()%>%
      addPolygons(stroke = T, 
                  color="black",
                  weight=1,
                  smoothFactor = 0.3, 
                  fillOpacity = 1,
                  fillColor = ~pal(unlist(ost[rok])),   #wypełnienie
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = F),
                  label = ~paste0(ost$nazwa, ": ", formatC(unlist(ost[rok])), "%")
                  )%>%
      
      
      addLegend(pal = pal, values = ~log10(unlist(ost[rok])), 
                layerId= "legenda",
                position="bottomleft",
                opacity = 1.0,
                title="Wartość",
                labFormat = labelFormat(suffix="%",transform = function(x) round(10^x)))
      
      
  
      
      
      
    output$cala<- renderPlot(height = 200,{
      ggplot(data=polska, aes(x=key, y=value, group=1))+
        geom_point()+
        geom_line(size=1)+
        geom_vline(aes(xintercept=input$rok), color="red", size=1)+
        theme_minimal()+
        labs(x="rok", y="Wartość (w %)")+
        scale_y_continuous(breaks = c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20),
                           limits = c(0,22.5))
        
      
      
      
    })
    output$plrok<-renderText(paste0("Wartość dla Polski w ", 
                                      input$rok,
                                      " roku to ",
                                      polska$value[input$rok-1994]," %"))
    output$meanrok<-renderText(paste0("Średnia wartość bezrobocia dla Polski to ", 
                                    
                                   round( mean(polska$value),2)," %"))
    output$rrpl<-renderText(paste0("Zmiana rok do roku w latach ", 
                                      input$rok-1,"/",input$rok,
                                      " to ", round(polska$value[input$rok-1995]-polska$value[input$rok-1994],2), " p.p."))
    
    
    
    
    
    
  })
  
}




shinyApp(ui, server)

