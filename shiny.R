library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Spotify Analyzer"),
    sidebarPanel(
      
   
      selectInput("Type1", "Please Select a Function",
                  choices=c("Single Artist Summary", "Song/hour","Summaries")),
      
      conditionalPanel(condition = "input.Type1 == 'Single Artist Summary'",
                       textInput ("Name", "Please Select a Name", "Amr Diab") ,
                       
                       
                       selectInput("Type2", "Please Select Data Type",
                                   choices=c("Graph", "Table")),
                       
                       conditionalPanel(condition = "input.Type2 == 'Graph'",
                                        
                                        actionButton("graph", "Graph!")),
                       
                       conditionalPanel(condition = "input.Type2 == 'Table'",
                                        
                                        actionButton("table", "Go!")),
                       
                       ),
      conditionalPanel(condition = "input.Type1 == 'Song/hour'",
                       textInput("Songhour", "Please Select a song", "PPP"),
                      
                       
                       
                       selectInput("sType2", "Please Select Data Type",
                                   choices=c("Graph", "Table")),
                       
                       conditionalPanel(condition = "input.sType2 == 'Graph'",
                                        
                                        actionButton("s_graph", "Graph!") ),
                       
                       conditionalPanel(condition = "input.sType2 == 'Table'",
                                        
                                        actionButton("s_table", "Go!")),
                       
      ),
      
      
      conditionalPanel(condition = "input.Type1 == 'Summaries'",
                      
                      actionButton("artistSummary", "Artist Summary"),
                      actionButton("Albumsummaryfull", "Album Summary"),
                      actionButton("favTracks", "Fav Tracks"),
                      actionButton("datesummary", "Dates Summary"),
      actionButton("timesummarydetailed", "Time Summary")),
      
      
      
      conditionalPanel(condition = "input.Type1 == 'Graphs'",
                       
                      
                       actionButton("timesummarydetailed", "Time Summary")),
                       
       ),
    mainPanel(
      # if( tq == "G"){
      #   plotOutput('plot')
      # 
      #   }
      #   else{
      # dataTableOutput('table')
      #   }
      
     
      conditionalPanel(condition =  " input.Type2 == 'Graph' && input.Type1 == 'Single Artist Summary' ||  input.sType2 == 'Graph' && input.Type1 == 'Song/hour' ",  plotOutput('plot')),
      conditionalPanel(condition =  "input.Type1 == 'Summaries' || input.Type2 == 'Table' && input.Type1 == 'Single Artist Summary' || input.sType2 == 'Table' && input.Type1 == 'Song/hour'  ",  dataTableOutput('table')),
      
     # , conditionalPanel(condition =  "input.Type1 == 'Song/hour'",
     #  conditionalPanel(condition =  "input.sType2 == 'Graph' && input.Type1 == 'Song/hour'",  plotOutput('plot')),
      # conditionalPanel(condition =  "input.sType2 == 'Table' && input.Type1 == 'Song/hour'",  dataTableOutput('table'))


     # conditionalPanel(condition =  "input.Type1 == 'Summaries'",
     #                  dataTableOutput('table'))
    
    
  )
))


server <- function(input, output, session) {


  observe({

  distType<- input$Type1
  if(distType == 'Single Artist Summary'){
  ty<-input$Type2
 k<-eventReactive(input$table,{
   
    s<-bestofArtist(input$Name)
    s
  } )
 m<-eventReactive(input$graph,{
 
   gr<<-Artisthouradj(input$Name)
   gr
 } )
 output$table<-renderDataTable({k()})


   output$plot<-renderPlot({m()})
 }
  else{if(distType == 'Song/hour'){
    
 m<-eventReactive(input$s_table,{
   n<-Songhour(input$Songhour)
   n
 } )

 k<-eventReactive(input$s_graph,{

   s<-Songhouradj(input$Songhour)
   s
 } )
 output$table<-renderDataTable({m()})
 output$plot<-renderPlot({k()})
 }
    else{
      observeEvent(input$artistSummary, {
        output$table <-  renderDataTable(Artistsummaryfull())
      })  
      observeEvent(input$Albumsummaryfull, {
        output$table <-  renderDataTable(Albumsummaryfull())
      }) 
      observeEvent(input$favTracks, {
        output$table <-  renderDataTable(favTracks())
      }) 
      observeEvent(input$datesummary, {
        output$table <-  renderDataTable(datesummary())
      }) 
      observeEvent(input$timesummarydetailed, {
        output$table <-  renderDataTable(timesummarydetailed())
      }) 
      
      
    }
    
    }
    
    
    })
  # observeEvent(input$go, {
  #   output$table <-  renderDataTable(bestofArtist(input$Name))
  # }) 
  # observeEvent(input$go2, {
  # 
  # 
  #   output$table <-  renderDataTable(Songhour(input$Songhour))
  # 
  # })
  # observe({  
  #   
  # distType<- input$Type1
  # if(distType == 'SingleArtistSummary'){
  # output$table <-  renderDataTable(bestofArtist(input$Name))
  # 
  # }
  # else {
  #   if(distType =="Song/hour")
  #   output$table <-  renderDataTable(Songhour(input$Songhour))
  # }})
  
}


sumofArtist<-function(name){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >30000)
  s1<-sum(subset(c1,artistName%in%name)[,5])/60000
  h<-s1/60
 # print(c(name,h))
  
}
init<-function(){
  #Installing Packages that are not already available in the system 
  list.of.packages <- c("ggplot2","dplyr","readxl")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {install.packages(new.packages)
    library(new.packages)}
}

bestofArtist<-function(name){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >30000)
  s1<-subset(c1,artistName%in%name)
  lis<-unique(s1)
  df = data.frame(Name=numeric(0), Reps=numeric(0));
  i<-1
  # while(i<=nrow(lis)){
  #  # temp<-subset(c1,trackName %in%lis[i,1])
  #   s<-sum(c1[,4]%in%lis[i,1])
  #   
  #   newrow = data.frame(Name=lis[i,1], Reps=s)
  #   df <- rbind(df, newrow)
  #   i<-i+1
  # }
  # jj<-df[order(df[,2],decreasing = TRUE),]
  # jj
  
  df<-as.data.frame(table(s1$trackName))
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
}
bestofArtistS<-function(name,temp){
  
  c1<-temp
  s1<-temp[,9]
  
  
  
  df<-as.data.frame(table(s1$trackName))
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj[1,]
  
  
}
bestofAlbumS<-function(name,temp){
  
  c1<-temp
  s1<-temp[,9]
  
  
  
  df<-as.data.frame(table(s1$trackName))
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj[1,]
  
  
}


Artistsummary<-function(){
  x <- read_excel("spotify.xlsx")
  c<-subset(x, msPlayed >10000)
  df = data.frame(Name = numeric(0), Hours = numeric(0));
  c<-c[order(c[,10]),decreasing = TRUE] 	
  lil<-unique(c[,10])
  i<-1
  while(i < nrow(lil)){
    
    temp<-subset(c,artistName %in%lil[i,1])
    h<-60
    
    
    h<-sum(temp[,5])/3600000
    
    
    
    newrow = data.frame(Name=lil[i,1], Hours=h)
    df <- rbind(df, newrow)
    #print(newrow)
    i=1+i
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
  
  
  
}

Albumsummary<-function(){
  x <- read_excel("spotify.xlsx")
  c<-subset(x, msPlayed >10000)
  df = data.frame(Name = numeric(0), Hours = numeric(0));
  c<-c[order(c[,11]),decreasing = TRUE] 	
  lil<-unique(c[,11])
  i<-1
  while(i < nrow(lil)){
    
    temp<-subset(c,album_name %in%lil[i,1])
    h<-60
    
    
    h<-sum(temp[,5])/3600000
    
    
    
    newrow = data.frame(Name=lil[i,1], Hours=h)
    df <- rbind(df, newrow)
    #print(newrow)
    i=1+i
  }
  # jj<-df[order(df[,2],decreasing = TRUE),]
  #jj
  df
  
}

Artistsummaryfull<-function(){
  x <- read_excel("spotify.xlsx")
  c<-subset(x, msPlayed >30000)
  df = data.frame(Name = numeric(0), Hours = numeric(0),Best= numeric(0),BestCount=numeric(0));
  c<-c[order(c[,10]),decreasing = TRUE] 	
  lil<-unique(c[,10])
  i<-1
  while(i < nrow(lil)){
    
    temp<-subset(c,artistName %in%lil[i,1])
    h<-60
    
    
    h<-sum(temp[,5])/3600000
    k<-bestofArtistS(lil[i,1],temp)
    
    
    newrow = data.frame(Name=lil[i,1], Hours=h,Best=k[1,1],BestCount=k[1,2])
    df <- rbind(df, newrow)
    #print(newrow)
    i=1+i
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
  
  
}
Albumsummaryfull<-function(){
  x <- read_excel("spotify.xlsx")
  c<-subset(x, msPlayed >30000)
  df = data.frame(Name = numeric(0),Artist = numeric(0), Hours = numeric(0),Best= numeric(0),BestCount=numeric(0));
  c<-c[order(c[,11]),decreasing = TRUE] 	
  lil<-unique(c[,11])
  i<-1
  while(i < nrow(lil)){
    
    temp<-subset(c,album_name %in%lil[i,1])
    h<-60
    
    
    h<-sum(temp[,5])/3600000
    k<-bestofAlbumS(lil[i,1],temp)
    
    
    newrow = data.frame(Name=lil[i,1],Artist= temp[1,10], Hours=h,Best=k[1,1],BestCount=k[1,2])
    df <- rbind(df, newrow)
    #print(newrow)
    i=1+i
  }
  jj<-df[order(df[,3],decreasing = TRUE),]
  jj
  
  
}


favTracks<-function(){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >90000)
  df1<-as.data.frame(table(c1$trackName))
  lil<-df1[order(df1[,2],decreasing = TRUE),]
  df = data.frame(Name = numeric(0),Artist = numeric(0),Count=numeric(0));
  
  i<-1
  while (i < nrow(lil)) {
    song<-lil[i,1]
    temp <- subset(c1, trackName %in% lil[i, 1])
    
    
    
    newrow = data.frame(Name = lil[i, 1],
                        Artist = temp[1, 10],
                        Count = lil[i, 2])
    df <- rbind(df, newrow)
    #print(newrow)
    i = 1 + i
  }
  df
  
}
favTracksyear<-function(year){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >90000)
  c1<-subset(c1,substr(x =ts, start = 1, stop = 4)%in% year)
  df<-as.data.frame(table(c1$trackName))
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
}
favTracksyearmonth<-function(year,month,x1){
  
  c1<-subset(x1, msPlayed >90000)
  c1<-subset(c1,substr(x =ts, start = 1, stop = 4)%in% year)
  
  c1<-subset(c1,substr(x =ts, start = 6, stop =7 )%in% month)
  
  df<-as.data.frame(table(c1$trackName))
  #print(c1)
  #print(c(month,year))
  if(dim(df)[1] == 0){
    return ("null")
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  as.character(jj[1,1])
}
favTracksyearmonthsum<-function(year,month){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >90000)
  c1<-subset(c1,substr(x =ts, start = 1, stop = 4)%in% year)
  
  c1<-subset(c1,substr(x =ts, start = 6, stop =7 )%in% month)
  
  df<-as.data.frame(table(c1$trackName))
  #print(c1)
  #print(c(month,year))
  if(dim(df)[1] == 0){
    return ("null")
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
}
datesummary<-function(){
  df = data.frame(Name = numeric(0),Artist= numeric(0), Month = numeric(0),Year= numeric(0));
  x1 <- read_excel("spotify.xlsx")
  y<-2016
  while(y<2022)
  {
    mon<-1
    while(mon<13)
    {
      m<-mon
      if(mon<10){
        m<-paste("0",mon,sep = "")}
     # print (m)
      f<-favTracksyearmonth(y,m,x1)
    #  print(f[1])
      temp <- subset(x1, trackName %in% f[1])
      newrow = data.frame(Name=f[1],Artist=temp[1,10], Month=mon,Year=y)
      df <- rbind(df, newrow)
      mon<-mon+1
    }
    y<-y+1
  }
  df
  
}
timesummarydetailed<-function(){
  df = data.frame(Name = numeric(0), Artist= numeric(0),Hour = numeric(0),Minute= numeric(0));
  x1 <- read_excel("spotify.xlsx")
  y<-0
  while(y<24)
  {
    mon<-0
    while(mon<60)
    {
      m<-mon
      ye<-y
      if(mon<10){
        m<-paste("0",mon,sep = "")}
      if(y<10){
        ye<-paste("0",y,sep = "")}
      
      f<-favTrackshourmin(ye,m,x1)
      temp <- subset(x1, trackName %in% f[1])
      newrow = data.frame(Name=f[1],Artist=temp[1,10] , Hour=ye,Minute=m)
      df <- rbind(df, newrow)
      mon<-mon+1
    }
    y<-y+1
  }
  df
  
}
favTrackshourmin<-function(year,month,x1){
  
  c1<-subset(x1, msPlayed >90000)
  c1<-subset(c1,substr(x =ts, start = 12, stop = 13)%in% year)
  
  c1<-subset(c1,substr(x =ts, start = 15, stop =16 )%in% month)
  
  df<-as.data.frame(table(c1$trackName))
  #print(c1)
  #print(c(month,year))
  if(dim(df)[1] == 0){
    return ("null")
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  as.character(jj[1,1])
}
favTrackshour<-function(year,x1){
  
  c1<-subset(x1, msPlayed >90000)
  c1<-subset(c1,substr(x =ts, start = 12, stop = 13)%in% year)
  
  
  
  df<-as.data.frame(table(c1$trackName))
  #print(c1)
  #print(c(month,year))
  if(dim(df)[1] == 0){
    return ("null")
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
}
timesummary<-function(){
  df = data.frame(Name = numeric(0), Hour = numeric(0),Frequency=numeric(0));
  x1 <- read_excel("spotify.xlsx")
  y<-0
  while(y<24)
  {
    ye<-y
    if(y<10){
      ye<-paste("0",y,sep = "")}
    
    f<-favTrackshour(ye,x1)
    
    newrow = data.frame(Name=as.character(f[1,1]),Hour=y,Frequency=Trackshourh(ye,x1))
    df <- rbind(df, newrow)
    
    
    y<-y+1
  }
  df
  
}

Trackshourh<-function(year,x1){
  
  
  c1<-subset(x1,substr(x =ts, start = 12, stop = 13)%in% year)
  
  sum(c1$msPlayed)/(1000*3600)
  
  
}
Songhour<-function(name){
  x1 <- read_excel("spotify.xlsx")
  df = data.frame( Hour = numeric(0),Frequency=numeric(0));
  y<-0
  while(y<24)
  {
    ye<-y
    if(y<10){
      ye<-paste("0",y,sep = "")}
    
    f<-songhourh(name,ye,x1)
    
    newrow = data.frame(Hour=y,Frequency=f)
    df <- rbind(df, newrow)
    
    
    y<-y+1
  }
  df
}
songhourh<-function(name,ye,x1){
  
  
  c1<-subset(x1,substr(x =ts, start = 12, stop = 13)%in% ye)
  c1<-subset(c1,trackName%in% name)
  # print (c1)
  sum(c1$msPlayed)/(1000*60)
  
  
}
Songhouradj<-function(name){
  x1 <- read_excel("spotify.xlsx")
  df = data.frame( Hour = numeric(0),Frequency=numeric(0));
  y<-0
  while(y<24)
  {
    ye<-y
    if(y<10){
      ye<-paste("0",y,sep = "")}
    
    f<-songhouradjh(name,ye,x1)
    
    newrow = data.frame(Hour=y,Frequency=f)
    df <- rbind(df, newrow)
    
    
    y<-y+1
  }
  ggplot(data = df, aes(x = Hour, y =Frequency )) +
    geom_line()
}
songhouradjh<-function(name,ye,x1){
  
  
  c1<-subset(x1,substr(x =ts, start = 12, stop = 13)%in% ye)
  c1<-subset(c1,trackName%in% name)
  # print (c1)
  sum(c1$msPlayed)/(1000*3600*Trackshourh(ye,x1))
  
  
}
Artisthouradj<-function(name){
  x1 <- read_excel("spotify.xlsx")
  df = data.frame( Hour = numeric(0),Frequency=numeric(0));
  y<-0
  while(y<24)
  {
    ye<-y
    if(y<10){
      ye<-paste("0",y,sep = "")}
    
    f<-Artisthouradjh(name,ye,x1)
    
    newrow = data.frame(Hour=y,Frequency=f)
    df <- rbind(df, newrow)
    
    
    y<-y+1
  }
  ggplot(data = df, aes(x = Hour, y =Frequency )) +
    geom_line()
}
Artisthouradjh<-function(name,ye,x1){
  
  
  c1<-subset(x1,substr(x =ts, start = 12, stop = 13)%in% ye)
  c1<-subset(c1,artistName%in% name)
  # print (c1)
  sum(c1$msPlayed)/(1000*3600*Trackshourh(ye,x1))
  
  
}
Artisthour<-function(name){
  x1 <- read_excel("spotify.xlsx")
  df = data.frame( Hour = numeric(0),Frequency=numeric(0));
  y<-0
  while(y<24)
  {
    ye<-y
    if(y<10){
      ye<-paste("0",y,sep = "")}
    
    f<-Artisthourh(name,ye,x1)
    
    newrow = data.frame(Hour=y,Frequency=f)
    df <- rbind(df, newrow)
    
    
    y<-y+1
  }
  df
}
Artisthourh<-function(name,ye,x1){
  
  
  c1<-subset(x1,substr(x =ts, start = 12, stop = 13)%in% ye)
  c1<-subset(c1,artistName%in% name)
  # print (c1)
  sum(c1$msPlayed)/(1000*3600)
  
  
}


shinyApp(ui, server)