sumofArtist<-function(name){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >30000)
  s1<-sum(subset(c1,artistName%in%name)[,5])/60000
  h<-s1/60
  print(c(name,h))
  
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
 # jj<-df[order(df[,2],decreasing = TRUE),]
  #jj
df
  
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
  df = data.frame(Name = numeric(0), Hours = numeric(0),Best= numeric(0),BestCount=numeric(0));
  c<-c[order(c[,11]),decreasing = TRUE] 	
  lil<-unique(c[,11])
  i<-1
  while(i < nrow(lil)){
    
    temp<-subset(c,album_name %in%lil[i,1])
    h<-60
    
    
    h<-sum(temp[,5])/3600000
    k<-bestofAlbumS(lil[i,1],temp)
    
    
    newrow = data.frame(Name=lil[i,1], Hours=h,Best=k[1,1],BestCount=k[1,2])
    df <- rbind(df, newrow)
    #print(newrow)
    i=1+i
  }
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
  
  
}

favTracks<-function(){
  x1 <- read_excel("spotify.xlsx")
  c1<-subset(x1, msPlayed >90000)
  df<-as.data.frame(table(c1$trackName))
  jj<-df[order(df[,2],decreasing = TRUE),]
  jj
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
  df = data.frame(Name = numeric(0), Month = numeric(0),Year= numeric(0));
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
      print (m)
      f<-favTracksyearmonth(y,m,x1)
      print(f[1])
      newrow = data.frame(Name=f[1], Month=mon,Year=y)
      df <- rbind(df, newrow)
      mon<-mon+1
    }
    y<-y+1
  }
  df
  
}
timesummarydetailed<-function(){
  df = data.frame(Name = numeric(0), Hour = numeric(0),Minute= numeric(0));
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
    
      newrow = data.frame(Name=f[1], Hour=ye,Minute=m)
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
  sum(c1$msPlayed)/(1000*3600)
  
  
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
  df
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
  df
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