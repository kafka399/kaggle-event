require(XML)
user=read.csv('f:./users.csv')

addr=as.character(levels(user$location))



url = paste('http://maps.google.com/maps/api/geocode/xml?address=',addr,'&sensor=false',sep='') 

rez=data.frame(addr=addr,lat='',long='',stringsAsFactors=FALSE)

for(i in c(2503:length(addr))){
  print(url[i])
  doc = xmlTreeParse(url[i])
  root = xmlRoot(doc)
  rez[i,2]= xmlValue(root[['result']][['geometry']][['location']][['lat']])
  rez[i,3]= xmlValue(root[['result']][['geometry']][['location']][['lng']]) 
  Sys.sleep(1/4)
}

write.csv(rez,'user_coord.csv')

##### fix for user's coordinates where name is mixed with number, ex: Paris 09
user_coor=read.csv('user_coord.csv')
tmp=sapply((user_coor$addr),function(x){
  addr=strsplit(x," ")
  ifelse(length(grep("[[:digit:]]",(addr[[1]][length(addr[[1]])])))>0,1,0)
})

addr=as.character(sapply(user_coor$addr[which(as.numeric(tmp)==1)],function(x){
  y=strsplit(x,"  ")[[1]]
  y[[1]][1:(length(y[[1]])-1)]
}))

url = paste('http://maps.google.com/maps/api/geocode/xml?address=',addr,'&sensor=false',sep='') 

rez=data.frame(addr=addr,lat='',long='',stringsAsFactors=FALSE)

for(i in c(1:length(addr))){
  print(url[i])
  doc = xmlTreeParse(url[i])
  root = xmlRoot(doc)
  rez[i,2]= xmlValue(root[['result']][['geometry']][['location']][['lat']])
  rez[i,3]= xmlValue(root[['result']][['geometry']][['location']][['lng']]) 
  Sys.sleep(1/4)
}
user_coor[tmp,]
