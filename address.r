require(XML)
user=read.csv('users.csv',stringsAsFactors=FALSE)

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
#write.csv(tmp[,2:4],'user_coord2.csv',row.names=FALSE)

