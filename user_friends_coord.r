#usr_coor=read.csv('user_coord.csv')
#tmp=merge(user,usr_coor,x.by=6,y.by=1)


user_friends_coord=apply((user[which(is.na(user$user_lat)),]),1,function(x){
  #get user's friends
  x[1]=as.numeric(x[1])
  #print(x[1])
  if(!any(friends$user==x[1]))#no friends
    return(data.frame(lat=NA,long=NA))
  fnd=strsplit(as.character(friends[which(friends$user==x[1]),]$friends),' ')[[1]]
    
  rez=user[which(user$user_id%in%fnd),]
  #print(x[1])
  lat=rez[which(!is.na(rez$user_lat)),]$user_lat
  #print(length(lat))
  long=rez[which(!is.na(rez$user_lat)),]$user_long
  #print(long)
  #print((lat))
  if(length(lat)>3 & !(length(unique(lat))==1 & length(unique(long))==1)){
    tryCatch({
      set.seed(33344)
      if(length(unique(lat))>2 | length(unique(long))>2&length(lat)>9)
        kmean=kmeans(data.frame(lat,long),3)
      else
        kmean=kmeans(data.frame(lat,long),2)
    k_max=aggregate(kmean$cluster,list(kmean$cluster),length)
    rez=c(as.numeric(t(kmean$centers[which(max(k_max[,2])==k_max[,2]),])))#,paste(lat))
    },
      warning=function(w){print('warn')},
      error=function(e){return(data.frame(lat=lat[1],long=long[1]))
    })
  }
  else
    rez=data.frame(lat=lat[1],long=long[1])
  (rez)
  
})
user$user_lat[which(is.na(user$user_lat))]=unlist(lapply(user_friends_coord,function(x)x[1]))
user$user_long[which(is.na(user$user_long))]=unlist(lapply(user_friends_coord,function(x)x[2]))

#temp=lapply(seq_along((user_friends_coord)),function(i) 
#  user[  as.numeric(names((user_friends_coord))[[i]]),c('user_lat','user_long')]=(user_friends_coord)[[i]])
#)
#user_friends_coord=do.call(rbind,user_friends_coord)
#rfImpute(user)