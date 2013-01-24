
yes= data.frame(event=as.character(attend[,1]),populiarity=sapply(strsplit(as.character((attend[,2])),' '), length))
yes$populiarity=yes$populiarity#(cut(log(yes$populiarity+1),breaks=seq(0,12,by=4),right=FALSE))
#yes$populiarity/max(yes$populiarity)
#yes=(yes[order(yes$populiarity,decreasing=TRUE),])

#rownames(yes)=(nrow(yes):1)
#yes$populiarity=as.numeric(rownames(yes))/nrow(yes)
#yes=data.frame(event = rep.int(attend$event, sapply(yes, length)), user = unlist(yes))
sapply(tmp,length)

#attend_yes=data.frame(event=rep.int(attend[,1],sapply(strsplit(as.character((attend[,2])),' '), length)),user=unlist(strsplit(as.character((attend[,2])),' ')),attend=1)

#tail(merge(db,attend_yes,x.by=c(1,2),y.by=c(1,2),all.x=TRUE))

tail(db[,1:2])

strsplit(as.character(attend[(which(attend$event%in%tail(db[,1:2],1)[,1])),]$invited),' ')

strsplit(as.character(friends[which(friends$user==tail(db[,1:2],1)[,2]),]$friends),' ')

length(which(strsplit(as.character(friends[which(friends$user==tail(db[,1:2],1)[,2]),]$friends),' ')[[1]]%in%
        strsplit(as.character(attend[(which(attend$event%in%tail(db[,1:2],1)[,1])),]$invited),' ')[[1]]))/
  length(strsplit(as.character(friends[which(friends$user==tail(db[,1:2],1)[,2]),]$friends),' ')[[1]])

attend_yes=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$yes),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
attend_no=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$no),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
attend_maybe=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$maybe),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
attend_invited=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$invited),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
db=cbind(db,friends_yes=attend_yes,friends_no=attend_no,friends_maybe=attend_maybe,friends_invited=attend_invited)