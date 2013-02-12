#how many users are going to attend the event
yes= data.frame(event=as.character(attend[,1]),populiarity=sapply(strsplit(as.character((attend[,2])),' '), length))

#how many friends said yes
attend_yes=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[1]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[2])),]$yes),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
#how many friends said no
attend_no=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[1]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[2])),]$no),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
#how many friends said maybe
attend_maybe=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[1]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[2])),]$maybe),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
#how many friends are invited
attend_invited=apply((db[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[1]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[2])),]$invited),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})

