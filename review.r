require(plyr)
require(randomForest)
require(geosphere)
setwd('~/git/event')
source('util.r')
attend=read.csv('data/event_attendees.csv')
event=read.csv('data/events.csv')
#event=read.csv('data/user_friends.csv')
user=read.csv('data/users.csv',stringsAsFactors=FALSE)

user_coor=read.csv('user_coord2.csv',stringsAsFactors=FALSE)
user=merge(user,user_coor,by.x=c('location'),by.y=c('addr'))
#change column order
user=data.frame(user[,-1],location=user[,1])

train=read.csv('train.csv')
test=read.csv('test.csv')
friends=read.csv('data/user_friends.csv')
#test=xts(test[,-4],order.by=as.POSIXct(as.character(test$timestamp)))
#train=xts(train[,-4],order.by=as.POSIXct(as.character(train$timestamp)))

db=merge((train),user,by.y=1,by.x=1)
source('populiarity.r')
db=cbind(db,friends_yes=attend_yes,friends_no=attend_no,friends_maybe=attend_maybe,friends=attend_invited)
event_merge=merge(event,yes,by.x=1,by.y=1)
event_merge$populiarity=log(event_merge$populiarity+1)
db=merge(db,event_merge,by.y=1,by.x=2)
#####data preparation######


db=db[grep('^\\d{4}',db$birthyear),]
db$birthyear=as.numeric(as.character(db$birthyear))
db$start_time=as.POSIXct(strptime(as.character((db$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db$timestamp=as.POSIXct(strptime(as.character((db$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')
db$time_diff=as.numeric(difftime(db$start_time,db$timestamp,units=c('hours')))
#db$time_diff[db$time_diff<0]=.0000001

#db$time_diff=log(db$time_diff)

db$timezone[which(is.na(db$timezone))]=0
db$timezone=cut(round(db$timezone/60),breaks=seq(-14,14,2))
#factor(round(db$timezone/60),levels)

db$locale=factor(sapply(as.character(db$locale),function(x)strsplit(x,"_")[[1]][1]))

db$invited=factor(db$invited)
db$joinedAt=as.numeric(as.POSIXct(as.character((db$joinedAt)))-as.POSIXct('2000-01-01'))
db$gender=factor(db$gender)
db$friend_summary(db$friends_yes-db$friends_no+db$friends_maybe*.5+db$friends*.5)
#db$month=factor(format(db$start_time,'%m'))

#db$c_6[which(db$c_6>300)]=median(db$c_6)
#db$c_1[which(db$c_1>2000)]=median(db$c_1)
#db$c_52[which(db$c_52>200)]=median(db$c_52)
#db$c_5[which(db$c_5>1200)]=median(db$c_5)
#db$c_7[which(db$c_7>1200)]=median(db$c_7)
#location
tmp=which(!is.na(db$lat)&!is.na(db$lng)&!is.na(db$user_lat)&!is.na(db$user_long))
temp=distVincentyEllipsoid(p1 = cbind(db$user_long,db$user_lat)[tmp,], p2 = cbind(db$lng,db$lat)[tmp,])

db=data.frame(db,distance=db$lng)
db$distance=100000000#median(temp)
db$distance[tmp]=temp
unique_users=unique(db$user)#2015
set.seed(333)
train_nr=unique_users[sample(1:length(unique_users),1338)]#
train_nr=which(db$user %in%train_nr)
#for(z in seq(5,45,by=5)){
interested=db[train_nr,c(match(c('interested','not_interested','distance',
                                 'invited','birthyear','gender'
                    #             ,'user_id'
                     #            ,'month'
                         ,'timezone'
                              ,'locale'
                         ,'populiarity'
                         ,'time_diff'
                        ,'friends'
                        ,'friends_yes','friends_no','friends_maybe','joinedAt'
                         ),colnames(db))
                 ,grep('c_',colnames(db))
                 )]
set.seed(333)
features=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150)#,nodesize=1)
z=30
cols= rownames(importance(features)[order(importance(features)[,5],decreasing=TRUE),])[1:z][which(rownames(importance(features)[order(randomForest::importance(features)[,5],decreasing=TRUE),])[1:z]%in%rownames(importance(features)[order(randomForest::importance(features)[,4],decreasing=TRUE),])[1:z])]
#cols=rownames(importance(features)[order(randomForest::importance(features)[,5],decreasing=TRUE),])[1:22]
#23features [,5]==.70669
#30 features [,4+5]=.7046811
interested=db[train_nr,c(match(c('interested','not_interested',cols),colnames(db)))]
#interested[,grep('c_',colnames(interested))]=log(interested[,grep('c_',colnames(interested))])
#require(gbm)
set.seed(333)
rez=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150)#,nodesize=1)
#rez=gbm(interested ~ .,data=interested)
#summary(rez)
#pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested','invited'#,'locale'
#                                 ,'friends'
#                                 ,'friends_yes'#,'friends_no'#,'friends_maybe'
#                                 ,'populiarity','birthyear','gender','timezone','time_diff'),
#                           colnames(db)) ,grep('c_',colnames(db)))]
pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols),colnames(db)))]
pred=predict(rez,pred_data[,-4],type='prob')

benchmark_data=pred_data[,1:4]
benchmark_data[,4]=(benchmark_data[,3]-benchmark_data[,4])/2+.5

pred_data=cbind(pred_data[,1:3],pred[,3])

benchmark_rez=ddply(benchmark_data,.(user),function(x)
{
  data.frame(event=output(x,.99));
})
#for(i in seq(.2,.75,by=.05)){
pred_rez=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.0001));
})
#print(i)
print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',benchmark_rez[,2])),' '),strsplit(as.character(sub("[[:space:]]+$",'',pred_rez[,2])),' ')))

#0.7046811
#}
#}
#test

#final_model=db[,c(match(c('interested','not_interested','distance',
#                          'invited','birthyear','gender'
#                          ,'timezone'
#                          ,'locale'
#                          ,'populiarity'
#                          ,'time_diff'
#                          ,'friends'
#                          ,'friends_yes','friends_no','friends_maybe','joinedAt'
                          
#                          ),colnames(db)),grep('c_',colnames(db))
#)]
#interested[,grep('c_',colnames(interested))]=log(interested[,grep('c_',colnames(interested))])
#require(gbm)
#set.seed(333)
#final_model=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=final_model,importance=TRUE)#,ntree=500,nodesize=1)

#final_cols= rownames(importance(final_model)[order(randomForest::importance(final_model)[,5],decreasing=TRUE),])[1:30][which(rownames(importance(final_model)[order(randomForest::importance(final_model)[,5],decreasing=TRUE),])[1:30]%in%rownames(importance(final_model)[order(randomForest::importance(final_model)[,4],decreasing=TRUE),])[1:30])]

final_model=db[,c(match(c('interested','not_interested',cols),colnames(db)))]

set.seed(333)
final_model=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=final_model,importance=TRUE)#,ntree=500,nodesize=1)


db_test=merge((test),user,by.y=1,by.x=1)
db_test=merge(db_test,event_merge,by.y=1,by.x=2,all.x=TRUE)
#db_test=merge(db_test,event,by.y=1,by.x=2)
#####data preparation######
attend_yes_final=apply((db_test[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$yes),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
attend_no_final=apply((db_test[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$no),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
attend_maybe_final=apply((db_test[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$maybe),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
attend_invited_final=apply((db_test[,1:2]),1,function(x){
  fnd=strsplit(as.character(friends[which(friends$user==x[2]),]$friends),' ')[[1]]
  length(which(fnd %in% strsplit(as.character(attend[(which(attend$event%in%x[1])),]$invited),' ')[[1]]))/length(fnd)
  #print(paste(fnd,att))        
  #print(str(x))
})
db_test=cbind(db_test,friends=attend_invited_final,friends_yes=attend_yes_final,friends_no=attend_no_final,friends_maybe=attend_maybe_final)

#db_test=db_test[grep('^\\d{4}',db_test$birthyear),]
db_test$birthyear[-(grep('^\\d{4}',db_test$birthyear))]=1977
db_test$birthyear=as.numeric(as.character(db_test$birthyear))
db_test$start_time=as.POSIXct(strptime(as.character((db_test$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db_test$timestamp=as.POSIXct(strptime(as.character((db_test$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')
db_test$time_diff=as.numeric(difftime(db_test$start_time,db_test$timestamp,units=c('hours')))

db_test$time_diff=as.numeric(difftime(db_test$start_time,db_test$timestamp,units=c('hours')))
#db_test$time_diff[db_test$time_diff<0]=.0000001

#db_test$time_diff=log(db_test$time_diff)

db_test$invited=factor(db_test$invited)

db_test$timezone[which(is.na(db_test$timezone))]=0
db_test$timezone=cut(round(db_test$timezone/60),breaks=seq(-14,14,2))#factor(round(db_test$timezone/60))

#db_test$locale=factor(sapply(as.character(db_test$locale),function(x)strsplit(x,"_")[[1]][1]))
tmp=(sapply(as.character(db_test$locale),function(x)strsplit(x,"_")[[1]][1]))
#db_test$locale[which(db_test$locale%in%levels(db_test$locale)[which(!levels(db_test$locale)%in%levels(db$locale))])]='en'
tmp[which(tmp%in%unique(tmp)[which(!unique(tmp)%in%levels(db$locale))])]='en'
temp=as.factor(c(as.character(db$locale),tmp))
db_test$locale=(tail(temp,length(tmp)))#droplevels(db_test$locale)
db_test$joinedAt=as.numeric(as.POSIXct(as.character((db_test$joinedAt)))-as.POSIXct('2000-01-01'))

#location
tmp=which(!is.na(db_test$lat)&!is.na(db_test$lng)&!is.na(db_test$user_lat)&!is.na(db_test$user_long))
temp=distVincentyEllipsoid(p1 = cbind(db_test$user_long,db_test$user_lat)[tmp,], p2 = cbind(db_test$lng,db_test$lat)[tmp,])

db_test=data.frame(db_test,distance=db_test$lng)
db_test$distance=100000000
db_test$distance[tmp]=temp
db_test$gender=factor(db_test$gender)
tmp=factor(c(format(db$start_time,'%m'),format(db_test$start_time,'%m')))

#db_test$month=tmp[(length(db$start_time)+1):length(tmp)]
test_selected=db_test[,match(cols,colnames(db_test))]

#test_selected=db_test[,c(match(c('invited','birthyear','gender','time_diff'
#                                 ,'timezone'#,'locale'
#                                 ,'friends'
#                                 ,'friends_yes','friends_no','friends_maybe'
#                                 ,'populiarity'
#                                 ),colnames(db_test))
#           ,grep('c_',colnames(db_test)))]

#pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',
 #                                cols),colnames(db)))]

#locate 
#test_selected=test_selected[(which(!test_selected$locale%in%levels(test_selected$locale)[which(!levels(test_selected$locale)%in%levels(interested$locale))])),]
#test_selected$locale=droplevels(test_selected$locale)
#timezone
#test_selected=test_selected[(which(!test_selected$timezone%in%levels(test_selected$timezone)[which(!levels(test_selected$timezone)%in%levels(interested$timezone))])),]
#test_selected$timezone=droplevels(test_selected$timezone)

pred_test=predict(final_model,test_selected,type='prob')
pred_data=cbind(db_test[,1:3],pred_test[,3])#isskirti ir isrusiuoti


pred_data=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.0001));
})


#pred_data=res
colnames(pred_data)=c('User','Events')
pred_data$Events=gsub("[[:space:]]*$","",pred_data$Events)
write.csv2(pred_data,'result.csv',row.names=FALSE,quote=FALSE)