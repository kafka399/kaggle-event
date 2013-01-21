require(plyr)
require(randomForest)
setwd('~/git/event')
source('util.r')
attend=read.csv('data/event_attendees.csv')
event=read.csv('data/events.csv')
#event=read.csv('data/user_friends.csv')
user=read.csv('data/users.csv')
train=read.csv('train.csv')
test=read.csv('test.csv')

db=merge((train),user,by.y=1,by.x=1)
source('populiarity.r')
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
db$timezone=cut(round(db$timezone/60),breaks=seq(-14,14,2))#factor(round(db$timezone/60),levels)

db$locale=factor(sapply(as.character(db$locale),function(x)strsplit(x,"_")[[1]][1]))

#db$invited=factor(db$invited)

unique_users=unique(db$user)#2015
set.seed(333)
train_nr=unique_users[sample(1:length(unique_users),1700)]#
train_nr=which(db$user %in%train_nr)
interested=db[train_nr,c(match(c('interested','not_interested','invited','birthyear','gender'
                         ,'timezone'
                              #,'locale'
                         ,'populiarity'
                         ,'time_diff'
                         ),colnames(db))
                 ,grep('c_',colnames(db))
                 )]
#interested[,grep('c_',colnames(interested))]=log(interested[,grep('c_',colnames(interested))])
#require(gbm)
set.seed(333)
rez=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150)#,nodesize=1)
#rez=gbm(interested ~ .,data=interested)
#summary(rez)
pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested','invited','locale','populiarity','birthyear','gender','timezone','time_diff'),
                           colnames(db)) ,grep('c_',colnames(db)))]

pred=predict(rez,pred_data[,-4],type='prob')

benchmark_data=pred_data[,1:4]
benchmark_data[,4]=(benchmark_data[,3]-benchmark_data[,4])/2+.5

pred_data=cbind(pred_data[,1:3],pred[,3])



pred_rez=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,-.00001));
})
benchmark_rez=ddply(benchmark_data,.(user),function(x)
{
  data.frame(event=output(x,.49));
})
(mapk(200,strsplit(as.character(benchmark_rez[,2]),' '),strsplit(as.character(pred_rez[,2]),' ')))


#test

final_model=db[,c(match(c('interested','not_interested','invited','birthyear','gender'
                                 ,'timezone'
                                 ,'populiarity'
                                 #,'locale'
                                 ,'time_diff'),colnames(db)),grep('c_',colnames(db))
)]
#interested[,grep('c_',colnames(interested))]=log(interested[,grep('c_',colnames(interested))])
#require(gbm)
set.seed(333)
final_model=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=final_model,importance=TRUE)#,ntree=300,nodesize=1)


db_test=merge((test),user,by.y=1,by.x=1)
db_test=merge(db_test,event_merge,by.y=1,by.x=2)
#db_test=merge(db_test,event,by.y=1,by.x=2)
#####data preparation######


#db_test=db_test[grep('^\\d{4}',db_test$birthyear),]
db_test$birthyear[-(grep('^\\d{4}',db_test$birthyear))]=1977
db_test$birthyear=as.numeric(as.character(db_test$birthyear))
db_test$start_time=as.POSIXct(strptime(as.character((db_test$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db_test$timestamp=as.POSIXct(strptime(as.character((db_test$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')
db_test$time_diff=as.numeric(difftime(db_test$start_time,db_test$timestamp,units=c('hours')))

db_test$time_diff=as.numeric(difftime(db_test$start_time,db_test$timestamp,units=c('hours')))
#db_test$time_diff[db_test$time_diff<0]=.0000001

#db_test$time_diff=log(db_test$time_diff)

#db_test$invited=factor(db_test$invited)

db_test$timezone[which(is.na(db_test$timezone))]=0
db_test$timezone=cut(round(db_test$timezone/60),breaks=seq(-14,14,2))#factor(round(db_test$timezone/60))

db_test$locale=factor(sapply(as.character(db_test$locale),function(x)strsplit(x,"_")[[1]][1]))

test_selected=db_test[,c(match(c('invited','birthyear','gender','time_diff'
                                 ,'timezone'#,'locale'
                                 
                                 ,'populiarity'
                                 ),colnames(db_test))
           ,grep('c_',colnames(db_test)))]

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