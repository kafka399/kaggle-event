setwd('~/git/event')
source('util.r')
attend=read.csv('data/event_attendees.csv')
event=read.csv('data/events.csv')
#event=read.csv('data/user_friends.csv')
user=read.csv('data/users.csv')
train=read.csv('train.csv')
test=read.csv('test.csv')

db=merge((train),user,by.y=1,by.x=1)
db=merge(db,event,by.y=1,by.x=2)
#####data preparation######


db=db[grep('^\\d{4}',db$birthyear),]
db$birthyear=as.numeric(as.character(db$birthyear))
db$start_time=as.POSIXct(strptime(as.character((db$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db$timestamp=as.POSIXct(strptime(as.character((db$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')
db$time_diff=as.numeric(difftime(db$start_time,db$timestamp,units=c('hours')))

db$timezone[which(is.na(db$timezone))]=0
db$timezone=factor(round(db$timezone/60))

db$locale=factor(sapply(as.character(db$locale),function(x)strsplit(x,"_")[[1]][1]))

interested=db[,c(match(c('interested','not_interested','invited','birthyear','gender'
                         #,'timezone','locale'
                         ,'time_diff'
                         ),colnames(db))
                 ,grep('c_',colnames(db))
                 )]
#interested[,grep('c_',colnames(interested))]=log(interested[,grep('c_',colnames(interested))])
#require(gbm)

rez=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE)#,ntree=300,nodesize=1)
#rez=gbm(interested ~ .,data=interested)
#summary(rez)
pred_data=head(db[,c(match(c('event','user','interested','invited','locale','birthyear','gender','timezone','time_diff'),
                           colnames(db)) ,grep('c_',colnames(db)))],5000)

pred=predict(rez,pred_data[,-3],type='prob')

pred_data=cbind(head(pred_data[,1:3],5000),pred[,3])

benchmark_data=pred_data[,1:4]
benchmark_data[,4]=benchmark_data[,3]


output=function(y){
  y=y[order(y[,4],decreasing=TRUE),]
  paste((ifelse(y[,4]>.2,y[,1],'')),collapse=' ')
}
  

pred_data=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x));
})
benchmark_data=ddply(benchmark_data,.(user),function(x)
{
  data.frame(event=output(x));
})

#random
mapk(200,head(db[,c(match(c('event','user','interested'),colnames(db)))],500),cbind(head(db[,c(match(c('event','user'),colnames(db)))],500),rez2))
#mine
mapk(200,benchmark_data[,2],pred_data[,2])

#test
db_test=merge((test),user,by.y=1,by.x=1)
db_test=merge(db_test,event,by.y=1,by.x=2)
#####data preparation######


#db_test=db_test[grep('^\\d{4}',db_test$birthyear),]
db_test$birthyear[-(grep('^\\d{4}',db_test$birthyear))]=1977
db_test$birthyear=as.numeric(as.character(db_test$birthyear))
db_test$start_time=as.POSIXct(strptime(as.character((db_test$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db_test$timestamp=as.POSIXct(strptime(as.character((db_test$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')
db_test$time_diff=as.numeric(difftime(db_test$start_time,db_test$timestamp,units=c('hours')))

db_test$timezone[which(is.na(db_test$timezone))]=0
db_test$timezone=factor(round(db_test$timezone/60))

db_test$locale=factor(sapply(as.character(db_test$locale),function(x)strsplit(x,"_")[[1]][1]))

test_selected=db_test[,c(match(c('invited','birthyear','gender','time_diff'
                                 #,'timezone','locale'
                                 ),colnames(db_test))
           ,grep('c_',colnames(db_test)))]

#locate 
test_selected=test_selected[(which(!test_selected$locale%in%levels(test_selected$locale)[which(!levels(test_selected$locale)%in%levels(interested$locale))])),]
test_selected$locale=droplevels(test_selected$locale)
#timezone
test_selected=test_selected[(which(!test_selected$timezone%in%levels(test_selected$timezone)[which(!levels(test_selected$timezone)%in%levels(interested$timezone))])),]
test_selected$timezone=droplevels(test_selected$timezone)

pred_test=predict(rez,test_selected,type='prob')
pred_data=cbind(db_test[,1:3],pred_test[,3])

pred_data=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x));
})
write.csv2(pred_data,'result.csv',row.names=FALSE,quote=FALSE)