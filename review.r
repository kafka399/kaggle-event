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

interested=tail(db[,c(match(c('interested','not_interested','invited','locale','birthyear','gender','timezone'
                         ,'time_diff'
                         ),colnames(db))
                 ,grep('c_',colnames(db))
                 )],5000)
#interested[,grep('c_',colnames(interested))]=log(interested[,grep('c_',colnames(interested))])
#require(gbm)

rez=randomForest(factor(interested-not_interested) ~ .,data=interested,importance=TRUE)#,ntree=300,nodesize=1)
#rez=gbm(interested ~ .,data=interested)
#summary(rez)
pred=predict(rez,head(db[,c(match(c('invited','locale','birthyear','gender','timezone','time_diff'),colnames(db)) ,grep('c_',colnames(db)))],500),type='prob')
mapk(2,head(interested$interested,500),pred[,2])