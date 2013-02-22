require(plyr)
require(randomForest)
#require(geosphere)
setwd('~/git/event')
source('util.r')
attend=read.csv('data/event_attendees.csv')
event=read.csv('data/events.csv')
user=read.csv('data/users.csv',stringsAsFactors=FALSE)


train=read.csv('train.csv')
test=read.csv('test.csv')
friends=read.csv('data/user_friends.csv')
#train=(train[!duplicated(train[,c('user','event')]),])

db=merge((train),user,by.y=1,by.x=1)

source('populiarity.r')

db=cbind(db,friends_yes=attend_yes,friends_no=attend_no,friends_maybe=attend_maybe,friends=attend_invited)
event_merge=merge(event,yes,by.x=1,by.y=1)
event_merge$populiarity=log(event_merge$populiarity+1)
db=merge(db,event_merge,by.y=1,by.x=2)
#####data preparation######

db$birthyear[-(grep('^\\d{4}',db$birthyear))]=median(as.numeric(db$birthyear[(grep('^\\d{4}',db$birthyear))]))#1977
db$birthyear=as.numeric(as.character(db$birthyear))

db$start_time=as.POSIXct(strptime(as.character((db$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db$timestamp=as.POSIXct(strptime(as.character((db$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')

db$timezone[which(is.na(db$timezone))]=0

db$time_diff=as.numeric(difftime(db$start_time,db$timestamp,units=c('hours')))#-db$timezone/60
db$timezone=cut(round(db$timezone/60),breaks=seq(-14,14,2))

db$locale=factor(sapply(as.character(db$locale),function(x)strsplit(x,"_")[[1]][1]))

db$invited=factor(db$invited)
db$joinedAt=as.numeric(as.POSIXct(as.character((db$joinedAt)))-as.POSIXct('2000-01-01'))
db$gender=factor(db$gender)
db$weekdays=factor(format(db$timestamp,'%a'))#cut(as.numeric(format(db$timestamp,'%H')),breaks=seq(from=0,to=24,by=3),right=FALSE)##

db$start_hour=factor(format(db$start_time,'%H'))#+user_time_diff*60
tmp=sapply((db$location),function(x){
 addr=strsplit(x," ")
 ifelse(length(grep("[[:digit:]]",(addr[[1]][length(addr[[1]])])))>0,1,0)
})

addr=as.character(sapply(db$location[which(as.numeric(tmp)==1)],function(x){
 y=strsplit(x,"  ")[[1]]
 y[[1]][1:(length(y[[1]])-1)]
}))
db$location[which(as.numeric(tmp)==1)]=addr

db$location_mat=apply(db[,c('country','location')],1,function(x)ifelse(nchar(as.character(x[1]))>1,grep(as.character(x[1]),as.character(x[2]),ignore.case=TRUE),0))+
  apply(db[,c('city','location')],1,function(x)ifelse(nchar(as.character(x[1]))>1,grep(as.character(x[1]),as.character(x[2]),ignore.case=TRUE),0))+
  apply(db[,c('state','location')],1,function(x)ifelse(nchar(as.character(x[1]))>1,grep(as.character(x[1]),as.character(x[2]),ignore.case=FALSE),0))

db$location_mat[is.na(db$location_mat)]=0


unique_users=unique(db$user)#2015
set.seed(333)
train_nr=unique_users[sample(1:length(unique_users),1338)]#
train_nr=which(db$user %in%train_nr)


####feature selection#####
interested=db[,c(match(c('interested','not_interested',#'distance',#'frequency',
                                 'invited','birthyear','gender'
                        #         ,'weekdays','start_hour'
                       #          ,'hour'
                    #             ,'user_id'
                     #            ,'month'
                         ,'timezone'
                          #       ,'weekdays','start_hour'
                              ,'locale'
                         ,'populiarity'
                         ,'time_diff'
                        ,'friends'
                        ,'friends_yes','friends_no','friends_maybe','joinedAt'
                         ),colnames(db))
                 ,grep('c_',colnames(db))
                         ,grep('location',colnames(db))
                 )]

set.seed(333)
features=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150)#,nodesize=1)

z=30
cols= rownames(importance(features)[order(importance(features)[,5],decreasing=TRUE),])[1:z][which(rownames(importance(features)[order(randomForest::importance(features)[,5],decreasing=TRUE),])[1:z]%in%rownames(importance(features)[order(randomForest::importance(features)[,4],decreasing=TRUE),])[1:z])]
cols=c(cols[1:19],'weekdays','start_hour')

#hardcoded features after cherry picking
cols=c("time_diff","friends",  "populiarity",
       #"country",
       #"distance", 
       "joinedAt", "birthyear","c_other",  "friends_yes", 
       "timezone","friends_maybe","c_6","friends_no","c_1", "locale", "c_52","c_3", "c_21", "c_2", "c_4", "weekdays",
       "start_hour",
       "location_mat"
       #"country.1", "countryIndonesia","countryCambodia",
       #"locationSpain",,"locationYogyakarta","location.1","locationCalifornia"
       #locationIndonesia","locationOntario"
      # ,"countryUnited.States", "countryDominican.Republic", "countryCanada","countryMauritius","countryUnited.Kingdom", "countryItaly" ,  "countryAustralia", "countryGreece" ,"countryHong.Kong","countryIndia", "countryUganda","countryFinland","countrySaint.Vincent.and.the.Grenadines", "countrySingapore","countryMexico","countrySpain","countryFrance","countryPakistan","countrySwitzerland"
       )
#cols=c("time_diff","friends",  "populiarity",   "joinedAt", "distance", "birthyear","c_other",  "friends_yes","timezone", "friends_maybe" ,"c_6", "friends_no","locale","c_1","c_2","c_52","c_3","c_5","c_4", "c_7", "c_9", "c_10","c_34")

interested=db[train_nr,c(match(c('interested','not_interested',cols),colnames(db)))]

set.seed(333)
rez=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150,nodesize=4)

pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols),colnames(db)))]
pred=predict(rez,pred_data[,-4],type='prob')

benchmark_data=pred_data[,1:4]
benchmark_data[,4]=(benchmark_data[,3]-benchmark_data[,4])/2+.5

pred_data=cbind(pred_data[,1:3],pred[,3])

benchmark_rez=ddply(benchmark_data,.(user),function(x)
{
  data.frame(event=output(x,.99));
})

pred_rez=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.0001));
})

print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',benchmark_rez[,2])),' '),strsplit(as.character(sub("[[:space:]]+$",'',pred_rez[,2])),' ')))


#final 0.6989714
#public 0.7284886

public=read.csv('public_leaderboard_solution.csv')
public=data.frame(event=public[,2],user=public[,1],0,1)

public_rez=ddply(public,.(user),function(x)
{
  data.frame(event=output(x,.99));
})




###############FINAL#############

ids=read.csv('event_popularity_benchmark_private_test_only.csv')

db_test=test
db_test=merge((db_test),user,by.y=1,by.x=1)
db_test=merge(db_test,event_merge,by.y=1,by.x=2,all.x=TRUE)

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
db_test$birthyear[-(grep('^\\d{4}',db_test$birthyear))]=median(db$birthyear)#1977
db_test$birthyear=as.numeric(as.character(db_test$birthyear))
db_test$start_time=as.POSIXct(strptime(as.character((db_test$start_time)),'%Y-%m-%dT%H:%M:%S'),tz='UTC')
db_test$timestamp=as.POSIXct(strptime(as.character((db_test$timestamp)),'%Y-%m-%d %H:%M:%S'),tz='UTC')
db_test$time_diff=as.numeric(difftime(db_test$start_time,db_test$timestamp,units=c('hours')))

db_test$invited=factor(db_test$invited)

db_test$timezone[which(is.na(db_test$timezone))]=0
test_user_time_diff=db_test$timezone
db_test$timezone=cut(round(db_test$timezone/60),breaks=seq(-14,14,2))#factor(round(db_test$timezone/60))

tmp=(sapply(as.character(db_test$locale),function(x)strsplit(x,"_")[[1]][1]))
tmp[which(tmp%in%unique(tmp)[which(!unique(tmp)%in%levels(db$locale))])]='en'
temp=as.factor(c(as.character(db$locale),tmp))
db_test$locale=(tail(temp,length(tmp)))#droplevels(db_test$locale)
db_test$joinedAt=as.numeric(as.POSIXct(as.character((db_test$joinedAt)))-as.POSIXct('2000-01-01'))


db_test$gender=factor(db_test$gender)

db_test$weekdays=factor(format(db_test$timestamp,'%a'))
db_test$start_hour=factor(format(db_test$start_time,'%H'))
db_test$seeing_time=cut(as.numeric(format(db_test$timestamp+test_user_time_diff*60,'%H')),breaks=seq(from=0,to=24,by=8),right=FALSE)

tmp=sapply((db_test$location),function(x){
  addr=strsplit(x," ")
  ifelse(length(grep("[[:digit:]]",(addr[[1]][length(addr[[1]])])))>0,1,0)
})

addr=as.character(sapply(db_test$location[which(as.numeric(tmp)==1)],function(x){
  y=strsplit(x,"  ")[[1]]
  y[[1]][1:(length(y[[1]])-1)]
}))
db_test$location[which(as.numeric(tmp)==1)]=addr

db_test$location_mat=(apply(db_test[,c('country','location')],1,function(x)ifelse(nchar(as.character(x[1]))>1,grep(as.character(x[1]),as.character(x[2]),ignore.case=TRUE),0))+
                        apply(db_test[,c('city','location')],1,function(x)ifelse(nchar(as.character(x[1]))>1,grep(as.character(x[1]),as.character(x[2]),ignore.case=TRUE),0))+
                        apply(db_test[,c('state','location')],1,function(x)ifelse(nchar(as.character(x[1]))>1,grep(as.character(x[1]),as.character(x[2]),ignore.case=FALSE),0)))
db_test$location_mat[is.na(db_test$location_mat)]=0


########predict final#########
tmp=data.frame(interested=1,not_interested=0,db_test[which(db_test$event%in%public$event &db_test$user%in%public$user),])
tmp=tmp[,c(match(c('interested','not_interested',cols),colnames(tmp)))]
final_model=rbind(db[,c(match(c('interested','not_interested',cols),colnames(db)))],tmp)

set.seed(333)
final_model3=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=final_model,importance=TRUE,nodesize=4)#,ntree=500,nodesize=1)

set.seed(33)
final_model1=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=final_model,importance=TRUE,nodesize=4)#,ntree=500,nodesize=1)

set.seed(3)
final_model2=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=final_model,importance=TRUE,nodesize=4)#,ntree=500,nodesize=1)

final_model=combine(final_model3,final_model1,final_model2)

test_selected=db_test[,match(cols,colnames(db_test))]

pred_test=predict(final_model,test_selected,type='prob')
pred_data=cbind(db_test[,1:3],pred_test[,3])


pred_data=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.0001));
})
tmp=merge(pred_data,public_rez,by.x=1,by.y=1,all.y=TRUE)
print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',tmp$event.y)),' '),strsplit(as.character(sub("[[:space:]]+$",'',tmp$event.x)),' ')))
#0.6971235
pred_data=pred_data[which(pred_data[,1]%in%ids[,1]),]

colnames(pred_data)=c('User','Events')
pred_data$Events=gsub("[[:space:]]*$","",pred_data$Events)
write.csv2(pred_data,'result.csv',row.names=FALSE,quote=FALSE)