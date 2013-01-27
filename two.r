#location
tmp=which(!is.na(db$lat)&!is.na(db$lng)&!is.na(db$user_lat)&!is.na(db$user_long))
temp=distVincentyEllipsoid(p1 = cbind(db$user_long,db$user_lat)[tmp,], p2 = cbind(db$lng,db$lat)[tmp,])

#db=data.frame(db,distance=db$lng)
db$distance=median(temp)
db$distance[tmp]=temp

unique_users=unique(db$user)#2015
set.seed(333)
train_nr=unique_users[sample(1:length(unique_users),1338)]#
train_nr=which(db$user %in%train_nr)
#for(z in seq(32,40,by=2)){
#users have coordinates
interested_coord=db[(train_nr),c(match(c('interested','not_interested','distance',
                                 'invited','birthyear','gender'
                                 ,'timezone'
                                 ,'locale'
                                 ,'populiarity'
                                 ,'time_diff'
                                 ,'friends'
                                 ,'friends_yes','friends_no','friends_maybe','joinedAt'
),colnames(db))
                         ,grep('c_',colnames(db))
)]
interested_coord=interested_coord[which(!is.na(interested_coord$distance)),]
set.seed(333)
features_coord=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested_coord,importance=TRUE,ntree=150)#,nodesize=1)

cols_coord= rownames(importance(features_coord)[order(randomForest::importance(features_coord)[,5],decreasing=TRUE),])[1:30][which(rownames(importance(features_coord)[order(importance(features_coord)[,5],decreasing=TRUE),])[1:30]%in%rownames(importance(features_coord)[order(randomForest::importance(features_coord)[,4],decreasing=TRUE),])[1:30])]
interested_coord=db[train_nr,c(match(c('interested','not_interested',cols_coord),colnames(db)))]
interested_coord=interested_coord[which(!is.na(interested_coord$distance)),]
set.seed(333)
rez_coord=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested_coord,importance=TRUE,ntree=150)#,nodesize=1)

interested=db[train_nr,c(match(c('interested','not_interested','distance',
                                                     'invited','birthyear','gender'
                                                     ,'timezone'
                                                     ,'locale'
                                                     ,'populiarity'
                                                     ,'time_diff'
                                                     ,'friends'
                                                     ,'friends_yes','friends_no','friends_maybe','joinedAt'
),colnames(db))
                                             ,grep('c_',colnames(db))
)]
interested=interested[which(is.na(interested$distance)),]
interested=interested[-which(colnames(interested)=='distance')]
set.seed(333)
features=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150)#,nodesize=1)

cols= rownames(importance(features)[order(randomForest::importance(features)[,5],decreasing=TRUE),])[1:30][which(rownames(importance(features)[order(randomForest::importance(features)[,5],decreasing=TRUE),])[1:30]%in%rownames(importance(features)[order(randomForest::importance(features)[,4],decreasing=TRUE),])[1:30])]
interested=interested[,c(match(c('interested','not_interested',cols),colnames(interested)))]

set.seed(333)
rez=randomForest(factor((interested-not_interested)/2+.5) ~ .,data=interested,importance=TRUE,ntree=150)#,nodesize=1)

pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols_coord),colnames(db)))]
pred=predict(rez_coord,pred_data[which(!is.na(pred_data$distance)),-4],type='prob')

pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested','distance',cols),colnames(db)))]
pred=rbind(predict(rez,pred_data[which(is.na(pred_data$distance)),-4],type='prob'),pred)
pred=pred[order(as.numeric(rownames(pred))),]


benchmark_data=pred_data[,1:4]
benchmark_data[,4]=(benchmark_data[,3]-benchmark_data[,4])/2+.5

pred_data=cbind(pred_data[,1:3],pred[,3])

benchmark_rez=ddply(benchmark_data,.(user),function(x)
{
  data.frame(event=output(x,.99));
})
#for(i in seq(.25,.35,by=.01)){
pred_rez=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.29));
})
#print(z)
print(mapk(200,strsplit(as.character(benchmark_rez[,2]),' '),strsplit(as.character(pred_rez[,2]),' ')))
