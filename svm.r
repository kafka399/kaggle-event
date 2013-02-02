require(e1071)
z=30
cols= rownames(importance(features)[order(importance(features)[,5],decreasing=TRUE),])[1:z][which(rownames(importance(features)[order(randomForest::importance(features)[,5],decreasing=TRUE),])[1:z]%in%rownames(importance(features)[order(randomForest::importance(features)[,4],decreasing=TRUE),])[1:z])]

pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols),colnames(db)))]
#svm_tune= tune.svm(factor((interested-not_interested)/2+.5) ~ .,data=interested, gamma=.5,cost=4)
rez_svm=gbm(factor((interested-not_interested)/2+.5) ~ .,data=interested,probability=TRUE,gamma=.01)
pred_svm=predict(rez_svm,pred_data[,-4])
table(fitted(rez_svm,interested),(interested$interested-interested$not_interested)/2+.5)
pred=attr(pred,'probabilities')

benchmark_data=pred_data[,1:4]
benchmark_data[,4]=(benchmark_data[,3]-benchmark_data[,4])/2+.5

pred_data=cbind(pred_data[,1:3],pred[,1])


benchmark_rez=ddply(benchmark_data,.(user),function(x)
{
  data.frame(event=output(x,.99));
})
#for(i in seq(.2,.75,by=.05)){
pred_rez=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.59));
})
#print(i)
print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',benchmark_rez[,2])),' '),strsplit(as.character(sub("[[:space:]]+$",'',pred_rez[,2])),' ')))


require(caret)
require(doMC)
registerDoMC()
bootControl<-trainControl(number=3,method='cv')
set.seed(333)
tmp=interested[,which(as.character(sapply(interested,class))%in%c('integer','numeric'))]
temp=preProcess(tmp[,3:ncol(tmp)])
nasa=predict(temp,tmp[,3:ncol(tmp)])
tmp=interested
tmp[,tail(which(as.character(sapply(interested,class))%in%c('integer','numeric')),-2)]=nasa
tmp=tmp[,!colnames(tmp)%in%c('event','user')]
rez_svm=train(((interested-not_interested)/2+.5) ~ .,data=tmp, method = "gbm",
              trControl=bootControl)#,scaled=FALSE, preProcess = c("center", "scale"))
#my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
#rez_nn=train(factor((interested-not_interested)/2+.5) ~ .,data=interested, method = "svm")   
pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols),colnames(db)))]
tmp=pred_data[,which(as.character(sapply(pred_data,class))%in%c('integer','numeric'))]
temp=preProcess(tmp[,5:ncol(tmp)])
nasa=predict(temp,tmp[,5:ncol(tmp)])
tmp=pred_data
tmp[,tail(which(as.character(sapply(pred_data,class))%in%c('integer','numeric')),-4)]=nasa

pred=predict(rez_svm, newdata = tmp[,-4])

pred_rez=ddply(data.frame(pred_data[,1:3],pred),.(user),function(x)
{
  data.frame(event=output(x,.3));
})
#print(i)
table(ifelse(pred>.3,1,0),benchmark_data[,4])
print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',benchmark_rez[,2])),' '),strsplit(as.character(sub("[[:space:]]+$",'',pred_rez[,2])),' ')))
