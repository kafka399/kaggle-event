require(e1071)
require(caret)
require(doMC)
registerDoMC()

bootControl<-trainControl(number=3,method='cv')
interested=db[train_nr,c(match(c('interested','not_interested',cols),colnames(db)))]

tmp=interested[,which(as.character(sapply(interested,class))%in%c('integer','numeric'))]
temp=preProcess(tmp[,3:ncol(tmp)])
set.seed(333)
nasa=predict(temp,tmp[,3:ncol(tmp)])
tmp=interested
tmp[,tail(which(as.character(sapply(interested,class))%in%c('integer','numeric')),-2)]=nasa
tmp=tmp[,!colnames(tmp)%in%c('event','user')]

my.grid = expand.grid(.interaction.depth = c(3,9,15), .n.trees = (3:5)*50, .shrinkage = .1)

rez_svm=train(((interested-not_interested)/2+.5) ~ .,data=tmp, method = "gbm",
              trControl=bootControl,tuneGrid = my.grid)#,scaled=FALSE, preProcess = c("center", "scale"))

#my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
#rez_nn=train(factor((interested-not_interested)/2+.5) ~ .,data=interested, method = "svm")   
pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols),colnames(db)))]
tmp=pred_data[,which(as.character(sapply(pred_data,class))%in%c('integer','numeric'))]
temp=preProcess(tmp[,5:ncol(tmp)])
set.seed(333)
nasa=predict(temp,tmp[,5:ncol(tmp)])
tmp=pred_data
tmp[,tail(which(as.character(sapply(pred_data,class))%in%c('integer','numeric')),-4)]=nasa

pred=predict(rez_svm$finalModel, newdata = tmp[,-4])

pred_rez=ddply(data.frame(pred_data[,1:3],pred),.(user),function(x)
{
  data.frame(event=output(x,.4));
})
#print(i)
table(ifelse(pred>.3,1,0),benchmark_data[,4])
print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',benchmark_rez[,2])),' '),strsplit(as.character(sub("[[:space:]]+$",'',pred_rez[,2])),' ')))
