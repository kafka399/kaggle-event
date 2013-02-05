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

rez_svm=train(factor((interested-not_interested)/2+.5) ~ .,data=tmp, method = "svmPoly",
              trControl=bootControl)#,tuneGrid = my.grid)#,scaled=FALSE, preProcess = c("center", "scale"))

#my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
#rez_nn=train(factor((interested-not_interested)/2+.5) ~ .,data=interested, method = "svm")   
pred_data=db[-train_nr,c(match(c('event','user','interested','not_interested',cols),colnames(db)))]
tmp=pred_data[,which(as.character(sapply(pred_data,class))%in%c('integer','numeric'))]
temp=preProcess(tmp[,5:ncol(tmp)])
set.seed(333)
nasa=predict(temp,tmp[,5:ncol(tmp)])
tmp=pred_data
tmp[,tail(which(as.character(sapply(pred_data,class))%in%c('integer','numeric')),-4)]=nasa

pred_svm=predict(rez_svm, newdata = tmp[,-4])

pred_rez_svm=ddply(data.frame(pred_data[,1:3],pred_svm),.(user),function(x)
{
  data.frame(event=output(x,.5998));
})
#print(i)
table(ifelse(pred_svm>.2,1,0),benchmark_data[,4])
print(mapk(200,strsplit(as.character(sub("[[:space:]]+$",'',benchmark_rez[,2])),' '),strsplit(as.character(sub("[[:space:]]+$",'',pred_rez_svm[,2])),' ')))


####submit####

final_model=db[,c(match(c('interested','not_interested',cols),colnames(db)))]

tmp=final_model[,which(as.character(sapply(final_model,class))%in%c('integer','numeric'))]
temp=preProcess(tmp[,3:ncol(tmp)])
set.seed(333)
nasa=predict(temp,tmp[,3:ncol(tmp)])
tmp=interested
tmp[,tail(which(as.character(sapply(interested,class))%in%c('integer','numeric')),-2)]=nasa
tmp=tmp[,!colnames(tmp)%in%c('event','user')]

my.grid = expand.grid(.interaction.depth = c(3,9,15), .n.trees = (3:5)*50, .shrinkage = .1)

rez_svm=train(((interested-not_interested)/2+.5) ~ .,data=tmp, method = "gbm",
              trControl=bootControl,tuneGrid = my.grid)#,scaled=FALSE, preProcess = c("center", "scale"))

##############

test_selected=db_test[,match(cols,colnames(db_test))]

tmp=test_selected[,which(as.character(sapply(test_selected,class))%in%c('integer','numeric'))]
temp=preProcess(tmp)
set.seed(333)
nasa=predict(temp,tmp)
tmp=test_selected
tmp[,which(as.character(sapply(test_selected,class))%in%c('integer','numeric'))]=nasa

pred_test=predict(rez_svm, newdata = tmp)

pred_data=cbind(db_test[,1:3],pred_test)#isskirti ir isrusiuoti


pred_data=ddply(pred_data,.(user),function(x)
{
  data.frame(event=output(x,.3));
})


#pred_data=res
colnames(pred_data)=c('User','Events')
pred_data$Events=gsub("[[:space:]]*$","",pred_data$Events)
write.csv2(pred_data,'result.csv',row.names=FALSE,quote=FALSE)