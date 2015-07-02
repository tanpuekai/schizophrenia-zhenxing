library(ROCR)
load("input-for-algo/step2-input-data-for-random-forest.RData")
y<-as.factor(rep(seq(2),c(114,545)))
tmp<-rbind(as.matrix(trainMatCase[,-c(1:5)]),
	as.matrix(trainMatControl[,-c(1:5)]))
x<-matrix(NA,nrow(tmp),ncol(tmp))
for(i in 1:25){
	x[,i]<-as.numeric(tmp[,i])
}

############################
args <- commandArgs(trailingOnly = TRUE)
indtask<-as.numeric(args)
folds<-seq(3,10)
numfold<-folds[indtask]

numiter<-100

AUC<-matrix(NA,numiter,numfold)


for(k in 1:numiter){
	dat<-data.frame(y,x)
	X.imputed <- rfImpute(y ~ .,dat)

	turns<-sample(1:numfold,659,replace=T)
	for(i in 1:numfold){
		indtest<- which(turns==i)
		dattrain<-X.imputed[-indtest,]
		dattest<-X.imputed[indtest,]

		variants.rf <- randomForest(y ~ ., dattrain,importance=TRUE,proximity=TRUE)
		print(variants.rf)

		heldout.rf.pr = predict(variants.rf,type="prob",newdata=dattest[,-1])[,2]
		heldout.rf.pred = prediction(heldout.rf.pr, dattest[,1])

		heldout.rf.perf = performance(heldout.rf.pred,"tpr","fpr")
	 
		plot(heldout.rf.perf,main=c(k,i),col=2,lwd=2)
		abline(a=0,b=1,lwd=2,lty=2,col="gray")
 
		auc <- performance(heldout.rf.pred,"auc")
 
		AUC[k,i] <- unlist(slot(auc, "y.values"))
	}

	print(AUC[k,])
}


print(c("numfold=",numfold, mean(AUC), sd(AUC)))




