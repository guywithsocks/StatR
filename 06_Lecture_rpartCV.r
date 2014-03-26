rparTune=function(formula,data,cvid,predtype='class',cpvec,...)
##' formula,data: self-explanatory
##' cvid: a vector of CV group assignments. Its length must equal the length of 'data'
##' predtype: passed on to predict.rpart as 'type'
##' cpvec: vector of Cp values to try out
##' ...: arguments passed on to rpart
{
require(rpart)
cat(date(),'\n')
cvg=unique(cvid) ### how many CV groups?
n=dim(data)[1]

cout=matrix(NA,nrow=n,ncol=length(cpvec)) # matrix of predictions. Each column represents a different Cp setting.

for (b in 1:length(cpvec))
{
	for (a in cvg)
	{
		ktree=rpart(formula=formula,data=data[cvid!=a,],control=rpart.control(cp=cpvec[b]),...)
		cout[cvid==a,b]=predict(ktree,newdata=data[cvid==a,],type=predtype)
	}
	cat('.')
}
cat(date(),'\n')
return(cout)
}
	