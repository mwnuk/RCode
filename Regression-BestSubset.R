# Best Subset Selection


library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)

#remove NAs
	sum(is.na(Hitters$Salary))
	Hitters=na.omit(Hitters)
	dim(Hitters)
	sum(is.na(Hitters))
#
	library(leaps)
	regfit.full=regsubsets(Salary~.,Hitters)
	summary(regfit.full)

	regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
	reg.summary=summary(regfit.full)
	names(reg.summary)
#
	reg.summary$rsq
	par(mfrow=c(2,2))
	plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
	plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
	which.max(reg.summary$adjr2)
	points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
	plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
	which.min(reg.summary$cp)
	points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
	which.min(reg.summary$bic)
	plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
	points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
	plot(regfit.full,scale="r2")
	plot(regfit.full,scale="adjr2")
	plot(regfit.full,scale="Cp")
	plot(regfit.full,scale="bic")
	coef(regfit.full,6)

