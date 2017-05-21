

#Second data sets: reponse= (G3 is)the final year grade of secondary education;variables=33, observations=649;regression;http://archive.ics.uci.edu/ml/datasets/Student+Performance
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#This data approach student achievement in secondary education of two Portuguese schools.
# The data attributes include student grades, demographic, social and school related features) 
#and it was collected by using school reports and questionnaires. 
#Two datasets are provided regarding the performance in two distinct subjects: Mathematics (mat) and Portuguese language (por). 
#In [Cortez and Silva, 2008], the two datasets were modeled under binary/five-level classification and regression tasks.
print(nrow(d3)) # 382 students
names(d3)
summary(d3)
names(d1)
names(d2)
names(d3)
##Test mdoels for each data sets
#1)Multiple regression 
myvars=names(d1) %in% c("G1","G2" )##omit the 1st and 2nd period grades,make final garde as a target(response)
newdata=d1[!myvars]
str(newdata)
lm.fit1=lm(G3~.,data=newdata)
summary(lm.fit1)
##Since sexM,failures,schoolsupyes,romanticyes and goout have significant p-value, use backward-selection
lm.fit2=lm(G3~sex+failures+school+romantic+goout,data=newdata)
options(scipen=999)
summary(lm.fit2)
lm.fit2=lm(G3~sex+failures+school+goout,data=newdata)#remove romantic
summary(lm.fit2)
lm.fit2=lm(G3~sex+failures+goout,data=newdata)#remove school
summary(lm.fit2)## all of the variables have signifincat p-values
names(lm.fit2)
plot(lm.fit2)

##1)K-fold cross validation(k=10)
str(d1);dim(d1)
newData=d1[-c((9:12),33)]#keep binary(dummy)variables remain and drop other categorical variables $$ G3 
newData=sapply(newData,as.numeric)
newData=row.names(newData)
library(cvTools)
set.seed(17)
names(d1)
str(d1);
cv.error.10=rep(0,10)
for(i in 1:10){
  glm.fit=glm(G3~poly(newData,i),data=d1)
  cv.error.10[i]=cv.glm(d1,glm.fit,K=10)$delta[1]
}
cv.error.10
min(cv.error.10)##compute minimum of cv.error
plot(cv.error.10)

glm.fit=glm(G3~.,data=d1)
cv.err=cv.glm(d1,glm.fit)
cv.err$delta
# K-fold cross-validation
library(DAAG)
?DAAG
?cv.lm
cv.lm(data = d1, form.lm = formula(G3~.,), m = 5, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
      main="Small symbols show cross-validation predicted values",
      legend.pos="topleft", printit = TRUE) # 5 fold cross-validation

##Boostrap(Estimatin the accuracy of multiple regression)
library(boot)
dim(d1);##numOfColums==395
library(Auto)
boot.fn=function(data,index)
  + return(coef(lm(G3~.,data=data,subset=index))) 
boot.fn(d1,1:395)
##Boostrap for estimating the intercept and slope terms
set.seed(1)
boot.fn(d1,sample(395,395,replace=T))
boot.fn(d1,sample(395,395,replace=T))

##estimate the standard erros of 1,000 boostrap estimates(boostrap gives a more accurate estimate of standdard erroes(p196))
boot(d1,boot.fn,1000)
##sumary using regression
summary(lm(G3~.,data=d1))$coef


#2)K-Means Clustering(scaling causes an error when data have categorical variables, created two data:one containing only numeric variables, the other contaning numeric and two-level(dummy) categorical variables)
myvars=names(d1) %in% c("G1","G2","G3" )
library(gdata)

myvars=remove.vars(d1,c("G1","G2","G3"),info=TRUE)##Remove responses(G1 ,G2,G3) from d1 data in oder to conduct unsupervised learning
names(myvars)
str(myvars)
firstData=myvars[c(-1,-2,-4,-5,-6,-9,-10,-11,-12,-c(16:23))]##drop categrical variables
firstData=na.omit(firstData)
firstData=scale(firstData)
set.seed(2)
str(firstData)
km.out=kmeans(firstData,4,nstart=20)##k=4(4 groups)
km.out$tot.withinss

km.out$cluster
plot(firstData, col=(km.out$cluster +1), main="K-Means Clustering Results with K=4", xlab="", ylab="", pch=20, cex=4)
km.out

secondData=myvars[-c(9:12)]#keep binary(dummy)variables remain and drop other categorical variables 
names(secondData)
secondData=na.omit(secondData)
secondData=sapply(secondData,as.numeric)
secondData=scale(secondData)
km.out=kmeans(secondData,4,nstart=20)##k=4(4 groups)
km.out$tot.withinss
plot(secondData, col=(km.out$cluster +1), main="K-Means Clustering Results with K=4", xlab="", ylab="", pch=20, cex=2)
km.out

##Even though we assign four groups to the data, it shows only two groups


#3)HierarchicalClsutering
myvars=remove.vars(d1,c("G1","G2","G3"),info=TRUE)##Remove responses(G1 ,G2,G3) from d1 data in oder to conduct unsupervised learning
names(myvars)
newmyvars=na.omit(myvars)
ScaledData=model.matrix(~.+0,data=newmyvars)##Convert to categorical variables to numeric ones 

hc.complete=hclust(dist(ScaledData), method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex =.9)##Unscaled data

scaled=scale(ScaledData)##Scale data
plot(hclust(dist(scaled), method="complete"), main="Hierarchical Clustering with Scaled Features ")##Scaled data


##(Correlation-based distance)
dd=as.dist(1-cor(t(scaled)))
plot(hclust(dd, method="complete"), main="Complete Linkage
  with Correlation -Based Distance", xlab="", sub="")


#4)Decision tree
library(tree)
attach(d1)
mean(d1$G3)##measure the mean of G3(final grade)
High=ifelse((G3<=mean(G3)),"No","Yes")
d1forTree=data.frame(d1,High)      
tree.d1forTree=tree(High~.-G3,d1)##Warning message
tree.d1forTree=tree(High~.-G3,lapply(d1,as.numeric))
summary(tree.d1forTree)
dim(d1);dim(High)
dim(High)
length(High)
length(d1$G3)
head(High)
head(d1$G3)
#5)PCA
variables=row.names(d1)
variables
pr.out=prcomp(scaled,scale=TRUE)
names(pr.out)
dim(pr.out$x)
biplot(pr.out,scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out,scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')
##As can be seen fron PVE graph, the first few compoents don't explain the variance so mcuh....so PCA is not useful in this dataset 

##Kaiser-criterion
ev <- pr.out$sdev^2
# Plot eigenvalues and percentages of variation of an ordination object
# Kaiser rule and broken stick model
# Usage: evplot(ev) where ev is a vector of eigenvalues
# License: GPL-2 # Author: Francois Gillet, 25 August 2012#http://www.davidzeleny.net/anadat-r/doku.php/en:numecolr:evplot

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}
evplot(ev)
sum(ev>1)
#As shown above, PCAs above red line(1) are first seventeen which should be kept according to Kaiser criterion 



plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')

##neuralnet
library(neuralnet)
library(Metrics)##to calculate MSE(function mse)
str(d1);dim(d1)
numericData=d1[-c(1:2,4:6,9:12,16:23)]##drop categrical variables
dim(numericData)
ind <- sample(1:nrow(numericData), 395/2)##split the data
train <- scale(numericData)[ind,]##Assign scaled data
test <- d1[-ind,]#Assign original data
test1=scale(numericData)[-ind,]
dim(train);dim(test);dim(test1)
##Hidden-layer is one
set.seed(1110)
names(numericData)
net.sqrt <- neuralnet(G3~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences+G1+G2,data=train,linear.output = TRUE ,hidden=1)
pr.nn=compute(net.sqrt,d1[-c(1:2,4:6,9:12,16:23,26)])$net.result##drop categrocal variables and G3
mse(test$G3,pr.nn)##Compute test MSE
length(test);length(pr.nn)

##Hidden-layers are five
set.seed(217)
net.sqrt= neuralnet(G3~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences+G1+G2,data=train,linear.output = TRUE ,hidden=5)
pr.nn=compute(net.sqrt,d1[-c(1:2,4:6,9:12,16:23,26)])$net.result
mse(test$G3,pr.nn)##Compute test MSE


pr.nn_ <- pr.nn$net.result*(max(numericData$G3)-min(numericData$G3))+min(numericData$G2)
test.r <- (test_$G3)*(max(numericData$G3)-min(numericData$G3))+min(numericData$G3)



dim(numericData)
index <- sample(1:nrow(numericData), 395/2)##split the data
maxs <- apply(numericData, 2, max) 
mins <- apply(numericData, 2, min)
scaled <- (scale(numericData, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]
n <- names(train_)
nn <- neuralnet(G3,data=train_,hidden=c(5,3),linear.output=T)
?as.formula
