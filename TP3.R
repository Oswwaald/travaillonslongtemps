## Appel du package Matrix.
install.packages('dplyr')
library(Matrix)
library(dplyr)


## Chargement des bases de données data, item & user.
u.data <- read.csv(file = 'u.data.csv', sep='|', header=T)

## Construction de la matrice de vote.
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
m<-as.matrix(m)

rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m[m==0] <- NA
###############
##
## Question 1 : Déterminez un point de comparaison pour la prévision de votes (une performance minimale).
## 
###############

## Récupération de la moyenne des notes pour chaque films.
cmean<-colMeans(m, na.rm=TRUE)
rmean<-rowMeans(m, na.rm=TRUE)

hist(colMeans(m, na.rm=TRUE))
hist(rowMeans(m, na.rm=TRUE))
colMeans(m, na.rm=TRUE)
emme<-t(matrix(colMeans(m, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))

###############
##
## Question 2 : Appliquer la décomposition SVD (en prenant soin de normaliser au préalable).
## 
###############
m2<-m

m2[is.na(m2)]<-emme[is.na(m2)]

##normalisation pour rendre les vecteurs unitaires

mNorm<-(m2/rowSums(m2**2)**(1/2))

mSvd=svd(mNorm)
mSvd$u
mSvd$d
mSvd$v

###############
##
## Question 3 :  Effectuez l'estimation des votes sur la base de SVD avec 10 dimensions.
## 
###############

reduc <- function(dim){
  return((mSvd$u[,1:dim]%*%diag(mSvd$d[1:dim])%*%t(mSvd$v)[1:dim,])*(rowSums(m2**2)**(1/2)))
  }


m10<-(mSvd$u[,1:10]%*%diag(mSvd$d[1:10])%*%t(mSvd$v)[1:10,])*(rowSums(m2**2)**(1/2))
mSvd$u[,1:1]%*%diag(mSvd$d[1:1])%*%t(mSvd$v)[1:1,]
diag(mSvd$d[1:2])
###############
##
## Question 4 : Calculez l'erreur absolue moyenne et l'erreur quadratique moyenne.
## 
###############

errorAbs<-sum(abs(m10[!is.na(m)]-m[!is.na(m)])/length(m[!is.na(m)]))
errorAbs

errorQuadr<-sum((m10[!is.na(m)]-m[!is.na(m)])**2/length(m[!is.na(m)]))**(1/2)
errorQuadr

errorQuadrFun<- function(dim){
  return(sum((reduc(dim)[!is.na(m)]-m[!is.na(m)])**2/length(m[!is.na(m)]))**(1/2))
}

###############
##
## Question 5 : Déterminez le nombre de dimensions optimal (sans appliquer de validation croisée). Un graphique doit indiquer la performance par nombre de dimension (semblable au rapport Sarwar et al.).
## 
###############

reduc()

possibledim<-seq(2,943,1)

sapply(possibledim,FUN=errorQuadrFun)


###############
##
## Question 6 : Déterminez le nombre optimal de dimensions, mais en utilisant cette fois une validation croisée.
## 
###############

mObserved=which(!is.na(m))
m.random<-sample(mObserved,length(mObserved))





svdOfCrossVal<-function(nb){
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train<-m
  m.train[set.test]<-NA
  m.colmean<-t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)]<-m.colmean[is.na(m.train)]
  rMean<-rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)]<-0
  m.train.norm<-(m.train-rMean)
  m.train.norm[is.na(m.train.norm)]<-0
  return(svd(m.train.norm))
}

reducForCrossVal<-function(trainSvd,dim){
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train<-m
  m.train[set.test]<-NA
  m.colmean<-t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)]<-m.colmean[is.na(m.train)]
  rMean<-rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)]<-0
  return(((matrix(unlist(trainSvd["u"]),ncol=nrow(m), nrow=nrow(m)))[,1:dim]%*%diag(unlist(trainSvd["d"])[1:dim])%*%t(matrix(unlist(trainSvd["v"]),ncol=ncol(m), nrow=ncol(m)))[1:dim,])+rMean)
}

trainSvd$d
set.test
aprox<-(trainSvd$u[,1:10]%*%diag(trainSvd$d[1:10])%*%t(trainSvd$v)[1:10,])+rowMeans(m.train)
trainSvd$u[1:10,1:10]

errorQuadrFun<- function(trainSvd,set.test,dim){
  return(sum((reducForCrossVal(trainSvd,dim)[set.test]-m[set.test])**2/length(m[set.test]))**(1/2))
}

errorAbsFun<- function(trainSvd,set.test,dim){
  return(sum(abs(reducForCrossVal(trainSvd,dim)[set.test]-m[set.test])/length(m[set.test])))
}

computeAllSVD<-function(set.test){
  return(sapply(seq(1,10,1),FUN=svdOfCrossVal))
}

SVDs=computeAllSVD(set.test)

(matrix(unlist(SVDs[,1]["u"]),ncol=nrow(m), nrow=nrow(m)))[,1:10]


calculateQuadrErr<-function(nb,dim){
set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
return(errorQuadrFun(SVDs[,nb],set.test,dim))}

calculateAbsErr<-function(nb,dim){
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  return(errorAbsFun(SVDs[,nb],set.test,dim))}

crossVal<-function(dimension){
return(sum((sapply(c(1,2,3,4,5,6,7,8,9,10),calculateQuadrErr,dim=dimension)**2)/10)**(1/2))
}
crossValAbs<-function(dimension){
  return(sum((sapply(c(1,2,3,4,5,6,7,8,9,10),calculateAbsErr,dim=dimension)**2)/10)**(1/2))
}

possibledim<-seq(2,30,1)

sapply(possibledim,crossVal)
sapply(possibledim,crossValAbs)

#dim optimale : 11

###############
##
## Question 7 : Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix (avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisée.
## 
###############


cosOfCrossVal<-function(nb){
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train<-m
  m.train[set.test]<-NA
  m.colmean<-t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)]<-m.colmean[is.na(m.train)]
  rMean<-rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)]<-0
  m.train.norm<-(m.train-rMean)
  m.train.norm[is.na(m.train.norm)]<-0
  m.train.norm<- m.train.norm/(colSums(m.train.norm**2)**(1/2) )
  return(t(m.train.norm)%*%m.train.norm)
}
(t(matrix(colSums(m.train.norm**2),nrow=ncol(m.train.norm),ncol=nrow(m.train.norm))))
nb=1
itemItemCrossVal<-function(nb){
  
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train<-m
  m.train[set.test]<-NA
  m.colmean<-t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)]<-m.colmean[is.na(m.train)]
  rMean<-rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)]<-0
  m.train.norm<-(m.train-rMean)
  m.train.norm[is.na(m.train.norm)]<-0
  m.train.normalise<- m.train.norm/(t(matrix(colSums(m.train.norm**2),nrow=ncol(m.train.norm),ncol=nrow(m.train.norm)))**(1/2) )
  m.train.normalise[is.infinite(m.train.normalise)]<-0
  m.train.normalise[is.nan(m.train.normalise)]<-0
  cosMat<-(t(m.train.normalise)%*%(m.train.normalise))
  return(m.train.norm%*%cosMat/t(matrix(rowSums(abs(cosMat)),nrow=ncol(m.train.norm),ncol=nrow(m.train.norm)))+rMean)
  
}

matrix(c(1,2,3),nrow=3,ncol=6)

itemItemCrossValQuadrErr<-function(nb){
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  return(sum((itemItemCrossVal(nb)[set.test]-m[set.test])**2/length(m[set.test]))**(1/2))
}

itemItemCrossValQuadrErr(1)
