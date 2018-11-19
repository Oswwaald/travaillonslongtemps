## Appel du package Matrix.
install.packages('dplyr')
library(Matrix)
library(dplyr)

## Chargement des bases de données data, item & user.
u.data <- read.csv(file = 'C:/Users/karl/Documents/R project/TP3/u.data.csv', sep='|', header=T)

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



head(m)

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
m.random=sample(mObserved,length(mObserved))


m.train[set.test]<-NA

nb<-2
m.random
length(m.random)
m.random[9000:10000]
m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]

set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]

svdOfCrossVal<-function(nb){
  set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train<-m
  m.train[set.test]<-NA
  m.colmean<-t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)]<-m.colmean[is.na(m.train)]
  m.train[is.na(m.train)]<-0
  rMean<-rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)]<-0
  m.train.norm<-(m.train-rMean)
  
  return(svd(m.train.norm))
}



reducForCrossVal<-function(trainSvd,dim){

  return(((matrix(unlist(trainSvd["u"]),ncol=nrow(m), nrow=nrow(m)))[,1:dim]%*%diag(unlist(trainSvd["d"])[1:dim])%*%t(matrix(unlist(trainSvd["v"]),ncol=ncol(m), nrow=ncol(m)))[1:dim,])+rMean)
}
trainSvd$d
set.test
aprox<-(trainSvd$u[,1:10]%*%diag(trainSvd$d[1:10])%*%t(trainSvd$v)[1:10,])+rowMeans(m.train)
trainSvd$u[1:10,1:10]

errorQuadrFun<- function(trainSvd,set.test,dim){
  return(sum((reducForCrossVal(trainSvd,dim)[set.test]-m[set.test])**2/length(m[set.test]))**(1/2))
}

computeAllSVD<-function(set.test){
  return(sapply(seq(1,10,1),FUN=svdOfCrossVal))
  }

SVDs=computeAllSVD(set.test)

(matrix(unlist(SVDs[,1]["u"]),ncol=nrow(m), nrow=nrow(m)))[,1:10]

nb<-4
set.test<-m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
errorQuadrFun(SVDs[,4],set.test,10)

###############
##
## Question 7 : Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix (avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisée.
## 
###############

