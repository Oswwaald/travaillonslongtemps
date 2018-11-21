## Appel du package Matrix.
library(Matrix)

## Chargement de la base de données data.
u.data <- read.csv(file = 'u.data.csv', sep='|', header=T)

## Construction de la matrice de vote.
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
m  <-as.matrix(m)
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m[m==0] <- NA

###############
##
## Question 1 : Déterminez un point de comparaison pour la prévision de votes (une performance minimale).
## 
###############

## Récupération de la moyenne des notes pour chaque films.
cmean <- colMeans(m, na.rm=TRUE)
rmean <- rowMeans(m, na.rm=TRUE)
hist(colMeans(m, na.rm=TRUE))
hist(rowMeans(m, na.rm=TRUE))

## Création de la matrice de référence de la moyenne arythmétique des moyennes des colonnes et des lignes.
meanMatrix <- (t(matrix(colMeans(m, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m))) + rowMeans(m, na.rm=TRUE))/2

## Création des fonctions de calculs des Erreurs.
meanAbsError <- function (m1,m2) {
  return (mean(abs((m1 - m2)[(!is.na(m1-m2))])))
}
meanSquarError <- function (m1,m2){
  return (sqrt(mean(((m1 - m2)[(!is.na(m1-m2))])^2, na.rm=T)))
}

## Calcul de l'Erreur Moyenne Absolue et de l'Erreur Quadratique Moyenne.
baselineMeanError <- meanAbsError(meanMatrix,m)
baselineSquarError<- meanSquarError(meanMatrix,m)

## Résutat 1 :
show(baselineMeanError)
show(baselineSquarError)

###############
##
## Question 2 : Appliquer la décomposition SVD (en prenant soin de normaliser au préalable).
## 
###############

mRef <- m
mRef[is.na(mRef)] <- meanMatrix[is.na(mRef)]

## Normalisation par la somme des lignes (comme défini dans l'article de référence) et Décomposition SVD.
mNorm <- (mRef - rowMeans(mRef))
mSvd <- svd(mNorm)

## Résutat 2 :
show(mSvd$u[1:5,1:5])
show(mSvd$d[1:5])
show(mSvd$v[1:5,1:5])

###############
##
## Question 3 :  Effectuez l'estimation des votes sur la base de SVD avec 10 dimensions.
## 
###############

## Création de la fonction pour la diminution de dimensions.
reduc <- function(msvd,dim){
  return ( (msvd$u[,1:dim] %*% diag(msvd$d[1:dim]) %*% t(msvd$v)[1:dim,]) + rowMeans(mRef))
}

## Application de la diminution de dimensions.
m10 <- reduc(mSvd,10)

## Résutat 3 :
show(m10[1:5,1:5])

###############
##
## Question 4 : Calculez l'erreur absolue moyenne et l'erreur quadratique moyenne.
## 
###############

m10AbsError <- meanAbsError(m10,m)
m10SquarError <- meanSquarError(m10,m)

## Résutat 4 :
show(m10AbsError)
show(m10SquarError)

###############
##
## Question 5 : Déterminez le nombre de dimensions optimal (sans appliquer de validation croisée). Un graphique doit indiquer la performance par nombre de dimension (semblable au rapport Sarwar et al.).
## 
###############

possibledim <- seq(2,943,1)
result <- sapply(possibledim, function(x, y) meanSquarError(reduc(mSvd,x),m), y=m)

## Résutat 5 :
plot(possibledim,result)

###############
##
## Question 6 : Déterminez le nombre optimal de dimensions, mais en utilisant cette fois une validation croisée.
## 
###############

mObserved <- which(!is.na(m))
m.random <- sample(mObserved,length(mObserved))

## Création de la fonction permettant de réaliser la décomposition SVD pour 1 repli de valeurs observées.
svdOfCrossVal <- function(nb){
  set.test <- m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train <- m
  m.train[set.test] <- NA
  m.colmean <- t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)] <- m.colmean[is.na(m.train)]
  rMean <- rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)] <- 0
  # Normalisation en soustrayant la moyenne des lignes aux valeurs (conformément à l'article)
  m.train.norm <- (m.train-rMean)
  m.train.norm[is.na(m.train.norm)] <- 0
  return(svd(m.train.norm))
}

computeAllSVD <- function(set.test){
  return(sapply(seq(1,10,1),FUN=svdOfCrossVal))
}

## Récupération des matrices décomposées pour tous les replis.
SVDs <- computeAllSVD(set.test)

## Création de la fonction permettant de reconstruire la matrice grâce à l'échantillon d'entrainement.
reducForCrossVal <- function(trainSvd,dim,set.test){
  m.train <- m
  m.train[set.test] <- NA
  m.colmean <- t(matrix(colMeans(m.train, na.rm=TRUE), ncol=nrow(m), nrow=ncol(m)))
  m.train[is.na(m.train)] <- m.colmean[is.na(m.train)]
  rMean <- rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)] <- 0
  return(((matrix(unlist(trainSvd["u"]),ncol=nrow(m), nrow=nrow(m)))[,1:dim]%*%diag(unlist(trainSvd["d"])[1:dim])%*%t(matrix(unlist(trainSvd["v"]),ncol=ncol(m), nrow=ncol(m)))[1:dim,])+rMean)
}

## Création des fonctions permettant de calculer les erreurs.
calculateQuadrErr <- function(nb,dim){
  set.test <- m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  return(sum((reducForCrossVal(SVDs[,nb],dim,set.test)[set.test]-m[set.test])**2/length(m[set.test]))**(1/2))
}
calculateAbsErr<-function(nb,dim){
  set.test <- m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  return(sum(abs(reducForCrossVal(SVDs[,nb],dim,set.test)[set.test]-m[set.test])/length(m[set.test])))
}
crossVal <- function(dimension){
  return(sum((sapply(c(1,2,3,4,5,6,7,8,9,10),calculateQuadrErr,dim=dimension)**2)/10)**(1/2))
}
crossValAbs <- function(dimension){
  return(sum(abs(sapply(c(1,2,3,4,5,6,7,8,9,10),calculateAbsErr,dim=dimension))/10))
}

possibledim <- seq(2,20,1)

resultErrorSquar <- sapply(possibledim,crossVal)
resultErrorAbs <- sapply(possibledim,crossValAbs)

## Résutat 6 :
plot(possibledim,resultErrorSquar)
plot(possibledim,resultErrorAbs)

###############
##
## Question 7 : Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix (avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisée.
## 
###############

## Création de la fonction permettant de récupérer la matrice avec les prédictions en fonction du numéro du repli.
itemItemCrossVal <- function(nb){
  set.test <- m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  m.train <- m
  m.train[set.test] <- NA
  rMean <- rowMeans(m.train, na.rm=TRUE)
  rMean[is.na(rMean)] <- 0
  m.train[is.na(m.train)] <- 0
  m.train.normalise <- m.train/(t(matrix(colSums(m.train**2),nrow=ncol(m.train),ncol=nrow(m.train)))**(1/2) )
  
  #On élimine les valeurs aberrantes
  m.train.normalise[is.infinite(m.train.normalise)] <- 0
  m.train.normalise[is.nan(m.train.normalise)] <- 0
  cosMat <- (t(m.train.normalise)%*%(m.train.normalise))
  
  m.train.centr <- (m.train-rMean)
  m.train.centr[is.na(m.train.centr)] <- 0
  
  #On ignore les valeurs n'ayant pas de votes
  m.train.centr[m.train==0] <- 0
  
  #On divise par la somme des cosinus des items partageant un vote commun
  pred<-(m.train.centr%*%cosMat)/((m.train!=0)%*%abs(cosMat))
  pred[is.nan(pred)]<-0
  return(pred+rMean)
}

## Création des fonctions permettant de calculer les erreurs.
itemItemCrossValQuadrErr <- function(nb){
  set.test <- m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  return(sum((itemItemCrossVal(nb)[set.test]-m[set.test])**2/length(m[set.test]))**(1/2))
}
itemItemCrossValAbsErr <- function(nb){
  set.test <- m.random[((nb-1)*length(m.random)/10):(nb*length(m.random)/10)]
  return(sum(abs(itemItemCrossVal(nb)[set.test]-m[set.test])/length(m[set.test])))
}

resultErrorAbsItem <- mean(sapply(seq(1,10,1),itemItemCrossValAbsErr))
resultErrorSquarItem <- mean(sapply(seq(1,10,1),itemItemCrossValQuadrErr)**2)**(1/2)

## Résutat 7 :
show(resultErrorAbsItem)
show(resultErrorSquarItem)
