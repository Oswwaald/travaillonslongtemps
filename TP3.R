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

meanMovie <- inner_join(u.data, u.item, by=c("item.id"="movie.id")) %>%
  group_by(item.id) %>%
  summarise(meanMov=mean(rating))

head(m)

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


###############
##
## Question 4 : Calculez l'erreur absolue moyenne et l'erreur quadratique moyenne.
## 
###############

errorAbs<-sum(abs(m10[!is.na(m)]-m[!is.na(m)])/length(m[!is.na(m)]))
errorAbs

errorQuadr<-sum((m10[!is.na(m)]-m[!is.na(m)])**2/length(m[!is.na(m)]))**(1/2)
errorQuadr

###############
##
## Question 5 : Déterminez le nombre de dimensions optimal (sans appliquer de validation croisée). Un graphique doit indiquer la performance par nombre de dimension (semblable au rapport Sarwar et al.).
## 
###############

possibledim<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,25)



###############
##
## Question 6 : Déterminez le nombre optimal de dimensions, mais en utilisant cette fois une validation croisée.
## 
###############





###############
##
## Question 7 : Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix (avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisée.
## 
###############

m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
m <- as.matrix(m)
m[m==0] <- NA
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m.baseline <- outer(rowMeans(m, na.rm=T),colMeans(m, na.rm=T),'+') / 2
mean(abs(m.baseline - m), na.rm=T)

m.imp <- m
m.imp[is.na(m)] <- m.baseline[is.na(m)]


