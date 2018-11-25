## Sources des données : https://www.kaggle.com/rdoume/beerreviews
#install.packages('dplyr',lib="/home/travail/travaillonslongtemps-master/packages")

## Appel du package Matrix.
library(Matrix)
library(dplyr)

## Récupération de toutes les données.
m.data <- read.csv('beer_reviews.csv', stringsAsFactors = FALSE)

###############
##
## Préparation des données.
##
###############

## Nettoyage des données avec absence d'utilisateur.
m.data <- m.data[m.data[,"review_profilename"]!="",]
## Extraction des données avec les moyennes de votes.
m.user <- select(m.data,review_profilename) %>%
  group_by(review_profilename) %>%
  summarise(count=sum(!is.na(review_profilename)))
## Nettoyage des utilisateurs avec un seul vote (diminution de 10443 utilisateurs)
m.user <- filter(m.user, count>1)
m.data <- filter(m.data,review_profilename %in% unlist(m.user["review_profilename"]))
## Nettoyage des données manquantes
m.data[is.na(m.data)] <- 0

m.beer <- select(m.data,beer_beerid,beer_name,beer_style,beer_abv,brewery_id,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
  group_by(beer_beerid,beer_name,beer_style,beer_abv,brewery_id) %>%
  summarise(count=sum(!is.na(beer_beerid)))

## Rédution des données d'utilisateurs pour l'exploitation des brasseries.
m.user.best <-filter(m.user,count>=50)
m.data.best <- filter(m.data,review_profilename %in% unlist(m.user.best["review_profilename"]))

###############
##
## Définition des fonctions.
##
###############

## Cosinus en fonction du type et du critère évalué.
cosTypeFunction <- function(userMap,typeMap,typeIdConfFun,mVoteCriteria,meanCriteria){
  m_type.sparse <- sparseMatrix(userMap[unlist(mVoteCriteria[,"review_profilename"]),],typeMap[sapply(unlist(mVoteCriteria[,2]),typeIdConfFun),],x=unlist(mVoteCriteria[,meanCriteria]),use.last.ij=TRUE)
  normalisatorPart<-t(m_type.sparse!=0)%*%(m_type.sparse*m_type.sparse)
  normalisator<-normalisatorPart**(1/2)*(t(normalisatorPart))**(1/2)
  cos_type<-(t(m_type.sparse)%*%m_type.sparse)/normalisator
  cos_type[is.na(cos_type)]<-0
  cos_type_mat<-matrix(cos_type,nrow=nrow(cos_type),ncol=ncol(cos_type))
  return (cos_type_mat)
}

extWithIndexSimple <- function(m.info,type){
  Map <- matrix(0,nrow=nrow(m.info),ncol=1)
  rownames(Map) <- unlist(m.info[,type])
  Map[,1] <- seq(1,nrow(m.info[,type]),1)
  return (Map)
}

extWithIndexComplex <- function(m.info,type,typeIdConfFun){
  Map <- matrix(0,nrow=nrow(m.info),ncol=1)
  rownames(Map) <- sapply(unlist(m.info[,type]),typeIdConfFun)
  Map[,1] <- seq(1,nrow(m.info[,type]),1)
  return (Map)
}

###############
##
## Extraction des utisateurs avec index.
##
###############

userMap <- extWithIndexSimple(m.user,"review_profilename")
userMap.best <- extWithIndexSimple(m.user.best,"review_profilename")

##
## Calcul des matrice creuse des votes sur la moyenne des styles.
##
m.style <- select(m.data,beer_style) %>%
  group_by(beer_style) %>%
  summarise(count=sum(!is.na(beer_style)))

m.vote_style <- m.data %>%
  group_by(review_profilename,beer_style)%>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

styleIdConfFun <- toString
styleMap <- extWithIndexSimple(m.style,"beer_style")

## Calcul des cosinus pour les Styles avec tous les votes des utilisateurs.
cos_style_palate <- cosTypeFunction(userMap,styleMap,styleIdConfFun,m.vote_style,"mean_palate")
cos_style_taste <- cosTypeFunction(userMap,styleMap,styleIdConfFun,m.vote_style,"mean_taste")
cos_style_appareance <- cosTypeFunction(userMap,styleMap,styleIdConfFun,m.vote_style,"mean_appearance")
cos_style_aroma <- cosTypeFunction(userMap,styleMap,styleIdConfFun,m.vote_style,"mean_aroma")
cos_style_overall <- cosTypeFunction(userMap,styleMap,styleIdConfFun,m.vote_style,"mean")

##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##
m.brewery <- select(m.data,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

m.vote_brewery <- m.data.best %>%
  group_by(review_profilename,brewery_id) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

brewIdConfFun <- function(x) paste("u",x,sep="")
breweryMap <- extWithIndexComplex(m.brewery,"brewery_id",brewIdConfFun)

#################### Mettre usermapbest dans la fonction
## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
cos_brewery_palate <- cosTypeFunction(userMap.best,breweryMap,brewIdConfFun,m.vote_brewery,"mean_palate")
cos_brewery_taste <- cosTypeFunction(userMap.best,breweryMap,brewIdConfFun,m.vote_brewery,"mean_taste")
cos_brewery_appareance <- cosTypeFunction(userMap.best,breweryMap,brewIdConfFun,m.vote_brewery,"mean_appearance")
cos_brewery_aroma <- cosTypeFunction(userMap.best,breweryMap,brewIdConfFun,m.vote_brewery,"mean_aroma")
cos_brewery_overall <- cosTypeFunction(userMap.best,breweryMap,brewIdConfFun,m.vote_brewery,"mean")

##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##
m.abv <- select(m.data,beer_abv) %>%
  group_by(beer_abv=round(beer_abv)) %>%
  summarise(count=sum(!is.na(beer_abv)))

m.vote_abv <- m.data %>%
  group_by(review_profilename,beer_abv=round(beer_abv)) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

abvIdConfFun <- function(x) toString(round(x))
breweryMap <- extWithIndexComplex(m.brewery,"brewery_id",brewIdConfFun)

## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
cos_abv_palate <- cosTypeFunction(userMap,abvMap,abvIdConfFun,m.vote_abv,"mean_palate")
cos_abv_taste <- cosTypeFunction(userMap,abvMap,abvIdConfFun,m.vote_abv,"mean_taste")
cos_abv_appareance <- cosTypeFunction(userMap,abvMap,abvIdConfFun,m.vote_abv,"mean_appearance")
cos_abv_aroma <- cosTypeFunction(userMap,abvMap,abvIdConfFun,m.vote_abv,"mean_aroma")
cos_abv_overall <- cosTypeFunction(userMap,abvMap,abvIdConfFun,m.vote_abv,"mean")

##
## Méthode ItemItem
##

breweryMap2 <- 0
breweryMap2 <- matrix(0,nrow=nrow(m.brewery),ncol=1)
colnames(breweryMap2) <- "index.brewery"
rownames(breweryMap2) <- sapply(unlist(m.brewery[,"brewery_id"]),function(x) paste("u",x,sep=""))
breweryMap2[,1] <- seq(1,nrow(m.brewery[,"brewery_id"]),1)

beerMap <- 0
beerMap <- matrix(0,nrow=nrow(m.beer),ncol=1)
colnames(beerMap) <- "index.beer"
rownames(beerMap) <- sapply(unlist(m.beer[,"beer_beerid"]),function(x) paste("b",x,sep=""))
beerMap[,1] <- seq(1,nrow(m.beer[,"beer_beerid"]),1)

beerAndBreweryMap <- sparseMatrix(breweryMap2[sapply(unlist(m.beer[,"brewery_id"]),function(x) paste("u",x,sep="")),"index.brewery"],beerMap[sapply(unlist(m.beer[,"beer_beerid"]),function(x) paste("b",x,sep="")),"index.beer"],x=1,use.last.ij=TRUE)

beerAndStyleMap <- sparseMatrix(styleMap[unlist(m.beer[,"beer_style"]),"style.id"],beerMap[sapply(unlist(m.beer[,"beer_beerid"]),function(x) paste("b",x,sep="")),"index.beer"],x=1,use.last.ij=TRUE)
rowSums(beerAndStyleMap)
colSums(beerAndStyleMap)

abvMap2 <- 0
abvMap2 <- matrix(0,nrow=nrow(m.abv),ncol=1)
colnames(abvMap2) <- "abv.id"
rownames(abvMap2) <- sapply(unlist(m.abv[,"beer_abv"]),function(x) paste("a",x,sep=""))
abvMap2[,1] <- seq(1,nrow(m.abv[,"beer_abv"]),1)

beerAndAbvMap <- sparseMatrix(abvMap2[sapply(unlist(m.beer[,"beer_abv"]),function(x) paste("a",round(x),sep="")),"abv.id"],beerMap[sapply(unlist(m.beer[,"beer_beerid"]),function(x) paste("b",round(x),sep="")),"index.beer"],x=1,use.last.ij=TRUE)
beerAndAbvMap[1:10,1:10]

rowSums(beerAndAbvMap)
colSums(beerAndAbvMap)

sapply(unlist(m.beer[,"beer_beerid"]),function(x) paste("b",x,sep=""))

rowSums(beerAndBrewMap)




######### PREDICTION

m.user.unit<-m.user[m.user[,"count"]>1,]

m.user.rand <- m.user.unit[(runif(nrow(m.user.unit), 0.0, 1.0)>0.994),]
dim(m.user.rand)
m.data.rand <- filter(m.data,review_profilename %in% unlist(m.user.rand["review_profilename"]))

userMap=0
userMap=matrix(0,nrow=nrow(m.user.rand),ncol=1)
rownames(userMap)<-unlist(m.user.rand[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user.rand[,"review_profilename"]),1)

######### PREDICTION STYLEEEEEEEEEE

#m_palate.sparse <- sparseMatrix(userMap[m.data.rand[,"review_profilename"],],beerMap[sapply(m.data.rand[,"beer_beerid"],function(x) paste("b",x,sep="")),],x=m.data.rand[,"review_palate"],use.last.ij=TRUE)
m_taste.sparse <- sparseMatrix(userMap[m.data.rand[,"review_profilename"],],beerMap[sapply(m.data.rand[,"beer_beerid"],function(x) paste("b",x,sep="")),],x=m.data.rand[,"review_taste"],use.last.ij=TRUE,dims=c(length(userMap),length(beerMap)))
#m_appearance.sparse <- sparseMatrix(userMap[m.data.rand[,"review_profilename"],],beerMap[sapply(m.data.rand[,"beer_beerid"],function(x) paste("b",x,sep="")),],x=m.data.rand[,"review_appearance"],use.last.ij=TRUE)
#m_aroma.sparse <- sparseMatrix(userMap[m.data.rand[,"review_profilename"],],beerMap[sapply(m.data.rand[,"beer_beerid"],function(x) paste("b",x,sep="")),],x=m.data.rand[,"review_aroma"],use.last.ij=TRUE)
#m_overall.sparse <- sparseMatrix(userMap[m.data.rand[,"review_profilename"],],beerMap[sapply(m.data.rand[,"beer_beerid"],function(x) paste("b",x,sep="")),],x=m.data.rand[,"review_overall"],use.last.ij=TRUE)

m_taste.sparse[m_taste.sparse==0]<-NA
rMean_taste <- rowMeans(m_taste.sparse, na.rm=TRUE)
rMean_taste[is.na(rMean_taste)] <- 0
m_taste.sparse.centr<-m_taste.sparse-rMean_taste
m_taste.sparse[is.na(m_taste.sparse)]<-0
m_taste.sparse.centr[is.na(m_taste.sparse.centr)]<-0

typeVoteCount <- (m_taste.sparse>0) %*% t(beerAndStyleMap)
voteOfStyle <-(m_taste.sparse.centr %*% t(beerAndStyleMap))
voteOfStyle[is.na(voteOfStyle)]<-0
m_taste.pred.style<-(voteOfStyle%*%cos_style_taste%*%beerAndStyleMap)/(typeVoteCount%*%cos_style_taste%*%beerAndStyleMap)




numStyle=(voteOfStyle%*%cos_style_taste%*%beerAndStyleMap)
denStyle=(typeVoteCount%*%cos_style_taste%*%beerAndStyleMap)




m_taste.pred.final.style<-m_taste.pred.style+rMean_taste

test.taste.style <- rsapply(1:nrow(m_taste.pred.final.style),function(x) max(m_taste.pred.final.style[x,])-min(m_taste.pred.final.style[x,]))

######### PREDICTION BRASSERIEEEEEEEEE


typeVoteCountBrewery <- (m_taste.sparse>0) %*% t(beerAndBreweryMap)
voteOfBrewery <-(m_taste.sparse.centr %*% t(beerAndBreweryMap))
voteOfBrewery[is.na(voteOfBrewery)]<-0


m_taste.pred.brewery<-(voteOfBrewery%*%cos_brewery_taste%*%beerAndBreweryMap)/(typeVoteCountBrewery%*%cos_brewery_taste%*%beerAndBreweryMap)

numBrew=(voteOfBrewery%*%cos_brewery_taste%*%beerAndBreweryMap)
denBrew=(typeVoteCountBrewery%*%cos_brewery_taste%*%beerAndBreweryMap)

typeVoteCountBrewery



m_taste.pred.brewery[is.na(m_taste.pred.brewery)]<-0

m_taste.pred.final.brewery<-m_taste.pred.brewery+rMean_taste


#Verification ::::
test <- sapply(1:nrow(finalPred),function(x) max(finalPred[x,])-min(finalPred[x,]))

mean(((finalPred-m_taste.sparse)**2)[!m_taste.sparse==0])




######### PREDICTION ABVVVVVVVVVVVVV

typeVoteCountAbv <- (m_taste.sparse>0) %*% t(beerAndAbvMap)
voteOfAbv <-(m_taste.sparse.centr %*% t(beerAndAbvMap))
voteOfAbv[is.na(voteOfAbv)]<-0



m_taste.pred.abv<-(voteOfAbv%*%cos_abv_taste%*%beerAndAbvMap)/(typeVoteCountAbv%*%cos_abv_taste%*%beerAndAbvMap)

numAbv=(voteOfAbv%*%cos_abv_taste%*%beerAndAbvMap)
denAbv=(typeVoteCountAbv%*%cos_abv_taste%*%beerAndAbvMap)


finalPred<-(numBrew+numStyle+50*numAbv)/(denBrew+denStyle+50*denAbv)+rMean_taste

mean(((rMean_taste-m_taste.sparse)**2)[!m_taste.sparse==0])


findmax <- function(vec,n){
  sorted<-sort(vec,index.return=TRUE,decreasing=TRUE)
  return((sorted$ix)[1:n])
}

sorterPred <- function(n){
  (which(m_taste.sparse[n,]!=0))[findmax(finalPred[n,which(m_taste.sparse[n,]!=0)],length(which(m_taste.sparse[n,]!=0)))]
}

sorterReal <- function(n){
  (which(m_taste.sparse[n,]!=0))[findmax(m_taste.sparse[n,which(m_taste.sparse[n,]!=0)],length(which(m_taste.sparse[n,]!=0)))]
}

sorterReal(1)

findmax(finalPred[1,],10)

## Identification du nombre de bières et d'utilisateurs différents.
nbVotesPerBeer <- m.data %>% 
  count(beer_beerid)
nbVotesPerUser <- m.data %>% 
  count(review_profilename)


## Visualisation du pannel de bières et du pannel d'utilisateur.
qplot(nbVotesPerBeer$n, log = "xy", geom = "histogram",xlab="Nombre de votes",ylab="Bières")
qplot(nbVotesPerUser$n, log = "xy", geom = "histogram",xlab="Nombre de votes",ylab="Utilisateurs")

#Test de recherches rapides
m.beer_best <- filter(m.beer,count>100)
m.data %>% 
  max(review_time)
m.data %>% 
  group_by() %>% 
  summarise(min = min(review_time))
m_palate.sparse[1,]

#Moyenne en fonction des critères sur l'ensemble de la base
overallMean <- mean(m.data[,4]) #3.893478
aromaMean <- mean(m.data[,5]) #3.831061
appearanceMean <- mean(m.data[,6]) #3.925168
palateMean <- mean(m.data[,9]) #3.838386
tasteMean <-mean(m.data[,10]) #3.885663
totalMean <- (overallMean + aromaMean + appearanceMean + palateMean + tasteMean)/5 #3.874751
