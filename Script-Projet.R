## Sources des données : https://www.kaggle.com/rdoume/beerreviews
#install.packages('dplyr',lib="/home/travail/travaillonslongtemps-master/packages")

## Appel du package Matrix.
library(Matrix)
library(dplyr,lib="/home/travail/travaillonslongtemps-master/packages")

## Récupération de toutes les données.
m.data <- read.csv('/home/travail/travaillonslongtemps-master/beer_reviews.csv', stringsAsFactors = FALSE)
m.data <- m.data[m.data[,"review_profilename"]!="",]

## Extraction des données avec les moyennes de votes.
m.user <- select(m.data,review_profilename) %>%
  group_by(review_profilename) %>%
  summarise(count=sum(!is.na(review_profilename)))

m.beer <- select(m.data,beer_beerid,beer_name,beer_style,beer_abv,brewery_id,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
  group_by(beer_beerid,beer_name,beer_style,beer_abv,brewery_id) %>%
  summarise(count=sum(!is.na(beer_beerid)))
  #En prenant les moyennes de vote dans m.beer.
  #summarise(mean_overall=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste), count=sum(!is.na(beer_beerid)))

## Réduction des matrices afin de simplifier les calculs et les temps de calculs.......
#m.user <- filter(m.user, count>300)
##### sans filtre : 33387 Users.
#m.beer <- filter (m.beer, count>300)
##### sans filtre : 66051 Items.
#m.data <- filter(filter(m.data,review_profilename %in% unlist(m.user["review_profilename"])),beer_beerid %in% unlist(m.beer["beer_beerid"]))
##### sans filtre : 1 586 614 Observations.

## Cosinus en fonction du type et du critère évalué.
cosTypeFunction <- function(data,typeMap,mVoteCriteria,meanCriteria){
  m_type.sparse <- sparseMatrix(userMap[unlist(mVoteCriteria[,"review_profilename"]),],typeMap[sapply(unlist(mVoteCriteria[,2]),toString),],x=unlist(mVoteCriteria[,meanCriteria]),use.last.ij=TRUE)
  m_type.normalise <- m_type.sparse/(t(matrix(colSums(m_type.sparse**2),nrow=ncol(m_type.sparse),ncol=nrow(m_type.sparse)))**(1/2) )
  m_type.normalise[is.na(m_type.normalise)]<-0
  cos_type<-t(m_type.normalise)%*%m_type.normalise
  cos_type_mat<-matrix(cos_type,nrow=nrow(cos_type),ncol=ncol(cos_type))
  return (cos_type_mat)
}
## Somme du nombre d'utilisateur votant par type et critère.
countFunction <- function(typeMap,mVoteCriteria,meanCriteria){
  m_type.sparse <- sparseMatrix(userMap[unlist(mVoteCriteria[,"review_profilename"]),],typeMap[sapply(unlist(mVoteCriteria[,2]),toString),],x=unlist(mVoteCriteria[,meanCriteria]),use.last.ij=TRUE)
  binarym<-(m_type.sparse!=0)
  cos_type<-((t(binarym)%*%binarym))
  cos_type_mat<-matrix(cos_type,nrow=nrow(cos_type),ncol=ncol(cos_type))
  return (cos_type_mat)
}

#
#m.data.best <- filter(m.data,review_profilename %in% unlist(m.user["review_profilename"]))
#

userMap <- 0
userMap=matrix(0,nrow=nrow(m.user),ncol=1)
colnames(userMap) <- "user.id"
rownames(userMap)<-unlist(m.user[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user[,"review_profilename"]),1)

##
## Calcul des matrice creuse des votes sur la moyenne des styles.
##
m.style <- select(m.data,beer_style) %>%
  group_by(beer_style) %>%
  
  summarise(count=sum(!is.na(beer_style)))

m.vote_style <- m.data %>%
  group_by(review_profilename,beer_style)%>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

styleMap <- matrix(0,nrow=nrow(m.style),ncol=1)
colnames(styleMap) <- "style.id"
rownames(styleMap) <- unlist(m.style[,"beer_style"])
styleMap[,1] <- seq(1,nrow(m.style[,"beer_style"]),1)

## Calcul des cosinus pour les Styles avec tous les votes des utilisateurs.
cos_style_palate <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_palate")
cos_style_taste <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_taste")
cos_style_appareance <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_appearance")
cos_style_aroma <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_aroma")
cos_style_overall <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean")

count_style <- countFunction(styleMap,m.vote_style,"mean")
#count_style_palate[1:10,1:10]
#min(count_style_palate)

##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##

#Userbest pour les brewery
m.user.best <-filter(m.user,count>=50)
m.data.best <- filter(m.data,review_profilename %in% unlist(m.user.best["review_profilename"]))
#

userMap.best=0
userMap.best=matrix(0,nrow=nrow(m.user.best),ncol=1)
rownames(userMap.best)<-unlist(m.user.best[,"review_profilename"])
userMap.best[,1]=seq(1,nrow(m.user.best[,"review_profilename"]),1)

m.brewery <- select(m.data,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

m.vote_brewery <- m.data.best %>%
  group_by(review_profilename,brewery_id) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))


breweryMap=0
breweryMap <- matrix(0,nrow=nrow(m.brewery),ncol=1)
rownames(breweryMap) <- sapply(unlist(m.brewery[,"brewery_id"]),toString)
breweryMap[,1] <- seq(1,nrow(m.brewery[,"brewery_id"]),1)

#################### Mettre usermapbest dans la fonction
## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
cos_brewery_palate <- cosTypeFunction(m.data.best,breweryMap,m.vote_brewery,"mean_palate")
cos_brewery_taste <- cosTypeFunction(m.data.best,breweryMap,m.vote_brewery,"mean_taste")
cos_brewery_appareance <- cosTypeFunction(m.data.best,breweryMap,m.vote_brewery,"mean_appearance")
cos_brewery_aroma <- cosTypeFunction(m.data.best,breweryMap,m.vote_brewery,"mean_aroma")
cos_brewery_overall <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean")

write.csv(cos_brewery_palate,"/home/travail/travaillonslongtemps-master/saves/cos_brewery_palate.csv", row.names = FALSE)
write.csv(cos_brewery_taste,"/home/travail/travaillonslongtemps-master/saves/cos_brewery_taste.csv", row.names = FALSE)
write.csv(cos_brewery_appareance,"/home/travail/travaillonslongtemps-master/saves/cos_brewery_appareance.csv", row.names = FALSE)
write.csv(cos_brewery_aroma,"/home/travail/travaillonslongtemps-master/saves/cos_brewery_aroma.csv", row.names = FALSE)
write.csv(cos_brewery_overall,"/home/travail/travaillonslongtemps-master/saves/cos_brewery_overall.csv", row.names = FALSE)

#User et data pour les brewery Oswald
m.brewery <- select(m.data,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

m.vote_brewery <- m.data %>%
  group_by(review_profilename,brewery_id) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

breweryMap <- 0
breweryMap <- matrix(0,nrow=nrow(m.brewery),ncol=1)
colnames(breweryMap) <- "index.brewery"
rownames(breweryMap) <- sapply(unlist(m.brewery[,"brewery_id"]),toString)
breweryMap[,1] <- seq(1,nrow(m.brewery[,"brewery_id"]),1)

#################### Mettre usermapbest dans la fonction
## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
cos_brewery_palate <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_palate")
cos_brewery_taste <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_taste")
cos_brewery_appareance <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_appearance")
cos_brewery_aroma <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_aroma")
cos_brewery_overall <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean")

count_brewery <- countFunction(breweryMap,m.vote_brewery,"mean")
##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##
round(5.5)
m.abv <- select(m.data,beer_abv) %>%
  group_by(beer_abv=round(beer_abv)) %>%
  summarise(count=sum(!is.na(beer_abv)))

m.vote_abv <- m.data %>%
  group_by(review_profilename,beer_abv=round(beer_abv)) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

abvMap <- 0
abvMap <- matrix(0,nrow=nrow(m.abv),ncol=1)
colnames(abvMap) <- "abv.id"
rownames(abvMap) <- sapply(unlist(m.abv[,"beer_abv"]),round)
abvMap[,1] <- seq(1,nrow(m.abv[,"beer_abv"]),1)

## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
cos_abv_palate <- cosTypeFunction(m.data,abvMap,m.vote_abv,"mean_palate")
cos_abv_taste <- cosTypeFunction(m.data,abvMap,m.vote_abv,"mean_taste")
cos_abv_appareance <- cosTypeFunction(m.data,abvMap,m.vote_abv,"mean_appearance")
cos_abv_aroma <- cosTypeFunction(m.data,abvMap,m.vote_abv,"mean_aroma")
cos_brewery_overall <- cosTypeFunction(m.data,abvMap,m.vote_abv,"mean")

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

#breweryMap[unlist(m.beer[,"brewery_id"])]
#breweryMap[sapply(unlist(m.beer[,"brewery_id"]),function(x) paste("u",x,sep="")),"index.brewery"]
#unlist(m.beer[,"brewery_id"])

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

m.user.rand <- m.user[(runif(nrow(m.user), 0.0, 1.0)>0.995),]
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



num<-(voteOfStyle%*%cos_style_taste)
den<-(typeVoteCount%*%cos_style_taste)

m_taste.pred.final.style<-m_taste.pred.style+rMean_taste

test.taste.style <- sapply(1:nrow(m_taste.pred.final.style),function(x) max(m_taste.pred.final.style[x,])-min(m_taste.pred.final.style[x,]))


######### PREDICTION BRASSERIEEEEEEEEE


typeVoteCountBrewery <- (m_taste.sparse>0) %*% t(beerAndBreweryMap)
voteOfBrewery <-(m_taste.sparse.centr %*% t(beerAndBreweryMap))
voteOfBrewery[is.na(voteOfStyleBrewery)]<-0
m_taste.pred.brewery<-(voteOfBrewery%*%cos_brewery_taste)/(voteOfBrewery%*%cos_brewery_taste)

numBrew=(voteOfBrewery%*%cos_brewery_taste%*%beerAndBreweryMap)
denBrew=(voteOfBrew%*%cos_brewery_taste%*%beerAndBreweryMap)

typeVoteCountBrewery

truc<-beerAndStyleMap%*%t(beerAndBreweryMap)%*%(cos_brewery_taste==0)%*%beerAndBreweryMap%*%t(beerAndStyleMap)

m_taste.pred.brewery[is.na(m_taste.pred.brewery)]<-0

m_taste.pred.final.brewery<-m_taste.pred.brewery+rMean_taste


#Verification ::::
test <- sapply(1:nrow(m_taste.pred.final.brewery),function(x) max(m_taste.pred.final.brewery[x,])-min(m_taste.pred.final.brewery[x,]))

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
