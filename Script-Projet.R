## Sources des données : https://www.kaggle.com/rdoume/beerreviews
install.packages('dplyr')
install.packages('ggplot2')
install.packages('gridExtra')

## Appel du package Matrix.
library(Matrix)
library(dplyr)
library(ggplot2)
library(gridExtra) 

## Récupération de toutes les données.
m.data <- read.csv('beer_reviews.csv', stringsAsFactors = FALSE)
m.data <- m.data[m.data[,"review_profilename"]!="",]

## Extraction des données avec les moyennes de votes.
m.user <- select(m.data,review_profilename) %>%
  group_by(review_profilename) %>%
  summarise(count=sum(!is.na(review_profilename)))

userMap=0
userMap=matrix(0,nrow=nrow(m.user),ncol=1)
rownames(userMap)<-unlist(m.user[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user[,"review_profilename"]),1)

##
## Calcul des matrice creuse des votes sur la moyenne des styles.
##

m.vote_style <- m.data %>%
  group_by(review_profilename,beer_style)%>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

m.style <- select(m.data,beer_style) %>%
  group_by(beer_style) %>%
  summarise(count=sum(!is.na(beer_style)))

styleMap=0
styleMap=matrix(0,nrow=nrow(m.style),ncol=1)
rownames(styleMap)<-unlist(m.style[,"beer_style"])
styleMap[,1]=seq(1,nrow(m.style[,"beer_style"]),1)
m.vote_style[, "review_profilename"]

m_style_palate.sparse <- sparseMatrix(userMap[unlist(m.vote_style[,"review_profilename"]),],styleMap[unlist(m.vote_style[,"beer_style"]),],x=unlist(m.vote_style[,"mean_palate"]),use.last.ij=TRUE)
m_style_taste.sparse <- sparseMatrix(userMap[unlist(m.vote_style[,"review_profilename"]),],styleMap[unlist(m.vote_style[,"beer_style"]),],x=unlist(m.vote_style[,"mean_taste"]),use.last.ij=TRUE)
m_style_appearance.sparse <- sparseMatrix(userMap[unlist(m.vote_style[,"review_profilename"]),],styleMap[unlist(m.vote_style[,"beer_style"]),],x=unlist(m.vote_style[,"mean_appearance"]),use.last.ij=TRUE)
m_style_aroma.sparse <- sparseMatrix(userMap[unlist(m.vote_style[,"review_profilename"]),],styleMap[unlist(m.vote_style[,"beer_style"]),],x=unlist(m.vote_style[,"mean_aroma"]),use.last.ij=TRUE)
m_style_overall.sparse <- sparseMatrix(userMap[unlist(m.vote_style[,"review_profilename"]),],styleMap[unlist(m.vote_style[,"beer_style"]),],x=unlist(m.vote_style[,"mean"]),use.last.ij=TRUE)

## Calcul des cosinus pour les Styles avec tous les votes des utilisateurs.
m_style_palate.normalise <- m_style_palate.sparse/(t(matrix(colSums(m_style_palate.sparse**2),nrow=ncol(m_style_palate.sparse),ncol=nrow(m_style_palate.sparse)))**(1/2) )
m_style_palate.normalise[is.na(m_style_palate.normalise)]<-0
cos_style_palate<-t(m_style_palate.normalise)%*%m_style_palate.normalise
cos_style_real_palate<-matrix(cos_style_palate,nrow=nrow(cos_style_palate),ncol=ncol(cos_style_palate))

m_style_taste.normalise <- m_style_taste.sparse/(t(matrix(colSums(m_style_taste.sparse**2),nrow=ncol(m_style_taste.sparse),ncol=nrow(m_style_taste.sparse)))**(1/2) )
m_style_taste.normalise[is.na(m_style_taste.normalise)]<-0
cos_style_taste<-t(m_style_taste.normalise)%*%m_style_taste.normalise
cos_style_real_taste<-matrix(cos_style_taste,nrow=nrow(cos_style_taste),ncol=ncol(cos_style_taste))

m_style_appearance.normalise <- m_style_appearance.sparse/(t(matrix(colSums(m_style_appearance.sparse**2),nrow=ncol(m_style_appearance.sparse),ncol=nrow(m_style_appearance.sparse)))**(1/2) )
m_style_appearance.normalise[is.na(m_style_appearance.normalise)]<-0
cos_style_appearance<-t(m_style_appearance.normalise)%*%m_style_appearance.normalise
cos_style_real_appearance<-matrix(cos_style_appearance,nrow=nrow(cos_style_appearance),ncol=ncol(cos_style_appearance))

m_style_aroma.normalise <- m_style_aroma.sparse/(t(matrix(colSums(m_style_aroma.sparse**2),nrow=ncol(m_style_aroma.sparse),ncol=nrow(m_style_aroma.sparse)))**(1/2) )
m_style_aroma.normalise[is.na(m_style_aroma.normalise)]<-0
cos_style_aroma<-t(m_style_aroma.normalise)%*%m_style_aroma.normalise
cos_style_real_aroma<-matrix(cos_style_aroma,nrow=nrow(cos_style_aroma),ncol=ncol(cos_style_aroma))

##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##

m.user.best<-filter(m.user,count>=100)
m.data.best <- filter(m.data,review_profilename %in% unlist(m.user.best["review_profilename"]))

userMap=0
userMap=matrix(0,nrow=nrow(m.user.best),ncol=1)
rownames(userMap)<-unlist(m.user.best[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user.best[,"review_profilename"]),1)


m.vote_brewery <- m.data.best %>%
  group_by(review_profilename,brewery_id) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

m.brewery <- select(m.data.best,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

breweryMap=0
breweryMap=matrix(0,nrow=nrow(m.brewery),ncol=1)
rownames(breweryMap)<-unlist(m.brewery[,"brewery_id"])
breweryMap[,1]=seq(1,nrow(m.brewery[,"brewery_id"]),1)
m.vote_brewery[, "review_profilename"]

m_brewery_palate.sparse <- sparseMatrix(userMap[unlist(m.vote_brewery[,"review_profilename"]),],breweryMap[sapply(unlist(m.vote_brewery[,"brewery_id"]),toString),],x=unlist(m.vote_brewery[,"mean_palate"]),use.last.ij=TRUE)
m_brewery_taste.sparse <- sparseMatrix(userMap[unlist(m.vote_brewery[,"review_profilename"]),],breweryMap[sapply(unlist(m.vote_brewery[,"brewery_id"]),toString),],x=unlist(m.vote_brewery[,"mean_taste"]),use.last.ij=TRUE)
m_brewery_appearance.sparse <- sparseMatrix(userMap[unlist(m.vote_brewery[,"review_profilename"]),],breweryMap[sapply(unlist(m.vote_brewery[,"brewery_id"]),toString),],x=unlist(m.vote_brewery[,"mean_appearance"]),use.last.ij=TRUE)
m_brewery_aroma.sparse <- sparseMatrix(userMap[unlist(m.vote_brewery[,"review_profilename"]),],breweryMap[sapply(unlist(m.vote_brewery[,"brewery_id"]),toString),],x=unlist(m.vote_brewery[,"mean_aroma"]),use.last.ij=TRUE)
m_brewery_overall.sparse <- sparseMatrix(userMap[unlist(m.vote_brewery[,"review_profilename"]),],breweryMap[sapply(unlist(m.vote_brewery[,"brewery_id"]),toString),],x=unlist(m.vote_brewery[,"mean"]),use.last.ij=TRUE)

## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
m_brewery_palate.normalise <- m_brewery_palate.sparse/(t(matrix(colSums(m_brewery_palate.sparse**2),nrow=ncol(m_brewery_palate.sparse),ncol=nrow(m_brewery_palate.sparse)))**(1/2) )
m_brewery_palate.normalise[is.na(m_brewery_palate.normalise)]<-0
cos_brewery_palate<-t(m_brewery_palate.normalise)%*%m_brewery_palate.normalise
cos_brewery_real_palate<-matrix(cos_brewery_palate,nrow=nrow(cos_brewery_palate),ncol=ncol(cos_brewery_palate))

m_brewery_taste.normalise <- m_brewery_taste.sparse/(t(matrix(colSums(m_brewery_taste.sparse**2),nrow=ncol(m_brewery_taste.sparse),ncol=nrow(m_brewery_taste.sparse)))**(1/2) )
m_brewery_taste.normalise[is.na(m_brewery_taste.normalise)]<-0
cos_brewery_taste<-t(m_brewery_taste.normalise)%*%m_brewery_taste.normalise
cos_brewery_real_taste<-matrix(cos_brewery_taste,nrow=nrow(cos_brewery_taste),ncol=ncol(cos_brewery_taste))

m_brewery_appearance.normalise <- m_brewery_appearance.sparse/(t(matrix(colSums(m_brewery_appearance.sparse**2),nrow=ncol(m_brewery_appearance.sparse),ncol=nrow(m_brewery_appearance.sparse)))**(1/2) )
m_brewery_appearance.normalise[is.na(m_brewery_appearance.normalise)]<-0
cos_brewery_appearance<-t(m_brewery_appearance.normalise)%*%m_brewery_appearance.normalise
cos_brewery_real_appearance<-matrix(cos_brewery_appearance,nrow=nrow(cos_brewery_appearance),ncol=ncol(cos_brewery_appearance))

m_brewery_aroma.normalise <- m_brewery_aroma.sparse/(t(matrix(colSums(m_brewery_aroma.sparse**2),nrow=ncol(m_brewery_aroma.sparse),ncol=nrow(m_brewery_aroma.sparse)))**(1/2) )
m_brewery_aroma.normalise[is.na(m_brewery_aroma.normalise)]<-0
cos_brewery_aroma<-t(m_brewery_aroma.normalise)%*%m_brewery_aroma.normalise
cos_brewery_real_aroma<-matrix(cos_brewery_aroma,nrow=nrow(cos_brewery_aroma),ncol=ncol(cos_brewery_aroma))





userWithRand<-mutate(m.user,rand=runif(33387, 0.0, 1.0))
#userTraining<-filter(userWithRand,rand>0.9)
#m_easy<-inner_join(m.data,userTraining,by=c("review_profilename"="review_profilename"))


m.user.best<-filter(m.user,count>=30)




m_easy.vote_brewery <- m_easy %>%
  group_by(review_profilename,brewery_id,brewery_name)%>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))




m.beer <- select(m.data,beer_beerid,beer_name,beer_style,beer_abv,brewery_id,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 group_by(beer_beerid,beer_name,beer_style,beer_abv,brewery_id) %>%
 summarise(mean_overall=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste), count=sum(!is.na(beer_beerid)))

m.beer.best <- filter(m.beer,count>10)

m.data.best <- filter(filter(m.data,review_profilename %in% unlist(m.user.best["review_profilename"])),beer_beerid %in% unlist(m.beer.best["beer_beerid"]))

m.brewery <- select(m.data,brewery_id,brewery_name,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 group_by(brewery_id,brewery_name) %>%
 summarise(mean_overall=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste),count=sum(!is.na(brewery_id)))

m.vote <- select(m.data,beer_beerid,review_profilename,review_time,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 group_by(beer_beerid,review_profilename,review_time,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 summarise()

userMap=0
userMap=matrix(0,nrow=nrow(m.user.best),ncol=1)
rownames(userMap)<-unlist(m.user.best[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user.best[,"review_profilename"]),1)


beerMap=0
beerMap=matrix(0,nrow=nrow(m.beer.best),ncol=1)
rownames(beerMap)<-unlist(m.beer.best[,"beer_beerid"])
beerMap[,1]=seq(1,nrow(m.beer.best[,"beer_beerid"]),1)
beerMap[58046,]

m_palate.sparse <- sparseMatrix(userMap[m.data.best[,"review_profilename"],],beerMap[sapply(m.data.best[,"beer_beerid"],toString),],x=m.data.best[,"review_palate"],use.last.ij=TRUE)
m_taste.sparse <- sparseMatrix(userMap[m.data.best[,"review_profilename"],],beerMap[sapply(m.data.best[,"beer_beerid"],toString),],x=m.data.best[,"review_taste"],use.last.ij=TRUE)
m_appearance.sparse <- sparseMatrix(userMap[m.data.best[,"review_profilename"],],beerMap[sapply(m.data.best[,"beer_beerid"],toString),],x=m.data.best[,"review_appearance"],use.last.ij=TRUE)
m_aroma.sparse <- sparseMatrix(userMap[m.data.best[,"review_profilename"],],beerMap[sapply(m.data.best[,"beer_beerid"],toString),],x=m.data.best[,"review_aroma"],use.last.ij=TRUE)
m_overall.sparse <- sparseMatrix(userMap[m.data.best[,"review_profilename"],],beerMap[sapply(m.data.best[,"beer_beerid"],toString),],x=m.data.best[,"review_overall"],use.last.ij=TRUE)

m_palate.normalise <- m_palate.sparse/(t(matrix(colSums(m_palate.sparse**2),nrow=ncol(m_palate.sparse),ncol=nrow(m_palate.sparse)))**(1/2) )
m_palate.normalise[is.na(m_palate.normalise)]<-0

cos_palate<-t(m_palate.normalise)%*%m_palate.normalise

colSums(m_palate.sparse**2)

(t(matrix(0,nrow=ncol(m_palate.sparse),ncol=nrow(m_palate.sparse))))
  
rownames(m.sparse) <- paste('u', 1:nrow(m.sparse), sep='')
colnames(m.sparse) <- paste('i', 1:ncol(m.sparse), sep='')


## Identification du nombre de bières et d'utilisateurs différents.
nbVotesPerBeer<-m.data %>% 
  count(beer_beerid)
nbVotesPerUser<-m.data %>% 
  count(review_profilename)

## Visualisation du pannel de bières et du pannel d'utilisateur.
qplot(nbVotesPerBeer$n, log = "xy", geom = "histogram",xlab="Nombre de votes",ylab="Bières")
qplot(nbVotesPerUser$n, log = "xy", geom = "histogram",xlab="Nombre de votes",ylab="Utilisateurs")

#Test de recherches rapides
m.beer_best<-filter(m.beer,count>100)
m.data %>% 
  max(review_time)
m.data %>% 
  group_by() %>% 
  summarise(min = min(review_time))
m_palate.sparse[1,]
