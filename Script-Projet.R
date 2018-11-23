## Sources des données : https://www.kaggle.com/rdoume/beerreviews
install.packages('dplyr')

## Appel du package Matrix.
library(Matrix)
library(dplyr)

## Récupération de toutes les données.
m.data <- read.csv('beer_reviews.csv', stringsAsFactors = FALSE)
m.data <- m.data[m.data[,"review_profilename"]!="",]

## Extraction des données avec les moyennes de votes.
m.user <- select(m.data,review_profilename) %>%
  group_by(review_profilename) %>%
  summarise(count=sum(!is.na(review_profilename)))

m.beer <- select(m.data,beer_beerid,beer_name,beer_style,beer_abv,brewery_id,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
  group_by(beer_beerid,beer_name,beer_style,beer_abv,brewery_id) %>%
  summarise(mean_overall=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste), count=sum(!is.na(beer_beerid)))


m.data.best <- filter(m.data,review_profilename %in% unlist(m.user["review_profilename"]))

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

styleMap <- matrix(0,nrow=nrow(m.style),ncol=1)
head(styleMap)
rownames(styleMap) <- unlist(m.style[,"beer_style"])
styleMap[,1] <- seq(1,nrow(m.style[,"beer_style"]),1)

## Fonction magique
##################OSWALD CHANGE les noms
cosTypeFunction <- function(data,typeMap,mVoteCriteria,meanCriteria){
  m_style.sparse <- sparseMatrix(userMap[unlist(mVoteCriteria[,"review_profilename"]),],typeMap[sapply(unlist(mVoteCriteria[,2]),toString),],x=unlist(mVoteCriteria[,meanCriteria]),use.last.ij=TRUE)
  m_style.normalise <- m_style.sparse/(t(matrix(colSums(m_style.sparse**2),nrow=ncol(m_style.sparse),ncol=nrow(m_style.sparse)))**(1/2) )
  m_style.normalise[is.na(m_style.normalise)]<-0
  cos_style<-t(m_style.normalise)%*%m_style.normalise
  cos_style_real<-matrix(cos_style,nrow=nrow(cos_style),ncol=ncol(cos_style))
  return (cos_style_real)
}

countFunction <- function(data,typeMap,mVoteCriteria,meanCriteria){
  m_style.sparse <- sparseMatrix(userMap[unlist(mVoteCriteria[,"review_profilename"]),],typeMap[sapply(unlist(mVoteCriteria[,2]),toString),],x=unlist(mVoteCriteria[,meanCriteria]),use.last.ij=TRUE)
  binarym<-(m_style.sparse!=0)
  cos_style<-((t(binarym)%*%binarym))
  cos_style_real<-matrix(cos_style,nrow=nrow(cos_style),ncol=ncol(cos_style))
  return (cos_style_real)
}

count_style_palate <- countFunction(m.data,styleMap,m.vote_style,"mean_palate")
min(count_style_palate)


count_brewery_palate <- countFunction(m.data,breweryMap,m.vote_brewery,"mean_palate")

## Calcul des cosinus pour les Styles avec tous les votes des utilisateurs.
cos_style_palate <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_palate")
cos_style_taste <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_taste")
cos_style_appareance <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_appearance")
cos_style_aroma <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_aroma")
#cos_style_overall <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_overall")

##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##
m.user.best <-filter(m.user,count>=100)
m.data.best <- filter(m.data,review_profilename %in% unlist(m.user.best["review_profilename"]))

userMap.best=0
userMap.best=matrix(0,nrow=nrow(m.user.best),ncol=1)
rownames(userMap.best)<-unlist(m.user.best[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user.best[,"review_profilename"]),1)


m.vote_brewery <- m.data.best %>%
  group_by(review_profilename,brewery_id) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

m.brewery <- select(m.data.best,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

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
#cos_brewery_overall <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_overall")

write.csv(cos_brewery_palate,"cos_brewery_palate.csv", row.names = FALSE, sep=",")
write.csv(cos_brewery_palate,"cos_brewery_palate.csv", row.names = FALSE, sep=",")
write.csv(cos_brewery_palate,"cos_brewery_palate.csv", row.names = FALSE, sep=",")
write.csv(cos_brewery_palate,"cos_brewery_palate.csv", row.names = FALSE, sep=",")

########### Faire de même pour l'alcoolémie.

## Méthode ItemItem


m.user.rand<-m.user[(runif(33387, 0.0, 1.0)>0.99),]
m.data.rand<-filter(m.data,review_profilename %in% unlist(m.user.rand["review_profilename"]))

userMap=0
userMap=matrix(0,nrow=nrow(m.user),ncol=1)
rownames(userMap)<-unlist(m.user[,"review_profilename"])
userMap[,1]=seq(1,nrow(m.user[,"review_profilename"]),1)
userMap[unlist(m.user.rand)]
m.user.rand
m.user.rand[,1]
userMap
userMap[unlist(m.user.rand[,1])]
userMap[unlist(m.user.rand[,1]),1]
m.pred<-m.data[userMap[unlist(m.user.rand[,1]),1],]
userMap[,1]

m.data[,"beer_beerid"]
m.pred
m.pred[,"beer_beerid"]
m.pred[,"review_profilename"]
m.pred[,"beer_beerid"]
unlist(m.beer[,"brewery_id"])
breweryMap[unlist(m.beer[,"brewery_id"])]
breweryMap
m.brewery <- select(m.data,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

breweryMap
breweryMap <- matrix(0,nrow=nrow(m.brewery),ncol=1)
rownames(breweryMap) <- sapply(unlist(m.brewery[,"brewery_id"]),toString)
breweryMap[,1] <- seq(1,nrow(m.brewery[,"brewery_id"]),1)
unlist(m.beer[,"brewery_id"])[10099]
breweryMap[9]

breweryMap[unlist(m.beer[,"brewery_id"])]

unlist(m.beer[,"brewery_id"])
m_taste.sparse <- sparseMatrix(breweryMap[sapply(unlist(m.beer[,"brewery_id"]),toString)],unlist(m.beer[,"beer_beerid"]),x=,use.last.ij=TRUE)

rowSums(m_taste.sparse)
m.pred[,"review_profilename"]
m_taste.sparse
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
