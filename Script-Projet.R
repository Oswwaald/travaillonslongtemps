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
m.user <-filter(m.user,count>=100)
m.data <- filter(m.data,review_profilename %in% unlist(m.user["review_profilename"]))

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
rownames(styleMap) <- unlist(m.style[,"beer_style"])
styleMap[,1]< - seq(1,nrow(m.style[,"beer_style"]),1)

## Fonction magique
cosTypeFunction <- function(data,typeMap,mVoteCriteria,meanCriteria){
  m_style_palate.sparse <- sparseMatrix(userMap[unlist(mVoteCriteria[,"review_profilename"]),],typeMap[unlist(mVoteCriteria[,2]),],x=unlist(mVoteCriteria[,meanCriteria]),use.last.ij=TRUE)
  m_style_palate.normalise <- m_style_palate.sparse/(t(matrix(colSums(m_style_palate.sparse**2),nrow=ncol(m_style_palate.sparse),ncol=nrow(m_style_palate.sparse)))**(1/2) )
  m_style_palate.normalise[is.na(m_style_palate.normalise)]<-0
  cos_style_palate<-t(m_style_palate.normalise)%*%m_style_palate.normalise
  cos_style_real_palate<-matrix(cos_style_palate,nrow=nrow(cos_style_palate),ncol=ncol(cos_style_palate))
  return (cos_style_real_palate)
}

## Calcul des cosinus pour les Styles avec tous les votes des utilisateurs.
cos_style_palate <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_palate")
cos_style_taste <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_taste")
cos_style_appareance <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_appearance")
cos_style_aroma <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_aroma")
#cos_style_overall <- cosTypeFunction(m.data,styleMap,m.vote_style,"mean_overall")

##
## Calcul des matrice creuse des votes sur la moyenne des brasseries.
##
m.vote_brewery <- m.data %>%
  group_by(review_profilename,brewery_id) %>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))

m.brewery <- select(m.data,brewery_id) %>%
  group_by(brewery_id) %>%
  summarise(count=sum(!is.na(brewery_id)))

breweryMap <- matrix(0,nrow=nrow(m.brewery),ncol=1)
rownames(breweryMap) <- unlist(m.brewery[,"brewery_id"])
breweryMap[,1] <- seq(1,nrow(m.brewery[,"brewery_id"]),1)

## Calcul des cosinus pour les Brasseries avec tous les votes des utilisateurs.
cos_brewery_palate <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_palate")
cos_brewery_taste <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_taste")
cos_brewery_appareance <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_appearance")
cos_brewery_aroma <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_aroma")
#cos_brewery_overall <- cosTypeFunction(m.data,breweryMap,m.vote_brewery,"mean_overall")




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
