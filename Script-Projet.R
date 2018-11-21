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
m.data <- read.csv('beer_reviews.csv')
head(m.data)


m_easy<-inner_join(m.data,userTraining,by=c("review_profilename"="review_profilename"))
## Extraction des données avec les moyennes de votes.

m.vote_style <- m.data %>%
  group_by(review_profilename,beer_style)%>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))


m_easy.vote_brewery <- m_easy %>%
  group_by(review_profilename,brewery_id,brewery_name)%>%
  summarise(count=sum(!is.na(review_profilename)),mean=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste))


m.user <- select(m.data,review_profilename) %>%
 group_by(review_profilename) %>%
 summarise(count=sum(!is.na(review_profilename)))
userWithRand<-mutate(m.user,rand=runif(33388, 0.0, 1.0))
userTraining<-filter(userWithRand,rand>0.9)

m.beer <- select(m.data,beer_beerid,beer_name,beer_style,beer_abv,brewery_id,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 group_by(beer_beerid,beer_name,beer_style,beer_abv,brewery_id) %>%
 summarise(mean_overall=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste), count=sum(!is.na(beer_beerid)))

m.brewery <- select(m.data,brewery_id,brewery_name,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 group_by(brewery_id,brewery_name) %>%
 summarise(mean_overall=mean(review_overall),mean_aroma=mean(review_aroma),mean_appearance=mean(review_appearance),mean_palate=mean(review_palate),mean_taste=mean(review_taste),count=sum(!is.na(brewery_id)))

m.vote <- select(m.data,beer_beerid,review_profilename,review_time,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 group_by(beer_beerid,review_profilename,review_time,review_overall,review_aroma,review_appearance,review_palate,review_taste) %>%
 summarise()

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

