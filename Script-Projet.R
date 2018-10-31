library(Matrix)


## https://www.kaggle.com/rdoume/beerreviews


##Init
m = read.csv('beer_reviews.csv')
m <- as.matrix(m)
head(m)
