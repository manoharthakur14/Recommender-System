rm(list=ls())
library(kableExtra)
library(data.table)
library(car)
library(tm)
library(Matrix)
library(slam)
library(babynames) # data package
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ggplot2)
library(tidyr)
library(lubridate)
library(Amelia)
library(stringr)
library(plotly)
library(recommenderlab)

# Row names and column names to dataframes
# rownames(usersdf) <- c(1:nrow(usersdf))
# rownames(moviesdf) <- c(1:nrow(moviesdf))
# rownames(ratingsdf) <- c(1:nrow(ratingsdf))
# colnames(usersdf) <- c("UserID","Gender","Age","Occupation","Zip-code")
# colnames(moviesdf) <- c("MovieID","Title","Genres")
# colnames(ratingsdf) <- c("UserID","MovieID","Rating","Timestamp")

# read in movies files
movies_raw = readLines('movies.dat')
movies_raw = gsub(pattern = "::", replacement = ";", movies_raw)
writeLines(movies_raw, "movies.csv")
closeAllConnections()
movies = fread('movies.csv', sep = ";", header = F)
rm(movies_raw)
colnames(movies) = c('MovieID', 'Title', 'Genres')

# read in ratings file
ratings_raw = readLines('ratings.dat')
ratings_raw = gsub("::", ";", ratings_raw)
writeLines(ratings_raw, 'ratings.csv')
closeAllConnections()
ratings = fread('ratings.csv', sep = ";", header = F)
rm(ratings_raw)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# read in tags data
users_raw = readLines('users.dat')
users_raw = gsub("::", ";", users_raw)
writeLines(users_raw, 'users.csv')
users = read.csv('users.csv', sep = ";", header = F)
rm(users_raw)
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip')


# colnames(movies) <- c("MovieID", "Title", "Genres")
# colnames(ratings) <- c("UserID", "MovieID", "Rating", "Timestamp")
# colnames(users) <- c("UserID", "Gender", "Age", "Occupation", "Zip-code")


str(movies)
summary(movies)
str(users)
summary(users)
str(ratings)
summary(ratings)



str(ratings)
ratings[, DateTime := as.POSIXct(Timestamp, origin = '1970-01-01')]
ratings[, Date := as.Date(DateTime)]
ratings[, Timestamp := NULL]

sum(is.na(ratings))



str(movies)
movies[, Year := str_sub(Title,-5,-2)]
movies[, Year := as.integer(Year)]
head(movies)

movies$Title <- gsub("\\(\\d+\\)", "", movies$Title)
#movies$Title <- str_trim(as.character(movies$Title), side = "both")
head(movies)

str(users)
occupation.encode <- function(x){
  switch(as.character(x), "0"="other or notspecified"
         ,"1"="academic/educator"
         ,"2"="artist"
         ,"3"="clerical/admin"
         ,"4"="college/gradstudent"
         ,"5"="customerservice"
         ,"6"="doctor/healthcare"
         ,"7"="executive/managerial"
         ,"8"="farmer"
         ,"9"="homemaker"
         ,"10"="K-12student"
         ,"11"="lawyer"
         ,"12"="programmer"
         ,"13"="retired"
         ,"14"="sales/marketing"
         ,"15"="scientist"
         ,"16"="self-employed"
         ,"17"="technician/engineer"
         ,"18"="tradesman/craftsman"
         ,"19"="unemployed"
         ,"20"="writer")
}

Age.encoding <- function(x){
  switch(as.character(x), "1"="Under 18"
         ,"18"="18-24"
         ,"25"="25-34"
         ,"35"="35-44"
         ,"45"="45-49"
         ,"50"="50-55"
         ,"56"="56+")
}

users$Occupation <-  sapply(users$Occupation, occupation.encode)
users$Age.Grouped <- sapply(users$Age, Age.encoding)

head(users)
any(is.na(users))

