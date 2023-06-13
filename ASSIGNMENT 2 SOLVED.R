library(imager)
#question 1
data("iris")
setosa<-iris[1:50,]
versicolor<-iris[51:100,]
virginica<-iris[101:150,]
par(mfrow=c(1,3))
boxplot(setosa$Sepal.Length,setosa$Sepal.Width,setosa$Petal.Length,setosa$Petal.Width,
        names = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
                  main = "boxplot of setosa")
boxplot(versicolor$Sepal.Length,versicolor$Sepal.Width,versicolor$Petal.Length,
        versicolor$Petal.Width,names = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
        main ="boxplot of versicolor")
boxplot(virginica$Sepal.Length,virginica$Sepal.Width,virginica$Petal.Length,
        virginica$Petal.Width,names = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"),
        main ="boxplot of virginica")
spiecescolor<-1*(iris$Species=="setosa")+2*(iris$Species=="versicolor")+3*(iris$Species=="virginica")
plot(iris$Sepal.Length,iris$Petal.Length,xlab ="Sepal length",ylab ="petal length",col= spiecescolor,pch=16)
legend("bottomright",pch=16,col = c(1,2,3),legend = c("setosa","versicolor","virginica"))



#question 3
library(MASS)
data("ships")
sumA<-sum(ships[1:8,c(5)])
for(i in 1:8){ships[i,4]<-sumA}
sumB<-sum(ships[9:16,c(5)])
for(i in 9:16){ships[i,4]<-sumB}
sumC<-sum(ships[17:24,c(5)])
for(i in 17:24){ships[i,4]<-sumC}
sumD<-sum(ships[25:32,c(5)])
for(i in 25:32){ships[i,4]<-sumD}
sumE<-sum(ships[33:40,c(5)])
for(i in 33:40){ships[i,4]<-sumE}
plot(ships$type,ships$service,xlab="#type",ylab="#accidents",pch=16)
#please zoom the plot to clearly visible
#I AGREE WITH THE STATEMENT
library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)



#question 4
html<-read_html("https://stats.stackexchange.com/questions?tab=Votes")
title<-html%>%html_elements(".s-post-summary--content-title a")%>%html_text()
title<-str_remove_all(title,'"')
random1<-html%>%html_elements(".s-post-summary--stats-item-number ")%>%html_text()
random<-array(0,dim=15)
for(i in 1:15){random[i]<-random1[3*i]}
views<-random[1:15]
for(i in 1:15){random[i]<-random1[3*i-1]}
votes<-random[1:15]
for(i in 1:15){random[i]<-random1[3*i-2]}
answers<-random[1:15]
data<-data.frame("The title of the questions"=title,"The number of views"=views,
                 "The number of answers"=answers,"The notes of votes"=votes)



#question 2
library(imager)
flip<-function(x){
  dim(x)
  col.mat <- as.array(x[, ,1, ])
  
  dims <- dim(col.mat)
  rot <- array(0, dim = dims)
  
  for(i in 1:dims[1])
  {
    rot[i, , ] <- col.mat[dims[1]-i+1, , ]
    
  }
  plot(as.cimg(rot))
  
}
dog<-load.image("dog.jpeg")
flip(dog)



#question 5
days<-function(){full<-99
half<-1
for(i in 1:199){
  if(rbinom(1,1,(full)/(full+half))){full<-full-1
  half=half+1}
  else{return(i+1)}
}
return(count)}
ndays<-numeric(1000)
for (i in 1:1000) {
  ndays[i]<-days()
}
mean(ndays)

