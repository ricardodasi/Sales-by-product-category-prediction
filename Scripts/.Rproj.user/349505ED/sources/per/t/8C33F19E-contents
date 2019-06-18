#load the packages ####

pacman::p_load(caret,mlbench,plotly,ggplot2,corrplot,usdm,rpart,rpart.plot)

#Load the data ####

existing.product.attributes <- 
  read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 5/Prediction of products by brand/Data Sets/existingproductattributes2017.csv')


#We dummify the categorical variables ####
#numeric way to be able to work with them in a

new.data.frame<-dummyVars("~.",data = existing.product.attributes)

ready.data<-data.frame(predict(new.data.frame,newdata = existing.product.attributes))


#we check the structure to see that there is no nominal data ####

str(ready.data)

#we can see from the results that all de columms are numerical
#we also check for the missing values

summary(ready.data)


#bestsellerrank has 15 na's
#for this experiment we decide to remove the missing column

ready.data$BestSellersRank<- NULL


#we create a correlation matrix to analize correlation and variable importance ####

# correlation.matrix <- cor(ready.data)
# 
# #now we plot it
# 
# corrplot(correlation.matrix,
#          type = "upper",
#          method = 'color',
#          tl.cex = 0.5,
#          addCoef.col = 'black',
#          number.cex = 0.3)
# 
# 
# #With this I decide some preliminary deletion of variables to use for the model
# #that have a good relation, I delete and then test colinearity
# #5star reviews gets deleted because it has a perfect correlation with volume
# 
# 
# ready.data<-ready.data[,c(16,17,18,19,20,21,22,28)]
# 
# #let's check specifically for colinearity
# 
# correlation.matrix.2<-cor(ready.data)
# 
# corrplot(correlation.matrix.2,
#          type = "upper",
#          method = 'color',
#          tl.cex = 0.7,
#          addCoef.col = 'black',
#          number.cex = 0.3)
# 
# #3 stars and 1 star stand to be removed, we also check variable importance through a 
# #decision tree method
# 
# b<- c(0,2000,4000,10000)
# 
# existing.product.attributes$Volume<- cut(existing.product.attributes$Volume,breaks = b)
# existing.product.attributes$BestSellersRank<- NULL
# existing.product.attributes$ProductType<-NULL
# existing.product.attributes$x5StarReviews<-NULL
# 
# decision.tree.model <- train(Volume ~.,
#                              data = existing.product.attributes,
#                              method = 'rpart',
#                              na.action=na.exclude)
# 
# decision.tree.prediction<- predict(decision.tree.model,existing.product.attributes)
# 
# postResample(decision.tree.prediction,existing.product.attributes$Volume)
# 
# 
# r.narco<-rpart(Volume~.,data = existing.product.attributes)
# 
# rpart.plot(r.narco, extra=10)
# 
# #after plotting the decision tree, we can see that the importance of variables 
# #is similar to what was chosen with the correlation matrix, we have decided to 
# #model with the following variables:>4stars >2stars >positive reviews 
# #>negative reviews and volume. switching previous tree to anotation


#Taking out the variables and working the model ####

ready.data<-ready.data[,c(14,16,18,20,21,28)]


#create data partition and set seed

set.seed(123)

intraining <- createDataPartition(ready.data$Volume,
                                  p=0.70,
                                  list = FALSE) 

training <- ready.data[intraining,]
testing <- ready.data[-intraining,]


  #creating and testing models

lin <-c('svmLinear','rf','knn')
comparison<-c()

for (i in lin){
  
  fitcontrol <- trainControl(method = 'cv')
  
  mods <- train(Volume~.,
                data = training,
                method = lin,
                trcontrol = fitcontrol)
  
  pr<- predict(mods,testing)
  
  results<-postResample(pr,testing$Volume)
  
  comparison <-cbind(comparison,results)
  
}

colnames(comparison)<-lin


#Checking for outliers  and visualizations, we plot the errors ####

ggplot(training, aes(x=Volume))+geom_histogram(color = 'black',
                                                 fill = "lightblue",
                                                 bins = 20)



#We can observe that there are outliers over the 6000 Volume mark

ggplot(training, aes(x=x4StarReviews))+geom_histogram(color = 'black',
                                                 fill = "lightblue",
                                                 bins = 20)

ggplot(training, aes(x=x2StarReviews))+geom_histogram(color = 'black',
                                                 fill = "lightblue",
                                                 bins = 20)

ggplot(training, aes(x=PositiveServiceReview))+geom_histogram(color = 'black',
                                                        fill = "lightblue",
                                                        bins = 20)

ggplot(training, aes(x=NegativeServiceReview))+geom_histogram(color = 'black',
                                                              fill = "lightblue",
                                                              bins = 20)

#we try removing outliers, the other variables have less dramatic
#gaps in behaviour

training<- training[-which(training$Volume>6000),]

training<- training[-which(training$x2StarReviews>20),]

training<- training[-which(training$PositiveServiceReview>200),]




ggplot(ready.data, aes(x=Volume))+geom_histogram(color = 'black',
                                                 fill = "lightblue",
                                                 bins = 20)

#We run the model again wihtout outliers

lin <-c('svmLinear','rf','knn')
comparison<-c()

for (i in lin){
  
  fitcontrol <- trainControl(method = 'cv')
  
  mods <- train(Volume~.,
                data = training,
                method = lin,
                trcontrol = fitcontrol)
  
  pr<- predict(mods,testing)
  
  results<-postResample(pr,testing$Volume)
  
  comparison <-cbind(comparison,results)

  
}

colnames(comparison)<-lin

#we run the model with the additional variable price and the results improve marginally,
#Given this, we chose to let this variable into the model, not because of the 
#efficiency, but because of the relevance into the decission making. 


#We have tested the optimized models and we ahve decided to run and SVM linear algorithm,
#now we save this one and we try to plot the errors to understand the flaws

fitcontrol.2 <- trainControl(method = 'boot',
                             number = 1000)

  
svm.model <- train(Volume ~ .,
                    data = training,
                    method = "svmLinear",
                    trcontrol = fitcontrol.2
                    )

pr<- predict(svm.model,testing)
  
results.2<-postResample(pr,testing$Volume)
  

testing$predictions<- pr
  
testing$Relative.error <- abs(testing$Volume-testing$predictions)/testing$Volume

ggplot(testing, aes(x = Volume, y = Relative.error)) + geom_point(size = 3,
                                                                  shape = 15,
                                                                  color = 'darkblue')



#after plotting the error, we find that the model fails with higher error and consistency
#around lower volumes, this is good for the intended analisys as we are trying to predict
#the products who will have higher sales volume/profit



#we load the final file and proceed to do the predictions with the model ####

new.products<- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 5/Prediction of products by brand/Data Sets/newproductattributes2017.csv')

#prepare the data and predict the results


new.products<- new.products[,c(3,5,7,9,10,18)]


predictctions.new.products<- predict(svm.model,new.products)


new.products$Volume<- predictctions.new.products


#plot the results


ggplot(data = new.products, aes(x=Volume,y = Price))+geom_point()

#we got negative results, we need to check this  try other models, train with the dummy

#trying the results with a KNN


fitcontrol.3 <- trainControl(method = 'boot',
                             number = 1000)


knn.model <- train(Volume ~ .,
                   data = training,
                   method = "knn",
                   trcontrol = fitcontrol.3
)
pr.2<- predict(knn.model,testing)

results<-postResample(pr.2,testing$Volume)



