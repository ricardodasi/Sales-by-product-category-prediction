#Load the libraries ####

pacman::p_load(caret,mlbench,plotly,ggplot2)



#load the first data set ####

existing.product.attributes <- read.csv(
  'c://Users/riqui/Desktop/Ubiqum course/Project 5/Prediction of products by brand/Data Sets/existingproductattributes2017.csv'
)


#Dummify the data ####

newDataFrame <- dummyVars(" ~ .", data = existing.product.attributes)

ready.data <- data.frame(predict(newDataFrame, newdata = existing.product.attributes))




#me quedo con las dummy y quito las otras que no sirven ####

ready.data<- ready.data[,c(1,2,3,4,5,6,7,8,9,10,11,12,14,16,18,20,21,29)]



#create data partition and set seed ####

set.seed(123)

intraining <- createDataPartition(ready.data$Volume,
                                  p=0.70,
                                  list = FALSE) 

training <- ready.data[intraining,]
testing <- ready.data[-intraining,]



#preprocessing ####

  #Checking Volume
ggplot(training, aes(x=Volume)) + geom_histogram()

  #take out volume outliers

training <-training[-which(training$Volume>6000),] 

  #checking price star reviews

ggplot(training, aes(x=Price)) + geom_histogram()

  #Removing price outliers
  
training <-training[-which(training$Price>1000),] 



#for loop for testing ####

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







#Training knn model manually to create predictions.  ####

  #We decided to switch to a KNN model because it will not predict negative 
  #values and the SVM was doing it. KNN cant predict highr of lower thant
  #the min max in the data

fitcontrol <- trainControl(method = 'boot',number = 1000)

knn.model<- train(Volume ~.,
                  data = training,
                  method = 'knn',
                  trcontrol = fitcontrol)

testing.predictions <-  predict(knn.model,testing)

testing.results <- postResample(testing.predictions,testing$Volume)

#Plotting the error again

testing$predictions<- testing.predictions

testing$relative.error<- abs(testing$Volume-testing$predictions)/testing$Volume

ggplot(testing,aes(x=Volume,y=testing$relative.error)) + geom_point()

testing.error<-abs(testing$Volume-testing$predictions)

ggplot(testing,aes(x=volume,y=error))+ geom_point()

  #exporting this to a CSV to see if the errors have anything to do with the category
  #errors seem to be heavier on pc and printer making half of the top errors
  #this might be explained as this are lower volume type of products, this is 
  #consistent with the plotting as the model seems o have higher failure on low
  #volume. we should not predict profitability given this, but we should be able to 
  #predict high volume products. median error between top 5 predicted is 38%.
  #way lower than the average on the rest of the products. these are also between
  #the categories of software, accesory and one warranty, leading us to believe that
  #there is a relation between the sales amount nature of the category and the ability
  #to predict it from the model with the current data set. we believe that we should
  #train to different models, maybe a split between volumes or a better split by low
  #amount of sales type of category and high amount

  


#Result prediction for the new products ####

new.products<- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 5/Prediction of products by brand/Data Sets/newproductattributes2017.csv')

  #dumify the variables

newDataFrame.2 <- dummyVars(" ~ .", data = new.products)

new.product.dummy <- data.frame(predict(newDataFrame.2, newdata = new.products))

final.predictions <- predict(knn.model,new.product.dummy)

new.product.dummy$Volume<- final.predictions

  #exporting to a csv.





