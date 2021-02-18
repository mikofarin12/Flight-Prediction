#Lecture 2 Group I
#Miko Farin (mikofarin12@gmail.com)
#Jack Krupinski (krupinskij2@gmail.com)
#Wonjae Lee (edd6dd@gmail.com)
#Matt Turk (mturk96@gmail.com)

library(tidyverse)
library(mice)

#loading data
FlightTestNoYNew1 <- read_csv("FlightTestNoYNew1.csv")[,-1]
FlightTrainNew1 <- read_csv("FlightTrainNew1.csv")[,-1]


x <- FlightTrainNew1 %>% filter(Cancelled == "YES")

#seeing if destination has anything to do with cancelation
total_number_canceled <- data.frame(table(x$Destination_airport))
total_number_flights <- data.frame(table(FlightTrainNew1$Destination_airport))
total1 <- merge(total_number_canceled, total_number_flights ,by="Var1")
total1 <- mutate(total1, cancelled_destination_frequency = total1$Freq.x/total1$Freq.y)
total1 <- total1[,c(1,4)]
names(total1)[names(total1) == "Var1"] <- "Destination_airport"

#joining frequencies to flighttraindata
joined_tibble <- left_join(FlightTrainNew1, total1,by = "Destination_airport" )

#seeing if origin airport has anything to do with cancelation
total_number_canceled <- data.frame(table(x$Origin_airport))
total_number_flights <- data.frame(table(FlightTrainNew1$Origin_airport))
total2 <- merge(total_number_canceled, total_number_flights ,by="Var1")
total2 <- mutate(total2, cancelled_origin_frequency = total2$Freq.x/total2$Freq.y)
total2 <- total2[,c(1,4)]
names(total2)[names(total2) == "Var1"] <- "Origin_airport"

#joining frequencies to flighttraindata
joined_tibble2 <- left_join(joined_tibble, total2,by = "Origin_airport" )

#seeing if  airline has anything to do with cancelation
total_number_canceled <- data.frame(table(x$AIRLINE))
total_number_flights <- data.frame(table(FlightTrainNew1$AIRLINE))
total3 <- merge(total_number_canceled, total_number_flights ,by="Var1")
total3 <- mutate(total3, airline_cancelation = total3$Freq.x/total3$Freq.y)
total3 <- total3[,c(1,4)]
names(total3)[names(total3) == "Var1"] <- "AIRLINE"

#joining frequencies to flighttraindata
joined_tibble3 <- left_join(joined_tibble2, total3,by = "AIRLINE")


#seeing if  destination city has anything to do with cancelation
total_number_canceled <- data.frame(table(x$Destination_city))
total_number_flights <- data.frame(table(FlightTrainNew1$Destination_city))
total4 <- merge(total_number_canceled, total_number_flights ,by="Var1")
total4 <- mutate(total4, destination_city_cancellation = total4$Freq.x/total4$Freq.y)
total4 <- total4[,c(1,4)]
names(total4)[names(total4) == "Var1"] <- "Destination_city"

#joining frequencies to flighttraindata
joined_tibble4 <- left_join(joined_tibble3, total4,by =  "Destination_city")

#seeing if  origin city has anything to do with cancelation
total_number_canceled <- data.frame(table(x$Origin_city))
total_number_flights <- data.frame(table(FlightTrainNew1$Origin_city))
total5 <- merge(total_number_canceled, total_number_flights ,by="Var1")
total5 <- mutate(total5, origin_city_cancellation = total5$Freq.x/total5$Freq.y)
total5 <- total5[,c(1,4)]
names(total5)[names(total5) == "Var1"] <- "Origin_city"

#joining frequencies to flighttraindata
joined_tibble5 <- left_join(joined_tibble4, total5,by =  "Origin_city")


#now create cleaned data set and impute missing values
temp <- mice(joined_tibble5[,c("Destination_airport", "cancelled_destination_frequency",
                       "Origin_airport", "cancelled_origin_frequency","Distance", "Org_airport_lat", 
                       "Org_airport_long", "Dest_airport_lat", 
                       "Dest_airport_long", "MONTH", "DAY", "DAY_OF_WEEK",
                       "AIRLINE", "SCHEDULED_TIME", "SCHEDULED_ARRIVAL", 
                       "Pass.Traffic", "Aircraft.Movement","destination_city_cancellation","origin_city_cancellation", 
                       "Cancelled")], method = "rf", maxit=3 , m=3)

#scaling dataset
cleaned_FifaTrainNew1<- complete(temp,1)
cleaned_FifaTrainNew1<- cleaned_FifaTrainNew1 %>% mutate_if(is.numeric, scale)
cleaned_FifaTrainNew1 <- cleaned_FifaTrainNew1[,-c(1,3,13)]
cleaned_FifaTrainNew1$Cancelled <- factor(cleaned_FifaTrainNew1$Cancelled)
head(cleaned_FifaTrainNew1)

#splitting data
n <- dim(cleaned_FifaTrainNew1)[1]
rows <- sample(1:n, 0.8*n)
train_flight <- cleaned_FifaTrainNew1[rows,]
test_flight <- cleaned_FifaTrainNew1[-rows,]


library(leaps)
library(tree)
library(dplyr)
library(randomForest)
library(xgboost)


random_forest_flight <- randomForest(Cancelled~. , data = train_flight, mtry = 10, ntree = 100, importance = TRUE)
random_forest_flight
summary(random_forest_flight)
varImpPlot(random_forest_flight)


#making prediction and confusion matrix
RF_pred1 <- predict(random_forest_flight, newdata = test_flight)
table(RF_pred1,test_flight$Cancelled)

#training accuracy
mean(RF_pred1==test_flight$Cancelled)

#looking at VarImpPlot we only really need 8 of the predictors
testing_preds <- train_flight[,c("DAY", "SCHEDULED_ARRIVAL","MONTH","SCHEDULED_TIME","DAY_OF_WEEK","Distance",
                                 "Org_airport_lat",  "Org_airport_long", "Cancelled")]


random_forest_flight_testing <- randomForest(Cancelled~. , data = testing_preds, mtry = 4, ntree = 100, importance = TRUE)

#making prediction and confusion matrix
RF_pred_test <- predict(random_forest_flight_testing, newdata = test_flight)
table(RF_pred_test,test_flight$Cancelled)

#training accuracy
mean(RF_pred_test==test_flight$Cancelled)


train_flight1 <- train_flight
train_flight1$Cancelled<- ifelse(train_flight1$Cancelled == "YES",1,0)

#boosting
library(gbm)
boost_flight <- gbm(Cancelled~., data = train_flight1,distribution = "bernoulli", n.trees = 500, interaction.depth = 4)
boost_pred <- predict(boost_flight, newdata = test_flight,n.trees = 500, type = "response")
prediction<- boost_pred > 0.5
prediction <- ifelse(prediction == T, "YES", "NO")

#confusion matrix
table(prediction,test_flight$Cancelled)
#training accuracy
mean(prediction==test_flight$Cancelled)

#neural net
library(nnet)
library(NeuralNetTools)
neuralnet<- nnet(Cancelled~. , data = train_flight, size = 20, maxit=1000)
plotnet(neuralnet)
neuralnet_Predictions<- predict(neuralnet,newdata = test_flight)
nnet_prediction<- neuralnet_Predictions > 0.5
nnet_prediction <- ifelse(nnet_prediction == T, "YES", "NO")

#confusion matrix
table(nnet_prediction,test_flight$Cancelled)
#training accuracy
mean(nnet_prediction==test_flight$Cancelled)



#cleaning testing data, same steps as for training data
test_join1 <- left_join(FlightTestNoYNew1, total1,by = "Destination_airport" )
test_join2<- left_join(test_join1, total2,by = "Origin_airport")
test_join3 <- left_join(test_join2, total3,by = "AIRLINE")
test_join4<- left_join(test_join3, total4,by = "Destination_city")
test_join5 <- left_join(test_join4, total5,by = "Origin_city")


#now create cleaned data set and impute missing values
temp1 <-  mice(test_join5[,c("Destination_airport", "cancelled_destination_frequency",
                                 "Origin_airport", "cancelled_origin_frequency","Distance", "Org_airport_lat", 
                                 "Org_airport_long", "Dest_airport_lat", 
                                 "Dest_airport_long", "MONTH", "DAY", "DAY_OF_WEEK",
                                 "AIRLINE", "SCHEDULED_TIME", "SCHEDULED_ARRIVAL", 
                                 "Pass.Traffic", "Aircraft.Movement",
                                 "destination_city_cancellation","origin_city_cancellation")], method = "rf",  maxit=3 , m=3)


cleaned_FifaTestNew1<- complete(temp1,1)
final_testing_data <- cleaned_FifaTestNew1 %>% mutate_if(is.numeric, scale)
final_testing_data <-  final_testing_data[,-c(1,3,13)]



#training our model with the full training data and making final prediction
random_forest_final <- randomForest(Cancelled~., data = cleaned_FifaTrainNew1, mtry = 8, ntree = 100, importance = TRUE)
random_forest_final
summary(random_forest_final)
varImpPlot(random_forest_final)

RF_pred_final <- predict(random_forest_final, newdata = final_testing_data)
RF_pred_final <- data.frame(Ob = 1:nrow(FlightTestNoYNew1),Cancelled= RF_pred_final)

























