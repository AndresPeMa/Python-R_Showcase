#Load packages 
library(haven)
library(dplyr)


#Load data
data2 = read_dta("E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/CAM_AMRU_rice_endline_raw_22.dta")

data =read_dta("E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/CAM_AMRU_rice_endline_renamed_22_2.dta")


#Bringing F9 variable (Our outcome variable) from data2 to data
data$F9 = data2$F9


#Getting rid of all columns with no values at all
data = data[, colSums(!is.na(data)) > 0]

#Get rid of useless variables
data = subset(data, select = -c(Date, SbjNum, DATASET, endline, name, grow_rice, currency)) #all grew rice and all have same currency


#Making non numeric variables numeric
data = mutate_if(data, is.character, as.numeric)  #NAs were created
data = mutate_if(data, is.factor, as.numeric)
data = mutate_if(data, is.logical, as.numeric)
data = mutate_if(data, is.integer, as.numeric)

#Getting rid of Nas
data[is.na(data)] = 0

#exporting modified data to use in other software
write.csv(data, file = "E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/modified_data2.csv", row.names = FALSE)



#OLS regression
which(names(data)=="F9")  # whatever it returns means that the string "F9" exactly matches the variable name of your dependent variable in your dataset.

#Create a formula object with F9 as the dependent variable and all other variables as predictors
formula = as.formula(paste("F9 ~", paste(names(data)[-which(names(data)=="F9")], collapse=" + ")))

# Run the OLS regression
mod1 = lm(formula, data = data)
summary(mod1)
#The ones that say NAs is because they are perfectly correlated with another variable so R gets rid of them 



#Random forest
library(randomForest)
library(rsq)

#Replace NAs with 0  (already done before but it doesnt hurt)
data[is.na(data)] = 0

#Split data into training and test sets  ????? does it mean treatment and control cause if it does we would use the variables I created at the start
set.seed(123)
train_index = sample(nrow(data), 0.8 * nrow(data))
train_data = data[train_index, ]
test_data = data[-train_index, ]

#Fit random forest model
mod2 = randomForest(F9 ~ ., data = train_data, ntree = 500, importance = TRUE)
summary(mod2)
print(mod2)  #print and summary show different stuff
rsq(mod2$F9, mod2$predicted)
mean(mod2$rsq)


#Make predictions on test data
predictions = predict(mod2, test_data)
predictions

#This will evaluate model performance
error = mean((predictions - test_data$F9)^2)
importance = importance(mod2)
importance

#This will assess the accuracy of the predictions
accuracy = mean(predictions == test_data$F9)
print(paste0("Accuracy: ", round(accuracy, 3)))


#This will plot the "variable importance"
varImpPlot(mod2)  #This will produce a plot with bars indicating the importance of each variable in the model.


#This will plot partial dependence for variable "x" "y" or "z"
partialPlot(mod2, train_data, "X")
partialPlot(mod2, train_data, "C4_2")  #This is a test with variable C4_2
#The partial dependence plot shows how the predicted response varies as a function of a single predictor variable, while holding all other predictors constant
#This will produce a plot with a curve showing how the predicted response changes as the value of X1 changes.



# Plot actual vs. predicted values
plot(test_data$F9, predictions, xlab = "Actual", ylab = "Predicted")
abline(a = 0, b = 1, col = "red")
#This will produce a scatterplot with the actual values of F9 on the x-axis and the predicted values on the y-axis. The red line indicates perfect prediction (where the actual and predicted values are the same).
#I looked at internet plots and this plot looks so off. I am not sure if I did it wrong or 
#The problem with the graph is that our y variable is either yes and no, so some point show yes and others no
#maybe use probit and logit









#For modified data 3
data2 = read_dta("E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/cleaned_farming (1).dta")
 
data =read_dta("E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/CAM_AMRU_rice_endline_raw_22.dta")


#Bringing F9 from data to data2
data2$F9 = data$F9

#renaming F9 to "lasting change"
names(data2)[names(data2) == "F9"] = "lasting_change"

#exporting to csv
write.csv(data2, file = "E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/modified_data4.csv", row.names = FALSE)

