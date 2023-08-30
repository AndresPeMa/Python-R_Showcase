#Load data
data = read.csv("E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/modified_data3.csv")

#Get rid of our Y variable F9
data = subset(data, select = -c(lasting_change))


library(psych)
#get stats of variables
desc_data = describe(data)

#round values
rounded_data = round(desc_data, 4)  #4 decimal places is the optimal amount to show the most without having any e

#rename n to obs
names(rounded_data)[names(rounded_data) == "n"] = "obs"


#drop all but obs, mean, and st dev.
mydata = subset(rounded_data, select = -c(vars, median, trimmed, mad, min, max, range, skew, kurtosis, se))

#Export it as csv
write.csv(mydata, file = "E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/descstats.csv", row.names = TRUE)


library(openxlsx)
#export it as xls
write.xlsx(mydata, file = "E:/CalPoly SLO/Spring Quarter 2023/World Bank Project/Working Directory/descriptivestats2.xlsx", sheetName = "Descriptivestats", rowNames = TRUE)
