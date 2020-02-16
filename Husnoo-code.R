############################################# TASK 1 #######################################################

#read txt file and assign txt file data to matrix
the.data <- as.matrix(read.table("Energy20.txt"))
#generate subset of 300 data
my.data <- the.data[sample(1:671,300),c(1:6)]
#rename columns of matrix to specific variable name to ease understanding
colnames(my.data) <- c("X1", "X2", "X3", "X4", "X5", "Y")
#convert matrix into data frame
my.data.dataframe <- data.frame(my.data)

#Scatter Plots for (X1...X5 vs Y)
plot(my.data.dataframe$X1, my.data.dataframe$Y, 
     xlab = "Temperature in kitchen area (Celsius) - X1", 
     ylab = "Energy Consumption of Appliances (Wh)",
     main = "Relationship between Kitchen Temperature
     and Energy Consumption")

`plot(my.data.dataframe$X2, my.data.dataframe$Y, 
      xlab = "Percentage Humidity in Kitchen Area - X2", 
      ylab = "Energy Consumption of Appliances (Wh)",
      main = "Relationship between Kitchen Humidity
     and Energy Consumption")`

plot(my.data.dataframe$X3, my.data.dataframe$Y, 
     xlab = "Outside Temperature (Celsius) - X3", 
     ylab = "Energy Consumption of Appliances (Wh)",
     main = "Relationship between Outside Temperature
     and Energy Consumption")

plot(my.data.dataframe$X4, my.data.dataframe$Y, 
     xlab = "Percentage Humidity Outdoor - X4", 
     ylab = "Energy Consumption of Appliances (Wh)",
     main = "Relationship between Outdoor Humidity
     and Energy Consumption")

plot(my.data.dataframe$X5, my.data.dataframe$Y, 
     xlab = "Outdoor Visibility (Km) - X5", 
     ylab = "Energy Consumption of Appliances (Wh)",
     main = "Relationship between Outdoor Visibility
     and Energy Consumption")

#Histogram for (X1...Y)

hist(my.data.dataframe$X1, 
     xlab = "Temperature in kitchen area (Celsius)", 
     main = "Distribution of Temperature in kitchen area (X1)")

hist(my.data.dataframe$X2, 
     xlab = "Percentage Humidity in Kitchen Area", 
     main = "Distribution of Percentage Humidity in Kitchen Area (X2)")

hist(my.data.dataframe$X3, 
     xlab = "Outside Temperature (Celsius)", 
     main = "Distribution of Outside Temperature (X3)")

hist(my.data.dataframe$X4, 
     xlab = "Percentage Humidity Outdoor", 
     main = "Distribution of Percentage Humidity Outdoor (X4)")

hist(my.data.dataframe$X5, 
     xlab = "Outdoor Visibility (Km)", 
     main = "Distribution of Outdoor Visibility (X5)")

hist(my.data.dataframe$Y, 
     xlab = "Energy Consumption of Appliances (Wh)", 
     main = "Distribution of Energy Consumption of Appliances (Y)")

#check correlation for all variables
cor(my.data.dataframe$X1, my.data.dataframe$Y, method = "pearson") #value: 0.2955542 -> moderate degree positive correlation
cor(my.data.dataframe$X2, my.data.dataframe$Y, method = "pearson") #value: 0.1368509 -> non-linear correlation
cor(my.data.dataframe$X3, my.data.dataframe$Y, method = "pearson") #value: 0.5140129 -> high degree positive correlation
cor(my.data.dataframe$X4, my.data.dataframe$Y, method = "pearson") #value: 0.04407539 -> non-linear correlation
cor(my.data.dataframe$X5, my.data.dataframe$Y, method = "pearson") #value: 0.2790298 -> low degree positive correlation

#check skewness of data (histograms plotted)
library(e1071)
skewness(my.data.dataframe$X1) #value: 0.2358083 -> bulmer - approximately symmetric -> normally distributed 
skewness(my.data.dataframe$X2) #value: 0.6162908 -> positively skewed
skewness(my.data.dataframe$X3) #value: -0.03072238 -> tends to 0, normally distributed
skewness(my.data.dataframe$X4) #value: -0.3043136 -> negatively skewed
skewness(my.data.dataframe$X5) #value: 0.658326 -> positively skewed
skewness(my.data.dataframe$Y)  #value: 1.822162 -> highly positively skewed

#check the mean and median for all the variables
mean(my.data.dataframe$X1)
median(my.data.dataframe$X1)
mean(my.data.dataframe$X2)
median(my.data.dataframe$X2)
mean(my.data.dataframe$X3)
median(my.data.dataframe$X3)
mean(my.data.dataframe$X4)
median(my.data.dataframe$X4)
mean(my.data.dataframe$X5)
median(my.data.dataframe$X5)
mean(my.data.dataframe$Y) #237.8
median(my.data.dataframe$Y)

############################################# TASK 2 #######################################################

#Chosen Variables X1, X2, X3, X4 & variable of interest Y

#transformation for X1 using linear feature scaling
minimum.value.X1 <- min(my.data.dataframe$X1) #23.377
maximum.value.X1 <- max(my.data.dataframe$X1) #15.064
transformed.valuesX1 <- (my.data.dataframe$X1 - minimum.value.X1)/ (maximum.value.X1 - minimum.value.X1)

#transformation for X2 using polynomial and linear feature scaling transformation
polynomial.transformed.valuesX2 <- (my.data.dataframe$X2) ^(1/100)
minimum.value.X2 <- min(polynomial.transformed.valuesX2) #1.03389
maximum.value.X2 <- max(polynomial.transformed.valuesX2) #1.041182
transformed.valuesX2 <- (polynomial.transformed.valuesX2 - minimum.value.X2)/ (maximum.value.X2 - minimum.value.X2)
skewness(transformed.valuesX2) #0.2388578

#transformation for X3 using linear feature scaling
minimum.value.X3 <- min(my.data.dataframe$X3) #0.044737
maximum.value.X3 <- max(my.data.dataframe$X3) #6.8345
transformed.valuesX3 <- (my.data.dataframe$X3 - minimum.value.X3)/ (maximum.value.X3 - minimum.value.X3)

#transformation for X4 using polynomial transformation and linear feature scaling
polynomial.transformed.valuesX4 <- (my.data.dataframe$X4)^(3)
minimum.value.X4 <- min(polynomial.transformed.valuesX4) #252831.6
maximum.value.X4 <- max(polynomial.transformed.valuesX4) #980508
transformed.valuesX4 <- (polynomial.transformed.valuesX4 - minimum.value.X4)/ (maximum.value.X4 - minimum.value.X4)
skewness(transformed.valuesX4) #0.09379242

#transformation for Y using log transformation and linear feature scaling
log.transformed.valuesY <- log10(my.data.dataframe$Y)
minimum.value.Y <- min(log.transformed.valuesY) #1.812913
maximum.value.Y <- max(log.transformed.valuesY) #2.89487
transformed.valuesY <- (log.transformed.valuesY - minimum.value.Y)/ (maximum.value.Y - minimum.value.Y)
skewness(transformed.valuesY) #0.374739

#put all transformed variables into an array
column.names <- c("Y", "X1", "X2", "X3", "X4")
your.data <- array(c(transformed.valuesY, transformed.valuesX1, transformed.valuesX2, transformed.valuesX3, 
                   transformed.valuesX4), dim = c(300,5),
                   dimnames = list(NULL, column.names))

#write array into text file
write.table(your.data, "Husnoo-transformed.txt")

############################################# TASK 3 #######################################################

#Adding AggWaFit file into R Workspace
source("AggWaFit718.R")

#reading transformed data file created in task 2
task3.data <-  as.matrix(read.table("Husnoo-transformed.txt"))

#Learning Parameters for WAM
#Results generated in Stats1 textfile
fit.QAM(task3.data[,c(1:5)])

#Learning Parameters for WPM (p=0.5)
#Results generated in Stats1 textfile
fit.QAM(task3.data[,c(1:5)], g = PM05, g.inv = invPM05)

#Learning Parameters for WPM (p=2)
#Results generated in Stats1 textfile
fit.QAM(task3.data[,c(1:5)], g = QM, g.inv = invQM)

#Learning Parameters for OWA
#Results generated in Stats1 textfile
fit.OWA(task3.data[,c(1:5)])

#Learning Parameters for Choquet
#Results generated in Stats1 textfile
fit.choquet(task3.data[,c(1:5)])

############################################# TASK 4 #######################################################

#X1=18; X2=44; X3=4; X4=74.8;

#Adding AggWaFit file into R Workspace
source("AggWaFit718.R")

#Applying data transformations to each of the variables same as in in Task 2

#transformation for X1 using linear feature scaling
minimum.value.X1 <- 23.377
maximum.value.X1 <- 15.064
transformed.valueX1 <- (18 - minimum.value.X1)/ (maximum.value.X1 - minimum.value.X1)

#transformation for X2 using polynomial transformation
polynomial.transformed.valueX2 <- (44) ^(1/100)
minimum.value.X2.polynomial <- 1.03389
maximum.value.X2.polynomial <- 1.041182
transformed.valueX2 <- (polynomial.transformed.valueX2 - minimum.value.X2.polynomial)/ (maximum.value.X2.polynomial - minimum.value.X2.polynomial)

#transformation for X3 using linear feature scaling
minimum.value.X3 <- 0.044737
maximum.value.X3 <- 6.8345
transformed.valueX3 <- (4 - minimum.value.X3)/ (maximum.value.X3 - minimum.value.X3)

#transformation for X4 using polynomial transformation
polynomial.transformed.valueX4 <- (74.8)^(3)
minimum.value.X4.polynomial <- 252831.6
maximum.value.X4.polynomial <- 980508
transformed.valueX4 <- (polynomial.transformed.valueX4 - minimum.value.X4.polynomial)/ (maximum.value.X4.polynomial - minimum.value.X4.polynomial)

#converting transformed values into a vector
transformed.values.for.prediction <- c(transformed.valueX1, transformed.valueX2,
                                       transformed.valueX3, transformed.valueX4)

#applying to best fitting model, in this case WPM with p=2
WPM.weights <- c(0.270403429531132, 0.226494725132403, 0.503101845336465, 0)
predicted.transformed.value.Y <- QAM(transformed.values.for.prediction, WPM.weights)
predicted.transformed.value.Y

#reversing the tranformations to obtain the predicted value of Y
minimum.value.Y.log <- 1.812913
maximum.value.Y.log <- 2.89487
reversing.linear.scaling = (predicted.transformed.value.Y * (maximum.value.Y.log-minimum.value.Y.log)) + minimum.value.Y.log
reversing.log.transformation <- 10^reversing.linear.scaling
predicted.value.Y <- reversing.log.transformation
predicted.value.Y

############################################# TASK 5 #######################################################

task5.data <-  as.matrix(read.table("Husnoo-transformed.txt"))

#convert data from matrix to dataframe
task5.data.dataframe <- as.data.frame(task5.data)
mean(task5.data.dataframe$Y)
#building linear regression model
linear.regression.model <- lm(Y ~ X1 + X2 + X3 + X4, data = task5.data.dataframe)

#viewing linear regression model summary
summary(linear.regression.model)

#Testing linear regression model
predicted.300.values.using.linear.regression <- predict(linear.regression.model, task5.data.dataframe[,2:5])

plot(task5.data.dataframe$Y, predicted.300.values.using.linear.regression, 
     ylab = "Predicted Values of Y without reversing Log and Linear Feature Scaling Transformation", 
     xlab = "True Values of Y after Log and Linear Feature Scaling Tranformation.",
     main = "Relationship between True Value and Predicted Value of Y")

predicted.Value.Y.Without.Transformation <- 0.0003248 + 0.2695620*transformed.valueX1 + (-0.0800171)* transformed.valueX2 + (0.6330935) * transformed.valueX3 + 0.2741480 * transformed.valueX4 
predicted.Value.Y.Without.Transformation

#reversing the tranformations to obtain the predicted value of Y
minimum.value.Y.log <- 1.812913
maximum.value.Y.log <- 2.89487
reversing.linear.scaling = (predicted.Value.Y.Without.Transformation * (maximum.value.Y.log-minimum.value.Y.log)) + minimum.value.Y.log
reversing.log.transformation <- 10^reversing.linear.scaling
predicted.value.Y <- reversing.log.transformation
predicted.value.Y
