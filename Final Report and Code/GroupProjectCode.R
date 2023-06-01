install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)
install.packages("leaps")
library(leaps)
install.packages("caTools")
library(caTools)
install.packages("Metrics")
library(Metrics)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)


#Uploading our dataset.
df = read.csv(file.choose(), header = T) #Select the data-set from the Windows Explorer pop-up


#'************2.1 Data Understanding: Descriptive Analytics*********************

#View data-frame
View(df)


#Print dimension information of dataset (Rows & Columns)
cat("Rows: ", dim(df)[1], "rows and", dim(df)[2], "columns")


#View structure of df
str(df)

#Count data-types
table(sapply(df, class))

#Dependent Variable (SalePrice) summary
summary(df$SalePrice)

#Visualize dependent variable (SalePrice) --> boxplot
#1 Data Visualization: Sale Price Box Plot 
boxplot(df['SalePrice'],                 
        col = "lightgray",              
        xlab = "SalePrice",             
        main = "Boxplot for SalePrice",  
        horizontal = TRUE,               
        outcol = "red") 

#Checking the quality of our data-set
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("yellow", "grey"), name="Missing\n(0=Yes, 1=No)") + 
  theme_light() + ylab("") + xlab("") + ggtitle(title)
}

#Visualizing "na" columns
plot_Missing(df[,colSums(is.na(df)) > 0,])


#Checking how many columns have values which are null. NOTE: It is 0.
cbind(
  lapply(
    lapply(df, is.null)
    , sum)
)



#'************2.1 Data Understanding: Selection of Independent Variables********
#We will be choosing our dependent variables through a combination of these:

  #1 Correlation Matrix (Only works with numeric variables)
  #2 Subset Regression   (Only works with numeric variables)
  #3 Logic


#1 Correlation Matrix 

#Getting all Numeric Variables
df_NumericVariables <- names(df)[which(sapply(df, is.numeric))]
df_num <- df[df_NumericVariables]


correlations <- cor(na.omit(df_num[,-1]))

#Sorting matrix by SalePrice (dependent variable)
corr_with_SalePrice <- as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))


#Get only those variables which has correlation bigger than equal to absolute (0.4)
corr.idx <- names(which(apply(corr_with_SalePrice,1, function(x) (abs(x) > 0.4))))

#Correlation Plot.
corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'full', method='shade', 
         addCoef.col = 'black', tl.cex = .8,cl.cex = .8, number.cex=.6)


#2 Subnet Regression
model_SubsetRegression1 = regsubsets(SalePrice ~ OverallQual + GrLivArea + GarageCars +
                                  GarageArea + TotalBsmtSF + X1stFlrSF +
                                  FullBath + TotRmsAbvGrd + YearBuilt +
                                  YearRemodAdd + GarageYrBlt + MasVnrArea + Fireplaces, 
                                  nbest = 2,data = df_num)

#Visualizing model_SubsetRegression1 model based on adjusted R-Squared
plot(model_SubsetRegression1, scale = "adjr2")



#SubsetRegression Model 2
model_SubsetRegression2 = regsubsets(SalePrice ~ OverallQual + GrLivArea+ GarageCars+ 
                                    GarageArea + TotalBsmtSF + X1stFlrSF + FullBath +
                                    TotRmsAbvGrd + YearBuilt + YearRemodAdd + GarageYrBlt +
                                    MasVnrArea + Fireplaces, nbest = 2,data = df_num)

#Visualizing model_SubsetRegression2 model based on adjusted R-Squared
plot(model_SubsetRegression2,scale="adjr2")


#3 Logically Determine Variables.
    #We logically chose the below attributes as important in predicting house Sale Price:
      # Neighborhood, MSZoning and BldgType.



#'****************2.2 Data Understanding: Data Quality Assessment***************

#Data Reduction: Horizontal Reduction (Only keeping relevant variables which will be in our model)
df_ImportantVariables = data.frame(df[c("SalePrice", "OverallQual", "GrLivArea", 
                                        "TotalBsmtSF", "YearBuilt", "Fireplaces",
                                        "YearRemodAdd", "Neighborhood","MSZoning",
                                        "BldgType")])



#Check for NA values by returning column names and sum of NA values in them. NOTE: There is 0
cbind(
  lapply(
    lapply(df_ImportantVariables, is.na)
    , sum)
)



#'****************2.2 Data Understanding: Data Exploration**********************
options(scipen = 999)


#1a: Data Visualization: Sale Price Box Plot: 
boxplot(df['SalePrice'],                 
        col = "lightgray",              
        xlab = "SalePrice",             
        main = "Boxplot for SalePrice",  
        horizontal = TRUE,               
        outcol = "red") 

#Note: I made several other box-plots by removing outliers (above 350k), the new outliers then were 330k, then I removed this,
#and the new outliers were 300k, then I removed these and the new outliers showed above 290k, when I removed these, no other outliers
#were detected. Hence, to have "clean-code" I am only visualizing houses with SalePrice <= $290,000.

#1b: SalePrice Box Plot Visualization of Sale Price <= $290,000.
df_SalePriceBelow290 = df_ImportantVariables[df_ImportantVariables$SalePrice <= 290000,] #data-frame which has houses below $290,000

boxplot(df_SalePriceBelow290['SalePrice'],                 
        col = "lightgray",              
        xlab = "SalePrice",             
        main = "Boxplot for SalePrice",  
        horizontal = TRUE,               
        outcol = "red") 


#1c: SalePrice Visualization --> Result shows right-skewness.
qplot(data=df_ImportantVariables, bins = 60, x=SalePrice, col = "darkmagenta",
      main = "Sale Price of Houses With Outliers",
      xlab = "Sale Price", ylab = "Frequency of Sale Price" )+
  geom_vline(xintercept = mean(df_ImportantVariables$SalePrice),color = "blue", size=0.6)


#1d: SalePrice Visualization of less than equal to $290,000 (No Outliers dataset).
qplot(data=df_SalePriceBelow290, bins = 60, x=SalePrice, col = "darkmagenta",
      main = "Sale Price of Houses Without Outliers (less than equal $290,000)",
      xlab = "Sale Price", ylab = "Frequency of Sale Price" )+
  geom_vline(xintercept = mean(df_SalePriceBelow290$SalePrice),color = "blue", size=0.6)


#1e: LogSalePrice Visualization: Box-plot showing "outliers" from BOTH left and right side.

#Log Transforming Sale Price
df_ImportantVariables['LogSalePrice'] = log(df_ImportantVariables$SalePrice)

boxplot(df_ImportantVariables['LogSalePrice'],                 
        col = "lightgray",              
        xlab = "LogSalePrice",             
        main = "Boxplot for LogSalePrice",  
        horizontal = TRUE,               
        outcol = "red") 


#1d: Visualizing to see if there is a normal distribution (RESULT: Yes! Normal Distribution)
qplot(data=df_ImportantVariables, bins = 60, x=LogSalePrice, col = "darkmagenta",
      main = "LogSalePrice of Houses",
      xlab = "LogSalePrice", ylab = "Frequency of LogSalePrice" )+
  geom_vline(xintercept = mean(df_ImportantVariables$LogSalePrice),color = "blue", size=0.6)







#2A: OverallQual By Count   #Don't know why the x-axis scale isn't displaying properly.
hist(df_ImportantVariables[['OverallQual']],
     main = "Histogram of Overall Quality",
     xlab = "OverallQual",
     ylab = "Number of Houses",
     col = "darkmagenta",
     labels = TRUE,
     breaks = seq(min(df_ImportantVariables$OverallQual), max(df_ImportantVariables$OverallQual), length.out = 11))

#2B: OverallQual By Sale Price
plot( df_ImportantVariables$SalePrice, df_ImportantVariables$OverallQual, main = "Sale Price by OverallQual",
     xlab = "Sale Price", ylab = "OverallQual", pch = 19)










#3A: GrLivArea By Count
hist(df_ImportantVariables[['GrLivArea']],
     main = "Histogram of Above Ground Living Area (GrLivArea)",
     xlab = "GrLivArea In Square Feet",
     ylab = "Number of Houses",
     col = "darkmagenta",
     breaks = 50
     )

#3B: GrLivArea By Sale Price
plot( df_ImportantVariables$SalePrice, df_ImportantVariables$GrLivArea, main = "Sale Price by Above Ground Living Area (GrLivArea)",
      xlab = "Sale Price", ylab = "GrLivArea In Square Feet", pch = 19)












#4A: TotalBsmtSF By Count
hist(df_ImportantVariables[['TotalBsmtSF']],
     main = "Histogram of Total Square Feet of Basement Area (TotalBsmtSF)",
     xlab = "TotalBsmtSF In Square Feet",
     ylab = "Number of Houses",
     col = "darkmagenta",
     breaks = 50
)

#4B: TotalBsmt By Sale Price
plot( df_ImportantVariables$SalePrice, df_ImportantVariables$TotalBsmtSF, main = "Sale Price by Total Square Feet of Basement Area (TotalBsmtSF)",
      xlab = "Sale Price", ylab = "TotalBsmtSF In Square Feet", pch = 19)












#5A: YearBuilt By Count
hist(df_ImportantVariables[['YearBuilt']],
     main = "Histogram of Year Built",
     xlab = "Year Built",
     ylab = "Number of Houses",
     col = "darkmagenta",
     breaks = 60
)

#5B: TotalBsmt By Sale Price
plot( df_ImportantVariables$YearBuilt, df_ImportantVariables$SalePrice, main = "Sale Price by Year Built",
      xlab = "Year Built", ylab = "Sale Price", pch = 19)











#6A: Fireplaces By Count
hist(df_ImportantVariables[['Fireplaces']],
     main = "Histogram of Fireplaces",
     xlab = "Number of Fireplaces",
     ylab = "Number of Houses",
     col = "darkmagenta",
     labels = TRUE)

#6B: Fireplaces By Sale Price
plot( df_ImportantVariables$Fireplaces, df_ImportantVariables$SalePrice, main = "Sale Price by Number of Fireplaces",
      xlab = "Number of Fireplaces", ylab = "Sale Price", pch = 19)









#7A: YearRemodAdd By Count
hist(df_ImportantVariables[['YearRemodAdd']],
     main = "Histogram of YearRemodAdd",
     xlab = "Remodeling Year",
     ylab = "Number of Houses",
     col = "darkmagenta",
     labels = TRUE)

#7B: YearRemodAdd By Sale Price
plot( df_ImportantVariables$YearRemodAdd, df_ImportantVariables$SalePrice, main = "Sale Price by Remodeling Year (YearRemodAdd)",
      xlab = "Year of Remodeling", ylab = "Sale Price", pch = 19)









#8: Neighborhood By Median Sale Price.
df_ImportantVariables %>%
  group_by(Neighborhood) %>%
  summarise(medianSalePrice = median(SalePrice)) %>%
  mutate(Neighborhood = fct_reorder(Neighborhood, medianSalePrice)) %>% 
  ggplot() + geom_col(aes(medianSalePrice, Neighborhood), 
                      col = "orange")










#9a: Median Sale Price By MSZoning
df_ImportantVariables %>%
  group_by(MSZoning) %>%
  summarise(medianSalePrice = median(SalePrice)) %>%
  mutate(MSZoning = fct_reorder(MSZoning, medianSalePrice)) %>% 
  ggplot() + geom_col(aes(medianSalePrice, MSZoning), 
                      col = "orange")


#9b: Sale Price By MSZoning (Boxplot)
boxplot(SalePrice ~ MSZoning, data = df_ImportantVariables, main = "Sale Price by Zoning (MSZoning)",
        xlab = "MSZoning", ylab = "SalePrice", pch = 19)





#10a: BldgType By Median Sale Price.
df_ImportantVariables %>%
  group_by(BldgType) %>%
  summarise(medianSalePrice = median(SalePrice)) %>%
  mutate(BldgType = fct_reorder(BldgType, medianSalePrice)) %>% 
  ggplot() + geom_col(aes(medianSalePrice, BldgType), 
                      col = "orange")


#10b: Sale Price By BldgType (Boxplot)
boxplot(SalePrice ~ BldgType, data = df_ImportantVariables, main = "Sale Price by Building Type (BldgType)",
        xlab = "BldgType", ylab = "SalePrice", pch = 19)


#'****************3.0 Data Transformation***************************************

#'*1.0 Changing CATEGORICAL variables to Factor data-type*
df_ImportantVariables$OverallQual = factor(df_ImportantVariables$OverallQual)
df_ImportantVariables$Neighborhood = factor(df_ImportantVariables$Neighborhood)
df_ImportantVariables$MSZoning = factor(df_ImportantVariables$MSZoning)
df_ImportantVariables$BldgType = factor(df_ImportantVariables$BldgType)


#'*2.0 Self-setting reference variables*
df_ImportantVariables$Neighborhood = relevel(df_ImportantVariables$Neighborhood, ref = "MeadowV")
df_ImportantVariables$MSZoning = relevel(df_ImportantVariables$MSZoning, ref = "C (all)")
df_ImportantVariables$BldgType = relevel(df_ImportantVariables$BldgType, ref = "2fmCon")


#'*3.0 Solving for True Outliers (Expensive Houses).*
#From the above visualization, we can see that there are several outliers  ~ > 290k.
#However, these are "true outliers", that is, these are just expensive houses. What we can do is:

# STEP 1: Filter the data-sets into two data-frames:

  #Storing outliers in a data-frame called df_SalePriceAbove290 (> 290,000)
  df_SalePriceAbove290 = df_ImportantVariables[df_ImportantVariables$SalePrice >= 290000,]


  #Storing non-outliers in a data-frame called df_SalePriceBelow290 (<= 290,000)
  df_SalePriceBelow290 = df_ImportantVariables[df_ImportantVariables$SalePrice < 290000,]
  

  

# STEP 2: Do an 80:20 split on each of these data-frames:
  
    #Doing an 80:20 split on df_SalePriceAbove290:
    set.seed(1) #reproducible
    df_SalePriceAbove290_sample = sample.split(df_SalePriceAbove290$SalePrice, SplitRatio = 0.80)
    train_SalePriceAbove290 = subset(df_SalePriceAbove290, df_SalePriceAbove290_sample == TRUE) #For Training Merge
    test_SalePriceAbove290 = subset(df_SalePriceAbove290, df_SalePriceAbove290_sample == FALSE) #For Testing Merge
    
    
    #Doing an 80:20 split on df_SalePriceBelow290:
    df_SalePriceBelow290_sample = sample.split(df_SalePriceBelow290$SalePrice, SplitRatio = 0.80)
    train_SalePriceBelow290 = subset(df_SalePriceBelow290, df_SalePriceBelow290_sample == TRUE) #For Training Merge
    test_SalePriceBelow290 = subset(df_SalePriceBelow290, df_SalePriceBelow290_sample == FALSE) #For Testing Merge
    
    
#STEP 3: Merge the train_SalePriceAbove290 and train_SalePriceBelow290 to make a training dataset.
    train_dataset = rbind(train_SalePriceAbove290, train_SalePriceBelow290)
    

#STEP 4: Merge the test_SalePriceAbove290 and test_SalePriceBelow290 to make a testing dataset.
    test_dataset = rbind(test_SalePriceAbove290, test_SalePriceBelow290)
    
    
    #NOTE: Make sure you test the robustness of your model with each of these testing dataset as well. For example
    #what's the RMSE on test_SalePriceAbove290 only, and what's the RMSE on test_SalePriceBelow290 only.




#'****************4.0 Data Mining***********************************************



#Building Linear Model on Training Data-set (train_dataset)
linear_Model = lm(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF +
                    YearBuilt + Fireplaces + YearRemodAdd + MSZoning + Neighborhood +
                    BldgType  , data=train_dataset)


#Getting Summary of Linear Model Summary (Adjusted R-Squared = 0.8291 (82.91%)
summary(linear_Model)

#'*RESIDUALS!*
plot(linear_Model)


#TESTING:

#Testing 0: Testing on train_dataset.
predictTest0 = predict(linear_Model, train_dataset)
summary(predictTest0)

rmse(train_dataset$SalePrice, predictTest0) #RMSE in terms of SalePrice



#-------------------------------------------------------------------------------

#Testing 1: Testing on test_dataset
predictTest1 = predict(linear_Model, test_dataset)
summary(predictTest1)

rmse(test_dataset$SalePrice, predictTest1) #RMSE in terms of SalePrice




#-------------------------------------------------------------------------------

#Testing 2: Testing on test_SalePriceAbove290. Predicting house Sale Price of houses which are above $290,000 (outliers).
predictTest2 = predict(linear_Model, test_SalePriceAbove290)
summary(predictTest2)

rmse(test_SalePriceAbove290$SalePrice, predictTest2) #RMSE in terms of SalePrice



#-------------------------------------------------------------------------------

#Testing 3: Testing on test_SalePriceBelow290. Predicting house Sale Price of houses which are below $290,000 (non-outliers).
predictTest3 = predict(linear_Model, test_SalePriceBelow290)
summary(predictTest3)

rmse(test_SalePriceBelow290$SalePrice, predictTest3) #RMSE in terms of SalePrice




