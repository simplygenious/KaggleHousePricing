options(scipen=999) # To ignore exponential notation
# Import libraries
#install.packages("tidyverse")
#install.packages("plyr")
#install.packages("moments")
#install.packeges("car")
#install.packages("scales")
#install.packages("reshape")
#install.packages("ggcorrplot")
library(ggcorrplot)
library(reshape)
library(tidyverse)
library(plyr)
#library(lubridate) # for integer conversion to year
library(moments) # for skewness and kurtosis
#library(qqplotr) # For quantile plots
#library(scales) # For Date factor
library(car) # For quantile plots
library(ggcorrplot)
# Update: Code will load the package only if the package is not loaded
# Explore the structure of the data:
housing <- read.csv("H://personal//Kaggle_House_Price//train.csv", header = T, sep = ",")

## Write control points to check whether the code is correct or not
## Understand the data: Data Dimension, Data structure, class and type of the data (categorical, numerical and others)  
dim(housing)
head(housing)
tail(housing)
str(housing)
colnames(housing)
table(sapply(housing, class))
list_type_housing <- sapply(housing, class)
data_type_housing <- ldply(list_type_housing, data.frame)
names(data_type_housing) <- c("variables", "type")
print(data_type_housing)

# Data type conversion (i.e character to factor, others)
## OverallQual and OverallCond should be factors
## MSSubClass should be a factor
housing[, c("MSSubClass", "OverallQual", "OverallCond")] = 
  lapply(housing[, c("MSSubClass", "OverallQual", "OverallCond")], as.factor)

housing <- housing %>%
  mutate(., MoSoldChar = as.factor(month.abb[housing[, "MoSold"]]))

# Missing value analysis

missing_data_table <- housing %>% 
  summarize_all(funs(round(sum(is.na(.))*100 / length(.)))) %>%
  t() %>%
  as.data.frame(row.names = NULL)

missing_data_table <- setNames(cbind(rownames(missing_data_table), missing_data_table, row.names = NULL), 
                               c("Variables", "Percentage"))

missing_data_table <- missing_data_table %>%
  arrange(desc(Percentage)) %>%
  filter(Percentage!=0)

print(missing_data_table)

# Remove variables PoolQC, MiscFeature, Alley, Fence, Fireplace Quality from analysis
housing_original <- housing
housing <- housing %>%
  select(-c(PoolQC, MiscFeature, Alley, Fence, FireplaceQu, LotFrontage))

dim(housing)

housing <- housing %>%
  filter(!is.na(MasVnrType)) %>%
  filter(!is.na(MasVnrArea)) %>%
  filter(!is.na(BsmtFinType1)) %>%
  filter(!is.na(BsmtFinType2)) %>%
  filter(!is.na(BsmtExposure)) %>%
  filter(!is.na(BsmtCond)) %>%
  filter(!is.na(BsmtQual)) %>%
  filter(!is.na(GarageType)) %>%
  filter(!is.na(GarageYrBlt)) %>%
  filter(!is.na(GarageFinish)) %>%
  filter(!is.na(GarageQual)) %>%
  filter(!is.na(GarageCond))

dim(housing)  
# Target (predictor) variable analysis
## Data distribution, skewness, kurtosis
ggplot(housing, aes(x=SalePrice)) + 
  geom_density(color = "dark blue", alpha=.1) + 
  geom_vline(aes(xintercept = mean(SalePrice)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(SalePrice)), color = "blue", linetype = 4, size = 1) +  
  geom_histogram(aes(y=..density..),colour = "white", fill = "cornflowerblue", alpha=0.5)

## Update and Note: Wanted to add normal density plot also to here, so add later.

# Skewness and kurtosis
sprintf("Skewness:", print(skewness(housing$SalePrice)))
sprintf("Kurtosis:", print(kurtosis(housing$SalePrice)))

## Check for outliers (in case of other numerical variables as well)
OutVals = boxplot(housing$SalePrice)$out
which(housing$SalePrice %in% OutVals)
boxplot.stats(housing$SalePrice)$out

housing %>%
  filter(SalePrice >= 700000 | SalePrice <= 50000) %>%
  print
# Not seems to be outlier when compared to other observations, as overall quality and other measures of variables are better than others. 

# Create categorical variable "HasPool" with values y or n 
housing <- housing %>%
  mutate(., HasPool = ifelse(PoolArea > 0, "Y", "N")) 

# Analysis for other numerical variables (density plots)

housing %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

housing %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

### Quantile plots for all the numerical variables

par(mfrow=c(4,2))  

numerical <- dplyr::select_if(housing, is.numeric)

par(mfrow=c(2, 2))
for(i in 1:ncol(numerical))
{
  qqPlot(numerical[, i], lwd=1, envelope = FALSE, ylab = colnames(numerical)[i], col = alpha("black", 0.3))
}

# Analysis for categorical variables (barplots)
housing %>%
  keep(is.factor) %>% 
  select(.,1:10) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

housing %>%
  keep(is.factor) %>% 
  select(.,10:20) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

housing %>%
  keep(is.factor) %>% 
  select(.,20:30) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

housing %>%
  keep(is.factor) %>% 
  select(.,30:40) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

## Scatterplot between numeric variables and SalePrice
housing %>%
  keep(is.numeric) %>%  
  select(1:17, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice, colour=SalePrice)) + 
  facet_wrap(~variable, scales = "free_x")

housing %>%
  keep(is.numeric) %>%  
  select(18:34, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice, colour=SalePrice)) + 
  facet_wrap(~variable, scales = "free_x")

# Correlation analysis between numerical variables
##### Network plot
#  install.packages("corrr")
#library(corrr)

detach(package:reshape)
p <- ggcorrplot(cor(numerical), 
                p.mat = cor_pmat(numerical), 
                hc.order=TRUE)

p + theme(axis.text.x = element_text(size=10, angle=90), 
          axis.text.y = element_text(size=10))

## Scatterplot between target and numerical
## Boxplot or any other plot for categorical vs target
## Create jitterplot for boxplot

library(reshape)
vartype <- sapply(housing, class)
categorical <- housing[,c(names(vartype[vartype=="factor"]), "SalePrice")]

categorical %>%
  select(1:7, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") 

categorical %>%
  select(8:14, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") 

categorical %>%
  select(15:21, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") 

categorical %>%
  select(22:28, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") 

categorical %>%
  select(29:35, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") 

# Scatterplot for categorical variable
categorical %>%
  select(1:6, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity") +
  facet_wrap(~variable, scales = "free_x")

categorical %>%
  select(7:14, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity") +
  facet_wrap(~variable, scales = "free_x")

categorical %>%
  select(15:21, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity") +
  facet_wrap(~variable, scales = "free_x")

categorical %>%
  select(22:28, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity") +
  facet_wrap(~variable, scales = "free_x")

categorical %>%
  select(29:35, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity") +
  facet_wrap(~variable, scales = "free_x")
