options(scipen=999) # To ignore exponential notation

# Import libraries
library(reshape)
library(tidyverse)
library(plyr)
library(moments) # for skewness and kurtosis
library(car) # For quantile plots
library(ggcorrplot)

# Explore the structure of the data:
housing <- read.csv("H://personal//Kaggle_House_Price//train.csv", header = T, sep = ",")

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
## Creating new variable for month, with abbreviations of months instead of numbers 
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

# Creating a table containing variable names with non zero missing values
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
  geom_vline(aes(xintercept = median(SalePrice)), color = "dark blue", linetype = 4, size = 1) +  
  geom_histogram(aes(y=..density..),colour = "white", fill = "cornflowerblue", alpha=0.5) +
  ggtitle("Density plot of SalePrice") +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

# Skewness and kurtosis
sprintf("Skewness:", print(skewness(housing$SalePrice)))
sprintf("Kurtosis:", print(kurtosis(housing$SalePrice)))

## Check for outliers (in case of other numerical variables as well)
OutVals = boxplot(housing$SalePrice)$out
which(housing$SalePrice %in% OutVals)
boxplot.stats(housing$SalePrice)$out
title("Boxplot to Check Outliers", ylab = "SalePrice")

housing %>%
  filter(SalePrice >= 700000 | SalePrice <= 50000) %>%
  print
# Not seems to be outlier when compared to other observations, as overall quality and other measures of variables are better than others. 

# Create categorical variable "HasPool" with values y or n 
housing <- housing %>%
  mutate(., HasPool = as.factor(ifelse(PoolArea > 0, "Y", "N"))) 

# Analysis for other numerical variables

# Histogram
housing %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(colour = "white", fill = "cornflowerblue") +
  ggtitle("Histogram of All the Numeric Variables") +
  theme(axis.text.x = element_text(size=9, angle= -45), 
        plot.title = element_text(hjust = 0.5, size = 15))

# Density plots
housing %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density(colour = "white", fill = "cornflowerblue") +
  ggtitle("Density Plots of All the Numeric Variables") +
  theme(axis.text.x = element_text(size=9, angle= -45), 
        plot.title = element_text(hjust = 0.5, size = 15))

### Quantile plots for all the numerical variables
### Update with the title for this plot 

numerical <- dplyr::select_if(housing, is.numeric)
par()              # view current settings
opar <- par()

par(mfrow=c(2, 2))
#title("Quantile-Quantile plot to check normality")
for(i in 1:ncol(numerical))
{
  qqPlot(numerical[, i], lwd=1, envelope = FALSE, ylab = colnames(numerical)[i], col = alpha("black", 0.3))
}
mtext("Quantile-Quantile plot to check normality", outer = TRUE, cex = 1.5)
par(opar)

# Analysis for categorical variables (barplots)
housing %>%
  keep(is.factor) %>% 
  select(.,1:10) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(colour = "white", fill = "cornflowerblue") +
  ggtitle("Bar Plots of the Factor Variables") +
  theme(axis.text.x = element_text(size=8, angle= 90), 
        plot.title = element_text(hjust = 0.5, size = 15))

housing %>%
  keep(is.factor) %>% 
  select(.,10:20) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(colour = "white", fill = "cornflowerblue") +
  ggtitle("Bar Plots of the Factor Variables") +
  theme(axis.text.x = element_text(size=8, angle= 90), 
        plot.title = element_text(hjust = 0.5, size = 15))

housing %>%
  keep(is.factor) %>% 
  select(.,20:30) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(colour = "white", fill = "cornflowerblue") +
  ggtitle("Bar Plots of the Factor Variables") +
  theme(axis.text.x = element_text(size=8, angle= 90), 
        plot.title = element_text(hjust = 0.5, size = 15))

housing %>%
  keep(is.factor) %>% 
  select(.,30:40) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(colour = "white", fill = "cornflowerblue") +
  ggtitle("Bar Plots of the Factor Variables") +
  theme(axis.text.x = element_text(size=8, angle= 90), 
        plot.title = element_text(hjust = 0.5, size = 15))

## Scatterplot between numeric variables and SalePrice
housing %>%
  keep(is.numeric) %>%  
  select(1:17, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), col=adjustcolor("cornflowerblue", alpha=0.4)) + 
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Numeric Variables") +
  theme(axis.text.x = element_text(size=9, angle= 45), 
        plot.title = element_text(hjust = 0.5, size = 15))

housing %>%
  keep(is.numeric) %>%  
  select(18:34, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), col=adjustcolor("cornflowerblue", alpha=0.4)) + 
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Numeric Variables") +
  theme(axis.text.x = element_text(size=9, angle= 45), 
        plot.title = element_text(hjust = 0.5, size = 15))

# Correlation analysis between numerical variables
##### Network plot
#  install.packages("corrr")
#library(corrr)

detach(package:reshape)
ggcorrplot(cor(numerical), 
                p.mat = cor_pmat(numerical), 
                hc.order=TRUE, type = "lower", method = "circle") +
  ggtitle("Correlation Plot of Numeric Variables") +
  theme(axis.text.x = element_text(size=10, angle= 90),
        axis.text.y = element_text(size=10),
        plot.title = element_text(hjust = 0.5, size = 15))
  
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
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Jitter Box Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(8:14, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Jitter Box Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15)) 

categorical %>%
  select(15:21, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Jitter Box Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(22:28, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Jitter Box Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(29:35, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_boxplot(aes(value, SalePrice)) +
  geom_jitter(aes(value, SalePrice), alpha = 0.3, color = "cornflowerblue") + 
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Jitter Box Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15)) 

# Scatterplot for categorical variable
categorical %>%
  select(1:6, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity", col=adjustcolor("cornflowerblue", alpha=0.5)) +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(7:14, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity", col=adjustcolor("cornflowerblue", alpha=0.5)) +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(15:21, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity", col=adjustcolor("cornflowerblue", alpha=0.5)) +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(22:28, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity", col=adjustcolor("cornflowerblue", alpha=0.5)) +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))

categorical %>%
  select(29:35, "SalePrice") %>%  
  melt(., id.vars = "SalePrice") %>%
  ggplot() + geom_point(aes(value, SalePrice), stat = "identity", col=adjustcolor("cornflowerblue", alpha=0.5)) +
  facet_wrap(~variable, scales = "free_x") +
  ggtitle("Scatter Plots of the SalePrice vs Factor Variables") +
  theme(axis.text.x = element_text(size=9, angle= 90),
        plot.title = element_text(hjust = 0.5, size = 15))