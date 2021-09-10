# __________________________________   R - PROJECT _____________________________
# _____________________________ E-COMMERCE SHIPPING DATA _______________________
# ________________________________ SOURCE : kaggle.com   _______________________

# Setting and getting the directory
setwd("C:\\Users\\Jordi\\OneDrive\\Documents\\EDUCATION\\METRO COLLEGE OF TECHNOLOGY\\DATA SCIENCE\\R FOR DATA ANALYTICS\\PROJECT")
getwd()

# Calling the library and reading the Excel dataset

df <- read.csv('E-Commerce Shipping Data.csv')

# Looking at the first 6 observations
dim(df)

# Replacing the NULL values with NA
df[df=='',]<- NA

# Total missing values 
sum(is.na(df)) # No missing value in the dataset

# Structure of the data
str(df)
dim(df)

# A. EXPLORATORY DATA ANALYSIS

# Checking existence duplicated values
sum(duplicated(df)) # No duplicated value in this dataset

#   I. Univariate Analysis
#     1. Numeric variables

# Removing categorical variables and 'ID','Reached.on.Time_Y.N' variables
dataset <- df[,-c(2,3,8,9,1,12)]
names(df)
head(dataset)

# Calculating sd and mean for all numeric variables
sapply(dataset, sd, na.rm = T)
sapply(dataset, mean, na.rm= T)

#       a. 'Customer_care_calls' variable

sum(is.na(df)) # No missing value in the whole dataset
summary(df['Customer_care_calls'])
mean(df[['Customer_care_calls']],na.rm = T)
quantile(df[['Customer_care_calls']],na.rm = T)

# Looking for levels 
table_cust_care_calls <- table(df[['Customer_care_calls']]) # Since there are only 6 levels, this variable 
# will be handled as categorical 
table_cust_care_calls # 6 levels
table_cust_care_calls_2 <- c('2' = 638,'3'=3217,'4'=3557,'5'=2328,'6'=1018,'7'=246)
pie_labels <- paste0(table_cust_care_calls_2, '=', round(100 * table_cust_care_calls_2/sum(table_cust_care_calls_2),2),'%')
pie(table_cust_care_calls_2, labels = pie_labels, 
    main = 'Count of Calls by Customer', col = rainbow(length(table_cust_care_calls_2)), cex=.8, edges = 10)
legend('topleft', legend = c('2 calls','3 calls','4 calls','5 calls','6 calls','7 calls'), cex = .7,
       fill = rainbow(length(table_cust_care_calls_2)) )


#       b. 'Customer_rating'

summary(df['Customer_rating'])
table(df[['Customer_rating']])
# Also the 'Customer_rating' has only 5 levels and will therefore handled as 
# categorical.

# install.packages('plotrix') # Installing 'plotrix' for 3D graphics
library(plotrix) 

table_Customer_rating <- c('1'= 2235,'2' = 2165,'3' = 2239,'4' = 2189,'5' = 2171)
pie_labels <- paste0(table_Customer_rating, '=', round(100 * table_Customer_rating/sum(table_Customer_rating),2),'%')
pie3D(table_Customer_rating, labels = pie_labels, 
      main = 'Rating by Customer', col = rainbow(length(table_Customer_rating)), cex=.8)
legend('topright', legend = c('1','2','3','4','5'), cex = .6,
       fill = rainbow(length(table_Customer_rating)) )


#       c. 'Cost_of_the_Product'

summary(df['Cost_of_the_Product'])
length(levels(factor(df$Cost_of_the_Product))) # 215 levels counted

hist(df[['Cost_of_the_Product']], xlab = 'Cost of Product', xlim = c(0,350), 
     main = 'COST OF PRODUCTS', col = 'cyan', probability = T)
lines(density(df$Cost_of_the_Product), col = 'red')


# Detecting outliers
boxplot(df$Cost_of_the_Product, main = 'Cost of Product', ylab = 'Cost') # No outliers


#       d. 'Prior_purchases'
summary(df['Prior_purchases'])
table(df[['Prior_purchases']]) # 8 levels identified which will be handled as 
# categorical
table_Prior_purchases <- c(2,3,4,5,6,7,8,10)

# Visusalization
barplot(table(df[['Prior_purchases']]), ylim = c(0,4000), 
        xlab = 'Number of  Purchases', ylab = 'Count of Customers', 
        axis.lty = 'solid', space = 0.05, col = 'gold2')



#       e. 'Discount_offered'
summary(df['Discount_offered'])
length(levels(factor(df$Discount_offered))) # 65 levels counted


hist(df[['Discount_offered']], xlab = 'Discount offered', xlim = c(0,70), 
     ylim = c(0,.08), main = 'How Many Customers get some discount', col = 'darkseagreen3', 
     probability = T)
lines(density(df$Discount_offered), col = 'blue')

# Detecting outliers
boxplot(df$Discount_offered, main = 'Discount offered Distribution', 
        ylab = 'Discount', xlab = 'Discount offered', horizontal = T)
# Outliers have been detected in the 'Discount_offered' variable. They will
# be handled soon.


#       f. 'Weight_in_gms'
summary(df[['Weight_in_gms']])
range(df[['Weight_in_gms']])
Range <- range(df[['Weight_in_gms']])[2] - range(df[['Weight_in_gms']])[1]
Range
length(levels(factor(df[['Weight_in_gms']]))) # 4,034 levels

hist(df[['Weight_in_gms']], xlab = 'Weight in grams', xlim = c(1000,7500), 
     ylim = c(0,.0004), main = 'Weight of Product (g)', col = 'darkseagreen3', 
     probability = TRUE)
lines(density(df$Weight_in_gms), col = 'red')

# The weights of products most lay in the ranges from 1,000 g to 2,000 and from 
# 4,000g to 6,000g.

# Detecting outliers
boxplot(df$Weight_in_gms, main = 'Weight in gms', ylab = 'Weight(g)') # No outlier


#       g. 'Reached.on.Time_Y.N'
summary(df$Reached.on.Time_Y.N)
levels(factor(df$Reached.on.Time_Y.N)) # 2 levels
# The ' Reached.on.Time_Y.N' variable has two levels and can be handled as a 
# categorical (nominal).

table(df$Reached.on.Time_Y.N)
table_Reached.on.Time_Y.N <- c('0' = 4436,'1'=6563)

pie_labels <- paste0(table_Reached.on.Time_Y.N, '=', 
                     round(100 * table_Reached.on.Time_Y.N/sum(table_Reached.on.Time_Y.N),2),'%')
pie3D(table_Reached.on.Time_Y.N, labels = c('No : 40.33%','Yes : 59.67%'), 
      main = 'Has the product arrived on time ?', 
      col = rainbow(length(table_Reached.on.Time_Y.N)), cex=.8, explode=0.1)
#legend('topright', legend = pie_labels, cex = .6,
#      fill = rainbow(length(table_Reached.on.Time_Y.N)) )


#     2. Categorical variables
#       a. 'Warehouse_block'
table(df$Warehouse_block) # 4 levels identified
table_Warehouse_block <- c('A' = 1833,'B'=1833, 'C'=1833, 'D'=1834, 'F'=3666)
pie_labels <- paste0(table_Warehouse_block, '=', 
                     round(100 * table_Warehouse_block/sum(table_Warehouse_block),2),'%')
pie3D(table_Warehouse_block, labels = c('A' ,'B', 'C', 'D', 'F'), 
      main = 'Which warehouse are products shipped from ? ', col = terrain.colors(length(table_Warehouse_block)), cex=.8)
legend('topright', legend = pie_labels, cex = .6,
       fill = terrain.colors(length(table_Warehouse_block)) )



#       b. 'Mode_of_Shipment'
table(df$Mode_of_Shipment) # 3 levels identified
table_Mode_of_Shipment <- c('Flight' = 1777,'Road'=1760, 'Ship'=7462)
pie_labels <- paste0(table_Mode_of_Shipment, '=', 
                     round(100 * table_Mode_of_Shipment/sum(table_Mode_of_Shipment),2),'%')
pie3D(table_Mode_of_Shipment, labels = c('Flight','Road','Ship'), 
      main = 'How are products shipped ? ', col = cm.colors(length(table_Mode_of_Shipment)), cex=.8)
legend('topright', legend = pie_labels, cex = .5,
       fill = cm.colors(length(table_Mode_of_Shipment)) )


#       c. 'Product_importance'

table_Product_importance <- table(df$Product_importance) # 3 levels identified
barplot(table_Product_importance, ylim = c(0,5000), main = 'Product By Importance',
        xlab = 'Product Importance',
        ylab = 'Frequency', space = 0.6, axis.lty = 'solid')
# Most products shipped are either of low or medium importance.



#   II. Bivariate Analysis
# Installing package 'doBy'
# install.packages("doBy")
# install.packages("Psych")
# install.packages('GGally')

# Loading library doBy and psych
library(doBy)
library(psych)
library(GGally)


# Let's run a pairs panel plot to look at the relationship within the 
# 'Reached.on.Time_Y.N' variable and the variables 'Cost_of_the_Product',
# 'Discount_offered' and 'Weight_in_gms'.


df_sub <- subset(df,select = c(6,10,11,12)) # Selecting some numeric variables

pairs.panels(na.omit(df_sub), scale=TRUE)

head(df_sub)


#     1. Is there any relation between Reached.on.Time_Y.N and Discount_offered ?

length((factor(df$Reached.on.Time_Y.N)))

d <- aggregate(Discount_offered ~ Reached.on.Time_Y.N, data = df, FUN = length)
d

summaryBy(Discount_offered ~ Reached.on.Time_Y.N, df,FUN= c(mean,sd,var,min,max),na.rm=T)

boxplot(Discount_offered ~ Reached.on.Time_Y.N, 
        df, main = 'Relation Reached.on.Time_Y.N vs Discount_offered',
        xlab = '0 = Not arrived on time / 1 = Arrived on time',
        ylab = 'Discount offered',
        col = rainbow(2),
        border = 'brown')

# Converting the Reached.on.Time_Y.N variable into character
dd <- df
dd$Del_On_Time <- as.character(dd$Reached.on.Time_Y.N)


ggplot(dd, aes(x=Discount_offered, color=Del_On_Time)) + 
  geom_histogram(fill="orange")

ggplot(dd, aes(x=Discount_offered, fill = Del_On_Time, color=Del_On_Time)) + 
  geom_histogram(fill="orange", alpha=0.3, position="dodge")


p<-ggplot(dd, aes(x=Discount_offered, fill=Del_On_Time, color=Del_On_Time)) +
  geom_histogram(position="identity", alpha=0.5)
p



#Although the above graph can allow to affirm that the Reached.on.Time_Y.N and 
# Discount_offered variables are dependent, let's test this assumption. 
# Let's run a T-test to check whether these features are dependent or not.
# We are verifying first the conditions before running the t-test method:
#  Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Discount_offered' are independent.

# -	Outliers have been detected in the 'Discount_offered' variable 
# Let's handle now these outliers.

# Saving all outliers in a vector:
df1 <- dd
outliers_Discount_offered <- boxplot(df1$Discount_offered, plot = F)$out

# Removing this vector from the active dataset df1:
df1 <- df1[-c(which(df1$Discount_offered %in% outliers_Discount_offered)),]
dim(df1)


# Saving the second new dataset
write.csv(df1,'New_E-Commerce_Shipping_Data.csv') 
dff <- read.csv('New_E-Commerce_Shipping_Data.csv')
dim(dff)
dff['X'] <- NULL

boxplot(dff$Discount_offered, main = 'Discount offered Distribution after removing outliers', 
        xlab = 'Discount offered', horizontal = T)



###############################################################################
# Checking presence of outliers using a UDF
notout<-function(x){
  print("summary before applying this method ")
  print(summary(x))  
  M1<- quantile(x,  0.25, na.rm = T)
  S1<- quantile(x,  0.75, na.rm = T)
  low1<-M1-3*S1
  up1<-M1+3*S1
  x[x<low1]<-NA #Method 2:x[x<low1]<-low1
  x[x>up1]<-NA  #mwthod 2:x[x>up1]<-up1 
  print("summary after applying this method ")
  print(summary(x)) 
  return(x)
}

as.data.frame(dff$Discount_offered)
sum(is.na(as.data.frame(dff$Discount_offered)))
# O missing values => No remaining outliers

###############################################################################



# Checking t-test assumptions:
# -	Normality assumption of Discount_offered
qqnorm(dff$Discount_offered, main = 'Checking Normality for Discount_offered', 
       pch=1, frame = F)
qqline(dff$Discount_offered, col='red', lwd=2)

# The normality assumption is not verified and we will transform the data into 
# a normal distribution, using a square root transformation.

df2 <- dff
df2$Discount_offered <- sqrt(df2$Discount_offered)

qqnorm(df2$Discount_offered, main = 'Transforming Discount_offered for Normality', 
       pch=1, frame = T)
qqline(df2$Discount_offered, col='green', lwd=2)

# Let's check the skewness of this distribution
skew(dff$Discount_offered, na.rm = T, type = 1) # Skewness before transformation
skew(df2$Discount_offered, na.rm = T, type=1) # Skewness after transformation

# The skewness has gone after transformation from 0.77 to -O.038 which makes 
# the distribution approximately symmetric.

# Saving the third new dataset
write.csv(df2,'shipping_data.csv') 
df3 <- read.csv('shipping_data.csv')
head(df3)
df3$X <- NULL
dim(df3)

# We can now look at the dependency between Reached.on.Time_Y.N and 
# Discount_offered 
t.test(df3$Discount_offered ~ df3$Reached.on.Time_Y.N)

# pvalue = 2.2e-16 < .05 which indicates that we can reject H0 and affirm that 
# 'Reached.on.Time_Y.N' and 'Discount_offered' variables are dependent.

#     2. Does the Reached.on.Time_Y.N variable depend on Warehouse_block?

round(100 * prop.table(table(Reached_On_Time = df3$Reached.on.Time_Y.N, 
                             Warehouse = df3$Warehouse_block)), digits=2)
a <- round(100 * prop.table(table(Reached_On_Time = df3$Reached.on.Time_Y.N, 
                                  Warehouse = df3$Warehouse_block)), digits=2)

table(Reached_On_Time = df3$Reached.on.Time_Y.N, 
      Warehouse = df3$Warehouse_block)


tbl_Time_Whse <- table(Reached_On_Time = df3$Reached.on.Time_Y.N, Warehouse = df3$Warehouse_block)
barplot(a, main = 'Reached_on_Time vs. Warehouse', ylab = '%', 
        ylim = c(0,40), xlab = 'Warehouse')

# Let's check the conditions to run a Chi-square test:
# -	All values in the contingency table are greater than 5 at least
# -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Warehouse_block' are independent

chisq.test(tbl_Time_Whse)


#     3. Is there any relation between Reached.on.Time_Y.N and Product_importance?

tbl_Time_Prod_Importance <- table(Del_On_Time = df3$Del_On_Time, 
                                  Product_importance = df3$Product_importance)

b <- round(100 * prop.table(table(Del_On_Time = df3$Del_On_Time, 
                                  Product_importance = df3$Product_importance)), digits=2)
b

barplot(b, xlab='Product Category',ylab='Percentages',
        main="Reached.on.Time_Y.N by Product_importance",
        beside=T,col=c("darkblue","lightcyan"), legend = rownames(b), 
        args.legend = list(x = "topleft"))




# Products of 'low' and 'medium' importance are the most handled but they are 
# the most late.

# We will run a chi-square to loot at the relation Let's check the conditions to run a Chi-square test:
#  -	All values in the contingency table are greater than 5 at least
#  -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Product_importance' are independent.


chisq.test(tbl_Time_Prod_Importance)

#     4. Does the Reached.on.Time_Y.N depend on Cost_of_the_Product?

c <- aggregate(Cost_of_the_Product ~ Del_On_Time, data = df3, FUN = mean)
c

# Plotting some grouped histograms

p<-ggplot(df3, aes(x=Cost_of_the_Product, fill=as.factor(Del_On_Time), color=as.factor(Del_On_Time))) +
  geom_histogram(position="identity", alpha=0.5)
p

levels(factor(df3$Del_On_Time))
ggplot(df3, aes(x=Cost_of_the_Product, color=as.factor(Del_On_Time))) + 
  geom_histogram(fill="orange")

ggplot(df3, aes(x=Cost_of_the_Product, fill = Del_On_Time, color=Del_On_Time)) + 
  geom_histogram(fill="green", alpha=0.3, position="dodge")

library(plyr)
mu <- ddply(df3, "Del_On_Time", summarise, group_mean_for_Del_On_Time=mean(Cost_of_the_Product))
mu
p+geom_vline(data=mu, aes(xintercept=group_mean_for_Del_On_Time, color=as.factor(Del_On_Time)),
             linetype="dashed")



# plot mean Cost_of_the_Product

boxplot(df3$Cost_of_the_Product ~ df3$Del_On_Time, 
        main = 'Cost of Product vs On-Time delivery',
        xlab = '0 : Not Arrived On Time / 1 : Arrived on Time',
        ylab = 'Cost of product',
        notch = T,
        col = terrain.colors(2),
        border = 'blue')

# The above graph can allow to affirm that the 'Reached.on.Time_Y.N' and
# 'Cost_of_the_Product' variables are dependent but let's test this assumption.
# Since the target variable has only two levels, a T-test will be run.
# Let's first verify the conditions to use it:
# -	No outlier detected in the 'Cost_of_the_Product' variable 
# -	Normality assumption of Cost_of_the_Product
# Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Cost_of_the_Product' are 
# independent.


qqnorm(df3$Cost_of_the_Product, main = 'Checking Normality for Cost_of_the_Product', 
       pch=1, frame = F)
qqline(df3$Cost_of_the_Product, col='blue', lwd=2)

skew(df3$Cost_of_the_Product, na.rm = T, type = 1) # Checking Skewness 

# The normality assumption verified since the distribution has a slight skewness 
# of -0.23 (between -0.5 and 0.5) which indicates it is approximatively symmetric.

# We can now test the hypothesis of dependency between 'Cost_of_the_Product' and
# 'Reached.on.Time_Y.N'

t.test(df3$Cost_of_the_Product ~ df3$Del_On_Time)

# pvalue = 0.1352 > .05 which indicates that we fail to reject H0 and affirm 
# that Reached.on.Time_Y.N and Cost_of_the_Product variables are independent 
# from each other.


# 5.	Does the Reached.on.Time_Y.N variable depend on Mode_of_Shipment?

tbl_Time_Mode_of_Shipment <- table(Del_On_Time = df3$Del_On_Time, 
                                   Mode_of_Shipment = df3$Mode_of_Shipment)
tbl_Time_Mode_of_Shipment

b <- round(100 * prop.table(table(Del_On_Time = df3$Del_On_Time, 
                                  Mode_of_Shipment = df3$Mode_of_Shipment)), digits=2)
b

barplot(b, xlab='Mode of Shipment',ylab='%',
        main="Reached.on.Time_Y.N by Mode_of_Shipment",
        beside=T,col=c("lightblue","lightgoldenrod"), legend = rownames(b), 
        args.legend = list(x = "topleft"))


p<-ggplot(df3, aes(x=Mode_of_Shipment, y=Del_On_Time, color=Mode_of_Shipment)) +
  geom_bar(stat="identity", fill="white")
p


p<-ggplot(df3, aes(x=Mode_of_Shipment, y=Del_On_Time, fill=Mode_of_Shipment)) +
  geom_bar(stat="identity")+theme_minimal()
p



# We can see that most of late-arriving products are shipped by road or sea.

# Let's check the conditions to run a Chi-square test:
# -	All values in the contingency table are greater than 5 at least
# -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Mode_of_Shipment' are independent

chisq.test(tbl_Time_Mode_of_Shipment)


# ----------------------------------------------------------------------------

#     6. Is there any relation between Del_On_Time and Weight_in_gms ?


p<-ggplot(df3, aes(x=Weight_in_gms, fill=as.factor(Del_On_Time), color=as.factor(Del_On_Time))) +
  geom_histogram(position="identity", alpha=0.5)
p


ggplot(df3, aes(x=Weight_in_gms, color=as.factor(Del_On_Time))) + 
  geom_histogram(fill="orange")

ggplot(df3, aes(x=Cost_of_the_Product, fill = Del_On_Time, color=Del_On_Time)) + 
  geom_histogram(fill="green", alpha=0.3, position="dodge")

library(plyr)
mu <- ddply(df3, "Del_On_Time", summarise, group_mean_for_Del_On_Time=mean(Cost_of_the_Product))
mu
p+geom_vline(data=mu, aes(xintercept=group_mean_for_Del_On_Time, color=as.factor(Del_On_Time)),
             linetype="dashed")






length((factor(df3$Weight_in_gms)))

d <- aggregate(Weight_in_gms ~ Del_On_Time, data = df3, FUN = mean)
d

boxplot(Weight_in_gms ~ Del_On_Time, 
        df3, main = 'Relation Del_On_Time vs Weight_in_gms',
        xlab = '0 = Not arrived on time / 1 = Arrived on time',
        ylab = 'Weight (gms)',
        col = 'bisque',
        border = 'chocolate2')

# Let's run a T-test to check whether these features are dependent or not.
# We are verifying first the conditions before running the t-test method:
#  Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Discount_offered' are independent.

# -	Some multivaiate outliers can be detected but they will not be removed since 
# they are relative to the 'Del_On_Time' variable and not actually existing 
# in the variable 'Weight_in_gms'.

dim(df3)
library(ggplot2)

# Checking t-test assumptions:
# -	Normality assumption of Weight_in_gms
qqnorm(df3$Weight_in_gms, main = 'Checking Normality for Weight_in_gms', 
       pch=1, frame = F)
qqline(df3$Weight_in_gms, col='red', lwd=2)
skew(df3$Weight_in_gms, na.rm = T, type = 3) # Negative skewness

# The 'Weight_in_gms' variable is not normal with a skewness of -0.63. 
# We will transform the data into a normal distribution, using a Log transformation.

head(df3)
df3_new <- df3
df3_new$Weight_in_gms <- log10(max(df3_new$Weight_in_gms + 1) - df3_new$Weight_in_gms)


# log10(max(x+1) - x)

qqnorm(df3_new$Weight_in_gms, main = 'Transforming Weight_in_gms for Normality', 
       pch=1, frame = T)
qqline(df3_new$Weight_in_gms, col='green', lwd=2)

# Let's check the skewness of this distribution
skew(df3_new$Weight_in_gms, na.rm = T, type=1) # Skewness after transformation

# The transformation brings the skewness of the distribution from -0.63 to -0.59.
hist(df3_new$Weight_in_gms, xlab = 'Weight (log10(max(x+1) - x))',  ylim = c(0,1.4), xlim = c(0,4),
     main = 'WEIGHT OF PRODUCT (log10(max(x+1) - x))', col = 'cyan', probability = T)
lines(density(df3_new$Weight_in_gms), col = 'red')



# Since it's hard to bring the distribution to normal, it will be discretized.
f <- function(x)
{
  if (x<3.5)
    x <- 'Weight < 3.5'
  else 
    x <- 'Weight >= 3.5'
  return(x)
}

# Creating a new variable
df4 <- df3_new
df4$New_Weight_in_gms <- sapply(df4$Weight_in_gms, f)
levels(factor(df4$New_Weight_in_gms))

# Saving the third new dataset
write.csv(df4,'New_E-Commerce_Shipping_Data2.csv') 
ds <- read.csv('New_E-Commerce_Shipping_Data2.csv')
head(ds)
ds$X <- NULL # Dropping this column
dim(ds)

# The contingency table with the target variable is as follows:

tbl_Time_New_Weight_in_gms <- table(Del_On_Time = ds$Del_On_Time, 
                                    New_Weight_in_gms = ds$New_Weight_in_gms)
tbl_Time_New_Weight_in_gms

b <- round(100 * prop.table(table(Del_On_Time = ds$Del_On_Time, 
                                  New_Weight_in_gms = ds$New_Weight_in_gms)), digits=2)
b

barplot(b, xlab='New_Weight_in_gms',ylab='%', ylim = c(0,50),
        main="Del_On_Time by New_Weight_in_gms",
        beside=T,col=c("chocolate1","cornsilk"), legend = rownames(b), 
        args.legend = list(x = "topright"))


# We can see that most shipped products weigh less than 3.5 (measure based on 
# the new scale) and are delivered late.

# Let's check the conditions to run a Chi-square test:
# -	All values in the contingency table are greater than 5 at least
# -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Del_On_Time' and 'New_Weight_in_gms' are independent

chisq.test(tbl_Time_New_Weight_in_gms)



#     7. Is there any relation between Reached.on.Time_Y.N and Customer_care_calls?

tbl_Time_Customer_care_calls <- table(Del_On_Time = factor(ds$Del_On_Time), 
                                      Customer_care_calls = factor(ds$Customer_care_calls))

tbl_Time_Customer_care_calls

b <- round(100 * prop.table(table(Del_On_Time = ds$Del_On_Time, 
                                  Customer_care_calls = ds$Customer_care_calls)), digits=2)
b


barplot(tbl_Time_Customer_care_calls, xlab='Number of care calls',ylab='Count',
        main="Del_On_Time by Customer_care_calls",
        beside=T,col = terrain.colors(nrow(tbl_Time_Customer_care_calls)), 
        legend = rownames(b), 
        args.legend = list(x = "topleft"))



# We can see that customers give from 4 to 6 calls at least for products that 
# arrive late.

# We will run a chi-square to loot at the relation Let's check the conditions to run a Chi-square test:
#  -	All values in the contingency table are greater than 5 at least
#  -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Reached.on.Time_Y.N' and 'Product_importance' are independent.


chisq.test(tbl_Time_Customer_care_calls)
# pvalue = 0.6157 > 0.05 => We failed to reject H0

#     8. Is there any relation between Del_On_Time_Y.N and Customer_rating?

tbl_Time_Customer_rating <- table(Del_On_Time = factor(ds$Del_On_Time), 
                                  Customer_rating = factor(ds$Customer_rating))

tbl_Time_Customer_rating

b <- round(100 * prop.table(table(Del_On_Time = ds$Del_On_Time, 
                                  Customer_rating = ds$Customer_rating)), digits=2)
b


barplot(tbl_Time_Customer_rating, xlab='Rating',ylab='Frequency',
        main="Del_On_Time by Customer_rating",
        beside=T,col = topo.colors(nrow(tbl_Time_Customer_care_calls)), 
        legend = rownames(b), 
        args.legend = list(x = "bottomleft"))



# Let's see whether some relation exists between 'Del_On_Time_Y.N' and 'Customer_rating'.

# We will run a chi-square to loot at the relation Let's check the conditions to run a Chi-square test:
#  -	All values in the contingency table are greater than 5 at least
#  -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Del_On_Time' and 'Customer_rating' are independent.


chisq.test(tbl_Time_Customer_rating)
# pvalue = 0,1966 > 0.05 => We failed to reject H0



#     9. Is there any relation between Del_On_Time_Y.N and Prior_purchases?

tbl_Time_Prior_purchases <- table(Del_On_Time = factor(ds$Del_On_Time), 
                                  Prior_purchases = factor(ds$Prior_purchases))

tbl_Time_Prior_purchases

b <- round(100 * prop.table(table(Del_On_Time = ds$Del_On_Time, 
                                  Prior_purchases = ds$Prior_purchases)), digits=2)
b

barplot(tbl_Time_Prior_purchases, xlab='Prior purchases',ylab='Frequency',
        main="Del_On_Time by Prior_purchases",
        beside=T,col = cm.colors(nrow(tbl_Time_Prior_purchases)), 
        legend = rownames(b), 
        args.legend = list(x = "bottomleft"))



# It seems that relative new customers with a small number of purchases have 
# their products delivered on time.
# Let's see whether the 'Del_On_Time_Y.N' variable has an established  relation 
# with 'Prior_purchases'.


# We will run a chi-square to loot at the relation Let's check the conditions to run a Chi-square test:
#  -	All values in the contingency table are greater than 5 at least
#  -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Del_On_Time' and 'Prior_purchases' are independent.


chisq.test(tbl_Time_Prior_purchases)
# pvalue = 0,1966 > 0.05 => We failed to reject H0


#     10. Is there any relation between Del_On_Time_Y.N and Gender?

tbl_Time_Gender <- table(Del_On_Time = factor(ds$Del_On_Time), 
                         Gender = factor(ds$Gender))

tbl_Time_Gender

b <- round(100 * prop.table(table(Del_On_Time = ds$Del_On_Time, 
                                  Gender = ds$Gender)), digits=2)
b

barplot(tbl_Time_Gender, xlab='Gender',ylab='Frequency',
        main="Del_On_Time by Gender",
        beside=T,col = heat.colors(nrow(tbl_Time_Gender)), 
        legend = rownames(b), 
        args.legend = list(x = "bottomleft"))



# It appears that the customers who have their products not delivered on time are 
# female. Let's evaluate the relation between the variables 'Del_On_Time_Y.N' 
# and 'Gender'.

# We will run a chi-square to loot at the relation Let's check the conditions to run a Chi-square test:
#  -	All values in the contingency table are greater than 5 at least
#  -	Total frequency = 8790 > 50
# Hypothesis: Ho = 'Del_On_Time' and 'Gender' are independent.


chisq.test(tbl_Time_Gender)
# pvalue = 0,2681 > 0.05 => We failed to reject H0



# ---------------------------------------------------------------------------

# _---------------------------------------------------


# ____________________   MODELLING ______________________________________

# B.	Modelling 



# Model 1 : KNN Model

# Encoding the target variable ('Reached.on.Time_Y.N')

# Installing necessary package for a Support Vector Machine modelling
#install.packages('caTools')
library(caTools)
set.seed(123)
# Splitting the dataset
split = sample.split(ds, SplitRatio = .75)

# Label Encoding the variables 'New_Weight_in_gms' and 'Product_importance'

ds$New_Weight_in_gms <- ifelse(ds$New_Weight_in_gms == 'Weight >= 3.5', 1, 0)
head(ds)

map <- function(x)
{
  if (x == 'low')
    x <- 0
  else if (x == 'medium')
    x <- 1
  else
    x <- 2
  return(x)
}

map('low') # Testing the UDF map function
levels(factor(ds$Product_importance))
# Replacing values in the column 'Product_importance'
ds$Product_importance <- sapply(ds$Product_importance, map)


# Let's create a normalization function to normalize the values in ds

normalize <- function(x)
{
  return(((x-min(x)))/(max(x)-min(x)))
}

# Let's apply the 'normalize' function to ds without target variable c(14)
ds_n <- as.data.frame(lapply(ds[,c(8,10,14,7,13)], normalize))
head(ds_n)

# Splitting the data set
train = subset(ds_n, split == T)
test = subset(ds_n, split == F)
names(ds[13])
tail(ds_n)


nrow(ds)

dim(train)
dim(test)
head(ds)

# Installing package 'class'
# install.packages('class')
library(class)

# Let's now train the train data set with a knn model
ds_test_pred <- knn(train = train, test = test, cl = train$Del_On_Time, k = 200)
summary(ds_test_pred)



# Let's evaluate the 1st KNN model performance
# install.packages('gmodels')
library(gmodels)
CrossTable(x = test$Del_On_Time, y = ds_test_pred, prop.chisq = F)


# Model 2 : Support Vector Classifier Model

# Train-test splitting the data set into 75% and 25%
ds_n <- ds[,c(8,10,14,7,13)]
ncol(ds)
train = subset(ds_n, split == T)
test = subset(ds_n, split == F)


# Fitting a Support Vector Classifier to the training set

# install.packages('e1071')
# install.packages('caret')
# install.packages('kernlab')
library(kernlab)
library(caret)
library(e1071)

# Calling the 'ksvm' function to fit the training dataset
svc <- ksvm(Del_On_Time ~ ., data = train, 
            type = 'C-svc', kernel = 'rbfdot', C=1, prob.model = T)
svc


# Evaluating
svc_pred <- predict(svc, test[-5])

library(caTools)

# Let's create a vector of factors with the target variable

CrossTable(x = test$Del_On_Time, y = ds_test_pred, prop.chisq = F)

#CrossTable(test$)

svc2 = svm(formula = as.factor(Del_On_Time) ~ .,
          data = train,
          cross = 10,
          type = 'C-classification',
          kernel = 'linear',
          probability = T)
svc2

# Model evaluation
summary(svc)
class(train$Discount_offered)

head(test)
predicted <- predict(svc, test[-5])
class(predicted)

# Building a Confusion Matrix
threshold = 0.5
predicted <- ifelse(as.numeric(predict(svc, test[-5], type = 'response')) > threshold, 1, 0)
result_svc <- predict(svc, newdata = test, type = 'response')

range(as.numeric(result_svc))
table(test$Del_On_Time, as.numeric(result_svc) > 0.5)


# ------------------------------------------------------------------------------

# Model 3 : Logistic Regression Model 

# Installing necessary packages
# install.packages('packagename')
# install.packages('aod')
library(aod)
logmodel <- glm(Del_On_Time ~ ., data = train, family = binomial)
summary(logmodel)


# Predictions and evaluation 
logpredict <- predict(logmodel, newdata = test, type = 'response')
View(logpredict)
range(logpredict)

# Building the confusion matrix
table(test$Del_On_Time, logpredict > 0.5)

# CIs using profiled log-likelihood
confint(logmodel)

# Odds ratio and 95% CI
exp(cbind(OR = coef(logmodel), confint(logmodel)))

# Because H0 can occur with the 'Product_importance' variable (the odds ratio 
# crosses risk ratio = 1), this feature will be removed from the model.
head(train)
logmodel2 <- glm(Del_On_Time ~ Prior_purchases + Discount_offered + 
                   New_Weight_in_gms, 
                 data = train, family = binomial)
summary(logmodel2)

logpredict2 <- predict(logmodel2, newdata = test, type = 'response') # New predictions
table(test$Del_On_Time, logpredict2 > 0.5) # New confusion matrix

exp(cbind(OR = coef(logmodel2), confint(logmodel2)))


# ------------------------------------------------------------------------------

# Model 4 : Decision tree Model 
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
head(train)

dt <- rpart(as.factor(Del_On_Time) ~ ., data = train)
dt
rpart.plot(dt, extra = 2, main = 'Decision Tree model')

# Model evaluation using a confusion matrix

dt.pred <- predict(dt, test, type = 'class')
confusionMatrix(dt.pred, as.factor(test$Del_On_Time))
