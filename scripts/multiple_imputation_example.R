# Tutorial following https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

# multiple permutation using {mice}
library(mice)
library(VIM)

data <- airquality
data[4:10,3] <- rep(NA, 7) # replace some datapoints with NAs
data[1:5,4] <- NA # replace some datapoints with NAs
data

# replacing categorical variables usually not advisable.
# let's get rid of categorical variables for now to avoid confusion
data <- data[-c(5,6)]
summary(data) # shows number of NAs per variable on follow row
# Ozone has the most missing datapoints.

# 5% is usually a good max threshold for missing data per variable
# check for the proportion of missing data per variable:
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)
# so Ozone is missing 24% - should maybe consider leaving it out
# or collecting more data

# Using {mice} to look at missing data pattern
mice::md.pattern(data)
# 104 samples are complete; 34 samples missing only Ozone

# or visualise using {VIM} package
agrr_plot <- VIM::aggr(data, col=c('navyblue', 'red'), numbers=T, sortVars=T, labels=names(data), 
                       cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# shows 70% of samples not missing any data; 22% missing the ozone value; remaining show other missing patterns

marginplot(data[c(1,2)])

# impute the missing values
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500) #m=5 means number of imputed datasets (default)
# meth='pmm' refers to imputation method, here 'predictive mean matching'. see methods(mice) for more
summary(tempData)

# check the imputed data for the Ozone variable
tempData$imp$Ozone
# shows the imputed data for each observation across the 5 imputations

# check imputation method used for each variable
tempData$meth

# get back the completed dataset using complete()
completedData <- complete(tempData,1)
head(completedData)

# compare the distributions of original and imputed data using some useful plots
xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)

# Let's compare the distributions of original and imputed data using a some useful plots.
# First of all we can use a scatterplot and plot Ozone against all the other variables
densityplot(tempData)
# The matching shape tells us that the imputed values are indeed "plausible values".

# Another helpful plot is the density plot:
stripplot(tempData, pch = 20, cex = 1.2)
# Again, under our previous assumptions we expect the distributions to be similar

## Pooling

# Suppose that the next step in our analysis is to fit a linear model to the data. You may ask what imputed dataset to choose. The mice package makes it again very easy to fit a a model to each of the imputed dataset and then pool the results together

modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))

summary(pool(modelFit1))

# The variable modelFit1 containts the results of the fitting performed over the imputed datasets, while the pool() function pools them all together. Apparently, only the Ozone variable is statistically significant.

# Note that there are other columns aside from those typical of the lm() model: fmi contains the fraction of missing information while lambda is the proportion of total variance that is attributable to the missing data

#Remember that we initialized the mice function with a specific seed, therefore the results are somewhat dependent on our initial choice. To reduce this effect, we can impute a higher number of dataset, by changing the default m=5 parameter in the mice() function as follows:

tempData2 <- mice(data,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))

# After having taken into account the random seed initialization, we obtain (in this case) more or less the same results as before with only Ozone showing statistical significance.