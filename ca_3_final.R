## Libraries
library(reshape2)
library(ggplot2)
library(dplyr)
library(corrplot)


## Property price dataset
prop_price <- read.csv("form_41a-price-new-property-area-by_year_2.csv", skip=1)
prop_price

# remove empty columns and rows
prop_price <- prop_price[1:46,1:8]


# change the name of first column to year
colnames(prop_price)[colnames(prop_price)=="X"] <- "year"


# start from 1975
prop_price <- prop_price[-(1:5),]


# cnvert to numeric
prop_price <- as.data.frame(apply(prop_price,c(1,2), function(x) {as.numeric(sub(",", "", as.character(x), fixed = TRUE))}))


# fill nan
prop_price$Other.Areas[prop_price$year==2015] <- prop_price$Other.Areas[prop_price$year==2014]


# bar plot
prop_price_plot <- melt(prop_price[,c("year","Dublin","Cork","Galway","Limerick","Waterford","Other.Areas")],id.vars = 1)
ggplot(prop_price_plot,aes(x = year,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") 

prop_price

## Population dataset
population <- read.csv("theme_1_electoral_divisions.csv")
population


# group cities by the names in property price dataset
population[,"group"] <- 0
population$group[population$COUNTY=="Carlow"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Fingal"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Dun Laoghaire-Rathdown"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Kildare"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Kilkenny"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Laoighis"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Longford"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Louth"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Meath"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Offaly"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Westmeath"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Wexford"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Wicklow"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Clare"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Kerry"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Tipperary"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Leitrim"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Mayo"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Roscommon"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Sligo"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Cavan"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Donegal"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Monaghan"] <- "Other.Areas.Pop"
population$group[population$COUNTY=="Dublin City"] <- "Dublin.Pop"
population$group[population$COUNTY=="South Dublin"] <- "Dublin.Pop"
population$group[population$COUNTY=="Limerick City"] <- "Limerick.Pop"
population$group[population$COUNTY=="Limerick County"] <- "Limerick.Pop"
population$group[population$COUNTY=="Waterford City"] <- "Waterford.Pop"
population$group[population$COUNTY=="Waterford County"] <- "Waterford.Pop"
population$group[population$COUNTY=="Galway City"] <- "Galway.Pop"
population$group[population$COUNTY=="Galway County"] <- "Galway.Pop"
population$group[population$COUNTY=="Cork City"] <- "Cork.Pop"
population$group[population$COUNTY=="Cork County"] <- "Cork.Pop"



# select total population for 2006 and 2011
population <- select(population, group, Pop_By_Sex_Total_Pop_2011, Pop_By_Sex_Total_Pop_2006)
year_06 <- aggregate(list(year_2006=population$Pop_By_Sex_Total_Pop_2006), by=list(city=population$group), FUN=sum)
year_11 <- aggregate(list(year_2011=population$Pop_By_Sex_Total_Pop_2011), by=list(city=population$group), FUN=sum)



# make a new population dataset
population <- merge(year_06,year_11, by="city")
population <- setNames(data.frame(t(population[,-1])), population[,1])
population$year <- list(2006,2011)
population <- as.data.frame(apply(population,c(1,2), function(x) {as.numeric(sub(",", "", as.character(x), fixed = TRUE))}))
population


## Merge prop_price and population
fin_data <- prop_price
fin_data <- merge(population,fin_data,by="year", all=TRUE)
str(fin_data)


## fill unknows populations
for (i in seq(2,7)){
  y1 <- fin_data[fin_data$year==2006,i]
  y2 <- fin_data[fin_data$year==2011,i]
  step <- (y2-y1)/5
  new_list <- seq(y1-31*step,y2+4*step, by=step)
  new_list <- round(new_list,0)
  fin_data[,c(i)] <- new_list
}

# to check the normality distribution of the two continuous variables, shapiro test is used


# checking the normality distribution of dublin population attribute
normality <- shapiro.test(fin_data$Dublin.Pop)
normality
normality$p.value
# from the shapiro test, the p-value is greater than 0.05, therefore the data is normally distributed

# checking the normality distribution of dublin property pricing attribute
normality1 <- shapiro.test(fin_data$Dublin)
normality1
normality1$p.value
# from the shapiro test, the p-value is lesser than 0.05, therefore the data is not normally distributed

# Since population variable is normally distributed and pricing variable is not normally distributed, 
# anyone, either parametric or non-parametric tests or both tests can be performed.   

# Parametric - pearson's correlation co-efficient test
hypothesis_pearson <- cor.test(x = fin_data$Dublin.Pop, y = fin_data$Dublin, method = 'pearson')
hypothesis_pearson


# Non-parametric - spearman's correlation co-efficient test
hypothesis_spearman <- cor.test(x = fin_data$Dublin.Pop, y = fin_data$Dublin, method = 'spearman')
hypothesis_spearman

# From both the tests, we can see that the p-value is lesser than 0.05, therefore the null hypothesis is rejected and 
# alternative hypothess is proved


# Power Analysis
library(pwr)

# Identifying the effect size
effectsize <- cohen.ES(test = "r", size = "large")
effectsize

# performing pwr.r.test to obtain sample size
samplesize <- pwr.r.test(r = effectsize$effect.size, sig.level = 0.05, power = 0.85, alternative = "two.sided")
samplesize

# plotting sample size
plot(samplesize)

# Generating samples from fin_data
library(dplyr)
sample_data <- sample_n(fin_data, 32)
sample_data
nrow(sample_data)

# performing correlation in sample data frame
corelation_test <- cor.test(sample_data$Dublin.Pop, sample_data$Dublin)
corelation_test
