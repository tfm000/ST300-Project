# *** ST300 Project *** #

# Loading Our Libraries #
```{r}
library(leaps)
library(ggplot2)
library(dplyr)
library(car)
```

# Loading Our Data #
```{r}
data = read.csv("Market_data_clean.csv",header = T)
head(data)
names(data)
```

# Data Cleaning and Checking #
```{r}
# Removing repeated index and Cost_of_Equity #
data$X = NULL
data$Cost_of_Equity = NULL
data<-na.omit(data)

# Removing Extreme PE Outliers #
ggplot(aes(x=PE,y=PE),data=data)+
  geom_point()+
  geom_vline(xintercept =400)+
  geom_vline(xintercept =100)+
  labs(title="Plot to determine extreme outliers")

length(data["PE"][data["PE"]>100]) # = 26
# We therefore note that 90.8% of our data has a PE ratio less than or equal to 100, with values exceeding this threshould being both very granular and potentially extreme influence points. Thus it is better to remove them, reducing our standard parameter error and resulting in better analysis.

data = data[which(data$PE <= 100),]

head(data)
names(data)
```

# Grouping Factor Levels #
```{r}
# Grouping Industry Factor into 14 Macro Levels #
NonFinancialServices = c("Advertising","Publishing & Newspapers", "Business & Consumer Services")

HeavyMachineryManufacturing = c("Aerospace/Defense","Shipbuilding & Marine","Auto & Truck","Auto Parts","Machinery" ,"Semiconductor",
"Semiconductor Equip")

Transport = c("Air Transport","Transportation", "Transportation (Railroads)","Trucking","Packaging & Container")

FinancialServices = c("Bank (Money Center)","Banks (Regional)","Brokerage & Investment Banking","Insurance (General)","Insurance (Life)","Insurance (Prop/Cas.)","Investments & Asset Management","Reinsurance","Financial Svcs. (Non-bank & Insurance)")

FoodBeverages = c("Beverage (Alcoholic)","Beverage (Soft)","Food Processing","Food Wholesalers")

RealEstate = c("Building Materials","Construction Supplies","Engineering/Construction","Furn/Home Furnishings","Homebuilding","R.E.I.T.","Real Estate (Development)","Real Estate (General/Diversified)",
"Real Estate (Operations & Services)")

TMT = c("Computers/Peripherals","Electronics (Consumer & Office)","Electronics (General)","Information Services","Software (Entertainment)","Software (Internet)","Computer Services","Electrical Equipment","Software (System & Application)","Broadcasting","Cable TV","Telecom (Wireless)","Telecom. Services","Telecom. Equipment")

Diversified = c("Diversified")

ChemicalsDrugsHealthcare = c("Chemical (Basic)","Chemical (Diversified)","Chemical (Speciality)","Drugs (Biotechnology)","Drugs (Pharmaceutical)","Healthcare Products","Healthcare Information and Technology","Hospitals/Healthcare Facilities","Healthcare Support Services")

PublicSector = c("Education","Environmental & Waste Services")

Entertainment = c("Entertainment","Hotel/Gaming","Recreation","Restaurant/Dining")

Energy = c("Green & Renewable Energy","Oil/Gas (Integrated)","Oil/Gas (Production and Exploration)","Oil/Gas Distribution","Oilfield Svcs/Equip.","Power","Coal & Related Energy")

Commodities = c("Metals & Mining","Paper/Forest Products","Precious Metals","Rubber& Tires","Steel","Tobacco","Farming/Agriculture","Utility (General)","Utility (Water)")

Retail = c("Retail (Automotive)","Retail (General)","Retail (Grocery and Food)","Retail (Online)","Retail (Special Lines)","Retail (Building Supply)","Retail (Distributors)","Shoe","Household Products","Office Equipment & Services")

# Modifying DataFrame #
data$IndustryMacro = with(data,ifelse(Industry %in% NonFinancialServices, "Non-Financial Services",ifelse(Industry %in% HeavyMachineryManufacturing, "Heavy Machinery Manufacturing",ifelse(Industry %in% Transport, "Transport",ifelse(Industry %in% FinancialServices, "Financial Services",ifelse(Industry %in% FoodBeverages, "Food & Beverages",ifelse(Industry %in% RealEstate, "Real Estate and Construction",ifelse(Industry %in% TMT, "TMT",ifelse(Industry %in% Diversified, "Diversified",ifelse(Industry %in% ChemicalsDrugsHealthcare, "Chemicals, Drugs and Healthcars",ifelse(Industry %in% PublicSector, "Public Sector",ifelse(Industry %in% Entertainment, "Entertainment",ifelse(Industry %in% Energy, "Energy",ifelse(Industry %in% Commodities, "Non-Energy Commodoties","Retail"))))))))))))))

```

```{r}
# Removing Industry factor #
data$Industry = NULL
data<-na.omit(data)
```



# Partial Residual Plots to determine the nature of the variable relationships  #
```{r}
universe = c(c(2:4),c(6:11)) # Non-factor variables
for (i in universe)
{
  # Estimating the linear relationship between our variable and PE #
  model = lm(paste("PE~",names(data)[i]),data = data)
  
  p1 = ggplot(aes_string(x=names(data)[i],y=names(data)[6],colour=names(data)[1]),data=data)+
          geom_point()+
          geom_smooth(method=lm,level=0.95)+
          ylim(0,15)+
          labs(x = paste(names(data)[i]),y="PE")
  
  p2 = ggplot(aes_string(x=names(data)[i],y=names(data)[6],colour=names(data)[12]),data=data)+
          geom_point()+
          geom_smooth(method=lm,level=0.95)+
          ylim(0,15)+
          labs(x = paste(names(data)[i]),y="PE")  
  
  #Producing a QQ-Plot #
  p3 = ggplot(model)+ 
          stat_qq(aes(sample = rstandard(model)))+
          geom_abline() + 
          labs(title = paste("QQ Residual Plot for",names(data)[i])) 
  
  correlation = cor(data["PE"],data[i])
  print(correlation)
  print(p1)
  print(p2)
  print(p3)
}

```

# Thoughts # 
```{r}
# we note that for several variables, there are many extreme outliers, which we shall have to remove using cooks distance before fitting any model.
#we also note that once the extreme PE values are removed, there is no clear linear relationship between PE and ROE, EPS Growth and PBV, with each having a correlation coefficient <0.1. Although we must conduct further statistical tests into these factors before removing them.

#We also note that the Number of firms, Beta, Cost of equity, PS, CEO and Insitutional holdings each have absolute correlations > 0.15, and thus messy, but slight linear relationships. We must however conduct statistical and multiple collinearity tests upon these before including them in our model.

#We also note, that there is a clear geographic factor interaction effect with some of the variables, especially with insitutional holdings, where the 95% quantiles dont overlap along significant portions of the linear models, showing a clear factor interaction effect. We note however, while PBV may look to have an extreme geographical effect, it is likely due to the presence of extreme influence points with the US level, and thus further statisitical analysis must be done.

#Given each predictor has excess kurtosis, represented through curved qq-plots, we must either apply a transformation or use a glm, as the residuals are clearly non-normal.

```

```{r}
#We note that PE~PS displays heterocedasticity, and therefore a log(PE)~log(PS) transformation is appropriate 
#similarily a log-log transformation on number of firms produces a linear relationship with PE - same with PBV
#a log(PE), ROE relationship results in a greater absolute correlation between the two variables, as the transformation reduces variability - same for EPS_Growth

# We determine the transformation to use by subsituting in and out different function transformations, manipulating our data to get a linear relationship. Below is example code for the log-log transformation of Cost_of_equity and PE. A similar approach is made for other continuous predictors.

ggplot(aes(x=log(Cost_of_equity),y=log(PE)),data=data)+
  geom_point()+
  labs(title="Plot to determine Appropiate Transformation")
cor(data["PE"],data["Cost_of_equity"])
cor(log(data["PE"]),data["Cost_of_equity"])
cor(data["PE"][data["Cost_of_equity"]>0],log(data["Cost_of_equity"][data["Cost_of_equity"]>0]))
cor(log(data["PE"][data["Cost_of_equity"]>0]),log(data["Cost_of_equity"][data["Cost_of_equity"]>0]))

# Transformations summary:
#log-log : PS, Number_of_firms, PBV, Institutional_holding, Cost_of_equity
#log(PE)~variable: ROE, EPS_Growth, Beta, CEO_holding
```


# Applying Transformations #
```{r}
transformedData = data.frame(Region = data$Region,IndustryMacro = data$IndustryMacro, Number_of_firms = log(data$Number_of_firms),ROE = data$ROE, EPS_Growth = data$EPS_Growth,PBV = log(data$PBV),PS = log(data$PS),Beta = data$Beta, CEO_holding = data$CEO_holding, Institutional_holding = log(data$Institutional_holding),Cost_of_equity = log(data$Cost_of_equity),PE = log(data$PE) )
head(transformedData)
names(transformedData)

```

```{r}
universe2 = c(3:11) # Non-factor variables
for (i in universe2)
{
  # Estimating the relationship between our variable and log(PE) #
  model = lm(paste("PE~",names(transformedData)[i]),data = transformedData)
  
  p1 = ggplot(aes_string(x=names(transformedData)[i],y=names(transformedData)[12],colour=names(transformedData)[1]),data=transformedData)+
          geom_point()+
          labs(x = paste(names(transformedData)[i]),y="PE")
  
  p2 = ggplot(aes_string(x=names(transformedData)[i],y=names(transformedData)[12],colour=names(transformedData)[2]),data=transformedData)+
          geom_point()+
          labs(x = paste(names(transformedData)[i]),y="PE")  
  
  #Producing a QQ-Plot #
  p3 = ggplot(model)+ 
          stat_qq(aes(sample = rstandard(model)))+
          geom_abline() + 
          labs(title = paste("QQ Residual Plot for",names(transformedData)[i])) 
  
  correlation = cor(transformedData["PE"],transformedData[i])
  print(correlation)
  print(p1)
  print(p2)
  print(p3)
}
```

# Comments on Transformations #
```{r}
# We now note that after we have applied our transformations, our absolute linear correlation coefficients have increased for all variable, whilst our residuals are now normal.

#Interaction effects -> inply different gradients. We therefore see this suggests
# Insitutional_holding has interaction effect with Region
```

# Variable Selection #
```{r}
# Stepwise - Forward selection
null = lm(PE~1,data=transformedData)
full = lm(PE~., data=transformedData)
mBalF = step(null,scope=list(lower=null,upper=full),direction='forward',trace=0)
summary(mBalF)

#Backward Elimination
mBalB = lm(PE~.,data=transformedData)
mBalB = step(mBalB,trace=0) #trace = 0 means it will output only the final model
summary(mBalB)

# Best Subsets #
BS = leaps::regsubsets(PE~.,nvmax=11,data=transformedData)
plot(BS,scale='bic')
```

# We note that backwards, forward and best subsets elimination suggest IndustryMacro levels, CEO_holdings, ROE to be individually statistically insignificant indicators at the 5% level. However, cost of equity is removed in backwards selection and Beta in forward & best subsets, and institutional holdings & PS removed by best subsets. We must therefore carry out a further statistical tests to determine the overall significance of the variables. 
#common factors are: intercept, Region, number_of_firms, EPS_Growth, PBV
#common in two slection models :PS, Cost_of_equity
#variables removed in 2 models : Beta
#common factors removed in all models: industrymacro, ROE, CEO_holding

# Partial F-Test on commonly removed variables #
```{r}
Rm = lm(PE~.-IndustryMacro - ROE - CEO_holding - Beta, data = transformedData)
uRm = lm(PE~., data = transformedData)
anova(Rm,uRm) # -> F-Test probability of 0.197 -> not statistically significant. Therefore we can remove these variables.
```

# Removing Variables #
```{r}
transformedData$IndustryMacro = NULL
transformedData$ROE = NULL
transformedData$Beta = NULL
transformedData$CEO_holding = NULL
transformedData<-na.omit(transformedData)
```


# Partial F-Test on variables common in two models #
```{r}
Rm = lm(PE~.- PS - Cost_of_equity, data = transformedData)
uRm = lm(PE~., data = transformedData)
anova(Rm,uRm) # -> F-Test probability of 0.002 -> we cannot reject at the 5% level that the variables are statistically significant. Looking at our remain variables further we see:
summary(uRm)
```


```{r}
# Therefore our data suggests that the Region factor may not be statistically significant. Conducting a Partial F-Test on this:
Rm = lm(PE~.- Region, data = transformedData)
uRm = lm(PE~., data = transformedData)
anova(Rm,uRm) # -> F-Test probability of 0.0013 -> we cannot reject at the 5% level that the Region Factor variables are statistically significant.

```

# Testing Interaction Effect #
```{r}
# We suggested earlier the idea that there was an interaction effect between insitutional holdings and the Region Factor. Conducting an F-Test on this:
Rm = lm(PE~., data = transformedData)
uRm = lm(PE~. + Institutional_holding*Region, data = transformedData)
anova(Rm,uRm) # -> F-Test probability of 0.103 and not a substantial increase in adjR^2-> we reject at the 5% level that there is a statistically significant interaction effect between these two variables.
```


# Testing For Multicollinearity#
```{r}
# Calculating GVIF #
car::vif(Rm)

# We therefore see that all the variables have GVIF < 10 , and thus Multicollinearity is not a significant problem for our model.
```

# Removing Outliers Using Cook's Difference #
```{r}
# Visualising Outliers # 
cooksd = cooks.distance(Rm)
plot(cooksd,pch="*",cex=2,main="Influential Observations by Cooks Distance")
cutoff = 4/nrow(data) # using 4/n threshold 
abline(h = cutoff,col="red")
text(x=1:length(cooksd)+1,y=cooksd,labels = ifelse(cooksd>cutoff,names(cooksd),""),col = "red")
# we note we have a significant number of outliers by this metric #

# Removing Outliers #
influentialPoints = as.numeric(names(cooksd)[cooksd > cutoff]) # 19 Oultiers
FinalData = transformedData[-influentialPoints,]
FinalModel = lm(PE~.,data=FinalData)
```

# Evaluating Final Model #
```{r}
summary(FinalModel)

ggplot(FinalModel)+
  stat_qq(aes(sample=.stdresid))+
  geom_abline()
car::avPlots(FinalModel)
```
