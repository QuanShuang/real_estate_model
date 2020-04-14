# Preliminary steps 

# Be sure we have the neccesary packages installed/loaded

# install.packages(c("tidyverse", "plm","glmnet","gridExtra","tidyr"))
 
library(tidyverse)  #collection of R packages for data science 

library(plm)          #to run panel data regressions

library(glmnet)      #to run Lasso regressions (ML)

library(gridExtra)  #to put many graphs in one (subplots)

library(tidyr)      #get some extra tools like separate

# Set working environment to my git folder
# setwd("D:/data/real_estate/real_estate_model")


# Load data

MLS <- read.csv("data/data4project.csv", stringsAsFactors = FALSE
                ,strip.white = TRUE,sep = ',') 
str(MLS)

# Check my data
head(MLS)
summary(MLS) #Descriptive


# Variable Distribution

table(MLS$type,MLS$bedrooms,exclude = NULL)  #Type of houses, bedrooms

table(MLS$type,MLS$fireplace,exclude=NULL)  #Type of houses, fireplaces

table(MLS$yearbuilt,exclude=NULL) #Built year

#let us check the distribution

MLS$lnprice <- log(MLS$price_sold)

MLS$age <- MLS$year-MLS$yearbuilt

##`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


# Some plots
plot(MLS$bedrooms,MLS$lnprice, xlab="bedrooms",ylab="lnprice")

plot(MLS$baths,MLS$lnprice,xlab="baths",ylab="lnprice")

plot(MLS$days_on_market,MLS$lnprice,xlab="days_on_the_market"
     ,ylab="lnprice")

plot(MLS$age,MLS$lnprice, xlab="age",ylab="lnprice")

#count
counts <- table(MLS$lnprice)
barplot(counts, ylab="counts", xlab="log of price sold")


#Need to do some cleaning...

#first, cleaning up the type:
unique(MLS$type)

MLS$type <- ifelse(MLS$type=="Res"   | MLS$type=="Resid", "Residential"
                   , MLS$type) 
MLS$type <- ifelse(MLS$type=="Multi" | MLS$type=="Mdw", "Multidweling"
                   , MLS$type) 
MLS$type <- ifelse(MLS$type=="Condo", "Built As Condomimum", MLS$type) 
MLS$type <- ifelse(MLS$type=="", "Other", MLS$type) 
unique(MLS$type)

#There are many different strings for pool and we only want a yes/no 
#type of variable

attach(MLS)
unique(MLS$pool)

MLS$pooldummy <- ifelse(pool!="None_"
                        & pool!="NONE_"
                        & pool!="No Swimming Pool"
                        & pool!="No Swimming Poo"
                        & pool!="None"
                        & pool!="Pool-No Swimming Poo"
                        & pool!="Above Ground,None"
                        & pool!="Ingrn,None_"
                        & pool!="INGRN,NONE_"
                        & pool!="ABOVE,NONE_"
                        & pool!="INGRN,NONE_"
                        , 1, NA)

# MLS$pooldummy

MLS$pooldummy <- ifelse(is.na(MLS$pooldummy)
                        , 0, ifelse(pool=="","",MLS$pooldummy))

# MLS$pooldummy

table(pool,MLS$pooldummy,exclude=NULL)

detach(MLS)


#now let us define basement

MLS$basement<-toupper(MLS$basement) 
MLS$basementdummy<-ifelse(grepl("FINSH",MLS$basement)
                          ,"Yes",ifelse(MLS$basement=="","","No")) 
table(MLS$basementdummy)


#now let us check the heater type
MLS$heattype<-toupper(MLS$heattype) 
unique(MLS$heattype)
MLS$centralheat<-ifelse(grepl("CNTRL",MLS$heat)
                        ,"Yes",ifelse(MLS$heattype=="","","No"))

table(MLS$centralheat)


cat("Unique MLS Styles \n")
unique(MLS$style)

MLStest <- MLS %>% separate(style, c("style"),extra='drop')
          #you could also try adding an specific separator like 'comma'
          # with adding the option sep=','

cat("\n New Unique MLS styles \n")
unique(MLStest$style)



attach(MLS)
MLS$style <- ifelse(style=="Tradt", "Traditional", style)
MLS$style <- ifelse(style=="Europ", "European", style)
MLS$style <- ifelse(style=="Colon", "Colonial", style)
MLS$style <- ifelse(style=="Cntmp", "Contemporary", style) 
MLS$style <- ifelse(style=="Cntry", "Country", style) 
MLS$style <- ifelse(style!="Traditional" & style!="European"
                    & style!="Colonial" & style!="Conte mporary" 
                    & style!="Country" & style!="Ranch", "Other"
                    , style)

detach(MLS)
unique(MLS$style)


MLS$school_dist <-ifelse(is.na(MLS$school_dist),100000
                         ,MLS$school_dist)


# Further cleaning based on weird and bias observations:
#   1. Houses with missing bedrooms/fireplace/baths 
#   2. Houses types with not many observations 
#   3. Houses that were built in an unknown/weird year 
#   4. Houses with missing zipcode 
#   5. Houses with missing info on heating/basement/pool

MLS<-MLS[(is.na(MLS$bedrooms)==FALSE 
          & is.na(MLS$fireplace)==FALSE 
          & is.na(MLS$baths)==FALSE),]

MLS<-MLS[MLS$type!="Deeded" 
         & MLS$type!="Conversion" 
         & MLS$type!="Quadraplex" 
         & MLS$type!="Duple x",] 

MLS<-MLS[MLS$yearbuilt<2009,]

MLS<-MLS[is.na(MLS$zipcode)==FALSE,] 

MLS<-MLS[MLS$pooldummy!="" 
         & MLS$basementdummy!=""& MLS$centralheat!="",]

# Convert some others to factor variables

MLS$pooldummy <- factor(MLS$pooldummy) 
MLS$basementdummy <- factor(MLS$basementdummy) 
MLS$centralheat <- factor(MLS$centralheat) 
MLS$type <- factor(MLS$type) 
MLS$style <- factor(MLS$style) 
MLS$school_dist <- factor(MLS$school_dist) 
MLS$year<- factor(MLS$year) 
MLS$zipcode <- factor(MLS$zipcode)

# Delete what we will not need in the analysis

MLS$yearbuilt<-NULL 
MLS$pool <- NULL 
MLS$sqft <- NULL 
MLS$heattype <- NULL 
MLS$basement <- NULL 
MLS$city <- NULL 
MLS$elemsch <- NULL 


## Predicting Home Prices 
# After doing all the preliminary work, 
# We can finally use the data to answer some questions. 

# Hedonic Regressions

sm<-summary(lm(lnprice ~
                 bedrooms + age + baths + style + type 
               + basementdummy  + pooldummy 
               , data = MLS))
sm

mean(sm$residuals^2)


# Including fixed-effects to account for 
# unobservables variables that are fix 
# within and vary across a neighborhoods 
# (school dist, postal code)

sm<-summary(lm(lnprice ~ 
                 bedrooms + age + baths + style + type 
               + basementdummy  + pooldummy + school_dist
               + year + zipcode , data = MLS))
sm

mean(sm$residuals^2)


# Lasso Regression 

grid = 10^seq(10, -2, length = 100)

# Let’s set up the data now.
# trim off the first column and leaving only the independent variables 

x = model.matrix(lnprice ~ bedrooms + age + baths
                 + style + type  + basementdummy  + pooldummy
                 + school_dist + school_dist + zipcode 
                 + year, MLS)[,-1] # only select the dependent variable 


y = MLS %>% select(lnprice) %>% unlist() %>% as.numeric()


#We now split the samples into a training set and a test set in order to
# estimate the test error of the Lasso Regression


set.seed(1) 

# we select half of the data to train the model and half to test it 


train = MLS %>% sample_frac(0.5) 
test = MLS %>% setdiff(train) 


# we now set the dependent and independent variables 

x_train = model.matrix(lnprice~ bedrooms + age + baths + style
                       + type  + basementdummy  + pooldummy
                       + school_dist + zipcode + year, train)[,-1] 

x_test = model.matrix(lnprice~bedrooms + age + baths + style + type
                      + basementdummy  + pooldummy + school_dist
                      + zipcode + year, test)[,-1]


y_train = train %>% select(lnprice) %>% unlist() %>% as.numeric()

y_test = test %>% select(lnprice) %>%  unlist() %>% as.numeric()

#The glmnet() function has an alpha argument that determines what type
# of model is fit. If alpha = 0 then a ridge regression model is fit, 
# and if alpha = 1 then a lasso model is fit. 

lasso_mod = glmnet(x_train, y_train, alpha = 1
                   ,lambda = grid)  # Fit lasso model on training data 

dim(coef(lasso_mod)) #Dimensions of the coefficients matrix

plot(lasso_mod)    # Draw plot of coefficients

# We now perform cross-validation (out of sample testing) 
# and compute the associated test error:

set.seed(1) 
cv.out = cv.glmnet(x_train, y_train, alpha = 1) 
            # Fit lasso model on training data 

plot(cv.out) # Draw plot of training MSE as a function of lambda

bestlam = cv.out$lambda.min # Select lamda that minimizes training MSE 
lasso_pred = predict(lasso_mod, s = bestlam, newx = x_test) 
            # Use best lambda to predict test data 

mean((lasso_pred - y_test)^2) # Calculate test MSE


out = glmnet(x, y, alpha = 1, lambda = grid) 
    # Fit lasso model on full dataset 

lasso_coef = predict(out, type = "coefficients", s = bestlam)[1:112,]
    # Display coefficients using lambda chosen by CV 
    #here if we have 113 index, then use 113. 

lasso_coef



# Selecting only the predictors with non-zero coefficients, 
# we see that the lasso model with λ chosen by crossvalidation 
# contains only 23 variables:
lasso_coef[lasso_coef != 0] # Display only non-zero coefficients
