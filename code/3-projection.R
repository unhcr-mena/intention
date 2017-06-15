### Script to generate population projection

###  Step 1: Weight the dataset -


## Step2: use weighted intention to return by:

# country
# case size, 
# area of origin
# when
# yes /maybe


## Step3: get proportion within original population

## country 
# case size, 
# area of origin

## Step 4: apply proportion



### analysis of correlation
install.packages("corrplot")
source("code/rquery_cormat.r")
install.packages("gplots")
library("gplots")

library("corrplot")
library("graphics")
# install.packages("vcd")
library("vcd")

#rquery.cormat(mydata)

# names(data) - subset data
mydata <- data[ , c("group_intro.goingback","size" )]
# Convert the data as a table
dt <- as.table(as.matrix(mydata))


dt <- table(data$group_intro.goingback, data$size)

# Graph
balloonplot(t(dt), main ="Balloon plot", xlab ="", ylab="", label = FALSE, show.margins = FALSE)

# plot just a subset of the table using Vcd
assoc(head(dt, 5), shade = TRUE, las=3)

## using mosaic
mosaicplot(dt, shade = TRUE, las=2, main = "Mosaic")


### chi sq in a loop

my.list.chisq <- lapply(c("COO_L1","size","CountryAsylum"),function(var)chisq.test(data$group_intro.goingback, data[,var]))

my.list.logi <- lapply(c("COO_L1","size","CountryAsylum"),         
                  function(var) {
                    formula    <- as.formula(paste("group_intro.goingback ~", var))
                    res.logist <- glm(formula, data = data, family = binomial)
                    summary(res.logist)
                  })

str(data$AVG_Age)
data$AVG_Age1 <- cut(as.integer(data$AVG_Age),5)
############################################################

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$COO_L1)
chisq$p.value
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$size)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$CountryAsylum)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$YearArrivalCategory)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$female.ratio)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$dependency)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$dem_ethnCat)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$occupationcat)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$needs)
chisq
corrplot(chisq$residuals, is.cor = FALSE)

## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$dem_marriagecat)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$dem_marriagecat)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$AVG_Age)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
data$At.Risk <- as.character(data$At.Risk)
data$At.Risk[is.na(data$At.Risk)] <- "No"
chisq <- chisq.test(data$group_intro.goingback, as.factor(data$At.Risk))
chisq
corrplot(chisq$residuals, is.cor = FALSE, main="Occurence in the household of person At.Risk")


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Child.Labour)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Child.marriage..parent.or.pregnancy)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Family.Needs)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Marginalised)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Medical)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Need.of.Care)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Problem.with.violence.law.recruitment)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Separated.Child)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Single.Parent)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Unaccompanied)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Victim.of.Violence)
chisq
corrplot(chisq$residuals, is.cor = FALSE)


## chech chi sq test
chisq <- chisq.test(data$group_intro.goingback, data$Woman.at.Risk)
chisq
corrplot(chisq$residuals, is.cor = FALSE)




##### testing logistic regression 


log.reason