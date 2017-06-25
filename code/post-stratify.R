#### post stratification of the sample

## cf tuto here: http://www.andrew.cmu.edu/user/jsmurray/teaching/303/files/lab.html
## https://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
## http://sdaza.com/survey/2012/08/25/raking/


library(survey)

source("code/1-loaddata.R")

#The survey package provides a survey.design object, which is a container for a dataset and the
# sampling design information, including sampling scheme, weights, population sizes (and more).

# The svydesign function is used to create survey.design objects.
#It has a number of arguments, but the most important for you are:

###  ids: Name of variable in the dataframe that contains cluster ids
##  ids = ~1 means there is no clustering.

###  strata: Names of stratification variables, as a formula: ~var1 + var2 + var3
## strata = NULL means there was no stratification.

## weights	: Formula or vector specifying sampling weights as an alternative to prob
# probs: Formula or data frame specifying cluster sampling probabilities

###  fpc (finite population correction) : A vector the same length as the data, giving the stratum population size for each observation.
##The name is confusing, since you don’t actually supply the finite population correction factor.
## fpc = rep(N, n): The function call rep(N, n) generates a vector of length n where each entry is
## N (the population size).

## “Independent sampling design” means that the sampling design is an SRS - Stratified Random Sample.
## When the population size is specified (via the fpc argument) it is assumed that the SRS is without replacement.

###  data: Dataframe containing the raw survey data
## data = dat tells svydesign where to find the actual data.

#################################################################
## First load the universe
##loading  case profile from progres
progrescase <- read_csv("data/progrescase-1.csv")

universe <- progrescase[progrescase$CountryAsylum %in% c("ARE", "IRQ", "JOR"), ]
N <- nrow(universe)
n <- nrow(data)

rm(progrescase)

######################################################################
## Option 1 - use  weight from the original sampling plan
## Abanadonned because of low response rate
#str(data$Prob)
data$Prob <- as.numeric(data$Prob)
data$Stratum2 <- paste(data$Stratum, data$ctr)
levels(as.factor(data$Stratum2))


## building the survey object
survey.data = svydesign(
  ids = ~ 1,
  strata = data$Stratum2,
  weights = data$Prob,
  fpc = rep(N, n),
  data = data
)
#summary(survey.data)


######################################################################
## Option 1 - Doing poststratification
## We will build 2 stratum - corresponding to the 2 dependent variable to intention as per the chi square test.
## ctry of Asylum
## Are of origin

## Relative frequencies for each of these levels from the population data frames

universe$ctr <- as.character(universe$CountryAsylum)
universe$ctr[universe$ctr == "ARE"] <- "Egypt"
universe$ctr[universe$ctr == "JOR"] <- "Jordan"
universe$ctr[universe$ctr == "IRQ"] <- "Iraq"

universe.ctr <- as.data.frame(table(universe$ctr))
names(universe.ctr)[1] <- "ctr"
ctr <- levels(as.factor(data$ctr))

## Now Area of Origin
# We need first to recategorise Cool1
data$key <- paste(data$ctr,data$COO_L1,sep="-")

universe$key <- paste(universe$ctr,universe$cool1,sep="-")
#levels(as.factor(universe$key))
#levels(as.factor(data$key))
cool1cat <- levels(as.factor(data$key))
universe$COO_L1 <- as.character(universe$cool1)
universe$COO_L1[!(universe$key %in% cool1cat)] <- "Other"
universe$key <- paste(universe$ctr,universe$COO_L1,sep="-")


### Add Case size stata

data$Case.size2 <- data$size
#levels(as.factor(data$Case.size2))
#prop.table(table(data$Case.size, useNA = "ifany"))
#check <- data[is.na(data$Case.size2),]

data$Case.size <- car::recode(data$Case.size2,"'Case.size.1'='Case.size.1';
                                 'Case.size.2'='Case.size.2';
                                 'Case.size.3'='Case.size.3.to.5';
                                 'Case.size.4'='Case.size.3.to.5';
                                 'Case.size.5'='Case.size.3.to.5';
                                 'Case.size.over.5'='Case.size.6.and.more';
                                 'Case.size.over.5'='Case.size.6.and.more'")
universe$Case.size2 <- universe$Case.size
universe$Case.size <- car::recode(universe$Case.size,"'Case.size.1'='Case.size.1';
                                 'Case.size.2'='Case.size.2';
                                 'Case.size.3'='Case.size.3.to.5';
                                 'Case.size.4'='Case.size.3.to.5';
                                 'Case.size.5'='Case.size.3.to.5';
                                 'Case.size.6'='Case.size.6.and.more';
                                 'Case.size.7.and.more'='Case.size.6.and.more'")


data$key2 <- paste(data$ctr,data$COO_L1,data$Case.size,sep="-")
universe$key2 <- paste(universe$ctr,universe$COO_L1,universe$Case.size,sep="-")

prop.table(table(data$key2, useNA = "ifany"))
prop.table(table(universe$key2, useNA = "ifany"))


universe.COO_L1 <- as.data.frame(table(universe$COO_L1))
names(universe.COO_L1)[1] <- "COO_L1"

universe.key <- as.data.frame(table(universe$key))
names(universe.key)[1] <- "key"


universe.key2 <- as.data.frame(table(universe$key2))
names(universe.key2)[1] <- "key2"

#data.key2 <- as.data.frame(table(data$key2))

cat("create the unweighted survey object\n")
## create the unweighted survey object
data.svy.unweighted <- svydesign(ids =  ~ 1,
                                 data = data)

## Post stratify on those relative frequency


cat("post stratification on ctr and on area of Origin\n")
## Try post stratification on ctr and on area of Origin
data.svy.rake.ctr.coo <- rake(
  design = data.svy.unweighted,
  #sample.margins = list( ~ ctr,  ~ COO_L1),
  #population.margins = list(universe.ctr, universe.COO_L1)
  sample.margins = list( ~ key),
  population.margins = list(universe.key)
)


cat("post stratification on ctr, area of Origin & case size\n")
## Try post stratification on ctr, area of Origin & case size
data.svy.rake.ctr.coo.size <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ key2),
  population.margins = list(universe.key2)
)


cat("post stratification only on area of Origin\n")
## Try post stratification only on area of Origin
data.svy.rake.coo <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ COO_L1),
  population.margins = list(universe.COO_L1)
)


cat("post stratification only on ctr of asylum\n")
## Try post stratification only on ctr of asylum
data.svy.rake.ctr <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ ctr),
  population.margins = list(universe.ctr)
)


prop.table(table(data$ctr, data$group_intro.goingback),1)
#svyby(  ~ group_intro.goingback,  by =  ~ ctr,  design = data.svy.rake.ctr.coo,  FUN = svymean)

proportion <- svyby(  ~ group_intro.goingback,  by =  ~ ctr+COO_L1,  design = data.svy.rake.ctr.coo,  FUN = svymean)

svytable( ~ group_intro.goingback+ ctr, data.svy.rake.ctr.coo )
svymean ( ~ group_intro.goingback + ctr, data.svy.rake.ctr.coo )

#svyratio( ~ group_intro.goingback,~ ctr, design =data.svy.rake.ctr.coo )
#svyglm( ~ group_intro.goingback,~ ctr, design =data.svy.rake.ctr.coo )

#ftable(svytable ( ~ group_intro.goingback + ctr, data.svy.rake.ctr.coo ))
#ftable(svymean ( ~ group_intro.goingback + ctr, data.svy.rake.ctr.coo ))

## Use post-stratify
#postStratify(design, strata, population, partial = FALSE, ...)
## strata           A formula or data frame of post-stratifying variables
## population       A table, xtabs or data.frame with population frequencies
## partial          if TRUE, ignore population strata not present in the sample

## sometimes it is necessary to trim weights, if they have grown too large or too small.
## This will make your data fit less well the population marginal distributions,
## but inflating a few cases to too much weight, is rarely ever sensible:
## Perhaps that one person that now counts as 50 is somewhat deranged, or otherwise not representative.
# So it is best to keep an eye on your weights.
summary(weights(data.svy.rake.ctr.coo))

cat("Trim weight for Asylum, Origin\n")
data.svy.rake.trim <- trimWeights(data.svy.rake.ctr.coo,
                                  lower = 0.3,
                                  upper = 3,
                                  strict = TRUE)

proportion.trim <- svyby(  ~ group_intro.goingback,  by =  ~ ctr+COO_L1,  design = data.svy.rake.trim,  FUN = svymean)

#names(proportion.trim)
proportion.trim$key <- paste(proportion.trim$ctr,proportion.trim$COO_L1,sep="-")


## Get orginal # from universe
universe.cool1.ctr <-  as.data.frame(table(universe$key))
names(universe.cool1.ctr)[1]<-"key"
## Average family size
universe.temp <- universe[ c("key","Num_Inds")]
universe.cool1.ctr.size <- aggregate(. ~ key, universe.temp, mean)


cat("Create estimation based on Asylum, Origin & Case size\n")

estimation <- join(x=universe.cool1.ctr, y=proportion.trim, by="key",type="left")
estimation <- join(x=estimation, y=universe.cool1.ctr.size, by="key",type="left")

#names(estimation)
#estimation$numyes <- format(estimation$Freq * estimation$group_intro.goingbackYes, digits=0, decimal.mark=".", big.mark=",", scientific=FALSE)

estimation$numyes <- estimation$Freq * estimation$group_intro.goingbackYes
estimation$numUndecided <- estimation$Freq * estimation$group_intro.goingbackUndecided
estimation$numyesind <- estimation$Freq * estimation$group_intro.goingbackYes * estimation$Num_Inds
estimation$numUndecidedind <- estimation$Freq * estimation$group_intro.goingbackUndecided * estimation$Num_Inds

estimation$se.numyes <- estimation$Freq * estimation$se.group_intro.goingbackYes
estimation$se.numUndecided <- estimation$Freq * estimation$se.group_intro.goingbackUndecided

### Aggregate info
estimation.less <- estimation[ ,c( "ctr" ,  "COO_L1", "Freq", "numyes", "numyesind", "numUndecided","numUndecidedind","se.numyes", "se.numUndecided" )]

estimation.less[] <- lapply(estimation.less, function(x) type.convert(as.character(x)))

#str(estimation1)
estimation.coo <-aggregate(. ~ COO_L1, estimation.less, sum)

write.csv(estimation.coo, "data/estimationcoo.csv")
write.csv(estimation, "data/estimation.csv")

##############################################################################
### Second one with case size in addition

cat("Trim weight for Asylum, Origin & Case size\n")

data.svy.rake.trim2 <- trimWeights(data.svy.rake.ctr.coo.size,
                                  lower = 0.3,
                                  upper = 3,
                                  strict = TRUE)

proportion.trim2 <- svyby(  ~ group_intro.goingback,  by =  ~ ctr+COO_L1+Case.size,  design = data.svy.rake.trim2,  FUN = svymean)

#names(proportion.trim)
proportion.trim2$key2 <- paste(proportion.trim2$ctr,proportion.trim2$COO_L1,proportion.trim2$Case.size,sep="-")

## Create estimation
cat("Create estimation based on Asylum, Origin & Case size\n")
universe.cool1.ctr.size <-  as.data.frame(table(universe$key2))
names(universe.cool1.ctr.size)[1]<-"key2"

estimation2 <- join(x=universe.cool1.ctr.size, y=proportion.trim2, by="key2",type="left")

estimation2$numyes <- estimation2$Freq * estimation2$group_intro.goingbackYes
estimation2$numUndecided <- estimation2$Freq * estimation2$group_intro.goingbackUndecided

estimation2$numyesind <- estimation2$numyes
estimation2$numUndecidedind  <- estimation2$numUndecided

estimation2$numyesind[estimation2$Case.size=="Case.size.2"] <- estimation2$numyes * 2
estimation2$numUndecidedind[estimation2$Case.size=="Case.size.2"]  <- estimation2$numUndecided * 2

estimation2$numyesind[estimation2$Case.size=="Case.size.3.to.5"] <- estimation2$numyes * 4
estimation2$numUndecidedind[estimation2$Case.size=="Case.size.3.to.5"]  <- estimation2$numUndecided * 4

estimation2$numyesind[estimation2$Case.size=="Case.size.6.and.more"] <- estimation2$numyes * 7
estimation2$numUndecidedind[estimation2$Case.size=="Case.size.6.and.more"]  <- estimation2$numUndecided * 7


### Aggregate info
names(estimation2)
estimation2.less <- estimation2[ ,c( "ctr" ,  "COO_L1","Case.size", "Freq", "numyes","numyesind",  "numUndecided" , "numUndecidedind" )]

estimation2.less[] <- lapply(estimation2.less, function(x) type.convert(as.character(x)))

#str(estimation1)
estimation2.coo <-aggregate(. ~ COO_L1, estimation2.less, sum)

write.csv(estimation2.coo, "data/estimation2coo.csv")
write.csv(estimation2, "data/estimation2.csv")



# names(data) -- Proportion by period
#proportion.trim.when <- svyby(  ~ group_intro.returnwhen,  by =  ~ ctr+COO_L1,  design = data.svy.rake.trim,  FUN = svymean)
#proportion.trim.when <- svyby(  ~ group_intro.returnwhen,  by =  ~ ctr,  design = data.svy.rake.trim,  FUN = svymean)

################################################
## A few test on return when

mean.goingwhen <- as.data.frame(svymean( ~ group_intro.returnwhen, design = data.svy.rake.trim))

prop.table(table(data$group_intro.returnwhen))

###################################################
### Compile a table to show impact of wieighting approach

cat("Compile a matrix of comparison between weighting\n")
compare.weight <- t(
  cbind(
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.unweighted,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = survey.data,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.ctr,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.ctr.coo,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.ctr.coo.size,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.coo,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.trim,
      FUN = svymean
    ),
    svyby(
      ~ group_intro.goingback,
      by =  ~ ctr,
      design = data.svy.rake.trim2,
      FUN = svymean
    )))





###################################################################
### Apply a series of function from Survey package

## The survey package provides a number of svy* functions that are similar to built in R functions,
## but account for survey design features.

# Calculate a mean, with the correct Sampling Error (including the fpc):
mean.goingback  <- as.data.frame(svymean( ~ group_intro.goingback, design = data.svy.rake.trim))
mean.goingback2 <- as.data.frame(prop.table(svytable( ~ group_intro.goingback, design = data.svy.rake.trim)))
rm(mean.goingback,mean.goingback2)
# Compute confidence intervals
confidence.goingback <- as.data.frame(confint(svymean( ~ group_intro.goingback,
                                                       by =  ~ ctr + COO_L1, design = data.svy.rake.trim2)))



#################################################
## clean
rm(goingback.ctr, proportion, proportion.trim,universe.cool1.ctr,universe.cool1.ctr.size, compare.weight,
   estimation, estimation2, mean.goingwhen, proportion.trim2, universe.COO_L1,
   universe.ctr, universe.key2, universe.key, universe.temp, cool1cat, ctr, N, n, confidence.goingback)


# Totals and proportions
#svytotal( ~ group_intro.goingback, design = data.svy.rake.trim)

# Calculate subgroup statistics with the correct Sampling Error’s
#goingback.ctr <-  as.data.frame(svyby( ~ group_intro.goingback,  by =  ~ ctr+COO_L1, design = data.svy.rake.trim, FUN = svymean))

# Fitting regression models
#linear.reg = svyglm(salary ~ gpa + female + class, design = des)
#summary(linear.reg)
#plot(linear.reg)

# For logistic regression: first transform the response into a numeric variable taking values 0 or 1,
## and specify family=quasibinomial() (this is essentially the same as using family=binomial in glm but avoids pesky warnings):

#logistic.reg = svyglm(as.numeric(internship=="Yes") ~ female + college,
#                      design = des,
#                      family=quasibinomial())
#summary(logistic.reg)


