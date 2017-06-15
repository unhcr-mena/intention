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

universe <-
  progrescase[progrescase$CountryAsylum %in% c("ARE", "IRQ", "JOR"), ]
N = nrow(universe)
n = nrow(data)

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
# We need first to recategorise
cool1cat <- levels(as.factor(data$COO_L1))
universe$COO_L1 <- as.character(universe$cool1)
universe$COO_L1[!(universe$cool1 %in% cool1cat)] <- "Other"

## Fix specific case with low occurence
universe$COO_L1[(universe$cool1 %in% c("Dara","Hassakeh", "Idleb")) & universe$cool1=="Egypt"] <- "Other"
universe$COO_L1[(universe$cool1 %in% c("Dara","Homs", "Idleb","Rural Damascus")) & universe$cool1=="Iraq"] <- "Other"
universe$COO_L1[(universe$cool1 %in% c("Hassakeh")) & universe$cool1=="Jordan"] <- "Other"

universe.COO_L1 <- as.data.frame(table(universe$COO_L1))
names(universe.COO_L1)[1] <- "COO_L1"




## create the unweighted survey object
data.svy.unweighted <- svydesign(ids =  ~ 1,
                                 data = data)

## Post stratify on those relative frequency

## Try post stratification on ctr and on area of Origin
data.svy.rake.ctr.coo <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ ctr,  ~ COO_L1),
  population.margins = list(universe.ctr, universe.COO_L1)
)

## Try post stratification only on area of Origin
data.svy.rake.coo <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ COO_L1),
  population.margins = list(universe.COO_L1)
)

## Try post stratification only on ctr of asylum
data.svy.rake.ctr <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ ctr),
  population.margins = list(universe.ctr)
)

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
      design = data.svy.rake.ctr.coo,
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
    design = data.svy.rake.coo,
    FUN = svymean
  )))


prop.table(table(data$ctr, data$group_intro.goingback),1)
svyby(  ~ group_intro.goingback,  by =  ~ ctr,  design = data.svy.rake.ctr.coo,  FUN = svymean)

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
## Perhaps that one person that now counts as 50 is somewhat deranged, or otherwise not representative. So it is best to keep an eye on your weights.

summary(weights(data.svy.rake.ctr.coo))
data.svy.rake.trim <- trimWeights(data.svy.rake.ctr.coo,
                                  lower = 0.3,
                                  upper = 3,
                                  strict = TRUE)

proportion.trim <- svyby(  ~ group_intro.goingback,  by =  ~ ctr+COO_L1,  design = data.svy.rake.trim,  FUN = svymean)
#names(proportion.trim)
proportion.trim$key <- paste(proportion.trim$ctr,proportion.trim$COO_L1,sep="-")

## Get orginal # from universe
universe.cool1.ctr <-  as.data.frame(table(universe$COO_L1,universe$ctr))
universe.cool1.ctr$key <- paste(universe.cool1.ctr$Var2,universe.cool1.ctr$Var1,sep="-")

estimation <- join(x=universe.cool1.ctr, y=proportion.trim, by="key",type="left")

###################################################################
### Apply a series of function from Survey package

## The survey package provides a number of svy* functions that are similar to built in R functions,
## but account for survey design features.

# Calculate a mean, with the correct Sampling Error (including the fpc):
mean.goingback <-
  as.data.frame(svymean( ~ group_intro.goingback, design = data.svy.rake.trim))
mean.goingback2 <-
  as.data.frame(prop.table(svytable( ~ group_intro.goingback, design = data.svy.rake.trim)))

# Compute confidence intervals
confidence.goingback <-
  as.data.frame(confint(svymean( ~ group_intro.goingback, design = data.svy.rake.trim)))

# Totals and proportions
svytotal( ~ group_intro.goingback, design = data.svy.rake.trim)

# Calculate subgroup statistics with the correct Sampling Error’s
goingback.ctr <-
  as.data.frame(svyby(
    ~ group_intro.goingback,
    by =  ~ ctr,
    design = data.svy.rake.trim,
    FUN = svymean
  ))

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


###################################################################
### Plotting

### 3 basic strategies for making plots with weighted survey data:

## - Compute weighted summary statistics & plot those (histograms, barplots, …)
## - Resample the data with probability proportional to the weights
## - In scatterplots, make point size proportional to weights

## Boxplot
#svyboxplot(salary~1, ps.des, main="Boxplot")

## Break these out by groups by adding variables to the right-hand side of the formula
#svyboxplot(salary~college, design = des, main="Boxplot ", ylim=c(50000,190000))

## Scatterplot - 2 variables
#svyplot(salary~gpa, design = ps.des, main="Scatterplot ", ylim=c(50000,190000))

## Barplots for counts/proportions

barplot(svymean( ~ group_intro.goingback, design = data.svy.rake.trim),
        main = "Barplot ")

## Barplots wth 2 variables

barplot(
  svyby(
    ~ group_intro.goingback,
    by =  ~ ctr,
    design = data.svy.rake.trim,
    FUN = svymean
  ),
  legend.text = TRUE
)

barplot(
  svyby(
    ~ group_intro.goingback,
    by =  ~ COO_L1,
    design = data.svy.rake.trim,
    FUN = svymean
  ),
  legend.text = TRUE
)


# Now check a few plots wit ggpubr
## https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/
install.packages("ggpubr")


ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se")+
  stat_compare_means() +                                         # Global p-value
  stat_compare_means(ref.group = "0.5", label = "p.signif",
                     label.y = c(22, 29))  
