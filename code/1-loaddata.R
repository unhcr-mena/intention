rm(list = ls())
################################################################
## Load all required packages
source("code/0-packages.R")
library(koboloadeR)

## kobo_projectinit()
##################################################################
## load all samples
##################################################################

library(readr)
samplefull <- read_csv("data/samplefull.csv")

############################################################


rm(data)

data.iraq <- read_delim("data/iraq.csv",   ";", escape_double = FALSE, trim_ws = TRUE)
data.jordan <- read_delim("data/jordan.csv", ";", escape_double = FALSE, trim_ws = TRUE)
data.egypt <- read_delim("data/egypt.csv", ";", escape_double = FALSE, trim_ws = TRUE)

data.iraq$context.moi <- ""
data.egypt$context.moi <- ""

dataall <- rbind(data.iraq,data.jordan,data.egypt)
rm(data.iraq,data.jordan,data.egypt)
#names(dataall)
#levels(as.factor(dataall$group_intro.answer))

data.available <- dataall[dataall$group_intro.answer== "Available",]
data.notavailable <- dataall[dataall$group_intro.answer!= "Available",] 

## Check that we donot have duplicate Response for the same case
uniquecase <- unique(data.available$refugeenumber)
uniquecase2 <- unique(samplefull$refugeenumber)

data.sample <- merge(x= samplefull,y=data.available,  by="refugeenumber", all.y=TRUE)
## Need to handle replacement -- i.e. case that were not in the inital sample

## taking them out of rtesting
data.sample1 <- data.sample
data.sample <- data.sample[ !(is.na(data.sample$size), ]


data.available2 <- merge(x= samplefull, y=data.available,  by="refugeenumber", all.x=TRUE)
data.notavailable2 <- merge(x= samplefull, y=data.notavailable,  by="refugeenumber", all.x=TRUE)
#data.or <- read.csv("data/data.csv", sep=";", encoding="UTF-8", na.strings="n/a")

### merge with sample
#data.or <- merge(x= samplefull,y=dataall,  by="refugeenumber", all.y=TRUE)

##############################################
## Load form
rm(form)
form <- "form.xls"
## Generate & Load dictionnary
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep=""), encoding="UTF-8", na.strings="")
rm(form)


#################################################################################
##### Re-encode correctly the dataset

data <- kobo_encode(data.sample, dico)
data <- kobo_label(data, dico)



