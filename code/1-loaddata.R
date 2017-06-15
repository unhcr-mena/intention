rm(list = ls())
################################################################
## Load all required packages
mainDir <- getwd()
source(paste0(mainDir,"/code/0-packages.R"))

#source("code/0-packages.R")
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

data.available <- dataall[dataall$group_intro.answer== "Available", c (
  "refugeenumber", "group_intro.answer", 
  "group_intro.goingback", "group_intro.returnwhen", 
  "reasonnot.propertydoc", "reasonnot.well_settled", "reasonnot.displacedcommunity", "reasonnot.fearsretaliation", 
  "reasonnot.resettlement", "reasonnot.no_educ", "reasonnot.fears", "reasonnot.resttleinterest", 
  "reasonnot.nosaving", "reasonnot.familydecision", "reasonnot.fearsdetention", "reasonnot.registerchild", 
  "reasonnot.livelihood", "reasonnot.avoid_military", "reasonnot.no_medservice", "reasonnot.spec_need", 
  "reasonnot.bad_secu", "reasonnot.lackshelter", "reasonnot.civildoc", "reasonnot.nowaterservice",  "reasonnot.noelect", 
  
  "reasonundecided.access_to_school", "reasonundecided.electrictiy", 
  "reasonundecided.amnesty", "reasonundecided.wait_resttle", "reasonundecided.shelter", "reasonundecided.political", 
  "reasonundecided.familydecision", "reasonundecided.community", "reasonundecided.livelihood", "reasonundecided.water", 
  "reasonundecided.wait_school", "reasonundecided.health", "reasonundecided.reliableinfo", "reasonundecided.civildoc", 
  "reasonundecided.spec_need", "reasonundecided.security", "reasonundecided.desert", "reasonundecided.wait_info", 
  
  "reasons.security", "reasons.political", "reasons.work", 
  "reasons.no_service", "reasons.shelter", "reasons.property", "reasons.educ", 
  "reasons.assit_return", "reasons.avoid_military", "reasons.recruitment", "reasons.specific_need", 
  "reasons.resettlement_pipe", "reasons.well_settled", "reasons.wait_info", "reasons.wait_school", 
  "reasons.family_reunifi", "reasons.no_saving", "reasons.expired_residency", "reasons.familyreturn", 
  "reasons.Communit", "reasons.problems", "reasons.docu_from_syria", "reasons.land_property", 
  
  "grouppriority.first", "grouppriority.second", "grouppriority.third", 
  
  "return.tempvisit", "return.tempvisitret", "return.returnhow", "return.retunrcommunity", "return.returnwher", 
  
  "context.familysyria", "context.syriaproperty", "context.alternateplace", "context.timevisitedsyria", 
  "context.documentsyria", "context.havedoc", "context.authoritysyria", "context.authoritysyria.unhcr", 
  "context.authoritysyria.host", "context.authoritysyria.syrian", "context.moi")]

data.notavailable <- dataall[dataall$group_intro.answer!= "Available",c ( "refugeenumber", "group_intro.answer", 
  "group_intro.goingback", "group_intro.returnwhen") ] 

## Check that we donot have duplicate Response for the same case
#uniquecase <- unique(data.available$refugeenumber)
#uniquecase2 <- unique(samplefull$refugeenumber)

data.sample <- merge(x= samplefull,y=data.available,  by="refugeenumber", all.y=TRUE)
## Need to handle replacement -- i.e. case that were not in the inital sample

## taking them out of rtesting
data.sample1 <- data.sample
data.sample <- data.sample[ !(is.na(data.sample$size)), ]
levels(as.factor(data.sample$size))

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
#data <- kobo_encode(dataall, dico)
data <- kobo_label(data, dico)


##### A few check on the data
## it seems that all factors are disorganised
for (i in 1:133){
  data[ , i] <- as.character(data[ , i])
  #levels(as.factor(data$group_intro.goingback))
}

rm(dataall, data.available, data.available2, data.notavailable, data.notavailable2, data.sample, data.sample1, samplefull, i, mainDir)

write.csv(data,"data/datafinal.csv", row.names = FALSE)
