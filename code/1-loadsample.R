##################################################################
## load all samples
##################################################################

library(readr)

files <- as.data.frame(list.files("data/sample"))
sampleall <- data.frame(X1=integer(),
                        CaseNo=character(),
                        COO_L1=character(),
                        phone=integer(),
                        size=character(),
                        needs=integer(),
                        ID_unit=integer(),
                        Prob=double(),
                        Stratum=integer(),
                        ctr=character(),
                        cool1=character(),
                        stringsAsFactors=FALSE)

for (i in 1:nrow(files)){
  # i <- 1
  sample <- read_csv(paste0("data/sample/", as.character( files[i , 1] )))
  sample$ctr <- substring(as.character( files[i , 1] ),1,3)
  sample$cool1 <- substring(as.character( files[i , 1] ),4,nchar(as.character( files[i , 1] ))-4)
  #str(sample)
  cat(as.character( files[i , 1] ))
  sampleall <- rbind(sampleall,sample)
}

### Different format

files2 <- as.data.frame(list.files("data/sampleJOR"))
sampleall2 <- data.frame(X1=integer(),
                         CaseNo=character(),
                         COO_L1=character(),
                         phone=integer(),
                         CashList=integer(),
                         MobileNo=integer(),
                         COA_L1=character(),
                         size=character(),
                         needs=integer(),
                         ID_unit=integer(),
                         Prob=double(),
                         Stratum=integer(),
                         ctr=character(),
                         cool1=character(),
                         stringsAsFactors=FALSE)

for (i in 1:nrow(files2)){
  # i <- 1
  sample <- read_csv(paste0("data/sampleJOR/", as.character( files2[i , 1] )))
  sample$ctr <- substring(as.character( files2[i , 1] ),1,3)
  sample$cool1 <- substring(as.character( files2[i , 1] ),4,nchar(as.character( files2[i , 1] ))-4)
  #str(sample)
  cat(as.character( files2[i , 1] ))
  sampleall2 <- rbind(sampleall2,sample)
}


sampleall$CashList <-as.integer("")
sampleall$MobileNo <-as.integer("")
sampleall$COA_L1 <-as.character("")

sample <- rbind(sampleall,sampleall2)
rm(sampleall,sampleall2,files,files2, i)

## Rename column for mergin
sample <- sample[ ,c("CaseNo", "COO_L1", "size", "needs", "ID_unit", "Prob", "Stratum", "ctr",  "CashList")]

#names(sample)[names(sample)=="CaseNo"] <- "refugeenumber"


## Checking if ID_unit can be used as unique ID
#uniqueid <- unique(sample$ID_unit)

#levels(as.factor(sample$COO_L1))
#levels(as.factor(sample$size))
#levels(as.factor(sample$needs))
str(sample$needs)
sample$needs <- as.factor(sample$needs)

##loading  case profile from progres
progrescase <- read_csv("data/progrescase-1.csv")

## Check arrivalcategory

prop.table(table(progrescase$YearArrivalCategory, useNA = "ifany"))


samplefull <- merge(x=sample, y=progrescase, by="CaseNo", all.x=TRUE)

prop.table(table(samplefull$YearArrivalCategory, useNA = "ifany"))

samplefull[is.na(samplefull$YearArrivalCategory), c("CaseNo") ]

#samplefull$YearArrivalCategory[is.na(samplefull$YearArrivalCategory)] <- "noData"

samplefull <- samplefull[!(is.na(samplefull$YearArrivalCategory)), ] 
samplefull <- samplefull[samplefull$YearArrivalCategory != "noData", ] 

samplefull$YearArrivalCategory[samplefull$YearArrivalCategory=="2011.or.before.or.unkown" ] <- "2011.or.before"
samplefull$YearArrivalCategory[samplefull$YearArrivalCategory=="2016.and.2017" ] <- "2016.or.after"

levels(as.factor(samplefull$YearArrivalCategory))


prop.table(table(samplefull$YearArrivalCategory, useNA = "ifany"))

rm(progrescase)
#field <- as.data.frame(names(samplefull))
#write.csv(field, "data/field.csv")

## Retain information from proGres
samplefull <- samplefull[ , c("CaseNo",
                              "COO_L1",
                              "size",
                              "needs",
                              "Prob",
                              "Stratum",
                              "ctr",
                              "CashList",
                              "CountryOrigin",
                              "cool1",
                              "cool2",
                              "CountryAsylum",
                              "coal1",
                              "coal2",
                              "Num_Inds",
                              "AVG_Age",
                              "STDEV_Age",
                              "Montharrival",
                              "YearArrival",
                              "YearArrivalCategory",
                              "dem_relation",
                              "dem_age",
                              "dem_agegroup",
                              "dem_sex",
                              "RefStatCategory",
                              "dem_ethnCat",
                              "dem_religionCat",
                              "Case.size",
                              "dependency",
                              "female.ratio",
                              "agecohort",
                              "AVGAgecohort",
                              "STDEVAgeclass",
                              "edu_highestcat",
                              "occupationcat",
                              "dem_marriagecat",
                              "bir_syria",
                              "At.Risk",
                              "Child.Labour",
                              "Child.marriage..parent.or.pregnancy",
                              "Family.Needs",
                              "Marginalised",
                              "Medical",
                              "Need.of.Care",
                              "Problem.with.violence.law.recruitment",
                              "Separated.Child",
                              "Single.Parent",
                              "Unaccompanied",
                              "Victim.of.Violence",
                              "Woman.at.Risk")]


names(samplefull)[names(samplefull)=="CaseNo"] <- "refugeenumber"

samplefull$COO_L1[samplefull$COO_L1=="Rural Damascus" ] <- "Rural.Damascus"
levels(as.factor(samplefull$COO_L1))


samplefull$size[samplefull$size=="1"] <- "Case.size.1"              
samplefull$size[samplefull$size=="2"] <- "Case.size.2"    
samplefull$size[samplefull$size=="_3_5"] <- "Case.size.3.to.5"          
samplefull$size[samplefull$size=="5"] <- "Case.size.over.5"
levels(as.factor(samplefull$size))

samplefull$needs <- as.character(samplefull$needs)
samplefull$needs[samplefull$needs=="0"] <- "No.specific.needs"          
samplefull$needs[samplefull$needs=="1"] <- "Has.specific.needs"
levels(as.factor(samplefull$needs))

############################################################

write.csv(samplefull,"data/samplefull.csv", row.names = FALSE)

