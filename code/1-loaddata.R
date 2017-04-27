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
names(sample)[names(sample)=="CaseNo"] <- "refugeenumber"


sample <- sample[ ,c("refugeenumber", "COO_L1", "size", "needs",  "Prob", "Stratum", "ctr",  "CashList")]

#levels(as.factor(sample$COO_L1))
#levels(as.factor(sample$size))
#levels(as.factor(sample$needs))
str(sample$needs)
sample$needs <- as.factor(sample$needs)
############################################################
#                                                          #
#   Position your form & your data in the data folder  
#                                                          #
############################################################

rm(data)
library(readr)
data.or <- read_delim("data/data.csv",   ";", escape_double = FALSE, trim_ws = TRUE)
#data.or <- read.csv("data/data.csv", sep=";", encoding="UTF-8", na.strings="n/a")

### merge with sample
data.or <- merge(x= sample,y=data.or,  by="refugeenumber", all.y=TRUE)

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

data <- kobo_encode(data.or, dico)
data <- kobo_label(data, dico)
#########################################################################################
## Produce graphs of all select_one questions
kobo_bar_one(data,dico)

#########################################################################################
## Produce graphs of all select_multiple questions
kobo_bar_multi(data,dico)

########################################################################################
### Produce faceted chart select_one

kobo_bar_one_facet(data,dico)




