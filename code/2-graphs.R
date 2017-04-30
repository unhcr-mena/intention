#### Produce graphs
source("code/1-loaddata.R")

#########################################################################################
## Produce graphs of all select_one questions
kobo_bar_one(data,dico)

#########################################################################################
## Produce graphs of all select_multiple questions
kobo_bar_multi(data,dico)

########################################################################################
### Produce faceted chart select_one

kobo_bar_one_facet(data,dico)

