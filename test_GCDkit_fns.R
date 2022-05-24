#### Setup ####

setwd("D:/GitProjects/ggcdkit")
source("ggcdkit.R")

data(atacazo)
accessVar("atacazo")

## OR
data(sazava)
accessVar("sazava")

## OR
data(blatna)
accessVar("blatna")

#### Binary ####
#### Ternary ####

#### Templates ####
##### Classification #####
claslist <-   .claslist()
diagram.testing <- F ## Set by .claslist, which doesn't set it back to what it was !!
# (it's not very polite to set glob-variables and not return them to previous state...)

# Remove the formatting items
diags<-claslist[claslist[,2]!="NULL"&claslist[,3]=="FALSE",1]

# Run every diagram in existence...
x<-sapply(diags,function(i){    
  cat("Classification plot ", claslist[claslist[, 1] == i, 2], "\n", sep = "")
  if(substr(i,1,4)!="QAPF") ggplotDiagram(claslist[claslist[, 1] == i, 2])
})  


# OConnorPlut calls GUI (OCOnnorVolc doesn't)
# In fact it would be good to align the API for these two very similar diagrams !!
# For the moment OConnorPlut cannot be used in batch mode (or an extra arg is needed)
ggplotDiagram(OConnorPlut,calc.cipw=T) ## Ok
ggplotDiagram(OConnorPlut) ## Shows dialog

ggplotDiagram(OConnorVolc) ## Ok


##### Geotectonic #####
tectlist <-   .tectlist()
diagram.testing <- F ## Set by .tectlist, which doesn't set it back to what it was !!

# Remove the formatting items
diags<-tectlist[tectlist[,2]!="NULL"&tectlist[,3]=="FALSE",1]

# Run every diagram in existence...
x<-sapply(diags,function(i){    
  cat("Classification plot ", tectlist[tectlist[, 1] == i, 2], "\n", sep = "")
  if(substr(i,1,4)!="QAPF") ggplotDiagram(tectlist[tectlist[, 1] == i, 2])
})  


## Some of the plot fail - for instance, with data = blatna sylvester() does not work 
# because there is only one row that returns results.
# x.data and y.data should be calculated using drop=F
# ... probably for all functions !

##### User #####
.userlist <-   .userlist()
diagram.testing <- F ## Set by .userlist, which doesn't set it back to what it was !!

# Remove the formatting items
diags<-.userlist[.userlist[,2]!="NULL"&.userlist[,3]=="FALSE",1]

# Run every diagram in existence...
x<-sapply(diags,function(i){    
  cat("Classification plot ", .userlist[.userlist[, 1] == i, 2], "\n", sep = "")
  if(substr(i,1,4)!="QAPF") ggplotDiagram(.userlist[.userlist[, 1] == i, 2])
})  

