#==========================================#
## Standarize Grocery Store               ##
#==========================================#

# Import data
grocery2011<-read.csv("/Users/asumi/Dropbox/RA_SNAP_Ed/2016 Fall/dataset/GroceryStores2011.csv")
grocery2013<-read.csv("/Users/asumi/Dropbox/RA_SNAP_Ed/2016 Fall/dataset/GroceryStores2013.csv")
grocery<-rbind(grocery2011, grocery2013)
grocery2011[1,]
grocery2013[1,]

# Standarize variable name to 2013
grocery2011$GEO.id<-grocery2011$Geographic.identifier.code
grocery2011$GEO.display.label<-grocery2011$Geographic.area.name
grocery2011$NAICS.id<-grocery2011$X2007.NAICS.code
grocery2011$NAICS.display.label<-grocery2011$Meaning.of.2007.NAICS.code
grocery2011$YEAR.id<-grocery2011$Year
grocery2011$EMPSZES.id<-grocery2011$Employment.size.of.establishment
grocery2011$EMPSZES.display.label<-grocery2011$Meaning.of.Employment.size.of.establishment
g2011.for.merge<-with(grocery2011, as.data.frame(cbind(GEO.id, Zip, GEO.display.label, NAICS.id, NAICS.display.label, YEAR.id, EMPSZES.id, EMPSZES.display.label, AllEstablishments,X1.4Employees, X5.9employees,X10.19Employees, X20.49employees, X50.99employees, X100.249employees, X250.499employees)
                    ))

# 2013 doesn't have "X250.499employees"
g2011.for.merge[1,]
grocery2013[1,]
summary(grocery2011$X250.499employees)
grocery2013[, (grocery2013$X1.4Employees==0&grocery2013$X5.9employees==0&grocery2013$X10.19Employees==0&grocery2013$X20.49employees==0&grocery2013$X50.99employees==0&grocery2013$X100.249employees==0)]
grocery2013$X250.499employees<-0

# Check if ther they are marigable 
grocery.merge<-as.data.frame(rbind(g2011.for.merge,grocery2013))

# OKAY!!


#==========================================#
## Standarize AZ Zip Population           ##
#==========================================#

# Import data
population2011<-read.csv("/Users/asumi/Dropbox/RA_SNAP_Ed/2016 Fall/dataset/Copy of AZ zip code population 2011.csv")
population2013<-read.csv("/Users/asumi/Dropbox/RA_SNAP_Ed/2016 Fall/dataset/Copy of AZ zip code population 2013.csv")
population2011[1,]
population2013[1,]

# Add year
population2011$YEAR.id<-2011
population2013$YEAR.id<-2013

#  Standarize variable name to 2013
population2011$zip<-population2011$Zip
pop2011.for.merge<-as.data.frame(with(population2011, cbind(Id, zip,Geography, Population, Margin.of.Error..Total, YEAR.id )))
pop2011.for.merge[1,]
population2013[1,]

# Check if ther they are marigable 
population.merge<-as.data.frame(rbind(pop2011.for.merge,population2013))


#==========================================#
## Standarize Orig + Econ + distance      ##
#==========================================#

# Import data
orig.econ.2011<-read.csv("/Users/asumi/Desktop/Rstudio/SNAP/orig.econ.2011.csv") #715 var
orig.econ.2013<-read.csv("/Users/asumi/Desktop/Rstudio/SNAP/orig.econ.2013.csv") #698 var

# Merge distance to orig.econ.2011 and orig.econ.2013
setwd("/Users/asumi/Desktop/Rstudio/SNAP")
des<-read.csv("DES_office_distance_comp.csv")
des$locator<-as.numeric(as.character(des$locator))
des$gmap<-as.numeric(as.character(des$gmap))
des$distance<-with(des, ifelse(is.na(locator), gmap, ifelse(locator>60, pmin(locator, gmap, na.rm=T), locator)))

# Match distance with orig.econ.2011/2013
orig.econ.2011$distance<-des$distance[match(orig.econ.2011$zip1, des$zip)]
orig.econ.2013$distance<-des$distance[match(orig.econ.2013$zip1, des$zip)]
summary(orig.econ.2011$distance)
summary(orig.econ.2013$distance)
summary(orig.econ.2013$zip1)

####Filling missing distance in 2013 ####
table(orig.econ.2013$aaz3_1, is.na(orig.econ.2013$distance))
# 279 missing in distance in 2013
missingzip.2013<-as.data.frame(table(subset(orig.econ.2013$zip1, is.na(orig.econ.2013$distance))))
write.csv(missingzip.2013, "missingzip2013.csv")
####DONE#####

names(orig.econ.2011)
names(orig.econ.2013)

## Select the variables for analysis and standarize them ##

# original variables + distance
o2011.test<-as.data.frame(with(orig.econ.2011, cbind(
  AGE,MARITAL,EDUCA,EMPLOY,INCOME2,SEX,SMOKDAY2,CTYCODE1,FOODSTAMP,FRLUNCH,WIC,
  X_FRUTSUM,X_VEGESUM,CHILDREN,NUMADULT,zip1,distance,X_STSTR,X_PSU,X_LLCPWT))) #20vars
o2013.test<-as.data.frame(with(orig.econ.2013, cbind(
  AGE,MARITAL,EDUCA,EMPLOY1,INCOME2,SEX,SMOKDAY2,CTYCODE1,aaz3_1,aaz3_2,aaz3_3,
  X_FRUTSUM,X_VEGESUM,CHILDREN,NUMADULT,zip1,distance,X_STSTR,X_PSU,X_LLCPWT))) #20vars

names(o2011.test)
names(o2013.test)


o2011.test$zip1<-as.numeric(as.character(o2011.test$zip1))

o2013.test$EMPLOY<-o2013.test$EMPLOY1
o2013.test$FOODSTAMP<-o2013.test$aaz3_1
o2013.test$WIC<-o2013.test$aaz3_2
o2013.test$FRLUNCH<-o2013.test$aaz3_3
o2013.test$zip1<-as.numeric(as.character(o2013.test$zip1))

names(o2011.test)
names(o2013.test)

o2011.test.merge<-with(o2011.test, as.data.frame(cbind(AGE,MARITAL,EDUCA,EMPLOY,INCOME2,SEX,SMOKDAY2,
CTYCODE1,FOODSTAMP,FRLUNCH,WIC,CHILDREN,NUMADULT,X_FRUTSUM,X_VEGESUM,zip1,distance,X_STSTR,X_PSU,X_LLCPWT))) #20var
o2013.test.merge<-with(o2013.test, as.data.frame(cbind(AGE,MARITAL,EDUCA,EMPLOY,INCOME2,SEX,SMOKDAY2,
CTYCODE1,FOODSTAMP,FRLUNCH,WIC,CHILDREN,NUMADULT,X_FRUTSUM,X_VEGESUM,zip1,distance,X_STSTR,X_PSU,X_LLCPWT))) #20var

names(o2011.test.merge)
names(o2013.test.merge)
# 20 variables are selected from original variables

# convert zip1 to numeric
class(o2011.test$zip1)
class(o2011.test.merge$zip1)
class(o2013.test.merge$zip1)
summary(o2013.test.merge$zip1)

# econ variables 
names(orig.econ.2011)
o2011.econ<-as.data.frame(orig.econ.2011[,707:714]) #8var
names(orig.econ.2013)
o2013.econ<-as.data.frame(orig.econ.2013[,608:615]) #8var
names(o2011.econ)
names(o2013.econ)
# 8 vaiables are selected from econ variables


# created variables
names(orig.econ.2011)
o2011.crtd<-as.data.frame(orig.econ.2011[,624:704]) #81var
names(orig.econ.2013)
o2013.crtd<-as.data.frame(orig.econ.2013[,617:698]) #82var
o2013.crtd2<-as.data.frame(o2013.crtd[,-38]) #81var
#81 variables are selected from created variables

o2011<-as.data.frame(cbind(o2011.test.merge,o2011.econ,o2011.crtd))
o2013<-as.data.frame(cbind(o2013.test.merge,o2013.econ,o2013.crtd2))

names(o2011)
names(o2013)

merged<-as.data.frame(rbind(o2011,o2013))
# now o2011 and o2013 are mergeable! #109variables


#===================================================#
## origecon(zip1) + grocery(Zip) + population(zip) ##
#===================================================#

# 2011: o2011, g2011.for.merge, pop2011.for.merge
# 2013: o2013, grocery2013, population2013

# Check the class of variables
class(o2011$zip1)
class(g2011.for.merge$Zip)
class(pop2011.for.merge$zip)
class(o2013$zip1)
class(grocery2013$Zip)
class(population2013$zip)

# Convert to numeric
g2011.for.merge$Zip<-as.numeric(as.character(g2011.for.merge$Zip))
o2013$zip1<-as.numeric(as.character(o2013$zip1))
grocery2013$Zip<-as.numeric(as.character(grocery2013$Zip))
population2013$zip<-as.numeric(as.character(population2013$zip))

# Summary
summary(o2011$zip1)
summary(g2011.for.merge$Zip)
summary(pop2011.for.merge$zip)
summary(o2013$zip1) #debug
summary(grocery2013$Zip)
summary(population2013$zip)

# TEST # sql #FAIL!!!!!
g2011_test<-g2011.for.merge
p2011_test<-pop2011.for.merge
library(sqldf)
sqldf("select*
      from p2011_test  left join g2011_test 
      on p2011_test.zip = g2011_test.Zip", 
      row.names = TRUE)  
# giveup


### Change the way of R<=>SAS ###
### import created csv
origecon2011<-read.csv("origecon2011.csv")
grocery2011<-read.csv("grocery2011.csv")
population2011<-read.csv("population2011.csv")
origecon2013<-mydata
grocery2013<-read.csv("grocery2013.csv")
population2013<-read.csv("population2013.csv")

### export in .dbf

library(foreign)
write.dbf(origecon2011,"origecon2011.dbf")
write.dbf(grocery2011,"grocery2011.dbf")
write.dbf(population2011, "population2011.dbf")
write.dbf(origecon2013, "origecon2013.dbf")
write.dbf(grocery2013, "grocery2013.dbf")
write.dbf(population2013, "population2013.dbf")


###======junk ========#####

write.csv(o2011,"origecon2011.csv")
write.csv(g2011.for.merge,"grocery2011.csv")
write.csv(pop2011.for.merge,"population2011.csv")
write.csv(o2013,"origecon2013.csv")
write.csv(grocery2013,"grocery2013.csv")
write.csv(population2013,"population2013.csv")


#=========================#
# Imported from SAS #
allmerge2011<-read.csv("allmerged2011.csv")
allmerge2013<-read.csv("allmerged2013.csv")

merged<-as.data.frame(rbind(allmerge2011,allmerge2013))



#==========================#
#junk

mydata4<-read.csv("origecon2013.csv")
summary(mydata$zip1)
mydata1<-read.csv("allmerged2013.csv")
summary(mydata1$zip1)
mydata2<-read.csv("allmerged2011.csv")
summary(mydata2$zip1)


mydata$zip1<-as.character(mydata$zip1)
mydata$zip1<-with(mydata, ifelse(is.na(zip1), zip1==".", zip1))

mydata$zip1[1:100, ]


mydata$zip1<-as.numeric(mydata$zip1)


install.packages("haven")
library(haven)

tmp <- tempfile(fileext = ".sas7bdat")
write_sas(mydata, tmp)

write_sas(mydata, "test.sas7bdat")


library(foreign)
write.dbf(mydata,"test2.dbf")
