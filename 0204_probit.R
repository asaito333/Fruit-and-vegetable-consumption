#### PROBIT MODEL ####

library(aod)
setwd("/Users/asumi/Desktop/Rstudio/SNAP")
mydata<-read.csv("probit2011_7.csv")

## WITH CTYCODE1, BASIC INFO, W/O SMOKE ###
myprobit <- glm(fs ~ age18to24+age25to34+age35to44+age45to54+age55to64+divorced+widowed+separated+nevermarried+unmarriedcouple+elementary+somehs+hsged+somecollege+employed+selfemployed+unemployed+homemaker+student+unablework+male+black+asian+americanindian+hispanic+other+apache+cochise+coconino+gila+graham+greenlee+lapaz+mohave+navajo+pima+pinal+stcruz+yavapai+yuma+lesss10000+lesss15000+lesss20000+lesss25000+lesss35000+lesss50000+lesss75000, family=binomial(link="probit"), data=mydata)
result1<-as.(summary(myprobit))
out1 <- capture.output(summary(myprobit))


## WITH BASIC INFO, W/O CTYCODE1, SMOKE ##

# PROBIT
myprobit2 <- glm(fs ~ age18to24+age25to34+age35to44+age45to54+age55to64+divorced+widowed+separated+nevermarried+unmarriedcouple+elementary+somehs+hsged+somecollege+employed+selfemployed+unemployed+homemaker+student+unablework+male+black+asian+americanindian+hispanic+other+lesss10000+lesss15000+lesss20000+lesss25000+lesss35000+lesss50000+lesss75000, family=binomial(link="probit"), data=mydata)
out2 <- capture.output(summary(myprobit2))

# PREDICTED VALUE
out2.predict <- predict.glm(myprobit2, type = "response")
mydata$fshat<-ifelse(out2.predict>=0.50, 1, 0)
with(mydata, table(fshat,fs))


### CREATE SUBSET ###

mydata$group4<-ifelse(mydata$eligfs6==0&mydata$fs==1,1,0)
table(mydata$group4)
