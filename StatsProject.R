#Concrete

setwd("D:/DataSets")

library(readxl)

concrete <- read_excel("D:/DataSets/Concrete_Data.xls")
head(concrete)

dim(concrete)

n <- dim(concrete)[1] # num of observations
d <- dim(concrete)[2] # num of variables

names(concrete)

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Cement (component 1)(kg in a m^3 mixture)`
plot(x,y,xlab="Cement",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Blast Furnace Slag (component 2)(kg in a m^3 mixture)`
plot(x,y,xlab="Blast Furnace Slag",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Water  (component 4)(kg in a m^3 mixture)`
plot(x,y,xlab="Water",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Superplasticizer (component 5)(kg in a m^3 mixture)`
plot(x,y,xlab="Superplasticizer",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Coarse Aggregate  (component 6)(kg in a m^3 mixture)`
plot(x,y,xlab="Coarse Aggregate",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Fine Aggregate (component 7)(kg in a m^3 mixture)`
plot(x,y,xlab="Fine Aggregate",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Age (day)`
plot(x,y,xlab="Age (Days)",ylab="Concrete compressive strength")

y <- concrete$`Concrete compressive strength(MPa, megapascals)`
x <- concrete$`Concrete compressive strength(MPa, megapascals)`
plot(x,y,xlab="Concrete compressive strength",ylab="Concrete compressive strength")

fit.FineAggregate <- lm(`Concrete compressive strength(MPa, megapascals)`~`Fine Aggregate (component 7)(kg in a m^3 mixture)`,data=concrete)
fit.FineAggregate2 <- lm(`Concrete compressive strength(MPa, megapascals)`~`Fine Aggregate (component 7)(kg in a m^3 mixture)`+I(`Fine Aggregate (component 7)(kg in a m^3 mixture)`^2),data=concrete)

fit.cement <- lm(`Concrete compressive strength(MPa, megapascals)`~`Cement (component 1)(kg in a m^3 mixture)`,data=concrete)
fit.cement2 <- lm(`Concrete compressive strength(MPa, megapascals)`~`Cement (component 1)(kg in a m^3 mixture)`+I(`Cement (component 1)(kg in a m^3 mixture)`^2),data=concrete)


fit.water <- lm(`Concrete compressive strength(MPa, megapascals)`~`Water  (component 4)(kg in a m^3 mixture)`,data=concrete)
fit.water2 <- lm(`Concrete compressive strength(MPa, megapascals)`~`Water  (component 4)(kg in a m^3 mixture)`+I(`Water  (component 4)(kg in a m^3 mixture)`^2),data=concrete)

fit.super <- lm(`Concrete compressive strength(MPa, megapascals)`~`Superplasticizer (component 5)(kg in a m^3 mixture)`,data=concrete)
fit.super2 <- lm(`Concrete compressive strength(MPa, megapascals)`~`Superplasticizer (component 5)(kg in a m^3 mixture)`+I(`Superplasticizer (component 5)(kg in a m^3 mixture)`^2),data=concrete)

fit.coarse <- lm(`Concrete compressive strength(MPa, megapascals)`~`Coarse Aggregate  (component 6)(kg in a m^3 mixture)`,data=concrete)
fit.coarse2 <- lm(`Concrete compressive strength(MPa, megapascals)`~`Coarse Aggregate  (component 6)(kg in a m^3 mixture)`+I(`Coarse Aggregate  (component 6)(kg in a m^3 mixture)`^2),data=concrete)
 
AIC(fit.cement,fit.cement2,fit.water,fit.water2,fit.super,fit.super2,fit.coarse,fit.coarse2,fit.FineAggregate,fit.FineAggregate2)

summary(fit.cement)
summary(fit.cement2)

#Best Model



concrete_new <- read.csv("D:/DataSets/concrete.csv")
head(concrete_new)





fit.cement_new <- lm(strength~cement,data=concrete_new)
fit.cement2_new <- lm(strength~cement+I(cement+I(cement^2)),data=concrete_new)



plot(concrete_new$cement,concrete_new$strength)
abline(fit.cement_new)

x <- concrete_new$cement
xmesh <- seq(0.5*min(x),2*max(x),by=0.1)
yhat <- predict(fit.cement_new,newdata=data.frame(cement=xmesh))


lines(xmesh,yhat,col="red")
legend("topright", #location of legend
       c("Linear","Quadratic"), # names
       lty=c(1,1), # leave as 1 for each thing you're adding
       col=c("black","blue")) # colors

m2.1 <- lm(strength~cement+I(cement^2)+water+I(water^2)+superplastic+I(superplastic^2)+coarse+I(coarse^2)+fine+I(fine^2),data=concrete_new)
summary(m2.1)
AIC(m2.1)

m2.2 <- lm(strength~cement+I(cement^2)+water+I(water^2)+superplastic+I(superplastic^2),data=concrete_new)
summary(m2.2)
AIC(m2.2)
