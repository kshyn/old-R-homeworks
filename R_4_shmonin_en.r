# Report 4. Regression analysis
# Student Kirill Shmonin
# Department of soil geography. Option 52
#check the directory
getwd
# clear all memory
rm(list=ls())
#read data into rdat variable
rdat=read.csv("4data_52.csv", dec = ",",sep = ";")
rdat
# check dimension
dim(rdat)
# see the names
names(rdat)
## Output only one variable with yield
rdat$y
#CALCULATION OF CORRELATION COEFFICIENT
#Construct a matrix of range diagrams for all variables from the rdat object.
plot(rdat)

#calculation of correlation coefficients
cor(rdat, method="pearson")
library(ellipse)
plotcorr(cor(rdat))
plotcorr(cor(rdat), type="lower")
plotcorr(cor(rdat), type="upper")
#beautiful charts to visualize correlations
library(GGally)

ggpairs(rdat)
#REGRESSION IN R
#set model
model1 = lm(y~K, data = rdat); model1
#Brief information about the model
summary(model1)
#check the applicability of the model using analysis of variance
anova(model1)
#look what's in the model
str(model1)
#look at these objects
model1$coefficients[1]
model1$coefficients[2]
#construct a graph of the dependence of the yield on the content of mobile potassium
plot(rdat$y ~ rdat$K)
abline(a=model1$coefficients[1], b =model1$coefficients[2], col="red")
plot(model1)
#draw only a graph on normal probabilistic paper
plot(model1, 2)
#Construct a graph of observed values versus predicted values.
##Find the sum of the residuals for model 1.
str(model1)
#Output vector of predicted values and plot observables from predicted yield values
model1$fitted.values
# Plot Observables vs Predicted Yields
plot(model1$fitted.values,rdat$y)
#Add line y=x
abline(a=0, b=1, col="red")
#display leftovers
model1$residuals
#Find the sum of the balances
sum(model1$residuals)
# Plot residuals from observed yield values
plot( rdat$y, model1$residuals)
#to find the coefficients for the line, we will set a model that connects the residuals and the yield
mo1=lm(model1$residuals~rdat$y)
abline(a=mo1$coefficients[1], b =mo1$coefficients[2], col="red")
#same for multidimensional case
model2 = lm(y~K+N+h+P+pH, data = rdat)
model2
summary(model2)
anova(model2)
sum(model2$residuals)
plot(model2, 2)
plot(rdat$y,model2$fitted.values)
abline(a=0, b=1, col="blue")
plot( rdat$y, model2$residuals)
mo2=lm(model2$residuals~rdat$y)
abline(a=mo2$coefficients[1], b =mo2$coefficients[2], col="blue")
#do the same for significant variables only
model3 = lm(y~K+N+P+pH, data = rdat)
model3
summary(model3)
anova(model3)
sum(model3$residuals)
plot(model3, 2)
plot(rdat$y,model3$fitted.values)
abline(a=0, b=1, col="blue")
plot( rdat$y, model3$residuals)
mo3=lm(model3$residuals~rdat$y)
abline(a=mo3$coefficients[1], b =mo3$coefficients[2], col="blue")

