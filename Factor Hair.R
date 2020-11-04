###Installing packages###
library(lattice)
library(ggplot2)
library(MASS)
library(dplyr)
library(car)
library(nFactors)
library(corrplot)
library(psych)
library(kableExtra)
library(boot)

###1.1&1.2 EDA - Basic data summary, Univariate, Bivariate analysis, graphs###
setwd("F:/GREAT LEARNING/ADVANCED STATISTICS/Project - Advanced Statistics - Factor Hair Revised/Project - Factor Hair - GL solution")
getwd()
mydata=read.csv("Factor-Hair-Revised.csv", header = TRUE)
attach(mydata)
summary(mydata)
str(mydata)
dim(mydata)
names(mydata)
rithu=mydata[,2:12]
rithu

###boxplot###
hairboxplot=boxplot(mydata[,-1],las=2)


variables = c("ProdQual" , "Ecom" , "TechSup" , "CompRes" , 
              "Advertising" , "ProdLine" , "SalesFImage", "ComPricing" , 
              "WartyClaim" , "OrdBilling" , "DelSpeed" , "Satisfaction")

## Hist for the independent Variables
#Convert Plotting space in 12
par(mfrow = c(2,6)) 
for (i in (1:12)) {
  h = round(max(rithu[,i]),0)+1
  l = round(min(rithu[,i]),0)-1
  n = variables[i]
  hist (rithu[,i],breaks = seq(l,h,((h-l)/6)), labels = T,
        include.lowest=T, right=T, 
        col=8, border=1, 
        main = NULL, xlab= n, ylab=NULL, 
        cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
        xlim = c(0,11), ylim = c(0,70))
}


hist (Satisfaction, breaks = c(0:11), labels = T,
      include.lowest=T, right=T, 
      col=8, border=1, 
      main = paste("Histogram of Customer Satisfaction"),
      xlab= "Satisfaction", ylab="COUNT", 
      xlim = c(0,11), ylim = c(0,35))

par(mfrow = c(2,5))
for (i in c(1:11)) {
  plot(rithu[,i],Satisfaction, 
       xlab = variables[i], ylab = NULL, col = "red", 
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
       xlim = c(0,10),ylim = c(0,10))
  abline(lm(formula = Satisfaction ~ rithu[,i]),col = "blue")
}

boxplot(Satisfaction, horizontal = T, xlab = variables[12], ylim=c(0,11))

####correlation
corrplot(cor(rithu),method ="number",type="upper")

####Outliers
list("OutLiers")
OutLiers <- rithu[(1:12),]
for (i in c(1:12)) {
  
  Box_Plot <- boxplot(rithu[,i],plot = F)$out
  OutLiers[,i] <- NA
  
  if (length(Box_Plot)>0) {
    OutLiers[(1:length(Box_Plot)),i] <- Box_Plot 
  }
}

summary(rithu)

OutLiers = OutLiers[(1:6),]
write.csv(OutLiers, "OutLiers.csv")

####Missing Value
sum(is.na(rithu))

####Summary of the dataset
summary(rithu)


###2. Check for Multicollinearity - Plot the graph based on Multicollinearity###
com1=lm(Satisfaction~.,data = rithu)
vif(com1)

###3. Simple Linear Regression (with every variable)###
lm.Prodqual=lm(Satisfaction~ProdQual,rithu)
lm.Prodqual

lm.Ecom=lm(Satisfaction~Ecom,rithu)
lm.Ecom

lm.Techsup=lm(Satisfaction~TechSup,rithu)
lm.Techsup

lm.compres=lm(Satisfaction~CompRes,rithu)
lm.compres

lm.Advertising=lm(Satisfaction~Advertising,rithu)
lm.Advertising

lm.Prodline=lm(Satisfaction~ProdLine,rithu)
lm.Prodline

lm.SalesFImage=lm(Satisfaction~SalesFImage,rithu)
lm.SalesFImage

lm.Compricing=lm(Satisfaction~ComPricing)
lm.Compricing

lm.WartyClaim=lm(Satisfaction~WartyClaim,rithu)
lm.WartyClaim

lm.OrdBilling=lm(Satisfaction~OrdBilling,rithu)
lm.OrdBilling

lm.DelSpeed=lm(Satisfaction~DelSpeed,rithu)
lm.DelSpeed

###4.1 Perform PCA/FA and Interpret the Eigen Values (apply Kaiser Normalization Rule)###
###Eigen valve computation###
corlnMatrix=cor(rithu[,-12])
corlnMatrix

cortest.bartlett(corlnMatrix,100)

A=eigen(corlnMatrix)
EV=A$values
EV

###4.2 Output Interpretation Tell why only 4 factors are being asked in the questions and tell whether it is correct in choosing 4 factors. Name the factors with correct explanations.###
library(psych)
unrotate=principal(rithu,nfactors = 4,rotate = "none")
print(unrotate,digits = 4)
unrotatedprofile=plot(unrotate,row.names(unrotate$loadings))
rotate=principal(rithu,nfactors=4,rotate="varimax")
print(rotate,digits=4)
rotatedprofile=plot(rotate,row.names(rotate$loadings),cex=1.0)



plot(EV, main = "Scree Plot", xlab = "Factors", ylab = "Eigen Values", pch = 20, col = "blue")
lines(EV, col = "red")
abline(h = 1, col = "green", lty = 2)

FourFactor1 = fa(r= rithu[,-12], nfactors =4, rotate ="none", fm ="pa")
print(FourFactor1)

hair.corr2=rithu[,1:11]
class(hair.corr2)
hair.corr2

rithu.pca=principal(hair.corr2,nfactors = 4,rotate = "varimax")
print(rithu.pca$loadings, cutoff = 0.6)

scores=round(rithu.pca$scores,2)
scores
as.data.frame(scores)

###5.1 Create a data frame with a minimum of 5 columns, 4 of which are different factors and the 5th column is Customer Satisfaction###

colnames(scores)=c("Pchexp","Bdrecog","Aftsvc","Prodt")
print(head(scores))

####5.2Perform Multiple Linear Regression with Customer Satisfaction as the Dependent Variable and the four factors as Independent Variables###
hair_s=mydata%>%select(c("ID","Satisfaction"))
hair_new=cbind(hair_s,scores)
print(head(hair_new))

###5.3MLR summary interpretation and significance (R, R2, Adjusted R2,Degrees of Freedom, f-statistic, coefficients along with p-values)###
m.linear.RegModel = lm(Satisfaction ~ Pchexp + Bdrecog + Aftsvc + Prodt, hair_new)
summary(m.linear.RegModel)

###5.4 Output Interpretation <making it meaningful for everybody>###
pred.Satisfn = predict(m.linear.RegModel)
as.data.frame(pred.Satisfn)

hair_new = cbind(hair_new, pred.Satisfn)
hair_new$pred.Satisfn = round(hair_new$pred.Satisfn, 1)
print(head(hair_new))


####Predicted vs Actual plot
plot(hair_new$Satisfaction, col="blue", xlab = "Data points", 
     ylab = "Satisfacton score",
     type = "b", cex = 1, pch = 21, bg = "blue")
lines(hair_new$pred.Satisfn, col= "red", type = "b")
text(28, 9.9, "Actual value", col = "blue")
text(14.5, 9, "Predicted value", col ="red")



####COMPLETE>>>THANK YOU

