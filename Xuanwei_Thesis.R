# - - - - - - - - - - - - - - - - - - - - - #
# Packages Needed to Complete this Analysis #
# - - - - - - - - - - - - - - - - - - - - - #

#packages <- c("doBy", "psych", "beswarm")
#library(packages)
#Import each origin .csv file excluding unnessesary rows and columns and export each as 'cleaned' dataset SECTION ON HOLD####
##Something along the lines of Brian High's Example

##Upload .CSV export csv with headers and units corrected SECTION ON HOLD####
###IMport and change headers
####From Surface
NP <- read.csv("C:/Users/KGH/OneDrive/Documents/Xuanwei/Files/Results/NP/NPAH_Final_Summary.csv")
names(NP) <- c("name","filterID", "sampletype", "np_2np_pg", "np _2nfl_pg", "np_1np_pg", "np_2np_flag",
               "np_2nfl_flag", "np_1np_flag")
names(NP)
View(NP)

####From UW
NP_pg <- read.csv("C:/Users/khartin/Desktop/NPAH_Final_Summary.csv") #Temporary need to finalize
names(NP) <- c("name","filterID", "sampletype", "np_2np_pg", "np _2nfl_pg", "np_1np_pg", "np_2np_flag",
               "np_2nfl_flag", "np_1np_flag")
names(NP)
View(NP)

NP_ug

###Correct COncentration

#Merge files into single dataset by filter ID
##Not sure how yet, something like coursera example


#Standardize units - CURRENTLY CALCULATED IN EXCEL BEFORE EXPOSRTING
##Apply same folrmulae as in Excel workbook

##Check mass balance

###Produce Table and Plot

#Data reduction

##Average concentrations by site - Arithmetic mean

## Reduce number of metals to those with at least 10 greater than 2*Unc####

grepl("Unc",names(X))
metals.unc <- names(X)[grepl("Unc",names(X))]

v<-vector()

for(i in 1:length(names(X))){
  if(grepl("Unc",names(X)[i])){
    if(sum(X[,i-1]>X[,i]*2)>10){
      v<-c(v,paste(names(X)[i-1],"V",sep=""))
    }
  }
}  
##For names with "ConcV"
#grepl("Conc",names(X))
#var.names <- names(X)[grepl("ConcV",names(X))] ##Update column names

#for loop AM of sites

v<-c(v,names(X)[grepl("ugV",names(X))])

this.formula <- as.character("conc~location + sitenum")  
for (i in v){
  this.formula <- paste(i,this.formula, sep = "+")
}

#Load dataset, exclude Xuanwei, view table - CURRENT WORKFLOW BELOW####
##file_locationUW <- C("H:/Seto_Projects/Xuanwei/Xuanwei_ForAnalysis_31Mar15.csv") ##From UW
#file_locationPC <- c("C:/Users/KGH/OneDrive/Documents/Xuanwei/Xuanwei_ForAnalysis_31Mar15.csv") ##From Surface
file_locationPC2 <- c("C:/Users/KGH/OneDrive/Documents/Xuanwei/Xuanwei_ForAnalysis_Test.csv") ##From Surface
##Xuanwei_ForAnalysis_31Mar15 <- read.csv(file_locationUW)
#Xuanwei_ForAnalysis_31Mar15 <- read.csv(file_locationPC)
#X<-subset(Xuanwei_ForAnalysis_31Mar15, village != "Xuanwei")
Xuanwei_ForAnalysis <- read.csv(file_locationPC2)
X2<-subset(Xuanwei_ForAnalysis, village != "Xuanwei")

#Count number of observations per location type #Not sure if necessary anymore####
#table(X$location)
#with(X,table(Concentration = conc,  sitenum))

#Summary By ####
#install.packages("doBy")
library(doBy)
##Subset data by location and create mean of concentrations

#Create new objects, one for Home and one for Ambient
##Create new objects -> Mean of sites, points for plotting by location ####
with(subset(X, location == "Home" & sitenum == 1), summary(conc)) #summaryBy way easuer
am.site <- summaryBy(conc ~ sitenum + location + village, data=X2, id = NULL, keep.names=TRUE, FUN=mean)
am.site <- droplevels(am.site)
am.in.points <- subset(am.site, location == "Home")
am.out.points <- subset(am.site, location == "Ambient")
##Points excluding the outlier sites 2,8####
am.site.no.2.8 <- subset(am.site, sitenum != 2 & sitenum != 8)
am.in.points.no.2.8 <- subset(am.site.no.2.8, location == "Home")
am.out.points.no.2.8 <- subset(am.site.no.2.8, location == "Ambient")


summaryBy(conc ~ sitenum + location, data=X, id = NULL, keep.names=TRUE, FUN=mean)
summaryBy(2NP_ugV ~ sitenum + location, data=X, id = NULL, keep.names=TRUE, FUN=mean)
summaryBy( X2NP_ugV + X2NFL_ugV  +  X1NP_ugV  ~ sitenum + location, data=X2, id = NULL, keep.names=TRUE, FUN=mean, na.rm = TRUE)
describeBy(X$conc, list(X$location, X$burn))
describeBy(X$conc, list(X$location,X$sitenum))


##Plots of Conc by Site Differentiating Indoor and Outdoor 

#Plot Arithmetic Mean (AM) of Indoor and Dup Samples by Site Showing NAAQS 24hr (35 ug/m3)####
plot(conc~sitenum, data = am.site, type="n",log="y",ylab = expression(paste("Ambient Conc ",mu,"g/m^3","",sep ="")), xlab="",main="") #Why cannot control y range with ylim=c(0, 2000)?
points(am.in.points[,"conc"],col=1,pch=19)
points(am.out.points[,"conc"],col=8,pch=15)
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor","Ambient","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

#Boxplot of AM of Indoor and Dup Samples by Village Showing NAAQS 24hr (35 ug/m3)####

#install.packages('beeswarm')
library(beeswarm)

boxplot(conc~village, data=am.site, pch=19, ylab=expression(paste("Ambient Conc ",mu,"g/m^3","",sep ="")), main="", 
        log="y", ylim=c(20,2000))
beeswarm(conc~village, data=am.site, pch=19, pwcol=ifelse(location=="Home",1,8), log="TRUE", 
         add=TRUE, ylim=c(20,2000))
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor", "Ambient","NAAQS 35 ug/m^3"), col=c(1,8), pch=c(19,15,NA),lty=c(NA,NA,4))

#Scatter plot of indoor vs outdoor####
##All sites
lm_byloc <- lm(am.out.points[,"conc"]~am.in.points[,"conc"])
###Summary details
cor(am.in.points[,"conc"],am.out.points[,"conc"])
[1] 0.09176041
attributes(lm_byloc)
lm_byloc$coefficients
y = 0.0009577x + 24.9727
summary(lm_byloc)
Call:
    lm(formula = am.out.points[, "conc"] ~ am.in.points[, "conc"])

Residuals:
    Min     1Q Median     3Q    Max 
-8.641 -5.061 -1.980  4.424 15.829 

Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
(Intercept)            2.497e+01  3.004e+00   8.312 7.13e-05 ***
    am.in.points[, "conc"] 9.577e-04  3.928e-03   0.244    0.814    
---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 8.19 on 7 degrees of freedom
Multiple R-squared:  0.00842,    Adjusted R-squared:  -0.1332 
F-statistic: 0.05944 on 1 and 7 DF,  p-value: 0.8144
attributes(summary(lm_byloc.no28))
###Plot
plot(am.in.points[,"conc"], am.out.points[,"conc"], 
     main = expression(paste("Indoor vs Outdoor ",PM[2.5]," Conc", sep = "")),
     xlab = expression(paste("Indoor Conc ",mu,"g/",m^3,"",sep ="")), 
     ylab = expression(paste("Ambient Conc ",mu,"g/",m^3,"",sep ="")))
abline(lm_byloc, untf=TRUE, col=1) # regression line (y~x) 
legend("topright", bty="O", legend=paste("R2 is",format(summary(lm_byloc)$r.squared, digits=4)))

##Scatter plot of indoor vs outdoor Without outliers site 2 and 8####
lm_byloc.no28 <- lm(am.out.points.no.2.8[,"conc"]~am.in.points.no.2.8[,"conc"])
###Summary details
cor(am.in.points.no.2.8[,"conc"],am.out.points.no.2.8[,"conc"])
[1] 0.3542588
attributes(lm_byloc.no28)
lm_byloc.no28$coefficients
y = 0.1775105x + 16.7835245
summary(lm_byloc.no28)
attributes(summary(lm_byloc.no28))
Call:
    lm(formula = am.out.points.no.2.8[, "conc"] ~ am.in.points.no.2.8[,"conc"])

Residuals:
    1       2       3       4       5       6       7 
-0.2781 -2.8211 -2.7167 -4.5912 -4.3231  6.8829  7.8473 

Coefficients:
    Estimate Std. Error t value Pr(>|t|)  
(Intercept)                    16.7835     7.7478   2.166   0.0825 .
am.in.points.no.2.8[, "conc"]   0.1775     0.2096   0.847   0.4356  
---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 5.73 on 5 degrees of freedom
Multiple R-squared:  0.1255,    Adjusted R-squared:  -0.0494 
F-statistic: 0.7175 on 1 and 5 DF,  p-value: 0.4356

###Plot
plot(am.in.points.no.2.8[,"conc"], am.out.points.no.2.8[,"conc"], 
     main = expression(paste("Indoor vs Outdoor without Outliers ",PM[2.5]," Conc", sep = "")),
     xlab = expression(paste("Indoor Conc ",mu,"g/",m^3,"",sep ="")), 
     ylab = expression(paste("Ambient Conc ",mu,"g/m^3","",sep ="")))
abline(lm_byloc.no28, col=1) # regression line (y~x) 
legend("topright", bty="O", legend=paste("R2 is",format(summary(lm_byloc.no28)$r.squared, digits=4)))


#lines(lowess(gm_site$conc[gm_site$location=="Home"], gm_site$conc[gm_site$location=="Ambient"]),
      col="blue") # lowess line (x,y)



##Questionable code####
#detach(X)
#attach(X) #Try to refrain so no ambiguity or conflict after published
#Log of Concentrations
#logconc<-log(Xuanwei_Final$conc)
#gm = function(x, na.rm=TRUE){
  #exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
#}
#gm_site <- summaryBy(conc ~location + sitenum, data=X, id = "village", keep.names=TRUE, FUN=gm)
#avgsiteconce_home<- subset(X,location=="Home" & avgsiteconc!="na")
#avgsiteconce_ambient<- subset(Xuanwei_Final,village !="Xuanwei" & location=="Ambient" & avgsiteconc!="na")
#Plot arithmetic Mean of Indoor and Dup Samples Showing Mean and Median of Indoor and Ambient ####

plot(conc~sitenum, type="n",log="y",ylab="Concentration ug/m^3", xlab="Site Number",
     main="PM2.5 Concentration by Site") #Why cannot control y range with ylim=c(0, 2000)?
points(ho_avgmeans_x,col=1,pch=19)
points(am_avgmeans_x,col=8,pch=15)
legend("topright", c("Indoor", "Mean","Median","Ambient", "Mean", "Median"),col=c(1,1,1,8,8,8),
       pch=c(19,NA,NA,15,NA,NA),lty=c(NA,1,5,NA,3,4))
abline(h=mean(ho_avgmeans_x), lty=1, col=1)
abline(h=median(ho_avgmeans_x), lty=5, col=1)
abline(h=mean(am_avgmeans_x), lty=3, col=8)
abline(h=median(am_avgmeans_x), lty=4, col=8)

#Geometric Mean of Indoor and Dup Samples by Site Showing Geometric Mean and Median of Indoor and Ambient#### 

plot(conc~sitenum, type="n",log="y",ylab="Concentration ug/m^3", xlab="Site Number",
     main="PM2.5 Concentration by Site") #Why cannot control y range with ylim=c(0, 2000)?
points(ho_gmeans_x,col=1,pch=19)
points(am_gmeans_x,col=8,pch=15)
legend("topright", c("Indoor", "GM","Median","Ambient", "GM", "Median"),col=c(1,1,1,8,8,8),
       pch=c(19,NA,NA,15,NA,NA),lty=c(NA,1,5,NA,3,4))
abline(h=mean(ho_gmeans_x), lty=1, col=1)
abline(h=median(ho_gmeans_x), lty=5, col=1)
abline(h=mean(am_gmeans_x), lty=3, col=8)
abline(h=median(am_gmeans_x), lty=4, col=8)

#Plot of indoor/outdoor ratios
ratio_site <- summaryBy(conc~location + sitenum, data=gm_site, id = "village", keep.names=TRUE, FUN=gm)

##Scatter Plots of Conc
#Fancy Scatterplot
#install.packages('car')
#library(car)
#scatterplot(conc~sitenum | location, data=gm_site)