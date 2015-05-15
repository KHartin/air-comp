 # - - - - - - - - - - - - - - - - - - - - - #
# Packages Needed to Complete this Analysis #
# - - - - - - - - - - - - - - - - - - - - - #

packages <- ("doBy", "psych", "beswarm")

#Import each origin .csv file excluding unnessesary rows and columns and export each as 'cleaned' dataset SECTION ON HOLD####
##Something along the lines of Brian High's Example

##Upload .CSV export csv with headers and units corrected SECTION ON HOILD####
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



## Reduce number of metals to those with at least 10 greater than 2*Unc

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
file_locationPC <- c("C:/Users/KGH/OneDrive/Documents/Xuanwei/Xuanwei_ForAnalysis_31Mar15.csv") ##From Surface
file_locationPC2 <- c("C:/Users/KGH/OneDrive/Documents/Xuanwei/Xuanwei_ForAnalysis_Test.csv") ##From Surface
##Xuanwei_ForAnalysis_31Mar15 <- read.csv(file_locationUW)
Xuanwei_ForAnalysis_31Mar15 <- read.csv(file_locationPC)
X<-subset(Xuanwei_ForAnalysis_31Mar15, village != "Xuanwei")
Xuanwei_ForAnalysis <- read.csv(file_locationPC2)
X2<-subset(Xuanwei_ForAnalysis, village != "Xuanwei")

#Count number of observations per location type #Not sure if necessary anymore
table(X$location)
with(X,table(Concentration = conc,  sitenum))

#Summary By ####
install.packages("doBy")
library(doBy)
##Subset data by location and create mean of concentratins
#Create new objects, one for Home and one for Ambient
##Create new objects -> Home, Ambient, Mean and Geometric Mean Home Concentrations ####
is.home <- subset(X, location == "Home")
is.ambient <- subset(X, location == "Ambient")
#home <- X[,"location"] #?
#Xuanwei_Final [is.ambient,] #? Ditto
with(subset(X, location == "Home" & sitenum == 1), summary(conc)) #summaryBy way easuer
am.site <- summaryBy(conc ~ sitenum + location + village, data=X, id = NULL, keep.names=TRUE, FUN=mean)
am.site
summaryBy(conc ~ sitenum + location, data=X, id = NULL, keep.names=TRUE, FUN=mean)
summaryBy(2NP_ugV ~ sitenum + location, data=X, id = NULL, keep.names=TRUE, FUN=mean)
summaryBy( X2NP_ugV + X2NFL_ugV  +  X1NP_ugV  ~ sitenum + location, data=X2, id = NULL, keep.names=TRUE, FUN=mean, na.rm = TRUE)
describeBy(X$conc, list(X$location, X$burn))
describeBy(X$conc, list(X$location,X$sitenum))


##Plots of Conc by Site Differentiating Indoor and Outdoor 

#Arithmetic Mean of Indoor and Dup Samples Showing Mean and Median of Indoor and Ambient ####

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

#Geometric Mean of Indoor and Dup Samples by Site Showing NAAQS 24hr (35 ug/m3)####
plot(conc~sitenum, type="n",log="y",ylab="Concentration ug/m^3", xlab="Site Number",
     main="PM2.5 Concentration by Site") #Why cannot control y range with ylim=c(0, 2000)?
points(ho_gmeans_x,col=1,pch=19)
points(am_gmeans_x,col=8,pch=15)
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor","Ambient","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

#GM of Indoor and Dup Samples by Village Showing NAAQS 24hr (35 ug/m3)####

install.packages('beeswarm')
library(beeswarm)

boxplot(conc~village, data=gm_mean_site, pch=19, ylab="Concentration ug/m^3", main="Conc by Village", 
        log="y", ylim=c(10,1000))
beeswarm(conc~village, data=gm_mean_site, pch=19, pwcol=ifelse(location=="Home",1,8), log="TRUE", 
         add=TRUE, ylim=c(10,1000))
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor", "Ambient","NAAQS 35 ug/m^3"), col=c(1,8), pch=c(19,15,NA),lty=c(NA,NA,4))

#Plot of indoor/outdoor ratios
ratio_site <- summaryBy(conc~location + sitenum, data=gm_site, id = "village", keep.names=TRUE, FUN=gm)

##Scatter Plots of Conc
#Fancy Scatterplot
#install.packages('car')
#library(car)
#scatterplot(conc~sitenum | location, data=gm_site)

#Scatter plot of log indoor vs outdoor ####

lm_byloc <- lm(gm_site$conc[gm_site$location=="Ambient"]~gm_site$conc[gm_site$location=="Home"])
plot(gm_site$conc[gm_site$location=="Home"], gm_site$conc[gm_site$location=="Ambient"], log = "xy", 
     main = expression(paste("Indoor vs Outdoor ",PM[2.5]," Conc", sep = "")),
     xlab = expression(paste("Indoor Conc ",mu,"g/",m^3,"",sep ="")), 
     ylab = expression(paste("Ambient Conc ",mu,"g/",m^3,"",sep ="")))
abline(lm_byloc, untf=TRUE, col=1) # regression line (y~x) 
legend("topright", bty="O", legend=paste("R2 is",format(summary(lm_byloc)$adj.r.squared, digits=4)))

#lines(lowess(gm_site$conc[gm_site$location=="Home"], gm_site$conc[gm_site$location=="Ambient"]),
      col="blue") # lowess line (x,y)

#Scatter plot of indoor vs outdoor ####

plot(gm_site$conc[gm_site$location=="Home"], gm_site$conc[gm_site$location=="Ambient"], 
     main = expression(paste("Indoor vs Outdoor ",PM[2.5]," Conc", sep = "")),
     xlab = expression(paste("Indoor Conc ",mu,"g/",m^3,"",sep ="")), 
     ylab = expression(paste("Ambient Conc ",mu,"g/",m^3,"",sep ="")))
abline(lm_byloc, col=1) # regression line (y~x) 
legend("topright", bty="O", legend=paste("R2 is",format(summary(lm_byloc)$adj.r.squared, digits=4)))

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
