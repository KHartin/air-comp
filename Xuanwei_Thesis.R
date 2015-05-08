#Load dataset, exclude Xuanwei, view table
#From UW
Xuanwei_ForAnalysis_31Mar15 <- read.csv("H:/Seto_Projects/Xuanwei/Xuanwei_ForAnalysis_31Mar15.csv")
X<-subset(Xuanwei_ForAnalysis_31Mar15, village != "Xuanwei")
#detach(X)
#attach(X)

#From Surface
#?

#Mass balance


###Data reduction

## Reduce number of metals to thos with at least 10 greater than 2*Unc

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

am.site <- summaryBy(as.formula(this.formula), data=X, id = "village", keep.names=TRUE, FUN=mean)




#Summary By ####
install.packages("doBy")
library(doBy)

#gm = function(x, na.rm=TRUE){
  #exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
#}

gm_site <- summaryBy(conc ~location + sitenum, data=X, id = "village", keep.names=TRUE, FUN=gm)

##Create new objects -> Home, Ambient, Mean and Geometric Mean Home Concentrations ####

is.home <- subset(X, location == "Home")
is.ambient <- subset(X, location == "Ambient")

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

