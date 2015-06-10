#CURRENT WORKFLOW BELOW
#Load Packages, Load and Subset Data####

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Packages Needed to Complete this Analysis                                    #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

pkg <- c("doBy", "psych", "beeswarm", "ggplot2", "reshape2", "scales")
for (i in seq(length(pkg))) {
    library(pkg[i], character.only = TRUE)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Load Dataset, Subset Excluding Xuanwei                                        #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##From UW

#file_UW <- c("C:/Users/khartin/Desktop/Xuanwei_ForAnalysis_Test.csv") 
##X <- subset(read.csv(file_UW),village != "Xuanwei")

##From PC

file_PC <- c("C:/Users/KGH/OneDrive/Documents/Xuanwei/air-comp/Xuanwei_ForAnalysis_Test.csv")
X <- subset(read.csv(file_PC),village != "Xuanwei")

#Summary Stats ####

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Summarize Data By Site, Location                                              #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##AM Conc of Components by Site, Location, Village

mass.site <- droplevels(
    summaryBy(conc ~ sitenum + location + village, data=X,id = NULL, 
              keep.names=TRUE, FUN=mean, na.rm = TRUE))
levo.site <- droplevels(
    summaryBy( Levo_ugV ~ sitenum + location + village, data=X, id = NULL, 
               keep.names=TRUE, FUN=mean, na.rm = TRUE))
nitro.site <- droplevels(
    summaryBy( X1NP_ugV + X2NP_ugV + X2NFL_ugV ~ sitenum + location + village, 
               data=X, id = NULL, keep.names=TRUE, FUN=mean, na.rm = TRUE))
BaP.site <- droplevels(
    summaryBy( Benzo.a.pyrene_ugV  ~ sitenum + location, data=X, id = NULL, 
           keep.names=TRUE, FUN=mean, na.rm = TRUE))
metals <- droplevels(
    summaryBy( PbConcV + AsConcV + SeConcV  ~ sitenum + location, data=X, 
               id = NULL, keep.names=TRUE, FUN=mean, na.rm = TRUE))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Scatterplot Data By Site, Location                                            #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##AM Mass by Site Showing NAAQS 24hr (35 ug/m3)

plot(conc ~ sitenum, data = mass.site, type = "n", log="y",axes = FALSE,
     frame.plot=TRUE,ylab =
         expression(paste("Concentration ( ",mu,"g/m^3",")",sep ="")),
     xlab="Site Number",main="")
points(conc ~ sitenum,data = subset(mass.site, location == "Home"), 
       col=1,pch=19)
points(conc ~ sitenum,data = subset(mass.site, location == "Ambient"), 
       col=8,pch=15)
Axis(side=1, at = 1:9, labels=c(1:9))
Axis(side=2, labels=TRUE)
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor","Outdoor","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

##AM Mass by Site Showing NAAQS 24hr (35 ug/m3), Arranged by Indoor Conc

mass.site[,"inrank"] <- c(6,6,9,9,7,7,4,4,1,1,2,2,5,5,8,8,3,3)
mass.in.points <- subset(mass.site, location == "Home")
mass.out.points <- subset(mass.site, location == "Ambient")
int <- order(mass.in.points$conc)
int_sorted <- mass.in.points[int,]

plot(conc ~ inrank, data = mass.site, type = "n", log="y",axes = FALSE,
     frame.plot=TRUE,ylab =
         expression(paste("Concentration ( ",mu,"g/m^3",")",sep ="")),
     xlab="Site Number",main="")
points(mass.in.points$inrank,mass.in.points$conc,col=1,pch=19)
points(mass.out.points$inrank,mass.out.points$conc,col=8,pch=15)
Axis(side=1, at = 1:9, labels=c(5,6,9,4,7,1,3,8,2))
Axis(side=2, labels=TRUE)
abline(h=35, lty=4, col=1)
legend("topleft", c("Indoor","Outdoor","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Boxplots by Village                                                           #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##Boxplots

###Mass by Village with NAAQS 24hr (35 ug/m3)####

boxplot(conc~village, data=mass.site, pch=19,
        ylab=expression(paste("Concentration ( ",mu,"g/m^3",")",sep ="")),
        main="",xlab = "Village", log="y", ylim=c(20,2000))
beeswarm(conc~village, data=am.site, pwpch=ifelse(location=="Home",19,15), 
         pwcol=ifelse(location=="Home",1,8),log="TRUE", add=TRUE, 
         ylim=c(20,2000))
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor", "Outdoor","NAAQS 35 ug/m^3"),
       col=c(1,8), pch=c(19,15,NA),lty=c(NA,NA,4))

###Levo by Village
boxplot(Levo_ugV ~village, data=levo.site, pch=19,
        ylab=expression(paste("Levoglucosan ( ",mu,"g/m^3",")",sep ="")),
        main="",xlab = "Village", log="y")
beeswarm(Levo_ugV~village, data=levo.site, pwpch=ifelse(location=="Home",19,15), 
         pwcol=ifelse(location=="Home",1,8),log="TRUE", add=TRUE)
legend("topright", c("Indoor", "Outdoor"),
       col=c(1,8), pch=c(19,15))

###Nitro-PAH by Village, Native Plot

par(mfrow=c(1,3))

boxplot(X1NP_ugV ~ village, data=nitro.site, pch=19,
        ylab=expression(paste("1-NP ( ",mu,"g/m^3",")",sep ="")),
        main="", log="y", ylim=c(2e-05,2e-02))
beeswarm(X1NP_ugV ~ village, data=nitro.site, 
         pwpch=ifelse(location=="Home",19,15), 
         pwcol=ifelse(location=="Home",1,8),log="TRUE", add=TRUE, 
         ylim=c(2e-05,2e-02))

boxplot(X2NP_ugV ~ village, data=nitro.site, pch=19,
        ylab=expression(paste("2-NP ( ",mu,"g/m^3",")",sep ="")),
        main="",xlab = "Village", log="y", ylim=c(2e-05,2e-02))
beeswarm(X2NP_ugV ~ village, data=nitro.site, 
         pwpch=ifelse(location=="Home",19,15), 
         pwcol=ifelse(location=="Home",1,8),log="TRUE", add=TRUE, 
         ylim=c(2e-05,2e-02))

boxplot(X2NFL_ugV ~ village, data=nitro.site, pch=19,
        ylab=expression(paste("2-NFL ( ",mu,"g/m^3",")",sep ="")),
        main="", log="y", ylim=c(2e-05,2e-02))
beeswarm(X2NFL_ugV ~ village, data=nitro.site, 
         pwpch=ifelse(location=="Home",19,15), 
         pwcol=ifelse(location=="Home",1,8),log="TRUE", add=TRUE, 
         ylim=c(2e-05,2e-02))
legend("topright", c("Indoor", "Outdoor"),
       col=c(1,8), pch=c(19,15))

###Nitro-PAH by Village, GG Plot

am.nitro.m <- melt(nitro.site, id.vars = c("sitenum", "location", "village"))
bxp.NP <- ggplot(am.nitro.m, aes(x=village, y=value))
bwPalette <- c("#999999", "#CCCCCC", "#FFFFFF")

bxp.NP + scale_y_continuous(trans=log10_trans()) + ylab(expression(
    paste("Concentration ( ",mu,"g/m^3",")",sep =""))) + 
    geom_boxplot(aes(fill = variable)) + theme_bw() +
    scale_fill_manual(values=bwPalette, name = "Nitro PAH",
                      labels = c("2-NP", "2-NFL","1-NP"))

###Faking the data, since you didn't provide any####
Gene <- data.frame(matrix(rweibull(100*4, 1), 100))
names(Gene) <- paste0("Ind", 1:4)
Gene <- rep(list(Gene), 4)

# Setup the panels
layout(t(1:3))
par(oma=c(2, 4, 4, 0), mar=rep(1, 4), cex=1)
# `mar` controls the space around each boxplot group

# Calculating the range so that the panels are comparable
my.ylim <- c(min(sapply(am.site.NP[,4:6], min, na.rm = TRUE)),
             max(sapply(am.site.NP[,4:6], max, na.rm = TRUE)))

# Plot all the boxes
for(i in 4:6){
    boxplot(am.site.NP[,i], ylim=my.ylim, axes=FALSE)
    mtext(paste("", i), 1, 0)
    if(i == 4){
        axis(2, las=1)
        mtext("Expression or what you have", 2, 3)
    }
}
title("Look at all my genes!", outer=TRUE)

###My attempt####
#boxplot(am.site.NP[,4:6])
#layout(t(1:3))
#par(oma=c(2, 4, 4, 0), mar=rep(1, 4), cex=1)

par(mfrow=c(1,3), mar = rep(1,3))

boxplot(X1NP_ugV ~ village, data = am.site.NP, names, ylim=my.ylim,
        at = c(0:2*3), pch=19, ylab=expression(paste("Conc ",mu,"g/m^3","",sep ="")), main="", 
        log="y", xlab = "")
boxplot(X2NP_ugV ~ village, data = am.site.NP, at = 0:2*3 + 1, pch=19,
        ylab=expression(paste("Conc ",mu,"g/m^3","",sep ="")), main="", 
        log="y")
boxplot(X2NFL_ugV ~ village, data = am.site.NP, at = 0:2*4 + 2, pch=19,
        ylab=expression(paste("Conc ",mu,"g/m^3","",sep ="")), main="", 
        log="y")
axis(1, at = 0:2*4 + 1.5, labels = colnames(am.site.NP), tick = TRUE)
beeswarm(X2NP_ugV ~ village, data = am.site.NP, pch=19,
         pwcol=ifelse(location=="Home",1,8), log="TRUE", add=TRUE)
legend("topright", c("Indoor", "Ambient"), col=c(1,8), pch=c(19,15))

bwplot(X1NP_ugV + X2NP_ugV + X2NFL_ugV ~ village, data = am.site.NP)

##Boxplot of Nitro PAHS by Village using ggplot2####
library(ggplot2)
library(reshape2)
library(scales)

am.nitro.m <- melt(nitro.site, id.vars = c("sitenum", "location", "village"))

bxp.NP <- ggplot(am.nitro.m, aes(x=village, y=value))

bwPalette <- c("#999999", "#CCCCCC", "#FFFFFF")

bxp.NP + scale_y_continuous(trans=log10_trans()) + ylab(expression(
    paste("Concentration ( ",mu,"g/m^3",")",sep =""))) + 
    geom_boxplot(aes(fill = variable)) + theme_bw() +
    scale_fill_manual(values=bwPalette, name = "Nitro PAH",
                      labels = c("2-NP", "2-NFL","1-NP"))


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

#Scatter plot of indoor by sample period ####

plot(X$conc ~ X$sitenum, type="n", log="y",ylab = expression(paste("Concentration ( ",mu,"g/m^3",
                                             ")",sep ="")),
     xlab="Site Number",main="")
points(am.in.points$inrank,am.in.points$conc,col=1,pch=19)
points(am.out.points$inrank,am.out.points$conc,col=8,pch=15)
Axis(side=1, at = 1:9, labels=c(6,9,7,4,1,2,5,8,3))
Axis(side=2, labels=TRUE)
abline(h=35, lty=4, col=1)
legend("topleft", c("Indoor","Outdoor","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

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
points(X$conc[,X$samper == "Night"],col=1,pch=19)
points(am_avgmeans_x,col=8,pch=15)
legend("topright", c("Indoor", "Mean","Median","Ambient", "Mean", "Median"),col=c(1,1,1,8,8,8),
       pch=c(19,NA,NA,15,NA,NA),lty=c(NA,1,5,NA,3,4))
abline(h=mean(ho_avgmeans_x), lty=1, col=1)
abline(h=median(ho_avgmeans_x), lty=5, col=1)
abline(h=mean(am_avgmeans_x), lty=3, col=8)
abline(h=median(am_avgmeans_x), lty=4, col=8)

#Geometric Mean of Indoor and Dup Samples by Site Showing Geometric Mean and Median of Indoor and Ambient#### 

#plot(conc~sitenum, type="n",log="y",ylab="Concentration ug/m^3", xlab="Site Number",
#     main="PM2.5 Concentration by Site") #Why cannot control y range with ylim=c(0, 2000)?
#points(ho_gmeans_x,col=1,pch=19)
#points(am_gmeans_x,col=8,pch=15)
#legend("topright", c("Indoor", "GM","Median","Ambient", "GM", "Median"),col=c(1,1,1,8,8,8),
#       pch=c(19,NA,NA,15,NA,NA),lty=c(NA,1,5,NA,3,4))
#abline(h=mean(ho_gmeans_x), lty=1, col=1)
#abline(h=median(ho_gmeans_x), lty=5, col=1)
#abline(h=mean(am_gmeans_x), lty=3, col=8)
#abline(h=median(am_gmeans_x), lty=4, col=8)

#Plot of indoor/outdoor ratios
ratio_site <- summaryBy(conc~location + sitenum, data=gm_site, id = "village", keep.names=TRUE, FUN=gm)

##Scatter Plots of Conc
#Fancy Scatterplot
#install.packages('car')
#library(car)
#scatterplot(conc~sitenum | location, data=gm_site)

#Import each origin .csv file excluding unnessesary rows and columns and export each as
#'cleaned' dataset SECTION ON HOLD
##Something along the lines of Brian High's Example

##Subset data by location and create mean of concentrations

#Create new objects, one for Home and one for Ambient
##Create new objects -> Mean of sites, points for plotting by location ####
#with(subset(X, location == "Home" & sitenum == 1), summary(conc)) 


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

###Correct Concentration

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

##Points excluding the outlier sites 2,8####
#am.site.no.2.8 <- subset(am.site, sitenum != 2 & sitenum != 8)
#am.in.points.no.2.8 <- subset(am.site.no.2.8, location == "Home")
#am.out.points.no.2.8 <- subset(am.site.no.2.8, location == "Ambient")