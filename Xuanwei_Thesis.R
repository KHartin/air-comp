#Packages####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Set Working Directory, Install Packages Needed to Complete this Analysis     #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##From UW

setwd("C:/Users/khartin/Desktop")

wd <- getwd()

##From Surface

setwd("C:/Users/KGH/OneDrive/Documents/Xuanwei/Files/Results")

wd <- getwd()

pkg <- c("doBy", "ggplot2", "reshape2", "beeswarm") #"psych", "beeswarm", "scales")
for (i in seq(length(pkg))) {
    library(pkg[i], character.only = TRUE)
}

#for (pkg in c("knitr", "RCurl", "hash", "rJava", "XLConnect", "dplyr", "ggmap",
#             "doBy", "psych", "beeswarm", "ggplot2", "reshape2", "scales")) {
#    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
#        install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
#        if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
#            stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
#        }
#    }
#}

#Merge files into single dataset by filterid####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Import Data                                                                  #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##Import Final Dataset from Directory - UPDATE to GitHub

grav <- read.csv(paste(wd, "/Xuanwei_Grav.csv", sep =""))


##Metals####
###Import Metals Results, Correct Column Names, Merge with gln_pah

xrf <- read.csv(paste(wd, "/Xuanwei_XRF.csv", sep = ""))

xrf.cols.dont.want <- c("PM_net.mass", "Analyzer.ID", "XRF.Analysis.Date",
                        "Test.Protocol")

xrf <- xrf[, ! names(xrf) %in% xrf.cols.dont.want, drop = F]

colnames(xrf)[c(1)] <- c("FilterId")

colnames(xrf) <- sub("Conc","ngcm2",colnames(xrf))

glnp_xrf <- merge(gln_pah, xrf, by="FilterId")

###Add New Columns Correcting ug/cm2 to ug

k=1
for(i in seq(68, 162, 2)) {
    glnp_xrf[,163+k] <- (glnp_xrf[,i] * (pi*1.85^2))
    s <- unlist(strsplit(colnames(glnp_xrf[i]), split='.', fixed=TRUE))
    names(glnp_xrf)[163+k]<-paste(s[1],".Mass.ug",sep="")
    k=k+1
}

###Add New Columns Converting Metal Mass.mg to Metal Mass.ug/m^3

k=1
for(i in 164:211) {
    glnp_xrf[,211+k] <- (glnp_xrf[,i] / AirVolume)
    s <- unlist(strsplit(colnames(glnp_xrf[i]), split='.', fixed=TRUE))
    names(glnp_xrf)[211+k]<-paste(s[1],".Mass.ug.m3",sep="")
    k=k+1
}

##Summary of Mass By Component####

####ALLL F'ed up


###Add Columns for Sum of Component Masses and Percent of Total Mass

fslv <- as.character(paste("~ FilterId + SiteNumber + Location + Village"))

all.mass <- as.character(c("PM2.5Mass.ug", "LgMass.ug", "X2npMass.ug",
                         "X2nflMass.ug", "X1npMass.ug", 
                         "FlAnMass.ug", "PyrMass.ug", "BaAMass.ug",
                         "ChryMass.ug", "BbFMass.ug", "BkFMass.ug",
                         "BaPMass.ug", "I123cdPMass.ug", "DahAMass.ug",
                         "BghiPMass.ug", "Na.Mass.ug", "Mg.Mass.ug", "Al.Mass.ug",
                            "Si.Mass.ug", "P.Mass.ug", "S.Mass.ug", "Cl.Mass.ug",
                            "K.Mass.ug", "Ca.Mass.ug", "Sc.Mass.ug", "Ti.Mass.ug",
                            "V.Mass.ug", "Cr.Mass.ug", "Mn.Mass.ug", "Fe.Mass.ug",
                            "Co.Mass.ug", "Ni.Mass.ug", "Cu.Mass.ug", "Zn.Mass.ug",
                            "Ga.Mass.ug", "As.Mass.ug", "Se.Mass.ug",
                            "Br.Mass.ug", "Rb.Mass.ug", "Sr.Mass.ug", "Y.Mass.ug",
                            "Zr.Mass.ug", "Nb.Mass.ug", "Mo.Mass.ug",
                            "Ag.Mass.ug", "Cd.Mass.ug", "In.Mass.ug", "Sn.Mass.ug",
                            "Sb.Mass.ug", "Cs.Mass.ug", "Ba.Mass.ug", "La.Mass.ug",
                            "Ce.Mass.ug", "Sm.Mass.ug", "Eu.Mass.ug", "Tb.Mass.ug",
                            "Hf.Mass.ug", "Ta.Mass.ug", "W.Mass.ug", "Ir.Mass.ug",
                            "Au.Mass.ug", "Hg.Mass.ug", "Pb.Mass.ug"))

total_mass_formula <-  as.formula(paste(paste(all.mass, collapse = "+"), fslv))

mass_by_component <- droplevels(summaryBy(total_mass_formula, data=glnp_xrf,
                                    id = NULL, keep.names=TRUE, 
                                    FUN=sum, na.rm = TRUE))

totalMass <- function(range) {
    apply(mass_by_component[,range],1,sum)
}

mass_by_component$Total.Prop <- (mass_by_component[, 5] / mass_by_component[, 5]) *100
mass_by_component$Lg.Prop <- (mass_by_component[, 6] / mass_by_component[, 5]) *100
mass_by_component$Npah.Prop <- (totalMass(7:9) / mass_by_component[, 5]) *100
mass_by_component$Pah.Prop <- (totalMass(10:19) / mass_by_component[, 5]) *100
mass_by_component$Metals.Prop <- (totalMass(20:67) / mass_by_component[, 5]) *100
mass_by_component$Component.Prop <- totalMass(69:72)

#mass_by_component$Npah.TotalMass <- totalMass(7:9)mass_by_component$Pah.TotalMass <- totalMass(10:19)mass_by_component$Metal.TotalMass <- totalMass(20:67)mass_by_component$Component.TotalMass <- totalMass(c(6, 68:70))

#write.csv(mass_by_component, file = "Xuanwei_TotalMassAnalysis.csv")

###Plot Proportion of Total Mass by Component 


#names(mass_by_component[, colnames(mass_by_component) == c("FilterId")])
#colnames(mass_by_component[, c(1:5, 7:67)])

melt.sum <- melt(mass_by_component, id.vars = colnames(mass_by_component[, c(1:68, 73)]), factorsAsStrings = F)
#bwPalette <- c("#999999", "#CCCCCC", "#FFFFFF", "#999999", "#CCCCCC")

ggplot(melt.sum, aes(x = FilterId, y = Component.Prop, fill = variable)) + 
    geom_bar(stat="identity") + 
    scale_x_discrete(breaks=droplevels(as.factor(mass_by_component[,1]))) + theme_bw() +
    scale_fill_manual(values=bwPalette, name = "Component",
                      labels = c("Levoglucosan", "Nitro-PAH","PAH", "Metals", "Total")) #+ facet_grid(~Village)  #+ scale_y_log10()
   






###Identify Metals with 2/3 of Samples Above 2 * Uncertainty####

MetalsZero <- data.frame(matrix(nrow = 48, ncol = 4))

names(MetalsZero) <- c("Metal", "SamplesAtZero", "SamplesAboveZero", 
                       "SamplesAbove2Unc")

k=1
for(i in seq(68,162,2)) {
    MetalsZero[k,1] <-paste(colnames(glnp_xrf[i]))
    MetalsZero[k,2] <- sum(glnp_xrf[,i] == 0)
    MetalsZero[k,3] <- sum(glnp_xrf[,i] > 0)
    MetalsZero[k,4] <- sum(glnp_xrf[,i] >= 2 * glnp_xrf[,i+1])
    k = k + 1
}

xrf.metals.not.for.analysis <- substr(as.character(subset(MetalsZero,
                                                    SamplesAbove2Unc < 20)[,1]),
                                       1, 2)

xrf.metals.not.for.analysis <- unlist(strsplit(xrf.metals.not.for.analysis,
                                               split='.', fixed=TRUE))

xrf.metals.not.for.analysis <- paste(xrf.metals.not.for.analysis,".", sep = "")

xfr.metals.excluded <- c()

k <- 1
for(i in 1:34) {
    a <- grep(xrf.metals.not.for.analysis[i], names(glnp_xrf), fixed = TRUE,
              value=TRUE)
    xfr.metals.excluded <- c(xfr.metals.excluded,a)
    k <- k + 1
}

glnp_xrf <- glnp_xrf[, ! names(glnp_xrf) %in% xfr.metals.excluded, drop = F]

colnames(glnp_xrf)

###Remove Unnecassary Columns

glnp_xrf.cols.dont.want <- c("Mg.ngcm2", "Mg.Unc", "Al.ngcm2", "Al.Unc",
                             "Si.ngcm2",  "Si.Unc", "P.ngcm2", "P.Unc",
                             "S.ngcm2", "S.Unc", "K.ngcm2",  "K.Unc",
                             "Ca.ngcm2", "Ca.Unc", "Ti.ngcm2", "Ti.Unc",
                             "Mn.ngcm2", "Mn.Unc", "Fe.ngcm2", "Fe.Unc",
                             "Cu.ngcm2", "Cu.Unc", "Zn.ngcm2", "Zn.Unc",
                             "Eu.ngcm2", "Eu.Unc", "Pb.ngcm2", "Pb.Unc")

final <- glnp_xrf[, ! names(glnp_xrf) %in% glnp_xrf.cols.dont.want, drop = F]

##Output Final Dataset to .csv

#write.csv(final, file = "XuanweiForAnalysis.csv")





###Change All Charcters to Lower Case, Necessary?
#X <- as.data.frame(sapply(X, toupper), stringsAsFactors=FALSE)

#Summary Stats #### Make into for loop? sapply? ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Summarize Data By Site, Location                                              #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##Arithmetic Mean of Component Concentrations by Site, Location, Village

slv <- as.character(paste("~ SiteNumber + Location + Village"))

###PM2.5 Total Mass / m^3

mass.names <- as.character(c("PM2.5Mass.ug.m3"))

mass.formula <-  as.formula(paste(paste(mass.names, collapse = "+"),slv))

mass.by.slv <- droplevels(summaryBy(mass.formula, data=final,
                                              id = NULL, keep.names=TRUE, 
                                              FUN=mean, na.rm = TRUE))

#write.csv(mass.by.slv, file = "mass_compile.csv")

###Levoglucosan Mass / m^3

lg.names <- as.character(c("LgMass.ug.m3", "LgMass.ug.m3.LOQ"))

lg.formula <-  as.formula(paste(paste(lg.names, collapse = "+"),slv))

levo.by.slv <- droplevels(summaryBy(lg.formula, data=final, id = NULL, 
                                     keep.names=TRUE, FUN=mean, na.rm = TRUE))

#write.csv(levo.by.slv, file = "levo_compile.csv")

###Nitro-PAH Mass / m^3

npah.names <- as.character(colnames(final[, 22:27]))

npah.formula <-  as.formula(paste(paste(npah.names, collapse = "+"), slv))

npah.by.slv <- droplevels(summaryBy(npah.formula, data=final,id = NULL, 
                                    keep.names=TRUE, FUN=mean, na.rm = TRUE))

#write.csv(npah.by.slv, file = "npah_compile.csv")

#specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
#npah.by.slv <- specify_decimal(npah.by.slv[,4:9], 7)

###PAH Mass / m^3

pah.names <- as.character(colnames(final[, 50:67]))

pah.formula <-  as.formula(paste(paste(pah.names, collapse = "+"), slv))

pah.by.slv <- droplevels(summaryBy(pah.formula, data=final,id = NULL, 
                                    keep.names=TRUE, FUN=mean, na.rm = TRUE))

#write.csv(pah.by.slv, file = "pah_compile.csv")

###Metal Mass / m^3

metals.names <- as.character(colnames(final[, 82:95]))

metals.formula <-  as.formula(paste(paste(metals.names, collapse = "+"), slv))

metals.by.slv <- droplevels(summaryBy(metals.formula, data=glnp_xrf,id = NULL, 
                                   keep.names=TRUE, FUN=mean, na.rm = TRUE))

#write.csv(metals.by.slv, file = "metals_compile.csv")

##Remove data not being used

rm(lg, grav, pah, npah, xrf, grav_lg, gl_npah, gln_pah, glnp_xrf, MetalsZero)

#Scatterplots####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Scatterplot Data By Site, Location                                            #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##PM2.5 Mass by Site Showing NAAQS 24hr (35 ug/m3)

plot(PM2.5Mass.ug.m3 ~ SiteNumber, data = mass.by.slv, type = "n", 
     log="y",axes = FALSE, frame.plot=TRUE,ylab =
         expression(paste("PM2.5 Concentration ( ",mu,"g/m^3",")",sep ="")),
     xlab="Site Number",main="")
points(PM2.5Mass.ug.m3 ~ SiteNumber, 
       data = subset(mass.by.slv, Location == "Indoor"), col=1,pch=19)
points(PM2.5Mass.ug.m3 ~ SiteNumber, 
       data = subset(mass.by.slv, Location == "Outdoor"), col=8,pch=15)
Axis(side=1, at = 1:9, labels=c(1:9))
Axis(side=2, labels=TRUE)
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor","Outdoor","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

##PM2.5Mass by Site Showing NAAQS 24hr (35 ug/m3), Arranged by Indoor Conc

mass.by.slv[,"inrank"] <- c(6,6,9,9,7,7,4,4,1,1,2,2,5,5,8,8,3,3)
mass.in.points <- subset(mass.by.slv, Location == "Indoor")
mass.out.points <- subset(mass.by.slv, Location == "Outdoor")
int <- order(mass.in.points$PM2.5Mass.ug.m3)
int_sorted <- mass.in.points[int,]

plot(PM2.5Mass.ug.m3 ~ inrank, data = mass.by.slv, type = "n", log="y", 
     axes = FALSE, frame.plot = TRUE, ylab =
         expression(paste("PM2.5 Concentration ( ",mu,"g/m^3",")",sep ="")),
     xlab="Site Number",main="")
points(mass.in.points$inrank,mass.in.points$PM2.5Mass.ug.m3,col=1,pch=19)
points(mass.out.points$inrank,mass.out.points$PM2.5Mass.ug.m3,col=8,pch=15)
Axis(side=1, at = 1:9, labels=c(5,6,9,4,7,1,3,8,2))
Axis(side=2, labels=TRUE)
abline(h=35, lty=4, col=1)
legend("topleft", c("Indoor","Outdoor","NAAQS 35 ug/m^3"),col=c(1,8,1),
       pch=c(19,15,NA),lty=c(NA,NA,4))

#Boxplots####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Boxplots by Village                                                           #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

minor.ticks.axis <- function(ax,n,t.ratio=0.5,mn,mx,...){
    
    lims <- par("usr")
    if(ax %in%c(1,3)) lims <- lims[1:2] else lims[3:4]
    
    major.ticks <- pretty(lims,n=5)
    if(missing(mn)) mn <- min(major.ticks)
    if(missing(mx)) mx <- max(major.ticks)
    
    major.ticks <- major.ticks[major.ticks >= mn & major.ticks <= mx]
    
    labels <- sapply(major.ticks,function(i)
        as.expression(bquote(10^ .(i)))
    )
    axis(ax,at=major.ticks,labels=labels,...)
    
    n <- n+2
    minors <- log10(pretty(10^major.ticks[1:2],n))-major.ticks[1]
    minors <- minors[-c(1,n)]
    
    minor.ticks = c(outer(minors,major.ticks,`+`))
    minor.ticks <- minor.ticks[minor.ticks > mn & minor.ticks < mx]
    
    
    axis(ax,at=minor.ticks,tcl=par("tcl")*t.ratio,labels=FALSE)
}

##Mass with NAAQS 24hr (35 ug/m3)

boxplot(PM2.5Mass.ug.m3 ~ Village, data = mass.by.slv, pch = 19,
        ylab = expression(paste("PM2.5 Concentration ( ",mu,"g/m^3",")",sep ="")),
        main = "",xlab = "Village", log = "y", ylim = c(10,2000))
beeswarm(PM2.5Mass.ug.m3 ~ Village, data = mass.by.slv,
         pwpch = ifelse(Location == "Indoor",19,15),
         pwcol = ifelse(Location == "Indoor", 1, 8),log = "TRUE", add = TRUE, 
         ylim = c(20,2000))
abline(h=35, lty=4, col=1)
legend("topright", c("Indoor", "Outdoor","NAAQS 35 ug/m^3"),
       col=c(1,8), pch=c(19,15,NA),lty=c(NA,NA,4))

###Levoglucosan
#levo.by.slv <- format(levo.by.slv, scientific = FALSE)
#library(Hmisc)
#minor.tick(nx=n, ny=n, tick.ratio=n)
options("scipen" = 20)
boxplot(LgMass.ug.m3 ~ Village, data = levo.by.slv, pch = 19,
        ylab = expression(paste("Levoglucosan Concentration ( ",mu,"g/m^3",")",
                                sep = "")), main = "", xlab = "Village",
        log = "y",  ylim = c(0.01, 200),  axes = FALSE, frame.plot = TRUE)
beeswarm(LgMass.ug.m3 ~ Village, data = levo.by.slv,
         pwpch = ifelse(Location == "Indoor", 19, 15), 
         pwcol = ifelse(Location == "Indoor", 1, 8), log = "TRUE", add = TRUE)
lg.y <- c(0.01, 0.10, 1.0, 10, 100)
Axis(side = 1, at = 1:3, labels = c("Jiu Bao", "Qi Long", "Tang Tang"))
Axis(side=2, at = lg.y, labels = c(lg.y))
legend("topright", c("Indoor", "Outdoor"),
       col=c(1,8), pch=c(19,15))
options("scipen" = 0)

###Benzo[a]pyrene

boxplot(BaPMass.ug.m3 ~ Village, data = pah.by.slv, pch=19,
        ylab = expression(paste("Benzo[a]pyrene ( ",mu,"g/m^3",")",sep = "")),
        main="",xlab = "Village", log = "y", axes = FALSE, frame.plot = TRUE)
beeswarm(BaPMass.ug.m3 ~ Village, data=pah.by.slv, pwpch=ifelse(
    Location == "Indoor",19,15), pwcol=ifelse(Location == "Indoor",1,8),
    log="TRUE", add=TRUE)
BaP.y <- c(0.001, 0.005, 0.01, 0.05, 0.10)
Axis(side = 1, at = 1:3, labels = c("Jiu Bao", "Qi Long", "Tang Tang"))
Axis(side=2, at = BaP.y, labels = c(BaP.y), las = 2)
legend("topright", c("Indoor", "Outdoor"),
       col=c(1,8), pch=c(19,15))

###Nitro-PAH, Native Plot

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

par(mfrow=c(1,1))

###Nitro-PAH by Village, GG Plot

npah.melt <- melt(npah.by.slv[, 1:6], id.vars = c("SiteNumber", "Location", "Village"))
bxp.npah <- ggplot(npah.melt, aes(x = Village, y = value))
bwPalette <- c("#999999", "#CCCCCC", "#FFFFFF")

bxp.npah + scale_y_log10() + ylab(expression(
    paste("Concentration ( ",mu,"g/m^3",")",sep = ""))) + xlab("Village") +
    geom_boxplot(aes(fill = variable)) + theme_bw() +
    scale_fill_manual(values=bwPalette, name = "Nitro PAH",
                      labels = c("2-Nitrpyrene", "2-Nitrofluorine","1-Nitropyrene"))

###PAH by Village, GG Plot

pah.melt <- melt(pah.by.slv[, 1:11], id.vars = c("SiteNumber", "Location", "Village"))
bxp.pah <- ggplot(pah.melt, aes(x = Village, y = value))
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


bxp.pah + scale_y_log10() + ylab(expression(
    paste("Concentration ( ",mu,"g/m^3",")",sep = ""))) + xlab("Village") +
    geom_boxplot(aes(fill = variable)) + theme_bw() +
    scale_fill_manual(values = cbbPalette, name = "PAH"), labels = c("Benzo[A]Anthracine", "2-Nitrofluorine","1-Nitropyrene"))

#Indoor vs Outdoor####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Significant Difference Between Indoor / Outdoor Concentrations                #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
http://stackoverflow.com/questions/4716152/why-do-r-objects-not-print-in-a-function-or-a-for-loop
##Wilcox / Mann-Whitney Test

###Mass
in.mass <- subset(mass.site, location == "Home")
out.mass <- subset(mass.site, location == "Ambient")
mass.wil <- wilcox.test(in.mass$conc,out.mass$conc, paired = FALSE)
mass.wil2 <- wilcox.test(in.mass$conc,out.mass$conc, paired = TRUE)

###Levoglucosan
in.levo <- subset(levo.site, location == "Home")
out.levo <- subset(levo.site, location == "Ambient")
levo.wil <- wilcox.test(in.levo$Levo_ugV,out.levo$Levo_ugV, paired = FALSE)
levo.wil2 <- wilcox.test(in.levo$Levo_ugV,out.levo$Levo_ugV, paired = TRUE)

###1-Nitropyrene
in.nitro <- subset(nitro.site, location == "Home")
out.nitro <- subset(nitro.site, location == "Ambient")
x1np.wil <- wilcox.test(in.nitro$X1NP_ugV,out.nitro$X1NP_ugV, paired = FALSE)
x1np.wil2 <- wilcox.test(in.nitro$X1NP_ugV,out.nitro$X1NP_ugV, paired = TRUE)

###Benzo[A]pyrene
in.BaP <- subset(BaP.site, location == "Home")
out.BaP <- subset(BaP.site, location == "Ambient")
BaP.wil <- wilcox.test(in.BaP$Benzo.a.pyrene_ugV,out.BaP$Benzo.a.pyrene_ugV,
                       paired = FALSE)
BaP.wil2 <- wilcox.test(in.BaP$Benzo.a.pyrene_ugV,out.BaP$Benzo.a.pyrene_ugV, 
                       paired = TRUE)

##Kruskal-Wallis Test

###Mass
mass.kw <- kruskal.test(conc ~ location, data = mass.site)

###Levoglucosan
levo.kw <- kruskal.test(Levo_ugV ~ location, data = levo.site)

###1-Nitropyrene
x1np.kw <- kruskal.test(X1NP_ugV ~ location, data = nitro.site)

###Benzo[A]pyrene
BaP.kw <- kruskal.test(Benzo.a.pyrene_ugV ~location, data = BaP.site)

##Plot of Non-parametric test stats
#par(mar = c(5,5,2,5))
#par(new = TRUE)
#wilcox.stat <- c(mass.wilcox$statistic, levo.wilcox$statistic, 
#                 NP1.wilcox$statistic,BaP.wilcox$statistic) 
#plot(wilcox.stat, type = "n",axes = FALSE,
#     frame.plot=TRUE,ylab = "P Value", xlab="Component",main="")
#mtext(side = 4, line = 3, "W Stat")
#text(wilcox.p, pos = 4, labels = wilcox.stat)

wil.p <- c(mass.wil$p.value, levo.wil$p.value, x1np.wil$p.value,
           BaP.wil$p.value)
wil2.p <- c(mass.wil2$p.value, levo.wil2$p.value, x1np.wil2$p.value,
           BaP.wil2$p.value)
kw.p <- c(mass.kw$p.value, levo.kw$p.value, x1np.kw$p.value,
            BaP.kw$p.value)
labels(wil.p,c("Mass", "Levo", "1-NP", "BaP"))
plot(wil.p, type = "n",axes = FALSE, frame.plot=TRUE,
     ylab = "P Value", xlab="Component",main="")
points(wil.p, pch = 25)
points(wil2.p, pch = 20)
points(kw.p, pch = 0)
Axis(side=2, labels=TRUE)
Axis(side=1, at = 1:4, labels=c("Mass", "Levo", "1-NP", "BaP"))
legend("topleft", c("Mann-Whitney", "Signed-Rank", "Kruskal-Wallis"), pch=c(25,20,0))


#Between Villages####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#Significant Concentration Difference Between Villages                         #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##Kruskal-Wallis Test

###Mass
mass.kw <- kruskal.test(conc ~ village, data = mass.site)

###Levoglucosan
levo.kw <- kruskal.test(Levo_ugV ~ village, data = levo.site)

###1-Nitropyrene
x1np.kw <- kruskal.test(X1NP_ugV ~ village, data = nitro.site)

###Benzo[A]pyrene
BaP.kw <- kruskal.test(Benzo.a.pyrene_ugV ~ village, data = BaP.site)

##Plot of Kruskal-Wallis Test Stats
kw.p <- c(mass.kw$p.value, levo.kw$p.value, x1np.kw$p.value,BaP.kw$p.value)
labels(kw.p,c("Mass", "Levo", "1-NP", "BaP"))
plot(kw.p, type = "n",axes = FALSE,frame.plot=TRUE, ylab = "P Value",
     xlab="Component", main="", ylim = c(0.0,1.0))
points(kw.p)
Axis(side=2, labels=TRUE)
Axis(side=1, at = 1:4, labels=c("Mass", "Levo", "1-NP", "BaP"))






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


#Linear Regression####

#Scatter plot of indoor vs outdoor

##All sites
lm_byloc <- lm(am.out.points[,"conc"]~am.in.points[,"conc"])
###Summary details
cor(am.in.points[,"conc"],am.out.points[,"conc"])
[1] 0.09176041
attributes(lm_byloc)
lm_byloc$coefficients
y = 0.0009577x + 24.9727
summary(lm_byloc)


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