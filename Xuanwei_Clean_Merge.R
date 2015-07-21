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

for (pkg in c("knitr", "RCurl", "hash", "rJava", "XLConnect", "dplyr", "ggmap",
              "doBy", "psych", "beeswarm", "ggplot2", "reshape2", "scales")) {
    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
        install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
        if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
            stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
        }
    }
}

#Merge files into single dataset by filterid####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Import, Merge, Clean Datasets                                                #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

##Gravimetric####
###Import Gravimetric Results, Exclude Columns, Change Column Names

grav <- read.csv(paste(wd, "/Xuanwei_Grav.csv", sep =""))

grav.cols.dont.want <- c("sampleid", "locnum", "sampernum", "burnnum", "onwt",
                         "offwt", "wtdiffmg", "ontime", "offtime",
                         "samplemin", "onflow", "offflow", "avgflow",  "samvoll")

grav <- grav[, ! names(grav) %in% grav.cols.dont.want, drop = F]

colnames(grav) <- c("FilterId", "SiteNumber", "Latitude", "Longitude",
                    "Location", "SamplePeriod", "Village", "BurnInHome",
                    "PM2.5Mass.ug", "SampledAirVolume.m3", "PM2.5Mass.ug.m3")

grav$Location  <- ifelse(grav$Location == "Ambient", "Outdoor", "Indoor")

##Levoglucosan####
###Import Levoglucosan Results, Exclude Columns, Change Column Names

lg <- read.csv(paste(wd, "/Xuanwei_LG.csv", sep = ""))

lg.cols.dont.want <- c("sampletype", "notes")

lg <- lg[, ! names(lg) %in% lg.cols.dont.want, drop = F]

colnames(lg) <- c("FilterId", "LgMass.ug")

####Merge Gravimetric and Levoglucosan Results

grav_lg <- merge(grav,lg,by="FilterId")

####Subset excluding Xuanwei City samples and Blanks

grav_lg <- subset(subset(grav_lg, SampledAirVolume.m3 != "NA"),
                  Village != "Xuanwei")

##Create Variable for the Volume of Air Sampled with Each Filter

AirVolume <- grav_lg$SampledAirVolume.m3

###Add New Column Replacing Levoglucosan <LOD with LOD/sqrt2

grav_lg$LgMass.ug.LOQ <- round(ifelse(grav_lg$LgMass.ug > 0.035,
                                      grav_lg$LgMass.ug, 0.035/sqrt(2)), 3)

###Add New Columns Correcting ug/filter to ug/m3

ToConcentration <- function (column, decimals) {
    round(column/AirVolume, decimals)
}

grav_lg$LgMass.ug.m3 <- ToConcentration(grav_lg$LgMass.ug, 3) 

grav_lg$LgMass.ug.m3.LOQ <- ToConcentration(grav_lg$LgMass.ug.LOQ, 3)

##Nitro PAH####
###Import Nitro PAH Results, Exclude Columns, Change Column Names

npah <- read.csv(paste(wd, "/Xuanwei_NPAH.csv", sep = ""))

npah.cols.dont.want <- c("Description", "sampletype", "X2NP_Flag", "X2NFL_Flag",
                         "X1NP_Flag")

npah <- npah[, ! names(npah) %in% npah.cols.dont.want, drop = F]

colnames(npah) <- c("FilterId", "X2np.pg", "X2nfl.pg", "X1np.pg")

###Add New Columns Replacing NPAH <LOQ with LOQ/sqrt2

ReplaceLOQ <- function (column, LOQ, decimals) {
    round(ifelse(column > LOQ, column, LOQ/sqrt(2)), decimals)
}

npah$X2np.pg.LOQ <- ReplaceLOQ(npah$X2np.pg, 0.7, 1)

npah$X2nfl.pg.LOQ <- ReplaceLOQ(npah$X2nfl.pg, 0.9, 1)

npah$X1np.pg.LOQ <- ReplaceLOQ(npah$X1np.pg, 2.6, 1)

###Merge Nitro PAH with Grav and Levo Results

gl_npah <- merge(grav_lg,npah,by="FilterId")

###Add New Columns Correcting pg/filter to ug/m3 #### Consolidate? sapply?####

ToMicrograms <- function (column, conversion) {
    column/conversion
}

k <- 1
for(i in 16:18){
    gl_npah[,21+k] <- ToMicrograms(gl_npah[,i],10^6)
    s <- unlist(strsplit(colnames(gl_npah[i]), split='.', fixed=TRUE))
    names(gl_npah)[21+k]<-paste(s[1],"Mass.ug",sep="")
    k <- k+1
}

k <- 1
for(i in 19:21){
    gl_npah[,24+k] <- ToMicrograms(gl_npah[,i],10^6)
    s <- unlist(strsplit(colnames(gl_npah[i]), split='.', fixed=TRUE))
    names(gl_npah)[24+k]<-paste(s[1],"Mass.ug.LOQ",sep="")
    k <- k+1
}

k <- 1
for(i in 22:24){
    gl_npah[,27+k] <- ToConcentration(gl_npah[,i], 7)
    s <- unlist(strsplit(colnames(gl_npah[i]), split='.', fixed=TRUE))
    names(gl_npah)[27+k]<-paste(s[1],".ug.m3",sep="")
    k <- k+1
}

k <- 1
for(i in 25:27){
    gl_npah[,30+k] <- ToConcentration(gl_npah[,i], 7)
    s <- unlist(strsplit(colnames(gl_npah[i]), split='.', fixed=TRUE))
    names(gl_npah)[30+k]<-paste(s[1],".ug.m3.LOQ",sep="")
    k=k+1
}

###Remove Unnecessary Columns

npah.cols.dont.want <- c("X2np.pg", "X2nfl.pg", "X1np.pg", "X2np.pg.LOQ",
                         "X2nfl.pg.LOQ", "X1np.pg.LOQ")

gl_npah <- gl_npah[, ! names(gl_npah) %in% npah.cols.dont.want, drop = F]

##PAH####
###Import PAH Results, Replace <0.5 with 0.5

pah <- read.csv(paste(wd, "/Xuanwei_PAH.csv", sep = ""))

colnames(pah) <- c("FilterId", "FlAn.ng", "Pyr.ng", "BaA.ng", "Chry.ng",
                   "BbF.ng", "BkF.ng", "BaP.ng", "I123cdP.ng", "DahA.ng", 
                   "BghiP.ng")

for(i in c(2:ncol(pah))) {
    pah[,i] <- as.numeric(as.character(pah[,i]))
}

pah[is.na(pah)] <- 0.5

###Add New Columns Replacing PAH <LOQ with LOQ/sqrt2

k <- 1
for(i in 2:11){
    pah[,11+k] <- ReplaceLOQ(pah[,i], 0.5, 2)
    s <- unlist(strsplit(colnames(pah[i]), split='.', fixed=TRUE))
    names(pah)[11+k]<-paste(s[1], s[2],"LOQ",sep=".")
    k <- k+1
}

###Merge PAH with Grav, Levo, Nitro PAH Results

gln_pah <- merge(gl_npah, pah, by="FilterId")

###Add New Columns Correcting ng/sample to ug/m3 #### Consolidate? sapply?####

k <- 1
for(i in 28:37){
    gln_pah[,47+k] <- ToMicrograms(gln_pah[,i],10^3)
    s <- unlist(strsplit(colnames(gln_pah[i]), split='.', fixed=TRUE))
    names(gln_pah)[47+k]<-paste(s[1],"Mass.ug",sep="")
    k <- k+1
} 

k <- 1
for(i in 38:47){
    gln_pah[,57+k] <- ToMicrograms(gln_pah[,i],10^3)
    s <- unlist(strsplit(colnames(gln_pah[i]), split='.', fixed=TRUE))
    names(gln_pah)[57+k]<-paste(s[1],"Mass.ug.LOQ",sep="")
    k <- k+1
} 

k=1
for(i in 48:57){
    gln_pah[,67+k] <- ToConcentration(gln_pah[,i], 4) 
    s <- unlist(strsplit(colnames(gln_pah[i]), split='.', fixed=TRUE))
    names(gln_pah)[67+k]<-paste(s[1],".ug.m3",sep="")
    k=k+1
}

k=1
for(i in 58:67){
    gln_pah[,77+k] <- ToConcentration(gln_pah[,i], 4) 
    s <- unlist(strsplit(colnames(gln_pah[i]), split='.', fixed=TRUE))
    names(gln_pah)[77+k]<-paste(s[1],".ug.m3.LOQ",sep="")
    k=k+1
}

###Remove Unnecessary Columns

pah.cols.dont.want <- c("FlAn.ng", "Pyr.ng", "BaA.ng", "Chry.ng", "BbF.ng",
                        "BkF.ng", "BaP.ng", "I123cdP.ng", "DahA.ng", "BghiP.ng",
                        "FlAn.ng.LOQ", "Pyr.ng.LOQ", "BaA.ng.LOQ", "Chry.ng.LOQ",
                        "BbF.ng.LOQ", "BkF.ng.LOQ", "BaP.ng.LOQ",
                        "I123cdP.ng.LOQ", "DahA.ng.LOQ", "BghiP.ng.LOQ")

gln_pah <- gln_pah[, ! names(gln_pah) %in% pah.cols.dont.want, drop = F]

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

###Export ALL COMPONENT CONCENTRATION .CSV

write.csv(metals.by.slv, file = "metals_compile.csv")


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


write.csv(metals.by.slv, file = "metals_compile.csv")
