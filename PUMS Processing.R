

rm(list=ls())
dev.off()
options(scipen = 999)

library(easypackages)
libraries("plyr", "dplyr")

#Request and download data from https://usa.ipums.org

#Set work drive
setwd("D:/Users/SGA1/Desktop")


#Bringing in the full PUMS file
df <- read.csv(file="PUMS_Collier_2015.csv", header=TRUE, sep=",")
#Adding a general count for persons
df['COUNT'] <- 1

#Filter just to Collier County SW FL
newdata <- subset(df, STATEFIP == 12 & COUNTYFIPS == 21)

#Remove extra detail grade info
newdata$GRADEATTD <- NULL

#Remove group homes and other HH types such a 'Boat' that are other than standard.
newdata <- subset(newdata, GQ %in% c(1,2,5))

#For QC, a general count of all the different grade levels. 
count(newdata$GRADEATT)

#Subset the codes to just grades 1 through 12 and rename for School Age Children
newdata$GRADEATT[newdata$GRADEATT<3 | newdata$GRADEATT >5] <- 0
newdata$GRADEATT[newdata$GRADEATT >= 3 & newdata$GRADEATT <= 5] <- 1
names(newdata)[names(newdata) == 'GRADEATT'] <- "SAC"
count(newdata$SAC)


#Set up place to bring ddata back to aggregated numbers
#In this instance, I am subsetting by those who have moved in within the past 5 years. 
key <- subset(newdata, MOVEDIN %in% c(1,2,3,4,5,6,7,8,9), select = c("SERIAL", "MOVEDIN", "UNITSSTR","BEDROOMS"))
#Agreegate HH info for people and children


#Bringing back in all the info about the HH, including move in date.
HH_totals <-aggregate(cbind(SAC, COUNT) ~ SERIAL, newdata, FUN=sum)
HH_merge <- merge(HH_totals, key, by = "SERIAL")

rm(key)
rm(HH_totals)

#Filtering to those that moved in within the past 5 years, and limiting to just normal houses.
HH_5years <- subset(HH_merge, MOVEDIN %in% c(1,2,3))
HH_5years <- subset(HH_5years, UNITSSTR > 2)
HH_5years$MOVEDIN <- NULL

rm(HH_merge)

#Commenting out count lines. 
#count(HH_5years$UNITSSTR)
#count(HH_5years$BEDROOMS)

#Grouping together similar housing typologies
HH_5years$UNITSSTR <- as.character (HH_5years$UNITSSTR)
HH_5years$UNITSSTR <- plyr::mapvalues(HH_5years$UNITSSTR, c("3","4","5","6","7","8","9","10"), c("SF_Detached", "SF_Attached","Duplex","SM_MF","Large_MF","Large_MF","Large_MF","Large_MF"))

#Grouping together similar housing sizes.
HH_5years$BEDROOMS <- as.character (HH_5years$BEDROOMS)
HH_5years$BEDROOMS <- plyr::mapvalues(HH_5years$BEDROOMS, c("1","2","3","4","5","6","7"),c("0-1 Rooms","2-3 Rooms","2-3 Rooms","4+ Rooms","4+ Rooms","4+ Rooms","4+ Rooms"))
count(HH_5years$BEDROOMS)


#Just running a total for QC throughout the process
sum(HH_5years$SAC)

#Noting the number of observations used for each average.
SAC_Obvs <-aggregate(SAC ~ UNITSSTR+BEDROOMS, HH_5years, FUN=length)
names(SAC_Obvs)[names(SAC_Obvs) == 'SAC'] <- "Observations"

#Aggregating SAC into an average by home typology and # bedrooms.
SAC_Sum <-aggregate(SAC ~ UNITSSTR+BEDROOMS, HH_5years, FUN=mean)

#Putting together the obersavtions and averages into a final output.
Final_Summary <- merge(SAC_Sum, SAC_Obvs, by = c("UNITSSTR","BEDROOMS"))
rm(SAC_Obvs,SAC_Sum)

write.csv(Final_Summary, file = "PUMS_SAC_Summary.csv")




######################################################################################


#Seaprate file and process for HH Size


######################################################################################





sum(HH_5years$COUNT)

COUNT_Obvs <-aggregate(COUNT ~ UNITSSTR+BEDROOMS, HH_5years, FUN=length)
names(COUNT_Obvs)[names(COUNT_Obvs) == 'COUNT'] <- "Observations"

COUNT_Sum <-aggregate(COUNT ~ UNITSSTR+BEDROOMS, HH_5years, FUN=mean)

Final_Summary <- merge(COUNT_Sum, COUNT_Obvs, by = c("UNITSSTR","BEDROOMS"))
rm(COUNT_Obvs,COUNT_Sum)

write.csv(Final_Summary, file = "PUMS_HHsize_Summary.csv")






######################################################################################
######################################################################################